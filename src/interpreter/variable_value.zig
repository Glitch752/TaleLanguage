const std = @import("std");

const Interpreter = @import("./interpreter.zig").Interpreter;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const Token = @import("../token.zig").Token;
const Statement = @import("../parser/statement.zig").Statement;
const RuntimeError = @import("./interpreter.zig").RuntimeError;
const Environment = @import("./environment.zig").Environment;
const InterpreterError = @import("./interpreter.zig").InterpreterError;
const FunctionExpression = @import("../parser/expression.zig").FunctionExpression;

pub const CallableNativeFunction = *const fn (*Interpreter, std.ArrayList(VariableValue)) VariableValue;

pub const CallableFunction = union(enum) {
    Native: CallableNativeFunction,
    User: struct {
        parameters: std.ArrayList(Token),
        body: *const Statement,
        parentEnvironment: *Environment,
    },

    pub fn call(self: CallableFunction, interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) !VariableValue {
        switch (self) {
            .Native => |data| return data(interpreter, arguments),
            .User => |data| {
                var environment = try interpreter.enterChildEnvironment(data.parentEnvironment, interpreter.activeEnvironment.?);
                defer environment.deinit(interpreter);

                // Check the number of arguments
                if (arguments.items.len != data.parameters.items.len) {
                    interpreter.runtimeError = RuntimeError.tokenError(interpreter, data.parameters.items[0], "Expected {d} arguments but got {d}.", .{ data.parameters.items.len, arguments.items.len });
                    return InterpreterError.RuntimeError;
                }

                // Bind the arguments to the scope
                for (arguments.items, 0..) |argument, index| {
                    try environment.define(data.parameters.items[index].lexeme, argument);
                }

                // Execute the function body
                try interpreter.interpretStatement(data.body);
                // TODO: return values
                return VariableValue.null();
            },
        }
    }

    pub fn native(function: CallableNativeFunction) CallableFunction {
        return .{ .Native = function };
    }
    pub fn user(function: FunctionExpression, parentEnvironment: *Environment) CallableFunction {
        return .{ .User = .{ .parameters = function.parameters, .body = function.body, .parentEnvironment = parentEnvironment } };
    }

    pub fn toString(self: CallableFunction, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .Native => return std.fmt.allocPrint(allocator, "<native function>", .{}),
            .User => return std.fmt.allocPrint(allocator, "<function>", .{}),
        }
    }
};

pub const Callable = struct {
    arity: u32,
    function: CallableFunction,

    pub fn call(self: Callable, interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) !VariableValue {
        return try self.function.call(interpreter, arguments);
    }
};

pub const VariableValue = union(enum) {
    Number: f64,
    String: struct {
        string: []const u8,
        allocated: bool,
    },
    Boolean: bool,

    Function: struct {
        arity: u32,
        function: CallableFunction,
    },

    Null,

    /// Deinitializes the value if it was allocated.
    /// Must be called before a string variable is discarded.
    pub fn deinit(self: VariableValue, allocator: std.mem.Allocator) void {
        switch (self) {
            .String => |value| if (value.allocated) allocator.free(value.string),
            else => {},
        }
    }

    // Accessors and type checks

    pub fn isNumber(self: VariableValue) bool {
        return self == .Number;
    }
    pub fn asNumber(self: VariableValue) f64 {
        return self.Number;
    }

    pub fn isString(self: VariableValue) bool {
        return self == .String;
    }
    pub fn asString(self: VariableValue) []const u8 {
        return self.String.string;
    }

    pub fn isBoolean(self: VariableValue) bool {
        return self == .Boolean;
    }
    pub fn asBoolean(self: VariableValue) bool {
        return self.Boolean;
    }

    pub fn isNull(self: VariableValue) bool {
        return self == .Null;
    }

    pub fn isCallable(self: VariableValue) bool {
        return self == .Function;
    }
    pub fn asCallable(self: VariableValue) Callable {
        return .{
            .arity = self.Function.arity,
            .function = self.Function.function,
        };
    }

    // Type coercion
    pub fn isTruthy(self: VariableValue) bool {
        switch (self) {
            .Number => |value| return value != 0,
            .String => |value| return value.string.len != 0,
            .Boolean => |value| return value,
            else => return false,
        }
    }

    pub fn isEqual(self: VariableValue, other: VariableValue) bool {
        switch (self) {
            .Number => |value| return other.isNumber() and other.asNumber() == value,
            .String => |value| return other.isString() and std.mem.eql(u8, other.asString(), value.string),
            .Boolean => |value| return other.isBoolean() and other.asBoolean() == value,
            .Null => return other.isNull(),
            else => return false,
        }
    }

    // Constructors

    pub fn fromNumber(number: f64) VariableValue {
        return .{ .Number = number };
    }

    pub fn fromString(string: []const u8, allocated: bool) VariableValue {
        return .{ .String = .{ .string = string, .allocated = allocated } };
    }

    pub fn fromBoolean(boolean: bool) VariableValue {
        return .{ .Boolean = boolean };
    }

    pub fn fromLiteral(literal: TokenLiteral) VariableValue {
        switch (literal) {
            .NumberLiteral => |value| return .{ .Number = value },
            .StringLiteral => |value| return .{ .String = .{ .string = value, .allocated = false } },
            .Identifier => |value| return .{ .String = .{ .string = value, .allocated = false } }, // Identifiers don't exist yet
            .True => return .{ .Boolean = true },
            .False => return .{ .Boolean = true },
            else => return .{ .Null = {} },
        }
    }

    pub fn nativeFunction(arity: u32, function: CallableNativeFunction) VariableValue {
        return .{ .Function = .{ .arity = arity, .function = CallableFunction.native(function) } };
    }
    pub fn fromFunction(function: FunctionExpression, activeEnvironment: *Environment) VariableValue {
        return .{ .Function = .{ .arity = @intCast(function.parameters.items.len), .function = CallableFunction.user(function, activeEnvironment) } };
    }

    pub fn @"null"() VariableValue {
        return .Null;
    }

    // Debugging and visualization
    /// Converts the value to a string for debugging and visualization purposes.
    /// A new string is always allocated, so the caller is responsible for freeing it.
    pub fn toString(self: VariableValue, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .Number => |value| return try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .String => |value| return std.fmt.allocPrint(allocator, "{s}", .{value.string}),
            .Boolean => return if (self.Boolean) std.fmt.allocPrint(allocator, "true", .{}) else std.fmt.allocPrint(allocator, "false", .{}),
            .Null => return std.fmt.allocPrint(allocator, "null", .{}),
            .Function => |value| return value.function.toString(allocator),
        }
    }
};
