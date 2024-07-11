const std = @import("std");

const Interpreter = @import("./interpreter.zig").Interpreter;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const Token = @import("../token.zig").Token;
const Statement = @import("../parser/statement.zig").Statement;
const RuntimeError = @import("./interpreter.zig").RuntimeError;
const Environment = @import("./environment.zig").Environment;
const InterpreterError = @import("./interpreter.zig").InterpreterError;
const FunctionExpression = @import("../parser/expression.zig").FunctionExpression;
const NativeError = @import("./natives.zig").NativeError;

pub const CallableNativeFunction = *const fn (*Interpreter, std.ArrayList(VariableValue)) NativeError!VariableValue;

var functionId: u32 = 0;

pub const CallableFunction = union(enum) {
    Native: CallableNativeFunction,
    User: struct {
        parameters: std.ArrayList(Token),
        body: *const Statement,
        parentEnvironment: *Environment,
        /// For debugging.
        id: u32,
    },

    pub fn deinit(self: CallableFunction, interpreter: *Interpreter) void {
        switch (self) {
            .User => |data| {
                for (data.parameters.items) |parameter| {
                    interpreter.allocator.free(parameter.lexeme);
                }
                data.parameters.deinit();

                data.parentEnvironment.unreference(interpreter);
            },
            else => {},
        }
    }

    pub fn call(self: CallableFunction, interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) anyerror!VariableValue {
        switch (self) {
            .Native => |data| return try data(interpreter, arguments),
            .User => |data| {
                var environment = try interpreter.enterChildEnvironment(data.parentEnvironment, interpreter.activeEnvironment.?);
                defer environment.exit(interpreter);

                // Check the number of arguments
                if (arguments.items.len != data.parameters.items.len) {
                    interpreter.runtimeError = RuntimeError.tokenError(interpreter, data.parameters.items[0], "Expected {d} arguments but got {d}.", .{ data.parameters.items.len, arguments.items.len });
                    return InterpreterError.RuntimeError;
                }

                // Bind the arguments to the scope
                for (arguments.items, 0..) |argument, index| {
                    try environment.define(data.parameters.items[index].lexeme, argument, interpreter);
                }

                // Execute the function body
                interpreter.interpretStatement(data.body, true) catch |err| {
                    switch (err) {
                        InterpreterError.Return => return interpreter.lastReturnValue,
                        else => return err,
                    }
                };
                return VariableValue.null();
            },
        }
    }

    pub fn native(function: CallableNativeFunction) CallableFunction {
        return .{ .Native = function };
    }
    pub fn user(function: FunctionExpression, parentEnvironment: *Environment, allocator: std.mem.Allocator) !CallableFunction {
        var parameters = std.ArrayList(Token).init(allocator);
        for (function.parameters.items) |parameter| {
            try parameters.append(try parameter.clone(allocator));
        }

        functionId += 1;
        parentEnvironment.referenceCount += 1;
        return .{ .User = .{ .parameters = parameters, .body = function.body, .parentEnvironment = parentEnvironment, .id = functionId } };
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

    pub fn call(self: Callable, interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) anyerror!VariableValue {
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
    pub fn deinit(self: VariableValue, interpreter: *Interpreter) void {
        switch (self) {
            .String => |value| if (value.allocated) interpreter.allocator.free(value.string),
            .Function => |value| value.function.deinit(interpreter),
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
    pub fn fromFunction(function: FunctionExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !VariableValue {
        return .{ .Function = .{ .arity = @intCast(function.parameters.items.len), .function = try CallableFunction.user(function, activeEnvironment, allocator) } };
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

/// Essentially a wrapper around VariableValue.
pub const ExpressionInterpretResult = struct {
    value: VariableValue,
    /// A value is an immediate value if it requires
    /// deinitialization after it is no longer needed.
    /// This doesn't happen, for example, on variable
    /// access. This probably isn't the best way to
    /// structure this, but it works for now.
    immediateValue: bool,

    pub fn deinit(self: *ExpressionInterpretResult, interpreter: *Interpreter) void {
        if (self.immediateValue) {
            self.value.deinit(interpreter);
        }
    }

    pub fn isNumber(self: *const ExpressionInterpretResult) bool {
        return self.value.isNumber();
    }
    pub fn asNumber(self: *const ExpressionInterpretResult) f64 {
        return self.value.asNumber();
    }

    pub fn isString(self: *const ExpressionInterpretResult) bool {
        return self.value.isString();
    }
    pub fn asString(self: *const ExpressionInterpretResult) []const u8 {
        return self.value.asString();
    }

    pub fn isBoolean(self: *const ExpressionInterpretResult) bool {
        return self.value.isBoolean();
    }
    pub fn asBoolean(self: *const ExpressionInterpretResult) bool {
        return self.value.asBoolean();
    }

    pub fn isCallable(self: *const ExpressionInterpretResult) bool {
        return self.value.isCallable();
    }
    pub fn asCallable(self: *const ExpressionInterpretResult) Callable {
        return self.value.asCallable();
    }

    pub fn isTruthy(self: *const ExpressionInterpretResult) bool {
        return self.value.isTruthy();
    }

    pub fn fromImmediateValue(value: VariableValue) ExpressionInterpretResult {
        return ExpressionInterpretResult{ .value = value, .immediateValue = true };
    }
    pub fn fromNonImmediateValue(value: VariableValue) ExpressionInterpretResult {
        return ExpressionInterpretResult{ .value = value, .immediateValue = false };
    }

    pub fn fromLiteral(value: TokenLiteral) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromLiteral(value));
    }
    pub fn fromNumber(value: f64) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromNumber(value));
    }
    pub fn fromString(value: []const u8, allocated: bool) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromString(value, allocated));
    }
    pub fn fromBoolean(value: bool) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromBoolean(value));
    }
    pub fn fromFunction(value: FunctionExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !ExpressionInterpretResult {
        return ExpressionInterpretResult.fromNonImmediateValue(try VariableValue.fromFunction(value, activeEnvironment, allocator));
    }
    pub fn @"null"() ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.null());
    }

    pub fn isEqual(self: ExpressionInterpretResult, other: ExpressionInterpretResult) bool {
        return self.value.isEqual(other.value);
    }
};
