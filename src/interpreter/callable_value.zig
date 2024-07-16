const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const VariableValue = @import("./variable_value.zig").VariableValue;
const NativeError = @import("./natives.zig").NativeError;
const Token = @import("../token.zig").Token;
const Statement = @import("../parser/statement.zig").Statement;
const Environment = @import("./environment.zig").Environment;
const ClassType = @import("./class_value.zig").ClassType;

const FunctionExpression = @import("../parser/expression.zig").FunctionExpression;
const ClassExpression = @import("../parser/expression.zig").ClassExpression;

const RuntimeError = @import("./interpreterError.zig").RuntimeError;
const InterpreterError = @import("./interpreterError.zig").InterpreterError;

pub const CallableNativeFunction = *const fn (*Interpreter, std.ArrayList(VariableValue)) NativeError!VariableValue;

var functionId: u32 = 0;

// TODO: Pass functions by reference?
pub const CallableFunction = union(enum) {
    Native: CallableNativeFunction,
    User: struct {
        parameters: std.ArrayList(Token),
        body: *const Statement,
        parentEnvironment: *Environment,
        /// For debugging.
        id: u32,
    },
    /// Similar to normal user functions, but has some distinct calling characteristics (notably `this`).
    ClassMethod: struct {
        parameters: std.ArrayList(Token),
        body: *const Statement,
        parentEnvironment: *Environment,
        /// For debugging.
        id: u32,
    },
    /// NOTE: This isn't a class instance, but a class type.
    ClassType: struct {
        class: *ClassType,
        /// For debugging.
        id: u32,
    },

    pub fn deinit(self: *CallableFunction, interpreter: *Interpreter) void {
        switch (self.*) {
            .User => |data| {
                for (data.parameters.items) |parameter| {
                    interpreter.allocator.free(parameter.lexeme);
                }
                data.parameters.deinit();

                data.parentEnvironment.unreference(interpreter);
            },
            .ClassType => {
                self.ClassType.class.unreference(interpreter);
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
            .ClassMethod => |data| {
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
            .ClassType => |data| {
                return try VariableValue.newClassInstance(data.class, interpreter.allocator);
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
    pub fn classType(expression: ClassExpression, parentEnvironment: *Environment, allocator: std.mem.Allocator) !CallableFunction {
        functionId += 1;
        return .{ .ClassType = .{ .class = try ClassType.new(parentEnvironment, expression, allocator), .id = functionId } };
    }
    pub fn classMethod(expression: FunctionExpression, parentEnvironment: *Environment, allocator: std.mem.Allocator) !CallableFunction {
        functionId += 1;
        parentEnvironment.referenceCount += 1;
        return .{ .ClassMethod = .{ .parameters = std.ArrayList(Token).init(allocator), .body = expression.body, .parentEnvironment = parentEnvironment, .id = functionId } };
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
