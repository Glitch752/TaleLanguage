const std = @import("std");
const Interpreter = @import("./interpreter.zig").Interpreter;
const VariableValue = @import("./variable_value.zig").VariableValue;
const NativeError = @import("./natives.zig").NativeError;
const Token = @import("../token.zig").Token;
const Statement = @import("../parser/statement.zig").Statement;
const Environment = @import("./environment.zig").Environment;
const ClassType = @import("./class_value.zig").ClassType;
const ClassTypeReference = @import("./class_value.zig").ClassTypeReference;
const ClassInstance = @import("./class_value.zig").ClassInstance;
const ClassInstanceReference = @import("./class_value.zig").ClassInstanceReference;

const FunctionExpression = @import("../parser/expression.zig").FunctionExpression;
const ClassExpression = @import("../parser/expression.zig").ClassExpression;

const RuntimeError = @import("./interpreterError.zig").RuntimeError;
const InterpreterError = @import("./interpreterError.zig").InterpreterError;

const RCSP = @import("../rcsp.zig");

pub const CallableNativeFunction = *const fn (*Interpreter, std.ArrayList(VariableValue)) NativeError!VariableValue;

const UserFunction = struct {
    parameters: std.ArrayListUnmanaged(Token),
    body: *const Statement,
    parentEnvironment: *Environment,
    /// For debugging.
    id: u32,

    pub fn deinit(self: *UserFunction, interpreter: *Interpreter) void {
        for (self.parameters.items) |parameter| {
            interpreter.allocator.free(parameter.lexeme);
            parameter.deinit(interpreter.allocator);
        }
        self.parameters.deinit(interpreter.allocator);

        self.parentEnvironment.unreference(interpreter);
    }
};
const UserFunctionReference = RCSP.DeinitializingRcSharedPointer(UserFunction, RCSP.NonAtomic, *Interpreter);

var functionId: u32 = 0;

// TODO: Pass functions by reference?
pub const CallableFunction = union(enum) {
    Native: struct {
        arity: u32,
        body: CallableNativeFunction,
    },
    User: UserFunctionReference,
    /// Similar to normal user functions, but has some distinct calling characteristics (notably `this`).
    BoundClassMethod: struct {
        method: UserFunctionReference,
        classInstance: ClassInstanceReference,
    },

    /// NOTE: This isn't a class instance, but a class type.
    ClassType: ClassTypeReference,

    pub fn deinit(self: *CallableFunction, interpreter: *Interpreter) void {
        switch (self.*) {
            .User => _ = self.User.deinit(interpreter),
            .BoundClassMethod => {
                _ = self.BoundClassMethod.method.deinit(interpreter);
                _ = self.BoundClassMethod.classInstance.deinit(interpreter);
            },
            .ClassType => _ = self.ClassType.deinit(interpreter),
            else => {},
        }
    }

    pub fn getArity(self: CallableFunction) u32 {
        switch (self) {
            .Native => |data| return data.arity,
            .User => |data| return @intCast(data.ptr().parameters.items.len),
            .BoundClassMethod => return @intCast(self.BoundClassMethod.method.ptr().parameters.items.len),
            .ClassType => return self.ClassType.ptr().getArity(),
        }
    }

    pub fn call(self: CallableFunction, interpreter: *Interpreter, callToken: Token, arguments: std.ArrayList(VariableValue)) anyerror!VariableValue {
        switch (self) {
            .Native => |data| {
                // Check the number of arguments
                if (arguments.items.len != data.arity) {
                    interpreter.runtimeError = RuntimeError.tokenError(interpreter, callToken, "Expected {d} arguments but got {d}.", .{ arguments.items.len, data.arity });
                    return InterpreterError.RuntimeError;
                }

                return try data.body(interpreter, arguments);
            },
            .User => |data| {
                return try callUserFunction(interpreter, callToken, data, arguments);
            },
            .BoundClassMethod => |data| {
                const previousEnvironment = interpreter.activeEnvironment;

                interpreter.activeEnvironment = data.classInstance.ptr().environment;
                try interpreter.activeEnvironment.?.printVariables(interpreter, 0);
                defer interpreter.activeEnvironment = previousEnvironment;

                return try callUserFunction(interpreter, callToken, data.method, arguments);
            },
            .ClassType => |data| {
                return try VariableValue.newClassInstance(data, interpreter, callToken, arguments);
            },
        }
    }

    pub fn native(arity: u32, function: CallableNativeFunction) CallableFunction {
        return .{ .Native = .{ .arity = arity, .body = function } };
    }
    pub fn user(function: FunctionExpression, parentEnvironment: *Environment, allocator: std.mem.Allocator) !CallableFunction {
        var parameters = std.ArrayListUnmanaged(Token){};
        for (function.parameters.items) |parameter| {
            try parameters.append(allocator, try parameter.clone(allocator));
        }

        functionId += 1;
        parentEnvironment.referenceCount += 1;
        return .{ .User = try UserFunctionReference.init(.{
            .parameters = parameters,
            .body = function.body,
            .parentEnvironment = parentEnvironment,
            .id = functionId,
        }, allocator) };
    }
    pub fn classType(expression: ClassExpression, parentEnvironment: *Environment, allocator: std.mem.Allocator) !CallableFunction {
        return .{ .ClassType = try ClassType.new(parentEnvironment, expression, allocator) };
    }

    pub fn bindToClass(self: *const CallableFunction, classInstance: ClassInstanceReference) CallableFunction {
        switch (self.*) {
            .User => return .{ .BoundClassMethod = .{ .method = self.User, .classInstance = classInstance } },
            else => std.debug.panic("Tried to bind a non-class method to a class instance", .{}),
        }
    }

    pub fn toString(self: CallableFunction, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .Native => return std.fmt.allocPrint(allocator, "<native function>", .{}),
            .User => return std.fmt.allocPrint(allocator, "<function>", .{}),
            .BoundClassMethod => return std.fmt.allocPrint(allocator, "<bound class method>", .{}),
            .ClassType => return std.fmt.allocPrint(allocator, "<class type>", .{}),
        }
    }
};

fn callUserFunction(interpreter: *Interpreter, callToken: Token, functionReference: UserFunctionReference, arguments: std.ArrayList(VariableValue)) anyerror!VariableValue {
    const function = functionReference.ptr();

    var environment = try interpreter.enterChildEnvironment(function.parentEnvironment, interpreter.activeEnvironment.?);
    defer environment.exit(interpreter);

    // Check the number of arguments
    if (arguments.items.len != function.parameters.items.len) {
        interpreter.runtimeError = RuntimeError.tokenError(interpreter, callToken, "Expected {d} arguments but got {d}.", .{ function.parameters.items.len, arguments.items.len });
        return InterpreterError.RuntimeError;
    }

    // Bind the arguments to the scope
    for (arguments.items, 0..) |argument, index| {
        try environment.define(function.parameters.items[index].lexeme, argument, interpreter);
    }

    // Execute the function body
    interpreter.interpretStatement(function.body, true) catch |err| {
        switch (err) {
            InterpreterError.Return => return interpreter.lastReturnValue,
            else => return err,
        }
    };
    return VariableValue.null();
}
