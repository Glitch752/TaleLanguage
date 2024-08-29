const std = @import("std");
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;
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

pub const CallableNativeFunction = *const fn (*ModuleInterpreter, std.ArrayList(VariableValue)) NativeError!VariableValue;

const UserFunction = struct {
    parameters: std.ArrayListUnmanaged(Token),
    body: *const Statement,
    parentEnvironment: *Environment,
    /// For debugging.
    id: u32,

    pub fn deinit(self: *UserFunction, allocator: std.mem.Allocator) void {
        for (self.parameters.items) |parameter| {
            allocator.free(parameter.lexeme);
            parameter.deinit(allocator);
        }
        self.parameters.deinit(allocator);

        self.parentEnvironment.unreference(allocator);
    }
};
const UserFunctionReference = RCSP.DeinitializingRcSharedPointer(UserFunction, RCSP.NonAtomic, std.mem.Allocator);

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

    pub fn deinit(self: *CallableFunction, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .User => _ = self.User.deinit(allocator),
            .BoundClassMethod => {
                _ = self.BoundClassMethod.method.deinit(allocator);
                _ = self.BoundClassMethod.classInstance.deinit(allocator);
            },
            .ClassType => _ = self.ClassType.deinit(allocator),
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

    pub fn call(self: CallableFunction, interpreter: *ModuleInterpreter, callToken: Token, arguments: std.ArrayList(VariableValue)) anyerror!VariableValue {
        switch (self) {
            .Native => |data| {
                // Check the number of arguments
                if (arguments.items.len != data.arity) {
                    interpreter.runtimeError = RuntimeError.tokenError(interpreter, callToken, "Expected {d} arguments but got {d}.", .{ arguments.items.len, data.arity });
                    return InterpreterError.RuntimeError;
                }

                defer {
                    for (arguments.items) |value| {
                        var val = value;
                        val.deinit(interpreter.allocator);
                    }
                }

                return try data.body(interpreter, arguments);
            },
            .User => |data| {
                var environment = try interpreter.enterChildEnvironment(data.ptr().parentEnvironment, interpreter.activeEnvironment.?);
                defer environment.exit(interpreter);

                return try callUserFunction(interpreter, callToken, environment, data, arguments);
            },
            .BoundClassMethod => |data| {
                const previousEnvironment = interpreter.activeEnvironment;

                interpreter.activeEnvironment = data.classInstance.ptr().environment;

                var environment = try interpreter.enterChildEnvironment(interpreter.activeEnvironment.?, previousEnvironment.?);
                defer environment.exit(interpreter);

                const function = data.method.ptr();

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
            },
            .ClassType => |data| {
                return try VariableValue.newClassInstance(data, callToken, arguments);
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
    pub fn classType(expression: ClassExpression, parentEnvironment: *Environment, parentModule: *ModuleInterpreter, superClass: ?ClassTypeReference) !CallableFunction {
        return .{ .ClassType = try ClassType.new(parentEnvironment, expression, parentModule, superClass) };
    }

    pub fn takeReference(self: CallableFunction) CallableFunction {
        switch (self) {
            .User => return .{ .User = self.User.strongClone() },
            .BoundClassMethod => return .{ .BoundClassMethod = .{
                .method = self.BoundClassMethod.method.strongClone(),
                .classInstance = self.BoundClassMethod.classInstance.strongClone(),
            } },
            .ClassType => return .{ .ClassType = self.ClassType.strongClone() },
            else => return self,
        }
    }

    /// NOTE: classInstance must be a unique reference. It's not cloned.
    pub fn bindToClass(self: *const CallableFunction, classInstance: ClassInstanceReference) CallableFunction {
        switch (self.*) {
            .User => return .{ .BoundClassMethod = .{ .method = self.User.strongClone(), .classInstance = classInstance } },
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

fn callUserFunction(interpreter: *ModuleInterpreter, callToken: Token, environment: *Environment, functionReference: UserFunctionReference, arguments: std.ArrayList(VariableValue)) anyerror!VariableValue {
    const function = functionReference.ptr();

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
