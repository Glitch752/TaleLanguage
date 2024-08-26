const std = @import("std");
const Token = @import("../token.zig").Token;
const Environment = @import("./environment.zig").Environment;
const Interpreter = @import("./interpreter.zig").Interpreter;
const VariableValue = @import("./variable_value.zig").VariableValue;

const RuntimeError = @import("./interpreterError.zig").RuntimeError;
const InterpreterError = @import("./interpreterError.zig").InterpreterError;

const CallableFunction = @import("./callable_value.zig").CallableFunction;

const ClassExpression = @import("../parser//expression.zig").ClassExpression;

const RCSP = @import("../rcsp.zig");

pub const ClassInstanceReference = RCSP.DeinitializingRcSharedPointer(ClassInstance, RCSP.NonAtomic, *Interpreter);

pub const ClassInstance = struct {
    classType: ClassTypeReference,
    environment: *Environment,

    fieldValues: std.StringHashMapUnmanaged(VariableValue),

    pub fn new(interpreter: *Interpreter, classType: ClassTypeReference, callToken: Token, arguments: std.ArrayList(VariableValue)) !ClassInstanceReference {
        const allocatedEnvironment = try interpreter.allocator.create(Environment);
        var classTypeParent = classType.ptr().parentEnvironment;
        allocatedEnvironment.* = classTypeParent.createChild(classTypeParent);

        const value = try ClassInstanceReference.init(.{
            .classType = classType.strongClone(),
            .environment = allocatedEnvironment,
            .fieldValues = std.StringHashMapUnmanaged(VariableValue){},
        }, interpreter.allocator);

        try allocatedEnvironment.define("this", VariableValue.weakClassInstanceReference(value.weakClone()), interpreter);
        // TODO: Implement super once inheritance is implemented

        var constructorMethod = classType.ptr().methods.get("constructor");
        if (constructorMethod != null) {
            var boundConstructor = constructorMethod.?.function.bindToClass(value);
            defer boundConstructor.deinit(interpreter);

            _ = try boundConstructor.call(interpreter, callToken, arguments); // Return value is ignored
        }

        return value;
    }

    pub fn deinit(self: *ClassInstance, interpreter: *Interpreter) void {
        var iter = self.fieldValues.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit(interpreter);
            interpreter.allocator.free(entry.key_ptr.*);
        }
        self.fieldValues.deinit(interpreter.allocator);

        std.debug.assert(self.classType.strongCount() != 0);
        _ = self.classType.deinit(interpreter);

        self.environment.unreference(interpreter);
    }

    pub fn toString(self: *const ClassInstance, allocator: std.mem.Allocator) ![]const u8 {
        _ = self;
        return std.fmt.allocPrint(allocator, "<instance <class>>", .{});
    }

    pub fn get(self: *const ClassInstance, name: Token, selfReference: ClassInstanceReference, interpreter: *Interpreter) !VariableValue {
        const classType = self.classType.ptr();
        const method = classType.methods.get(name.lexeme);
        if (method != null) {
            if (method.?.static) {
                std.debug.panic("TODO: Implement static method calls on class instances", .{});
            } else {
                return VariableValue.newFunctionReference(method.?.function.bindToClass(selfReference));
            }
        }

        const property = self.fieldValues.get(name.lexeme);
        if (property != null) {
            return property.?.takeReference(interpreter);
        }

        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "No proprety or method '{s}' found.", .{name.lexeme});
        return InterpreterError.RuntimeError;
    }

    pub fn set(self: *ClassInstance, name: Token, value: VariableValue, interpreter: *Interpreter) !void {
        if (self.classType.ptr().methods.contains(name.lexeme)) {
            interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Cannot assign to method '{s}'.", .{name.lexeme});
            return InterpreterError.RuntimeError;
        }

        if (self.fieldValues.contains(name.lexeme)) {
            self.fieldValues.getPtr(name.lexeme).?.* = value;
            return;
        }

        const duplicatedName = try interpreter.allocator.dupe(u8, name.lexeme);
        try self.fieldValues.put(interpreter.allocator, duplicatedName, value);
    }
};

pub const ClassTypeReference = RCSP.DeinitializingRcSharedPointer(ClassType, RCSP.NonAtomic, *Interpreter);

/// NOTE: This isn't a class instance, but a class type.
pub const ClassType = struct {
    methods: std.StringHashMapUnmanaged(ClassMethod),
    parentEnvironment: *Environment,

    pub fn new(parentEnvironment: *Environment, expression: ClassExpression, allocator: std.mem.Allocator) !ClassTypeReference {
        var methods = std.StringHashMapUnmanaged(ClassMethod){};
        for (expression.methods.items) |method| {
            const function = try CallableFunction.user(method.function, parentEnvironment, allocator);
            const duplicatedName = try allocator.dupe(u8, method.name.lexeme);
            try methods.put(allocator, duplicatedName, ClassMethod.new(method.static, try method.name.clone(allocator), function));
        }

        const value = try ClassTypeReference.init(.{
            .methods = methods,
            .parentEnvironment = parentEnvironment,
        }, allocator);

        parentEnvironment.referenceCount += 1;
        return value;
    }

    pub fn deinit(self: *ClassType, interpreter: *Interpreter) void {
        var iterator = self.methods.iterator();
        while (iterator.next()) |method| {
            method.value_ptr.deinit(interpreter);
            interpreter.allocator.free(method.key_ptr.*);
        }
        self.methods.deinit(interpreter.allocator);

        self.parentEnvironment.unreference(interpreter);
    }

    pub fn getArity(self: *const ClassType) u32 {
        const constructorMethod = self.methods.get("constructor");

        if (constructorMethod == null) {
            return 0;
        }

        return constructorMethod.?.getArity();
    }

    pub fn toString(self: *const ClassType, allocator: std.mem.Allocator) ![]const u8 {
        _ = self;
        return std.fmt.allocPrint(allocator, "<class>", .{});
    }
};

/// NOTE: This exists on a class type, not a class instance.
pub const ClassMethod = struct {
    static: bool,
    name: Token,
    function: CallableFunction,

    pub fn new(static: bool, name: Token, function: CallableFunction) ClassMethod {
        return .{ .static = static, .name = name, .function = function };
    }

    pub fn deinit(self: *ClassMethod, interpreter: *Interpreter) void {
        self.name.deinit(interpreter.allocator);
        interpreter.allocator.free(self.name.lexeme);
        self.function.deinit(interpreter);
    }

    pub fn getArity(self: *const ClassMethod) u32 {
        return self.function.getArity();
    }
};
