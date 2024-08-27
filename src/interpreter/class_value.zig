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
        errdefer allocatedEnvironment.exit(interpreter);

        var value = try ClassInstanceReference.init(.{
            .classType = classType.strongClone(),
            .environment = allocatedEnvironment,
            .fieldValues = std.StringHashMapUnmanaged(VariableValue){},
        }, interpreter.allocator);
        errdefer _ = value.deinit(interpreter);

        try allocatedEnvironment.define("this", VariableValue.weakClassInstanceReference(value.weakClone()), interpreter);

        const constructorMethod = classType.ptr().getInstanceMethod("constructor");
        if (constructorMethod != null) {
            var boundConstructor = constructorMethod.?.function.bindToClass(value.strongClone());
            defer boundConstructor.deinit(interpreter);

            var result = try boundConstructor.call(interpreter, callToken, arguments);
            _ = result.deinit(interpreter);
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
        const method = classType.getInstanceMethod(name.lexeme);
        if (method != null) {
            return VariableValue.newFunctionReference(method.?.function.bindToClass(selfReference.strongClone()));
        }

        const property = self.fieldValues.get(name.lexeme);
        if (property != null) {
            return property.?.takeReference(interpreter);
        }

        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "No proprety or method '{s}' found.", .{name.lexeme});
        return InterpreterError.RuntimeError;
    }

    pub fn set(self: *ClassInstance, name: Token, value: VariableValue, interpreter: *Interpreter) !void {
        if (self.classType.ptr().instanceMethods.contains(name.lexeme)) {
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
const ClassTypeReferencePointer = *align(8) anyopaque; // ClassTypeReference.*;

/// NOTE: This isn't a class instance, but a class type.
pub const ClassType = struct {
    instanceMethods: std.StringHashMapUnmanaged(ClassMethod),
    staticMethods: std.StringHashMapUnmanaged(ClassMethod),

    parentEnvironment: *Environment,
    _superClass: ?ClassTypeReferencePointer,

    staticFieldValues: std.StringHashMapUnmanaged(VariableValue),

    pub fn superClass(self: *const ClassType) ?ClassTypeReference {
        if (self._superClass == null) {
            return null;
        }

        return @as(*ClassTypeReference, @ptrCast(self._superClass.?)).*;
    }

    pub fn new(parentEnvironment: *Environment, expression: ClassExpression, allocator: std.mem.Allocator, superClassType: ?ClassTypeReference) !ClassTypeReference {
        var instanceMethods = std.StringHashMapUnmanaged(ClassMethod){};
        var staticMethods = std.StringHashMapUnmanaged(ClassMethod){};
        for (expression.methods.items) |method| {
            const function = try CallableFunction.user(method.function, parentEnvironment, allocator);
            const duplicatedName = try allocator.dupe(u8, method.name.lexeme);
            try (if (method.static) staticMethods else instanceMethods).put(allocator, duplicatedName, ClassMethod.new(try method.name.clone(allocator), function));
        }

        var super: ?ClassTypeReferencePointer = null;
        if (superClassType != null) {
            const superClassPointer: *ClassTypeReference = try allocator.create(ClassTypeReference);
            superClassPointer.* = superClassType.?;
            super = @ptrCast(superClassPointer);
        }

        const value = try ClassTypeReference.init(.{
            .instanceMethods = instanceMethods,
            .staticMethods = staticMethods,

            .parentEnvironment = parentEnvironment,
            ._superClass = super,

            .staticFieldValues = std.StringHashMapUnmanaged(VariableValue){},
        }, allocator);

        parentEnvironment.referenceCount += 1;
        return value;
    }

    pub fn deinit(self: *ClassType, interpreter: *Interpreter) void {
        var instanceIterator = self.instanceMethods.iterator();
        while (instanceIterator.next()) |method| {
            method.value_ptr.deinit(interpreter);
            interpreter.allocator.free(method.key_ptr.*);
        }
        self.instanceMethods.deinit(interpreter.allocator);

        var staticIterator = self.staticMethods.iterator();
        while (staticIterator.next()) |method| {
            method.value_ptr.deinit(interpreter);
            interpreter.allocator.free(method.key_ptr.*);
        }
        self.staticMethods.deinit(interpreter.allocator);

        var fieldIterator = self.staticFieldValues.iterator();
        while (fieldIterator.next()) |field| {
            field.value_ptr.deinit(interpreter);
            interpreter.allocator.free(field.key_ptr.*);
        }
        self.staticFieldValues.deinit(interpreter.allocator);

        if (self._superClass != null) {
            var super = self.superClass();
            _ = super.?.deinit(interpreter);

            interpreter.allocator.destroy(@as(*ClassTypeReference, @ptrCast(self._superClass.?)));
        }

        self.parentEnvironment.unreference(interpreter);
    }

    pub fn getStatic(self: *const ClassType, name: Token, interpreter: *Interpreter) !VariableValue {
        const method = self.getStaticMethod(name.lexeme);
        if (method != null) {
            return VariableValue.newFunctionReference(method.?.function.takeReference());
        }

        const property = self.staticFieldValues.get(name.lexeme);
        if (property != null) {
            return property.?.takeReference(interpreter);
        }

        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "No static proprety or method '{s}' found.", .{name.lexeme});
        return InterpreterError.RuntimeError;
    }

    pub fn setStatic(self: *ClassType, name: Token, value: VariableValue, interpreter: *Interpreter) !void {
        if (self.staticMethods.contains(name.lexeme)) {
            interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Cannot assign to static method '{s}'.", .{name.lexeme});
            return InterpreterError.RuntimeError;
        }

        if (self.staticFieldValues.contains(name.lexeme)) {
            self.staticFieldValues.getPtr(name.lexeme).?.* = value;
            return;
        }

        const duplicatedName = try interpreter.allocator.dupe(u8, name.lexeme);
        try self.staticFieldValues.put(interpreter.allocator, duplicatedName, value);
    }

    fn getStaticMethod(self: *const ClassType, name: []const u8) ?ClassMethod {
        const value = self.staticMethods.get(name);
        if (value == null and self._superClass != null) {
            return self.superClass().?.ptr().getStaticMethod(name);
        }
        return value;
    }

    pub fn getArity(self: *const ClassType) u32 {
        const constructorMethod = self.instanceMethods.get("constructor");

        if (constructorMethod == null) {
            return 0;
        }

        return constructorMethod.?.getArity();
    }

    pub fn getInstanceMethod(self: *const ClassType, name: []const u8) ?ClassMethod {
        const value = self.instanceMethods.get(name);
        if (value == null and self._superClass != null) {
            return self.superClass().?.ptr().getInstanceMethod(name);
        }
        return value;
    }

    pub fn toString(self: *const ClassType, allocator: std.mem.Allocator) ![]const u8 {
        _ = self;
        return std.fmt.allocPrint(allocator, "<class>", .{});
    }
};

/// NOTE: This exists on a class type, not a class instance.
pub const ClassMethod = struct {
    name: Token,
    function: CallableFunction,

    pub fn new(name: Token, function: CallableFunction) ClassMethod {
        return .{ .name = name, .function = function };
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
