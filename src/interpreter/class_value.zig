const std = @import("std");
const Token = @import("../token.zig").Token;
const Environment = @import("./environment.zig").Environment;
const Interpreter = @import("./interpreter.zig").Interpreter;
const VariableValue = @import("./variable_value.zig").VariableValue;

const RuntimeError = @import("./interpreterError.zig").RuntimeError;
const InterpreterError = @import("./interpreterError.zig").InterpreterError;

const CallableFunction = @import("./callable_value.zig").CallableFunction;

const ClassExpression = @import("../parser//expression.zig").ClassExpression;

pub const ClassInstance = struct {
    referenceCount: usize,
    classType: *ClassType,

    fieldValues: std.StringHashMapUnmanaged(VariableValue),

    pub fn new(allocator: std.mem.Allocator, classType: *ClassType) !*ClassInstance {
        const value = try allocator.create(ClassInstance);
        value.* = .{
            .referenceCount = 1,
            .classType = classType,
            .fieldValues = std.StringHashMapUnmanaged(VariableValue){},
        };
        classType.referenceCount += 1;
        return value;
    }

    pub fn unreference(self: *ClassInstance, interpreter: *Interpreter) void {
        self.referenceCount -= 1;
        if (self.referenceCount == 0) {
            // TODO
            std.debug.print("Parent reference count: {d}\n", .{self.classType.referenceCount});
            self.classType.unreference(interpreter);

            var iter = self.fieldValues.iterator();
            while (iter.next()) |entry| {
                entry.value_ptr.deinit(interpreter);
                interpreter.allocator.free(entry.key_ptr.*);
            }
            self.fieldValues.deinit(interpreter.allocator);

            interpreter.allocator.destroy(self);
        }
    }

    pub fn toString(self: *const ClassInstance, allocator: std.mem.Allocator) ![]const u8 {
        _ = self;
        return std.fmt.allocPrint(allocator, "<instance <class>>", .{});
    }

    pub fn get(self: *const ClassInstance, name: Token, interpreter: *Interpreter) !VariableValue {
        const method = self.classType.methods.get(name.lexeme);
        if (method != null) {
            std.debug.panic("TODO: Implement method calls on class instances", .{});
        }

        const property = self.fieldValues.get(name.lexeme);
        if (property != null) {
            return property.?;
        }

        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "No proprety or method '{s}' found.", .{name.lexeme});
        return InterpreterError.RuntimeError;
    }

    pub fn set(self: *ClassInstance, name: Token, value: VariableValue, interpreter: *Interpreter) !void {
        if (self.classType.methods.contains(name.lexeme)) {
            interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Cannot assign to method '{s}'.", .{name.lexeme});
            return InterpreterError.RuntimeError;
        }

        const duplicatedName = try interpreter.allocator.dupe(u8, name.lexeme);
        try self.fieldValues.put(interpreter.allocator, duplicatedName, value);
    }
};

/// NOTE: This isn't a class instance, but a class type.
pub const ClassType = struct {
    referenceCount: usize,
    methods: std.StringHashMapUnmanaged(ClassMethod),
    parentEnvironment: *Environment,

    pub fn new(parentEnvironment: *Environment, expression: ClassExpression, allocator: std.mem.Allocator) !*ClassType {
        var methods = std.StringHashMapUnmanaged(ClassMethod){};
        for (expression.methods.items) |method| {
            const function = try CallableFunction.classMethod(method.function, parentEnvironment, allocator);
            const duplicatedName = try allocator.dupe(u8, method.name.lexeme);
            try methods.put(allocator, duplicatedName, ClassMethod.new(method.static, try method.name.clone(allocator), function));
        }

        const value = try allocator.create(ClassType);

        parentEnvironment.referenceCount += 1;
        value.* = .{
            .referenceCount = 1,
            .methods = methods,
            .parentEnvironment = parentEnvironment,
        };
        return value;
    }

    pub fn unreference(self: *ClassType, interpreter: *Interpreter) void {
        if (self.referenceCount == 0) {
            std.debug.panic("Unreferencing a class type with a reference count of 0", .{});
        }

        self.referenceCount -= 1;
        self.parentEnvironment.unreference(interpreter);

        if (self.referenceCount == 0) {
            var iterator = self.methods.iterator();
            while (iterator.next()) |method| {
                method.value_ptr.deinit(interpreter);
                interpreter.allocator.free(method.key_ptr.*);
            }
            self.methods.deinit(interpreter.allocator);

            interpreter.allocator.destroy(self);
        }
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
