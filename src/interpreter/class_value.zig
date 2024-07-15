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
            self.classType.unreference(interpreter.allocator);

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
        for (self.classType.methods.items) |method| {
            if (std.mem.eql(u8, method.name.lexeme, name.lexeme)) {
                // TODO
                std.debug.panic("TODO: Implement method lookup", .{});
            }
        }

        const property = self.fieldValues.get(name.lexeme);
        if (property != null) {
            return property.?;
        }

        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "No proprety or method '{s}' found.", .{name.lexeme});
        return InterpreterError.RuntimeError;
    }

    pub fn set(self: *ClassInstance, name: Token, value: VariableValue, interpreter: *Interpreter) !void {
        for (self.classType.methods.items) |method| {
            if (std.mem.eql(u8, method.name.lexeme, name.lexeme)) {
                interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Cannot assign to method '{s}'.", .{name.lexeme});
                return InterpreterError.RuntimeError;
            }
        }

        const duplicatedName = try interpreter.allocator.dupe(u8, name.lexeme);
        try self.fieldValues.put(interpreter.allocator, duplicatedName, value);
    }
};

/// NOTE: This isn't a class instance, but a class type.
pub const ClassType = struct {
    referenceCount: usize,
    methods: std.ArrayListUnmanaged(ClassMethod),

    pub fn new(expression: ClassExpression, allocator: std.mem.Allocator) !*ClassType {
        var array = std.ArrayListUnmanaged(ClassMethod){};
        for (expression.methods.items) |method| {
            try array.append(allocator, ClassMethod.new(method.static, try method.name.clone(allocator)));
        }

        const value = try allocator.create(ClassType);
        value.* = .{
            .referenceCount = 1,
            .methods = array,
        };
        return value;
    }

    pub fn unreference(self: *ClassType, allocator: std.mem.Allocator) void {
        self.referenceCount -= 1;

        if (self.referenceCount == 0) {
            for (self.methods.items) |method| {
                method.deinit(allocator);
            }
            self.methods.deinit(allocator);

            allocator.destroy(self);
        }
    }

    pub fn getArity(self: *const ClassType) u32 {
        // TODO: Return constructor arity
        _ = self;
        return 0;
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

    pub fn new(static: bool, name: Token) ClassMethod {
        return .{ .static = static, .name = name };
    }

    pub fn deinit(self: *const ClassMethod, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
    }
};
