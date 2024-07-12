const std = @import("std");
const Token = @import("../token.zig").Token;
const Environment = @import("./environment.zig").Environment;
const Interpreter = @import("./interpreter.zig").Interpreter;

const CallableFunction = @import("./callable_value.zig").CallableFunction;

const ClassExpression = @import("../parser//expression.zig").ClassExpression;

pub const ClassValue = struct {
    referenceCount: usize,
    callable: CallableFunction,

    pub fn new(allocator: std.mem.Allocator, class: ClassExpression, activeEnvironment: *Environment) !*ClassValue {
        const value = try allocator.create(ClassValue);
        value.* = .{
            .referenceCount = 1,
            .callable = try CallableFunction.class(class, activeEnvironment, allocator),
        };
        return value;
    }

    pub fn deinit(self: *ClassValue, interpreter: *Interpreter) void {
        self.referenceCount -= 1;
        if (self.referenceCount == 0) {
            self.callable.deinit(interpreter);
            interpreter.allocator.destroy(self);
        }
    }
};

pub const Class = struct {
    methods: std.ArrayListUnmanaged(ClassMethod),

    pub fn new(expression: ClassExpression, allocator: std.mem.Allocator) !Class {
        var array = std.ArrayListUnmanaged(ClassMethod){};
        for (expression.methods.items) |method| {
            try array.append(allocator, ClassMethod.new(method.static, try method.name.clone(allocator)));
        }
        return .{ .methods = array };
    }

    pub fn deinit(self: *Class, allocator: std.mem.Allocator) void {
        for (self.methods.items) |method| {
            method.deinit(allocator);
        }
        self.methods.deinit(allocator);
    }
};

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
