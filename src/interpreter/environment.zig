const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;

const RuntimeError = @import("./interpreter.zig").RuntimeError;
const Interpreter = @import("./interpreter.zig").Interpreter;
const InterpreterError = @import("./interpreter.zig").InterpreterError;

const Token = @import("../token.zig").Token;

pub const Environment = @This();

const ValueWrapper = struct {
    value: VariableValue,
    name: []const u8,
};

allocator: std.mem.Allocator,
values: std.StringHashMapUnmanaged(*ValueWrapper),
parent: ?*Environment = null,

pub fn init(allocator: std.mem.Allocator) Environment {
    return .{
        .allocator = allocator,
        .values = std.StringHashMapUnmanaged(*ValueWrapper){},
        .parent = null,
    };
}

pub fn createChild(self: *Environment) Environment {
    return .{
        .allocator = self.allocator,
        .values = std.StringHashMapUnmanaged(*ValueWrapper){},
        .parent = self,
    };
}

pub fn deinit(self: *Environment) void {
    var iter = self.values.iterator();
    while (iter.next()) |entry| {
        const wrapper = entry.value_ptr.*;
        self.allocator.free(wrapper.name);
        wrapper.value.deinit(self.allocator);
        self.allocator.destroy(wrapper);
    }
    self.values.deinit(self.allocator);
}

pub fn define(self: *Environment, name: []const u8, value: VariableValue) !void {
    // We need to copy the name because the string is owned by the parser and will be deallocated
    const wrapper = try self.allocator.create(ValueWrapper);
    wrapper.* = .{ .value = value, .name = try self.allocator.dupe(u8, name) };
    const previousValue = try self.values.fetchPut(self.allocator, wrapper.name, wrapper);

    if (previousValue != null) {
        const prev = previousValue.?.value;
        self.allocator.free(prev.name);
        prev.value.deinit(self.allocator);
        self.allocator.destroy(prev);
    }
}

pub fn assign(self: *Environment, name: Token, value: VariableValue, interpreter: *Interpreter) !void {
    const pointer = self.values.getPtr(name.lexeme);
    if (pointer == null) {
        if (self.parent != null) {
            try self.parent.?.assign(name, value, interpreter);
            return;
        }

        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to assign to {s}, which is undefined.", .{name.lexeme});
        return;
    }

    pointer.?.*.value = value;
}

pub fn get(self: *Environment, name: Token, interpreter: *Interpreter) !VariableValue {
    const entry = self.values.get(name.lexeme);
    if (entry != null) return entry.?.value;

    if (self.parent != null) {
        return try self.parent.?.get(name, interpreter);
    }

    interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to access {s}, which is undefined.", .{name.lexeme});
    return InterpreterError.RuntimeError;
}
