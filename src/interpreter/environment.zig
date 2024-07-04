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
previous: ?*Environment = null,
deactive: bool = false,

pub fn init(allocator: std.mem.Allocator) Environment {
    return .{
        .allocator = allocator,
        .values = std.StringHashMapUnmanaged(*ValueWrapper){},
        .parent = null,
    };
}

pub fn createChild(self: *Environment, previous: *Environment) Environment {
    return .{ .allocator = self.allocator, .values = std.StringHashMapUnmanaged(*ValueWrapper){}, .parent = self, .previous = previous };
}

pub fn deinit(self: *Environment, interpreter: *Interpreter) void {
    var iter = self.values.iterator();
    while (iter.next()) |entry| {
        const wrapper = entry.value_ptr.*;
        self.allocator.free(wrapper.name);
        wrapper.value.deinit(self.allocator);
        self.allocator.destroy(wrapper);
    }
    self.values.deinit(self.allocator);

    if (self.parent != null) {
        interpreter.activeEnvironment = self.previous;

        self.allocator.destroy(self);
    }

    self.deactive = true;
}

pub fn define(self: *Environment, name: []const u8, value: VariableValue) !void {
    std.debug.print("Allocator: {any}", .{self.allocator});
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

pub fn repeat(count: usize, character: u8, allocator: std.mem.Allocator) ![]const u8 {
    const buffer = try allocator.alloc(u8, count);
    for (buffer, 0..) |_, i| {
        buffer[i] = character;
    }
    return buffer;
}
pub fn printVariables(self: *Environment, interpreter: *Interpreter, indent: u32) !void {
    const spaces = try repeat(indent, ' ', interpreter.allocator);
    defer interpreter.allocator.free(spaces);

    if (indent == 0) std.debug.print("---- Variables -----\n", .{});
    var iter = self.values.iterator();
    while (iter.next()) |entry| {
        const wrapper = entry.value_ptr.*;
        const name = wrapper.name;
        const value = wrapper.value;

        std.debug.print("{s}{s} = {s}\n", .{ spaces, name, try value.toString(interpreter.allocator) });
    }

    if (self.parent != null) {
        std.debug.print("{s}->\n", .{spaces});
        try self.parent.?.printVariables(interpreter, indent + 1);
    }
    if (indent == 0) std.debug.print("-----------\n", .{});
}
