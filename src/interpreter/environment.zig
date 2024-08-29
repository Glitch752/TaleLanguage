const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;

const RuntimeError = @import("./interpreterError.zig").RuntimeError;
const InterpreterError = @import("./interpreterError.zig").InterpreterError;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;

const Token = @import("../token.zig").Token;

pub const Environment = @This();

// TODO: Remove this
const ValueWrapper = struct {
    value: VariableValue,
    name: []const u8,
};

const ValueMap = std.StringHashMapUnmanaged(*ValueWrapper);

allocator: std.mem.Allocator,
values: ValueMap,
parent: ?*Environment = null,
previous: ?*Environment = null,

/// Poor man's reference counting
referenceCount: usize = 1,

pub fn init(allocator: std.mem.Allocator) Environment {
    return .{
        .allocator = allocator,
        .values = ValueMap{},
        .parent = null,
    };
}

pub fn createChild(self: *Environment, previous: *Environment) Environment {
    self.referenceCount += 1;
    return .{ .allocator = self.allocator, .values = ValueMap{}, .parent = self, .previous = previous };
}

pub fn unreference(self: *Environment, allocator: std.mem.Allocator) void {
    std.debug.assert(self.referenceCount != 0); // Tried to unreference an environment that has 0 references

    self.referenceCount -= 1;
    if (self.referenceCount == 0) {
        std.debug.assert(self.parent != null); // Tried to deinit the root environment
        self.deinit(allocator);
    }
}

/// Exit is like unreference, but also returns to the previous environment
pub fn exit(self: *Environment, interpreter: *ModuleInterpreter) void {
    std.debug.assert(self.referenceCount != 0); // Tried to unreference an environment that has 0 references

    if (self.parent != null) {
        interpreter.activeEnvironment = self.previous;
    }

    self.referenceCount -= 1;
    if (self.referenceCount == 0) {
        std.debug.assert(self.parent != null); // Tried to deinit the root environment
        self.deinit(interpreter.allocator);
    }
}

/// Used to circumvent a (compiler bug?) issue where segfaults occur if we don't use value names in a certain way
fn discard(args: anytype) void {
    _ = args;
}

pub fn deinit(self: *Environment, allocator: std.mem.Allocator) void {
    var iter = self.values.iterator();
    while (iter.next()) |entry| {
        const wrapper = entry.value_ptr.*;
        discard(.{wrapper.name});

        wrapper.value.deinit(allocator);
        self.allocator.free(wrapper.name);

        self.allocator.destroy(wrapper);
    }
    self.values.deinit(self.allocator);

    if (self.parent != null) {
        self.parent.?.unreference(allocator);
        self.allocator.destroy(self);
    }
}

pub fn define(self: *Environment, name: []const u8, value: VariableValue, interpreter: ?*ModuleInterpreter) !void {
    // We need to copy the name because the string is owned by the parser and will be deallocated
    const wrapper = try self.allocator.create(ValueWrapper);
    wrapper.* = .{ .value = value, .name = try self.allocator.dupe(u8, name) };

    try self.values.ensureUnusedCapacity(self.allocator, 2);
    const previousValue = self.values.fetchPutAssumeCapacity(wrapper.name, wrapper);

    if (previousValue != null) {
        std.debug.assert(interpreter != null); // Tried to define a variable that already exists when the interpreter is not available.

        const prev = previousValue.?.value;
        self.allocator.free(prev.name);
        prev.value.deinit(interpreter.?.allocator);
        self.allocator.destroy(prev);
    }
}

pub fn assign(self: *Environment, name: Token, value: VariableValue, interpreter: *ModuleInterpreter) !void {
    const pointer = self.values.getPtr(name.lexeme);
    if (pointer == null) {
        if (self.parent != null) {
            try self.parent.?.assign(name, value, interpreter);
            return;
        }

        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to assign to {s}, which is undefined.", .{name.lexeme});
        return;
    }

    pointer.?.*.value.deinit(interpreter.allocator);

    pointer.?.*.value = value;
}

pub fn get(self: *Environment, name: Token, interpreter: *ModuleInterpreter) !VariableValue {
    const entry = self.values.get(name.lexeme);
    if (entry != null) return entry.?.value;

    if (self.parent != null) {
        return try self.parent.?.get(name, interpreter);
    }

    interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to access {s}, which is undefined.", .{name.lexeme});
    return InterpreterError.RuntimeError;
}

fn ancestorAtDepth(self: *Environment, depth: u32) !*Environment {
    var current = self;
    var i: u32 = 0;
    while (i < depth) {
        if (current.parent == null) std.debug.panic("Tried to access an ancestor at depth {d}, but the environment only has {d} ancestors.", .{ depth, i });
        current = current.parent.?;
        i += 1;
    }
    return current;
}

pub fn getAtDepth(self: *Environment, name: Token, depth: u32, interpreter: *ModuleInterpreter) !VariableValue {
    const environment = try self.ancestorAtDepth(depth);

    const entry = environment.values.get(name.lexeme);
    if (entry != null) return entry.?.value;

    interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to access {s} at depth {d}, which is undefined.", .{ name.lexeme, depth });
    return InterpreterError.RuntimeError;
}

pub fn getLexemeAtDepth(self: *Environment, lexeme: []const u8, errorToken: Token, depth: u32, interpreter: *ModuleInterpreter) !VariableValue {
    const environment = try self.ancestorAtDepth(depth);

    const entry = environment.values.get(lexeme);
    if (entry != null) return entry.?.value;

    interpreter.runtimeError = RuntimeError.tokenError(interpreter, errorToken, "Tried to access {s} at depth {d}, which is undefined.", .{ lexeme, depth });
    return InterpreterError.RuntimeError;
}

pub fn lexemeExistsAtDepth(self: *Environment, lexeme: []const u8, depth: u32) bool {
    const environment = try self.ancestorAtDepth(depth);
    return environment.values.contains(lexeme);
}

pub fn assignAtDepth(self: *Environment, name: Token, value: VariableValue, depth: u32, interpreter: *ModuleInterpreter) !void {
    const environment = try self.ancestorAtDepth(depth);

    const pointer = environment.values.getPtr(name.lexeme);
    if (pointer == null) {
        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to assign to {s}, which is undefined.", .{name.lexeme});
        return;
    }

    pointer.?.*.value.deinit(interpreter.allocator);

    pointer.?.*.value = value;
}

pub fn repeat(count: usize, character: u8, allocator: std.mem.Allocator) ![]const u8 {
    const buffer = try allocator.alloc(u8, count);
    for (buffer, 0..) |_, i| {
        buffer[i] = character;
    }
    return buffer;
}
pub fn printVariables(self: *Environment, interpreter: *ModuleInterpreter, indent: u32) !void {
    const spaces = try repeat(indent, ' ', interpreter.allocator);
    defer interpreter.allocator.free(spaces);

    if (indent == 0) std.debug.print("---- Variables -----\n", .{});
    var iter = self.values.iterator();
    while (iter.next()) |entry| {
        const wrapper = entry.value_ptr.*;
        const name = wrapper.name;
        const value = wrapper.value;
        const string = try value.toString(interpreter.allocator);
        defer interpreter.allocator.free(string);

        std.debug.print("{s}{s} = {s}\n", .{ spaces, name, string });
    }

    if (self.parent != null) {
        std.debug.print("{s}->\n", .{spaces});
        try self.parent.?.printVariables(interpreter, indent + 1);
    }
    if (indent == 0) std.debug.print("-----------\n", .{});
}
