const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;

const RuntimeError = @import("./interpreter.zig").RuntimeError;
const Interpreter = @import("./interpreter.zig").Interpreter;
const InterpreterError = @import("./interpreter.zig").InterpreterError;

const Token = @import("../token.zig").Token;

pub const Environment = @This();

allocator: std.mem.Allocator,
values: std.StringHashMap(VariableValue),
parent: ?*Environment = null,

pub fn init(allocator: std.mem.Allocator) Environment {
    return .{
        .allocator = allocator,
        .values = std.StringHashMap(VariableValue).init(allocator),
        .parent = null,
    };
}

pub fn createChild(self: *Environment) Environment {
    return .{
        .allocator = self.allocator,
        .values = std.StringHashMap(VariableValue).init(self.allocator),
        .parent = self,
    };
}

pub fn deinit(self: *Environment) void {
    var iter = self.values.iterator();
    while (iter.next()) |entry| {
        entry.value_ptr.deinit(self.allocator);
        self.allocator.free(entry.key_ptr.*);
    }
    self.values.deinit();
}

pub fn define(self: *Environment, name: []const u8, value: VariableValue) !void {
    // We need to copy the name because the string is owned by the parser and will be deallocated
    const copiedName = try self.allocator.dupe(u8, name);
    try self.values.put(copiedName, value);
}

pub fn assign(self: *Environment, name: Token, value: VariableValue, interpreter: *Interpreter) !void {
    if (self.parent != null) {
        try self.parent.?.assign(name, value, interpreter);
        return;
    }

    const entry = self.values.get(name.lexeme);
    if (entry == null) {
        interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to assign to {s}, which is undefined.", .{name.lexeme});
        return;
    }

    try self.values.put(name.lexeme, value);
}

pub fn get(self: *Environment, name: Token, interpreter: *Interpreter) !VariableValue {
    const entry = self.values.get(name.lexeme);
    if (entry != null) return entry.?;

    if (self.parent != null) {
        return try self.parent.?.get(name, interpreter);
    }

    interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Tried to access {s}, which is undefined.", .{name.lexeme});
    return InterpreterError.RuntimeError;
}
