const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;

const RuntimeError = @import("./interpreter.zig").RuntimeError;
const InterpreterError = @import("./interpreter.zig").InterpreterError;

const Token = @import("../token.zig").Token;

pub const Environment = @This();

allocator: std.mem.Allocator,
values: std.HashMap([]const u8, VariableValue),

pub fn init(allocator: std.mem.Allocator) Environment {
    return .{
        .allocator = allocator,
        .values = std.HashMap([]const u8, VariableValue).init(allocator),
    };
}

pub fn deinit(self: *Environment) void {
    self.values.deinit();
}

pub fn define(self: *Environment, name: []const u8, value: VariableValue) void {
    self.values.put(name, value);
}

pub fn get(self: *Environment, name: Token, runtimeError: *?RuntimeError) !VariableValue {
    const entry = self.values.get(name.lexeme);
    if (entry == null) {
        runtimeError.* = RuntimeError.tokenError(runtimeError, "Undefined variable", name);
        return InterpreterError.RuntimeError;
    }
    return entry.*;
}
