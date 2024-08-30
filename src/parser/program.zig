const std = @import("std");
const Statement = @import("./statement.zig").Statement;

pub const Program = @This();

statements: std.ArrayList(*Statement),

pub fn init(allocator: std.mem.Allocator) Program {
    return .{ .statements = std.ArrayList(*Statement).init(allocator) };
}

pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
    for (self.statements.items) |statement| {
        statement.*.uninit(self.statements.allocator);
    }

    self.statements.deinit();

    allocator.destroy(self);
}

pub fn addStatement(self: *Program, statement: *Statement) !void {
    try self.statements.append(statement);
}
