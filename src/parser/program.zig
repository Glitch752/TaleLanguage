const std = @import("std");
const Statement = @import("./statement.zig").Statement;

pub const Program = @This();

// TODO: move file name and contents into this struct?

statements: std.ArrayList(*Statement),

pub fn init(allocator: std.mem.Allocator) Program {
    return .{ .statements = std.ArrayList(*Statement).init(allocator) };
}

pub fn deinit(self: *Program, allocator: std.mem.Allocator, destroy: bool) void {
    for (self.statements.items) |statement| {
        statement.*.uninit(self.statements.allocator);
    }

    self.statements.deinit();

    if (destroy) allocator.destroy(self);
}

pub fn addStatement(self: *Program, statement: *Statement) !void {
    try self.statements.append(statement);
}
