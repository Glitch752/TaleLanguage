const Expression = @import("./expression.zig").Expression;
const std = @import("std");

pub const ASTPrinter = @This();

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) ASTPrinter {
    return .{
        .allocator = allocator,
    };
}

pub fn print(self: *const ASTPrinter, expression: Expression) !void {
    switch (expression) {
        .Binary => |values| {
            std.debug.print("(", .{});
            try self.print(values.left.*);
            std.debug.print(" ", .{});
            std.debug.print("{s}", .{values.operator.lexeme});
            std.debug.print(" ", .{});
            try self.print(values.right.*);
            std.debug.print(")", .{});
        },
        .Grouping => |values| {
            std.debug.print("[", .{});
            try self.print(values.expression.*);
            std.debug.print("]", .{});
        },
        .Literal => |values| {
            const str = try values.value.toString(self.allocator);
            defer self.allocator.free(str);
            std.debug.print("{s}", .{str});
        },
        .Unary => |values| {
            std.debug.print("(", .{});
            std.debug.print("{s}", .{values.operator.lexeme});
            try self.print(values.right.*);
            std.debug.print(")", .{});
        },
    }
}
