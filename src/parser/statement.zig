const std = @import("std");
const Expression = @import("./expression.zig").Expression;
const Token = @import("../token.zig").Token;

pub const Statement = union(enum) {
    Expression: struct { expression: *const Expression },
    Let: struct { name: Token, initializer: *const Expression },

    pub fn uninit(self: *const Statement, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn expression(allocator: std.mem.Allocator, value: *const Expression) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .{ .Expression = .{ .expression = value } };
        return alloc;
    }
    pub fn let(allocator: std.mem.Allocator, name: Token, initializer: *const Expression) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .{ .Let = .{ .name = name, .initializer = initializer } };
        return alloc;
    }
};
