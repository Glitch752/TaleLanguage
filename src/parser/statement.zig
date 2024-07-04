const std = @import("std");
const Expression = @import("./expression.zig").Expression;
const Token = @import("../token.zig").Token;

pub const Statement = union(enum) {
    Expression: struct { expression: *const Expression },
    Let: struct { name: Token, initializer: *const Expression },
    Block: struct { statements: std.ArrayList(*Statement) },

    // Control flow
    If: struct { condition: *const Expression, trueBranch: *const Statement, falseBranch: ?*const Statement },
    While: struct { condition: *const Expression, body: *const Statement },

    Return: struct { value: *const Expression },
    Break,
    Continue,

    pub fn uninit(self: *const Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Expression => |data| data.expression.uninit(allocator),
            .Let => |data| data.initializer.uninit(allocator),
            .Block => |data| {
                for (data.statements.items) |statement| {
                    statement.*.uninit(allocator);
                }
                data.statements.deinit();
            },

            .If => |data| {
                data.condition.uninit(allocator);
                data.trueBranch.uninit(allocator);
                if (data.falseBranch != null) {
                    data.falseBranch.?.uninit(allocator);
                }
            },
            .While => |data| {
                data.condition.uninit(allocator);
                data.body.uninit(allocator);
            },

            .Return => |data| data.value.uninit(allocator),
            .Break => {},
            .Continue => {},
        }

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
    pub fn block(allocator: std.mem.Allocator, statements: std.ArrayList(*Statement)) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .{ .Block = .{ .statements = statements } };
        return alloc;
    }

    pub fn ifBlock(allocator: std.mem.Allocator, condition: *const Expression, trueBranch: *const Statement, falseBranch: ?*const Statement) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .{ .If = .{ .condition = condition, .trueBranch = trueBranch, .falseBranch = falseBranch } };
        return alloc;
    }
    pub fn whileBlock(allocator: std.mem.Allocator, condition: *const Expression, body: *const Statement) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .{ .While = .{ .condition = condition, .body = body } };
        return alloc;
    }

    pub fn returnStatement(allocator: std.mem.Allocator, value: *const Expression) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .{ .Return = .{ .value = value } };
        return alloc;
    }
    pub fn breakStatement(allocator: std.mem.Allocator) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .Break;
        return alloc;
    }
    pub fn continueStatement(allocator: std.mem.Allocator) !*Statement {
        const alloc = try allocator.create(Statement);
        alloc.* = .Continue;
        return alloc;
    }
};
