const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const std = @import("std");

pub const Expression = union(enum) {
    Grouping: struct { expression: *const Expression },
    Literal: struct { value: TokenLiteral },

    Binary: struct { left: *const Expression, operator: Token, right: *const Expression },
    Unary: struct { operator: Token, right: *const Expression },
    Logical: struct { left: *const Expression, operator: Token, right: *const Expression },

    FunctionCall: struct { callee: *const Expression, startToken: Token, arguments: std.ArrayList(*const Expression) },

    VariableAccess: struct { name: Token },
    VariableAssignment: struct { name: Token, value: *const Expression },

    pub fn uninit(self: *const Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Binary => |data| {
                data.left.uninit(allocator);
                data.right.uninit(allocator);
            },
            .Grouping => |data| data.expression.uninit(allocator),
            .Unary => |data| data.right.uninit(allocator),
            .Logical => |data| {
                data.left.uninit(allocator);
                data.right.uninit(allocator);
            },

            .FunctionCall => |data| {
                data.callee.uninit(allocator);
                for (data.arguments.items) |argument| {
                    argument.*.uninit(allocator);
                }
                data.arguments.deinit();
            },

            .VariableAssignment => |data| data.value.uninit(allocator),
            else => {},
        }
        allocator.destroy(self);
    }
    pub fn grouping(allocator: std.mem.Allocator, expression: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .Grouping = .{ .expression = expression } };
        return alloc;
    }
    pub fn literal(allocator: std.mem.Allocator, value: TokenLiteral) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .Literal = .{ .value = value } };
        return alloc;
    }

    pub fn unary(allocator: std.mem.Allocator, operator: Token, right: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .Unary = .{ .operator = operator, .right = right } };
        return alloc;
    }
    pub fn binary(allocator: std.mem.Allocator, left: *const Expression, operator: Token, right: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .Binary = .{ .left = left, .operator = operator, .right = right } };
        return alloc;
    }
    pub fn logical(allocator: std.mem.Allocator, left: *const Expression, operator: Token, right: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .Logical = .{ .left = left, .operator = operator, .right = right } };
        return alloc;
    }

    pub fn functionCall(allocator: std.mem.Allocator, callee: *const Expression, startToken: Token, arguments: std.ArrayList(*const Expression)) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .FunctionCall = .{ .callee = callee, .startToken = startToken, .arguments = arguments } };
        return alloc;
    }

    pub fn variableAccess(allocator: std.mem.Allocator, name: Token) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .VariableAccess = .{ .name = name } };
        return alloc;
    }
    pub fn variableAssignment(allocator: std.mem.Allocator, name: Token, value: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        alloc.* = .{ .VariableAssignment = .{ .name = name, .value = value } };
        return alloc;
    }
};
