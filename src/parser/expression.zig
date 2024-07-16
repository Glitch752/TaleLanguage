const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const Statement = @import("./statement.zig").Statement;
const std = @import("std");

pub const FunctionExpression = struct {
    parameters: std.ArrayListUnmanaged(Token),
    body: *const Statement,

    pub fn deinit(self: *const FunctionExpression, allocator: std.mem.Allocator) void {
        allocator.free(self.parameters.allocatedSlice());
        self.body.uninit(allocator);
    }
};

pub const ClassExpression = struct {
    methods: std.ArrayListUnmanaged(ClassExpressionMethod),

    pub fn deinit(self: *const ClassExpression, allocator: std.mem.Allocator) void {
        for (self.methods.items) |method| {
            method.deinit(allocator);
        }
        allocator.free(self.methods.allocatedSlice());
    }
};
pub const ClassExpressionMethod = struct {
    static: bool,
    name: Token,
    function: FunctionExpression,

    pub fn new(static: bool, name: Token, function: FunctionExpression) ClassExpressionMethod {
        return .{ .static = static, .name = name, .function = function };
    }

    pub fn deinit(self: *const ClassExpressionMethod, allocator: std.mem.Allocator) void {
        self.function.deinit(allocator);
    }
};

var globalId: u32 = 0;

pub const Expression = struct {
    id: u32,
    value: union(enum) {
        Grouping: struct { expression: *const Expression },
        Literal: struct { value: TokenLiteral },

        Binary: struct { left: *const Expression, operator: Token, right: *const Expression },
        Unary: struct { operator: Token, right: *const Expression },
        Logical: struct { left: *const Expression, operator: Token, right: *const Expression },
        Bitwise: struct { left: *const Expression, operator: Token, right: *const Expression },

        FunctionCall: struct { callee: *const Expression, startToken: Token, arguments: std.ArrayListUnmanaged(*const Expression) },
        Function: FunctionExpression,

        Class: ClassExpression,

        VariableAccess: struct { name: Token },
        VariableAssignment: struct { name: Token, value: *const Expression },

        PropertyAccess: struct { object: *const Expression, name: Token },
        PropertyAssignment: struct { object: *const Expression, name: Token, value: *const Expression },
    },

    pub fn uninit(self: *const Expression, allocator: std.mem.Allocator) void {
        switch (self.*.value) {
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
            .Bitwise => |data| {
                data.left.uninit(allocator);
                data.right.uninit(allocator);
            },

            .FunctionCall => |data| {
                data.callee.uninit(allocator);
                for (data.arguments.items) |argument| {
                    argument.*.uninit(allocator);
                }
                allocator.free(data.arguments.allocatedSlice());
            },
            .Function => |functionExpression| functionExpression.deinit(allocator),
            .VariableAssignment => |data| data.value.uninit(allocator),

            .PropertyAccess => |data| data.object.uninit(allocator),
            .PropertyAssignment => |data| {
                data.object.uninit(allocator);
                data.value.uninit(allocator);
            },

            .Class => |classExpression| classExpression.deinit(allocator),
            else => {},
        }
        allocator.destroy(self);
    }
    pub fn grouping(allocator: std.mem.Allocator, expression: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Grouping = .{ .expression = expression } } };
        return alloc;
    }
    pub fn literal(allocator: std.mem.Allocator, value: TokenLiteral) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Literal = .{ .value = value } } };
        return alloc;
    }

    pub fn unary(allocator: std.mem.Allocator, operator: Token, right: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Unary = .{ .operator = operator, .right = right } } };
        return alloc;
    }
    pub fn binary(allocator: std.mem.Allocator, left: *const Expression, operator: Token, right: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Binary = .{ .left = left, .operator = operator, .right = right } } };
        return alloc;
    }
    pub fn logical(allocator: std.mem.Allocator, left: *const Expression, operator: Token, right: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Logical = .{ .left = left, .operator = operator, .right = right } } };
        return alloc;
    }
    pub fn bitwise(allocator: std.mem.Allocator, left: *const Expression, operator: Token, right: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Bitwise = .{ .left = left, .operator = operator, .right = right } } };
        return alloc;
    }

    pub fn functionCall(allocator: std.mem.Allocator, callee: *const Expression, startToken: Token, arguments: std.ArrayListUnmanaged(*const Expression)) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .FunctionCall = .{ .callee = callee, .startToken = startToken, .arguments = arguments } } };
        return alloc;
    }
    pub fn function(allocator: std.mem.Allocator, functionExpression: FunctionExpression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Function = functionExpression } };
        return alloc;
    }

    pub fn class(allocator: std.mem.Allocator, methods: std.ArrayListUnmanaged(ClassExpressionMethod)) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .Class = .{ .methods = methods } } };
        return alloc;
    }

    pub fn variableAccess(allocator: std.mem.Allocator, name: Token) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .VariableAccess = .{ .name = name } } };
        return alloc;
    }
    pub fn variableAssignment(allocator: std.mem.Allocator, name: Token, value: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .VariableAssignment = .{ .name = name, .value = value } } };
        return alloc;
    }

    pub fn propertyAccess(allocator: std.mem.Allocator, object: *const Expression, name: Token) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .PropertyAccess = .{ .object = object, .name = name } } };
        return alloc;
    }
    pub fn propertyAssignment(allocator: std.mem.Allocator, object: *const Expression, name: Token, value: *const Expression) !*Expression {
        const alloc = try allocator.create(Expression);
        globalId = globalId + 1;
        alloc.* = .{ .id = globalId, .value = .{ .PropertyAssignment = .{ .object = object, .name = name, .value = value } } };
        return alloc;
    }
};
