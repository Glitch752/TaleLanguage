const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;

pub const Expression = union(enum) {
    Binary: struct { left: *const Expression, operator: Token, right: *const Expression },
    Grouping: struct { expression: *const Expression },
    Literal: struct { value: TokenLiteral },
    Unary: struct { operator: Token, right: *const Expression },

    pub fn binary(left: *const Expression, operator: Token, right: *const Expression) Expression {
        return .{ .Binary = .{ .left = left, .operator = operator, .right = right } };
    }
    pub fn grouping(expression: *const Expression) Expression {
        return .{ .Grouping = .{ .expression = expression } };
    }
    pub fn literal(value: TokenLiteral) Expression {
        return .{ .Literal = .{ .value = value } };
    }
    pub fn unary(operator: Token, right: *const Expression) Expression {
        return .{ .Unary = .{ .operator = operator, .right = right } };
    }
};
