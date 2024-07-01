const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;

pub const Expression = union(enum) {
    Binary: struct {
        left: *const Expression,
        operator: Token,
        right: *const Expression,

        pub fn init(left: *const Expression, operator: Token, right: *const Expression) Expression {
            return .{ .Binary = .{ .left = left, .operator = operator, .right = right } };
        }
    },

    Grouping: struct {
        expression: *const Expression,

        pub fn init(expression: *const Expression) Expression {
            return .{ .Grouping = .{ .expression = expression } };
        }
    },

    Literal: struct {
        value: TokenLiteral,

        pub fn init(value: TokenLiteral) Expression {
            return .{ .Literal = .{ .value = value } };
        }
    },

    Unary: struct {
        operator: Token,
        right: *const Expression,

        pub fn init(operator: Token, right: *const Expression) Expression {
            return .{ .Unary = .{ .operator = operator, .right = right } };
        }
    },
};
