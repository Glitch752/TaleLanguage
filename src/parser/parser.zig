const std = @import("std");

const TokenType = @import("../token.zig").TokenType;
const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;

const ArgsFlags = @import("../args_parser.zig").ArgsFlags;

const Expression = @import("./expression.zig").Expression;
const ASTPrinter = @import("./ast_printer.zig").ASTPrinter;

const Parser = @This();

const prettyError = @import("../errors.zig").prettyError;
const errorContext = @import("../errors.zig").errorContext;

tokens: []Token,
current: usize = 0,

originalBuffer: []const u8,
fileName: []const u8,

allocator: std.mem.Allocator,
flags: ArgsFlags,

/// Errors in Zig can't hold payloads, so we separate the actual error data from the error type.
const ParseError = struct { @"error": union(enum) { ConsumeFailed: struct {
    string: []const u8,
    token: Token,
} } };

const ParseErrorEnum = error{
    Unknown,
};

pub fn init(tokens: []Token, fileName: []const u8, originalBuffer: []const u8, flags: ArgsFlags, allocator: std.mem.Allocator) !Parser {
    return .{ .tokens = tokens, .fileName = fileName, .originalBuffer = originalBuffer, .flags = flags, .allocator = allocator };
}

pub fn parse(self: *Parser) anyerror!Expression {
    const expression = try self.consumeExpression();

    if (self.flags.debugAST) {
        const printer = ASTPrinter.init(self.allocator);
        try printer.print(expression);
    }

    return expression;
}

pub fn deinit(self: *Parser) void {
    _ = self;
}

fn isAtEnd(self: *Parser) bool {
    return self.current >= self.tokens.len;
}

fn peek(self: *Parser) Token {
    return self.tokens[self.current];
}

fn peekPrevious(self: *Parser) Token {
    return self.tokens[self.current - 1];
}

fn advance(self: *Parser) Token {
    if (!self.isAtEnd()) {
        self.current += 1;
    }
    return self.peekPrevious();
}

fn matchToken(self: *Parser, @"type": TokenType) bool {
    if (self.isAtEnd()) {
        return false;
    }
    if (self.peek().type == @"type") {
        self.advance();
        return true;
    }
    return false;
}

/// Attempts to consume a token of the given type. If the token is not of the given type, returns an error.
fn consume(self: *Parser, @"type": TokenType, errorMessage: []const u8) !Token {
    if (self.check(@"type")) {
        return self.advance();
    }

    prettyError(self.allocator, errorMessage);
    errorContext(self.originalBuffer, self.fileName, self.peek().position, self.allocator);

    const err = ParseError{ .@"error" = .{.ConsumeFailed{
        .string = errorMessage,
        .token = self.peek(),
    }} };
    err.print(self.allocator);
    return ParseErrorEnum.Unknown;
}

// Grammar rules

fn consumeExpression(self: *Parser) !Expression {
    return try self.consumeEquality();
}

fn consumeEquality(self: *Parser) !Expression {
    var expression = try self.consumeComparison();

    while (self.matchToken(TokenType.NotEqual) or self.matchToken(TokenType.Equality)) {
        const operator = self.peekPrevious();
        const right = try self.consumeComparison();
        expression = Expression.binary(expression, operator, right);
    }

    return expression;
}

fn consumeComparison(self: *Parser) !Expression {
    var expression = try self.consumeTerm();

    while (self.matchToken(TokenType.GreaterThan) or
        self.matchToken(TokenType.GreaterThanEqual) or
        self.matchToken(TokenType.LessThan) or
        self.matchToken(TokenType.LessThanEqual))
    {
        const operator = self.peekPrevious();
        const right = try self.consumeTerm();
        expression = Expression.binary(expression, operator, right);
    }

    return expression;
}

fn consumeTerm(self: *Parser) !Expression {
    var expression = try self.consumeFactor();

    while (self.matchToken(TokenType.Minus) or self.matchToken(TokenType.Plus)) {
        const operator = self.peekPrevious();
        const right = try self.consumeFactor();
        expression = Expression.binary(expression, operator, right);
    }

    return expression;
}

fn consumeFactor(self: *Parser) !Expression {
    var expression = try self.consumeUnary();

    while (self.matchToken(TokenType.Slash) or self.matchToken(TokenType.Star)) {
        const operator = self.peekPrevious();
        const right = try self.consumeUnary();
        expression = Expression.binary(expression, operator, right);
    }

    return expression;
}

fn consumeUnary(self: *Parser) !Expression {
    if (self.matchToken(TokenType.Negate) or self.matchToken(TokenType.Minus)) {
        const operator = self.peekPrevious();
        const right = try self.consumeUnary();
        return Expression.unary(operator, right);
    }

    return try self.consumePrimary();
}

fn consumePrimary(self: *Parser) !Expression {
    if (self.matchToken(TokenType.FalseKeyword)) {
        return Expression.literal(TokenLiteral.False);
    }
    if (self.matchToken(TokenType.TrueKeyword)) {
        return Expression.literal(TokenLiteral.True);
    }
    if (self.matchToken(TokenType.NullKeyword)) {
        return Expression.literal(TokenLiteral.Null);
    }

    if (self.matchToken(TokenType.NumberLiteral) or self.matchToken(TokenType.StringLiteral)) { // or self.matchToken(TokenType.Identifier)
        return Expression.literal(self.peekPrevious().literal);
    }

    if (self.matchToken(TokenType.LeftParen)) {
        const expression = try self.consumeExpression();
        if (!self.matchToken(TokenType.RightParen)) {
            return error.ParseError.Unknown;
        }
        return Expression.grouping(expression);
    }

    return ParseErrorEnum.Unknown;
}

// Error handling

fn synchronize(self: *Parser) void {
    self.advance();

    while (!self.isAtEnd()) {
        if (self.peekPrevious().type == TokenType.Semicolon) {
            return;
        }

        switch (self.peek().type) {
            .ClassKeyword => return,
            .FunctionKeyword => return,
            .LetKeyword => return,
            .IfKeyword => return,
            .WhileKeyword => return,
            .ForKeyword => return,
            .ReturnKeyword => return,
            else => {},
        }

        self.advance();
    }
}
