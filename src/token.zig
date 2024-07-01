const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    OpenParen, // (
    CloseParen, // )
    OpenCurly, // {
    CloseCurly, // }
    Semicolon, // ;

    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equality, // ==
    NotEqual, // !=

    And, // &&
    Or, // ||

    Plus, // +
    Minus, // -
    Star, // *
    Slash, // /
    Percent, // %
    Negate, // !

    FunctionKeyword, // function
    ClassKeyword, // class

    SuperKeyword, // super
    ThisKeyword, // this

    IfKeyword, // if
    ElseKeyword, // else
    ForKeyword, // for
    WhileKeyword, // while

    TrueKeyword, // true
    FalseKeyword, // false

    ReturnKeyword, // return
    LetKeyword, // let

    // Literals
    NumberLiteral,
    StringLiteral,
    Identifier,

    Comma, // ,
    Dot, // .

    Assign, // =

    EOF,

    pub fn typeNameString(self: TokenType) []const u8 {
        return @tagName(self);
    }
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,

    position: usize,

    pub fn init(@"type": TokenType, lexeme: []const u8, position: usize) Token {
        return .{ .type = @"type", .lexeme = lexeme, .position = position };
    }

    pub fn deinit(self: *const Token, allocator: *const std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }

    pub fn toString(self: *const Token, allocator: *const std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{s} {s}", .{ self.type.typeNameString(), self.lexeme });
    }

    pub const keywordMap = std.ComptimeStringMap(TokenType, .{
        // This comment is to prevent zigfmt from collapsing the lines
        .{ "function", TokenType.FunctionKeyword },
        .{ "if", TokenType.IfKeyword },
        .{ "else", TokenType.ElseKeyword },
        .{ "for", TokenType.ForKeyword },
        .{ "return", TokenType.ReturnKeyword },
        .{ "let", TokenType.LetKeyword },
        .{ "while", TokenType.WhileKeyword },
        .{ "true", TokenType.TrueKeyword },
        .{ "false", TokenType.FalseKeyword },
        .{ "class", TokenType.ClassKeyword },
        .{ "super", TokenType.SuperKeyword },
        .{ "this", TokenType.ThisKeyword },
    });
    pub const symbolMap = std.ComptimeStringMap(TokenType, .{
        // This comment is to prevent zigfmt from collapsing the lines
        .{ "(", TokenType.OpenParen },
        .{ ")", TokenType.CloseParen },
        .{ "{", TokenType.OpenCurly },
        .{ "}", TokenType.CloseCurly },
        .{ ";", TokenType.Semicolon },
        .{ "<", TokenType.LessThan },
        .{ "<=", TokenType.LessThanEqual },
        .{ ">", TokenType.GreaterThan },
        .{ ">=", TokenType.GreaterThanEqual },
        .{ "=", TokenType.Assign },
        .{ "!=", TokenType.NotEqual },
        .{ "==", TokenType.Equal },
        .{ "+", TokenType.Plus },
        .{ "-", TokenType.Minus },
        .{ "*", TokenType.Star },
        .{ "/", TokenType.Slash },
        .{ "%", TokenType.Percent },
        .{ "&&", TokenType.And },
        .{ "||", TokenType.Or },
        .{ "!", TokenType.Negate },
        .{ "..", TokenType.Range },
        .{ ",", TokenType.Comma },
        .{ ".", TokenType.Dot },
        .{ ":", TokenType.Colon },
    });
    pub const maxSymbolLength = symbolMap.kvs[symbolMap.kvs.len - 1].key.len;
};
