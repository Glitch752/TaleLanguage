const std = @import("std");

pub const TokenType = enum {
    Identifier,

    OpenParen, // (
    CloseParen, // )

    OpenSquare, // [
    CloseSquare, // ]

    OpenCurly, // {
    CloseCurly, // }

    SemiColon,

    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal, // ==
    NotEqual, // !=

    And, // &&
    Or, // ||

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Negate, // !

    FunctionKeyword,
    IfKeyword,
    ElseKeyword,
    ForKeyword,
    // TODO: While
    ReturnKeyword,
    // TODO: Continue, Break
    LetKeyword,

    IntLiteral,
    // No floats for now
    StringLiteral,

    Range, // ..
    Comma, // ,
    Colon, // :
    Dot, // .

    Assign, // =

    EOF,
    Error,
};

pub const Token = union(TokenType) {
    Identifier: []const u8,

    OpenParen: void,
    CloseParen: void,

    OpenSquare: void,
    CloseSquare: void,

    OpenCurly: void,
    CloseCurly: void,

    SemiColon: void,

    LessThan: void,
    LessThanEqual: void,
    GreaterThan: void,
    GreaterThanEqual: void,
    Equal: void,
    NotEqual: void,

    And: void,
    Or: void,

    Plus: void,
    Minus: void,
    Star: void,
    Slash: void,
    Percent: void,
    Negate: void,

    FunctionKeyword: void,
    IfKeyword: void,
    ElseKeyword: void,
    ForKeyword: void,
    ReturnKeyword: void,
    LetKeyword: void,

    IntLiteral: u64,
    StringLiteral: []const u8,

    Range: void,
    Comma: void,
    Colon: void,
    Dot: void,

    Assign: void,

    // Special tokens
    EOF: void,
    Error: []const u8,

    pub fn toString(self: Token) []const u8 {
        return switch (self) {
            .Identifier => |token| token,

            .OpenParen => return "(",
            .CloseParen => return ")",
            .OpenSquare => return "[",
            .CloseSquare => return "]",
            .OpenCurly => return "{",
            .CloseCurly => return "}",

            .SemiColon => return ";",

            .LessThan => return "<",
            .LessThanEqual => return "<=",
            .GreaterThan => return ">",
            .GreaterThanEqual => return ">=",
            .Equal => return "==",
            .NotEqual => return "!=",

            .And => return "&&",
            .Or => return "||",

            .Plus => return "+",
            .Minus => return "-",
            .Star => return "*",
            .Slash => return "/",
            .Percent => return "%",
            .Negate => return "!",

            .FunctionKeyword => return "function",
            .IfKeyword => return "if",
            .ElseKeyword => return "else",
            .ForKeyword => return "for",
            .ReturnKeyword => return "return",
            .LetKeyword => return "let",

            .IntLiteral => |token| {
                var buf: [21]u8 = undefined;
                _ = std.fmt.formatIntBuf(buf[0..], token, 10, .lower, .{});
                return buf[0..];
            },
            .StringLiteral => |token| token,

            .Range => return "..",
            .Comma => return ",",
            .Colon => return ":",
            .Dot => return ".",
            .Assign => return "=",

            .EOF => return "EOF",
            .Error => |message| message,
        };
    }

    pub fn typeNameString(self: Token) []const u8 {
        return switch (self) {
            .Identifier => return "Identifier",

            .OpenParen => return "OpenParen",
            .CloseParen => return "CloseParen",
            .OpenSquare => return "OpenSquare",
            .CloseSquare => return "CloseSquare",
            .OpenCurly => return "OpenCurly",
            .CloseCurly => return "CloseCurly",

            .SemiColon => return "SemiColon",

            .LessThan => return "LessThan",
            .LessThanEqual => return "LessThanEqual",
            .GreaterThan => return "GreaterThan",
            .GreaterThanEqual => return "GreaterThanEqual",
            .Equal => return "Equal",
            .NotEqual => return "NotEqual",

            .And => return "And",
            .Or => return "Or",

            .Plus => return "Plus",
            .Minus => return "Minus",
            .Star => return "Star",
            .Slash => return "Slash",
            .Percent => return "Percent",
            .Negate => return "Negate",

            .FunctionKeyword => return "FunctionKeyword",
            .IfKeyword => return "IfKeyword",
            .ElseKeyword => return "ElseKeyword",
            .ForKeyword => return "ForKeyword",
            .ReturnKeyword => return "ReturnKeyword",
            .LetKeyword => return "LetKeyword",

            .IntLiteral => return "IntLiteral",
            .StringLiteral => return "StringLiteral",

            .Range => return "Range",
            .Comma => return "Comma",
            .Colon => return "Colon",
            .Assign => return "Assign",
            .Dot => return "Dot",

            .EOF => return "EOF",
            .Error => return "Error",
        };
    }

    pub fn toStringWithType(self: Token, allocator: std.mem.Allocator) ![]const u8 {
        const tokenString = self.toString();
        const typeName = self.typeNameString();
        const buffer = try allocator.alloc(u8, tokenString.len + typeName.len + 3);
        for (typeName, 0..) |c, i| {
            buffer[i] = c;
        }
        buffer[typeName.len] = '(';
        for (tokenString, 0..) |c, i| {
            buffer[typeName.len + 1 + i] = c;
        }
        buffer[tokenString.len + 1 + typeName.len] = ')';
        buffer[tokenString.len + 2 + typeName.len] = ' ';
        return buffer;
    }

    pub const keyword_map = std.ComptimeStringMap(Token, .{ .{ "function", Token.FunctionKeyword }, .{ "if", Token.IfKeyword }, .{ "else", Token.ElseKeyword }, .{ "for", Token.ForKeyword }, .{ "return", Token.ReturnKeyword }, .{ "let", Token.LetKeyword } });
    pub const symbol_map = std.ComptimeStringMap(Token, .{ .{ "(", Token.OpenParen }, .{ ")", Token.CloseParen }, .{ "[", Token.OpenSquare }, .{ "]", Token.CloseSquare }, .{ "{", Token.OpenCurly }, .{ "}", Token.CloseCurly }, .{ ";", Token.SemiColon }, .{ "<", Token.LessThan }, .{ "<=", Token.LessThanEqual }, .{ ">", Token.GreaterThan }, .{ ">=", Token.GreaterThanEqual }, .{ "=", Token.Assign }, .{ "!=", Token.NotEqual }, .{ "==", Token.Equal }, .{ "+", Token.Plus }, .{ "-", Token.Minus }, .{ "*", Token.Star }, .{ "/", Token.Slash }, .{ "%", Token.Percent }, .{ "&&", Token.And }, .{ "||", Token.Or }, .{ "!", Token.Negate }, .{ "..", Token.Range }, .{ ",", Token.Comma }, .{ ".", Token.Dot }, .{ ":", Token.Colon } });
    pub const maxSymbolLength = 2;
};

pub const TokenData = struct {
    token: Token,
    line: usize,
    column: usize,

    pub fn deinit(self: *const TokenData, allocator: *const std.mem.Allocator) void {
        switch (self.token) {
            .Identifier => |token| allocator.free(token),
            .StringLiteral => |token| allocator.free(token),
            .Error => |message| allocator.free(message),
            else => {},
        }
    }
};
