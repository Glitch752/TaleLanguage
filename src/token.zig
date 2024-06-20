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

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    FunctionKeyword,
    IfKeyword,
    ElseKeyword,
    ForKeyword,
    ReturnKeyword,
    LetKeyword,

    IntLiteral,
    // No floats for now
    StringLiteral,

    Range, // ..
    Comma, // ,
    Dot, // .

    EOF,
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

    Plus: void,
    Minus: void,
    Star: void,
    Slash: void,
    Percent: void,

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
    Dot: void,

    EOF: void,

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
            .Plus => return "+",
            .Minus => return "-",
            .Star => return "*",
            .Slash => return "/",
            .Percent => return "%",
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
            .Dot => return ".",
            .EOF => return "EOF",
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
            .Plus => return "Plus",
            .Minus => return "Minus",
            .Star => return "Star",
            .Slash => return "Slash",
            .Percent => return "Percent",
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
            .Dot => return "Dot",
            .EOF => return "EOF",
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
};

pub const TokenData = struct {
    token: Token,
    line: u32,
    column: u32,

    pub fn deinit(self: *const TokenData, allocator: *const std.mem.Allocator) void {
        switch (self.token) {
            .Identifier => |token| allocator.free(token),
            .StringLiteral => |token| allocator.free(token),
            else => {},
        }
    }
};
