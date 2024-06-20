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

    pub fn toStringWithType(self: Token) []const u8 {
        return self.toString() ++ " (" ++ @typeName(@TypeOf(self)) ++ ")";
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
