const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    OpenParen, // (
    CloseParen, // )
    OpenCurly, // {
    CloseCurly, // }
    OpenSquare, // [
    CloseSquare, // ]
    Semicolon, // ;

    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equality, // ==
    NotEqual, // !=

    And, // &&
    Or, // ||

    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseXor, // ^

    Plus, // +
    Minus, // -
    Star, // *
    Slash, // /
    Percent, // %
    Negate, // !

    FunctionKeyword, // function
    ClassKeyword, // class
    ThisKeyword, // this
    SuperKeyword, // super

    ExtendingKeyword, // extending
    StaticKeyword, // static

    IfKeyword, // if
    ElseKeyword, // else
    ForKeyword, // for
    WhileKeyword, // while

    TrueKeyword, // true
    FalseKeyword, // false
    NullKeyword, // null

    ReturnKeyword, // return
    BreakKeyword, // break
    ContinueKeyword, // continue
    LetKeyword, // let
    ExportKeyword, // export

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

pub const TokenLiteral = union(enum) {
    NumberLiteral: f64,
    StringLiteral: []const u8,
    Identifier: []const u8,

    False,
    True,
    Null,

    None,

    pub fn toString(self: TokenLiteral, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .NumberLiteral => |number| return try std.fmt.allocPrint(allocator, "{d}", .{number}),
            .StringLiteral => |string| return try std.fmt.allocPrint(allocator, "{s}", .{string}),
            .Identifier => |identifier| return try std.fmt.allocPrint(allocator, "{s}", .{identifier}),
            .False => return try std.fmt.allocPrint(allocator, "False", .{}),
            .True => return try std.fmt.allocPrint(allocator, "True", .{}),
            .Null => return try std.fmt.allocPrint(allocator, "Null", .{}),
            .None => return try std.fmt.allocPrint(allocator, "None", .{}),
        }
    }

    pub fn clone(self: TokenLiteral, allocator: std.mem.Allocator) !TokenLiteral {
        switch (self) {
            .StringLiteral => return .{ .StringLiteral = try allocator.dupe(u8, self.StringLiteral) },
            .Identifier => return .{ .Identifier = try allocator.dupe(u8, self.Identifier) },
            else => return self,
        }
    }
};

pub const Token = struct {
    type: TokenType,
    /// The actual text of the token.
    lexeme: []const u8,
    /// The literal value of the token, if applicable. Somewhat redundant with type, but I find it easier to write the parser when they're separate.
    literal: TokenLiteral,

    position: usize,

    pub fn init(@"type": TokenType, lexeme: []const u8, literal: TokenLiteral, position: usize) Token {
        return .{ .type = @"type", .lexeme = lexeme, .literal = literal, .position = position };
    }

    pub fn clone(self: Token, allocator: std.mem.Allocator) !Token {
        const lexeme = try allocator.dupe(u8, self.lexeme);
        return .{ .type = self.type, .lexeme = lexeme, .literal = try self.literal.clone(allocator), .position = self.position };
    }

    pub fn deinit(self: *const Token, allocator: std.mem.Allocator) void {
        switch (self.literal) {
            .StringLiteral => allocator.free(self.literal.StringLiteral),
            .Identifier => allocator.free(self.literal.Identifier),
            else => {},
        }
    }

    pub fn toString(self: *const Token, allocator: std.mem.Allocator) ![]const u8 {
        const literalString = try self.literal.toString(allocator);
        defer allocator.free(literalString);

        return try std.fmt.allocPrint(allocator, "{s} {s} {s}", .{ self.type.typeNameString(), self.lexeme, literalString });
    }

    pub fn toCondensedString(self: *const Token, allocator: std.mem.Allocator) ![]const u8 {
        switch (self.literal) {
            .None => return try std.fmt.allocPrint(allocator, "{s} ", .{self.type.typeNameString()}),
            else => {},
        }
        const literalString = try self.literal.toString(allocator);
        defer allocator.free(literalString);
        return try std.fmt.allocPrint(allocator, "{s}({s}) ", .{ self.type.typeNameString(), literalString });
    }

    pub const keywordMap = std.StaticStringMap(TokenType).initComptime(.{
        .{ "function", TokenType.FunctionKeyword },
        .{ "if", TokenType.IfKeyword },
        .{ "else", TokenType.ElseKeyword },

        .{ "let", TokenType.LetKeyword },
        .{ "export", TokenType.ExportKeyword },

        .{ "for", TokenType.ForKeyword },
        .{ "while", TokenType.WhileKeyword },

        .{ "true", TokenType.TrueKeyword },
        .{ "false", TokenType.FalseKeyword },
        .{ "null", TokenType.NullKeyword },

        .{ "class", TokenType.ClassKeyword },
        .{ "extending", TokenType.ExtendingKeyword },
        .{ "static", TokenType.StaticKeyword },
        .{ "this", TokenType.ThisKeyword },
        .{ "super", TokenType.SuperKeyword },

        .{ "break", TokenType.BreakKeyword },
        .{ "continue", TokenType.ContinueKeyword },
        .{ "return", TokenType.ReturnKeyword },
    });
    pub const symbolMap = std.StaticStringMap(TokenType).initComptime(.{
        .{ "(", TokenType.OpenParen },
        .{ ")", TokenType.CloseParen },
        .{ "{", TokenType.OpenCurly },
        .{ "}", TokenType.CloseCurly },
        .{ "[", TokenType.OpenSquare },
        .{ "]", TokenType.CloseSquare },

        .{ "<", TokenType.LessThan },
        .{ "<=", TokenType.LessThanEqual },
        .{ ">", TokenType.GreaterThan },
        .{ ">=", TokenType.GreaterThanEqual },
        .{ "=", TokenType.Assign },
        .{ "!=", TokenType.NotEqual },
        .{ "==", TokenType.Equality },

        .{ "+", TokenType.Plus },
        .{ "-", TokenType.Minus },
        .{ "*", TokenType.Star },
        .{ "/", TokenType.Slash },
        .{ "%", TokenType.Percent },

        .{ "&&", TokenType.And },
        .{ "||", TokenType.Or },

        .{ "&", TokenType.BitwiseAnd },
        .{ "|", TokenType.BitwiseOr },
        .{ "^", TokenType.BitwiseXor },

        .{ "!", TokenType.Negate },
        .{ ",", TokenType.Comma },
        .{ ".", TokenType.Dot },
        .{ ";", TokenType.Semicolon },
    });
    pub const maxSymbolLength = findMaxKeyLength(symbolMap);
};

fn findMaxKeyLength(comptime map: std.StaticStringMap(TokenType)) usize {
    var max = 0;
    for (map.keys()) |key| {
        if (key.len > max) {
            max = key.len;
        }
    }
    return max;
}
