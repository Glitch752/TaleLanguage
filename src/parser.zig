const TokenData = @import("token.zig").TokenData;
const std = @import("std");
const AST = @import("ast.zig").AST;
const FunctionParameter = @import("ast.zig").FunctionParameter;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const pretty_error = @import("main.zig").pretty_error;
const initParserPatterns = @import("parser/grammar.zig").initParserPatterns;
const repeat = @import("parser/grammar.zig").repeat;
const ArgsFlags = @import("args_parser.zig").ArgsFlags;
const GrammarPattern = @import("parser/grammarPattern.zig").GrammarPattern;

const Parser = @This();

tokens: []TokenData,
allocator: std.mem.Allocator,
position: usize,
file_name: []const u8,
ast: ?*const AST,
flags: ArgsFlags,
parserPatterns: std.StringHashMap(GrammarPattern),

pub const ParseError = error{Unknown};

pub fn init(tokens: []TokenData, flags: ArgsFlags, allocator: std.mem.Allocator, file_name: []const u8) !Parser {
    return .{
        .tokens = tokens,
        .flags = flags,
        .allocator = allocator,
        .position = 0,
        .file_name = file_name,
        .ast = null,
        .parserPatterns = try initParserPatterns(allocator),
    };
}

pub fn parse(self: *Parser) anyerror!?*const AST {
    const rootPattern = self.parserPatterns.get("root") orelse return ParseError.Unknown;
    const result = try rootPattern.consumeIfExist(self.flags, self.tokens, self.allocator, &self.parserPatterns);
    if (result == null) {
        try pretty_error("Failed to parse grammar -- No AST consumed initially");
        return ParseError.Unknown;
    }
    if (result.?.asts == null) {
        try pretty_error("Failed to parse grammar -- No ASTs consumed");
        return ParseError.Unknown;
    }
    defer self.allocator.free(result.?.asts.?);

    self.ast = result.?.asts.?[0];
    return self.ast;
}

pub fn deinit(self: *Parser) void {
    if (self.ast != null) {
        self.ast.?.*.deinit(self.ast.?, self.allocator);
    }

    self.parserPatterns.deinit();
}

fn parseType(self: *Parser) !*AST {
    var typePointer = try self.allocator.create(AST);
    errdefer typePointer.deinit(self.allocator);

    // TODO: real types
    // For now, just store everything in the parentheses
    var parentheses: usize = 0;
    var string = try self.allocator.alloc(u8, 0);

    errdefer {
        self.allocator.free(string);
    }

    while (self.tokens[self.position].token == Token.OpenParen and self.position < self.tokens.len) {
        parentheses += 1;
        self.position += 1;
        try self.ensurePositionInBounds("Type declaration - open parenthesis");

        string = try self.allocator.realloc(string, parentheses);
        string[parentheses - 1] = '(';
    }
    while (parentheses > 0 and self.position < self.tokens.len) {
        if (self.tokens[self.position].token == Token.CloseParen) {
            parentheses -= 1;
        } else if (self.tokens[self.position].token == Token.OpenParen) {
            parentheses += 1;
        }

        const tokenString = self.tokens[self.position].token.toString();
        string = try self.allocator.realloc(string, string.len + tokenString.len);
        for (tokenString, 0..) |c, i| {
            string[string.len - tokenString.len + i] = c;
        }

        if (parentheses <= 0) break;

        self.position += 1;
        try self.ensurePositionInBounds("Type declaration - parentheses");
    }

    self.position += 1;
    try self.ensurePositionInBounds("Type declaration - close parenthesis");

    typePointer.* = .{
        .line = self.tokens[self.position].line,
        .column = self.tokens[self.position].column,
        .node = .{ .Type = .{
            .identifier = string,
        } },

        .deinit = *deinitType,
        .print = *printType,
    };
    return typePointer;
}

fn deinitType(self: *AST, allocator: std.mem.Allocator) void {
    allocator.free(self.node.Type.identifier);
    allocator.destroy(self);
}

fn printType(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = repeat("  ", indent, allocator);
    defer allocator.free(indentString);
    try writer.print("{s}{s}\n", .{ indentString, self.node.Type.identifier });
}
