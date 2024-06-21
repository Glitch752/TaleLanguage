const GrammarPattern = @import("./grammarPattern.zig").GrammarPattern;
const GrammarPatternElement = @import("./grammarPattern.zig").GrammarPatternElement;
const TokenType = @import("../token.zig").TokenType;
const AST = @import("../ast.zig").AST;
const std = @import("std");
const oneOf = @import("./grammarPattern.zig").oneOf;
const atLeastOne = @import("./grammarPattern.zig").atLeastOne;

pub fn repeat(s: []const u8, times: usize, allocator: std.mem.Allocator) ![]u8 {
    const repeated = try allocator.alloc(u8, s.len * times);

    var i: usize = 0;
    while (i < s.len * times) : (i += 1) {
        repeated[i] = s[i % 2];
    }

    return repeated;
}

const statement = GrammarPattern.create(&[_]GrammarPatternElement{
    oneOf(&[_]GrammarPatternElement{
        // TODO: Add more statements
        // .{ .Pattern = TokenType.FunctionKeyword },
        // .{ .Pattern = TokenType.IfKeyword },
        // .{ .Pattern = TokenType.ElseKeyword },
        // .{ .Pattern = TokenType.ForKeyword },
        // .{ .Pattern = TokenType.ReturnKeyword },
        .{ .Pattern = TokenType.LetKeyword },
    }),
}, createStatementAST);
fn createStatementAST(self: *GrammarPattern, tokens: []TokenType, allocator: std.mem.Allocator) ?*AST {
    return self.elements[0].Pattern.getAST(self.elements[0].Pattern, tokens, allocator);
}

/// Contains the definition of the language's grammar
const block = GrammarPattern.create(&[_]GrammarPatternElement{
    .{ .Token = TokenType.OpenCurly },
    atLeastOne(statement),
    .{ .Token = TokenType.CloseCurly },
}, createBlockAST);
fn createBlockAST(self: *GrammarPattern, tokens: []TokenType, allocator: std.mem.Allocator) ?*AST {
    // First and last tokens can be skipped
    const allocation = try allocator.create(AST);

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Block = .{ .statements = null } }, // TODO: Statements

        .deinit = deinitBlock,
        .print = printBlock,
    };

    _ = self;
    _ = tokens;

    return allocation;
}
fn deinitBlock(self: *AST, allocator: std.mem.Allocator) void {
    allocator.free(self.node.Block.statements);
    allocator.destroy(self);
}
fn printBlock(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}BLOCK:\n", .{indentString});
    // TODO: Print statements
    _ = self;
}

pub const grammar: GrammarPattern = GrammarPattern.create(&[_]GrammarPatternElement{atLeastOne(statement)}, createGrammarAST);
fn createGrammarAST(self: *GrammarPattern, tokens: []TokenType, allocator: std.mem.Allocator) ?*AST {
    const allocation = try allocator.create(AST);

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Block = .{ .statements = null } }, // TODO: Statements

        .deinit = deinitGrammar,
        .print = printGrammar,
    };

    _ = self;
    _ = tokens;

    return allocation;
}
fn deinitGrammar(self: *AST, allocator: std.mem.Allocator) void {
    allocator.free(self.node.Block.statements);
    allocator.destroy(self);
}
fn printGrammar(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}GRAMMAR:\n", .{indentString});
    _ = self;
}
