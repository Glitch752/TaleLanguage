const GrammarPattern = @import("./grammarPattern.zig").GrammarPattern;
const PatternType = @import("./grammarPattern.zig").PatternType;
const GrammarPatternElement = @import("./grammarPattern.zig").GrammarPatternElement;
const TokenType = @import("../token.zig").TokenType;
const TokenData = @import("../token.zig").TokenData;
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

// fn passSingleASTForward(self: *GrammarPattern, tokens: []TokenType, allocator: std.mem.Allocator) ?*AST {
//     return self.elements[0].Pattern.getAST(self.elements[0].Pattern, tokens, allocator);
// }

// const expression = GrammarPattern.create(

// const letPattern = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{ .{ .Token = TokenType.LetKeyword }, .{ .Token = TokenType.Identifier }, .{ .Token = TokenType.Assign }, .{ .Pattern = expression } });

const statement = GrammarPattern.create(PatternType.OneOf, &[_]GrammarPatternElement{
    // TODO: Add more statements
    // .{ .Pattern = TokenType.FunctionKeyword },
    // .{ .Pattern = TokenType.IfKeyword },
    // .{ .Pattern = TokenType.ElseKeyword },
    // .{ .Pattern = TokenType.ForKeyword },
    // .{ .Pattern = TokenType.ReturnKeyword },
    // .{ .Pattern = TokenType.LetKeyword },
    .{ .Token = TokenType.LetKeyword }, // Testing
}, createStatementAST);
fn createStatementAST(self: GrammarPattern, childASTs: []*AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*AST {
    const allocation = try allocator.create(AST);

    _ = childASTs;

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Literal = .{ .IntLiteral = 10 } }, // TODO: Expression

        .deinit = deinitStatement,
        .print = printStatement,
    };

    _ = self;
    _ = tokens;

    return allocation;
}
fn deinitStatement(self: *AST, allocator: std.mem.Allocator) void {
    allocator.destroy(self);
}
fn printStatement(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}STATEMENT:\n", .{indentString});
    _ = self;
}

// /// Contains the definition of the language's grammar
// const block = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
//     .{ .Token = TokenType.OpenCurly },
//     .{ .Pattern = GrammarPattern.create(PatternType.AtLeastOne, &[_]GrammarPatternElement{
//         .{ .Pattern = statement },
//     }, passSingleASTForward), .getAST = createBlockAST },
//     .{ .Token = TokenType.CloseCurly },
// });
// fn createBlockAST(self: *GrammarPattern, tokens: []TokenType, allocator: std.mem.Allocator) ?*AST {
//     // First and last tokens can be skipped
//     const allocation = try allocator.create(AST);

//     allocation.* = AST{
//         .column = 0,
//         .line = 0,
//         .node = .{ .Block = .{ .statements = null } }, // TODO: Statements

//         .deinit = deinitBlock,
//         .print = printBlock,
//     };

//     _ = self;
//     _ = tokens;

//     return allocation;
// }
// fn deinitBlock(self: *AST, allocator: std.mem.Allocator) void {
//     allocator.free(self.node.Block.statements);
//     allocator.destroy(self);
// }
// fn printBlock(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
//     const indentString = repeat("  ", indent, allocator);
//     defer allocator.free(indentString);

//     try writer.print("{s}BLOCK:\n", .{indentString});
//     // TODO: Print statements
//     _ = self;
// }

pub const grammar: GrammarPattern = GrammarPattern.create(PatternType.AtLeastOne, &[_]GrammarPatternElement{
    .{ .Pattern = &statement },
}, createGrammarAST);
fn createGrammarAST(self: GrammarPattern, patternASTs: []*AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*AST {
    const allocation = try allocator.create(AST);

    _ = patternASTs;

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Block = .{ .statements = &[_]*AST{} } }, // TODO: Statements

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
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}GRAMMAR:\n", .{indentString});
    _ = self;
}
