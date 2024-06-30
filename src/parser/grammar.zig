const GrammarPattern = @import("./grammarPattern.zig").GrammarPattern;
const PatternType = @import("./grammarPattern.zig").PatternType;
const GrammarPatternElement = @import("./grammarPattern.zig").GrammarPatternElement;
const TokenType = @import("../token.zig").TokenType;
const TokenData = @import("../token.zig").TokenData;
const AST = @import("../ast.zig").AST;
const std = @import("std");
const oneOf = @import("./grammarPattern.zig").oneOf;
const atLeastOne = @import("./grammarPattern.zig").atLeastOne;
const eliminateLeftRecursion = @import("./leftRecursionElimination.zig").eliminateLeftRecursion;

pub fn repeat(s: []const u8, times: usize, allocator: std.mem.Allocator) ![]u8 {
    const repeated = try allocator.alloc(u8, s.len * times);

    var i: usize = 0;
    while (i < s.len * times) : (i += 1) {
        repeated[i] = s[i % 2];
    }

    return repeated;
}

fn deinitBlock(self: *const AST, allocator: std.mem.Allocator) void {
    for (self.node.Block.statements) |stmt| {
        stmt.deinit(stmt, allocator);
    }
    allocator.free(self.node.Block.statements);
    allocator.destroy(self);
}

fn printBlock(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}BLOCK:\n", .{indentString});
    for (self.node.Block.statements) |stmt| {
        try stmt.print(stmt.*, writer, indent + 1, allocator);
    }
}

const typeArray = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
    .{ .type = .{ .Token = TokenType.OpenParen }, .debugName = "Type array paren start" },
    .{ .type = .{ .PatternId = "typeInnerPattern" }, .debugName = "Type inner type" },
    .{ .type = .{ .Token = TokenType.CloseParen }, .debugName = "Type array paren end" },
    .{ .type = .{ .Token = TokenType.OpenSquare }, .debugName = "Type array start" },
    .{ .type = .{ .Token = TokenType.CloseSquare }, .debugName = "Type array end" },
}, createTypeArrayAST, "Type array pattern");
fn createTypeArrayAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    const allocation = try allocator.create(AST);
    defer patternASTs[0].deinit(patternASTs[0], allocator);

    const innerString = try allocator.alloc(u8, patternASTs[0].node.Type.identifier.len + 2);
    for (patternASTs[0].node.Type.identifier, 0..patternASTs[0].node.Type.identifier.len) |c, i| {
        innerString[i] = c;
    }
    innerString[patternASTs[0].node.Type.identifier.len] = '[';
    innerString[patternASTs[0].node.Type.identifier.len + 1] = ']';

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Type = .{ .identifier = innerString } },

        .deinit = deinitTypeArray,
        .print = printTypeArray,
    };

    _ = self;
    _ = tokens;

    return allocation;
}
fn deinitTypeArray(self: *const AST, allocator: std.mem.Allocator) void {
    allocator.free(self.node.Type.identifier);
    allocator.destroy(self);
}
fn printTypeArray(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);
    try writer.print("{s}TYPE ARRAY:\n", .{indentString});
    try writer.print("{s}  {s}\n", .{ indentString, self.node.Type.identifier });
}

const typeInnerPattern = GrammarPattern.create(PatternType.OneOf, &[_]GrammarPatternElement{
    .{ .type = .{ .PatternId = "typeArray" }, .debugName = "Type inner array" },
    .{ .type = .{ .Token = TokenType.Identifier }, .debugName = "Type inner identifier" },
    .{ .type = .{ .PatternId = "typePattern" }, .debugName = "Type inner type" },
}, createInnerTypeAST, "Type inner pattern");
fn createInnerTypeAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    // Pass the AST forward if it's already created, otherwise create a new with the token data
    if (patternASTs.len > 0) {
        return patternASTs[0];
    }

    const allocation = try allocator.create(AST);

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Type = .{ .identifier = tokens[0].token.Identifier } },

        .deinit = deinitIdentifierType,
        .print = printIdentifierType,
    };

    _ = self;
    return allocation;
}
fn deinitIdentifierType(self: *const AST, allocator: std.mem.Allocator) void {
    allocator.destroy(self);
}
fn printIdentifierType(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);
    try writer.print("{s}IDENTIFIER TYPE:\n", .{indentString});
    try writer.print("{s}  {s}\n", .{ indentString, self.node.Type.identifier });
}

const typePattern = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
    .{ .type = .{ .Token = TokenType.OpenParen }, .debugName = "Type start" },
    .{ .type = .{ .PatternId = "typeInnerPattern" }, .debugName = "Type inner" },
    .{ .type = .{ .Token = TokenType.CloseParen }, .debugName = "Type end" },
}, createTypeAST, "Type pattern");
fn createTypeAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    const allocation = try allocator.create(AST);

    const innerString = try allocator.alloc(u8, tokens[1].token.Identifier.len + 2);
    innerString[0] = '(';
    for (tokens[1].token.Identifier, 0..tokens[1].token.Identifier.len) |c, i| {
        innerString[i + 1] = c;
    }
    innerString[tokens[1].token.Identifier.len + 1] = ')';

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Type = .{ .identifier = innerString } },

        .deinit = deinitType,
        .print = printType,
    };

    _ = self;
    _ = patternASTs;

    return allocation;
}
fn deinitType(self: *const AST, allocator: std.mem.Allocator) void {
    allocator.free(self.node.Type.identifier);
    allocator.destroy(self);
}
fn printType(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);
    try writer.print("{s}TYPE:\n", .{indentString});
    try writer.print("{s}  {s}\n", .{ indentString, self.node.Type.identifier });
}

const expressionPattern = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
    .{ .type = .{ .Token = TokenType.IntLiteral }, .debugName = "Int literal" }, // For now, expressions are just integers
}, createExpressionAST, "Expression pattern");
fn createExpressionAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    const allocation = try allocator.create(AST);

    const intLiteral = tokens[0].token.IntLiteral;

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Literal = .{ .IntLiteral = intLiteral } },

        .deinit = deinitExpression,
        .print = printExpression,
    };

    _ = self;
    _ = patternASTs;

    return allocation;
}
fn deinitExpression(self: *const AST, allocator: std.mem.Allocator) void {
    allocator.destroy(self);
}
fn printExpression(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);
    try writer.print("{s}EXPRESSION:\n", .{indentString});
    try writer.print("{s}  {d}\n", .{ indentString, self.node.Literal.IntLiteral });
}

const returnStatement = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
    .{ .type = .{ .Token = TokenType.ReturnKeyword }, .debugName = "Return keyword" },
    .{ .type = .{ .PatternId = "expressionPattern" }, .debugName = "Return statement expression" },
}, createReturnStatementAST, "Return statement");
fn createReturnStatementAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    const allocation = try allocator.create(AST);

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Return = .{ .value = patternASTs[0] } },

        .deinit = deinitReturnStatement,
        .print = printReturnStatement,
    };

    _ = self;
    _ = tokens;

    return allocation;
}
fn deinitReturnStatement(self: *const AST, allocator: std.mem.Allocator) void {
    self.node.Return.value.deinit(self.node.Return.value, allocator);
    allocator.destroy(self);
}
fn printReturnStatement(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}RETURN:\n", .{indentString});
    try self.node.Return.value.print(self.node.Return.value.*, writer, indent + 1, allocator);
}

const letStatement = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
    .{ .type = .{ .Token = TokenType.LetKeyword }, .debugName = "Let keyword" },
    .{ .type = .{ .Token = TokenType.Identifier }, .debugName = "Let statement identifier" },
    .{ .type = .{ .Token = TokenType.Colon }, .debugName = "Let statement type colon" },
    .{ .type = .{ .PatternId = "typePattern" }, .debugName = "Let statement type" },
    .{ .type = .{ .Token = TokenType.Assign }, .debugName = "Let statement equal" },
    .{ .type = .{ .PatternId = "expressionPattern" }, .debugName = "Let statement value" },
}, createLetStatementAST, "Let statement");
fn createLetStatementAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    const allocation = try allocator.create(AST);

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{
            .Assignment = .{
                // Temporary dummy data
                .identifier = "x",
                .value = patternASTs[1],
                .type = patternASTs[0],
            },
        },

        .deinit = deinitAssignment,
        .print = printAssignment,
    };

    _ = self;
    _ = tokens;

    return allocation;
}
fn deinitAssignment(self: *const AST, allocator: std.mem.Allocator) void {
    self.node.Assignment.value.deinit(self.node.Assignment.value, allocator);
    self.node.Assignment.type.deinit(self.node.Assignment.type, allocator);
    allocator.destroy(self);
}
fn printAssignment(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}ASSIGNMENT:\n", .{indentString});
    try writer.print("{s}  Identifier: {s}\n", .{ indentString, self.node.Assignment.identifier });
    try self.node.Assignment.type.print(self.node.Assignment.type.*, writer, indent + 1, allocator);
    try self.node.Assignment.value.print(self.node.Assignment.value.*, writer, indent + 1, allocator);
}

const statement = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
    // A statement
    .{ .type = .{ .PatternId = "innerStatement" }, .debugName = "Inner statement" },
    // And a Semicolon
    .{ .type = .{ .Token = TokenType.Semicolon }, .debugName = "Semicolon" },
}, passSingleASTForward, "Statement pattern");
fn passSingleASTForward(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    _ = tokens;
    _ = allocator;
    _ = self;
    return patternASTs[0];
}

const innerStatement = GrammarPattern.create(PatternType.OneOf, &[_]GrammarPatternElement{
    // TODO: Add more statements
    // .{ .Pattern = TokenType.FunctionKeyword },
    // .{ .Pattern = TokenType.IfKeyword },
    // .{ .Pattern = TokenType.ElseKeyword },
    // .{ .Pattern = TokenType.ForKeyword },
    .{ .type = .{ .PatternId = "returnStatement" }, .debugName = "Return statement" }, // Testing
    .{ .type = .{ .PatternId = "letStatement" }, .debugName = "Let statement" }, // Testing
}, createStatementAST, "Statement inner pattern");
fn createStatementAST(self: GrammarPattern, childASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*AST {
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
fn deinitStatement(self: *const AST, allocator: std.mem.Allocator) void {
    allocator.destroy(self);
}
fn printStatement(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}STATEMENT:\n", .{indentString});
    try self.node.print(writer, indent + 1, allocator);
}

const grammar: GrammarPattern = GrammarPattern.create(PatternType.AtLeastOne, &[_]GrammarPatternElement{
    .{ .type = .{ .PatternId = "statement" }, .debugName = "Statement" },
}, createGrammarAST, "Root grammar pattern");
fn createGrammarAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    const allocation = try allocator.create(AST);

    std.log.debug("Pattern ASTs: {any}", .{patternASTs});

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Block = .{ .statements = patternASTs } }, // TODO: Statements

        .deinit = deinitGrammar,
        .print = printGrammar,
    };

    _ = self;
    _ = tokens;

    return allocation;
}
fn deinitGrammar(self: *const AST, allocator: std.mem.Allocator) void {
    for (self.node.Block.statements) |stmt| {
        stmt.deinit(stmt, allocator);
    }
    allocator.free(self.node.Block.statements);
    allocator.destroy(self);
}
fn printGrammar(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);

    try writer.print("{s}GRAMMAR:\n", .{indentString});
    for (self.node.Block.statements) |stmt| {
        try stmt.print(stmt.*, writer, indent + 1, allocator);
    }
}

pub fn initParserPatterns(allocator: std.mem.Allocator) !std.StringHashMap(GrammarPattern) {
    var patterns = std.StringHashMap(GrammarPattern).init(allocator);

    try put_alloc(&patterns, "typePattern", typePattern);
    try put_alloc(&patterns, "typeInnerPattern", typeInnerPattern);
    try put_alloc(&patterns, "typeArray", typeArray);
    try put_alloc(&patterns, "expressionPattern", expressionPattern);
    try put_alloc(&patterns, "returnStatement", returnStatement);
    try put_alloc(&patterns, "letStatement", letStatement);
    try put_alloc(&patterns, "statement", statement);
    try put_alloc(&patterns, "innerStatement", innerStatement);
    try put_alloc(&patterns, "root", grammar);

    return eliminateLeftRecursion(allocator, patterns);
}

fn put_alloc(patterns: *std.StringHashMap(GrammarPattern), key: []const u8, value: GrammarPattern) !void {
    const key_alloc = try patterns.allocator.alloc(u8, key.len);
    std.mem.copyForwards(u8, key_alloc, key);
    try patterns.put(key_alloc, value);
}
