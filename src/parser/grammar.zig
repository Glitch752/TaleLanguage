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

const letStatement = GrammarPattern.create(PatternType.All, &[_]GrammarPatternElement{
    .{ .type = .{ .Token = TokenType.LetKeyword }, .debugName = "Let keyword" },
}, createLetStatementAST, "Let statement");
fn createLetStatementAST(self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) !?*const AST {
    const allocation = try allocator.create(AST);

    const tempValueAllocation = try allocator.create(AST);

    tempValueAllocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Literal = .{ .IntLiteral = 10 } }, // TODO: Expression

        .deinit = deinitStatement,
        .print = printStatement,
    };

    const tempTypeAllocation = try allocator.create(AST);

    tempTypeAllocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{ .Type = .{ .identifier = "Int" } },

        .deinit = deinitType,
        .print = printType,
    };

    _ = patternASTs;

    allocation.* = AST{
        .column = 0,
        .line = 0,
        .node = .{
            .Assignment = .{
                // Temporary dummy data
                .identifier = "x",
                .value = tempValueAllocation,
                .type = tempTypeAllocation,
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

fn deinitType(self: *const AST, allocator: std.mem.Allocator) void {
    allocator.destroy(self);
}
fn printType(self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
    const indentString = try repeat("  ", indent, allocator);
    defer allocator.free(indentString);
    try writer.print("{s}TYPE:\n", .{indentString});
    try writer.print("{s}  {s}\n", .{ indentString, self.node.Type.identifier });
}

const statement = GrammarPattern.create(PatternType.OneOf, &[_]GrammarPatternElement{
    // TODO: Add more statements
    // .{ .Pattern = TokenType.FunctionKeyword },
    // .{ .Pattern = TokenType.IfKeyword },
    // .{ .Pattern = TokenType.ElseKeyword },
    // .{ .Pattern = TokenType.ForKeyword },
    // .{ .Pattern = TokenType.ReturnKeyword },
    // .{ .Pattern = TokenType.LetKeyword },
    .{ .type = .{ .Pattern = &letStatement }, .debugName = "Let statement" }, // Testing
}, createStatementAST, "Statement pattern");
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

pub const grammar: GrammarPattern = GrammarPattern.create(PatternType.AtLeastOne, &[_]GrammarPatternElement{
    .{ .type = .{ .Pattern = &statement }, .debugName = "Statement" },
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
