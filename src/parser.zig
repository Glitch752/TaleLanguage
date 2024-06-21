const TokenData = @import("token.zig").TokenData;
const std = @import("std");
const AST = @import("ast.zig").AST;
const NodeData = @import("ast.zig").NodeData;
const FunctionParameter = @import("ast.zig").FunctionParameter;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const pretty_error = @import("main.zig").pretty_error;

const Parser = @This();

tokens: []TokenData,
allocator: std.mem.Allocator,
position: usize,
file_name: []const u8,
ast: ?AST,

pub const ParseError = error{ ExpectedIdentifier, ExpectedAssignment, ExpectedExpression };

pub fn init(tokens: []TokenData, allocator: std.mem.Allocator, file_name: []const u8) Parser {
    return .{ .tokens = tokens, .allocator = allocator, .position = 0, .file_name = file_name, .ast = null };
}

pub fn parse(self: *Parser) !*AST {
    const ast = AST{ .root = try self.parseBlock() };
    self.ast = ast;
    return &self.ast.?;
}

pub fn deinit(self: *Parser) void {
    if (self.ast != null) {
        self.ast.?.deinit(self.allocator);
    }
}

fn parseBlock(self: *Parser) anyerror!*NodeData {
    var blockPointer = try self.allocator.create(NodeData);
    errdefer blockPointer.deinit(self.allocator);

    var statements = std.ArrayList(NodeData).init(self.allocator);
    errdefer {
        for (statements.items) |statement| {
            statement.deinit(self.allocator);
        }
    }

    while (self.position < self.tokens.len) {
        const token = self.tokens[self.position].token;
        if (token == Token.EOF) {
            break;
        }

        const statement = try self.parseStatement();
        if (statement == null) {
            break;
        }
        try statements.append(statement.?);
    }

    blockPointer.* = .{ .Block = .{ .statements = try statements.toOwnedSlice() } };
    // blockPointer.* = .{ .Block = .{ .statements = &[_]NodeData{} } };
    return blockPointer;
}

fn parseStatement(self: *Parser) !?NodeData {
    const token = self.tokens[self.position].token;
    const statement = switch (token) {
        .LetKeyword => {
            const declaration = try self.parseVariableDeclaration();
            return declaration;
        },
        .ReturnKeyword => return try self.parseReturnStatement(),
        // .IfKeyword => return try self.parseIfStatement(),
        // .ForKeyword => return try self.parseForStatement(),
        .FunctionKeyword => return try self.parseFunctionDeclaration(),
        // else => return try self.parseExpression(),
        // TODO
        else => {
            return null;
        },
    };

    self.position += 1;
    try self.ensurePositionInBounds();

    // Next token should be a semicolon
    const semicolon = self.tokens[self.position];
    try self.expectTokenType(semicolon, TokenType.Semicolon);

    return statement;
}

fn ensurePositionInBounds(self: *Parser) !void {
    if (self.position >= self.tokens.len) {
        const position = try self.programPosition();
        defer self.allocator.free(position);

        const message = try std.fmt.allocPrint(self.allocator, "Unexpected end of file - {s}", .{position});
        defer self.allocator.free(message);
        try pretty_error(message);
    }
}

fn expectTokenType(self: *Parser, token: TokenData, expected: TokenType) !void {
    if (token.token != expected) {
        const position = try self.programPosition();
        defer self.allocator.free(position);

        const message = try std.fmt.allocPrint(self.allocator, "Expected {s}, found {s} | {s}", .{ expected.typeNameString(), token.token.typeNameString(), position });
        defer self.allocator.free(message);
        try pretty_error(message);

        switch (expected) {
            TokenType.Identifier => return ParseError.ExpectedIdentifier,
            TokenType.Assign => return ParseError.ExpectedAssignment,
            else => return ParseError.ExpectedExpression,
        }
    }
}

fn parseVariableDeclaration(self: *Parser) !NodeData {
    // let identifier = expression;
    // First token is the let keyword
    self.position += 1;
    try self.ensurePositionInBounds();

    const identifier = self.tokens[self.position];
    try self.expectTokenType(identifier, TokenType.Identifier);
    const identifierString = try self.allocator.alloc(u8, identifier.token.Identifier.len);
    for (identifier.token.Identifier, 0..) |c, i| {
        identifierString[i] = c;
    }

    self.position += 1;
    try self.ensurePositionInBounds();

    const colon = self.tokens[self.position];
    try self.expectTokenType(colon, TokenType.Colon);

    self.position += 1;
    try self.ensurePositionInBounds();

    var typePointer = try self.parseType();
    errdefer typePointer.deinit(self.allocator);

    const assign = self.tokens[self.position];
    try self.expectTokenType(assign, TokenType.Assign);

    self.position += 1;
    try self.ensurePositionInBounds();

    const expressionPointer = try self.parseExpression();
    errdefer expressionPointer.deinit(self.allocator);

    const node = NodeData{ .Assignment = .{ .identifier = identifierString, .value = expressionPointer, .type = typePointer } };
    return node;
}

fn parseReturnStatement(self: *Parser) !NodeData {
    // return expression;
    // First token is the return keyword
    self.position += 1;
    try self.ensurePositionInBounds();

    const expressionPointer = try self.parseExpression();
    errdefer expressionPointer.deinit(self.allocator);

    const node = NodeData{ .Return = .{ .value = expressionPointer } };
    return node;
}

fn parseFunctionDeclaration(self: *Parser) !NodeData {
    // function() { block } (type is handled by the variable being assigned)
    // First token is the function keyword
    self.position += 1;
    try self.ensurePositionInBounds();

    // Next token should be an open parenthesis
    const openParen = self.tokens[self.position];
    try self.expectTokenType(openParen, TokenType.OpenParen);

    self.position += 1;
    try self.ensurePositionInBounds();

    // Next is parameters
    var parameters = std.ArrayList(FunctionParameter).init(self.allocator);
    errdefer {
        for (parameters.items) |parameter| {
            parameter.deinit(self.allocator);
        }
    }

    while (self.tokens[self.position].token != Token.CloseParen) {
        const parameter = try self.parseParameter();
        try parameters.append(parameter);
    }

    // Next token should be a close parenthesis
    const closeParen = self.tokens[self.position];
    try self.expectTokenType(closeParen, TokenType.CloseParen);

    self.position += 1;
    try self.ensurePositionInBounds();

    // Next token should be an open curly brace
    const openCurly = self.tokens[self.position];
    try self.expectTokenType(openCurly, TokenType.OpenCurly);

    self.position += 1;
    try self.ensurePositionInBounds();

    // Next is the block
    const block = try self.parseBlock();

    // Next token should be a close curly brace
    const closeCurly = self.tokens[self.position];
    try self.expectTokenType(closeCurly, TokenType.CloseCurly);

    self.position += 1;
    try self.ensurePositionInBounds();

    const node = NodeData{ .Function = .{ .parameters = try parameters.toOwnedSlice(), .block = block } };
    return node;
}

fn parseParameter(self: *Parser) !FunctionParameter {
    // identifier: type
    const identifier = self.tokens[self.position];
    try self.expectTokenType(identifier, TokenType.Identifier);

    const identifierString = try self.allocator.alloc(u8, identifier.token.Identifier.len);
    for (identifier.token.Identifier, 0..) |c, i| {
        identifierString[i] = c;
    }

    self.position += 1;
    try self.ensurePositionInBounds();

    const colon = self.tokens[self.position];
    try self.expectTokenType(colon, TokenType.Colon);

    self.position += 1;
    try self.ensurePositionInBounds();

    var typePointer = try self.parseType();
    errdefer typePointer.deinit(self.allocator);

    return FunctionParameter{ .identifier = identifierString, .type = typePointer };
}

fn programPosition(self: *Parser) ![]const u8 {
    const token = self.tokens[self.position];

    const line_string = try std.fmt.allocPrint(self.allocator, "{d}", .{token.line});
    const column_string = try std.fmt.allocPrint(self.allocator, "{d}", .{token.column});

    defer {
        self.allocator.free(line_string);
        self.allocator.free(column_string);
    }

    const file_name = self.file_name;
    return try std.fmt.allocPrint(self.allocator, "{s}:{s}:{s}", .{ file_name, line_string, column_string });
}

fn parseExpression(self: *Parser) !*NodeData {
    var expressionPointer = try self.allocator.create(NodeData);
    errdefer expressionPointer.deinit(self.allocator);

    expressionPointer.* = .{ .Literal = .{ .IntLiteral = 10 } };
    return expressionPointer;
}

fn parseType(self: *Parser) !*NodeData {
    var typePointer = try self.allocator.create(NodeData);
    errdefer typePointer.deinit(self.allocator);

    // TODO: real types
    // For now, just store everything in the parentheses
    var parentheses: usize = 0;
    var string = try self.allocator.alloc(u8, 0);

    errdefer {
        self.allocator.free(string);
    }

    while (self.tokens[self.position].token == Token.OpenParen) {
        parentheses += 1;
        self.position += 1;
        try self.ensurePositionInBounds();

        string = try self.allocator.realloc(string, parentheses);
        string[parentheses - 1] = '(';
    }
    while (parentheses > 0) {
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
        try self.ensurePositionInBounds();
    }

    self.position += 1;
    try self.ensurePositionInBounds();

    typePointer.* = .{ .Type = .{ .identifier = string } };
    return typePointer;
}
