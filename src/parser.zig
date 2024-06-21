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

pub fn parse(self: *Parser) anyerror!*AST {
    const block = try self.parseBlock();
    const ast = AST{ .root = block };
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
    errdefer {
        blockPointer.deinit(self.allocator);
        self.allocator.destroy(blockPointer);
    }

    var statements = std.ArrayList(NodeData).init(self.allocator);
    errdefer {
        for (statements.items) |statement| {
            statement.deinit(self.allocator);
            self.allocator.destroy(&statement);
        }
        statements.deinit();
    }

    while (self.position < self.tokens.len) {
        const token = self.tokens[self.position].token;
        if (token == Token.EOF) {
            break;
        }

        const statement = self.parseStatement() catch {
            self.position += 1;
            // This segfaults if we return an error here for some reason... I don't know why.
            continue;
        } orelse {
            break;
        };
        try statements.append(statement);
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
        .ForKeyword => return try self.parseForStatement(),
        // else => return try self.parseExpression(),
        // TODO
        else => {
            return null;
        },
    };

    self.position += 1;
    try self.ensurePositionInBounds("Statement");

    // Next token should be a semicolon
    const semicolon = self.tokens[self.position];
    try self.expectTokenType(semicolon, TokenType.Semicolon, "Statement - semicolon");

    self.position += 1;
    try self.ensurePositionInBounds("Statement - semicolon");

    return statement;
}

fn ensurePositionInBounds(self: *Parser, source: []const u8) !void {
    if (self.position >= self.tokens.len) {
        const position = try self.programPosition();
        defer self.allocator.free(position);

        const message = try std.fmt.allocPrint(self.allocator, "Unexpected end of file - {s}", .{position});
        defer self.allocator.free(message);
        try pretty_error(message, source);
    }
}

fn expectTokenType(self: *Parser, token: TokenData, expected: TokenType, source: []const u8) !void {
    if (token.token != expected) {
        const position = try self.programPosition();
        defer self.allocator.free(position);

        const message = try std.fmt.allocPrint(self.allocator, "Expected {s}, found {s} | {s}", .{ expected.typeNameString(), token.token.typeNameString(), position });
        defer self.allocator.free(message);
        try pretty_error(message, source);

        return switch (expected) {
            TokenType.Identifier => return ParseError.ExpectedIdentifier,
            TokenType.Assign => return ParseError.ExpectedAssignment,
            else => return ParseError.ExpectedExpression,
        };
    }
}

fn parseVariableDeclaration(self: *Parser) !NodeData {
    // let identifier = expression;
    // First token is the let keyword
    self.position += 1;
    try self.ensurePositionInBounds("Variable declaration");

    const identifier = self.tokens[self.position];
    try self.expectTokenType(identifier, TokenType.Identifier, "Variable declaration - identifier");
    const identifierString = try self.allocator.alloc(u8, identifier.token.Identifier.len);
    errdefer self.allocator.free(identifierString);
    for (identifier.token.Identifier, 0..) |c, i| {
        identifierString[i] = c;
    }

    self.position += 1;
    try self.ensurePositionInBounds("Variable declaration - identifier");

    const colon = self.tokens[self.position];
    try self.expectTokenType(colon, TokenType.Colon, "Variable declaration - colon");

    self.position += 1;
    try self.ensurePositionInBounds("Variable declaration - colon");

    var typePointer = try self.parseType();
    errdefer typePointer.deinit(self.allocator);

    const assign = self.tokens[self.position];
    try self.expectTokenType(assign, TokenType.Assign, "Variable declaration - assignment");

    self.position += 1;
    try self.ensurePositionInBounds("Variable declaration - assignment");

    const expressionPointer = try self.parseExpression();
    errdefer {
        expressionPointer.deinit(self.allocator);
        self.allocator.destroy(expressionPointer);
    }

    const node = NodeData{ .Assignment = .{ .identifier = identifierString, .value = expressionPointer, .type = typePointer } };
    return node;
}

fn parseReturnStatement(self: *Parser) !NodeData {
    // return expression;
    // First token is the return keyword
    self.position += 1;
    try self.ensurePositionInBounds("Return statement - return");

    const expressionPointer = try self.parseExpression();
    errdefer {
        expressionPointer.deinit(self.allocator);
        self.allocator.destroy(expressionPointer);
    }

    const node = NodeData{ .Return = .{ .value = expressionPointer } };
    return node;
}

fn parseFunctionDeclaration(self: *Parser) !NodeData {
    // function() { block } (type is handled by the variable being assigned)
    // First token is the function keyword
    self.position += 1;
    try self.ensurePositionInBounds("Function declaration - function");

    // Next token should be an open parenthesis
    const openParen = self.tokens[self.position];
    try self.expectTokenType(openParen, TokenType.OpenParen, "Function declaration - open parenthesis");

    self.position += 1;
    try self.ensurePositionInBounds("Function declaration - open parenthesis");

    // Next is parameters
    var parameters = std.ArrayList(FunctionParameter).init(self.allocator);
    errdefer {
        for (parameters.items) |parameter| {
            parameter.deinit(self.allocator);
            self.allocator.destroy(&parameter);
        }
        parameters.deinit();
    }

    while (self.tokens[self.position].token != Token.CloseParen and self.position < self.tokens.len) {
        const parameter = try self.parseParameter();
        try parameters.append(parameter);
    }

    // Next token should be a close parenthesis
    const closeParen = self.tokens[self.position];
    try self.expectTokenType(closeParen, TokenType.CloseParen, "Function declaration - close parenthesis");

    self.position += 1;
    try self.ensurePositionInBounds("Function declaration - close parenthesis");

    // Next token should be an open curly brace
    const openCurly = self.tokens[self.position];
    try self.expectTokenType(openCurly, TokenType.OpenCurly, "Function declaration - open curly brace");

    self.position += 1;
    try self.ensurePositionInBounds("Function declaration - open curly brace");

    // Next is the block
    const block = try self.parseBlock();
    errdefer {
        block.deinit(self.allocator);
        self.allocator.destroy(block);
    }

    // Next token should be a close curly brace
    const closeCurly = self.tokens[self.position];
    self.expectTokenType(closeCurly, TokenType.CloseCurly, "Function declaration - close curly brace") catch {
        // This also segfaults if we return an error here for some reason... I don't know why.
        // Ugh Zig errors are so unreliable
        return NodeData{ .Function = .{ .parameters = &[_]FunctionParameter{}, .block = block } };
    };

    self.position += 1;
    try self.ensurePositionInBounds("Function declaration - close curly brace");

    const node = NodeData{ .Function = .{ .parameters = try parameters.toOwnedSlice(), .block = block } };
    return node;
}

fn parseParameter(self: *Parser) !FunctionParameter {
    // identifier: type
    const identifier = self.tokens[self.position];
    try self.expectTokenType(identifier, TokenType.Identifier, "Function parameter - identifier");

    const identifierString = try self.allocator.alloc(u8, identifier.token.Identifier.len);
    for (identifier.token.Identifier, 0..) |c, i| {
        identifierString[i] = c;
    }

    self.position += 1;
    try self.ensurePositionInBounds("Function parameter - identifier");

    const colon = self.tokens[self.position];
    try self.expectTokenType(colon, TokenType.Colon, "Function parameter - colon");

    self.position += 1;
    try self.ensurePositionInBounds("Function parameter - colon");

    var typePointer = try self.parseType();
    errdefer typePointer.deinit(self.allocator);

    return FunctionParameter{ .identifier = identifierString, .type = typePointer };
}

fn parseForStatement(self: *Parser) !NodeData {
    // TODO: Make listIdentifier an actual expression
    // for(identifier : listIdentifier) { block }

    // First token is the for keyword
    self.position += 1;
    try self.ensurePositionInBounds("For loop - for");

    // Next token should be an open parenthesis
    const openParen = self.tokens[self.position];
    try self.expectTokenType(openParen, TokenType.OpenParen, "For loop - open parenthesis");

    self.position += 1;
    try self.ensurePositionInBounds("For loop - open parenthesis");

    // Next is the identifier
    const identifier = self.tokens[self.position];
    try self.expectTokenType(identifier, TokenType.Identifier, "For loop - identifier");

    const identifierString = try self.allocator.alloc(u8, identifier.token.Identifier.len);
    errdefer self.allocator.free(identifierString);
    for (identifier.token.Identifier, 0..) |c, i| {
        identifierString[i] = c;
    }

    self.position += 1;
    try self.ensurePositionInBounds("For loop - identifier");

    const colon = self.tokens[self.position];
    try self.expectTokenType(colon, TokenType.Colon, "For loop - colon");

    self.position += 1;
    try self.ensurePositionInBounds("For loop - colon");

    const listIdentifier = self.tokens[self.position];
    try self.expectTokenType(listIdentifier, TokenType.Identifier, "For loop - list identifier");

    const listIdentifierString = try self.allocator.alloc(u8, listIdentifier.token.Identifier.len);
    errdefer self.allocator.free(listIdentifierString);
    for (listIdentifier.token.Identifier, 0..) |c, i| {
        listIdentifierString[i] = c;
    }

    self.position += 1;
    try self.ensurePositionInBounds("For loop - list identifier");

    // Next token should be a close parenthesis
    const closeParen = self.tokens[self.position];
    try self.expectTokenType(closeParen, TokenType.CloseParen, "For loop - close parenthesis");

    self.position += 1;
    try self.ensurePositionInBounds("For loop - close parenthesis");

    // Next token should be an open curly brace
    const openCurly = self.tokens[self.position];
    try self.expectTokenType(openCurly, TokenType.OpenCurly, "For loop - open curly brace");

    self.position += 1;
    try self.ensurePositionInBounds("For loop - open curly brace");

    // Next is the block
    const block = try self.parseBlock();
    errdefer {
        block.deinit(self.allocator);
        self.allocator.destroy(block);
    }

    // Next token should be a close curly brace
    const closeCurly = self.tokens[self.position];
    try self.expectTokenType(closeCurly, TokenType.CloseCurly, "For loop - close curly brace");

    self.position += 1;
    try self.ensurePositionInBounds("For loop - close curly brace");

    const node = NodeData{ .ForLoop = .{ .iteratorIdentifier = identifierString, .listIdentifier = listIdentifierString, .block = block } };
    return node;
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
    std.log.debug("Parsing expression, tokens: {any}\n", .{self.tokens[self.position .. self.position + 5]});
    var expressionPointer = try self.allocator.create(NodeData);
    errdefer expressionPointer.deinit(self.allocator);

    // Expressions can be many things. First, functions:
    const token = self.tokens[self.position];
    if (token.token == Token.FunctionKeyword) {
        const function = try self.parseFunctionDeclaration();
        expressionPointer.* = function;
        return expressionPointer;
    }

    // If it's an int literal:
    if (token.token == Token.IntLiteral) {
        // If the next character is a range operator, it's a range. Otherwise, it's a literal.
        // TODO: Make ranges support expressions
        const nextToken = self.tokens[self.position + 1];
        if (nextToken.token == Token.Range) {
            self.position += 2;
            try self.ensurePositionInBounds("Expression - range");

            const toValue = self.tokens[self.position];
            try self.expectTokenType(toValue, TokenType.IntLiteral, "Expression - range end");

            const range = try self.allocator.create(NodeData);
            errdefer {
                range.deinit(self.allocator);
                self.allocator.destroy(range);
            }

            const startNode = try self.allocator.create(NodeData);
            errdefer {
                startNode.deinit(self.allocator);
                self.allocator.destroy(startNode);
            }

            startNode.* = .{ .Literal = .{ .IntLiteral = token.token.IntLiteral } };

            const endNode = try self.allocator.create(NodeData);
            errdefer {
                endNode.deinit(self.allocator);
                self.allocator.destroy(endNode);
            }

            endNode.* = .{ .Literal = .{ .IntLiteral = toValue.token.IntLiteral } };

            range.* = .{ .Range = .{ .startValue = startNode, .endValue = endNode } };
            return expressionPointer;
        } else {
            // Just a literal
            expressionPointer.* = .{ .Literal = .{ .IntLiteral = token.token.IntLiteral } };
            return expressionPointer;
        }
    }

    // If it's a string literal:
    if (token.token == Token.StringLiteral) {
        expressionPointer.* = .{ .Literal = .{ .StringLiteral = token.token.StringLiteral } };
        return expressionPointer;
    }

    expressionPointer.* = .{ .Literal = .{ .IntLiteral = 0 } };
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

    typePointer.* = .{ .Type = .{ .identifier = string } };
    return typePointer;
}
