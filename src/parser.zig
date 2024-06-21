const TokenData = @import("token.zig").TokenData;
const std = @import("std");
const AST = @import("ast.zig").AST;
const NodeData = @import("ast.zig").NodeData;
const BlockNodeData = @import("ast.zig").BlockNodeData;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const pretty_error = @import("main.zig").pretty_error;

const Parser = @This();

pub const NodeDataList = struct {
    nodes: std.ArrayList(NodeData),

    pub fn init(allocator: std.mem.Allocator) NodeDataList {
        return .{ .nodes = std.ArrayList(NodeData).init(allocator) };
    }

    pub fn deinit(self: NodeDataList, allocator: std.mem.Allocator) void {
        for (self.nodes.items) |node| {
            node.deinit(allocator);
        }
    }

    pub fn append(self: *NodeDataList, node: NodeData) !void {
        try self.nodes.append(node);
    }
};

tokens: []TokenData,
allocator: std.mem.Allocator,
position: usize,
file_name: []const u8,
nodeDataList: NodeDataList,
ast: ?*AST,

pub const ParseError = error{ ExpectedIdentifier, ExpectedAssignment, ExpectedExpression };

pub fn init(tokens: []TokenData, allocator: std.mem.Allocator, file_name: []const u8) Parser {
    return .{ .tokens = tokens, .allocator = allocator, .position = 0, .file_name = file_name, .nodeDataList = NodeDataList.init(allocator), .ast = null };
}

pub fn parse(self: *Parser) !AST {
    var ast = AST{ .root = try self.parseBlock() };
    self.ast = &ast;
    return ast;
}

pub fn deinit(self: *Parser) void {
    if (self.ast != null) {
        self.ast.?.*.deinit(self.allocator);
    }
    self.nodeDataList.deinit(self.allocator);
}

fn parseBlock(self: *Parser) !BlockNodeData {
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
        try statements.append(statement);
    }

    return .{ .statements = try statements.toOwnedSlice() };
}

fn parseStatement(self: *Parser) !NodeData {
    const token = self.tokens[self.position].token;
    return switch (token) {
        .LetKeyword => {
            const declaration = try self.parseVariableDeclaration();
            return declaration;
        },
        // .ReturnKeyword => return try self.parseReturnStatement(),
        // .IfKeyword => return try self.parseIfStatement(),
        // .ForKeyword => return try self.parseForStatement(),
        // .FunctionKeyword => return try self.parseFunctionDeclaration(),
        // else => return try self.parseExpression(),
        // TODO
        else => {
            // TODO
            self.position += 1;
            return .{
                .Literal = .{ .IntLiteral = 10 },
            };
        },
    };
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

    var varType = try self.parseType();
    errdefer varType.deinit(self.allocator);

    const assign = self.tokens[self.position];
    try self.expectTokenType(assign, TokenType.Assign);

    self.position += 1;
    try self.ensurePositionInBounds();

    var expression = try self.parseExpression();
    errdefer expression.deinit(self.allocator);

    try self.nodeDataList.append(expression);

    const node = NodeData{ .Assignment = .{ .identifier = identifierString, .value = &expression, .type = &varType } };
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

fn parseExpression(self: *Parser) !NodeData {
    _ = self;
    // TODO
    return .{ .Literal = .{ .IntLiteral = 10 } };
}

fn parseType(self: *Parser) !NodeData {
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
        if (parentheses <= 0) break;

        self.position += 1;
        try self.ensurePositionInBounds();

        const tokenString = self.tokens[self.position].token.toString();
        string = try self.allocator.realloc(string, string.len + tokenString.len);
        for (tokenString, 0..) |c, i| {
            string[string.len - tokenString.len + i] = c;
        }
    }

    self.position += 1;
    try self.ensurePositionInBounds();

    return .{ .Type = .{ .identifier = string } };
}
