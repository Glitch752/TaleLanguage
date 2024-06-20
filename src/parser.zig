const TokenData = @import("token.zig").TokenData;
const std = @import("std");
const AST = @import("ast.zig").AST;
const NodeData = @import("ast.zig").NodeData;
const BlockNodeData = @import("ast.zig").BlockNodeData;
const Token = @import("token.zig").Token;
const pretty_error = @import("main.zig").pretty_error;

const Parser = @This();

tokens: []TokenData,
allocator: std.mem.Allocator,
position: usize,
file_name: []const u8,

pub const ParseError = error{ ExpectedIdentifier, ExpectedAssignment, ExpectedExpression };

pub fn init(tokens: []TokenData, allocator: std.mem.Allocator, file_name: []const u8) Parser {
    return .{ .tokens = tokens, .allocator = allocator, .position = 0, .file_name = file_name };
}

pub fn parse(self: *Parser) !AST {
    return AST{ .root = try self.parseBlock() };
}

fn parseBlock(self: *Parser) !BlockNodeData {
    var statements = std.ArrayList(NodeData).init(self.allocator);
    defer statements.deinit();

    while (self.position < self.tokens.len) {
        const token = self.tokens[self.position].token;
        if (token == Token.EOF) {
            break;
        }

        const statement = try self.parseStatement();
        try statements.append(statement);
    }

    return .{ .statements = &statements.items };
}

fn parseStatement(self: *Parser) !NodeData {
    const token = self.tokens[self.position].token;
    return switch (token) {
        .LetKeyword => return try self.parseVariableDeclaration(),
        // .ReturnKeyword => return try self.parseReturnStatement(),
        // .IfKeyword => return try self.parseIfStatement(),
        // .ForKeyword => return try self.parseForStatement(),
        // .FunctionKeyword => return try self.parseFunctionDeclaration(),
        // else => return try self.parseExpression(),
        // TODO
        else => {
            // TODO
            return .{
                .Block = .{ .statements = undefined },
            };
        },
    };
}

fn parseVariableDeclaration(self: *Parser) !NodeData {
    // let identifier = expression;
    // First token is the let keyword
    const identifier = self.tokens[self.position + 1];
    const assign = self.tokens[self.position + 2];
    self.position += 3;
    var expression = try self.parseExpression();

    if (identifier.token != Token.Identifier) {
        try pretty_error("Expected identifier");
        return ParseError.ExpectedIdentifier;
    }
    if (assign.token != Token.Assign) {
        const position = try self.programPosition();
        defer self.allocator.free(position);

        const message = try std.fmt.allocPrint(self.allocator, "Expected assignment (=) - {s}", .{position});
        defer self.allocator.free(message);
        try pretty_error(message);
        return ParseError.ExpectedAssignment;
    }

    return .{ .Assignment = .{ .identifier = identifier.token.Identifier, .value = &expression } };
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
    // TODO
    _ = self;
    return .{
        .Block = .{ .statements = undefined },
    };
}
