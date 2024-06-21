const TokenData = @import("token.zig").TokenData;
const std = @import("std");
const AST = @import("ast.zig").AST;
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

fn repeat(s: []const u8, times: usize, allocator: std.mem.Allocator) ![]u8 {
    const repeated = try allocator.alloc(u8, s.len * times);

    var i: usize = 0;
    while (i < s.len * times) : (i += 1) {
        repeated[i] = s[i % 2];
    }

    return repeated;
}

pub fn parse(self: *Parser) anyerror!*AST {

}

pub fn deinit(self: *Parser) void {
    if (self.ast != null) {
        self.ast.?.deinit(self.allocator);
    }
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

    const deinit = *fn(self: *AST, allocator: std.mem.Allocator) void {
        self.allocator.free(string);
    };

    typePointer.* = .{
        .line = self.tokens[self.position].line,
        .column = self.tokens[self.position].column,
        .node = .{
            .Type = .{
                .identifier = string,
            }
        },

        .deinit = *deinit,
        .print = *fn(self, writer, indent) anyerror!void {
            const repeated = try repeat("  ", indent, self.allocator);
            defer self.allocator.free(repeated);
            try writer.print("{s}", .{ repeated });
            try writer.print("Type: {s}\n", .{ self.node.Type.identifier });
        },
    }
    return typePointer;
}
