const std = @import("std");
const io = @import("std").io;

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const TokenLiteral = @import("token.zig").TokenLiteral;

const prettyError = @import("errors.zig").prettyError;
const errorContext = @import("errors.zig").errorContext;

const Tokenizer = @This();

const TokenizerErrorPayload = union(enum) {
    InvalidCharacter: u8,
};

buffer: []const u8,
tokenStartPosition: usize = 0,
position: usize = 0,
allocator: std.mem.Allocator,
tokens: std.ArrayList(Token),
filePath: []const u8,

const escapeSequences = std.StaticStringMap(u8).initComptime(.{
    .{ "n", '\n' },
    .{ "r", '\r' },
    .{ "t", '\t' },
    .{ "\\", '\\' },
    .{ "\"", '"' },
    .{ "'", '\'' },
});

pub fn init(allocator: std.mem.Allocator, filePath: []const u8, buffer: []const u8) Tokenizer {
    return .{
        .allocator = allocator,
        // The source data to tokenize
        .buffer = buffer,
        .filePath = filePath,
        .tokens = std.ArrayList(Token).init(allocator),
    };
}

pub fn deinit(self: *Tokenizer) void {
    for (self.tokens.items) |token| {
        token.deinit(self.allocator);
    }
    self.tokens.deinit();
}

fn isAtEnd(self: *Tokenizer) bool {
    return self.position >= self.buffer.len;
}

/// Always returns false to make `return try self.addToken(...)` work
fn addToken(self: *Tokenizer, @"type": TokenType, literal: TokenLiteral) !bool {
    const lexeme = self.buffer[self.tokenStartPosition..self.position];
    try self.tokens.append(Token.init(@"type", lexeme, literal, self.tokenStartPosition));
    return false;
}

pub fn getAllTokens(self: *Tokenizer) !?[]Token {
    var errorPayload = TokenizerErrorPayload{ .InvalidCharacter = 0 };

    while (!self.isAtEnd()) {
        if (try self.takeNext(&errorPayload)) {
            // TODO: Better errors with at least some context and line numbers
            const errorMessage = try std.fmt.allocPrint(self.allocator, "Invalid character: {c}\n", .{errorPayload.InvalidCharacter});
            defer self.allocator.free(errorMessage);
            try prettyError(errorMessage);
            try errorContext(self.buffer, self.filePath, self.position - 1, 1, self.allocator);
            return null;
        }
    }

    try self.tokens.append(Token.init(TokenType.EOF, "", TokenLiteral.None, self.position));

    return self.tokens.items;
}

/// Returns an error if there was an allocation error, but otherwise returns a boolean indicating if there was a tokenizer error.
/// True means there was an error, and errorPayload will be set.
/// This isn't an ideal solution, but Zig's lack of error payloads makes it difficult to get detailed error information out of a function.
fn takeNext(self: *Tokenizer, errorPayload: *TokenizerErrorPayload) !bool {
    while (self.position < self.buffer.len) {
        self.tokenStartPosition = self.position;

        // Read the next character
        const c = self.buffer[self.position];
        self.position += 1;

        // First, skip whitespace
        if (std.ascii.isWhitespace(c)) continue;

        // Next, handle comments
        if (c == '/') {
            if (self.buffer[self.position] == '/') {
                // Skip to the end of the line
                while (self.position < self.buffer.len and self.buffer[self.position] != '\n') {
                    self.position += 1;
                }
                continue;
            } else if (self.buffer[self.position] == '*') {
                // Skip to the end of the block comment
                while (self.position < self.buffer.len and self.buffer[self.position] != '*' or self.buffer[self.position + 1] != '/') {
                    self.position += 1;
                }
                self.position += 2;
                continue;
            }
        }

        // Next, keywords and identifiers
        if (std.ascii.isAlphabetic(c)) {
            var keyword = try self.allocator.alloc(u8, 1);
            keyword[0] = c;
            var i: u16 = 1;
            while (self.position < self.buffer.len and (std.ascii.isAlphanumeric(self.buffer[self.position]) or self.buffer[self.position] == '_')) {
                keyword = try self.allocator.realloc(keyword, i + 1);
                keyword[i] = self.buffer[self.position];
                i += 1;

                self.position += 1;
            }

            const token = Token.keywordMap.get(keyword);
            if (token != null) {
                self.allocator.free(keyword);
                return try self.addToken(token.?, TokenLiteral.None);
            }

            // If it's not a keyword, it's an identifier
            return try self.addToken(TokenType.Identifier, .{ .Identifier = keyword });
        }

        // Next, numbers
        if (std.ascii.isDigit(c)) {
            var number: u64 = 0;
            self.position -= 1;
            while (self.position < self.buffer.len and std.ascii.isDigit(self.buffer[self.position])) {
                number = number * 10 + (self.buffer[self.position] - '0');
                self.position += 1;
                while (self.position < self.buffer.len and self.buffer[self.position] == '_') self.position += 1;
            }
            if (self.position < self.buffer.len and self.buffer[self.position] == '.' and std.ascii.isDigit(self.buffer[self.position + 1])) {
                self.position += 1;
                var decimal: f64 = 0.0;
                var decimalPlace: f64 = 0.1;
                while (self.position < self.buffer.len and std.ascii.isDigit(self.buffer[self.position])) {
                    decimal += @as(f64, @floatFromInt(self.buffer[self.position] - '0')) * decimalPlace;
                    decimalPlace /= 10.0;
                    self.position += 1;
                    while (self.position < self.buffer.len and self.buffer[self.position] == '_') self.position += 1;
                }
                return try self.addToken(TokenType.NumberLiteral, .{ .NumberLiteral = @as(f64, @floatFromInt(number)) + decimal });
            }

            return try self.addToken(TokenType.NumberLiteral, .{ .NumberLiteral = @as(f64, @floatFromInt(number)) });
        }

        // Next, strings
        if (c == '"' or c == '\'') {
            var string = try self.allocator.alloc(u8, 0);
            var isEscaping = false;
            while (self.buffer[self.position] != c or isEscaping) {
                if (isEscaping) {
                    isEscaping = false;

                    // Handle escape sequences
                    const escapeIdentifier = self.buffer[self.position .. self.position + 1];
                    if (std.mem.eql(u8, escapeIdentifier, "x")) {
                        if (self.position >= self.buffer.len - 2) {
                            errorPayload.* = TokenizerErrorPayload{ .InvalidCharacter = 0 };
                            return true;
                        }

                        // This is a hex escape sequence
                        string = try self.allocator.realloc(string, string.len + 1);
                        self.position += 1;
                        const hex = self.buffer[self.position .. self.position + 2];
                        const hexValue = try std.fmt.parseInt(u8, hex, 16);
                        string[string.len - 1] = hexValue;
                        self.position += 1;
                    } else {
                        const escapeChar = escapeSequences.get(escapeIdentifier);
                        if (escapeChar != null) {
                            string = try self.allocator.realloc(string, string.len + 1);
                            string[string.len - 1] = escapeChar.?;
                        } else {
                            // If it's not a valid escape sequence, just print both characters
                            string = try self.allocator.realloc(string, string.len + 2);
                            string[string.len - 2] = self.buffer[self.position - 1];
                            string[string.len - 1] = self.buffer[self.position];
                        }
                    }
                } else if (self.buffer[self.position] == '\\') {
                    isEscaping = true;
                } else {
                    string = try self.allocator.realloc(string, string.len + 1);
                    string[string.len - 1] = self.buffer[self.position];
                }

                self.position += 1;
                if (self.position >= self.buffer.len) break;
            }
            self.position += 1;

            return try self.addToken(TokenType.StringLiteral, .{ .StringLiteral = string });
        }

        // Next, symbols.
        var maxLength: usize = Token.maxSymbolLength;
        if (self.position + maxLength > self.buffer.len) {
            maxLength = self.buffer.len - self.position + 1;
        }

        var symbol = try self.allocator.alloc(u8, maxLength);
        defer self.allocator.free(symbol);
        for (0..maxLength) |i| {
            symbol[i] = self.buffer[self.position - 1 + i];
        }

        // Try progressively shorter symbols until we find a match
        while (symbol.len > 0) {
            const tokenType = Token.symbolMap.get(symbol);
            if (tokenType != null) {
                self.position += symbol.len - 1;
                return try self.addToken(tokenType.?, TokenLiteral.None);
            }

            symbol = try self.allocator.realloc(symbol, symbol.len - 1);
        }

        // If we got here, it's an unknown token
        errorPayload.* = TokenizerErrorPayload{ .InvalidCharacter = c };
        return true;
    }

    return false;
}
