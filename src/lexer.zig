const std = @import("std");
const io = @import("std").io;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const prettyError = @import("errors.zig").prettyError;

const Tokenizer = @This();

const TokenizerErrorPayload = union(enum) {
    InvalidCharacter: u8,
};

buffer: []const u8,
tokenStartPosition: usize = 0,
position: usize = 0,
allocator: std.mem.Allocator,
tokens: std.ArrayList(Token),

const escapeSequences = std.ComptimeStringMap(u8, .{
    .{ "n", '\n' },
    .{ "r", '\r' },
    .{ "t", '\t' },
    .{ "\\", '\\' },
    .{ "\"", '"' },
    .{ "'", '\'' },
});

pub fn init(allocator: std.mem.Allocator, buffer: []const u8) Tokenizer {
    return .{
        .allocator = allocator,
        // The source data to tokenize
        .buffer = buffer,
        .tokens = std.ArrayList(Token).init(allocator),
    };
}

pub fn deinit(self: *Tokenizer) void {
    self.tokens.deinit();
}

pub fn getAllTokens(self: *Tokenizer) ![]Token {
    var errorPayload = TokenizerErrorPayload{ .InvalidCharacter = 0 };

    while (true) {
        const next = try self.getNext(&errorPayload) orelse {
            if (errorPayload.InvalidCharacter != 0) {
                const errorMessage = try std.fmt.allocPrint(self.allocator, "Invalid character: {c}\n", .{errorPayload.InvalidCharacter});
                defer self.allocator.free(errorMessage);
                prettyError(errorMessage);
            }
            break;
        };

        self.tokens.append(next);

        if (next.token == Token.EOF) {
            break;
        }
    }

    return self.tokens.items;
}

fn getNext(self: *Tokenizer, errorPayload: *TokenizerErrorPayload) !?Token {
    self.tokenStartPosition = self.position;

    while (self.position < self.buffer.len) {
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
                return Token.init(token.?, token.?.typeNameString(), self.tokenStartPosition);
            }

            // If it's not a keyword, it's an identifier
            return Token.init(TokenType.Identifier, keyword, self.tokenStartPosition);
        }

        // Next, numbers
        if (std.ascii.isDigit(c)) {
            var number: u64 = 0;
            self.position -= 1;
            while (std.ascii.isDigit(self.buffer[self.position])) {
                number = number * 10 + (self.buffer[self.position] - '0');
                self.position += 1;
            }
            if (self.buffer[self.position] == '.' and std.ascii.isDigit(self.buffer[self.position + 1])) {
                self.position += 1;
                var decimal: f64 = 0.0;
                var decimalPlace: f64 = 0.1;
                while (std.ascii.isDigit(self.buffer[self.position])) {
                    decimal += (self.buffer[self.position] - '0') * decimalPlace;
                    decimalPlace /= 10.0;
                    self.position += 1;
                }
                return Token.init(TokenType.NumberLiteral, number + decimal, self.tokenStartPosition);
            }

            return Token.init(TokenType.NumberLiteral, number, self.tokenStartPosition);
        }

        // Next, strings
        if (c == '"' or c == '\'') {
            var string = try self.allocator.alloc(u8, 0);
            var isEscaping = false;
            while (self.buffer[self.position] != c or isEscaping) {
                if (isEscaping) {
                    isEscaping = false;

                    // Handle escape sequences
                    const escapeChar = escapeSequences.get(self.buffer[self.position .. self.position + 1]);
                    if (escapeChar != null) {
                        string = try self.allocator.realloc(string, string.len + 1);
                        string[string.len - 1] = escapeChar.?;
                    } else {
                        // If it's not a valid escape sequence, just print both characters
                        string = try self.allocator.realloc(string, string.len + 2);
                        string[string.len - 2] = self.buffer[self.position - 1];
                        string[string.len - 1] = self.buffer[self.position];
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

            return .{ .token = Token{ .StringLiteral = string }, .line = self.line, .column = self.column };
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
                return Token.init(tokenType.?, tokenType.?.typeNameString(), self.position);
            }

            symbol = try self.allocator.realloc(symbol, symbol.len - 1);
        }

        // If we got here, it's an unknown token
        errorPayload.* = TokenizerErrorPayload{ .InvalidCharacter = c };
        return null;
    }

    return Token.init(TokenType.EOF, "EOF", self.position);
}
