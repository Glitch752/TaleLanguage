const std = @import("std");
const io = @import("std").io;
const TokenData = @import("token.zig").TokenData;
const Token = @import("token.zig").Token;

const Tokenizer = @This();

buffer: []u8,
line: usize,
column: usize,
position: usize,

const escapeSequences = std.ComptimeStringMap(u8, .{
    .{ "n", '\n' },
    .{ "r", '\r' },
    .{ "t", '\t' },
    .{ "\\", '\\' },
    .{ "\"", '"' },
    .{ "'", '\'' },
});

pub fn init(buffer: []u8) Tokenizer {
    return .{ .buffer = buffer, .line = 1, .column = 1, .position = 0 };
}

pub fn getNext(self: *Tokenizer, allocator: std.mem.Allocator) !TokenData {
    while (self.position < self.buffer.len) {
        // Read the next character
        const c = self.buffer[self.position];
        self.position += 1;

        // First, skip whitespace
        if (std.ascii.isWhitespace(c)) {
            if (c == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }

            continue;
        }

        // Next, handle comments
        if (c == '/') {
            if (self.buffer[self.position] == '/') {
                // Skip to the end of the line
                while (self.buffer[self.position] != '\n') {
                    self.position += 1;
                }
                self.line += 1;
                self.column = 1;
                continue;
            } else if (self.buffer[self.position] == '*') {
                // Skip to the end of the block comment
                while (self.buffer[self.position] != '*' and self.buffer[self.position + 1] != '/') {
                    if (self.buffer[self.position] == '\n') {
                        self.line += 1;
                        self.column = 1;
                    } else {
                        self.column += 1;
                    }
                    self.position += 1;
                }
                self.position += 2;
                continue;
            }
        }

        // Next, keywords
        if (std.ascii.isAlphanumeric(c)) {
            var keyword = try allocator.alloc(u8, 1);
            keyword[0] = c;
            var i: u16 = 1;
            while (std.ascii.isAlphanumeric(self.buffer[self.position])) {
                keyword = try allocator.realloc(keyword, i + 1);
                keyword[i] = self.buffer[self.position];
                i += 1;

                self.position += 1;
                self.column = 1;
            }

            const token = Token.keyword_map.get(keyword);
            if (token != null) {
                allocator.free(keyword);
                return .{ .token = token.?, .line = self.line, .column = self.column };
            }

            // If it's not a keyword, it's an identifier
            return .{ .token = Token{ .Identifier = keyword }, .line = self.line, .column = self.column };
        }

        // Next, numbers
        if (std.ascii.isDigit(c)) {
            var number: u64 = 0;
            while (std.ascii.isDigit(self.buffer[self.position])) {
                number = number * 10 + (self.buffer[self.position] - '0');
                self.position += 1;
                self.column = 1;
            }

            return .{ .token = Token{ .IntLiteral = number }, .line = self.line, .column = self.column };
        }

        // Next, strings
        if (c == '"' or c == '\'') {
            var string = try allocator.alloc(u8, 0);
            var isEscaping = false;
            while (self.buffer[self.position] != c or isEscaping) {
                if (isEscaping) {
                    isEscaping = false;

                    // Handle escape sequences
                    const escapeChar = escapeSequences.get(self.buffer[self.position .. self.position + 1]);
                    if (escapeChar != null) {
                        string = try allocator.realloc(string, string.len + 1);
                        string[string.len - 1] = escapeChar.?;
                    } else {
                        // If it's not a valid escape sequence, just print both characters
                        string = try allocator.realloc(string, string.len + 2);
                        string[string.len - 2] = self.buffer[self.position - 1];
                        string[string.len - 1] = self.buffer[self.position];
                    }
                } else if (self.buffer[self.position] == '\\') {
                    isEscaping = true;
                } else {
                    string = try allocator.realloc(string, string.len + 1);
                    string[string.len - 1] = self.buffer[self.position];
                }

                self.position += 1;
                self.column += 1;
                if (self.position >= self.buffer.len) break;

                if (self.buffer[self.position] == '\n') {
                    self.line += 1;
                    self.column = 1;
                }
            }
            self.position += 1;

            return .{ .token = Token{ .StringLiteral = string }, .line = self.line, .column = self.column };
        }

        // Next, symbols.
        // This could be more dynamic to support longer-than-2-character symbols, but it works for now.
        var maxLength: usize = Token.maxSymbolLength;
        if (self.position + maxLength > self.buffer.len) {
            maxLength = self.buffer.len - self.position + 1;
        }

        var symbol = try allocator.alloc(u8, maxLength);
        defer allocator.free(symbol);
        for (0..maxLength) |i| {
            symbol[i] = self.buffer[self.position - 1 + i];
        }

        // Try progressively shorter symbols until we find a match
        while (symbol.len > 0) {
            const token = Token.symbol_map.get(symbol);
            if (token != null) {
                self.position += symbol.len - 1;
                return .{ .token = token.?, .line = self.line, .column = self.column };
            }

            symbol = try allocator.realloc(symbol, symbol.len - 1);
        }

        // If we got here, it's an unknown token
        return .{ .token = Token{ .Error = try std.fmt.allocPrint(allocator, "Unexpected character: {u}", .{c}) }, .line = self.line, .column = self.column };
    }

    return .{ .token = Token.EOF, .line = self.line, .column = self.column };
}
