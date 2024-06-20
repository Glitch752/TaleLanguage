const std = @import("std");
const io = @import("std").io;
const Token = @import("token.zig").Token;

const Tokenizer = @This();

buffer: []u8,
line: u32,
column: u32,
position: u32,

pub fn init(buffer: []u8) Tokenizer {
    return .{ .buffer = buffer, .line = 1, .column = 1, .position = 0 };
}

pub fn getNext(self: *Tokenizer) Token {
    if (self.position >= self.buffer.len) {
        return Token.EOF;
    }

    // Read the next character
    const c = self.buffer[self.position];
    self.position += 1;

    _ = c;

    // TODO
    return Token.EOF;
}
