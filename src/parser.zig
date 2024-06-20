const TokenData = @import("token.zig").TokenData;
const std = @import("std");

const Parser = @This();

tokens: []TokenData,
allocator: std.mem.Allocator,
position: usize,

pub fn init(tokens: []TokenData, allocator: std.mem.Allocator) Parser {
    return .{ .tokens = tokens, .allocator = allocator, .position = 0 };
}
