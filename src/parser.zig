const TokenData = @import("token.zig").TokenData;
const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const pretty_error = @import("main.zig").pretty_error;
const ArgsFlags = @import("args_parser.zig").ArgsFlags;

const Parser = @This();

tokens: []TokenData,
allocator: std.mem.Allocator,
file_name: []const u8,
flags: ArgsFlags,

pub const ParseError = error{Unknown};

pub fn init(tokens: []TokenData, flags: ArgsFlags, allocator: std.mem.Allocator, file_name: []const u8) !Parser {
    return .{ .tokens = tokens, .flags = flags, .allocator = allocator, .file_name = file_name };
}

pub fn parse(self: *Parser) anyerror!void {
    // Temporary
    _ = self;
    return ParseError.Unknown;
}

pub fn deinit(self: *Parser) void {
    _ = self;
}
