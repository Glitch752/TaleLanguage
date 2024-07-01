const std = @import("std");

const TokenType = @import("../token.zig").TokenType;
const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;

const ArgsFlags = @import("../args_parser.zig").ArgsFlags;

const Expression = @import("./expression.zig").Expression;
const ASTPrinter = @import("./ast_printer.zig").ASTPrinter;

const Parser = @This();

tokens: []Token,
allocator: std.mem.Allocator,
flags: ArgsFlags,

pub const ParseError = error{Unknown};

pub fn init(tokens: []Token, flags: ArgsFlags, allocator: std.mem.Allocator) !Parser {
    return .{ .tokens = tokens, .flags = flags, .allocator = allocator };
}

pub fn parse(self: *Parser) anyerror!void {
    // Temporary

    std.debug.print("Parser not implemented yet\n", .{});

    const expression = Expression{
        .Binary = .{
            //
            .left = &Expression{ .Unary = .{
                .operator = Token{ .type = TokenType.Minus, .lexeme = "-", .literal = TokenLiteral.None, .position = 0 },
                .right = &Expression{ .Literal = .{ .value = .{ .NumberLiteral = 123 } } },
            } },
            .operator = Token{ .type = TokenType.Star, .lexeme = "*", .literal = TokenLiteral.None, .position = 0 },
            .right = &Expression{ .Grouping = .{
                .expression = &Expression{ .Literal = .{ .value = .{ .NumberLiteral = 45.67 } } },
            } },
        },
    };

    const printer = ASTPrinter.init(self.allocator);
    try printer.print(expression);

    return;
}

pub fn deinit(self: *Parser) void {
    _ = self;
}
