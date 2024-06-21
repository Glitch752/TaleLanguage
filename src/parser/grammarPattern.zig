const TokenType = @import("../token.zig").TokenType;
const TokenData = @import("../token.zig").TokenData;
const AST = @import("../ast.zig").AST;
const std = @import("std");

pub const GrammarPattern = @This();

pub const PatternType = enum {
    All,
    OneOf,
    AtLeastOne,
};

pub const GrammarPatternElement = struct {
    type: union(enum) {
        Token: TokenType,
        Pattern: *const GrammarPattern,
    },
    getAST: *const fn (childASTs: []*AST, allocator: std.mem.Allocator) anyerror!?*AST,

    pub fn check(self: GrammarPatternElement, remainingTokens: []TokenType) u32 {
        switch (self.type) {
            .Token => |token| {
                if (token != remainingTokens[0]) {
                    return 0;
                }
                return 1;
            },
            .Pattern => |pattern| {
                return pattern.check(remainingTokens);
            },
        }
    }

    pub fn consumeIfExist(self: GrammarPatternElement, remainingTokens: []TokenData, allocator: std.mem.Allocator) !?ConsumeResult {
        switch (self.type) {
            .Token => |token| {
                if (@tagName(token) != @tagName(remainingTokens[0].token)) {
                    return null;
                }
                return .{ .consumed = 1, .asts = null };
            },
            .Pattern => |pattern| {
                const result = try pattern.consumeIfExist(remainingTokens, allocator) orelse return null;
                return .{ .consumed = result.consumed, .asts = try self.getAST(result.asts, allocator) };
            },
        }
    }
};

pub const ConsumeResult = struct {
    consumed: usize,
    asts: ?[]*AST,
};

elements: []const GrammarPatternElement,
patternType: PatternType,
getAST: *const fn (self: GrammarPattern, patternASTs: []*AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*AST,

pub fn create(comptime patternType: PatternType, comptime elements: []const GrammarPatternElement, comptime getAST: fn (self: GrammarPattern, patternASTs: []*AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*AST) GrammarPattern {
    return .{ .elements = elements, .patternType = patternType, .getAST = getAST };
}

pub fn check(self: *const GrammarPattern, remainingTokens: []TokenData) bool {
    var tokenIndex: usize = 0;
    for (self.elements) |element| {
        const consumed = element.check(remainingTokens[tokenIndex..]);
        if (consumed == 0) return false;
        tokenIndex += consumed;
    }
    return true;
}

pub fn consumeIfExist(self: *const GrammarPattern, remainingTokens: []TokenData, allocator: std.mem.Allocator) !?*ConsumeResult {
    if (!self.check(remainingTokens)) {
        return null;
    }

    var consumed: usize = 0;
    var asts = std.ArrayList(AST).init(allocator);
    for (self.elements) |element| {
        const result = try element.consumeIfExist(remainingTokens[consumed..], allocator) orelse return null;
        consumed += result.consumed;
        if (result.asts != null) {
            for (result.asts) |ast| {
                try asts.append(ast);
            }
        }
    }

    const astSlice = try asts.toOwnedSlice();
    return .{ .consumed = consumed, .ast = self.getAST(astSlice, remainingTokens[0..consumed], allocator) };
}
