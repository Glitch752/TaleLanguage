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

pub const GrammarPatternElement = union(enum) {
    Token: TokenType,
    Pattern: *const GrammarPattern,
    getAST: fn (childASTs: []*AST, allocator: std.mem.Allocator) ?*AST,

    pub fn check(self: GrammarPatternElement, remainingTokens: []TokenType) u32 {
        switch (self) {
            GrammarPatternElement.Token => |token| {
                if (token != remainingTokens[0]) {
                    return 0;
                }
                return 1;
            },
            GrammarPatternElement.Pattern => |pattern| {
                return pattern.check(remainingTokens);
            },
        }
    }

    pub fn consumeIfExist(self: GrammarPatternElement, remainingTokens: []TokenType, allocator: std.mem.Allocator) ?ConsumeResult {
        switch (self) {
            GrammarPatternElement.Token => |token| {
                if (token != remainingTokens[0]) {
                    return null;
                }
                return .{ .consumed = 1, .asts = null };
            },
            GrammarPatternElement.Pattern => |pattern| {
                const result = pattern.consumeIfExist(remainingTokens, allocator);
                if (result == null) return null;
                return .{ .consumed = result.consumed, .ast = self.getAST(result.asts, allocator) };
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
getAST: fn (self: GrammarPattern, patternASTs: []*AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*AST,

pub fn create(comptime patternType: PatternType, comptime elements: []const GrammarPatternElement, comptime getAST: fn (self: GrammarPattern, patternASTs: []*AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*AST) GrammarPattern {
    return .{ .elements = elements, .patternType = patternType, .getAST = getAST };
}

pub fn check(self: *GrammarPattern, remainingTokens: []TokenData) bool {
    var tokenIndex: usize = 0;
    for (self.elements) |element| {
        const consumed = element.check(remainingTokens[tokenIndex..]);
        if (consumed == 0) return false;
        tokenIndex += consumed;
    }
    return true;
}

pub fn consumeIfExist(self: *GrammarPattern, remainingTokens: []TokenData) *ConsumeResult {
    if (!self.check(remainingTokens)) {
        return 0;
    }

    var consumed: usize = 0;
    for (self.elements) |element| {
        const result = element.consumeIfExist(remainingTokens[consumed..]);
        consumed += result.consumed;
    }
    return .{ .consumed = consumed, .ast = self.getAST(remainingTokens[0..consumed]) };
}
