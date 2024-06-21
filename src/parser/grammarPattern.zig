const TokenType = @import("../token.zig").TokenType;
const AST = @import("../ast.zig").AST;
const std = @import("std");
const Closure = @import("./closure.zig").Closure;
const closure = @import("./closure.zig").closure;

pub const GrammarPattern = @This();

const GrammarPatternType = enum {
    All,
    OneOf,
    AtLeastOne,
};

pub const GrammarPatternElement = union(enum) {
    Token: TokenType,
    Pattern: *GrammarPattern,
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

elements: []GrammarPatternElement,
getAST: fn (*GrammarPattern, []TokenType, allocator: std.mem.Allocator) ?*AST,
patternType: GrammarPatternType,

pub fn create(patternType: GrammarPatternType, elements: []GrammarPatternElement, getAST: (fn (*GrammarPattern, []TokenType, allocator: std.mem.Allocator) ?*AST)) GrammarPattern {
    return .{ .elements = elements, .getAST = getAST, .patternType = patternType };
}

pub fn check(self: *GrammarPattern, remainingTokens: []TokenType) bool {
    var tokenIndex: usize = 0;
    for (self.elements) |element| {
        const consumed = element.check(remainingTokens[tokenIndex..]);
        if (consumed == 0) return false;
        tokenIndex += consumed;
    }
    return true;
}

pub fn consumeIfExist(self: *GrammarPattern, remainingTokens: []TokenType) *ConsumeResult {
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
