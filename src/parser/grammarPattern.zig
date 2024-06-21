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
    getAST: *const fn (childASTs: []*const AST, allocator: std.mem.Allocator) anyerror!?*const AST,

    pub fn check(self: GrammarPatternElement, remainingTokens: []TokenData) u32 {
        switch (self.type) {
            .Token => |token| {
                if (!std.mem.eql(u8, @tagName(token), @tagName(remainingTokens[0].token))) {
                    return 0;
                }
                return 1;
            },
            .Pattern => |pattern| {
                return pattern.check(remainingTokens);
            },
        }
    }

    pub fn consumeIfExist(self: GrammarPatternElement, remainingTokens: []TokenData, allocator: std.mem.Allocator) anyerror!?ConsumeResult {
        switch (self.type) {
            .Token => |token| {
                if (!std.mem.eql(u8, @tagName(token), @tagName(remainingTokens[0].token))) {
                    return null;
                }
                return .{ .consumed = 1, .asts = null };
            },
            .Pattern => |pattern| {
                const result = try pattern.consumeIfExist(remainingTokens, allocator) orelse return null;
                // Empty array if no ASTs
                var childASTs: []*const AST = undefined;
                if (result.asts != null) {
                    childASTs = result.asts.?;
                } else {
                    childASTs = &[_]*const AST{};
                }

                const selfASTs = try allocator.alloc(*const AST, result.consumed);
                const ast = try self.getAST(childASTs, allocator);
                if (ast == null) {
                    allocator.free(selfASTs);
                    return .{ .consumed = result.consumed, .asts = &[_]*AST{} };
                }
                selfASTs[0] = ast.?;
                return .{ .consumed = result.consumed, .asts = selfASTs };
            },
        }
    }
};

pub const ConsumeResult = struct {
    consumed: usize,
    asts: ?[]*const AST,
};

elements: []const GrammarPatternElement,
patternType: PatternType,
getAST: *const fn (self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*const AST,

pub fn create(comptime patternType: PatternType, comptime elements: []const GrammarPatternElement, comptime getAST: fn (self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*const AST) GrammarPattern {
    return .{ .elements = elements, .patternType = patternType, .getAST = getAST };
}

pub fn check(self: *const GrammarPattern, remainingTokens: []TokenData) u32 {
    var tokenIndex: u32 = 0;
    for (self.elements) |element| {
        const consumed = element.check(remainingTokens[tokenIndex..]);
        if (consumed == 0) return 0;
        tokenIndex += consumed;
    }
    return tokenIndex;
}

pub fn consumeIfExist(self: *const GrammarPattern, remainingTokens: []TokenData, allocator: std.mem.Allocator) !?ConsumeResult {
    if (self.check(remainingTokens) == 0) {
        return null;
    }

    var consumed: usize = 0;
    var asts = std.ArrayList(*const AST).init(allocator);
    for (self.elements) |element| {
        const result = try element.consumeIfExist(remainingTokens[consumed..], allocator) orelse return null;
        consumed += result.consumed;
        if (result.asts != null) {
            for (result.asts.?) |ast| {
                try asts.append(ast);
            }
        }
    }

    const astSlice = try asts.toOwnedSlice();
    const selfAST = try self.getAST(self.*, astSlice, remainingTokens[0..consumed], allocator);
    if (selfAST == null) {
        return .{ .consumed = consumed, .asts = &[_]*AST{} };
    }
    const resultASTs = try allocator.alloc(*const AST, 1);
    resultASTs[0] = selfAST.?;
    return .{ .consumed = consumed, .asts = resultASTs };
}
