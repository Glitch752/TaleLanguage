const TokenType = @import("../token.zig").TokenType;
const AST = @import("../ast.zig").AST;
const std = @import("std");

pub const GrammarPattern = @This();

pub const CustomInterface = struct {
    /// Checks if the start of the tokens match the pattern
    check: fn (*GrammarPattern, []TokenType) bool,
    /// Returns the number of tokens consumed
    consumeIfExist: fn (*GrammarPattern, []TokenType) ConsumeResult,
};

pub const GrammarPatternElement = union(enum) {
    Token: TokenType,
    Pattern: *GrammarPattern,
    CustomInterface: *const CustomInterface,
};

pub const ConsumeResult = struct {
    consumed: usize,
    ast: ?*AST,
};

elements: []GrammarPatternElement,
getAST: fn (*GrammarPattern, []TokenType, allocator: std.mem.Allocator) ?*AST,

pub fn create(elements: []GrammarPatternElement, getAST: (fn (*GrammarPattern, []TokenType, allocator: std.mem.Allocator) ?*AST)) GrammarPattern {
    return .{ .elements = elements, .getAST = getAST };
}

pub fn check(self: *GrammarPattern, remainingTokens: []TokenType) bool {
    var tokenIndex: usize = 0;
    for (self.elements) |element| {
        switch (element) {
            GrammarPatternElement.Token => {
                if (tokenIndex >= remainingTokens.len) {
                    return false;
                }
                if (element.Token != remainingTokens[tokenIndex]) {
                    return false;
                }
                tokenIndex += 1;
            },
            GrammarPatternElement.Pattern => {
                if (!element.Pattern.check(remainingTokens[tokenIndex..])) {
                    return false;
                }
            },
            GrammarPatternElement.CustomInterface => {
                if (!element.CustomInterface.check(self, remainingTokens)) {
                    return false;
                }
            },
        }
    }
    return true;
}

pub fn consumeIfExist(self: *GrammarPattern, remainingTokens: []TokenType) *ConsumeResult {
    if (!self.check(remainingTokens)) {
        return 0;
    }

    var consumed: usize = 0;
    for (self.elements) |element| {
        switch (element) {
            GrammarPatternElement.Token => {
                consumed += 1;
            },
            GrammarPatternElement.Pattern => |pattern| {
                consumed += pattern.consumeIfExist(remainingTokens[consumed..]);
            },
            GrammarPatternElement.CustomInterface => |interface| {
                consumed += interface.consumeIfExist(self, remainingTokens[consumed..]);
            },
        }
    }
    return .{ .consumed = consumed, .ast = self.getAST(remainingTokens[0..consumed]) };
}

pub const oneOf: GrammarPatternElement = .{ .CustomInterface = oneOfCustomInterface };
const oneOfCustomInterface = &CustomInterface{
    .check = oneOfCheck,
    .consumeIfExist = oneOfConsumeIfExist,
};
fn oneOfCheck(self: *GrammarPattern, remainingTokens: []TokenType) bool {
    for (self.elements) |element| {
        if (element.check(remainingTokens)) {
            return true;
        }
    }
    return false;
}
fn oneOfConsumeIfExist(self: *GrammarPattern, remainingTokens: []TokenType) ConsumeResult {
    for (self.elements) |element| {
        if (element.check(remainingTokens)) {
            return element.consumeIfExist(remainingTokens);
        }
    }
    return 0;
}

pub fn atLeastOne(comptime pattern: GrammarPattern) GrammarPatternElement {
    const atLeastOneCustomInterface = &CustomInterface{
        .check = atLeastOneCheck(pattern),
        .consumeIfExist = atLeastOneConsumeIfExist(pattern)
    };
    return .{ .CustomInterface = atLeastOneCustomInterface };
}
// If only...
// I need to figure out a way to make this work with Zig, which doesn't have anonymous functions...
fn atLeastOneCheck(comptime pattern: GrammarPattern) type {
    return fn(self: *GrammarPattern, remainingTokens: []TokenType) bool {
        if (!pattern.check(remainingTokens)) {
            return false;
        }
        return true;
    };
}
fn atLeastOneConsumeIfExist(comptime pattern: GrammarPattern, self: *GrammarPattern, remainingTokens: []TokenType) ConsumeResult {
    var consumed: usize = 0;
    while (pattern.check(remainingTokens[consumed..])) {
        consumed += pattern.consumeIfExist(remainingTokens[consumed..]);
    }
    return .{
        .consumed = consumed,
        .ast = self.getAST(remainingTokens[0..consumed]),
    };
}
