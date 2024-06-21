const TokenType = @import("../token.zig").TokenType;
const AST = @import("../ast.zig").AST;

const GrammarPattern = @This();

pub const CustomInterface = struct {
    /// Checks if the start of the tokens match the pattern
    check: fn (*GrammarPattern, []TokenType) bool,
    /// Returns the number of tokens consumed
    consumeIfExist: fn (*GrammarPattern, []TokenType) ConsumeResult,
};

pub const GrammarPatternElement = union(enum) {
    Token: TokenType,
    Pattern: *GrammarPattern,
    CustomInterface: *CustomInterface,
};

pub const ConsumeResult = struct {
    consumed: usize,
    ast: ?*AST,
};

elements: []GrammarPatternElement,
getAST: fn (*GrammarPattern, []TokenType) ?*AST,

pub fn create(elements: []*GrammarPatternElement, getAST: (fn (*GrammarPattern, []TokenType) ?*AST)) GrammarPattern {
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
