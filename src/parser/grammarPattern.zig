const TokenType = @import("../token.zig").TokenType;
const TokenData = @import("../token.zig").TokenData;
const AST = @import("../ast.zig").AST;
const std = @import("std");
const ArgsFlags = @import("../args_parser.zig").ArgsFlags;
const repeat = @import("grammar.zig").repeat;

pub const GrammarPattern = @This();

pub const PatternType = enum {
    All,
    OneOf,
    AtLeastOne,
};

pub const GrammarPatternElement = struct {
    type: union(enum) { Token: TokenType, PatternId: []const u8 },

    debugName: []const u8,

    pub fn check(self: GrammarPatternElement, flags: ArgsFlags, prevIndentString: []const u8, remainingTokens: []TokenData, parserPatterns: *std.StringHashMap(GrammarPattern), allocator: std.mem.Allocator) u32 {
        const indentString = std.mem.concat(allocator, u8, &[_][]const u8{ prevIndentString, "\x1b[1m│\x1b[22m \x1b[33m" }) catch ""; // Yellow
        defer allocator.free(indentString);

        if (flags.extremely_verbose) std.debug.print("{s}└─\x1b[0m Checking '{s}' pattern\n", .{ indentString, self.debugName });

        switch (self.type) {
            .Token => |token| {
                if (!std.mem.eql(u8, @tagName(token), @tagName(remainingTokens[0].token))) {
                    if (flags.verbose) std.debug.print("{s}└─\x1b[0m TOP-LEVEL FAILURE: Expected token '{s}', got '{s}' - Failed to consume pattern '{s}'\n", .{ indentString, token.typeNameString(), remainingTokens[0].token.typeNameString(), self.debugName });
                    return 0;
                }
                return 1;
            },
            .PatternId => |patternId| {
                const pattern = parserPatterns.get(patternId) orelse return 0;
                const result = pattern.check(flags, indentString, remainingTokens, parserPatterns, allocator);
                if (result == 0 and flags.verbose) {
                    std.debug.print("{s}└─\x1b[0m\x1b[30m Failed to consume pattern '{s}'\n", .{ indentString, patternId });
                }
                return result;
            },
        }
    }

    pub fn consumeIfExist(self: GrammarPatternElement, flags: ArgsFlags, remainingTokens: []TokenData, allocator: std.mem.Allocator, parserPatterns: *std.StringHashMap(GrammarPattern)) anyerror!?ConsumeResult {
        if (flags.extremely_verbose) std.debug.print("Consuming '{s}' pattern\n", .{self.debugName});

        switch (self.type) {
            .Token => |token| {
                if (!std.mem.eql(u8, @tagName(token), @tagName(remainingTokens[0].token))) {
                    return null;
                }
                return .{ .consumed = 1, .asts = null };
            },
            .PatternId => |patternId| {
                const pattern = parserPatterns.get(patternId) orelse return null;
                return try pattern.consumeIfExist(flags, remainingTokens, allocator, parserPatterns) orelse return null;
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
/// NOTE: If an AST is passed and not used, it _must_ be freed.
getAST: *const fn (self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*const AST,
debugName: []const u8,

pub fn create(comptime patternType: PatternType, comptime elements: []const GrammarPatternElement, comptime getAST: fn (self: GrammarPattern, patternASTs: []*const AST, tokens: []TokenData, allocator: std.mem.Allocator) anyerror!?*const AST, comptime debugName: []const u8) GrammarPattern {
    return .{ .elements = elements, .patternType = patternType, .getAST = getAST, .debugName = debugName };
}

pub fn check(self: *const GrammarPattern, flags: ArgsFlags, prevIndentString: []const u8, remainingTokens: []TokenData, parserPatterns: *std.StringHashMap(GrammarPattern), allocator: std.mem.Allocator) u32 {
    const indentString = std.mem.concat(allocator, u8, &[_][]const u8{ prevIndentString, "\x1b[1m│\x1b[22m \x1b[36m" }) catch ""; // Cyan
    defer allocator.free(indentString);

    if (flags.extremely_verbose) std.debug.print("{s}└─\x1b[0m\x1b[36m Checking '{s}' pattern\x1b[0m\n", .{ indentString, self.debugName });

    switch (self.patternType) {
        PatternType.All => {
            var tokenIndex: u32 = 0;
            for (self.elements) |element| {
                const consumed = element.check(flags, indentString, remainingTokens[tokenIndex..], parserPatterns, allocator);
                if (consumed == 0) {
                    if (flags.verbose) std.debug.print("{s}└─\x1b[0m\x1b[30m Failed to consume 'All' pattern '{s}' because of element '{s}'\x1b[0m\n", .{ indentString, self.debugName, element.debugName });
                    return 0;
                }
                tokenIndex += consumed;
                if (flags.verbose) std.debug.print("{s}└─\x1b[0m\x1b[32m Consumed '{s}' pattern\x1b[0m\n", .{ indentString, element.debugName });
            }
            return tokenIndex;
        },
        PatternType.AtLeastOne => {
            var tokenIndex: u32 = 0;
            while (tokenIndex < remainingTokens.len) {
                var consumed: u32 = 0;
                for (self.elements) |element| {
                    const elementConsumed = element.check(flags, indentString, remainingTokens[tokenIndex..], parserPatterns, allocator);
                    if (elementConsumed == 0) break;
                    consumed += elementConsumed;
                }
                if (consumed == 0) {
                    if (flags.verbose) std.debug.print("{s}└─\x1b[0m\x1b[30m Failed to consume any 'AtLeastOne' pattern '{s}'\x1b[0m\n", .{ indentString, self.debugName });
                    return tokenIndex;
                }
                tokenIndex += consumed;
                if (flags.verbose) std.debug.print("{s}└─\x1b[0m\x1b[32m Consumed '{s}' pattern\x1b[0m\n", .{ indentString, self.debugName });
            }
            return tokenIndex;
        },
        PatternType.OneOf => {
            for (self.elements) |element| {
                const consumed = element.check(flags, indentString, remainingTokens, parserPatterns, allocator);
                if (consumed != 0) {
                    if (flags.verbose) {
                        std.debug.print("{s}└─\x1b[0m\x1b[32m Consumed '{s}' pattern\x1b[0m\n", .{ indentString, element.debugName });
                    }
                    return consumed;
                }
            }
            if (flags.verbose) std.debug.print("{s}└─\x1b[0m\x1b[30m Failed to consume any 'OneOf' patterns in '{s}'\x1b[0m\n", .{ indentString, self.debugName });
            return 0;
        },
    }
}

pub fn consumeIfExist(self: *const GrammarPattern, flags: ArgsFlags, remainingTokens: []TokenData, allocator: std.mem.Allocator, parserPatterns: *std.StringHashMap(GrammarPattern)) !?ConsumeResult {
    if (self.check(flags, "\x1b[31m> ", remainingTokens, parserPatterns, allocator) == 0) {
        return null;
    }

    if (flags.extremely_verbose) std.debug.print("\x1b[0mConsuming '{s}' pattern\n", .{self.debugName});

    switch (self.patternType) {
        PatternType.All => {
            var consumed: usize = 0;
            var asts = std.ArrayList(*const AST).init(allocator);
            if (flags.verbose) std.debug.print("Consuming 'All' pattern {s}\n", .{self.debugName});
            for (self.elements) |element| {
                const resultErr = try element.consumeIfExist(flags, remainingTokens[consumed..], allocator, parserPatterns);
                if (resultErr == null or resultErr.?.consumed == 0) {
                    if (flags.verbose) std.debug.print("Failed to consume 'All' pattern '{s}' because of element '{s}'\n", .{ self.debugName, element.debugName });
                    return null;
                }
                const result = resultErr.?;

                consumed += result.consumed;
                if (result.asts != null) {
                    if (flags.verbose) std.debug.print("Consumed {d} tokens for pattern '{s}'\n", .{ result.consumed, element.debugName });
                    for (result.asts.?) |ast| {
                        try asts.append(ast);
                    }
                    allocator.free(result.asts.?);
                }
            }

            const astSlice = try asts.toOwnedSlice();
            defer allocator.free(astSlice); // Intentionally not freeing the ASTs themselves, as they may be used by getAST

            const selfAST = try self.getAST(self.*, astSlice, remainingTokens[0..consumed], allocator);
            if (selfAST == null) {
                return .{ .consumed = consumed, .asts = &[_]*AST{} };
            }
            const resultASTs = try allocator.alloc(*const AST, 1);
            resultASTs[0] = selfAST.?;
            return .{ .consumed = consumed, .asts = resultASTs };
        },
        PatternType.AtLeastOne => {
            var consumed: usize = 0;
            var asts = std.ArrayList(*const AST).init(allocator);

            if (flags.verbose) std.debug.print("Consuming at least one pattern '{s}'\n", .{self.elements[0].debugName});
            while (consumed < remainingTokens.len) {
                if (flags.verbose) std.debug.print("Consuming at least one pattern '{s}'' at index {d}\n", .{ self.elements[0].debugName, consumed });

                var elementConsumed: usize = 0;
                for (self.elements) |element| {
                    const result = try element.consumeIfExist(flags, remainingTokens[consumed..], allocator, parserPatterns) orelse continue;
                    if (flags.verbose) std.debug.print("Consumed {d} tokens for pattern '{s}'\n", .{ result.consumed, element.debugName });
                    elementConsumed += result.consumed;
                    if (result.asts != null) {
                        for (result.asts.?) |ast| {
                            try asts.append(ast);
                        }
                        allocator.free(result.asts.?);
                    }
                }
                if (elementConsumed == 0) break;
                consumed += elementConsumed;
            }

            const astSlice = try asts.toOwnedSlice();
            if (flags.verbose) std.debug.print("Consumed {d} tokens for pattern '{s}'\n", .{ consumed, self.elements[0].debugName });

            const selfAST = try self.getAST(self.*, astSlice, remainingTokens[0..consumed], allocator);
            if (selfAST == null) {
                if (flags.verbose) std.debug.print("Failed to get AST for pattern '{s}'\n", .{self.elements[0].debugName});
                return .{ .consumed = consumed, .asts = &[_]*AST{} };
            }
            const resultASTs = try allocator.alloc(*const AST, 1);
            resultASTs[0] = selfAST.?;
            return .{ .consumed = consumed, .asts = resultASTs };
        },
        PatternType.OneOf => {
            if (flags.verbose) {
                std.debug.print("Consuming one of patterns:", .{});
                for (self.elements) |element| {
                    std.debug.print(" {s},", .{element.debugName});
                }
                std.debug.print("\n", .{});
            }

            for (self.elements) |element| {
                if (flags.verbose) std.debug.print("When consuming '{s}'' | remaining tokens: {d}\n", .{ element.debugName, remainingTokens.len });
                const result = try element.consumeIfExist(flags, remainingTokens, allocator, parserPatterns);
                if (result != null) {
                    if (flags.verbose) std.debug.print("Consumed {d} tokens for pattern '{s}'\n", .{ result.?.consumed, element.debugName });
                    return result;
                }
            }
            if (flags.verbose) std.debug.print("Failed to consume any patterns\n", .{});
            return null;
        },
    }
}

pub fn printAsCFG(self: *const GrammarPattern, indent: u32, allocator: std.mem.Allocator) void {
    const indentString = repeat("  ", indent, allocator) catch "";
    defer allocator.free(indentString);

    std.debug.print("{s}{s} -> ", .{ indentString, self.debugName });
    switch (self.patternType) {
        PatternType.All => {
            var i: usize = 0;
            while (i < self.elements.len) : (i += 1) {
                const element = self.elements[i];
                if (i == 0) {
                    std.debug.print("\x1b[32m{s}\x1b[0m", .{element.debugName});
                } else {
                    std.debug.print(" + \x1b[32m{s}\x1b[0m", .{element.debugName});
                }
            }
        },
        PatternType.AtLeastOne => {
            var i: usize = 0;
            while (i < self.elements.len) : (i += 1) {
                const element = self.elements[i];
                if (i == 0) {
                    std.debug.print("\x1b[31m{s}\x1b[0m", .{element.debugName});
                } else {
                    std.debug.print(" | \x1b[31m{s}\x1b[0m", .{element.debugName});
                }
            }
            std.debug.print(" | \x1b[31m{s}\x1b[0m + \x1b[31m{s}\x1b[0m", .{ self.debugName, self.debugName });
        },
        PatternType.OneOf => {
            var i: usize = 0;
            while (i < self.elements.len) : (i += 1) {
                const element = self.elements[i];
                if (i == 0) {
                    std.debug.print("\x1b[33m{s}\x1b[0m", .{element.debugName});
                } else {
                    std.debug.print(" | \x1b[33m{s}\x1b[0m", .{element.debugName});
                }
            }
        },
    }
    std.debug.print("\n", .{});
}
