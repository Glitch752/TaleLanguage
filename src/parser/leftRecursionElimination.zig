const std = @import("std");
const GrammarPattern = @import("grammarPattern.zig").GrammarPattern;
const PatternType = @import("grammarPattern.zig").PatternType;

const NonTerminal = struct { name: []const u8, pattern: *GrammarPattern };

fn range(len: usize) []const u0 {
    return @as([*]u0, undefined)[0..len];
}

pub fn eliminateLeftRecursion(allocator: std.mem.Allocator, patterns: std.StringHashMap(GrammarPattern)) std.StringHashMap(GrammarPattern) {
    var patterns_as_non_terminals = std.ArrayList(NonTerminal).init(allocator);
    defer patterns_as_non_terminals.deinit();

    for (patterns.iterator()) |pattern| {
        patterns_as_non_terminals.append(NonTerminal{ .name = pattern.key, .pattern = pattern.value });
    }

    var i = 0;
    while (i < patterns_as_non_terminals.items.len) : (i += 1) {
        var j = 0;
        while (j < i) : (j += 1) {
            solveIndirectLeftRecursion(allocator, &patterns_as_non_terminals, patterns.items[i], patterns.items[j]);
        }
        solveDirectLeftRecursion(allocator, &patterns_as_non_terminals, &patterns, patterns.items[i]);
    }
}

fn solveIndirectLeftRecursion(
    allocator: std.mem.Allocator,
    // The list of non-terminals
    patterns_as_non_terminals: *std.ArrayList(NonTerminal),
    pattern: NonTerminal,
    other_pattern: NonTerminal,
) void {
    _ = allocator;
    _ = patterns_as_non_terminals;

    switch (pattern.patternType) {
        PatternType.All => {
            // If the first element of the pattern is the other pattern
            std.debug.assert(pattern.pattern.elements.len > 0);

            switch (pattern.pattern.elements[0].type) {
                .Token => {
                    // Indirect left recursion is not possible
                },
                .PatternId => {
                    if (pattern.pattern.elements[0].patternId == other_pattern.name) {
                        // Indirect left recursion is possible
                    }
                },
            }
        },
    }
}

fn solveDirectLeftRecursion(
    allocator: std.mem.Allocator,
    // The list of non-terminals
    patterns_as_non_terminals: *std.ArrayList(NonTerminal),
    // The hashmap of name to non-terminal
    patterns: *std.StringHashMap(GrammarPattern),
    pattern: NonTerminal,
) void {
    const pattern_name = pattern.name;

    const new_pattern_name = allocator.alloc(u8, pattern_name.len + 1);
    std.mem.copyForwards(u8, new_pattern_name, pattern_name);
    new_pattern_name[pattern_name.len] = '\'';

    std.debug.print("New pattern name: {s}\n", .{new_pattern_name});

    _ = patterns_as_non_terminals;
    _ = patterns;
}
