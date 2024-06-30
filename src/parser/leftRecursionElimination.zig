const std = @import("std");
const GrammarPattern = @import("grammarPattern.zig").GrammarPattern;
const PatternType = @import("grammarPattern.zig").PatternType;
const ArgFlags = @import("../args_parser.zig").ArgsFlags;

const NonTerminal = struct { name: []const u8, pattern: *GrammarPattern };

fn range(len: usize) []const u0 {
    return @as([*]u0, undefined)[0..len];
}

pub fn eliminateLeftRecursion(allocator: std.mem.Allocator, patterns: *std.StringHashMap(GrammarPattern), flags: ArgFlags) !void {
    var patterns_as_non_terminals = std.ArrayList(NonTerminal).init(allocator);
    defer patterns_as_non_terminals.deinit();

    var iterator = patterns.iterator();
    while (iterator.next()) |pattern| {
        try patterns_as_non_terminals.append(NonTerminal{ .name = pattern.key_ptr.*, .pattern = pattern.value_ptr });
    }

    var i: usize = 0;
    while (i < patterns_as_non_terminals.items.len) : (i += 1) {
        var j: usize = 0;
        while (j < i) : (j += 1) {
            try solveIndirectLeftRecursion(allocator, &patterns_as_non_terminals, patterns_as_non_terminals.items[i], patterns_as_non_terminals.items[j]);
        }
        try solveDirectLeftRecursion(allocator, &patterns_as_non_terminals, patterns, patterns_as_non_terminals.items[i]);
    }

    if (flags.debug_rules) {
        var rulesIterator = patterns.iterator();
        while (rulesIterator.next()) |pattern| {
            std.debug.print("Pattern: {s}\n", .{pattern.key_ptr});
            pattern.value_ptr.printAsCFG(1, allocator);
        }
    }
}

fn solveIndirectLeftRecursion(
    allocator: std.mem.Allocator,
    // The list of non-terminals
    patterns_as_non_terminals: *std.ArrayList(NonTerminal),
    pattern: NonTerminal,
    other_pattern: NonTerminal,
) !void {
    _ = allocator;
    _ = patterns_as_non_terminals;

    switch (pattern.pattern.patternType) {
        PatternType.All => {
            // If the first element of the pattern is the other pattern
            std.debug.assert(pattern.pattern.elements.len > 0);

            switch (pattern.pattern.elements[0].type) {
                .Token => {
                    // Indirect left recursion is not possible
                },
                .PatternId => |patternId| {
                    if (std.mem.eql(u8, patternId, other_pattern.name)) {
                        // Indirect left recursion is possible
                    }
                },
            }
        },
        else => {
            // TODO
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
) !void {
    const pattern_name = pattern.name;

    const new_pattern_name = try allocator.alloc(u8, pattern_name.len + 1);
    errdefer allocator.free(new_pattern_name);

    std.mem.copyForwards(u8, new_pattern_name, pattern_name);
    new_pattern_name[pattern_name.len] = '\'';

    var alpha = std.ArrayList(GrammarPattern).init(allocator);
    defer alpha.deinit();

    var beta = std.ArrayList(GrammarPattern).init(allocator);
    defer beta.deinit();

    switch (pattern.pattern.patternType) {
        .All => {},
        .AtLeastOne => {},
        .OneOf => {},
    }

    if (alpha.items.len == 0) {
        return;
    }

    if (beta.items.len == 0) {}

    std.debug.print("New pattern name: {s}\n", .{new_pattern_name});

    _ = patterns_as_non_terminals;
    _ = patterns;

    // Temporary
    allocator.free(new_pattern_name);
}
