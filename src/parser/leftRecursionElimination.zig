const std = @import("std");
const GrammarPattern = @import("grammarPattern.zig").GrammarPattern;
const GrammarPatternElement = @import("grammarPattern.zig").GrammarPatternElement;
const PatternType = @import("grammarPattern.zig").PatternType;
const ArgFlags = @import("../args_parser.zig").ArgsFlags;

const NonTerminal = struct {
    name: []const u8,
    rules: std.ArrayList(
    // All required patterns
    std.ArrayList(GrammarPatternElement)),
};

pub fn eliminateLeftRecursion(allocator: std.mem.Allocator, patterns: *std.StringHashMap(GrammarPattern), flags: ArgFlags) !void {
    var patterns_as_non_terminals = std.ArrayList(NonTerminal).init(allocator);
    defer patterns_as_non_terminals.deinit();

    var iterator = patterns.iterator();
    while (iterator.next()) |patternEntry| {
        const pattern = patternEntry.value_ptr;

        switch (pattern.patternType) {
            .All => {
                // Convert each pattern to a non-terminal and make this non-terminal have one rule -- all the elements
                var non_terminal = NonTerminal{
                    .name = patternEntry.key_ptr.*,
                    .rules = std.ArrayList(std.ArrayList(GrammarPatternElement)).init(allocator),
                };

                var rule = std.ArrayList(GrammarPatternElement).init(allocator);

                for (pattern.elements) |element| {
                    try rule.append(element);
                }

                try non_terminal.rules.append(rule);
                try patterns_as_non_terminals.append(non_terminal);
            },
            .OneOf => {
                // The simplest case -- each element is a rule
                var non_terminal = NonTerminal{
                    .name = patternEntry.key_ptr.*,
                    .rules = std.ArrayList(std.ArrayList(GrammarPatternElement)).init(allocator),
                };

                for (pattern.elements) |element| {
                    var rule = std.ArrayList(GrammarPatternElement).init(allocator);
                    try rule.append(element);

                    try non_terminal.rules.append(rule);
                }

                try patterns_as_non_terminals.append(non_terminal);
            },
            .AtLeastOne => {
                // A bit trickier. There is one rule for each element, and one rule for itself repeated.
                var non_terminal = NonTerminal{
                    .name = patternEntry.key_ptr.*,
                    .rules = std.ArrayList(std.ArrayList(GrammarPatternElement)).init(allocator),
                };

                var self_rule = std.ArrayList(GrammarPatternElement).init(allocator);
                try self_rule.appendNTimes(GrammarPatternElement{ .type = .{ .PatternId = patternEntry.key_ptr.* }, .debugName = "" }, 2);

                try non_terminal.rules.append(self_rule);

                for (pattern.elements) |element| {
                    var rule = std.ArrayList(GrammarPatternElement).init(allocator);
                    try rule.append(element);

                    try non_terminal.rules.append(rule);
                }

                try patterns_as_non_terminals.append(non_terminal);
            },
        }
    }

    if (flags.debug_rules) {
        std.debug.print("\n\nPatterns as non-terminals:\n", .{});

        for (patterns_as_non_terminals.items) |pattern| {
            std.debug.print("'\x1b[33m{s}\x1b[0m': ", .{pattern.name});

            std.debug.print("{s} -> ", .{pattern.name});
            for (pattern.rules.items) |rule| {
                for (rule.items) |element| {
                    switch (element.type) {
                        .Token => |token| {
                            std.debug.print("{s} ", .{token.typeNameString()});
                        },
                        .PatternId => |patternId| {
                            std.debug.print("'{s}' ", .{patternId});
                        },
                    }
                }
                std.debug.print("| ", .{});
            }

            std.debug.print("\n", .{});
            for (pattern.rules.items) |rule| { // Zig LSP gets the type wrong here, so no type hint...
                std.debug.print("| OR:\n", .{});
                for (rule.items) |element| {
                    switch (element.type) {
                        .Token => |token| {
                            std.debug.print("| | Token: \x1b[31m{s}\x1b[0m\n", .{token.typeNameString()});
                        },
                        .PatternId => |patternId| {
                            std.debug.print("| | PatternId: \x1b[31m{s}\x1b[0m\n", .{patternId});
                        },
                    }
                }
            }
        }
    }

    // var i: usize = 0;
    // while (i < patterns_as_non_terminals.items.len) : (i += 1) {
    //     var j: usize = 0;
    //     while (j < i) : (j += 1) {
    //         try solveIndirectLeftRecursion(allocator, &patterns_as_non_terminals, patterns_as_non_terminals.items[i], patterns_as_non_terminals.items[j]);
    //     }
    //     try solveDirectLeftRecursion(allocator, &patterns_as_non_terminals, patterns, patterns_as_non_terminals.items[i]);
    // }

    if (flags.debug_rules) {
        std.debug.print("\n\nPatterns generated:\n", .{});

        var rulesIterator = patterns.iterator();
        while (rulesIterator.next()) |pattern| {
            std.debug.print("> Pattern: {s}\n", .{pattern.key_ptr.*});
            pattern.value_ptr.printAsCFG(2, allocator);
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

    var alpha = std.ArrayList(GrammarPatternElement).init(allocator);
    defer alpha.deinit();

    var beta = std.ArrayList(GrammarPatternElement).init(allocator);
    defer beta.deinit();

    // Essentially:
    //   A -> Aa | b
    // Becomes:
    //   A -> bA'
    //   A' -> aA' | ε (empty string)
    for (pattern.rules) |rule| {
        switch (rule.type) {
            .Token => {
                beta.append(rule);
            },
            .PatternId => |patternId| {
                if (std.mem.eql(u8, patternId, pattern_name)) {
                    // TODO
                } else {
                    beta.append(rule);
                }
            },
        }
    }

    if (alpha.items.len == 0) {
        return;
    }

    if (beta.items.len == 0) {}

    const new_pattern_name = try allocator.alloc(u8, pattern_name.len + 1);

    std.mem.copyForwards(u8, new_pattern_name, pattern_name);
    new_pattern_name[pattern_name.len] = '\'';

    std.debug.print("New pattern name: {s}\n", .{new_pattern_name});

    _ = patterns_as_non_terminals;
    _ = patterns;

    // Temporary
    allocator.free(new_pattern_name);
}
