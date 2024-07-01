const std = @import("std");
const prettyError = @import("errors.zig").prettyError;

pub const ArgsFlags = struct {
    /// If the tokens should be printed to stdout
    debugTokens: bool = false,
    /// If the AST should be printed to stdout
    debugAST: bool = false,

    /// If we should avoid parsing the tokens to an AST
    stopAfterTokens: bool = false,
    /// If the parser should print debug information
    verbose: bool = false,
    /// If the parser should print extremely verbose information
    extremelyVerbose: bool = false,
};
pub const Args = struct {
    mode: union(enum) {
        RunFile: []const u8,
        RunRepl,
    },
    flags: ArgsFlags,

    allocator: std.mem.Allocator,
    pub fn deinit(self: *const Args) void {
        switch (self.mode) {
            .RunFile => self.allocator.free(self.mode.RunFile),
            .RunRepl => {},
        }
    }
};

pub const ArgParseError = error{MultiplePathsProvided};

pub fn parse(allocator: std.mem.Allocator) !Args {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var filePath: ?[]u8 = null;
    var flags = ArgsFlags{};

    // Skip the first argument, which is the program name
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--debug-tokens")) {
            flags.debugTokens = true;
        } else if (std.mem.eql(u8, arg, "--debug-ast")) {
            flags.debugAST = true;
        } else if (std.mem.eql(u8, arg, "--stop-after-tokens")) {
            flags.stopAfterTokens = true;
        } else if (std.mem.eql(u8, arg, "--verbose")) {
            flags.verbose = true;
        } else if (std.mem.eql(u8, arg, "--extremely-verbose")) {
            flags.verbose = true;
            flags.extremelyVerbose = true;
        } else {
            if (filePath != null) {
                try prettyError(try std.fmt.allocPrint(allocator, "Usage: {s} [<file>]\n", .{args[0]}));
                try prettyError(try std.fmt.allocPrint(allocator, "Multiple paths provided: {s} and {s}\n", .{ filePath.?, arg }));
                return ArgParseError.MultiplePathsProvided;
            }

            filePath = arg;
        }
    }

    if (filePath == null) {
        return Args{ .mode = .RunRepl, .flags = flags, .allocator = allocator };
    }

    // We clone filePath so we don't need to worry about the lifetime of the arguments
    const clonedFilePath: []u8 = try allocator.alloc(u8, filePath.?.len);
    std.mem.copyForwards(u8, clonedFilePath, filePath.?);

    return Args{ .mode = .{ .RunFile = clonedFilePath }, .flags = flags, .allocator = allocator };
}
