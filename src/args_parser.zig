const std = @import("std");
const pretty_error = @import("main.zig").pretty_error;

pub const ArgsFlags = struct {
    /// If the tokens should be printed to stdout
    debug_tokens: bool = false,
    /// If the AST should be printed to stdout
    debug_ast: bool = false,
    /// If the grammar rules should be printed to stdout
    debug_rules: bool = false,

    /// If we should avoid parsing the tokens to an AST
    stop_after_tokens: bool = false,
    /// If the parser should print debug information
    verbose: bool = false,
    /// If the parser should print extremely verbose information
    extremely_verbose: bool = false,
};
const Args = struct {
    file_path: []const u8,
    flags: ArgsFlags,

    allocator: std.mem.Allocator,
    pub fn deinit(self: *const Args) void {
        self.allocator.free(self.file_path);
    }
};

const ArgParseError = error{ NoFileProvided, MultiplePathsProvided };

pub fn parse(allocator: std.mem.Allocator) !Args {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var file_path: ?[]u8 = null;
    var flags = ArgsFlags{};

    // Skip the first argument, which is the program name
    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--debug-tokens")) {
            flags.debug_tokens = true;
        } else if (std.mem.eql(u8, arg, "--debug-ast")) {
            flags.debug_ast = true;
        } else if (std.mem.eql(u8, arg, "--debug-rules")) {
            flags.debug_rules = true;
        } else if (std.mem.eql(u8, arg, "--stop-after-tokens")) {
            flags.stop_after_tokens = true;
        } else if (std.mem.eql(u8, arg, "--verbose")) {
            flags.verbose = true;
        } else if (std.mem.eql(u8, arg, "--extremely-verbose")) {
            flags.verbose = true;
            flags.extremely_verbose = true;
        } else {
            if (file_path != null) {
                try pretty_error("Multiple file paths provided\n");
                return ArgParseError.MultiplePathsProvided;
            }

            file_path = arg;
        }
    }

    if (file_path == null) {
        try pretty_error(try std.fmt.allocPrint(allocator, "Usage: {s} <file>\n", .{ .name_string = args[0] }));
        return ArgParseError.NoFileProvided;
    }

    // Clone file_path into a new buffer
    const cloned_file_path: []u8 = try allocator.alloc(u8, file_path.?.len);
    std.mem.copyForwards(u8, cloned_file_path, file_path.?);

    return Args{ .file_path = cloned_file_path, .flags = flags, .allocator = allocator };
}
