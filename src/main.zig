const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const args_parser = @import("args_parser.zig");
const Token = @import("token.zig").Token;
const TokenData = @import("token.zig").TokenData;

pub fn pretty_error(message: []const u8, source: []const u8) !void {
    const stderr_file = std.io.getStdErr().writer();
    var bw_err = std.io.bufferedWriter(stderr_file);
    const stderr = bw_err.writer();
    defer bw_err.flush() catch |err| {
        std.debug.panic("Failed to flush stderr: {any}\n", .{err});
    };

    try stderr.print("\x1b[1;31m Error: {s} (from: {s}) \x1b[0m\n", .{ message, source });
}

pub fn main() !void {
    // A general purpose allocator -- needed for nearly everything requiring memory allocation
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    // Get the standard output file
    const stdout_file = std.io.getStdOut().writer();
    // Temporary: Don't buffer the output
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();
    const stdout = stdout_file;
    // defer bw.flush() catch |err| {
    //     std.debug.panic("Failed to flush stdout: {any}\n", .{err});
    // };

    // Print the process arguments
    const args = try args_parser.parse(allocator);
    defer args.deinit();

    // Open the file in the arguments of the program
    const file_path = args.file_path;
    const file = std.fs.cwd().openFile(file_path, .{ .mode = .read_only }) catch |err|
        switch (err) {
        error.FileNotFound => {
            try stdout.print("File not found: {s}\n", .{ .name_string = file_path });
            return;
        },
        else => return err,
    };

    defer file.close();

    const buffer = try file.readToEndAllocOptions(allocator, std.math.maxInt(usize), null, @alignOf(u8), 0);
    defer allocator.free(buffer);

    var lxr = lexer.init(buffer);

    var tokens = std.ArrayList(TokenData).init(allocator);
    defer {
        for (tokens.items) |tokenData| {
            tokenData.deinit(&allocator);
        }
        tokens.deinit();
    }

    if (args.flags.debug_tokens) {
        try stdout.print("Tokens:\n", .{});
    }

    while (true) {
        const tokenData = try lxr.getNext(allocator);

        if (tokenData.token == Token.EOF) {
            break;
        }
        if (tokenData.token == Token.Error) {
            return try pretty_error(try std.fmt.allocPrint(allocator, "\x1b[1;31m Error: {d}:{d} - {s} \x1b[0m\n", .{ tokenData.line, tokenData.column, tokenData.token.Error }), "Lexing step");
        }

        if (args.flags.debug_tokens) {
            const tokenString = try tokenData.token.toStringWithType(allocator);
            defer allocator.free(tokenString);
            try stdout.print("{s}", .{tokenString});
        }

        try tokens.append(tokenData);
    }

    if (args.flags.debug_tokens) {
        try stdout.print("\n\n", .{});
    }

    var prsr = parser.init(tokens.items, allocator, file_path);
    defer prsr.deinit();

    const ast = prsr.parse() catch {
        return try pretty_error("Unexpected error parsing the AST\n", "Parsing step");
    };

    if (args.flags.debug_ast) {
        try stdout.print("AST:\n", .{});
        try ast.print(&stdout.any(), 0);
        std.debug.print("\n", .{});
    }
}
