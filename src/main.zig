const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("token.zig").Token;

pub fn main() !void {
    // A general purpose allocator -- needed for nearly everything requiring memory allocation
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    // Get the standard output file
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    defer bw.flush() catch |err| {
        std.debug.panic("Failed to flush stdout: {any}\n", .{err});
    };

    const stderr_file = std.io.getStdErr().writer();
    var bw_err = std.io.bufferedWriter(stderr_file);
    const stderr = bw_err.writer();
    defer bw_err.flush() catch |err| {
        std.debug.panic("Failed to flush stderr: {any}\n", .{err});
    };

    // Print the process arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // // Temporary: print the arguments to the program
    // for (args) |arg| {
    //     try stdout.print("{s} ", .{arg});
    // }

    if (args.len < 2) {
        try stdout.print("Usage: {s} <file>\n", .{ .name_string = args[0] });
        return;
    }

    // Open the file in the arguments of the program
    const file_path = args[1];
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

    while (true) {
        const tokenData = try lxr.getNext(allocator);
        defer tokenData.deinit(&allocator);

        if (tokenData.token == Token.EOF) {
            break;
        }
        if (tokenData.token == Token.Error) {
            try stderr.print("\x1b[1;31m Error: {d}:{d} - {s} \x1b[0m\n", .{ tokenData.line, tokenData.column, tokenData.token.Error });
            break;
        }

        const tokenString = try tokenData.token.toStringWithType(allocator);
        defer allocator.free(tokenString);
        try stdout.print("{s}", .{tokenString});
    }
}
