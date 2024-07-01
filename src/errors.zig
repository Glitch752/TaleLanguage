const std = @import("std");

pub fn displayerror(allocator: std.mem.Allocator, line: isize, message: []const u8) void {
    reportError(allocator, line, "", message);
}

pub fn reportError(allocator: std.mem.Allocator, line: isize, where: []const u8, message: []const u8) void {
    const string = try std.fmt.allocPrint(allocator, "[line {d}] Error{s}: {s}", .{ line, where, message });
    defer allocator.free(string);

    try prettyError(string);
}

pub fn prettyError(message: []const u8) !void {
    const stderrFile = std.io.getStdErr().writer();
    var bwErr = std.io.bufferedWriter(stderrFile);
    const stderr = bwErr.writer();
    defer bwErr.flush() catch |err| {
        std.debug.panic("Failed to flush stderr: {any}\n", .{err});
    };

    try stderr.print("\x1b[1;31m Error: {s}\x1b[0m\n", .{message});
}
