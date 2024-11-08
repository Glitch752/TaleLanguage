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

pub fn repeat(count: usize, character: u8, allocator: std.mem.Allocator) ![]const u8 {
    const buffer = try allocator.alloc(u8, count);
    for (buffer, 0..) |_, i| {
        buffer[i] = character;
    }
    return buffer;
}

pub fn errorContext(buffer: []const u8, filePath: []const u8, position: usize, length: usize, allocator: std.mem.Allocator) !void {
    const stderrFile = std.io.getStdErr().writer();
    var bwErr = std.io.bufferedWriter(stderrFile);
    const stderr = bwErr.writer();
    defer bwErr.flush() catch |err| {
        std.debug.panic("Failed to flush stderr: {any}\n", .{err});
    };

    var lines: u32 = 1;
    var scanPosition: u32 = 0;

    var lineStart: u32 = 0;
    var lineEnd: u32 = 0;

    while (scanPosition < position) {
        if (buffer[scanPosition] == '\n') {
            lines += 1;
            lineStart = scanPosition + 1;
        }
        scanPosition += 1;
    }
    while (scanPosition < buffer.len) {
        if (buffer[scanPosition] == '\n') {
            lineEnd = scanPosition;
            break;
        }
        scanPosition += 1;
    }
    if (lineEnd == 0) {
        lineEnd = @as(u32, @intCast(buffer.len));
    }

    const line = buffer[lineStart..lineEnd];
    const column = position - lineStart + 1;

    if (std.fs.path.isAbsolute(filePath)) {
        const cwdPath = try std.fs.cwd().realpathAlloc(allocator, ".");
        defer allocator.free(cwdPath);
        const relativePath = try std.fs.path.relative(allocator, cwdPath, filePath);
        defer allocator.free(relativePath);
        try stderr.print("{s}:{d}:{d}\n", .{ relativePath, lines, column });
    } else {
        try stderr.print("{s}:{d}:{d}\n", .{ filePath, lines, column });
    }

    const lineString = try std.fmt.allocPrint(allocator, "{d}", .{lines});
    defer allocator.free(lineString);

    const caretSpaces = try repeat(column + lineString.len + 2, ' ', allocator);
    defer allocator.free(caretSpaces);

    const carets = try repeat(length, '^', allocator);
    defer allocator.free(carets);

    const string = try std.fmt.allocPrint(allocator, "{s} | {s}\n{s}\x1b[1;31m{s}", .{ lineString, line, caretSpaces, carets });
    defer allocator.free(string);

    try stderr.print("{s}\x1b[0m\n", .{string});
}
