const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const args_parser = @import("args_parser.zig");
const Token = @import("token.zig").Token;
const TokenData = @import("token.zig").TokenData;
const prettyError = @import("errors.zig").prettyError;

pub const Main = @This();
allocator: std.mem.Allocator,
hadError: bool = false,
args: ?*const args_parser.Args = null,

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var instance = Main{ .allocator = allocator };
    defer instance.deinit();
    try instance.entry();
}

pub fn entry(self: *Main) !void {
    // Print the process arguments
    const args = try self.allocator.create(args_parser.Args);
    args.* = args_parser.parse(self.allocator) catch |err| {
        switch (err) {
            args_parser.ArgParseError.MultiplePathsProvided => std.process.exit(1),
            else => return err,
        }
    };
    self.args = args;

    defer self.args.?.deinit();

    switch (self.args.?.mode) {
        .RunFile => {
            try self.runFile();
        },
        .RunRepl => {
            try self.runRepl();
        },
    }
}

pub fn deinit(self: *Main) void {
    _ = self;
}

fn runFile(self: *Main) !void {
    // Open the file in the arguments of the program
    const filePath = self.args.?.mode.RunFile;
    const file = std.fs.cwd().openFile(filePath, .{ .mode = .read_only }) catch |err|
        switch (err) {
        error.FileNotFound => {
            std.debug.print("File not found: {s}\n", .{ .name_string = filePath });
            return;
        },
        else => return err,
    };

    defer file.close();

    const buffer = try file.readToEndAllocOptions(self.allocator, std.math.maxInt(usize), null, @alignOf(u8), 0);
    defer self.allocator.free(buffer);

    try self.run(buffer);

    if (self.hadError) {
        std.process.exit(65);
    }
}

fn runRepl(self: *Main) !void {
    const stdin = std.io.getStdIn().reader();

    while (true) {
        const bare_line = stdin.readUntilDelimiterOrEofAlloc(self.allocator, '\n', 1000000) catch {
            try prettyError("Failed to read line from stdin");
            break;
        } orelse {
            break;
        };
        defer self.allocator.free(bare_line);

        const line = std.mem.trim(u8, bare_line, "\r");

        if (line.len == 0) {
            break;
        }

        try self.run(line);
        self.hadError = false;
    }
}

fn run(self: *Main, source: []const u8) !void {
    var sourceLexer = lexer.init(self.allocator, source);
    const tokens = try sourceLexer.getAllTokens();
    defer sourceLexer.deinit();

    if (self.args.?.flags.debugTokens) {
        std.debug.print("Tokens:\n", .{});
        for (tokens) |token| {
            const tokenString = try token.toCondensedString(self.allocator);
            defer self.allocator.free(tokenString);
            std.debug.print("{s}", .{tokenString});
        }
    }

    // while (true) {
    //     const tokenData = try lxr.getNext(self.allocator);

    //     if (tokenData.token == Token.EOF) {
    //         break;
    //     }
    //     if (tokenData.token == Token.Error) {
    //         return try prettyError(try std.fmt.allocPrint(self.allocator, "Lexer Error: {d}:{d} - {s}", .{ tokenData.line, tokenData.column, tokenData.token.Error }));
    //     }

    //     if (args.flags.debugTokens) {
    //         const tokenString = try tokenData.token.toStringWithType(self.allocator);
    //         defer self.allocator.free(tokenString);
    //         std.debug.print("{s}", .{tokenString});
    //     }

    //     try tokens.append(tokenData);
    // }

    // if (args.flags.debugTokens) {
    //     std.debug.print("\n\n", .{});
    // }

    // if (args.flags.stopAfterTokens) {
    //     return;
    // }

    // var prsr = try parser.init(tokens.items, args.flags, self.allocator, filePath);
    // defer prsr.deinit();
}
