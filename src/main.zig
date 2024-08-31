const std = @import("std");

const ModuleInterpreter = @import("./interpreter/module_interpreter.zig");
const Module = @import("interpreter/module_value.zig").Module;
const Interpreter = @import("interpreter/interpreter.zig").Interpreter;

const args_parser = @import("args_parser.zig");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const prettyError = @import("errors.zig").prettyError;

pub const Main = @This();
allocator: std.mem.Allocator,
hadError: bool = false,
args: ?*const args_parser.Args = null,
interpreter: Interpreter,

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    const useUnsafeAllocator = @import("builtin").mode != std.builtin.OptimizeMode.Debug and @import("builtin").mode != std.builtin.OptimizeMode.ReleaseSafe;
    var allocator = if (useUnsafeAllocator) std.heap.raw_c_allocator else gpa.allocator();

    var instance = Main{
        .allocator = allocator,
        .interpreter = try Interpreter.new(allocator, args_parser.ArgsFlags{}),
    };
    defer instance.deinit();
    instance.entry() catch |err| {
        switch (err) {
            std.fs.SelfExePathError.FileNotFound => {
                try prettyError("File not found");
                std.process.exit(1);
            },
            else => {
                const message = try std.fmt.allocPrint(allocator, "Error in main entry: {any}", .{err});
                defer allocator.free(message);
                try prettyError(message);
                return err;
            },
        }
    };

    if (instance.hadError) {
        std.process.exit(65);
    }
}

pub fn deinit(self: *Main) void {
    self.interpreter.deinit();
}

fn entry(self: *Main) !void {
    // Print the process arguments
    const args = try self.allocator.create(args_parser.Args);
    args.* = args_parser.parse(self.allocator) catch |err| {
        switch (err) {
            args_parser.ArgParseError.MultiplePathsProvided => std.process.exit(1),
            else => return err,
        }
    };
    self.args = args;
    self.interpreter.flags = self.args.?.flags;

    defer self.args.?.deinit();

    switch (self.args.?.mode) {
        .RunFile => try self.enterFile(),
        .RunRepl => try self.enterRepl(),
    }
}

fn enterRepl(self: *Main) !void {
    var astInterpreter = try ModuleInterpreter.init(self.allocator, &self.interpreter, "repl");
    errdefer astInterpreter.deinit();

    const stdin = std.io.getStdIn().reader();

    while (true) {
        std.debug.print("> ", .{});
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

        _ = try self.interpreter.run(line, &astInterpreter);
    }
}

fn enterFile(self: *Main) !void {
    const filePath = try std.fs.realpathAlloc(self.allocator, self.args.?.mode.RunFile);
    _ = self.interpreter.importModule(filePath) catch {
        self.hadError = true;
        return;
    };
    // No need to deinitialize the module since it will be deinitialized by the interpreter
}
