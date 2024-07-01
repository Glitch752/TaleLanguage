const std = @import("std");

const lexer = @import("lexer.zig");
const parser = @import("./parser/parser.zig");
const interpreter = @import("./interpreter/interpreter.zig");

const args_parser = @import("args_parser.zig");

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

const ASTPrinter = @import("parser/ast_printer.zig").ASTPrinter;
const prettyError = @import("errors.zig").prettyError;

pub const Main = @This();
allocator: std.mem.Allocator,
hadError: bool = false,
args: ?*const args_parser.Args = null,
interpreter: ?interpreter.Interpreter = null,

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var instance = Main{
        .allocator = allocator,
        .interpreter = interpreter.init(allocator),
    };
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

    try self.run(filePath, buffer, false);

    if (self.hadError) {
        std.process.exit(65);
    }
}

fn runRepl(self: *Main) !void {
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

        try self.run("repl", line, true);
        self.hadError = false;
    }
}

fn run(self: *Main, fileName: []const u8, source: []const u8, allowEvaluatingExpressions: bool) !void {
    // LEXING -------------------------------------
    var sourceLexer = lexer.init(self.allocator, fileName, source);
    defer sourceLexer.deinit();
    const tokens = try sourceLexer.getAllTokens() orelse {
        return;
    };

    if (self.args.?.flags.debugTokens) {
        std.debug.print("Tokens:\n", .{});
        for (tokens) |token| {
            if (self.args.?.flags.extremelyVerbose) {
                std.debug.print("  {any}\n", .{token});
            } else {
                const tokenString = try token.toCondensedString(self.allocator);
                defer self.allocator.free(tokenString);
                std.debug.print("{s}", .{tokenString});
            }
        }
        std.debug.print("\n\n", .{});
    }

    if (self.args.?.flags.stopAfterTokens) {
        std.debug.print("Stopped early after token generation.\n", .{});
        return;
    }

    // PARSING -------------------------------------
    var sourceParser = try parser.init(tokens, fileName, source, self.args.?.flags, self.allocator);
    defer sourceParser.uninit();

    if (allowEvaluatingExpressions and
        tokens[tokens.len - 2].type != TokenType.Semicolon and
        tokens[tokens.len - 2].type != TokenType.CloseCurly)
    {
        var expression = sourceParser.parseExpression() catch {
            self.hadError = true;
            return;
        };
        defer expression.uninit(self.allocator);

        if (self.args.?.flags.debugAST) {
            const printer = ASTPrinter.init(self.allocator);
            try printer.printExpression(expression);
            std.debug.print("\n\n", .{});
        }

        // INTERPRETING -------------------------------------
        const result = self.interpreter.?.runExpression(expression, source, fileName) catch {
            self.hadError = true;
            return;
        };

        std.debug.print("=> {s}\n", .{try result.toString(self.allocator)});
    } else {
        var program = sourceParser.parse() catch {
            self.hadError = true;
            return;
        };
        defer program.deinit();

        if (self.args.?.flags.debugAST) {
            const printer = ASTPrinter.init(self.allocator);
            try printer.printProgram(&program);
            std.debug.print("\n\n", .{});
        }

        // INTERPRETING -------------------------------------
        try self.interpreter.?.run(&program, source, fileName);
    }
}
