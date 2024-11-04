const std = @import("std");

const lexer = @import("../lexer.zig");
const parser = @import("../parser/parser.zig");
const resolver = @import("../resolver/resolver.zig");
const ASTPrinter = @import("../parser/ast_printer.zig").ASTPrinter;

const ArgsFlags = @import("../args_parser.zig").ArgsFlags;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;

const VariableValue = @import("./variable_value.zig").VariableValue;
const NativeError = @import("./natives.zig").NativeError;
const Module = @import("./module_value.zig").Module;

const natives = @import("./natives.zig");

const Program = @import("../parser/program.zig").Program;

pub const Interpreter = @This();

allocator: std.mem.Allocator,
flags: ArgsFlags,

lexers: std.ArrayListUnmanaged(lexer), // Used to deallocate lexers after the program is run
programs: std.ArrayListUnmanaged(*Program), // Used to deallocate programs after the program is run

pub fn new(allocator: std.mem.Allocator, flags: ArgsFlags) !Interpreter {
    return Interpreter{
        .allocator = allocator,
        .flags = flags,
        .lexers = std.ArrayListUnmanaged(lexer){},
        .programs = std.ArrayListUnmanaged(*Program){},
    };
}

pub fn deinit(self: *Interpreter) void {
    for (self.lexers.items) |lexerValue| {
        var l = lexerValue;
        l.deinit();
    }
    self.lexers.deinit(self.allocator);

    for (self.programs.items) |program| {
        program.deinit(self.allocator, true);
    }
    self.programs.deinit(self.allocator);
}

/// Returns if there was an error.
pub fn runFile(self: *Interpreter, interpreter: *ModuleInterpreter) !bool {
    const file = std.fs.cwd().openFile(interpreter.filePath, .{ .mode = .read_only }) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("File not found: {s}\n", .{ .name_string = interpreter.filePath });
            return true;
        },
        else => return err,
    };

    defer file.close();

    const file_size = (file.stat() catch return NativeError.Unknown).size;
    const buffer = self.allocator.alloc(u8, file_size) catch return NativeError.Unknown;
    _ = file.readAll(buffer) catch return NativeError.Unknown;
    return try self.run(buffer, interpreter);
}

/// Returns if there was an error.
pub fn run(self: *Interpreter, source: []const u8, interpreter: *ModuleInterpreter) !bool {
    // LEXING -------------------------------------
    var sourceLexer = lexer.init(self.allocator, interpreter.filePath, source);
    const tokens = try sourceLexer.getAllTokens() orelse {
        self.allocator.free(source);
        return true;
    };
    try self.lexers.append(self.allocator, sourceLexer);

    if (self.flags.debugTokens) {
        std.debug.print("Tokens:\n", .{});
        for (tokens) |token| {
            if (self.flags.extremelyVerbose) {
                std.debug.print("  {any}\n", .{token});
            } else {
                const tokenString = try token.toCondensedString(self.allocator);
                defer self.allocator.free(tokenString);
                std.debug.print("{s}", .{tokenString});
            }
        }
        std.debug.print("\n\n", .{});
    }

    if (self.flags.stopAfterTokens) {
        std.debug.print("Stopped early after token generation.\n", .{});
        return false;
    }

    // PARSING -------------------------------------
    var sourceParser = try parser.init(tokens, interpreter.filePath, source, self.flags, self.allocator);
    const program = sourceParser.parse() catch {
        self.allocator.free(source);
        return true;
    };
    try self.programs.append(self.allocator, program);

    var sourceResolver = resolver.init(interpreter, source, interpreter.filePath);
    defer sourceResolver.deinit();

    sourceResolver.resolveProgram(program) catch {
        self.allocator.free(source);
        return true;
    };

    if (self.flags.debugAST) {
        const printer = ASTPrinter.init(self.allocator);
        try printer.printProgram(program);
        std.debug.print("\n\n", .{});
    }

    // INTERPRETING -------------------------------------
    try interpreter.run(program, source);
    if (interpreter.runtimeError != null) return true;

    return false;
}
