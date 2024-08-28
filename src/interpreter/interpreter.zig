const std = @import("std");

const lexer = @import("../lexer.zig");
const parser = @import("../parser/parser.zig");
const resolver = @import("../resolver/resolver.zig");
const ASTPrinter = @import("../parser/ast_printer.zig").ASTPrinter;

const ArgsFlags = @import("../args_parser.zig").ArgsFlags;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;

const VariableValue = @import("./variable_value.zig").VariableValue;
const NativeError = @import("./natives.zig").NativeError;

pub const Interpreter = @This();

allocator: std.mem.Allocator,
flags: ArgsFlags,

pub fn new(allocator: std.mem.Allocator, flags: ArgsFlags) !Interpreter {
    return Interpreter{ .allocator = allocator, .flags = flags };
}

pub fn deinit(self: *Interpreter) void {
    _ = self;
}

/// Returns if there was an error.
pub fn runFile(self: *Interpreter, filePath: []const u8, interpreter: *ModuleInterpreter) !bool {
    // Open the file in the arguments of the program
    const file = std.fs.cwd().openFile(filePath, .{ .mode = .read_only }) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("File not found: {s}\n", .{ .name_string = filePath });
            return true;
        },
        else => return err,
    };

    defer file.close();

    const buffer = try file.readToEndAllocOptions(self.allocator, std.math.maxInt(usize), null, @alignOf(u8), 0);
    defer self.allocator.free(buffer);

    return try self.run(filePath, buffer, interpreter);
}

/// Arity: 1
pub fn import(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    // TODO
    const argument = arguments.items[0];
    return VariableValue.fromString(VariableValue.toString(argument, interpreter.allocator) catch return NativeError.Unknown, true);
}

/// Returns if there was an error.
pub fn run(self: *Interpreter, filePath: []const u8, source: []const u8, interpreter: *ModuleInterpreter) !bool {
    // LEXING -------------------------------------
    var sourceLexer = lexer.init(self.allocator, filePath, source);
    defer sourceLexer.deinit();
    const tokens = try sourceLexer.getAllTokens() orelse return true;

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
    var sourceParser = try parser.init(tokens, filePath, source, self.flags, self.allocator);
    defer sourceParser.deinit();

    var program = sourceParser.parse() catch return true;
    defer program.deinit();

    var sourceResolver = resolver.init(interpreter, source, filePath);
    defer sourceResolver.deinit();

    sourceResolver.resolveProgram(&program) catch return true;

    if (self.flags.debugAST) {
        const printer = ASTPrinter.init(self.allocator);
        try printer.printProgram(&program);
        std.debug.print("\n\n", .{});
    }

    // INTERPRETING -------------------------------------
    try interpreter.run(&program, source, filePath);
    if (interpreter.runtimeError != null) return true;

    return false;
}
