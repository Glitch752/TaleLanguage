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

pub const Interpreter = @This();

allocator: std.mem.Allocator,
flags: ArgsFlags,

loadedModules: std.StringHashMapUnmanaged(VariableValue),

pub fn new(allocator: std.mem.Allocator, flags: ArgsFlags) !Interpreter {
    return Interpreter{
        .allocator = allocator,
        .flags = flags,
        .loadedModules = std.StringHashMapUnmanaged(VariableValue){},
    };
}

pub fn deinit(self: *Interpreter) void {
    var iter = self.loadedModules.iterator();
    while (iter.next()) |entry| {
        entry.value_ptr.deinit(self.allocator);
        self.allocator.free(entry.key_ptr.*);
    }
    self.loadedModules.deinit(self.allocator);
}

/// Returns if there was an error.
pub fn runFile(self: *Interpreter, filePath: []const u8, interpreter: *ModuleInterpreter) !bool {
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
    const argument = arguments.items[0];
    if (!argument.isString()) return NativeError.InvalidOperand;

    // interpreter.interpreter actually references ourself
    return (try interpreter.interpreter.importFromPath(argument.asString(), interpreter.filePath)).takeReference(interpreter.allocator) catch return NativeError.Unknown;
}

pub fn importFromPath(self: *Interpreter, path: []const u8, filePath: []const u8) NativeError!VariableValue {
    // Resolve the path
    var resolvedPath = std.fs.path.resolve(self.allocator, &[2][]const u8{ std.fs.path.dirname(filePath) orelse return NativeError.Unknown, path }) catch return NativeError.Unknown;

    if (self.flags.extremelyVerbose) {
        std.debug.print("Importing from path: {s}\n", .{resolvedPath});
    }

    // If the path has no file extension, add .tale
    // If it does and the file extension isn't .tale, import it as a string. Otherwise, import it as a module.
    const extension = std.fs.path.extension(resolvedPath);
    if (std.mem.eql(u8, extension, "") or std.mem.eql(u8, extension, ".tale")) {
        if (std.mem.eql(u8, extension, "")) {
            resolvedPath = self.allocator.realloc(resolvedPath, resolvedPath.len + 5) catch return NativeError.Unknown;
            std.mem.copyForwards(u8, resolvedPath[resolvedPath.len - 5 ..], ".tale");
        }
        return try self.importModule(resolvedPath);
    } else {
        return self.importString(resolvedPath);
    }
}

pub fn importModule(self: *Interpreter, path: []const u8) NativeError!VariableValue {
    if (self.loadedModules.contains(path)) {
        defer self.allocator.free(path);
        return self.loadedModules.get(path).?;
    }

    var module: Module = Module.load(self, path) catch return NativeError.Unknown;
    errdefer module.deinit({});

    const moduleVariable = VariableValue.fromModule(module, self.allocator) catch return NativeError.Unknown;

    const duplicatedPath = self.allocator.dupe(u8, path) catch return NativeError.Unknown;
    self.loadedModules.put(self.allocator, duplicatedPath, moduleVariable) catch return NativeError.Unknown;

    return moduleVariable;
}

pub fn importString(self: *Interpreter, path: []const u8) NativeError!VariableValue {
    defer self.allocator.free(path);
    if (self.loadedModules.contains(path)) {
        return self.loadedModules.get(path).?;
    }

    const file = std.fs.cwd().openFile(path, .{ .mode = .read_only }) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("File not found: {s}\n", .{path});
            return NativeError.InvalidOperand;
        },
        else => return NativeError.Unknown,
    };
    defer file.close();

    const file_size = (file.stat() catch return NativeError.Unknown).size;
    const buffer = self.allocator.alloc(u8, file_size) catch return NativeError.Unknown;
    _ = file.readAll(buffer) catch return NativeError.Unknown;
    const stringVariable = VariableValue.fromString(buffer, true);

    const duplicatedPath = self.allocator.dupe(u8, path) catch return NativeError.Unknown;
    self.loadedModules.put(self.allocator, duplicatedPath, stringVariable) catch return NativeError.Unknown;

    return stringVariable;
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
