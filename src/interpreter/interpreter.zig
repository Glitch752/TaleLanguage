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

const Program = @import("../parser/program.zig").Program;

pub const Interpreter = @This();

allocator: std.mem.Allocator,
flags: ArgsFlags,

lexers: std.ArrayListUnmanaged(lexer), // Used to deallocate lexers after the program is run
programs: std.ArrayListUnmanaged(*Program), // Used to deallocate programs after the program is run

loadedModules: std.StringHashMapUnmanaged(VariableValue),
globalModules: std.StringHashMapUnmanaged(VariableValue),

pub fn new(allocator: std.mem.Allocator, flags: ArgsFlags) !Interpreter {
    const globalModules = std.StringHashMapUnmanaged(VariableValue){};

    // globalModules.put(allocator, "std", try Module.load())

    return Interpreter{
        .allocator = allocator,
        .flags = flags,
        .loadedModules = std.StringHashMapUnmanaged(VariableValue){},
        .lexers = std.ArrayListUnmanaged(lexer){},
        .programs = std.ArrayListUnmanaged(*Program){},
        .globalModules = globalModules,
    };
}

pub fn deinit(self: *Interpreter) void {
    var iter = self.loadedModules.iterator();
    while (iter.next()) |entry| {
        entry.value_ptr.deinit(self.allocator);
        if (entry.value_ptr.* == .Module) {
            entry.value_ptr.*.Module.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.Module);
        }
        self.allocator.free(entry.key_ptr.*);
    }
    self.loadedModules.deinit(self.allocator);

    for (self.lexers.items) |lexerValue| {
        var l = lexerValue;
        l.deinit();
    }
    self.lexers.deinit(self.allocator);

    for (self.programs.items) |program| {
        program.deinit(self.allocator);
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

/// Arity: 1
pub fn import(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    const argument = arguments.items[0];
    if (!argument.isString()) return NativeError.InvalidOperand;

    // interpreter.interpreter actually references ourself
    const self = interpreter.interpreter;

    if (self.globalModules.contains(argument.asString())) {
        return self.globalModules.get(argument.asString()).?;
    }

    return (try self.importFromPath(argument.asString(), interpreter.filePath)).takeReference(interpreter.allocator) catch return NativeError.Unknown;
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

    var module = self.allocator.create(Module) catch return NativeError.Unknown;
    module.* = Module.load(self, path) catch return NativeError.Unknown;
    errdefer module.deinit(self.allocator);

    const moduleVariable = VariableValue.fromModule(module) catch return NativeError.Unknown;

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
pub fn run(self: *Interpreter, source: []const u8, interpreter: *ModuleInterpreter) !bool {
    // LEXING -------------------------------------
    var sourceLexer = lexer.init(self.allocator, interpreter.filePath, source);
    const tokens = try sourceLexer.getAllTokens() orelse return true;
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
    const program = sourceParser.parse() catch return true;
    try self.programs.append(self.allocator, program);

    var sourceResolver = resolver.init(interpreter, source, interpreter.filePath);
    defer sourceResolver.deinit();

    sourceResolver.resolveProgram(program) catch return true;

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
