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

loadedModules: std.StringHashMapUnmanaged(VariableValue),
globalModules: std.StringHashMapUnmanaged(VariableValue),

pub fn new(allocator: std.mem.Allocator, flags: ArgsFlags) !Interpreter {
    var globalModules = std.StringHashMapUnmanaged(VariableValue){};

    try globalModules.put(allocator, "std", try Module.makeNativeModule("std", allocator, .{
        .{ .name = "print", .value = try VariableValue.nativeFunction(1, &natives.print) },
        .{ .name = "println", .value = try VariableValue.nativeFunction(1, natives.println) },

        .{ .name = "sin", .value = try VariableValue.nativeFunction(1, &natives.sin) },
        .{ .name = "cos", .value = try VariableValue.nativeFunction(1, &natives.cos) },
        .{ .name = "tan", .value = try VariableValue.nativeFunction(1, &natives.tan) },
        .{ .name = "asin", .value = try VariableValue.nativeFunction(1, &natives.asin) },
        .{ .name = "acos", .value = try VariableValue.nativeFunction(1, &natives.acos) },
        .{ .name = "atan", .value = try VariableValue.nativeFunction(1, &natives.atan) },

        .{ .name = "exp", .value = try VariableValue.nativeFunction(1, &natives.exp) },
        .{ .name = "exp2", .value = try VariableValue.nativeFunction(1, &natives.exp2) },
        .{ .name = "log", .value = try VariableValue.nativeFunction(1, &natives.log) },
        .{ .name = "log2", .value = try VariableValue.nativeFunction(1, &natives.log2) },
        .{ .name = "log10", .value = try VariableValue.nativeFunction(1, &natives.log10) },
        .{ .name = "pow", .value = try VariableValue.nativeFunction(2, &natives.pow) },
        .{ .name = "sqrt", .value = try VariableValue.nativeFunction(1, &natives.sqrt) },
        .{ .name = "floor", .value = try VariableValue.nativeFunction(1, &natives.floor) },
        .{ .name = "ceil", .value = try VariableValue.nativeFunction(1, &natives.ceil) },
        .{ .name = "round", .value = try VariableValue.nativeFunction(1, &natives.round) },
        .{ .name = "abs", .value = try VariableValue.nativeFunction(1, &natives.abs) },
        .{ .name = "min", .value = try VariableValue.nativeFunction(2, &natives.min) },
        .{ .name = "max", .value = try VariableValue.nativeFunction(2, &natives.max) },

        .{ .name = "PI", .value = VariableValue.fromNumber(std.math.pi) },
        .{ .name = "E", .value = VariableValue.fromNumber(std.math.e) },
        .{ .name = "PHI", .value = VariableValue.fromNumber(std.math.phi) },
        .{ .name = "INFINITY", .value = VariableValue.fromNumber(std.math.inf(f64)) },
        .{ .name = "NAN", .value = VariableValue.fromNumber(std.math.nan(f64)) },
        .{ .name = "NEGATIVE_INFINITY", .value = VariableValue.fromNumber(-std.math.inf(f64)) },

        .{ .name = "substring", .value = try VariableValue.nativeFunction(3, &natives.substring) },
        .{ .name = "intChar", .value = try VariableValue.nativeFunction(1, &natives.intChar) },
        .{ .name = "charInt", .value = try VariableValue.nativeFunction(1, &natives.charInt) },
        .{ .name = "length", .value = try VariableValue.nativeFunction(1, &natives.length) },
        .{ .name = "string", .value = try VariableValue.nativeFunction(1, &natives.toString) },

        .{ .name = "clock", .value = try VariableValue.nativeFunction(0, &natives.clock) },
        .{ .name = "panic", .value = try VariableValue.nativeFunction(1, &natives.panic) },
        .{ .name = "assert", .value = try VariableValue.nativeFunction(1, &natives.assert) },

        .{ .name = "random", .value = try Module.makeNativeModule("random", allocator, .{
            .{ .name = "normalized", .value = try VariableValue.nativeFunction(0, &natives.randomNormalized) },
            .{ .name = "seed", .value = try VariableValue.nativeFunction(1, &natives.randomSeed) },
        }) },
    }));

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

    var iter2 = self.globalModules.iterator();
    while (iter2.next()) |entry| {
        entry.value_ptr.deinit(self.allocator);
        if (entry.value_ptr.* == .Module) {
            entry.value_ptr.*.Module.deinit(self.allocator);
            self.allocator.destroy(entry.value_ptr.Module);
        }
    }
    self.globalModules.deinit(self.allocator);

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
    errdefer self.allocator.destroy(module);

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
