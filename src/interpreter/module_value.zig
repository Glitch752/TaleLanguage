const std = @import("std");
const RCSP = @import("../rcsp.zig");
const Environment = @import("./environment.zig").Environment;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;
const Interpreter = @import("./interpreter.zig").Interpreter;
const Main = @import("../main.zig");
const CallableNativeFunction = @import("./callable_value.zig").CallableNativeFunction;
const Token = @import("../token.zig").Token;
const VariableValue = @import("./variable_value.zig").VariableValue;
const RuntimeError = @import("./interpreterError.zig").RuntimeError;
const InterpreterError = @import("./interpreterError.zig").InterpreterError;

pub const ExportedValueSet = std.StringHashMapUnmanaged(u0);

pub const ModuleError = error{
    InterpreterError,
};

pub const Module = union(enum) {
    CodeModule: struct {
        moduleInterpreter: *ModuleInterpreter,
        path: []const u8,
    },
    NativeModule: struct {
        exports: *std.StringHashMapUnmanaged(VariableValue),
        name: []const u8,
    },

    pub fn makeNativeModule(name: []const u8, allocator: std.mem.Allocator, exports: anytype) !VariableValue {
        // Ensure that exports is a tuple with all {name: string, value: VariableValue} fields.
        const exportsType = @TypeOf(exports);
        const exportsTypeInfo = @typeInfo(exportsType);
        if (exportsTypeInfo != .Struct) {
            @compileError("Expected tuple of .{ .name = []const u8, .value = VariableValue }, found " ++ @typeName(exportsType));
        }
        const fieldsInfo = exportsTypeInfo.Struct.fields;
        comptime {
            // There's probably a better way to do this, but I'm not sure what it is.
            for (fieldsInfo) |field| {
                const fieldTypeInfo = @typeInfo(field.type);
                if (fieldTypeInfo != .Struct) @compileError("Expected tuple of .{ .name = []const u8, .value = VariableValue }, found " ++ @typeName(field.type));
                const nameField = fieldTypeInfo.Struct.fields[0];
                const valueField = fieldTypeInfo.Struct.fields[1];
                if (!std.mem.eql(u8, nameField.name, "name") or !std.mem.eql(u8, valueField.name, "value")) @compileError("Expected tuple of .{ .name = []const u8, .value = VariableValue }, found " ++ @typeName(field.type));
                const nameTypeInfo = @typeInfo(nameField.type);
                if (nameTypeInfo != .Pointer or valueField.type != VariableValue) @compileError("Expected tuple of .{ .name = []const u8, .value = VariableValue }, found " ++ @typeName(field.type));
                const nameChildTypeInfo = @typeInfo(nameTypeInfo.Pointer.child);
                if (nameChildTypeInfo != .Array or nameChildTypeInfo.Array.child != u8) @compileError("Expected tuple of .{ .name = []const u8, .value = VariableValue }, found " ++ @typeName(field.type));
            }
        }

        const exportsMap = try allocator.create(std.StringHashMapUnmanaged(VariableValue));
        exportsMap.* = std.StringHashMapUnmanaged(VariableValue){};

        const fieldCount = fieldsInfo.len;
        inline for (0..fieldCount) |i| {
            const field = @field(exports, fieldsInfo[i].name);
            try exportsMap.put(allocator, field.name, field.value);
        }

        const module = try allocator.create(Module);
        module.* = .{
            .NativeModule = .{
                .name = name,
                .exports = exportsMap,
            },
        };

        return VariableValue.fromModule(module);
    }

    pub fn getPath(self: *const Module) []const u8 {
        return switch (self.*) {
            .CodeModule => self.CodeModule.path,
            .NativeModule => self.NativeModule.name,
        };
    }

    fn getRootEnvironment(self: *const Module) *Environment {
        return &self.CodeModule.moduleInterpreter.rootEnvironment;
    }
    fn isExported(self: *const Module, name: Token) bool {
        return self.CodeModule.moduleInterpreter.exportedValues.contains(name.lexeme);
    }
    /// The interpreter is only used for error handling.
    pub fn accessExport(self: *const Module, name: Token, interpreter: *ModuleInterpreter) !VariableValue {
        switch (self.*) {
            .CodeModule => {
                if (!self.isExported(name)) {
                    interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Module does not export {s}.", .{name.lexeme});
                    return InterpreterError.RuntimeError;
                }
                return (try self.getRootEnvironment().getSelf(name, interpreter)).takeReference(interpreter.allocator);
            },
            .NativeModule => {
                const entry = self.NativeModule.exports.get(name.lexeme);
                if (entry != null) return entry.?;
                interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Native module does not export {s}.", .{name.lexeme});
                return InterpreterError.RuntimeError;
            },
        }
    }

    /// Path will be freed by the interpreter when deinitialized; it should not be freed by the caller.
    pub fn load(interpreter: *Interpreter, path: []const u8) !Module {
        try {
            var moduleInterpreter = try interpreter.allocator.create(ModuleInterpreter);
            moduleInterpreter.* = try ModuleInterpreter.init(interpreter.allocator, interpreter, path);

            const err = try interpreter.runFile(moduleInterpreter);
            if (err) {
                moduleInterpreter.deinit();
                return ModuleError.InterpreterError;
            }

            return .{
                .CodeModule = .{
                    .moduleInterpreter = moduleInterpreter,
                    .path = path,
                },
            };
        } catch |err| {
            interpreter.allocator.free(path);
            return err;
        };
    }

    pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .CodeModule => |values| {
                values.moduleInterpreter.deinit();
                allocator.destroy(values.moduleInterpreter);
            },
            .NativeModule => |values| {
                var iter = values.exports.iterator();
                while (iter.next()) |entry| {
                    entry.value_ptr.deinit(allocator);
                }
                values.exports.deinit(allocator);
                allocator.destroy(values.exports);
            },
        }
    }
};
