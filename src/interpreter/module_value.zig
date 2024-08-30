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

pub const Module = struct {
    moduleInterpreter: *ModuleInterpreter,
    path: []const u8,

    fn getRootEnvironment(self: *const Module) *Environment {
        return &self.moduleInterpreter.rootEnvironment;
    }
    fn isExported(self: *const Module, name: Token) bool {
        return self.moduleInterpreter.exportedValues.contains(name.lexeme);
    }
    /// The interpreter is only used for error handling.
    pub fn accessExport(self: *const Module, name: Token, interpreter: *ModuleInterpreter) !VariableValue {
        if (!self.isExported(name)) {
            interpreter.runtimeError = RuntimeError.tokenError(interpreter, name, "Module does not export {s}.", .{name.lexeme});
            return InterpreterError.RuntimeError;
        }
        return (try self.getRootEnvironment().get(name, interpreter)).takeReference(interpreter.allocator);
    }

    /// Path will be freed by the interpreter when deinitialized; it should not be freed by the caller.
    pub fn load(interpreter: *Interpreter, path: []const u8) !Module {
        try {
            var moduleInterpreter = try interpreter.allocator.create(ModuleInterpreter);
            moduleInterpreter.* = try ModuleInterpreter.init(interpreter.allocator, interpreter);

            const err = try interpreter.runFile(path, moduleInterpreter);
            if (err) {
                moduleInterpreter.deinit();
                return ModuleError.InterpreterError;
            }

            return .{
                .moduleInterpreter = moduleInterpreter,
                .path = path,
            };
        } catch |err| {
            interpreter.allocator.free(path);
            return err;
        };
    }

    pub fn deinit(self: *Module) void {
        self.moduleInterpreter.deinit();
        self.moduleInterpreter.allocator.destroy(self.moduleInterpreter);
    }
};
