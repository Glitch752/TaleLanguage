const std = @import("std");
const RCSP = @import("../rcsp.zig");
const Environment = @import("./environment.zig").Environment;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;
const Interpreter = @import("./interpreter.zig").Interpreter;
const Main = @import("../main.zig");
const CallableNativeFunction = @import("./callable_value.zig").CallableNativeFunction;

pub const ModuleReference = RCSP.DeinitializingRcSharedPointer(Module, RCSP.NonAtomic, void);
pub const ExportedValueSet = std.StringHashMapUnmanaged(u0);

pub const Module = struct {
    moduleInterpreter: ModuleInterpreter,
    exportedValues: ExportedValueSet,
    path: []const u8,

    pub fn getRootEnvironment(self: *Module) *Environment {
        return self.moduleInterpreter.rootEnvironment;
    }

    pub fn load(interpreter: *Interpreter, path: []const u8, import: CallableNativeFunction) !Module {
        var moduleInterpreter = try ModuleInterpreter.init(interpreter.allocator, import);
        _ = try interpreter.runFile(path, &moduleInterpreter);

        return .{
            .moduleInterpreter = moduleInterpreter,
            .exportedValues = ExportedValueSet{},
            .path = path,
        };
    }

    pub fn deinit(self: *Module, _: void) void {
        self.moduleInterpreter.deinit();
    }
};
