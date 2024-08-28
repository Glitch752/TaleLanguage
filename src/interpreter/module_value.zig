const std = @import("std");
const RCSP = @import("../rcsp.zig");
const Environment = @import("./environment.zig").Environment;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;
const Main = @import("../main.zig");

pub const ModuleReference = RCSP.DeinitializingRcSharedPointer(Module, RCSP.NonAtomic, void);
pub const ExportedValueSet = std.StringHashMapUnmanaged(u0);

pub const Module = struct {
    interpreter: ModuleInterpreter,
    exportedValues: ExportedValueSet,
    path: []const u8,

    pub fn getRootEnvironment(self: *Module) *Environment {
        return self.interpreter.rootEnvironment;
    }

    pub fn load(main: *Main, path: []const u8) !Module {
        var interpreter = try ModuleInterpreter.init(main.allocator);
        try main.runFile(path, &interpreter);

        return .{
            .interpreter = interpreter,
            .exportedValues = ExportedValueSet{},
            .path = path,
        };
    }

    pub fn deinit(self: *Module, _: void) void {
        self.interpreter.deinit();
    }
};
