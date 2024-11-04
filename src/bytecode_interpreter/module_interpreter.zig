const std = @import("std");

const Expression = @import("../parser/expression.zig").Expression;
const Statement = @import("../parser/statement.zig").Statement;
const Program = @import("../parser/program.zig").Program;

const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;

const RuntimeError = @import("./interpreter_error.zig").RuntimeError;
const InterpreterError = @import("./interpreter_error.zig").InterpreterError;

const Interpreter = @import("./interpreter.zig").Interpreter;

pub const ModuleInterpreter = @This();

allocator: std.mem.Allocator,
runtimeError: ?RuntimeError = null,

buffer: []const u8,
filePath: []const u8,

interpreter: *Interpreter,

pub fn init(allocator: std.mem.Allocator, interpreter: *Interpreter, filePath: []const u8, originalBuffer: []const u8) !ModuleInterpreter {
    return .{
        .allocator = allocator,
        .interpreter = interpreter,
        .filePath = filePath,
        .buffer = originalBuffer,
    };
}

pub fn deinit(self: *ModuleInterpreter) void {
    if (self.runtimeError != null) {
        self.runtimeError.?.printAndDeinit();
        self.runtimeError = null;
    }

    self.rootEnvironment.deinit(self.allocator);
    self.expressionDefinitionDepth.deinit(self.allocator);

    self.allocator.free(self.filePath);

    // Free source buffers
    self.allocator.free(self.buffer);
    for (self.oldBuffers.items) |buffer| {
        self.allocator.free(buffer);
    }
    self.oldBuffers.deinit(self.allocator);

    self.exportedValues.deinit(self.allocator);
}

pub fn run(self: *ModuleInterpreter, program: *const Program) !void {
    self.runtimeError = null;
    _ = self.interpret(program) catch null;

    if (self.runtimeError != null) {
        self.runtimeError.?.printAndDeinit();
        self.runtimeError = null;
    }
}

fn interpret(self: *ModuleInterpreter, program: *const Program) !void {
    // TODO: Compile the program into bytecode and run it
    _ = self;
    _ = program;
}
