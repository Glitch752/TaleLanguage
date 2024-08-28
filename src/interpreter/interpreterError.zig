const std = @import("std");
const Token = @import("../token.zig").Token;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;
const prettyError = @import("../errors.zig").prettyError;
const errorContext = @import("../errors.zig").errorContext;

/// Zig errors can't hold payloads, so we separate the actual error data from the error type.
pub const RuntimeError = struct {
    message: []const u8,
    allocatedMessage: bool,

    allocator: std.mem.Allocator,

    token: Token,

    originalBuffer: []const u8,
    filePath: []const u8,

    pub fn tokenError(interpreter: *ModuleInterpreter, token: Token, comptime message: []const u8, format: anytype) RuntimeError {
        const formattedMessage = std.fmt.allocPrint(interpreter.allocator, message, format) catch {
            return .{
                .allocator = interpreter.allocator,
                .message = message,
                .allocatedMessage = false,
                .token = token,
                .originalBuffer = interpreter.originalBuffer,
                .filePath = interpreter.filePath,
            };
        };
        return .{
            .allocator = interpreter.allocator,
            .message = formattedMessage,
            .allocatedMessage = true,
            .token = token,
            .originalBuffer = interpreter.originalBuffer,
            .filePath = interpreter.filePath,
        };
    }

    pub fn printAndDeinit(self: *RuntimeError) void {
        self.print();
        if (self.allocatedMessage) {
            self.allocator.free(self.message);
        }
    }

    pub fn print(self: *const RuntimeError) void {
        const tokenString = self.token.toString(self.allocator) catch {
            return;
        };
        defer self.allocator.free(tokenString);

        const errorMessage = std.fmt.allocPrint(self.allocator, "\"{s}\" at {s}", .{ self.message, tokenString }) catch {
            return;
        };
        defer self.allocator.free(errorMessage);

        prettyError(errorMessage) catch {
            return;
        };
        errorContext(self.originalBuffer, self.filePath, self.token.position, self.token.lexeme.len, self.allocator) catch {
            return;
        };
    }
};

pub const InterpreterError = error{
    RuntimeError,
    /// Used when returning from functions. Functions catch this and place the return value in `lastReturnValue`.
    Return,
    /// Used when breaking out of loops. Loops catch this and break out of the loop.
    Break,
    /// Used when continuing in loops. Loops catch this and continue to the next iteration.
    Continue,
};
