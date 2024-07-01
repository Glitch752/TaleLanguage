const std = @import("std");

const Expression = @import("../parser/expression.zig").Expression;
const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const VariableValue = @import("./variable_value.zig").VariableValue;

const prettyError = @import("../errors.zig").prettyError;
const errorContext = @import("../errors.zig").errorContext;

pub const Interpreter = @This();

/// Zig errors can't hold payloads, so we separate the actual error data from the error type.
const RuntimeError = struct {
    message: []const u8,
    token: *const Token,

    originalBuffer: []const u8,
    fileName: []const u8,

    pub fn init(interpreter: *Interpreter, message: []const u8, token: *const Token) RuntimeError {
        return .{ .message = message, .token = token, .originalBuffer = interpreter.originalBuffer, .fileName = interpreter.fileName };
    }

    pub fn print(self: *const RuntimeError, allocator: std.mem.Allocator) void {
        const tokenString = self.token.toString(allocator) catch {
            return;
        };
        defer allocator.free(tokenString);

        const errorMessage = std.fmt.allocPrint(allocator, "{s} at {s}", .{ self.message.string, tokenString }) catch {
            return;
        };
        defer allocator.free(errorMessage);

        prettyError(errorMessage) catch {
            return;
        };
        errorContext(self.originalBuffer, self.fileName, self.token.position, allocator) catch {
            return;
        };
    }
};

const InterpretError = error{
    RuntimeError,
};

allocator: std.mem.Allocator,
runtimeError: ?RuntimeError = null,

originalBuffer: []const u8 = "",
fileName: []const u8 = "",

pub fn init(allocator: std.mem.Allocator) Interpreter {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Interpreter) void {
    // No cleanup needed for now
    _ = self;
}

pub fn run(self: *Interpreter, expression: *const Expression, originalBuffer: []const u8, fileName: []const u8) !void {
    self.originalBuffer = originalBuffer;
    self.fileName = fileName;

    self.runtimeError = null;
    const result: ?VariableValue = self.interpret(expression) catch null;

    if (self.runtimeError != null) {
        self.runtimeError.?.print(self.allocator);
    } else {
        const str = try result.?.toString(self.allocator);
        defer self.allocator.free(str);

        std.debug.print("{s}\n", .{str});
    }
}

fn interpret(self: *Interpreter, expression: *const Expression) !VariableValue {
    switch (expression.*) {
        .Binary => |values| {
            const left = try self.interpret(values.left);
            const right = try self.interpret(values.right);

            switch (values.operator.type) {
                .Minus => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() - right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must be numbers", &values.operator);
                    return InterpretError.RuntimeError;
                },
                .Slash => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() / right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must be numbers", &values.operator);
                    return InterpretError.RuntimeError;
                },
                .Star => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() * right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must be numbers", &values.operator);
                    return InterpretError.RuntimeError;
                },
                .Plus => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() + right.asNumber());
                    }
                    if (left.isString() and right.isString()) {
                        const mergedString = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{
                            left.asStringDeinit(self.allocator),
                            right.asStringDeinit(self.allocator),
                        });
                        return VariableValue.fromString(mergedString);
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must both be numbers or strings", &values.operator);
                    return InterpretError.RuntimeError;
                },

                .GreaterThan => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() > right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must be numbers", &values.operator);
                    return InterpretError.RuntimeError;
                },
                .GreaterThanEqual => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() >= right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must be numbers", &values.operator);
                    return InterpretError.RuntimeError;
                },
                .LessThan => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() < right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must be numbers", &values.operator);
                    return InterpretError.RuntimeError;
                },
                .LessThanEqual => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() <= right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operands must be numbers", &values.operator);
                    return InterpretError.RuntimeError;
                },

                .Equality => {
                    return VariableValue.fromBoolean(left == right);
                },
                .NotEqual => {
                    return VariableValue.fromBoolean(left != right);
                },

                else => {
                    self.runtimeError = RuntimeError.init(self, "Unknown operator", &values.operator);
                    return InterpretError.RuntimeError;
                },
            }
        },
        .Grouping => |values| {
            return self.interpret(values.expression);
        },
        .Literal => |values| {
            return VariableValue.fromLiteral(values.value);
        },
        .Unary => |values| {
            switch (values.operator.type) {
                .Minus => {
                    const right = try self.interpret(values.right);
                    if (right.isNumber()) {
                        return VariableValue.fromNumber(-right.asNumber());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operand must be a number", &values.right);
                    return InterpretError.RuntimeError;
                },
                .Negate => {
                    const right = try self.interpret(values.right);
                    if (right.isBoolean()) {
                        return VariableValue.fromBoolean(!right.isTruthy());
                    }
                    self.runtimeError = RuntimeError.init(self, "Operand must be a boolean", &values.right);
                    return InterpretError.RuntimeError;
                },
                else => {
                    return VariableValue.null();
                },
            }
        },
    }
}
