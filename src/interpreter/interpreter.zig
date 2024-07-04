const std = @import("std");

const Expression = @import("../parser/expression.zig").Expression;
const Statement = @import("../parser/statement.zig").Statement;
const Program = @import("../parser/program.zig").Program;
const Environment = @import("./environment.zig").Environment;

const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const VariableValue = @import("./variable_value.zig").VariableValue;

const prettyError = @import("../errors.zig").prettyError;
const errorContext = @import("../errors.zig").errorContext;

const natives = @import("./natives.zig");

pub const Interpreter = @This();

/// Zig errors can't hold payloads, so we separate the actual error data from the error type.
pub const RuntimeError = struct {
    message: []const u8,
    allocatedMessage: bool,

    allocator: std.mem.Allocator,

    token: Token,

    originalBuffer: []const u8,
    fileName: []const u8,

    pub fn tokenError(interpreter: *Interpreter, token: Token, comptime message: []const u8, format: anytype) RuntimeError {
        const formattedMessage = std.fmt.allocPrint(interpreter.allocator, message, format) catch {
            return .{
                .allocator = interpreter.allocator,
                .message = message,
                .allocatedMessage = false,
                .token = token,
                .originalBuffer = interpreter.originalBuffer,
                .fileName = interpreter.fileName,
            };
        };
        return .{
            .allocator = interpreter.allocator,
            .message = formattedMessage,
            .allocatedMessage = true,
            .token = token,
            .originalBuffer = interpreter.originalBuffer,
            .fileName = interpreter.fileName,
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
        errorContext(self.originalBuffer, self.fileName, self.token.position, self.token.lexeme.len, self.allocator) catch {
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

allocator: std.mem.Allocator,
runtimeError: ?RuntimeError = null,

lastReturnValue: VariableValue = VariableValue.null(),

originalBuffer: []const u8 = "",
fileName: []const u8 = "",

rootEnvironment: Environment,
activeEnvironment: ?*Environment = null,

pub fn init(allocator: std.mem.Allocator) !Interpreter {
    var environment = Environment.init(allocator);

    try environment.define("print", VariableValue.nativeFunction(1, &natives.print));

    return .{ .allocator = allocator, .rootEnvironment = environment };
}

pub fn deinit(self: *Interpreter) void {
    self.rootEnvironment.deinit(self);
}

pub fn run(self: *Interpreter, program: *const Program, originalBuffer: []const u8, fileName: []const u8) !void {
    self.originalBuffer = originalBuffer;
    self.fileName = fileName;

    self.activeEnvironment = &self.rootEnvironment;

    self.runtimeError = null;
    _ = self.interpret(program) catch null;

    if (self.runtimeError != null) {
        self.runtimeError.?.printAndDeinit();
    }
}

pub fn runExpression(self: *Interpreter, expression: *const Expression, originalBuffer: []const u8, fileName: []const u8) !VariableValue {
    self.originalBuffer = originalBuffer;
    self.fileName = fileName;

    self.activeEnvironment = &self.rootEnvironment;

    self.runtimeError = null;
    const result: ?VariableValue = self.interpretExpression(expression) catch null;

    if (self.runtimeError != null) {
        self.runtimeError.?.printAndDeinit();
    }
    if (result == null) {
        return VariableValue.null();
    }

    return result.?;
}

pub fn enterNewEnvironment(self: *Interpreter) !*Environment {
    const child = try self.allocator.create(Environment);
    child.* = self.activeEnvironment.?.createChild(self.activeEnvironment.?);
    self.activeEnvironment = child;
    return child;
}

pub fn enterChildEnvironment(self: *Interpreter, parent: *Environment, previous: *Environment) !*Environment {
    const child = try self.allocator.create(Environment);
    child.* = parent.createChild(previous);
    self.activeEnvironment = child;
    return child;
}

fn interpret(self: *Interpreter, program: *const Program) !void {
    for (program.statements.items) |statement| {
        try self.interpretStatement(statement);
    }
}

pub fn interpretStatement(self: *Interpreter, statement: *const Statement) anyerror!void {
    switch (statement.*) {
        .Expression => |values| {
            const result = try self.interpretExpression(values.expression);
            defer result.deinit(self.allocator);
        },
        .Let => |values| {
            const value = try self.interpretExpression(values.initializer);
            defer value.deinit(self.allocator);

            try self.activeEnvironment.?.define(values.name.lexeme, value);
        },
        .Block => |values| {
            var childEnvironment = try self.enterNewEnvironment();
            defer childEnvironment.deinit(self);

            for (values.statements.items) |childStatement| {
                try self.interpretStatement(childStatement);
            }
        },

        .If => |values| {
            const condition = try self.interpretExpression(values.condition);
            defer condition.deinit(self.allocator);

            if (condition.isTruthy()) {
                try self.interpretStatement(values.trueBranch);
            } else if (values.falseBranch != null) {
                try self.interpretStatement(values.falseBranch.?);
            }
        },
        .While => |values| {
            while ((try self.interpretExpression(values.condition)).isTruthy()) {
                self.interpretStatement(values.body) catch |err| switch (err) {
                    InterpreterError.Break => break,
                    InterpreterError.Continue => continue,
                    else => |value| return value,
                };
            }
        },

        .Return => |values| {
            self.lastReturnValue = try self.interpretExpression(values.value);
            return InterpreterError.Return;
        },
        .Break => return InterpreterError.Break,
        .Continue => return InterpreterError.Continue,
    }
}

fn interpretExpression(self: *Interpreter, expression: *const Expression) anyerror!VariableValue {
    switch (expression.*) {
        .Grouping => |values| {
            return self.interpretExpression(values.expression);
        },
        .Literal => |values| {
            return VariableValue.fromLiteral(values.value);
        },

        .Binary => |values| {
            const left = try self.interpretExpression(values.left);
            errdefer left.deinit(self.allocator);

            const right = try self.interpretExpression(values.right);
            errdefer right.deinit(self.allocator);

            switch (values.operator.type) {
                .Minus => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() - right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },
                .Slash => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() / right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },
                .Star => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() * right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },
                .Plus => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() + right.asNumber());
                    }
                    if (left.isString() and right.isString()) {
                        const mergedString = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{
                            left.asString(),
                            right.asString(),
                        });

                        left.deinit(self.allocator);
                        right.deinit(self.allocator);

                        return VariableValue.fromString(mergedString, true);
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must both be numbers or strings", .{});
                    return InterpreterError.RuntimeError;
                },
                .Percent => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(@mod(left.asNumber(), right.asNumber()));
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },

                .GreaterThan => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() > right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },
                .GreaterThanEqual => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() >= right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },
                .LessThan => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() < right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },
                .LessThanEqual => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() <= right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operands must be numbers", .{});
                    return InterpreterError.RuntimeError;
                },

                .Equality => {
                    return VariableValue.fromBoolean(left.isEqual(right));
                },
                .NotEqual => {
                    return VariableValue.fromBoolean(!left.isEqual(right));
                },

                else => {
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Unknown operator", .{});
                    return InterpreterError.RuntimeError;
                },
            }
        },
        .Unary => |values| {
            switch (values.operator.type) {
                .Minus => {
                    const right = try self.interpretExpression(values.right);
                    if (right.isNumber()) {
                        return VariableValue.fromNumber(-right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operand must be a number", .{});
                    return InterpreterError.RuntimeError;
                },
                .Negate => {
                    const right = try self.interpretExpression(values.right);
                    if (right.isBoolean()) {
                        return VariableValue.fromBoolean(!right.isTruthy());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operand must be a boolean", .{});
                    return InterpreterError.RuntimeError;
                },
                else => {
                    return VariableValue.null();
                },
            }
        },
        .Logical => |values| {
            const left = try self.interpretExpression(values.left);
            errdefer left.deinit(self.allocator);

            if (values.operator.type == .Or) {
                if (left.isTruthy()) {
                    return left;
                }
            } else {
                if (!left.isTruthy()) {
                    return left;
                }
            }

            const right = try self.interpretExpression(values.right);
            errdefer right.deinit(self.allocator);

            return right;
        },
        .Bitwise => |value| {
            // We take the Javascript approach and treat bitwise operators as integer operations
            const left = try self.interpretExpression(value.left);
            errdefer left.deinit(self.allocator);

            const right = try self.interpretExpression(value.right);
            errdefer right.deinit(self.allocator);

            if (!left.isNumber() or !right.isNumber()) {
                self.runtimeError = RuntimeError.tokenError(self, value.operator, "Operands must be numbers", .{});
                return InterpreterError.RuntimeError;
            }

            const leftInt = @as(u64, @intFromFloat(left.asNumber()));
            const rightInt = @as(u64, @intFromFloat(right.asNumber()));

            switch (value.operator.type) {
                .BitwiseAnd => return VariableValue.fromNumber(@as(f64, @floatFromInt(leftInt & rightInt))),
                .BitwiseOr => return VariableValue.fromNumber(@as(f64, @floatFromInt(leftInt | rightInt))),
                .BitwiseXor => return VariableValue.fromNumber(@as(f64, @floatFromInt(leftInt ^ rightInt))),
                else => {
                    self.runtimeError = RuntimeError.tokenError(self, value.operator, "Unknown operator", .{});
                    return InterpreterError.RuntimeError;
                },
            }
        },

        .FunctionCall => |values| {
            const callee = try self.interpretExpression(values.callee);
            defer callee.deinit(self.allocator);

            if (!callee.isCallable()) {
                self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Can only call functions and classes", .{});
                return InterpreterError.RuntimeError;
            }

            const callable = callee.asCallable();

            if (values.arguments.items.len != callable.arity) {
                self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Expected {d} arguments but got {d}", .{ callable.arity, values.arguments.items.len });
                return InterpreterError.RuntimeError;
            }

            var argumentValues = std.ArrayList(VariableValue).init(self.allocator);
            defer argumentValues.deinit();

            for (values.arguments.items) |argument| {
                const value = try self.interpretExpression(argument);
                defer value.deinit(self.allocator);

                try argumentValues.append(value);
            }

            return callable.call(self, argumentValues);
        },
        .Function => |values| {
            const function = VariableValue.fromFunction(values, self.activeEnvironment.?);
            return function;
        },

        .VariableAccess => |values| {
            const variable = try self.activeEnvironment.?.get(values.name, self);
            return variable;
        },
        .VariableAssignment => |values| {
            const value = try self.interpretExpression(values.value);
            defer value.deinit(self.allocator);

            try self.activeEnvironment.?.assign(values.name, value, self);
            if (self.runtimeError != null) {
                return InterpreterError.RuntimeError;
            }
            return value;
        },
    }
}
