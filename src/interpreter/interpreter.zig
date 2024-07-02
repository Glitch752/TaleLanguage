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

pub const Interpreter = @This();

/// Zig errors can't hold payloads, so we separate the actual error data from the error type.
pub const RuntimeError = struct {
    message: []const u8,
    token: Token,

    originalBuffer: []const u8,
    fileName: []const u8,

    pub fn tokenError(interpreter: *Interpreter, message: []const u8, token: Token) RuntimeError {
        return .{ .message = message, .token = token, .originalBuffer = interpreter.originalBuffer, .fileName = interpreter.fileName };
    }

    pub fn print(self: *const RuntimeError, allocator: std.mem.Allocator) void {
        const tokenString = self.token.toString(allocator) catch {
            return;
        };
        defer allocator.free(tokenString);

        const errorMessage = std.fmt.allocPrint(allocator, "{s} at {s}", .{ self.message, tokenString }) catch {
            return;
        };
        defer allocator.free(errorMessage);

        prettyError(errorMessage) catch {
            return;
        };
        errorContext(self.originalBuffer, self.fileName, self.token.position, self.token.lexeme.len, allocator) catch {
            return;
        };
    }
};

pub const InterpreterError = error{
    RuntimeError,
};

allocator: std.mem.Allocator,
runtimeError: ?RuntimeError = null,

originalBuffer: []const u8 = "",
fileName: []const u8 = "",

rootEnvironment: Environment,
activeEnvironment: ?*Environment = null,

pub fn init(allocator: std.mem.Allocator) Interpreter {
    return .{ .allocator = allocator, .rootEnvironment = Environment.init(allocator) };
}

pub fn deinit(self: *Interpreter) void {
    // No cleanup needed for now
    _ = self;
}

pub fn run(self: *Interpreter, program: *const Program, originalBuffer: []const u8, fileName: []const u8) !void {
    self.originalBuffer = originalBuffer;
    self.fileName = fileName;

    self.activeEnvironment = &self.rootEnvironment;

    self.runtimeError = null;
    _ = self.interpret(program) catch null;

    if (self.runtimeError != null) {
        self.runtimeError.?.print(self.allocator);
    }
}

pub fn runExpression(self: *Interpreter, expression: *const Expression, originalBuffer: []const u8, fileName: []const u8) !VariableValue {
    self.originalBuffer = originalBuffer;
    self.fileName = fileName;

    self.activeEnvironment = &self.rootEnvironment;

    self.runtimeError = null;
    const result: ?VariableValue = self.interpretExpression(expression) catch null;

    if (self.runtimeError != null) {
        self.runtimeError.?.print(self.allocator);
    }
    if (result == null) {
        return VariableValue.null();
    }

    return result.?;
}

fn interpret(self: *Interpreter, program: *const Program) !void {
    for (program.statements.items) |statement| {
        try self.interpretStatement(statement);
    }
}

fn interpretStatement(self: *Interpreter, statement: *const Statement) !void {
    switch (statement.*) {
        .Expression => |values| {
            const result = try self.interpretExpression(values.expression);
            defer result.deinit(self.allocator);
        },
        .Let => |values| {
            const value = try self.interpretExpression(values.initializer);
            defer value.deinit(self.allocator);

            try self.rootEnvironment.define(values.name.lexeme, value);
        },
        .Block => |values| {
            var childEnvironment = self.rootEnvironment.createChild();
            defer childEnvironment.deinit();

            self.activeEnvironment = &childEnvironment;
            defer self.activeEnvironment = &self.rootEnvironment;

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
                try self.interpretStatement(values.body);
            }
        },
    }
}

fn interpretExpression(self: *Interpreter, expression: *const Expression) !VariableValue {
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
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
                    return InterpreterError.RuntimeError;
                },
                .Slash => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() / right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
                    return InterpreterError.RuntimeError;
                },
                .Star => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(left.asNumber() * right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
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
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must both be numbers or strings", values.operator);
                    return InterpreterError.RuntimeError;
                },
                .Percent => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromNumber(@mod(left.asNumber(), right.asNumber()));
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
                    return InterpreterError.RuntimeError;
                },

                .GreaterThan => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() > right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
                    return InterpreterError.RuntimeError;
                },
                .GreaterThanEqual => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() >= right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
                    return InterpreterError.RuntimeError;
                },
                .LessThan => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() < right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
                    return InterpreterError.RuntimeError;
                },
                .LessThanEqual => {
                    if (left.isNumber() and right.isNumber()) {
                        return VariableValue.fromBoolean(left.asNumber() <= right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operands must be numbers", values.operator);
                    return InterpreterError.RuntimeError;
                },

                .Equality => {
                    return VariableValue.fromBoolean(left.isEqual(right));
                },
                .NotEqual => {
                    return VariableValue.fromBoolean(!left.isEqual(right));
                },

                else => {
                    self.runtimeError = RuntimeError.tokenError(self, "Unknown operator", values.operator);
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
                    self.runtimeError = RuntimeError.tokenError(self, "Operand must be a number", values.operator);
                    return InterpreterError.RuntimeError;
                },
                .Negate => {
                    const right = try self.interpretExpression(values.right);
                    if (right.isBoolean()) {
                        return VariableValue.fromBoolean(!right.isTruthy());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, "Operand must be a boolean", values.operator);
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

        .FunctionCall => |values| {
            const callee = try self.interpretExpression(values.callee);
            defer callee.deinit(self.allocator);

            if (!callee.isCallable()) {
                self.runtimeError = RuntimeError.tokenError(self, "Can only call functions and classes", values.callee.token);
                return InterpreterError.RuntimeError;
            }

            const callable = callee.asCallable();

            if (values.arguments.items.len != callable.arity()) {
                self.runtimeError = RuntimeError.tokenError(self, "Expected {d} arguments but got {d}", .{ callable.arity(), values.arguments.items.len }, values.callee.token);
                return InterpreterError.RuntimeError;
            }

            const argumentValues = std.ArrayList(VariableValue).init(self.allocator);
            defer argumentValues.deinit();

            for (values.arguments.items) |argument| {
                const value = try self.interpretExpression(argument);
                defer value.deinit(self.allocator);

                argumentValues.append(value);
            }

            return callable.call(argumentValues, self);
        },

        .VariableAccess => |values| {
            const variable = try self.activeEnvironment.?.get(values.name, self);
            return variable;
        },
        .VariableAssignment => |values| {
            const value = try self.interpretExpression(values.value);
            defer value.deinit(self.allocator);

            try self.activeEnvironment.?.assign(values.name, value, self);
            return value;
        },
    }
}
