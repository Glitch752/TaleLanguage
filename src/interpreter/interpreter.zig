const std = @import("std");

const Expression = @import("../parser/expression.zig").Expression;
const Statement = @import("../parser/statement.zig").Statement;
const Program = @import("../parser/program.zig").Program;
const Environment = @import("./environment.zig").Environment;

const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const VariableValue = @import("./variable_value.zig").VariableValue;

const RuntimeError = @import("./interpreterError.zig").RuntimeError;
const InterpreterError = @import("./interpreterError.zig").InterpreterError;

const natives = @import("./natives.zig");
const NativeError = @import("./natives.zig").NativeError;

const ClassTypeReference = @import("./class_value.zig").ClassTypeReference;
const ClassMethod = @import("./class_value.zig").ClassMethod;

pub const Interpreter = @This();

allocator: std.mem.Allocator,
runtimeError: ?RuntimeError = null,

lastReturnValue: VariableValue = VariableValue.null(),

originalBuffer: []const u8 = "",
fileName: []const u8 = "",

rootEnvironment: Environment,
activeEnvironment: ?*Environment = null,

expressionDefinitionDepth: std.AutoHashMapUnmanaged(u32, u32),

pub fn init(allocator: std.mem.Allocator) !Interpreter {
    var environment = Environment.init(allocator);

    try environment.define("print", try VariableValue.nativeFunction(1, &natives.print), null);
    try environment.define("sin", try VariableValue.nativeFunction(1, &natives.sin), null);
    try environment.define("cos", try VariableValue.nativeFunction(1, &natives.cos), null);
    try environment.define("tan", try VariableValue.nativeFunction(1, &natives.tan), null);
    try environment.define("exp", try VariableValue.nativeFunction(1, &natives.exp), null);
    try environment.define("exp2", try VariableValue.nativeFunction(1, &natives.exp2), null);
    try environment.define("log", try VariableValue.nativeFunction(1, &natives.log), null);
    try environment.define("log2", try VariableValue.nativeFunction(1, &natives.log2), null);
    try environment.define("log10", try VariableValue.nativeFunction(1, &natives.log10), null);
    try environment.define("floor", try VariableValue.nativeFunction(1, &natives.floor), null);
    try environment.define("substring", try VariableValue.nativeFunction(3, &natives.substring), null);
    try environment.define("intChar", try VariableValue.nativeFunction(1, &natives.intChar), null);
    try environment.define("length", try VariableValue.nativeFunction(1, &natives.length), null);
    try environment.define("string", try VariableValue.nativeFunction(1, &natives.toString), null);

    var expressionDepths = std.AutoHashMapUnmanaged(u32, u32){};
    try expressionDepths.put(allocator, 0, 1);

    return .{ .allocator = allocator, .rootEnvironment = environment, .expressionDefinitionDepth = expressionDepths };
}

pub fn deinit(self: *Interpreter) void {
    self.rootEnvironment.deinit(self);
    self.expressionDefinitionDepth.deinit(self.allocator);
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

pub fn resolve(self: *Interpreter, expressionId: u32, depth: u32) !void {
    try self.expressionDefinitionDepth.put(self.allocator, expressionId, depth);
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
        try self.interpretStatement(statement, false);
    }
}

pub fn interpretStatement(self: *Interpreter, statement: *const Statement, avoidDefiningBlockScope: bool) anyerror!void {
    switch (statement.*) {
        .Expression => |values| {
            var result = try self.interpretExpression(values.expression);
            result.deinit(self);
        },
        .Let => |values| {
            const value = try self.interpretExpression(values.initializer);
            try self.activeEnvironment.?.define(values.name.lexeme, value, self);
        },
        .Block => |values| {
            var childEnvironment = if (!avoidDefiningBlockScope) try self.enterNewEnvironment() else self.activeEnvironment.?;
            defer if (!avoidDefiningBlockScope) childEnvironment.exit(self);

            for (values.statements.items) |childStatement| {
                try self.interpretStatement(childStatement, false);
            }
        },

        .If => |values| {
            var condition = try self.interpretExpression(values.condition);
            defer condition.deinit(self);

            if (condition.isTruthy()) {
                try self.interpretStatement(values.trueBranch, false);
            } else if (values.falseBranch != null) {
                try self.interpretStatement(values.falseBranch.?, false);
            }
        },
        .While => |values| {
            while ((try self.interpretExpression(values.condition)).isTruthy()) {
                self.interpretStatement(values.body, false) catch |err| switch (err) {
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

fn lookUpVariable(self: *Interpreter, name: Token, expression: *const Expression) !VariableValue {
    const depth = self.expressionDefinitionDepth.get(expression.id);
    if (depth == null) {
        return try (try self.rootEnvironment.get(name, self)).takeReference(self);
    }

    return try (try self.activeEnvironment.?.getAtDepth(name, depth.?, self)).takeReference(self);
}

pub fn interpretExpression(self: *Interpreter, expression: *const Expression) anyerror!VariableValue {
    switch (expression.*.value) {
        .Grouping => |values| {
            return self.interpretExpression(values.expression);
        },
        .Literal => |values| {
            return VariableValue.fromLiteral(values.value);
        },

        .Binary => |values| {
            var left = try self.interpretExpression(values.left);
            defer left.deinit(self);

            var right = try self.interpretExpression(values.right);
            defer right.deinit(self);

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
                    var right = try self.interpretExpression(values.right);
                    if (right.isNumber()) {
                        return VariableValue.fromNumber(-right.asNumber());
                    }
                    self.runtimeError = RuntimeError.tokenError(self, values.operator, "Operand must be a number", .{});
                    return InterpreterError.RuntimeError;
                },
                .Negate => {
                    var right = try self.interpretExpression(values.right);
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
            var left = try self.interpretExpression(values.left);
            errdefer left.deinit(self);

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
            errdefer right.deinit(self);

            return right;
        },
        .Bitwise => |value| {
            // We take the Javascript approach and treat bitwise operators as integer operations
            var left = try self.interpretExpression(value.left);
            defer left.deinit(self);

            var right = try self.interpretExpression(value.right);
            defer right.deinit(self);

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
            var callee = try self.interpretExpression(values.callee);
            defer callee.deinit(self);

            if (!callee.isCallable()) {
                self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Can only call functions and class types", .{});
                return InterpreterError.RuntimeError;
            }

            var callable = callee.asCallable().takeReference();
            defer _ = callable.deinit(self);

            if (values.arguments.items.len != callable.getArity()) {
                self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Expected {d} arguments but got {d}", .{ callable.getArity(), values.arguments.items.len });
                return InterpreterError.RuntimeError;
            }

            var argumentResults = std.ArrayList(VariableValue).init(self.allocator);
            defer {
                for (argumentResults.items) |value| {
                    var val = value;
                    val.deinit(self);
                }
                argumentResults.deinit();
            }

            for (values.arguments.items) |argument| {
                var value = try self.interpretExpression(argument);
                errdefer value.deinit(self);

                try argumentResults.append(value);
            }

            var argumentValues = std.ArrayList(VariableValue).init(self.allocator);
            defer argumentValues.deinit();
            for (argumentResults.items) |result| {
                try argumentValues.append(result);
            }

            return callable.call(self, values.startToken, argumentValues) catch |err| switch (err) {
                NativeError.InvalidOperand => {
                    self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Invalid operand", .{});
                    return InterpreterError.RuntimeError;
                },
                else => {
                    return err;
                },
            };
        },
        .Function => |values| {
            const function = try VariableValue.newFunction(values, self.activeEnvironment.?, self.allocator);
            return function;
        },

        .Class => |values| {
            var environment = try self.enterNewEnvironment();
            defer environment.exit(self);

            var superClass: ?ClassTypeReference = null;
            if (values.superClass != null) {
                superClass = (try self.interpretExpression(values.superClass.?)).asClassType().strongClone();
                errdefer _ = superClass.?.deinit(self);
                try environment.define("super", .{ .ClassType = .{ .ClassType = superClass.? } }, self);
            }

            const class = try VariableValue.newClassType(values, environment, self.allocator, superClass);
            // We don't copy the value when defining the class here because we can't create a loop of references
            try environment.define("this", class.takeWeakReference(), self); // "this" is defined here because static methods should access the class type itself

            return class;
        },
        .This => |token| return try self.lookUpVariable(token, expression),
        .Super => |call| {
            const depth = self.expressionDefinitionDepth.get(expression.id);
            if (depth == null) {
                self.runtimeError = RuntimeError.tokenError(self, call.superToken, "Cannot use 'super' outside of a class", .{});
                return InterpreterError.RuntimeError;
            }

            const superClass = try self.activeEnvironment.?.getAtDepth(call.superToken, depth.?, self);

            if (!self.activeEnvironment.?.lexemeExistsAtDepth("this", depth.? - 1)) {
                // This is a static method
                if (call.method == null) {
                    self.runtimeError = RuntimeError.tokenError(self, call.superToken, "Cannot use a super constructor in a static method", .{});
                    return InterpreterError.RuntimeError;
                }

                return superClass.asClassType().ptr().getStatic(call.method.?, self);
            }

            const object = try self.activeEnvironment.?.getLexemeAtDepth("this", call.superToken, depth.? - 1, self);

            var lexeme: []const u8 = "constructor";
            if (call.method != null) {
                lexeme = call.method.?.lexeme;
            }

            const method = superClass.asClassType().ptr().getInstanceMethod(lexeme);
            if (method == null) {
                self.runtimeError = RuntimeError.tokenError(self, call.superToken, "Undefined method '{s}'", .{call.method.?.lexeme});
                return InterpreterError.RuntimeError;
            }

            return .{ .Function = method.?.function.bindToClass(object.referenceClassInstance()) };
        },

        .VariableAccess => |values| {
            return try self.lookUpVariable(values.name, expression);
        },
        .VariableAssignment => |values| {
            var value = try self.interpretExpression(values.value);
            // Don't deinit unless there's an error -- we want to keep the value since we're storing it
            errdefer value.deinit(self);

            const depth = self.expressionDefinitionDepth.get(expression.id);
            if (depth == null) {
                try self.rootEnvironment.assign(values.name, try value.takeReference(self), self);
                if (self.runtimeError != null) {
                    return InterpreterError.RuntimeError;
                }
            } else {
                try self.activeEnvironment.?.assignAtDepth(values.name, try value.takeReference(self), depth.?, self);
                if (self.runtimeError != null) {
                    return InterpreterError.RuntimeError;
                }
            }

            return value;
        },

        .PropertyAccess => |values| {
            var object = try self.interpretExpression(values.object);
            defer object.deinit(self);

            if (!object.isClassInstance()) {
                if (!object.isClassType()) {
                    self.runtimeError = RuntimeError.tokenError(self, values.name, "Can only access properties on class instances or types", .{});
                    return InterpreterError.RuntimeError;
                }
                var classType = object.asClassType();
                return try classType.ptr().getStatic(values.name, self);
            }

            var instance = object.referenceClassInstance();
            defer _ = instance.deinit(self);
            return try instance.ptr().get(values.name, instance, self);
        },
        .PropertyAssignment => |values| {
            var object = try self.interpretExpression(values.object);
            defer object.deinit(self);

            var value = try self.interpretExpression(values.value);
            errdefer value.deinit(self);

            if (!object.isClassInstance()) {
                if (!object.isClassType()) {
                    self.runtimeError = RuntimeError.tokenError(self, values.name, "Can only access properties on class instances or types", .{});
                    return InterpreterError.RuntimeError;
                }
                var classType = object.asClassType();
                try classType.unsafePtr().setStatic(values.name, value, self);
                if (self.runtimeError != null) return InterpreterError.RuntimeError;

                return value;
            }

            var instance = object.referenceClassInstance();
            defer _ = instance.deinit(self);

            try instance.unsafePtr().set(values.name, value, self);
            if (self.runtimeError != null) return InterpreterError.RuntimeError;

            return value;
        },
    }
}
