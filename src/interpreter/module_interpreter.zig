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

const CallableNativeFunction = @import("./callable_value.zig").CallableNativeFunction;

const Interpreter = @import("./interpreter.zig").Interpreter;
const ExportedValueSet = @import("./module_value.zig").ExportedValueSet;

const Module = @import("./module_value.zig").Module;

pub const ModuleInterpreter = @This();

allocator: std.mem.Allocator,
runtimeError: ?RuntimeError = null,

lastReturnValue: VariableValue = VariableValue.null(),

buffer: []const u8 = "",
oldBuffers: std.ArrayListUnmanaged([]const u8) = std.ArrayListUnmanaged([]const u8){},

filePath: []const u8,

rootEnvironment: Environment,
activeEnvironment: ?*Environment = null,

expressionDefinitionDepth: std.AutoHashMapUnmanaged(u32, u32),

interpreter: *Interpreter,
exportedValues: ExportedValueSet,

pub fn init(allocator: std.mem.Allocator, interpreter: *Interpreter, filePath: []const u8) !ModuleInterpreter {
    var environment = Environment.init(allocator);
    try environment.define("import", try VariableValue.nativeFunction(1, Interpreter.import), null);

    var expressionDepths = std.AutoHashMapUnmanaged(u32, u32){};
    try expressionDepths.put(allocator, 0, 1);

    return .{ .allocator = allocator, .rootEnvironment = environment, .expressionDefinitionDepth = expressionDepths, .exportedValues = ExportedValueSet{}, .interpreter = interpreter, .filePath = filePath };
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

    var exportedValueIter = self.exportedValues.iterator();
    while (exportedValueIter.next()) |entry| {
        self.allocator.free(entry.key_ptr.*);
    }
    self.exportedValues.deinit(self.allocator);
}

pub fn run(self: *ModuleInterpreter, program: *const Program, originalBuffer: []const u8) !void {
    if (self.buffer.len != 0) {
        try self.oldBuffers.append(self.allocator, self.buffer);
    }
    self.buffer = originalBuffer;

    self.activeEnvironment = &self.rootEnvironment;

    self.runtimeError = null;
    _ = self.interpret(program) catch null;

    if (self.runtimeError != null) {
        self.runtimeError.?.printAndDeinit();
        self.runtimeError = null;
    }
}

pub fn runExpression(self: *ModuleInterpreter, expression: *const Expression, originalBuffer: []const u8, filePath: []const u8) !VariableValue {
    self.buffer = originalBuffer;
    self.filePath = filePath;

    self.activeEnvironment = &self.rootEnvironment;

    const result: VariableValue = try self.interpretExpression(expression);
    return result;
}

pub fn resolve(self: *ModuleInterpreter, expressionId: u32, depth: u32) !void {
    try self.expressionDefinitionDepth.put(self.allocator, expressionId, depth);
}

pub fn enterNewEnvironment(self: *ModuleInterpreter) !*Environment {
    const child = try self.allocator.create(Environment);
    child.* = self.activeEnvironment.?.createChild(self.activeEnvironment.?);
    self.activeEnvironment = child;
    return child;
}

pub fn enterChildEnvironment(self: *ModuleInterpreter, parent: *Environment, previous: *Environment) !*Environment {
    const child = try self.allocator.create(Environment);
    child.* = parent.createChild(previous);
    self.activeEnvironment = child;
    return child;
}

fn interpret(self: *ModuleInterpreter, program: *const Program) !void {
    for (program.statements.items) |statement| {
        try self.interpretStatement(statement, false);
    }
}

pub fn interpretStatement(self: *ModuleInterpreter, statement: *const Statement, avoidDefiningBlockScope: bool) anyerror!void {
    switch (statement.*) {
        .Expression => |values| {
            var result = try self.interpretExpression(values.expression);
            result.deinit(self.allocator);
        },
        .Let => |values| {
            const value = try self.interpretExpression(values.initializer);
            try self.activeEnvironment.?.define(values.name.lexeme, value, self);
            if (values.exported) {
                const duplicatedName = try self.allocator.dupe(u8, values.name.lexeme);
                try self.exportedValues.put(self.allocator, duplicatedName, 0);
            }
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
            defer condition.deinit(self.allocator);

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

fn lookUpVariable(self: *ModuleInterpreter, name: Token, expression: *const Expression) !VariableValue {
    const depth = self.expressionDefinitionDepth.get(expression.id);
    if (depth == null) {
        return try (try self.rootEnvironment.getSelf(name, self)).takeReference(self.allocator);
    }

    return try (try self.activeEnvironment.?.getAtDepth(name, depth.?, self)).takeReference(self.allocator);
}

pub fn interpretExpression(self: *ModuleInterpreter, expression: *const Expression) anyerror!VariableValue {
    switch (expression.*.value) {
        .Grouping => |values| {
            return self.interpretExpression(values.expression);
        },
        .Literal => |values| {
            return VariableValue.fromLiteral(values.value);
        },

        .Binary => |values| {
            var left = try self.interpretExpression(values.left);
            defer left.deinit(self.allocator);

            var right = try self.interpretExpression(values.right);
            defer right.deinit(self.allocator);

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
                    defer right.deinit(self.allocator);
                    return VariableValue.fromBoolean(!right.isTruthy()); // We intentionally coerce the value to a boolean here
                },
                else => {
                    return VariableValue.null();
                },
            }
        },
        .Logical => |values| {
            var left = try self.interpretExpression(values.left);
            {
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
            }

            left.deinit(self.allocator);

            const right = try self.interpretExpression(values.right);
            errdefer right.deinit(self.allocator);

            return right;
        },
        .Bitwise => |value| {
            // We take the Javascript approach and treat bitwise operators as integer operations
            var left = try self.interpretExpression(value.left);
            defer left.deinit(self.allocator);

            var right = try self.interpretExpression(value.right);
            defer right.deinit(self.allocator);

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
            defer callee.deinit(self.allocator);

            if (!callee.isCallable()) {
                self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Can only call functions and class types; tried to call {s}", .{callee.typeNameString()});
                return InterpreterError.RuntimeError;
            }

            var callable = callee.asCallable();

            if (values.arguments.items.len != callable.getArity()) {
                self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Expected {d} arguments but got {d}", .{ callable.getArity(), values.arguments.items.len });
                return InterpreterError.RuntimeError;
            }

            var argumentResults = std.ArrayList(VariableValue).init(self.allocator);
            defer argumentResults.deinit();

            for (values.arguments.items) |argument| {
                var value = try self.interpretExpression(argument);
                errdefer value.deinit(self.allocator);

                try argumentResults.append(value);
            }

            return callable.call(self, values.startToken, argumentResults) catch |err| switch (err) {
                NativeError.InvalidOperand => {
                    self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Invalid operand", .{});
                    return InterpreterError.RuntimeError;
                },
                NativeError.Panic => {
                    self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Panic", .{});
                    return InterpreterError.RuntimeError;
                },
                else => {
                    return err;
                },
            };
        },
        .Function => |values| {
            const function = try VariableValue.newFunction(values, self.activeEnvironment.?, self, self.allocator);
            return function;
        },

        .Class => |values| {
            var environment = try self.enterNewEnvironment();
            defer environment.exit(self);

            var superClass: ?ClassTypeReference = null;
            errdefer _ = if (superClass != null) superClass.?.deinit(self.allocator);
            if (values.superClass != null) {
                var value = try self.interpretExpression(values.superClass.?);
                defer value.deinit(self.allocator);

                if (!value.isClassType()) {
                    self.runtimeError = RuntimeError.tokenError(self, values.startToken, "Must extend class type", .{});
                    return InterpreterError.RuntimeError;
                }

                superClass = value.asClassType().strongClone();
                errdefer _ = superClass.?.deinit(self.allocator);

                try environment.define("super", value.takeWeakReference(), self);
            }

            const class = try VariableValue.newClassType(values, self, environment, superClass);

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

            var superClassReference = superClass.referenceClassType();
            defer _ = superClassReference.deinit(self.allocator);

            if (!self.activeEnvironment.?.lexemeExistsAtDepth("this", depth.? - 1)) {
                // This is a static method
                if (call.method == null) {
                    self.runtimeError = RuntimeError.tokenError(self, call.superToken, "Cannot use a super constructor in a static method", .{});
                    return InterpreterError.RuntimeError;
                }

                return superClassReference.ptr().getStatic(call.method.?, self);
            }

            const object = try self.activeEnvironment.?.getLexemeAtDepth("this", call.superToken, depth.? - 1, self);

            var lexeme: []const u8 = "constructor";
            if (call.method != null) {
                lexeme = call.method.?.lexeme;
            }

            const method = superClassReference.ptr().getInstanceMethod(lexeme);
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
            errdefer value.deinit(self.allocator);

            const depth = self.expressionDefinitionDepth.get(expression.id);
            if (depth == null) {
                try self.rootEnvironment.assign(values.name, try value.takeReference(self.allocator), self);
                if (self.runtimeError != null) {
                    return InterpreterError.RuntimeError;
                }
            } else {
                try self.activeEnvironment.?.assignAtDepth(values.name, try value.takeReference(self.allocator), depth.?, self);
                if (self.runtimeError != null) {
                    return InterpreterError.RuntimeError;
                }
            }

            return value;
        },

        .PropertyAccess => |values| {
            var object = try self.interpretExpression(values.object);
            defer object.deinit(self.allocator);

            if (!object.isClassInstance()) {
                if (!object.isClassType()) {
                    if (!object.isModuleType()) {
                        self.runtimeError = RuntimeError.tokenError(self, values.name, "Can only access properties on class instances, class types, and modules. Accessing {s} on value {s}.", .{ values.name.lexeme, object.typeNameString() });
                        return InterpreterError.RuntimeError;
                    }
                    return try object.asModuleType().accessExport(values.name, self);
                }
                var classType = object.referenceClassType();
                defer _ = classType.deinit(self.allocator);
                return try classType.ptr().getStatic(values.name, self);
            }

            var instance = object.referenceClassInstance();
            defer _ = instance.deinit(self.allocator);
            return try instance.ptr().get(values.name, instance, self);
        },
        .PropertyAssignment => |values| {
            var object = try self.interpretExpression(values.object);
            defer object.deinit(self.allocator);

            var value = try self.interpretExpression(values.value);
            errdefer value.deinit(self.allocator);

            if (!object.isClassInstance()) {
                if (!object.isClassType()) {
                    self.runtimeError = RuntimeError.tokenError(self, values.name, "Can only assign properties on class instances or class types. Accessing {s} on value {s}.", .{ values.name.lexeme, object.typeNameString() });
                    return InterpreterError.RuntimeError;
                }
                var classType = object.referenceClassType();
                defer _ = classType.deinit(self.allocator);

                try classType.unsafePtr().setStatic(values.name, value, self);
                if (self.runtimeError != null) return InterpreterError.RuntimeError;

                return value;
            }

            var instance = object.referenceClassInstance();
            defer _ = instance.deinit(self.allocator);

            try instance.unsafePtr().set(values.name, value, self);
            if (self.runtimeError != null) return InterpreterError.RuntimeError;

            return value;
        },
    }
}
