const std = @import("std");
const Statement = @import("../parser/statement.zig").Statement;
const Expression = @import("../parser/expression.zig").Expression;
const Program = @import("../parser/program.zig").Program;
const Interpreter = @import("../interpreter/interpreter.zig").Interpreter;
const Token = @import("../token.zig").Token;

const errorContext = @import("../errors.zig").errorContext;
const prettyError = @import("../errors.zig").prettyError;

const Scopes = std.DoublyLinkedList(std.StringHashMapUnmanaged(bool));

const Resolver = @This();

const ResolverError = error{Unknown};

interpreter: *const Interpreter,
scopes: Scopes,

originalBuffer: []const u8 = "",
fileName: []const u8 = "",

pub fn init(interpreter: *const Interpreter, originalBuffer: []const u8, fileName: []const u8) Resolver {
    return .{
        .interpreter = interpreter,
        .originalBuffer = originalBuffer,
        .fileName = fileName,
    };
}

pub fn resolveProgram(self: *const Resolver, program: *const Program) !void {
    for (program.statements.items) |statement| {
        try self.resolveStatement(statement, 1);
    }
}

fn errorOn(self: *const Resolver, token: Token, message: []const u8, params: anytype) !void {
    const tokenString = try token.toString(self.interpreter.allocator);
    defer self.interpreter.allocator.free(tokenString);

    const formattedMessage = try std.fmt.allocPrint(self.interpreter.allocator, message, params);
    defer self.interpreter.allocator.free(formattedMessage);
    prettyError(formattedMessage);

    errorContext(self.originalBuffer, self.fileName, token.position, token.position + token.lexeme.len, self.interpreter.allocator);
}

fn unknownError(self: *const Resolver, message: []const u8, params: anytype) !void {
    const formattedMessage = try std.fmt.allocPrint(self.interpreter.allocator, message, params);
    defer self.interpreter.allocator.free(formattedMessage);
    prettyError(formattedMessage);
}

fn beginScope(self: *Resolver) void {
    self.scopes.append(std.StringHashMapUnmanaged(bool){});
}

fn endScope(self: *Resolver) void {
    const node = self.scopes.pop();
    if (node == null) {
        self.unknownError("Tried to end a scope but no scope was found", .{});
        return ResolverError.Unknown;
    }
}

fn declare(self: *const Resolver, name: Token) anyerror!void {
    if (self.scopes.len == 0) return;
    self.scopes.last.?.data.put(self.interpreter.allocator, name.lexeme, false);
}

fn define(self: *const Resolver, name: Token) anyerror!void {
    if (self.scopes.len == 0) return;
    self.scopes.last.?.data.put(self.interpreter.allocator, name.lexeme, true);
}

fn resolve(self: *const Resolver, name: Token) anyerror!void {
    if (self.scopes.len == 0) return;
    var current = self.scopes.last.?;
    var index = self.scopes.len - 1;
    while (current != null) {
        if (current.data.get(name.lexeme) != null) {
            self.interpreter.resolve(name, self.scopes.len - index - 1);
            return;
        }
        current = current.previous;
        index -= 1;
    }
}

fn resolveStatement(self: *const Resolver, statement: *const Statement, avoidDefiningBlockScope: bool) anyerror!void {
    switch (statement.*) {
        .Expression => |values| {
            try self.resolveExpression(values.expression);
        },
        .Let => |values| {
            try self.declare(values.name);
            try self.resolveExpression(values.initializer);
            try self.define(values.name);
        },
        .Block => |values| {
            if (!avoidDefiningBlockScope) self.beginScope();
            for (values.statements.items) |childStatement| {
                try self.resolveStatement(childStatement, false);
            }
            if (!avoidDefiningBlockScope) self.endScope();
        },

        .If => |values| {
            try self.resolveExpression(values.condition);
            try self.resolveStatement(values.trueBranch, false);
            if (values.falseBranch != null) {
                try self.resolveStatement(values.falseBranch.?, false);
            }
        },
        .While => |values| {
            try self.resolveExpression(values.condition);
            try self.resolveStatement(values.body, false);
        },

        .Return => |values| {
            try self.resolveExpression(values.value);
        },
        .Break => {},
        .Continue => {},
    }
}

fn resolveExpression(self: *const Resolver, expression: *const Expression) anyerror!void {
    switch (expression.*) {
        .Grouping => |values| {
            try self.resolveExpression(values.expression);
        },
        .Literal => |values| {
            const str = try values.value.toString(self.allocator);
            defer self.allocator.free(str);
        },

        .Binary => |values| {
            try self.resolveExpression(values.left);
            try self.resolveExpression(values.right);
        },
        .Unary => |values| {
            try self.resolveExpression(values.right);
        },
        .Logical => |values| {
            try self.resolveExpression(values.left);
            try self.resolveExpression(values.right);
        },
        .Bitwise => |values| {
            try self.resolveExpression(values.left);
            try self.resolveExpression(values.right);
        },

        .Function => |values| {
            try self.declare(self, values.name);
            try self.define(self, values.name);

            self.beginScope();
            for (values.parameters.items) |parameter| {
                try self.declare(self, parameter);
                try self.define(self, parameter);
            }
            try self.resolveStatement(values.body, true);
            self.endScope();
        },
        .FunctionCall => |values| {
            try self.resolveExpression(values.callee);
            for (values.arguments.items) |argument| {
                try self.resolveExpression(argument);
            }
        },

        .VariableAccess => |values| {
            if (self.scopes.len > 0 and self.scopes.last.?.data.get(values.name.lexeme) == false) {
                try self.errorOn(values.name, "Cannot read local variable in its own initializer", .{});
            }

            try self.resolve(values.name);
        },
        .VariableAssignment => |values| {
            try self.resolveExpression(values.value);
            try self.resolve(values.name);
        },
    }
}
