const std = @import("std");
const Statement = @import("../parser/statement.zig").Statement;
const Expression = @import("../parser/expression.zig").Expression;
const FunctionExpression = @import("../parser/expression.zig").FunctionExpression;
const Program = @import("../parser/program.zig").Program;
const Interpreter = @import("../interpreter/interpreter.zig").Interpreter;
const Token = @import("../token.zig").Token;

const errorContext = @import("../errors.zig").errorContext;
const prettyError = @import("../errors.zig").prettyError;

const Scopes = std.DoublyLinkedList(std.StringHashMapUnmanaged(bool));

const Resolver = @This();

const ResolverError = error{Unknown};

interpreter: *Interpreter,
scopes: Scopes,

originalBuffer: []const u8 = "",
fileName: []const u8 = "",

pub fn init(interpreter: *Interpreter, originalBuffer: []const u8, fileName: []const u8) Resolver {
    return .{
        .interpreter = interpreter,
        .originalBuffer = originalBuffer,
        .fileName = fileName,
        .scopes = Scopes{},
    };
}

pub fn deinit(self: *Resolver) void {
    while (self.scopes.len > 0) { // Should be empty, but just in case
        self.endScope() catch unreachable;
    }
}

pub fn resolveProgram(self: *Resolver, program: *const Program) !void {
    for (program.statements.items) |statement| {
        try self.resolveStatement(statement, false);
    }
}

fn errorOn(self: *const Resolver, token: Token, comptime message: []const u8, params: anytype) !void {
    const tokenString = try token.toString(self.interpreter.allocator);
    defer self.interpreter.allocator.free(tokenString);

    const formattedMessage = try std.fmt.allocPrint(self.interpreter.allocator, message, params);
    defer self.interpreter.allocator.free(formattedMessage);
    try prettyError(formattedMessage);

    try errorContext(self.originalBuffer, self.fileName, token.position, token.position + token.lexeme.len, self.interpreter.allocator);
}

fn unknownError(self: *const Resolver, comptime message: []const u8, params: anytype) !void {
    const formattedMessage = try std.fmt.allocPrint(self.interpreter.allocator, message, params);
    defer self.interpreter.allocator.free(formattedMessage);
    try prettyError(formattedMessage);
}

fn beginScope(self: *Resolver) !void {
    const newNode = try self.interpreter.allocator.create(Scopes.Node);
    // Append handles updating previous and next
    newNode.* = Scopes.Node{ .data = std.StringHashMapUnmanaged(bool){}, .prev = null, .next = null };
    self.scopes.append(newNode);
}

fn endScope(self: *Resolver) !void {
    var node = self.scopes.pop();
    if (node == null) {
        try self.unknownError("Tried to end a scope but no scope was found", .{});
        return ResolverError.Unknown;
    }

    var iter = node.?.data.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*;
        if (std.mem.eql(u8, key, "this") or std.mem.eql(u8, key, "super")) continue; // A special case is a bit hacky, but it's fine

        self.interpreter.allocator.free(key);
    }
    node.?.data.deinit(self.interpreter.allocator);
    self.interpreter.allocator.destroy(node.?);
}

fn declare(self: *Resolver, name: Token) anyerror!void {
    if (self.scopes.len == 0) return;
    const duplicatedName = try self.interpreter.allocator.dupe(u8, name.lexeme);
    const last = try self.scopes.last.?.data.fetchPut(self.interpreter.allocator, duplicatedName, false);
    if (last != null) {
        try self.errorOn(name, "Variable with this name already declared in this scope", .{});
    }
}

fn define(self: *Resolver, name: Token) anyerror!void {
    if (self.scopes.len == 0) return;
    const pointer = self.scopes.last.?.data.getPtr(name.lexeme);
    if (pointer == null) {
        try self.unknownError("Tried to define a variable that was not declared", .{});
        return ResolverError.Unknown;
    }
    pointer.?.* = true;
}

fn resolveLocal(self: *Resolver, expression: *const Expression, name: Token) anyerror!void {
    if (self.scopes.len == 0) return;
    var current: ?*Scopes.Node = self.scopes.last.?;
    const scopeCount = @as(u32, @intCast(self.scopes.len));
    var index = scopeCount - 1;
    while (current != null) {
        if (current.?.data.get(name.lexeme) != null) {
            try self.interpreter.resolve(expression.id, scopeCount - index - 1);
            return;
        }
        if (index == 0) break;

        current = current.?.prev;
        index -= 1;
    }
}

fn resolveStatement(self: *Resolver, statement: *const Statement, avoidDefiningBlockScope: bool) anyerror!void {
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
            if (!avoidDefiningBlockScope) try self.beginScope();
            for (values.statements.items) |childStatement| {
                try self.resolveStatement(childStatement, false);
            }
            if (!avoidDefiningBlockScope) try self.endScope();
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

fn resolveExpression(self: *Resolver, expression: *const Expression) anyerror!void {
    switch (expression.*.value) {
        .Grouping => |values| {
            try self.resolveExpression(values.expression);
        },
        .Literal => {
            // Do nothing
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
            try self.resolveFunction(values);
        },
        .FunctionCall => |values| {
            try self.resolveExpression(values.callee);
            for (values.arguments.items) |argument| {
                try self.resolveExpression(argument);
            }
        },

        .Class => |values| {
            try self.beginScope();
            try self.scopes.last.?.data.put(self.interpreter.allocator, "this", true);
            try self.scopes.last.?.data.put(self.interpreter.allocator, "super", true);

            for (values.methods.items) |method| {
                try self.resolveFunction(method.function);
            }

            try self.endScope();
        },
        .This => {},
        .Super => {},

        .VariableAccess => |values| {
            if (self.scopes.len > 0 and self.scopes.last.?.data.get(values.name.lexeme) == false) {
                try self.errorOn(values.name, "Cannot read local variable in its own initializer", .{});
            }

            try self.resolveLocal(expression, values.name);
        },
        .VariableAssignment => |values| {
            try self.resolveExpression(values.value);
            try self.resolveLocal(expression, values.name);
        },

        .PropertyAccess => |values| {
            try self.resolveExpression(values.object);
        },
        .PropertyAssignment => |values| {
            try self.resolveExpression(values.object);
            try self.resolveExpression(values.value);
        },
    }
}

fn resolveFunction(self: *Resolver, values: FunctionExpression) anyerror!void {
    try self.beginScope();
    for (values.parameters.items) |parameter| {
        try self.declare(parameter);
        try self.define(parameter);
    }
    try self.resolveStatement(values.body, true);
    try self.endScope();
}
