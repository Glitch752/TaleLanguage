const std = @import("std");
const Statement = @import("../parser/statement.zig").Statement;
const Expression = @import("../parser/expression.zig").Expression;

const Resolver = @This();

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Resolver {
    return .{
        .allocator = allocator,
    };
}

pub fn resolveProgram(self: *const Resolver, program: *const Program) !void {
    std.debug.print("Program:\n", .{});
    for (program.statements.items) |statement| {
        try self.resolveStatement(statement, 1);
    }
}

pub fn resolveStatement(self: *const Resolver, statement: *const Statement) anyerror!void {
    switch (statement.*) {
        .Expression => |values| {
            try self.resolveExpression(values.expression);
        },
        .Let => |values| {
            try self.resolveExpression(values.initializer);
        },
        .Block => |values| {
            for (values.statements.items) |childStatement| {
                try self.resolveStatement(childStatement);
            }
        },

        .If => |values| {
            try self.resolveExpression(values.condition);
            try self.resolveStatement(values.trueBranch);
            if (values.falseBranch != null) {
                try self.resolveStatement(values.falseBranch.?);
            }
        },
        .While => |values| {
            try self.resolveExpression(values.condition);
            try self.resolveStatement(values.body);
        },

        .Return => |values| {
            try self.resolveExpression(values.value);
        },
        .Break => {},
        .Continue => {},
    }
}

pub fn resolveExpression(self: *const Resolver, expression: *const Expression) anyerror!void {
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
            for (values.parameters.items, 0..) |parameter, index| {}
            try self.printStatement(values.body, 1);
        },
        .FunctionCall => |values| {
            try self.resolveExpression(values.callee);
            for (values.arguments.items) |argument| {
                try self.resolveExpression(argument);
            }
        },

        .VariableAccess => |values| {},
        .VariableAssignment => |values| {
            try self.resolveExpression(values.value);
        },
    }
}
