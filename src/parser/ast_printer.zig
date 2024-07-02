const Expression = @import("./expression.zig").Expression;
const Statement = @import("./statement.zig").Statement;
const Program = @import("./program.zig").Program;

const std = @import("std");

pub const ASTPrinter = @This();

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) ASTPrinter {
    return .{
        .allocator = allocator,
    };
}

pub fn printProgram(self: *const ASTPrinter, program: *const Program) !void {
    std.debug.print("Program:\n", .{});
    for (program.statements.items) |statement| {
        try self.printStatement(statement, 1);
    }
}

pub fn printStatement(self: *const ASTPrinter, statement: *const Statement, indent: u32) !void {
    var i: u32 = 0;
    while (i < indent) : (i += 1) {
        std.debug.print("  ", .{});
    }

    switch (statement.*) {
        .Expression => |values| {
            try self.printExpression(values.expression);
            std.debug.print(";", .{});
        },
        .Let => |values| {
            std.debug.print("let ", .{});
            std.debug.print("{s}", .{values.name.lexeme});
            std.debug.print(" = ", .{});
            try self.printExpression(values.initializer);
            std.debug.print(";", .{});
        },
        .Block => |values| {
            std.debug.print("{{", .{});
            for (values.statements.items) |childStatement| {
                try self.printStatement(childStatement, indent + 1);
            }
            std.debug.print("}}", .{});
        },

        .If => |values| {
            std.debug.print("if ", .{});
            try self.printExpression(values.condition);
            std.debug.print(" \n", .{});
            try self.printStatement(values.trueBranch, indent + 1);
            if (values.falseBranch != null) {
                std.debug.print(" else \n", .{});
                try self.printStatement(values.falseBranch.?, indent + 1);
            }
        },
        .While => |values| {
            std.debug.print("while ", .{});
            try self.printExpression(values.condition);
            std.debug.print(" \n", .{});
            try self.printStatement(values.body, indent + 1);
        },
    }
}

pub fn printExpression(self: *const ASTPrinter, expression: *const Expression) !void {
    switch (expression.*) {
        .Grouping => |values| {
            std.debug.print("[", .{});
            try self.printExpression(values.expression);
            std.debug.print("]", .{});
        },
        .Literal => |values| {
            const str = try values.value.toString(self.allocator);
            defer self.allocator.free(str);
            std.debug.print("{s}", .{str});
        },

        .Binary => |values| {
            std.debug.print("(", .{});
            try self.printExpression(values.left);
            std.debug.print(" {s} ", .{values.operator.lexeme});
            try self.printExpression(values.right);
            std.debug.print(")", .{});
        },
        .Unary => |values| {
            std.debug.print("(", .{});
            std.debug.print("{s}", .{values.operator.lexeme});
            try self.printExpression(values.right);
            std.debug.print(")", .{});
        },
        .Logical => |values| {
            std.debug.print("(", .{});
            try self.printExpression(values.left);
            std.debug.print(" {s} ", .{values.operator.lexeme});
            try self.printExpression(values.right);
            std.debug.print(")", .{});
        },
        .Bitwise => |values| {
            std.debug.print("(", .{});
            try self.printExpression(values.left);
            std.debug.print(" {s} ", .{values.operator.lexeme});
            try self.printExpression(values.right);
            std.debug.print(")", .{});
        },

        .FunctionCall => |values| {
            try self.printExpression(values.callee);
            std.debug.print("(", .{});
            for (values.arguments.items) |argument| {
                try self.printExpression(argument);
                if (argument != values.arguments.items[values.arguments.items.len - 1]) {
                    std.debug.print(", ", .{});
                }
            }
            std.debug.print(")", .{});
        },

        .VariableAccess => |values| {
            std.debug.print("{s}", .{values.name.lexeme});
        },
        .VariableAssignment => |values| {
            std.debug.print("{s} = ", .{values.name.lexeme});
            try self.printExpression(values.value);
        },
    }
}
