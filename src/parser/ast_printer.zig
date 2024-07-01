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
    for (program.statements.items) |statement| {
        try self.printStatement(statement);
    }
}

pub fn printStatement(self: *const ASTPrinter, statement: *const Statement) !void {
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
    }
}

pub fn printExpression(self: *const ASTPrinter, expression: *const Expression) !void {
    switch (expression.*) {
        .Binary => |values| {
            std.debug.print("(", .{});
            try self.printExpression(values.left);
            std.debug.print(" ", .{});
            std.debug.print("{s}", .{values.operator.lexeme});
            std.debug.print(" ", .{});
            try self.printExpression(values.right);
            std.debug.print(")", .{});
        },
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
        .Unary => |values| {
            std.debug.print("(", .{});
            std.debug.print("{s}", .{values.operator.lexeme});
            try self.printExpression(values.right);
            std.debug.print(")", .{});
        },
        .VariableAccess => |values| {
            std.debug.print("{s}", .{values.name.lexeme});
        },
        .VariableAssignment => |values| {
            std.debug.print("{s}", .{values.name.lexeme});
            std.debug.print(" = ", .{});
            try self.printExpression(values.value);
        },
    }
}
