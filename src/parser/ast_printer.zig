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

pub fn printStatement(self: *const ASTPrinter, statement: *const Statement, indent: u32) anyerror!void {
    switch (statement.*) {
        .Expression => |values| {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            try self.printExpression(values.expression, indent);
            std.debug.print(";\n", .{});
        },
        .Let => |values| {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("let ", .{});
            std.debug.print("{s}", .{values.name.lexeme});
            std.debug.print(" = ", .{});
            try self.printExpression(values.initializer, indent);
            std.debug.print(";\n", .{});
        },
        .Block => |values| {
            std.debug.print("{{\n", .{});
            for (values.statements.items) |childStatement| {
                try self.printStatement(childStatement, indent + 1);
            }
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("}}", .{});
        },

        .If => |values| {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("if ", .{});
            try self.printExpression(values.condition, indent);
            std.debug.print(" ", .{});
            try self.printStatement(values.trueBranch, indent);
            if (values.falseBranch != null) {
                std.debug.print(" else ", .{});
                try self.printStatement(values.falseBranch.?, indent);
            }
            std.debug.print(" \n", .{});
        },
        .While => |values| {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("while ", .{});
            try self.printExpression(values.condition, indent);
            std.debug.print(" ", .{});
            try self.printStatement(values.body, indent);
            std.debug.print(" \n", .{});
        },

        .Return => |values| {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("return ", .{});
            try self.printExpression(values.value, indent);
            std.debug.print(";\n", .{});
        },
        .Break => {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("break;\n", .{});
        },
        .Continue => {
            for (0..indent) |_| {
                std.debug.print("  ", .{});
            }
            std.debug.print("continue;\n", .{});
        },
    }
}

const escape_map = std.StaticStringMap([]const u8).initComptime(.{
    .{ "\n", "\\n" },     .{ "\r", "\\r" },     .{ "\t", "\\t" },     .{ "\"", "\\\"" },    .{ "\\", "\\\\" },
    .{ "\x00", "\\0" },   .{ "\x01", "\\x01" }, .{ "\x02", "\\x02" }, .{ "\x03", "\\x03" }, .{ "\x04", "\\x04" },
    .{ "\x05", "\\x05" }, .{ "\x06", "\\x06" }, .{ "\x07", "\\x07" }, .{ "\x08", "\\x08" }, .{ "\x09", "\\x09" },
    .{ "\x0a", "\\x0a" }, .{ "\x0b", "\\x0b" }, .{ "\x0c", "\\x0c" }, .{ "\x0d", "\\x0d" }, .{ "\x0e", "\\x0e" },
    .{ "\x0f", "\\x0f" }, .{ "\x10", "\\x10" }, .{ "\x11", "\\x11" }, .{ "\x12", "\\x12" }, .{ "\x13", "\\x13" },
    .{ "\x14", "\\x14" }, .{ "\x15", "\\x15" }, .{ "\x16", "\\x16" }, .{ "\x17", "\\x17" }, .{ "\x18", "\\x18" },
    .{ "\x19", "\\x19" }, .{ "\x1a", "\\x1a" }, .{ "\x1b", "\\x1b" }, .{ "\x1c", "\\x1c" }, .{ "\x1d", "\\x1d" },
    .{ "\x1e", "\\x1e" }, .{ "\x1f", "\\x1f" },
});

fn escape_string(allocator: std.mem.Allocator, string: []const u8) ![]const u8 {
    var escaped = try allocator.alloc(u8, string.len * 2);
    var escaped_index: usize = 0;
    for (string) |char| {
        if (escaped_index >= escaped.len) {
            const new_size = escaped.len * 2;
            const new_escaped = try allocator.realloc(escaped, new_size);
            escaped = new_escaped;
        }

        const escape = escape_map.get(&[1]u8{char});
        if (escape != null) {
            for (escape.?) |escapeChar| {
                escaped[escaped_index] = escapeChar;
                escaped_index += 1;
            }
        } else {
            escaped[escaped_index] = char;
            escaped_index += 1;
        }
    }

    escaped = try allocator.realloc(escaped, escaped_index);
    return escaped;
}

pub fn printExpression(self: *const ASTPrinter, expression: *const Expression, indent: u32) anyerror!void {
    switch (expression.*.value) {
        .Grouping => |values| {
            std.debug.print("[", .{});
            try self.printExpression(values.expression, indent);
            std.debug.print("]", .{});
        },
        .Literal => |values| {
            switch (values.value) {
                .NumberLiteral => |num| std.debug.print("{d}", .{num}),
                .StringLiteral => |str| {
                    const escaped = try escape_string(self.allocator, str);
                    defer self.allocator.free(escaped);
                    std.debug.print("\"{s}\"", .{escaped});
                },
                .True => std.debug.print("true", .{}),
                .False => std.debug.print("false", .{}),
                .None => std.debug.print("none", .{}),
                .Null => std.debug.print("null", .{}),
                .Identifier => |lexeme| std.debug.print("{s}", .{lexeme}),
            }
        },

        .Binary => |values| {
            std.debug.print("(", .{});
            try self.printExpression(values.left, indent);
            std.debug.print(" {s} ", .{values.operator.lexeme});
            try self.printExpression(values.right, indent);
            std.debug.print(")", .{});
        },
        .Unary => |values| {
            std.debug.print("(", .{});
            std.debug.print("{s}", .{values.operator.lexeme});
            try self.printExpression(values.right, indent);
            std.debug.print(")", .{});
        },
        .Logical => |values| {
            std.debug.print("(", .{});
            try self.printExpression(values.left, indent);
            std.debug.print(" {s} ", .{values.operator.lexeme});
            try self.printExpression(values.right, indent);
            std.debug.print(")", .{});
        },
        .Bitwise => |values| {
            std.debug.print("(", .{});
            try self.printExpression(values.left, indent);
            std.debug.print(" {s} ", .{values.operator.lexeme});
            try self.printExpression(values.right, indent);
            std.debug.print(")", .{});
        },

        .Function => |values| {
            std.debug.print("function(", .{});
            for (values.parameters.items, 0..) |parameter, index| {
                std.debug.print("{s}", .{parameter.lexeme});
                if (index != values.parameters.items.len - 1) {
                    std.debug.print(", ", .{});
                }
            }
            std.debug.print(") ", .{});
            try self.printStatement(values.body, indent);
        },
        .FunctionCall => |values| {
            try self.printExpression(values.callee, indent);
            std.debug.print("(", .{});
            for (values.arguments.items) |argument| {
                try self.printExpression(argument, indent);
                if (argument != values.arguments.items[values.arguments.items.len - 1]) {
                    std.debug.print(", ", .{});
                }
            }
            std.debug.print(")", .{});
        },

        .Class => |values| {
            std.debug.print("class {{", .{});
            for (values.methods.items) |method| {
                if (method.static) {
                    std.debug.print("static {s}(", .{method.name.lexeme});
                } else {
                    std.debug.print("{s}(", .{method.name.lexeme});
                }
                for (method.function.parameters.items, 0..) |parameter, index| {
                    std.debug.print("{s}", .{parameter.lexeme});
                    if (index != method.function.parameters.items.len - 1) {
                        std.debug.print(", ", .{});
                    }
                }
                std.debug.print(") ", .{});
                try self.printStatement(method.function.body, indent);
            }
            std.debug.print("}}", .{});
        },
        .This => std.debug.print("this", .{}),
        .Super => std.debug.print("super", .{}),

        .VariableAccess => |values| {
            std.debug.print("{s}", .{values.name.lexeme});
        },
        .VariableAssignment => |values| {
            std.debug.print("{s} = ", .{values.name.lexeme});
            try self.printExpression(values.value, indent);
        },

        .PropertyAccess => |values| {
            try self.printExpression(values.object, indent);
            std.debug.print(".{s}", .{values.name.lexeme});
        },
        .PropertyAssignment => |values| {
            try self.printExpression(values.object, indent);
            std.debug.print(".{s} = ", .{values.name.lexeme});
            try self.printExpression(values.value, indent);
        },

        .DynamicPropertyAccess => |values| {
            try self.printExpression(values.object, indent);
            std.debug.print("[", .{});
            try self.printExpression(values.name, indent);
            std.debug.print("]", .{});
        },
        .DynamicPropertyAssignment => |values| {
            try self.printExpression(values.object, indent);
            std.debug.print("[", .{});
            try self.printExpression(values.name, indent);
            std.debug.print("] = ", .{});
            try self.printExpression(values.value, indent);
        },
    }
}
