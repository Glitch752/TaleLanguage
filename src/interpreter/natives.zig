const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;
const Interpreter = @import("./interpreter.zig").Interpreter;

/// Arity: 1
pub fn print(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) VariableValue {
    _ = interpreter;

    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| std.debug.print("{d}", .{value}),
        .String => |value| std.debug.print("{s}", .{value.string}),
        .Boolean => |value| std.debug.print("{s}", .{if (value) "true" else "false"}),
        .Null => std.debug.print("null", .{}),
        else => {},
    }

    return .Null;
}
