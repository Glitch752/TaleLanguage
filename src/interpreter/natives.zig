const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;
const Interpreter = @import("./interpreter.zig").Interpreter;

/// TODO: Payload
pub const NativeError = error{InvalidOperand};

/// NOTE: Having all these globals is not the end goal, but they're provided to allow testing out more complex programs.
/// Arity: 1
pub fn print(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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

/// Arity: 1
pub fn sin(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@sin(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn cos(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@cos(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn tan(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@tan(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn exp(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@exp(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn exp2(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@exp2(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn log(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@log(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn log2(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@log2(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn log10(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@log10(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn floor(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@floor(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}
