const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;
const Interpreter = @import("./interpreter.zig").Interpreter;

/// TODO: Payload
pub const NativeError = error{ InvalidOperand, Unknown };

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
        .ClassType => std.debug.print("<class>", .{}),
        .Function => std.debug.print("<function>", .{}),
        .ClassInstance => std.debug.print("<instance>", .{}),
        .WeakReference => |value| {
            switch (value) {
                .Function => std.debug.print("<weak function>", .{}),
                .ClassType => std.debug.print("<weak class>", .{}),
                else => std.debug.print("<weak>", .{}),
            }
        },
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

// Arity: 3
pub fn substring(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    const string = arguments.items[0];
    const start = arguments.items[1];
    const end = arguments.items[2];

    if (string != .String or start != .Number or end != .Number) {
        return NativeError.InvalidOperand;
    }

    const stringBytes = string.asString();
    const startNumber: u32 = @intFromFloat(start.asNumber());
    const endNumber: u32 = @intFromFloat(end.asNumber());

    if (startNumber >= stringBytes.len or endNumber > stringBytes.len or startNumber > endNumber) {
        return NativeError.InvalidOperand;
    }

    const newString = interpreter.allocator.dupe(u8, stringBytes[startNumber..endNumber]) catch return NativeError.Unknown;
    return VariableValue.fromString(newString, true);
}

// Arity: 1
pub fn intChar(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    const string = arguments.items[0];

    if (string != .String) {
        return NativeError.InvalidOperand;
    }

    const stringBytes = string.asString();

    if (stringBytes.len != 1) {
        return NativeError.InvalidOperand;
    }

    _ = interpreter;

    return VariableValue.fromNumber(@floatFromInt(@as(u32, @intCast(stringBytes[0]))));
}

// Arity: 1
pub fn length(interpreter: *Interpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    const string = arguments.items[0];

    if (string != .String) {
        return NativeError.InvalidOperand;
    }

    const stringBytes = string.asString();

    _ = interpreter;

    return VariableValue.fromNumber(@floatFromInt(@as(u32, @intCast(stringBytes.len))));
}
