const std = @import("std");
const VariableValue = @import("./variable_value.zig").VariableValue;
const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;

/// TODO: Payload
pub const NativeError = error{ InvalidOperand, Unknown, Panic };

/// NOTE: Having all these globals is not the end goal, but they're provided to allow testing out more complex programs.
/// Arity: 1
pub fn print(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
                .ClassInstance => std.debug.print("<weak class instance>", .{}),
                .ClassType => std.debug.print("<weak class type>", .{}),
            }
        },
        .Module => |module| std.debug.print("<module {s}>", .{module.getPath()}),
    }

    return .Null;
}

/// Arity: 1
pub fn println(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = try print(interpreter, arguments);
    std.debug.print("\n", .{});
    return .Null;
}

/// Arity: 1
pub fn toString(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    const argument = arguments.items[0];
    return VariableValue.fromString(VariableValue.toString(argument, interpreter.allocator) catch return NativeError.Unknown, true);
}

/// Arity: 1
pub fn sin(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn cos(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn tan(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn asin(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(std.math.asin(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn acos(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(std.math.acos(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn atan(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(std.math.atan(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn exp(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn exp2(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn log(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn log2(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn log10(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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
pub fn floor(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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

/// Arity: 1
pub fn ceil(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@ceil(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn round(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@round(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn abs(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@abs(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 1
pub fn pow(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const base = arguments.items[0];
    const exponent = arguments.items[1];

    if (base != .Number or exponent != .Number) {
        return NativeError.InvalidOperand;
    }

    return VariableValue.fromNumber(std.math.pow(f64, base.asNumber(), exponent.asNumber()));
}

/// Arity: 1
pub fn sqrt(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const argument = arguments.items[0];

    switch (argument) {
        .Number => |value| {
            return VariableValue.fromNumber(@sqrt(value));
        },
        else => {},
    }

    return NativeError.InvalidOperand;
}

/// Arity: 2
pub fn min(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const first = arguments.items[0];
    const second = arguments.items[1];

    if (first != .Number or second != .Number) {
        return NativeError.InvalidOperand;
    }

    return VariableValue.fromNumber(@min(first.asNumber(), second.asNumber()));
}

/// Arity: 2
pub fn max(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const first = arguments.items[0];
    const second = arguments.items[1];

    if (first != .Number or second != .Number) {
        return NativeError.InvalidOperand;
    }

    return VariableValue.fromNumber(@max(first.asNumber(), second.asNumber()));
}

/// Arity: 3
pub fn substring(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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

/// Arity: 1
pub fn intChar(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
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

/// Arity: 1
pub fn charInt(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    const number = arguments.items[0];

    if (number != .Number) {
        return NativeError.InvalidOperand;
    }

    const numberValue = number.asNumber();

    if (numberValue < 0 or numberValue > 255) {
        return NativeError.InvalidOperand;
    }

    const stringValue = interpreter.allocator.dupe(u8, &[_:1]u8{@intFromFloat(numberValue)}) catch return NativeError.Unknown;
    return VariableValue.fromString(stringValue, true);
}

/// Arity: 1
pub fn length(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    const string = arguments.items[0];

    if (string != .String) {
        return NativeError.InvalidOperand;
    }

    const stringBytes = string.asString();

    _ = interpreter;

    return VariableValue.fromNumber(@floatFromInt(@as(u32, @intCast(stringBytes.len))));
}

/// Arity: 0
pub fn clock(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = arguments;
    _ = interpreter;

    const time = std.time.milliTimestamp();
    return VariableValue.fromNumber(@floatFromInt(time));
}

/// Arity: 1
pub fn panic(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const message = arguments.items[0];

    if (message != .String) {
        return NativeError.InvalidOperand;
    }

    std.debug.print("Program panicked: {s}\n", .{message.String.string});
    return NativeError.Panic;
}

/// Arity: 1
pub fn assert(interpreter: *ModuleInterpreter, arguments: std.ArrayList(VariableValue)) NativeError!VariableValue {
    _ = interpreter;
    const condition = arguments.items[0];

    if (condition != .Boolean) {
        return NativeError.InvalidOperand;
    }

    if (!condition.Boolean) {
        std.debug.print("Program panicked: assertion did not hold true!\n", .{});
        return NativeError.Panic;
    }

    return .Null;
}
