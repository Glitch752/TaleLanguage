const std = @import("std");

const Expression = @import("../parser/expression.zig").Expression;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const VariableValue = @import("./variable_value.zig").VariableValue;

pub const Interpreter = @This();

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) Interpreter {
    return .{
        .allocator = allocator,
    };
}

pub fn deinit(self: *Interpreter) void {
    // No cleanup needed for now
    _ = self;
}

pub fn interpret(self: *Interpreter, expression: *const Expression) !VariableValue {
    switch (expression.*) {
        .Binary => {},
        .Grouping => {
            return self.interpret(expression.expression);
        },
        .Literal => |values| {
            return VariableValue.fromLiteral(values.value);
        },
        .Unary => |values| {
            switch (values.operator.type) {
                .Minus => {
                    const right = try self.interpret(values.right);
                    if (right.isNumber()) {
                        return VariableValue.fromNumber(-right.asNumber());
                    }
                    return VariableValue.null();
                },
                .Negate => {
                    const right = try self.interpret(values.right);
                    if (right.isBoolean()) {
                        return VariableValue.fromBoolean(!right.isTruthy());
                    }
                    return VariableValue.null();
                },
                else => {
                    return VariableValue.null();
                },
            }
        },
    }
}
