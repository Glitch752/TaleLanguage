const std = @import("std");

const TokenLiteral = @import("../token.zig").TokenLiteral;

pub const VariableValue = union(enum) {
    Number: f64,
    String: struct {
        string: []const u8,
        allocated: bool,
    },
    Boolean: bool,
    Null,

    /// Deinitializes the value if it was allocated.
    /// Must be called before a string variable is discarded.
    pub fn deinit(self: VariableValue) []const u8 {
        if (self.String.allocated) {
            return string;
        } else {
            return self.String;
        }
    }

    // Accessors and type checks

    pub fn isNumber(self: VariableValue) bool {
        return self == .Number;
    }
    pub fn asNumber(self: VariableValue) f64 {
        return self.Number;
    }

    pub fn isString(self: VariableValue) bool {
        return self == .String;
    }
    pub fn asString(self: VariableValue) []const u8 {
        return self.String.string;
    }

    pub fn isBoolean(self: VariableValue) bool {
        return self == .Boolean;
    }
    pub fn asBoolean(self: VariableValue) bool {
        return self.Boolean;
    }

    pub fn isNull(self: VariableValue) bool {
        return self == .Null;
    }

    // Type coercion
    pub fn isTruthy(self: VariableValue) bool {
        switch (self) {
            .Number => return self.Number != 0,
            .String => return self.String.len != 0,
            .Boolean => return self.Boolean,
            .Null => return false,
        }
    }

    // Constructors

    pub fn fromNumber(number: f64) VariableValue {
        return .Number(number);
    }

    pub fn fromString(string: []const u8) VariableValue {
        return .String(string);
    }

    pub fn fromBoolean(boolean: bool) VariableValue {
        return .Boolean(boolean);
    }

    pub fn fromLiteral(literal: TokenLiteral) VariableValue {
        switch (literal) {
            .NumberLiteral => |value| return .Number(value),
            .StringLiteral => |value| return .String(value),
            .Identifier => |value| return .String(value), // Identifiers don't exist yet
            .True => return .Boolean(true),
            .False => return .Boolean(false),
            .Null => return .Null,
        }
    }

    pub fn @"null"() VariableValue {
        return .Null;
    }

    // Debugging and visualization
    /// Converts the value to a string for debugging and visualization purposes.
    /// A new string is always allocated, so the caller is responsible for freeing it.
    pub fn toString(self: VariableValue, allocator: std.mem.Allocator) []const u8 {
        switch (self) {
            .Number => |value| return try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .String => |value| return std.fmt.allocPrint("{s}", .{value}),
            .Boolean => return if (self.Boolean) std.fmt.allocPrint("true", .{}) else std.fmt.allocPrint("false", .{}),
            .Null => return std.fmt.allocPrint("null", .{}),
        }
    }
};
