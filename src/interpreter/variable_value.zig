const TokenLiteral = @import("../token.zig").TokenLiteral;

pub const VariableValue = union(enum) {
    Number: f64,
    String: []const u8,
    Boolean: bool,
    Null,

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
        return self.String;
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
};
