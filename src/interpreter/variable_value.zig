const std = @import("std");

const Interpreter = @import("./interpreter.zig").Interpreter;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const Token = @import("../token.zig").Token;
const Environment = @import("./environment.zig").Environment;

const Callable = @import("./callable_value.zig").Callable;
const CallableFunction = @import("./callable_value.zig").CallableFunction;
const CallableNativeFunction = @import("./callable_value.zig").CallableNativeFunction;

const FunctionExpression = @import("../parser/expression.zig").FunctionExpression;
const ClassExpression = @import("../parser/expression.zig").ClassExpression;

const ClassInstance = @import("./class_value.zig").ClassInstance;
const ClassType = @import("./class_value.zig").ClassType;

pub const VariableValue = union(enum) {
    Number: f64,
    String: struct {
        string: []const u8,
        allocated: bool,
    },
    Boolean: bool,

    Function: struct {
        arity: u32,
        function: CallableFunction,
    },

    /// NOTE: This isn't a class instance, but a class type.
    ClassType: struct { class: CallableFunction },
    ClassInstance: struct { instance: *ClassInstance },

    Null,

    /// Deinitializes the value if it was allocated.
    /// Must be called before a string variable is discarded.
    pub fn deinit(self: *VariableValue, interpreter: *Interpreter) void {
        switch (self.*) {
            .String => |value| if (value.allocated) interpreter.allocator.free(value.string),
            .Function => self.Function.function.deinit(interpreter),
            .ClassType => self.ClassType.class.deinit(interpreter),
            .ClassInstance => self.ClassInstance.instance.unreference(interpreter),
            else => {},
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

    pub fn isCallable(self: VariableValue) bool {
        return self == .Function or self == .ClassType;
    }
    pub fn asCallable(self: VariableValue) Callable {
        switch (self) {
            .Function => |value| return .{
                .arity = value.arity,
                .function = value.function,
            },
            .ClassType => |value| return .{
                .arity = value.class.ClassType.class.getArity(),
                .function = value.class,
            },
            else => std.debug.panic("Tried to get a callable from a non-callable value.", .{}),
        }
        return;
    }

    pub fn isClassType(self: VariableValue) bool {
        return self == .ClassType;
    }
    pub fn asClassType(self: VariableValue) *ClassType {
        return self.ClassType.class.ClassType.class;
    }

    pub fn isClassInstance(self: VariableValue) bool {
        return self == .ClassInstance;
    }
    pub fn asClassInstance(self: VariableValue) *ClassInstance {
        return self.ClassInstance.instance;
    }

    // Type coercion
    pub fn isTruthy(self: VariableValue) bool {
        switch (self) {
            .Number => |value| return value != 0,
            .String => |value| return value.string.len != 0,
            .Boolean => |value| return value,
            else => return false,
        }
    }

    pub fn isEqual(self: VariableValue, other: VariableValue) bool {
        switch (self) {
            .Number => |value| return other.isNumber() and other.asNumber() == value,
            .String => |value| return other.isString() and std.mem.eql(u8, other.asString(), value.string),
            .Boolean => |value| return other.isBoolean() and other.asBoolean() == value,
            .Null => return other.isNull(),
            else => return false,
        }
    }

    // Constructors

    pub fn fromNumber(number: f64) VariableValue {
        return .{ .Number = number };
    }

    pub fn fromString(string: []const u8, allocated: bool) VariableValue {
        return .{ .String = .{ .string = string, .allocated = allocated } };
    }

    pub fn fromBoolean(boolean: bool) VariableValue {
        return .{ .Boolean = boolean };
    }

    pub fn fromLiteral(literal: TokenLiteral) VariableValue {
        switch (literal) {
            .NumberLiteral => |value| return .{ .Number = value },
            .StringLiteral => |value| return .{ .String = .{ .string = value, .allocated = false } },
            .Identifier => |value| return .{ .String = .{ .string = value, .allocated = false } }, // Identifiers don't exist yet
            .True => return .{ .Boolean = true },
            .False => return .{ .Boolean = true },
            else => return .{ .Null = {} },
        }
    }

    pub fn nativeFunction(arity: u32, function: CallableNativeFunction) VariableValue {
        return .{ .Function = .{ .arity = arity, .function = CallableFunction.native(function) } };
    }
    pub fn newFunction(function: FunctionExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !VariableValue {
        return .{ .Function = .{ .arity = @intCast(function.parameters.items.len), .function = try CallableFunction.user(function, activeEnvironment, allocator) } };
    }

    pub fn newClassType(class: ClassExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !VariableValue {
        return .{ .ClassType = .{ .class = try CallableFunction.classType(class, activeEnvironment, allocator) } };
    }
    pub fn newClassInstance(classType: *ClassType, allocator: std.mem.Allocator) !VariableValue {
        return .{ .ClassInstance = .{ .instance = try ClassInstance.new(allocator, classType) } };
    }

    pub fn @"null"() VariableValue {
        return .Null;
    }

    // Debugging and visualization
    /// Converts the value to a string for debugging and visualization purposes.
    /// A new string is always allocated, so the caller is responsible for freeing it.
    pub fn toString(self: VariableValue, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .Number => |value| return try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .String => |value| return std.fmt.allocPrint(allocator, "{s}", .{value.string}),
            .Boolean => return if (self.Boolean) std.fmt.allocPrint(allocator, "true", .{}) else std.fmt.allocPrint(allocator, "false", .{}),
            .Null => return std.fmt.allocPrint(allocator, "null", .{}),
            .Function => |value| return value.function.toString(allocator),
            .ClassType => |value| return value.class.ClassType.class.toString(allocator),
            .ClassInstance => |value| return value.instance.toString(allocator),
        }
    }
};

/// Essentially a wrapper around VariableValue.
pub const ExpressionInterpretResult = struct {
    value: VariableValue,
    /// A value is an immediate value if it requires
    /// deinitialization after it is no longer needed.
    /// This doesn't happen, for example, on variable
    /// access. This probably isn't the best way to
    /// structure this, but it works for now.
    immediateValue: bool,

    pub fn deinit(self: *ExpressionInterpretResult, interpreter: *Interpreter) void {
        if (self.immediateValue) {
            self.value.deinit(interpreter);
        }
    }

    pub fn isNumber(self: *const ExpressionInterpretResult) bool {
        return self.value.isNumber();
    }
    pub fn asNumber(self: *const ExpressionInterpretResult) f64 {
        return self.value.asNumber();
    }

    pub fn isString(self: *const ExpressionInterpretResult) bool {
        return self.value.isString();
    }
    pub fn asString(self: *const ExpressionInterpretResult) []const u8 {
        return self.value.asString();
    }

    pub fn isBoolean(self: *const ExpressionInterpretResult) bool {
        return self.value.isBoolean();
    }
    pub fn asBoolean(self: *const ExpressionInterpretResult) bool {
        return self.value.asBoolean();
    }

    pub fn isCallable(self: *const ExpressionInterpretResult) bool {
        return self.value.isCallable();
    }
    pub fn asCallable(self: *const ExpressionInterpretResult) Callable {
        return self.value.asCallable();
    }

    pub fn isClassType(self: *const ExpressionInterpretResult) bool {
        return self.value.isClassType();
    }
    pub fn asClassType(self: *const ExpressionInterpretResult) *ClassType {
        return self.value.asClassType();
    }

    pub fn isClassInstance(self: *const ExpressionInterpretResult) bool {
        return self.value.isClassInstance();
    }
    pub fn asClassInstance(self: *const ExpressionInterpretResult) *ClassInstance {
        return self.value.asClassInstance();
    }

    pub fn isTruthy(self: *const ExpressionInterpretResult) bool {
        return self.value.isTruthy();
    }

    pub fn fromImmediateValue(value: VariableValue) ExpressionInterpretResult {
        return ExpressionInterpretResult{ .value = value, .immediateValue = true };
    }
    pub fn fromNonImmediateValue(value: VariableValue) ExpressionInterpretResult {
        return ExpressionInterpretResult{ .value = value, .immediateValue = false };
    }

    pub fn fromLiteral(value: TokenLiteral) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromLiteral(value));
    }
    pub fn fromNumber(value: f64) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromNumber(value));
    }
    pub fn fromString(value: []const u8, allocated: bool) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromString(value, allocated));
    }
    pub fn fromBoolean(value: bool) ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.fromBoolean(value));
    }
    pub fn newFunction(value: FunctionExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !ExpressionInterpretResult {
        return ExpressionInterpretResult.fromNonImmediateValue(try VariableValue.newFunction(value, activeEnvironment, allocator));
    }
    pub fn newClassType(value: ClassExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !ExpressionInterpretResult {
        return ExpressionInterpretResult.fromNonImmediateValue(try VariableValue.newClassType(value, activeEnvironment, allocator));
    }
    pub fn newClassInstance(value: *ClassInstance, allocator: std.mem.Allocator) !ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(try VariableValue.newClassInstance(value, allocator));
    }
    pub fn @"null"() ExpressionInterpretResult {
        return ExpressionInterpretResult.fromImmediateValue(VariableValue.null());
    }

    pub fn isEqual(self: ExpressionInterpretResult, other: ExpressionInterpretResult) bool {
        return self.value.isEqual(other.value);
    }
};
