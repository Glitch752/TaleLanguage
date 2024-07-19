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

const RCSP = @import("../rcsp.zig");
fn ReferenceCountedVariable(comptime T: type) type {
    return RCSP.RcSharedPointer(T, RCSP.NonAtomic);
}

const RCSPCallable = ReferenceCountedVariable(CallableFunction);
const RCSPClassInstance = ReferenceCountedVariable(ClassInstance);

pub const VariableValue = union(enum) {
    Number: f64,
    String: struct {
        string: []const u8,
        allocated: bool,
    },
    Boolean: bool,

    Function: RCSPCallable,

    /// NOTE: This isn't a class instance, but a class type.
    ClassType: RCSPCallable,
    ClassInstance: RCSPClassInstance,

    WeakReference: union(enum) {
        Function: RCSPCallable.Weak,
        ClassType: RCSPCallable.Weak,
        ClassInstance: RCSPClassInstance.Weak,
    },

    Null,

    /// Deinitializes the value if it was allocated.
    /// Must be called before a string variable is discarded.
    pub fn deinit(self: *VariableValue, interpreter: *Interpreter) void {
        switch (self.*) {
            .String => |value| if (value.allocated) interpreter.allocator.free(value.string),
            .Function => self.Function.deinit(interpreter),
            .ClassType => self.ClassType.class.deinit(interpreter),
            .ClassInstance => self.ClassInstance.instance.unreference(interpreter),
            else => {},
        }
    }

    pub fn takeReference(self: VariableValue) VariableValue {
        switch (self) {
            .Function => |value| return .{ .Function = value.strongClone() },
            .ClassType => |value| return .{ .ClassType = value.strongClone() },
            .ClassInstance => |value| return .{ .ClassInstance = value.strongClone() },
            else => return self,
        }
    }

    /// Takes a weak reference to the inner value.
    pub fn takeWeakReference(self: VariableValue) VariableValue {
        switch (self) {
            .Function => |value| return .{ .WeakReference = .{ .Function = value.weakClone() } },
            .ClassType => |value| return .{ .WeakReference = .{ .ClassType = value.weakClone() } },
            .ClassInstance => |value| return .{ .WeakReference = .{ .ClassInstance = value.weakClone() } },
            else => return self,
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
    pub fn asCallable(self: VariableValue) RCSPCallable {
        switch (self) {
            .Function => |value| return value.strongClone(),
            .ClassType => |value| return value.strongClone(),
            .WeakReference => |value| {
                switch (value) {
                    .Function => |weak| return weak.strongClone() orelse std.debug.panic("Tried to access weakly-referenced variable that was already deallocated", .{}),
                    .ClassType => |weak| return weak.strongClone() orelse std.debug.panic("Tried to access weakly-referenced variable that was already deallocated", .{}),
                    else => std.debug.panic("Tried to access non-callable weakly-referenced variable as a callable", .{}),
                }
            },
            else => std.debug.panic("Tried to access non-callable variable as a callable", .{}),
        }
    }

    pub fn isClassType(self: VariableValue) bool {
        return self == .ClassType;
    }
    pub fn asClassType(self: VariableValue) *ClassType {
        return self.ClassType.strongClone().ptr().ClassType.class;
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

    pub fn nativeFunction(arity: u32, function: CallableNativeFunction, allocator: std.mem.Allocator) !VariableValue {
        return .{ .Function = try RCSPCallable.init(CallableFunction.native(arity, function), allocator) };
    }
    pub fn newFunction(function: FunctionExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !VariableValue {
        return .{ .Function = try RCSPCallable.init(try CallableFunction.user(function, activeEnvironment, allocator), allocator) };
    }

    pub fn newClassType(class: ClassExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !VariableValue {
        return .{ .ClassType = try RCSPCallable.init(try CallableFunction.classType(class, activeEnvironment, allocator), allocator) };
    }
    pub fn newClassInstance(classType: *ClassType, interpreter: *Interpreter, callToken: Token, arguments: std.ArrayList(VariableValue)) !VariableValue {
        return .{ .ClassInstance = try RCSPClassInstance.init(try ClassInstance.new(interpreter, classType, callToken, arguments), interpreter.allocator) };
    }

    pub fn classInstance(instance: *ClassInstance) VariableValue {
        instance.referenceCount += 1;
        return .{ .ClassInstance = .{ .instance = instance } };
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
            .Function => |value| return value.toString(allocator),
            .ClassType => |value| return value.class.ClassType.class.toString(allocator),
            .ClassInstance => |value| return value.instance.toString(allocator),
        }
    }
};
