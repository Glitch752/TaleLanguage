const std = @import("std");

const ModuleInterpreter = @import("./module_interpreter.zig").ModuleInterpreter;
const TokenLiteral = @import("../token.zig").TokenLiteral;
const Token = @import("../token.zig").Token;
const Environment = @import("./environment.zig").Environment;

const Callable = @import("./callable_value.zig").Callable;
const CallableFunction = @import("./callable_value.zig").CallableFunction;
const CallableNativeFunction = @import("./callable_value.zig").CallableNativeFunction;

const FunctionExpression = @import("../parser/expression.zig").FunctionExpression;
const ClassExpression = @import("../parser/expression.zig").ClassExpression;

const ClassInstance = @import("./class_value.zig").ClassInstance;
const ClassInstanceReference = @import("./class_value.zig").ClassInstanceReference;
const ClassType = @import("./class_value.zig").ClassType;
const ClassTypeReference = @import("./class_value.zig").ClassTypeReference;

const ModuleReference = @import("./module_value.zig").ModuleReference;

const RCSP = @import("../rcsp.zig");

pub const VariableValue = union(enum) {
    Number: f64,
    String: struct {
        string: []const u8,
        allocated: bool,
    },
    Boolean: bool,

    Function: CallableFunction,

    /// NOTE: This isn't a class instance, but a class type.
    ClassType: CallableFunction,
    ClassInstance: ClassInstanceReference,

    WeakReference: union(enum) {
        ClassInstance: ClassInstanceReference.Weak,
        ClassType: ClassTypeReference.Weak,

        pub fn deinit(self: *@This()) void {
            switch (self.*) {
                .ClassInstance => _ = self.ClassInstance.deinit(),
                .ClassType => _ = self.ClassType.deinit(),
            }
        }

        pub fn toString(self: *const @This(), allocator: std.mem.Allocator) ![]const u8 {
            switch (self.*) {
                .ClassInstance => return std.fmt.allocPrint(allocator, "<class instance>", .{}),
                .ClassType => return std.fmt.allocPrint(allocator, "<class type>", .{}),
            }
        }
    },

    Module: ModuleReference,

    Null,

    /// Deinitializes the value, dropping the reference if necessary.
    pub fn deinit(self: *VariableValue, interpreter: *ModuleInterpreter) void {
        switch (self.*) {
            .String => |value| if (value.allocated) interpreter.allocator.free(value.string),
            .Function => _ = self.Function.deinit(interpreter),
            .ClassType => {
                std.debug.assert(self.ClassType.ClassType.strongCount() != 0);
                _ = self.ClassType.deinit(interpreter);
            },
            .ClassInstance => {
                std.debug.assert(self.ClassInstance.strongCount() != 0);
                _ = self.ClassInstance.deinit(interpreter);
            },
            .WeakReference => _ = self.WeakReference.deinit(),
            else => {},
        }
    }

    /// Clones the value, incrementing the reference count if necessary.
    pub fn takeReference(self: VariableValue, interpreter: *ModuleInterpreter) !VariableValue {
        switch (self) {
            .ClassInstance => |value| return .{ .ClassInstance = value.strongClone() },
            .ClassType => |value| return .{ .ClassType = value.takeReference() },
            .Function => |value| return .{ .Function = value.takeReference() },
            .String => |value| return .{ .String = .{ .string = try interpreter.allocator.dupe(u8, value.string), .allocated = true } },
            else => return self,
        }
    }

    /// Takes a weak reference to the inner value.
    pub fn takeWeakReference(self: VariableValue) VariableValue {
        switch (self) {
            .ClassInstance => |value| return .{ .WeakReference = .{ .ClassInstance = value.weakClone() } },
            .ClassType => |value| return .{ .WeakReference = .{ .ClassType = value.ClassType.weakClone() } },
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
    pub fn asCallable(self: VariableValue) CallableFunction {
        switch (self) {
            .Function => |value| return value,
            .ClassType => |value| return value,
            else => std.debug.panic("Tried to access non-callable variable as a callable", .{}),
        }
    }

    pub fn isClassType(self: VariableValue) bool {
        return self == .ClassType;
    }
    pub fn asClassType(self: VariableValue) ClassTypeReference {
        return self.ClassType.ClassType;
    }
    pub fn referenceClassType(self: VariableValue) ClassTypeReference {
        if (self == .ClassType) {
            return self.ClassType.ClassType.strongClone();
        } else if (self == .WeakReference) {
            std.debug.assert(self.WeakReference.ClassType.strongCount() != 0);
            return self.WeakReference.ClassType.strongClone().?;
        } else {
            std.debug.panic("Tried to access non-class type as a class type", .{});
        }
    }

    pub fn isClassInstance(self: VariableValue) bool {
        return self == .ClassInstance or (self == .WeakReference and self.WeakReference == .ClassInstance);
    }
    pub fn referenceClassInstance(self: VariableValue) ClassInstanceReference {
        if (self == .ClassInstance) {
            return self.ClassInstance.strongClone();
        } else if (self == .WeakReference) {
            std.debug.assert(self.WeakReference.ClassInstance.strongCount() != 0);
            return self.WeakReference.ClassInstance.strongClone().?;
        } else {
            std.debug.panic("Tried to access non-class instance as a class instance", .{});
        }
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

    pub fn nativeFunction(arity: u32, function: CallableNativeFunction) !VariableValue {
        return .{ .Function = CallableFunction.native(arity, function) };
    }
    pub fn newFunction(function: FunctionExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator) !VariableValue {
        return .{ .Function = try CallableFunction.user(function, activeEnvironment, allocator) };
    }
    pub fn newFunctionReference(function: CallableFunction) VariableValue {
        return .{ .Function = function };
    }

    pub fn newClassType(class: ClassExpression, activeEnvironment: *Environment, allocator: std.mem.Allocator, superClass: ?ClassTypeReference) !VariableValue {
        return .{ .ClassType = try CallableFunction.classType(class, activeEnvironment, allocator, superClass) };
    }
    pub fn newClassInstance(classType: ClassTypeReference, interpreter: *ModuleInterpreter, callToken: Token, arguments: std.ArrayList(VariableValue)) !VariableValue {
        return .{ .ClassInstance = try ClassInstance.new(interpreter, classType, callToken, arguments) };
    }

    pub fn classInstance(instance: *ClassInstance) VariableValue {
        instance.referenceCount += 1;
        return .{ .ClassInstance = .{ .instance = instance } };
    }

    pub fn weakClassInstanceReference(instance: ClassInstanceReference.Weak) VariableValue {
        return .{ .WeakReference = .{ .ClassInstance = instance } };
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
            .ClassType => |value| return value.ClassType.ptr().toString(allocator),
            .ClassInstance => |value| return value.ptr().toString(allocator),
            .WeakReference => |value| return value.toString(allocator),
            .Module => |module| return try std.fmt.allocPrint(allocator, "<module {s}>", .{module.ptr().path}),
        }
    }
};
