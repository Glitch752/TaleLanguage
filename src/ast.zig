const std = @import("std");

pub const NodeType = enum { Null, Assignment, ForLoop, Range, Function, FunctionCall, Return, If, Literal, Block, Identifier, ArithmeticOperation, ComparisonOperation, BooleanOperation, Type };

pub const LiteralType = enum { IntLiteral, StringLiteral };

pub const AirthmeticOperationType = enum { Add, Subtract, Multiply, Divide, Modulus };
pub const ComparisonOperationType = enum { LessThan, LessThanEqual, GreaterThan, GreaterThanEqual, Equal, NotEqual };
pub const BooleanOperationType = enum { And, Or, Not };

pub const FunctionParameter = struct {
    identifier: []const u8,
    type: *const AST,

    pub fn deinit(self: FunctionParameter, allocator: std.mem.Allocator) void {
        allocator.free(self.identifier);
        self.type.deinit(allocator);
        allocator.destroy(self.type);
    }

    pub fn write(self: FunctionParameter, writer: *const std.io.AnyWriter, indent: usize) anyerror!void {
        try writer.print("{s}: ", .{self.identifier});
        try self.type.print(writer, indent);
        try writer.print(", ", .{});
    }
};

pub const ASTNode = union(NodeType) {
    Null: void,
    Assignment: struct {
        identifier: []const u8,
        value: *const AST,
        type: *const AST,
    },
    ForLoop: struct {
        iteratorIdentifier: []const u8,
        listIdentifier: []const u8,
        block: *const AST,
    },
    Range: struct {
        /// Inclusive
        startValue: *const AST,
        /// Exclusive
        endValue: *const AST,
    },
    Function: struct {
        parameters: []FunctionParameter,
        block: *const AST,
    },
    FunctionCall: struct {
        identifier: []const u8,
        arguments: []*const AST,
    },
    Return: struct {
        value: *const AST,
    },
    If: struct {
        condition: *const AST,
        block: *const AST,
        elseBlock: ?*const AST,
    },
    Literal: union(LiteralType) {
        IntLiteral: u64,
        StringLiteral: []const u8,
    },
    Block: struct { statements: []*const AST },
    Identifier: struct {
        identifier: []const u8,
    },
    ArithmeticOperation: struct {
        operation: AirthmeticOperationType,
        left: *const AST,
        right: *const AST,
    },
    ComparisonOperation: struct {
        operation: ComparisonOperationType,
        left: *const AST,
        right: *const AST,
    },
    BooleanOperation: struct {
        operation: BooleanOperationType,
        left: *const AST,
        right: *const AST,
    },
    Type: struct {
        // TODO: Proper types
        identifier: []const u8,
    },
};

pub const AST = struct {
    line: u32,
    column: u32,

    node: ASTNode,

    // Set when node is created
    deinit: *const fn (self: *const AST, allocator: std.mem.Allocator) void,

    // Set when node is created
    print: *const fn (self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void,
};
