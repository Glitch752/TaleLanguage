const std = @import("std");

pub const NodeType = enum { Null, Assignment, ForLoop, Range, Function, FunctionCall, Return, If, Literal, Block, Identifier, ArithmeticOperation, ComparisonOperation, BooleanOperation, Type };

pub const LiteralType = enum { IntLiteral, StringLiteral };

pub const AirthmeticOperationType = enum { Add, Subtract, Multiply, Divide, Modulus };
pub const ComparisonOperationType = enum { LessThan, LessThanEqual, GreaterThan, GreaterThanEqual, Equal, NotEqual };
pub const BooleanOperationType = enum { And, Or, Not };

pub const FunctionParameter = struct {
    identifier: []const u8,
    type: *AST,

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
        value: *AST,
        type: *AST,
    },
    ForLoop: struct {
        iteratorIdentifier: []const u8,
        listIdentifier: []const u8,
        block: *AST,
    },
    Range: struct {
        /// Inclusive
        startValue: *AST,
        /// Exclusive
        endValue: *AST,
    },
    Function: struct {
        parameters: []FunctionParameter,
        block: *AST,
    },
    FunctionCall: struct {
        identifier: []const u8,
        arguments: []*AST,
    },
    Return: struct {
        value: *AST,
    },
    If: struct {
        condition: *AST,
        block: *AST,
        elseBlock: ?*AST,
    },
    Literal: union(LiteralType) {
        IntLiteral: u64,
        StringLiteral: []const u8,
    },
    Block: struct { statements: []*AST },
    Identifier: struct {
        identifier: []const u8,
    },
    ArithmeticOperation: struct {
        operation: AirthmeticOperationType,
        left: *AST,
        right: *AST,
    },
    ComparisonOperation: struct {
        operation: ComparisonOperationType,
        left: *AST,
        right: *AST,
    },
    BooleanOperation: struct {
        operation: BooleanOperationType,
        left: *AST,
        right: *AST,
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
    deinit: *fn (self: *AST, allocator: std.mem.Allocator) void,

    // Set when node is created
    print: *fn (self: AST, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void,
};
