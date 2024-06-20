const std = @import("std");

pub const NodeType = enum { Assignment, ForLoop, Range, Function, FunctionCall, Return, If, Literal, Block, Identifier, ArithmeticOperation, ComparisonOperation, BooleanOperation, Type };

pub const LiteralType = enum { IntLiteral, StringLiteral };

pub const AirthmeticOperationType = enum { Add, Subtract, Multiply, Divide, Modulus };
pub const ComparisonOperationType = enum { LessThan, LessThanEqual, GreaterThan, GreaterThanEqual, Equal, NotEqual };
pub const BooleanOperationType = enum { And, Or, Not };

pub const NodeData = union(NodeType) {
    Assignment: struct {
        identifier: []const u8,
        value: *NodeData,
        type: *NodeData,
    },
    ForLoop: struct {
        expression: *NodeData,
        block: *NodeData,
    },
    Range: struct {
        /// Inclusive
        startValue: *NodeData,
        /// Exclusive
        endValue: *NodeData,
    },
    Function: struct {
        parameters: []struct {
            identifier: []const u8,
            type: *NodeData,
        },
        block: *NodeData,
    },
    FunctionCall: struct {
        identifier: []const u8,
        arguments: []*NodeData,
    },
    Return: struct {
        value: *NodeData,
    },
    If: struct {
        condition: *NodeData,
        block: *NodeData,
        elseBlock: ?*NodeData,
    },
    Literal: union(LiteralType) {
        IntLiteral: u64,
        StringLiteral: []const u8,
    },
    Block: BlockNodeData,
    Identifier: struct {
        identifier: []const u8,
    },
    ArithmeticOperation: struct {
        operation: AirthmeticOperationType,
        left: *NodeData,
        right: *NodeData,
    },
    ComparisonOperation: struct {
        operation: ComparisonOperationType,
        left: *NodeData,
        right: *NodeData,
    },
    BooleanOperation: struct {
        operation: BooleanOperationType,
        left: *NodeData,
        right: *NodeData,
    },
    Type: struct {
        // TODO: Proper types
        identifier: []const u8,
    },

    pub fn print(self: NodeData, writer: anytype, indent: usize) !void {
        switch (self) {
            .Assignment => {
                try writer.print("{s} = ", .{self.Assignment.identifier});
                try self.Assignment.value.print(writer, indent);
            },
            .ForLoop => {
                try writer.print("for ", .{});
                try self.ForLoop.expression.print(writer, indent);
                try writer.print(" ", .{});
                try self.ForLoop.block.print(writer, indent);
            },
            .Range => {
                try self.Range.startValue.print(writer, indent);
                try writer.print("..<", .{});
                try self.Range.endValue.print(writer, indent);
            },
            .Function => {
                try writer.print("fn(", .{});
                for (self.Function.parameters) |parameter| {
                    try writer.print("{s}: ", .{parameter.identifier});
                    try parameter.type.print(writer, indent);
                    try writer.print(", ", .{});
                }
                try writer.print(") ", .{});
                try self.Function.block.print(writer, indent);
            },
            .FunctionCall => {
                try writer.print("{s}(", .{self.FunctionCall.identifier});
                for (self.FunctionCall.arguments) |argument| {
                    try argument.print(writer, indent);
                    try writer.print(", ", .{});
                }
                try writer.print(")", .{});
            },
            .Return => {
                try writer.print("return ", .{});
                try self.Return.value.print(writer, indent);
            },
            .If => {
                try writer.print("if ", .{});
                try self.If.condition.print(writer, indent);
                try writer.print(" ", .{});
                try self.If.block.print(writer, indent);
                if (self.If.elseBlock != null) {
                    try writer.print(" else ", .{});
                    try self.If.elseBlock.?.*.print(writer, indent);
                }
            },
            .Literal => {
                switch (self.Literal) {
                    .IntLiteral => try writer.print("{d}", .{self.Literal.IntLiteral}),
                    .StringLiteral => try writer.print("{s}", .{self.Literal.StringLiteral}),
                }
            },
            .Block => {
                try writer.print("{{", .{});
                for (self.Block.statements.*) |statement| {
                    try statement.print(writer, indent + 1);
                    try writer.print("\n", .{});
                }
                try writer.print("}}", .{});
            },
            .Identifier => try writer.print("{s}", .{self.Identifier.identifier}),
            .ArithmeticOperation => {
                try self.ArithmeticOperation.left.print(writer, indent);
                switch (self.ArithmeticOperation.operation) {
                    .Add => try writer.print(" + ", .{}),
                    .Subtract => try writer.print(" - ", .{}),
                    .Multiply => try writer.print(" * ", .{}),
                    .Divide => try writer.print(" / ", .{}),
                    .Modulus => try writer.print(" % ", .{}),
                }
                try self.ArithmeticOperation.right.print(writer, indent);
            },
            .ComparisonOperation => {
                try self.ComparisonOperation.left.print(writer, indent);
                switch (self.ComparisonOperation.operation) {
                    .LessThan => try writer.print(" < ", .{}),
                    .LessThanEqual => try writer.print(" <= ", .{}),
                    .GreaterThan => try writer.print(" > ", .{}),
                    .GreaterThanEqual => try writer.print(" >= ", .{}),
                    .Equal => try writer.print(" == ", .{}),
                    .NotEqual => try writer.print(" != ", .{}),
                }
                try self.ComparisonOperation.right.print(writer, indent);
            },
            .BooleanOperation => {
                try self.BooleanOperation.left.print(writer, indent);
                switch (self.BooleanOperation.operation) {
                    .And => try writer.print(" && ", .{}),
                    .Or => try writer.print(" || ", .{}),
                    .Not => try writer.print(" ! ", .{}),
                }
                try self.BooleanOperation.right.print(writer, indent);
            },
            .Type => try writer.print("{s}", .{self.Type.identifier}),
        }
    }

    pub fn deinit(self: NodeData, allocator: std.mem.Allocator) void {
        switch (self) {
            .Assignment => |node| {
                node.value.deinit(allocator);
                node.type.deinit(allocator);
            },
            .ForLoop => |node| {
                node.expression.deinit(allocator);
                node.block.deinit(allocator);
            },
            .Range => |node| {
                node.startValue.deinit(allocator);
                node.endValue.deinit(allocator);
            },
            .Function => |node| {
                for (node.parameters) |parameter| {
                    parameter.type.deinit(allocator);
                }
                node.block.deinit(allocator);
            },
            .FunctionCall => |node| {
                for (node.arguments) |argument| {
                    argument.deinit(allocator);
                }
            },
            .Return => |node| {
                node.value.deinit(allocator);
            },
            .If => |node| {
                node.condition.deinit(allocator);
                node.block.deinit(allocator);
                if (node.elseBlock != null) {
                    node.elseBlock.?.deinit(allocator);
                }
            },
            .Literal => |node| {
                // Nothing to deinit
                _ = node;
            },
            .Block => |node| {
                node.deinit(allocator);
            },
            .Identifier => |node| {
                // Nothing to deinit
                _ = node;
            },
            .ArithmeticOperation => |node| {
                node.left.deinit(allocator);
                node.right.deinit(allocator);
            },
            .ComparisonOperation => |node| {
                node.left.deinit(allocator);
                node.right.deinit(allocator);
            },
            .BooleanOperation => |node| {
                node.left.deinit(allocator);
                node.right.deinit(allocator);
            },
            .Type => |node| {
                allocator.free(node.identifier);
            },
        }
    }
};

fn repeat(s: []const u8, times: usize, allocator: std.mem.Allocator) ![]u8 {
    const repeated = try allocator.alloc(u8, s.len * times);

    var i: usize = 0;
    while (i < s.len * times) : (i += 1) {
        repeated[i] = s[i % 2];
    }

    return repeated;
}

pub const BlockNodeData = struct {
    statements: *[]NodeData,

    pub fn print(self: BlockNodeData, writer: anytype, indent: usize) !void {
        std.debug.print("{d}", .{self.statements.len});
        _ = indent;
        for (self.statements.*) |statement| {
            // const allocator = std.heap.page_allocator;
            // std.debug.print("{d}", .{indent});
            // const repeated = try repeat("    ", indent, allocator);
            // defer allocator.free(repeated);

            // try writer.print("{s}", .{repeated});
            // // try statement.print(writer, indent);
            _ = statement;
            // try writer.print("\n", .{});
        }
        _ = writer;
    }

    pub fn deinit(self: BlockNodeData, allocator: std.mem.Allocator) void {
        for (self.statements.*) |statement| {
            statement.deinit(allocator);
        }
    }
};

pub const Node = struct {
    node: NodeData,
    line: u32,
    column: u32,
};

pub const AST = struct {
    root: BlockNodeData,

    pub fn print(self: AST, writer: anytype, indent: usize) !void {
        try self.root.print(writer, indent);
    }

    pub fn deinit(self: AST, allocator: std.mem.Allocator) void {
        self.root.deinit(allocator);
    }
};
