const std = @import("std");

pub const NodeType = enum { Null, Assignment, ForLoop, Range, Function, FunctionCall, Return, If, Literal, Block, Identifier, ArithmeticOperation, ComparisonOperation, BooleanOperation, Type };

pub const LiteralType = enum { IntLiteral, StringLiteral };

pub const AirthmeticOperationType = enum { Add, Subtract, Multiply, Divide, Modulus };
pub const ComparisonOperationType = enum { LessThan, LessThanEqual, GreaterThan, GreaterThanEqual, Equal, NotEqual };
pub const BooleanOperationType = enum { And, Or, Not };

pub const FunctionParameter = struct {
    identifier: []const u8,
    type: *NodeData,

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

pub const NodeData = union(NodeType) {
    Null: void,
    Assignment: struct {
        identifier: []const u8,
        value: *NodeData,
        type: *NodeData,
    },
    ForLoop: struct {
        iteratorIdentifier: []const u8,
        listIdentifier: []const u8,
        block: *NodeData,
    },
    Range: struct {
        /// Inclusive
        startValue: *NodeData,
        /// Exclusive
        endValue: *NodeData,
    },
    Function: struct {
        parameters: []FunctionParameter,
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
    Block: struct { statements: []NodeData },
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

    pub fn print(self: NodeData, writer: *const std.io.AnyWriter, indent: usize) !void {
        const allocator = std.heap.page_allocator;
        const indentSpaces = try repeat("    ", indent, allocator);
        defer allocator.free(indentSpaces);

        switch (self) {
            .Null => |node| {
                _ = node;
            },
            .Assignment => |node| {
                try writer.print("{s}let {s}: ", .{ indentSpaces, node.identifier });
                try node.type.print(writer, indent);
                try writer.print(" = ", .{});
                try node.value.print(writer, indent);
                try writer.print(";", .{});
            },
            .ForLoop => |node| {
                try writer.print("for({s} : {s}) ", .{ node.iteratorIdentifier, node.listIdentifier });
                try node.block.print(writer, indent);
            },
            .Range => |node| {
                try node.startValue.print(writer, indent);
                try writer.print("..<", .{});
                try node.endValue.print(writer, indent);
            },
            .Function => |node| {
                try writer.print("function(", .{});
                for (node.parameters) |parameter| {
                    try parameter.write(writer, indent);
                }
                try writer.print(") ", .{});
                try node.block.print(writer, indent);
            },
            .FunctionCall => |node| {
                try writer.print("{s}(", .{node.identifier});
                for (node.arguments) |argument| {
                    try argument.print(writer, indent);
                    try writer.print(", ", .{});
                }
                try writer.print(")", .{});
            },
            .Return => |node| {
                try writer.print("{s}return ", .{indentSpaces});
                try node.value.print(writer, indent);
                try writer.print(";", .{});
            },
            .If => |node| {
                try writer.print("if ", .{});
                try node.condition.print(writer, indent);
                try writer.print(" ", .{});
                try node.block.print(writer, indent);
                if (node.elseBlock != null) {
                    try writer.print(" else ", .{});
                    try node.elseBlock.?.*.print(writer, indent);
                }
            },
            .Literal => |node| {
                switch (node) {
                    .IntLiteral => |val| try writer.print("{d}", .{val}),
                    .StringLiteral => |val| try writer.print("{s}", .{val}),
                }
            },
            .Block => |node| {
                try writer.print("{{\n", .{});
                for (node.statements) |statement| {
                    try writer.print("{s}", .{indentSpaces});
                    try statement.print(writer, indent + 1);
                    try writer.print("\n", .{});
                }
                try writer.print("{s}}}", .{indentSpaces});
            },
            .Identifier => |node| try writer.print("{s}", .{node.identifier}),
            .ArithmeticOperation => |node| {
                try node.left.print(writer, indent);
                switch (node.operation) {
                    .Add => try writer.print(" + ", .{}),
                    .Subtract => try writer.print(" - ", .{}),
                    .Multiply => try writer.print(" * ", .{}),
                    .Divide => try writer.print(" / ", .{}),
                    .Modulus => try writer.print(" % ", .{}),
                }
                try node.right.print(writer, indent);
            },
            .ComparisonOperation => |node| {
                try node.left.print(writer, indent);
                switch (node.operation) {
                    .LessThan => try writer.print(" < ", .{}),
                    .LessThanEqual => try writer.print(" <= ", .{}),
                    .GreaterThan => try writer.print(" > ", .{}),
                    .GreaterThanEqual => try writer.print(" >= ", .{}),
                    .Equal => try writer.print(" == ", .{}),
                    .NotEqual => try writer.print(" != ", .{}),
                }
                try node.right.print(writer, indent);
            },
            .BooleanOperation => |node| {
                try node.left.print(writer, indent);
                switch (node.operation) {
                    .And => try writer.print(" && ", .{}),
                    .Or => try writer.print(" || ", .{}),
                    .Not => try writer.print(" !", .{}),
                }
                try node.right.print(writer, indent);
            },
            .Type => |node| {
                try writer.print("{s}", .{node.identifier});
            },
        }
    }

    pub fn deinit(self: NodeData, allocator: std.mem.Allocator) void {
        switch (self) {
            .Null => |node| {
                _ = node;
            },
            .Assignment => |node| {
                allocator.free(node.identifier);
                node.value.deinit(allocator);
                allocator.destroy(node.value);
                node.type.deinit(allocator);
                allocator.destroy(node.type);
            },
            .ForLoop => |node| {
                allocator.free(node.iteratorIdentifier);
                allocator.free(node.listIdentifier);
                node.block.deinit(allocator);
                allocator.destroy(node.block);
            },
            .Range => |node| {
                node.startValue.deinit(allocator);
                allocator.destroy(node.startValue);
                node.endValue.deinit(allocator);
                allocator.destroy(node.endValue);
            },
            .Function => |node| {
                for (node.parameters) |parameter| {
                    parameter.deinit(allocator);
                }
                node.block.deinit(allocator);
                allocator.free(node.parameters);
            },
            .FunctionCall => |node| {
                for (node.arguments) |argument| {
                    argument.deinit(allocator);
                    allocator.destroy(argument);
                }
                allocator.free(node.arguments);
            },
            .Return => |node| {
                node.value.deinit(allocator);
                allocator.destroy(node.value);
            },
            .If => |node| {
                node.condition.deinit(allocator);
                allocator.destroy(node.condition);
                node.block.deinit(allocator);
                allocator.destroy(node.block);
                if (node.elseBlock != null) {
                    node.elseBlock.?.deinit(allocator);
                    allocator.destroy(node.elseBlock.?);
                }
            },
            .Literal => |node| {
                // Nothing to deinit
                _ = node;
            },
            .Block => |node| {
                for (node.statements) |statement| {
                    statement.deinit(allocator);
                }
                allocator.free(node.statements);
            },
            .Identifier => |node| {
                allocator.free(node.identifier);
            },
            .ArithmeticOperation => |node| {
                node.left.deinit(allocator);
                node.right.deinit(allocator);
                allocator.destroy(node.left);
                allocator.destroy(node.right);
            },
            .ComparisonOperation => |node| {
                node.left.deinit(allocator);
                node.right.deinit(allocator);
                allocator.destroy(node.left);
                allocator.destroy(node.right);
            },
            .BooleanOperation => |node| {
                node.left.deinit(allocator);
                node.right.deinit(allocator);
                allocator.destroy(node.left);
                allocator.destroy(node.right);
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

pub const Node = struct {
    node: NodeData,
    line: u32,
    column: u32,
};

pub const AST = struct {
    root: *NodeData,

    pub fn print(self: AST, writer: *const std.io.AnyWriter, indent: usize) !void {
        try self.root.print(writer, indent);
    }

    pub fn deinit(self: AST, allocator: std.mem.Allocator) void {
        self.root.deinit(allocator);
        allocator.destroy(self.root);
    }
};
