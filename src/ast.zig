const std = @import("std");
const repeat = @import("./parser/grammar.zig").repeat;

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

    pub fn write(self: FunctionParameter, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
        try writer.print("{s}: ", .{self.identifier});
        try self.type.print(self.type.*, writer, indent, allocator);
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

    pub fn print(self: *const ASTNode, writer: *const std.io.AnyWriter, indent: usize, allocator: std.mem.Allocator) anyerror!void {
        const indentString = try repeat("  ", indent, allocator);
        defer allocator.free(indentString);
        switch (self.*) {
            .Null => try writer.print("{s}NULL", .{indentString}),
            .Assignment => |node| {
                try writer.print("{s}ASSIGNMENT:\n", .{indentString});
                try writer.print("{s}  Identifier: {s}\n", .{ indentString, node.identifier });
                try writer.print("{s}  Value:\n", .{indentString});
                try node.value.print(node.value.*, writer, indent + 1, allocator);
                try writer.print("{s}  Type:\n", .{indentString});
                try node.type.print(node.type.*, writer, indent + 1, allocator);
            },
            .ForLoop => |node| {
                try writer.print("{s}FOR LOOP:\n", .{indentString});
                try writer.print("{s}  Iterator Identifier: {s}\n", .{ indentString, node.iteratorIdentifier });
                try writer.print("{s}  List Identifier: {s}\n", .{ indentString, node.listIdentifier });
                try writer.print("{s}  Block:\n", .{indentString});
                try node.block.print(node.block.*, writer, indent + 1, allocator);
            },
            .Range => |node| {
                try writer.print("{s}RANGE:\n", .{indentString});
                try writer.print("{s}  Start Value:\n", .{indentString});
                try node.startValue.print(node.startValue.*, writer, indent + 1, allocator);
                try writer.print("{s}  End Value:\n", .{indentString});
                try node.endValue.print(node.endValue.*, writer, indent + 1, allocator);
            },
            .Function => |node| {
                try writer.print("{s}FUNCTION:\n", .{indentString});
                try writer.print("{s}  Parameters:\n", .{indentString});
                for (node.parameters) |param| {
                    try param.write(writer, indent + 1, allocator);
                }
                try writer.print("{s}  Block:\n", .{indentString});
                try node.block.print(node.block.*, writer, indent + 1, allocator);
            },
            .FunctionCall => |node| {
                try writer.print("{s}FUNCTION CALL:\n", .{indentString});
                try writer.print("{s}  Identifier: {s}\n", .{ indentString, node.identifier });
                try writer.print("{s}  Arguments:\n", .{indentString});
                for (node.arguments) |arg| {
                    try arg.print(arg.*, writer, indent + 1, allocator);
                }
            },
            .Return => |node| {
                try writer.print("{s}RETURN:\n", .{indentString});
                try writer.print("{s}  Value:\n", .{indentString});
                try node.value.print(node.value.*, writer, indent + 1, allocator);
            },
            .If => |node| {
                try writer.print("{s}IF:\n", .{indentString});
                try writer.print("{s}  Condition:\n", .{indentString});
                try node.condition.print(node.condition.*, writer, indent + 1, allocator);
                try writer.print("{s}  Block:\n", .{indentString});
                try node.block.print(node.block.*, writer, indent + 1, allocator);
                if (node.elseBlock != null) {
                    try writer.print("{s}  Else Block:\n", .{indentString});
                    try node.elseBlock.?.print(node.elseBlock.?.*, writer, indent + 1, allocator);
                }
            },
            .Literal => |node| {
                switch (node) {
                    .IntLiteral => try writer.print("{s}INT LITERAL: {d}\n", .{ indentString, node.IntLiteral }),
                    .StringLiteral => try writer.print("{s}STRING LITERAL: {s}\n", .{ indentString, node.StringLiteral }),
                }
            },
            .Block => |node| {
                try writer.print("{s}BLOCK:\n", .{indentString});
                for (node.statements) |statement| {
                    try statement.print(statement.*, writer, indent + 1, allocator);
                }
            },
            .Identifier => |node| {
                try writer.print("{s}IDENTIFIER: {s}\n", .{ indentString, node.identifier });
            },
            .ArithmeticOperation => |node| {
                try writer.print("{s}ARITHMETIC OPERATION:\n", .{indentString});
                try writer.print("{s}  Operation: ", .{indentString});
                switch (node.operation) {
                    .Add => try writer.print("Add\n", .{}),
                    .Subtract => try writer.print("Subtract\n", .{}),
                    .Multiply => try writer.print("Multiply\n", .{}),
                    .Divide => try writer.print("Divide\n", .{}),
                    .Modulus => try writer.print("Modulus\n", .{}),
                }
                try writer.print("{s}  Left:\n", .{indentString});
                try node.left.print(node.left.*, writer, indent + 1, allocator);
                try writer.print("{s}  Right:\n", .{indentString});
                try node.right.print(node.right.*, writer, indent + 1, allocator);
            },
            .ComparisonOperation => |node| {
                try writer.print("{s}COMPARISON OPERATION:\n", .{indentString});
                try writer.print("{s}  Operation: ", .{indentString});
                switch (node.operation) {
                    .LessThan => try writer.print("Less Than\n", .{}),
                    .LessThanEqual => try writer.print("Less Than Equal\n", .{}),
                    .GreaterThan => try writer.print("Greater Than\n", .{}),
                    .GreaterThanEqual => try writer.print("Greater Than Equal\n", .{}),
                    .Equal => try writer.print("Equal\n", .{}),
                    .NotEqual => try writer.print("Not Equal\n", .{}),
                }
                try writer.print("{s}  Left:\n", .{indentString});
                try node.left.print(node.left.*, writer, indent + 1, allocator);
                try writer.print("{s}  Right:\n", .{indentString});
                try node.right.print(node.right.*, writer, indent + 1, allocator);
            },
            .BooleanOperation => |node| {
                try writer.print("{s}BOOLEAN OPERATION:\n", .{indentString});
                try writer.print("{s}  Operation: ", .{indentString});
                switch (node.operation) {
                    .And => try writer.print("And\n", .{}),
                    .Or => try writer.print("Or\n", .{}),
                    .Not => try writer.print("Not\n", .{}),
                }
                try writer.print("{s}  Left:\n", .{indentString});
                try node.left.print(node.left.*, writer, indent + 1, allocator);
                try writer.print("{s}  Right:\n", .{indentString});
                try node.right.print(node.right.*, writer, indent + 1, allocator);
            },
            .Type => |node| {
                try writer.print("{s}TYPE: {s}\n", .{ indentString, node.identifier });
            },
        }
    }
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
