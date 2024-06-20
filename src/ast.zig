pub const NodeType = enum { Assignment, ForLoop, Range, Function, FunctionCall, Return, If, Literal, Block, Identifier, ArithmeticOperation, ComparisonOperation, BooleanOperation, Type };

pub const LiteralType = enum { IntLiteral, StringLiteral };

pub const AirthmeticOperationType = enum { Add, Subtract, Multiply, Divide, Modulus };
pub const ComparisonOperationType = enum { LessThan, LessThanEqual, GreaterThan, GreaterThanEqual, Equal, NotEqual };
pub const BooleanOperationType = enum { And, Or, Not };

pub const NodeData = union(NodeType) {
    Assignment: struct {
        identifier: []const u8,
        value: *NodeData,
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
        elseBlock: *NodeData,
    },
    Literal: union(LiteralType) {
        IntLiteral: u64,
        StringLiteral: []const u8,
    },
    Block: struct {
        nodes: []*NodeData,
    },
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
};

pub const Node = struct {
    node: NodeData,
    line: u32,
    column: u32,
};
