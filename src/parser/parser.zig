const std = @import("std");

const TokenType = @import("../token.zig").TokenType;
const Token = @import("../token.zig").Token;
const TokenLiteral = @import("../token.zig").TokenLiteral;

const ArgsFlags = @import("../args_parser.zig").ArgsFlags;

const Program = @import("./program.zig").Program;
const Expression = @import("./expression.zig").Expression;
const Statement = @import("./statement.zig").Statement;
const ASTPrinter = @import("./ast_printer.zig").ASTPrinter;

const ClassMethod = @import("./expression.zig").ClassExpressionMethod;
const FunctionExpression = @import("./expression.zig").FunctionExpression;

const Parser = @This();

const prettyError = @import("../errors.zig").prettyError;
const errorContext = @import("../errors.zig").errorContext;

tokens: []Token,
current: usize = 0,

originalBuffer: []const u8,
filePath: []const u8,

allocator: std.mem.Allocator,
flags: ArgsFlags,

hasError: bool = false,

/// Errors in Zig can't hold payloads, so we separate the actual error data from the error type.
const ParseError = struct {
    @"error": union(enum) { ConsumeFailed: struct {
        string: []const u8,
        token: Token,
    } },

    filePath: []const u8,
    originalBuffer: []const u8,

    pub fn print(self: *const ParseError, allocator: std.mem.Allocator) void {
        switch (self.@"error") {
            .ConsumeFailed => |value| {
                const tokenString = value.token.toString(allocator) catch {
                    return;
                };
                defer allocator.free(tokenString);

                const errorMessage = std.fmt.allocPrint(allocator, "{s} at {s}", .{ value.string, tokenString }) catch {
                    return;
                };
                defer allocator.free(errorMessage);

                prettyError(errorMessage) catch {
                    return;
                };
                errorContext(self.originalBuffer, self.filePath, value.token.position, value.token.lexeme.len, allocator) catch {
                    return;
                };
            },
        }
    }

    pub fn consumeFailed(parser: *const Parser, string: []const u8, token: Token) ParseError {
        return .{ .@"error" = .{ .ConsumeFailed = .{ .string = string, .token = token } }, .filePath = parser.filePath, .originalBuffer = parser.originalBuffer };
    }
};

const ParseErrorEnum = error{
    Unknown,
};

pub fn init(tokens: []Token, filePath: []const u8, originalBuffer: []const u8, flags: ArgsFlags, allocator: std.mem.Allocator) !Parser {
    return .{ .tokens = tokens, .filePath = filePath, .originalBuffer = originalBuffer, .flags = flags, .allocator = allocator };
}

pub fn parse(self: *Parser) anyerror!*Program {
    self.hasError = false;

    var program = Program.init(self.allocator);
    errdefer program.deinit(self.allocator, false);

    var hadError = false;
    while (!self.isAtEnd()) {
        const declaration = self.consumeDeclarationAndSynchronize(true) orelse {
            hadError = true;
            continue;
        };
        try program.addStatement(declaration);
    }

    if (hadError) {
        return ParseErrorEnum.Unknown;
    }

    // We can't use matchToken here because it will detect we're at an EOF and return false
    if (self.peek().type != TokenType.EOF) {
        const err = ParseError.consumeFailed(self, "Expected EOF", self.peek());
        err.print(self.allocator);
        return ParseErrorEnum.Unknown;
    }

    const programAllocated = try self.allocator.create(Program);
    programAllocated.* = program;
    return programAllocated;
}

pub fn parseExpression(self: *Parser) anyerror!*Expression {
    const expression = try self.consumeExpression();

    if (!self.isAtEnd()) {
        const err = ParseError.consumeFailed(self, "Expected EOF", self.peek());
        err.print(self.allocator);
        return ParseErrorEnum.Unknown;
    }

    return expression;
}

/// Sets "hasError" to true and prints an error message on the current token.
fn errorOccured(self: *Parser, errorMessage: []const u8) void {
    const err = ParseError.consumeFailed(self, errorMessage, self.peek());
    err.print(self.allocator);
    self.hasError = true;
}
/// Sets "hasError" to true and prints an error message on a specific token.
fn errorOccuredAt(self: *Parser, errorMessage: []const u8, token: Token) void {
    const err = ParseError.consumeFailed(self, errorMessage, token);
    err.print(self.allocator);
    self.hasError = true;
}

fn isAtEnd(self: *Parser) bool {
    return self.current >= self.tokens.len - 1; // -1 because the last token is always EOF
}

fn peek(self: *Parser) Token {
    return self.tokens[self.current];
}

fn peekPrevious(self: *Parser) Token {
    return self.tokens[self.current - 1];
}

fn advance(self: *Parser) Token {
    if (!self.isAtEnd()) {
        self.current += 1;
    }
    return self.peekPrevious();
}

fn matchToken(self: *Parser, @"type": TokenType) bool {
    if (self.isAtEnd()) {
        return false;
    }
    if (self.peek().type == @"type") {
        _ = self.advance();
        return true;
    }
    return false;
}

/// Attempts to consume a token of the given type. If the token is not of the given type, returns an error.
fn consume(self: *Parser, @"type": TokenType, errorMessage: []const u8) !Token {
    if (self.matchToken(@"type")) {
        return self.peekPrevious();
    }

    self.errorOccured(errorMessage);
    return ParseErrorEnum.Unknown;
}

//
// Grammar rules
//

fn consumeDeclarationAndSynchronize(self: *Parser, topLevel: bool) ?*Statement {
    return self.consumeDeclaration(topLevel) catch {
        self.synchronize();
        return null;
    };
}

fn consumeDeclaration(self: *Parser, topLevel: bool) anyerror!*Statement {
    if (self.matchToken(TokenType.LetKeyword)) {
        return try self.consumeLetStatement(false);
    }
    if (self.matchToken(TokenType.ExportKeyword)) {
        if (!topLevel) {
            self.errorOccured("Cannot export a variable in a block statement");
            return ParseErrorEnum.Unknown;
        }

        _ = try self.consume(TokenType.LetKeyword, "Expected 'let' after 'export'");

        return try self.consumeLetStatement(true);
    }

    return try self.consumeStatement();
}

fn consumeStatement(self: *Parser) anyerror!*Statement {
    if (self.matchToken(TokenType.IfKeyword)) {
        return try self.consumeIfStatement();
    }
    if (self.matchToken(TokenType.WhileKeyword)) {
        return try self.consumeWhileStatement();
    }
    if (self.matchToken(TokenType.OpenCurly)) {
        return try self.consumeBlockStatement();
    }
    if (self.matchToken(TokenType.ReturnKeyword)) {
        return try self.consumeReturnStatement();
    }
    if (self.matchToken(TokenType.BreakKeyword)) {
        return try self.consumeBreakStatement();
    }
    if (self.matchToken(TokenType.ContinueKeyword)) {
        return try self.consumeContinueStatement();
    }

    return try self.consumeExpressionStatement();
}

fn consumeReturnStatement(self: *Parser) anyerror!*Statement {
    const value = if (self.peek().type == .Semicolon)
        try Expression.literal(self.allocator, TokenLiteral.Null)
    else
        try self.consumeExpression();

    _ = try self.consume(TokenType.Semicolon, "Expected ';' after a return statement");

    return Statement.returnStatement(self.allocator, value);
}

fn consumeBreakStatement(self: *Parser) anyerror!*Statement {
    _ = try self.consume(TokenType.Semicolon, "Expected ';' after a break statement");

    return Statement.breakStatement(self.allocator);
}

fn consumeContinueStatement(self: *Parser) anyerror!*Statement {
    _ = try self.consume(TokenType.Semicolon, "Expected ';' after a continue statement");

    return Statement.continueStatement(self.allocator);
}

fn consumeIfStatement(self: *Parser) anyerror!*Statement {
    _ = try self.consume(TokenType.OpenParen, "Expected '(' after 'if'");
    const condition = try self.consumeExpression();
    _ = try self.consume(TokenType.CloseParen, "Expected ')' after if condition");
    const trueBranch = try self.consumeStatement();

    const falseBranch: ?*Statement = if (self.matchToken(TokenType.ElseKeyword)) try self.consumeStatement() else null;
    return Statement.ifBlock(self.allocator, condition, trueBranch, falseBranch);
}

fn consumeWhileStatement(self: *Parser) anyerror!*Statement {
    _ = try self.consume(TokenType.OpenParen, "Expected '(' after 'while'");
    const condition = try self.consumeExpression();
    _ = try self.consume(TokenType.CloseParen, "Expected ')' after while condition");
    const body = try self.consumeStatement();

    return Statement.whileBlock(self.allocator, condition, body);
}

fn consumeBlockStatement(self: *Parser) anyerror!*Statement {
    var statements = std.ArrayList(*Statement).init(self.allocator);
    errdefer {
        for (statements.items) |statement| {
            statement.uninit(self.allocator);
        }
        statements.deinit();
    }

    while (!(self.peek().type == TokenType.CloseCurly) and !self.isAtEnd()) {
        const statement = self.consumeDeclarationAndSynchronize(false) orelse continue;
        try statements.append(statement);
    }

    _ = try self.consume(TokenType.CloseCurly, "Expected '}' after a block statement");

    return Statement.block(self.allocator, statements);
}

fn consumeLetStatement(self: *Parser, exported: bool) anyerror!*Statement {
    const name = try self.consume(TokenType.Identifier, "Expected variable name");
    errdefer name.deinit(self.allocator);

    const initializer = if (self.matchToken(TokenType.Assign))
        try self.consumeExpression()
    else
        try Expression.literal(self.allocator, TokenLiteral.Null);
    errdefer initializer.uninit(self.allocator);

    _ = try self.consume(TokenType.Semicolon, "Expected ';' after a variable declaration.");

    return Statement.let(self.allocator, name, initializer, exported);
}

fn consumeExpressionStatement(self: *Parser) anyerror!*Statement {
    const expression = try self.consumeExpression();
    errdefer expression.uninit(self.allocator);

    _ = try self.consume(TokenType.Semicolon, "Expected ';' after an expression statement.");

    return Statement.expression(self.allocator, expression);
}

// Expression parsing
// Operators with higher precedence are parsed first

fn consumeExpression(self: *Parser) anyerror!*Expression {
    if (self.matchToken(TokenType.FunctionKeyword)) {
        return try self.consumeFunctionExpression();
    }
    if (self.matchToken(TokenType.ClassKeyword)) {
        return try self.consumeClassExpression();
    }

    return try self.consumeAssignment();
}

fn consumeClassExpression(self: *Parser) anyerror!*Expression {
    var methods = std.ArrayListUnmanaged(ClassMethod){};

    const startToken = self.peekPrevious();

    var superclass: ?*Expression = null;
    if (self.matchToken(TokenType.ExtendingKeyword)) {
        superclass = try self.consumeExpression();
    }

    _ = try self.consume(TokenType.OpenCurly, "Expected '{' after 'class'");
    while (!self.matchToken(TokenType.CloseCurly) and !self.isAtEnd()) {
        const method = try self.consumeClassMethod();
        try methods.append(self.allocator, method);
    }

    return Expression.class(self.allocator, methods, startToken, superclass);
}

fn consumeClassMethod(self: *Parser) anyerror!ClassMethod {
    const static = self.matchToken(TokenType.StaticKeyword);
    const name = try self.consume(TokenType.Identifier, "Expected method name");
    const function = try self.consumeFunctionExpressionInner();

    return ClassMethod.new(static, name, function);
}

fn consumeFunctionExpression(self: *Parser) anyerror!*Expression {
    const functionExpression = try consumeFunctionExpressionInner(self);
    return Expression.function(self.allocator, functionExpression);
}

fn consumeFunctionExpressionInner(self: *Parser) anyerror!FunctionExpression {
    var parameters = std.ArrayListUnmanaged(Token){};

    _ = try self.consume(TokenType.OpenParen, "Expected '(' after 'function'");
    if (!self.matchToken(TokenType.CloseParen)) {
        while (true) {
            if (parameters.items.len >= 255) {
                // Continue parsing but alert the user
                self.errorOccured("Too many parameters in function; can't have more than 255 parameters");
            }

            const parameter = self.consume(TokenType.Identifier, "Expected parameter name") catch {
                self.errorOccured("Expected parameter name");
                return ParseErrorEnum.Unknown;
            };
            try parameters.append(self.allocator, parameter);

            if (!self.matchToken(TokenType.Comma)) {
                break;
            }
        }

        _ = self.consume(TokenType.CloseParen, "Expected ')' after function parameters") catch {
            self.errorOccured("Expected ')' after function parameters");
            return ParseErrorEnum.Unknown;
        };
    }

    _ = try self.consume(TokenType.OpenCurly, "Expected '{' before function body");

    const body = try self.consumeBlockStatement();

    return .{ .parameters = parameters, .body = body };
}

fn consumeAssignment(self: *Parser) anyerror!*Expression {
    const expression = try self.consumeLogicalOr();

    if (self.matchToken(TokenType.Assign)) {
        // This is an assignment

        const equals = self.peekPrevious(); // Used for error reporting
        const value = try self.consumeAssignment();

        switch (expression.*.value) {
            .VariableAccess => |access| {
                expression.uninit(self.allocator);
                const variable = access.name;
                return Expression.variableAssignment(self.allocator, variable, value);
            },
            .PropertyAccess => |access| {
                self.allocator.destroy(expression); // Don't uninit the entire tree, just the single node
                return Expression.propertyAssignment(self.allocator, access.object, access.name, value);
            },
            .DynamicPropertyAccess => |access| {
                self.allocator.destroy(expression); // Don't uninit the entire tree, just the single node
                return Expression.dynamicPropertyAssignment(self.allocator, access.object, access.name, value, access.startToken);
            },
            else => {
                value.uninit(self.allocator);
            },
        }

        const err = ParseError.consumeFailed(self, "Invalid assignment target", equals);
        err.print(self.allocator);
        return ParseErrorEnum.Unknown;
    }

    return expression;
}

fn consumeLogicalOr(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeLogicalAnd();

    while (self.matchToken(TokenType.Or)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeLogicalAnd();
        errdefer right.uninit(self.allocator);

        expression = try Expression.logical(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeLogicalAnd(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeEquality();

    while (self.matchToken(TokenType.And)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeEquality();
        errdefer right.uninit(self.allocator);

        expression = try Expression.logical(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeEquality(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeComparison();

    while (self.matchToken(TokenType.NotEqual) or self.matchToken(TokenType.Equality)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeComparison();
        expression = try Expression.binary(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeComparison(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeBitwiseOr();

    while (self.matchToken(TokenType.GreaterThan) or
        self.matchToken(TokenType.GreaterThanEqual) or
        self.matchToken(TokenType.LessThan) or
        self.matchToken(TokenType.LessThanEqual))
    {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeBitwiseOr();
        errdefer right.uninit(self.allocator);

        expression = try Expression.binary(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeBitwiseOr(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeBitwiseXor();

    while (self.matchToken(TokenType.BitwiseOr)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeBitwiseXor();
        errdefer right.uninit(self.allocator);

        expression = try Expression.bitwise(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeBitwiseXor(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeBitwiseAnd();

    while (self.matchToken(TokenType.BitwiseXor)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeBitwiseAnd();
        errdefer right.uninit(self.allocator);

        expression = try Expression.bitwise(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeBitwiseAnd(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeTerm();

    while (self.matchToken(TokenType.BitwiseAnd)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeTerm();
        errdefer right.uninit(self.allocator);

        expression = try Expression.bitwise(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeTerm(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeFactor();

    while (self.matchToken(TokenType.Minus) or self.matchToken(TokenType.Plus)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeFactor();
        errdefer right.uninit(self.allocator);

        expression = try Expression.binary(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeFactor(self: *Parser) anyerror!*Expression {
    var expression = try self.consumeUnary();

    while (self.matchToken(TokenType.Slash) or self.matchToken(TokenType.Star) or self.matchToken(TokenType.Percent)) {
        errdefer expression.uninit(self.allocator);

        const operator = self.peekPrevious();
        const right = try self.consumeUnary();
        errdefer right.uninit(self.allocator);

        expression = try Expression.binary(self.allocator, expression, operator, right);
    }

    return expression;
}

fn consumeUnary(self: *Parser) anyerror!*Expression {
    if (self.matchToken(TokenType.Negate) or self.matchToken(TokenType.Minus)) {
        const operator = self.peekPrevious();

        if (self.peek().type == TokenType.Negate or self.peek().type == TokenType.Minus) {
            const right = try self.consumeUnary();
            errdefer right.uninit(self.allocator);
            return try Expression.unary(self.allocator, operator, right);
        }
        const right = try self.consumeFunctionCall();
        errdefer right.uninit(self.allocator);

        return try Expression.unary(self.allocator, operator, right);
    }

    return try self.consumeFunctionCall();
}

fn consumeFunctionCall(self: *Parser) anyerror!*Expression {
    var expression = try self.consumePrimary();
    errdefer expression.uninit(self.allocator);

    while (true) {
        if (self.matchToken(TokenType.OpenParen)) {
            expression = try self.finishFunctionCall(expression);
        } else if (self.matchToken(TokenType.Dot)) {
            expression = try self.consumePropertyAccess(expression);
        } else if (self.matchToken(TokenType.OpenSquare)) {
            const startToken = self.peekPrevious();
            const index = try self.consumeExpression();
            _ = try self.consume(TokenType.CloseSquare, "Expected ']' after dynamic property access index");
            expression = try Expression.dynamicPropertyAccess(self.allocator, expression, index, startToken);
        } else {
            break;
        }
    }

    return expression;
}

fn consumePropertyAccess(self: *Parser, object: *Expression) anyerror!*Expression {
    const property = try self.consume(TokenType.Identifier, "Expected property name after '.'");
    return Expression.propertyAccess(self.allocator, object, property);
}

fn finishFunctionCall(self: *Parser, callee: *Expression) anyerror!*Expression {
    const startToken = self.peekPrevious();

    var arguments = std.ArrayListUnmanaged(*const Expression){};
    errdefer {
        for (arguments.items) |argument| {
            argument.uninit(self.allocator);
        }
        arguments.deinit(self.allocator);
    }

    if (!self.matchToken(TokenType.CloseParen)) {
        while (true) {
            if (arguments.items.len >= 255) {
                // Continue parsing but alert the user
                self.errorOccured("Too many arguments in function call; can't have more than 255 arguments");
            }

            const argument = try self.consumeExpression();
            try arguments.append(self.allocator, argument);

            if (!self.matchToken(TokenType.Comma)) {
                break;
            }
        }

        _ = try self.consume(TokenType.CloseParen, "Expected ')' after function arguments");
    }

    return Expression.functionCall(self.allocator, callee, startToken, arguments);
}

fn consumePrimary(self: *Parser) anyerror!*Expression {
    if (self.matchToken(TokenType.FalseKeyword)) {
        return Expression.literal(self.allocator, TokenLiteral.False);
    }
    if (self.matchToken(TokenType.TrueKeyword)) {
        return Expression.literal(self.allocator, TokenLiteral.True);
    }
    if (self.matchToken(TokenType.NullKeyword)) {
        return Expression.literal(self.allocator, TokenLiteral.Null);
    }

    if (self.matchToken(TokenType.NumberLiteral) or self.matchToken(TokenType.StringLiteral)) { // or self.matchToken(TokenType.Identifier)
        return Expression.literal(self.allocator, self.peekPrevious().literal);
    }

    if (self.matchToken(TokenType.OpenParen)) {
        const expression = try self.consumeExpression();
        if (!self.matchToken(TokenType.CloseParen)) {
            const err = ParseError.consumeFailed(self, "Expected ')'", self.peek());
            err.print(self.allocator);
            return ParseErrorEnum.Unknown;
        }
        return Expression.grouping(self.allocator, expression);
    }

    if (self.matchToken(TokenType.Identifier)) {
        return Expression.variableAccess(self.allocator, self.peekPrevious());
    }

    if (self.matchToken(TokenType.ThisKeyword)) {
        return Expression.this(self.allocator, self.peekPrevious());
    }
    if (self.matchToken(TokenType.SuperKeyword)) {
        const keyword = self.peekPrevious();
        if (!self.matchToken(TokenType.Dot)) {
            if (self.peek().type != TokenType.OpenParen) {
                const err = ParseError.consumeFailed(self, "Expected '.' or '(' after 'super'.", self.peek());
                err.print(self.allocator);
                return ParseErrorEnum.Unknown;
            }

            return Expression.super(self.allocator, keyword, null);
        }
        const method = try self.consume(TokenType.Identifier, "Expected method name after 'super.'");
        return Expression.super(self.allocator, keyword, method);
    }

    const err = ParseError.consumeFailed(self, "Expected expression", self.peek());
    err.print(self.allocator);
    return ParseErrorEnum.Unknown;
}

// Error handling

fn synchronize(self: *Parser) void {
    _ = self.advance();

    while (!self.isAtEnd()) {
        if (self.peekPrevious().type == TokenType.Semicolon) {
            return;
        }

        switch (self.peek().type) {
            .ClassKeyword => return,
            .FunctionKeyword => return,
            .LetKeyword => return,
            .IfKeyword => return,
            .WhileKeyword => return,
            .ForKeyword => return,
            .ReturnKeyword => return,
            else => {},
        }

        _ = self.advance();
    }
}
