const ast = @import("ast.zig");
const std = @import("std");
const l = @import("lexer.zig");

pub const ParserError = error{
    InvalidToken,
    MemoryAllocation,
    ExpectedInteger,
    ExpectedIndentifier,
    ExpectedOperator,
};

fn getOperatorFromToken(token: l.Token) !ast.Operator {
    return switch (token.token_type) {
        .ASSIGN => ast.Operator.assign,
        .PLUS => ast.Operator.plus,
        .MINUS => ast.Operator.minus,
        .BANG => ast.Operator.bang,
        .ASTERISK => ast.Operator.asterisk,
        .SLASH => ast.Operator.slash,
        .LT => ast.Operator.lt,
        .GT => ast.Operator.gt,
        .EQ => ast.Operator.equal,
        .NOT_EQ => ast.Operator.notEqual,
        else => ParserError.ExpectedOperator,
    };
}

const Priority = enum(u4) {
    lowest = 0,
    equal_ne = 1,
    less_greater = 2,
    plus_minus = 3,
    prod_div = 4,
    prefix = 5,
    call = 6,

    fn fromToken(token: l.Token) Priority {
        return switch (token.token_type) {
            .EQ => Priority.equal_ne,
            .NOT_EQ => Priority.equal_ne,
            .LT => Priority.less_greater,
            .GT => Priority.less_greater,
            .PLUS => Priority.plus_minus,
            .MINUS => Priority.plus_minus,
            .ASTERISK => Priority.prod_div,
            .SLASH => Priority.prod_div,
            .LPAREN => Priority.call,
            else => Priority.lowest,
        };
    }

    fn lessThan(self: Priority, other: Priority) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *l.Lexer,
    curr_token: l.Token = undefined,
    peek_token: l.Token = undefined,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, lexer: *l.Lexer) Self {
        var parser = Self{
            .lexer = lexer,
            .allocator = allocator,
        };
        // running twice means curr_token will point at 0th index in lexer output
        // peek token will point at 1st index in lexer output
        parser.nextToken();
        parser.nextToken();
        return parser;
    }

    fn parseProgram(self: *Self) !ast.Program {
        var prog = ast.Program.init(self.allocator);
        while (self.curr_token.token_type != l.TokenType.EOF) {
            // std.debug.print("R2D1\n{s}\t{s}\n", .{ self.curr_token.literal, self.peek_token.literal });
            var stmt = try self.parseStatement();
            try prog.statements.append(stmt);
            self.nextToken(); // move to the next statement
        }
        return prog;
    }

    fn nextToken(self: *Self) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
        // std.debug.print("Current: {s}\tPeek: {s}", .{ self.curr_token.literal, self.peek_token.literal });
    }

    fn parseStatement(self: *Self) !ast.Statement {
        return switch (self.curr_token.token_type) {
            l.TokenType.LET => ast.Statement{ .LET = try self.parseLetStatement() },
            l.TokenType.RETURN => ast.Statement{ .RETURN = try self.parseReturnStatement() },
            else => {
                std.debug.print("R2D2 {s}\n", .{@tagName(self.curr_token.token_type)});
                return ParserError.InvalidToken;
            },
        };
    }

    fn parseReturnStatement(self: *Self) !ast.Return {
        // we reach here, we know current token is return
        self.nextToken();
        const returnValue = try self.parseExpression(.lowest);

        // advance to next line not needed here since parseProgram does it? !NO
        // we have to consume the semicolon? oof these off by one issues!
        if (self.peekTokenIs(l.TokenType.SEMICOLON)) {
            self.nextToken();
        }
        const retPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        retPtr.* = returnValue;
        return ast.Return{ .value = retPtr };
    }

    fn parseLetStatement(self: *Self) !ast.Let {
        // next has to be identifier
        if (!self.expectPeek(l.TokenType.IDENT)) {
            return ParserError.InvalidToken;
        }
        const name = ast.Identifier{
            .value = self.curr_token.literal,
        };
        // next has to be equals sign
        if (!self.expectPeek(l.TokenType.ASSIGN)) {
            return ParserError.InvalidToken;
        }
        // after last expectPeek moves current on = sign, not beyond it.
        // for actually parsing expression, discard the = and go beyond
        self.nextToken();
        var expr = try self.parseExpression(Priority.lowest);
        // advance to next line not needed here since parseProgram does it? !NO
        // we have to consume the semicolon? oof these off by one issues!
        if (self.peekTokenIs(l.TokenType.SEMICOLON)) {
            self.nextToken();
        }
        const exprPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        exprPtr.* = expr;
        return ast.Let{ .name = name, .value = exprPtr };
    }

    fn parseIdentifier(self: Self) ParserError!ast.Identifier {
        switch (self.curr_token.token_type) {
            l.TokenType.IDENT => |v| {
                _ = v;
                return ast.Identifier{ .value = self.curr_token.literal };
            },
            else => {
                return ParserError.ExpectedIndentifier;
            },
        }
    }

    fn parseInteger(self: Self) ParserError!ast.Integer {
        switch (self.curr_token.token_type) {
            l.TokenType.INT => {
                const int_val = std.fmt.parseInt(i64, self.curr_token.literal, 10) catch return ParserError.InvalidToken;
                return ast.Integer{ .value = int_val };
            },
            else => {
                return ParserError.ExpectedInteger;
            },
        }
    }

    fn parseExpression(self: *Self, priority: Priority) ParserError!ast.Expression {
        var leftExpression = try self.parseExpressionByPrefixToken();

        while ((!self.peekTokenIs(l.TokenType.SEMICOLON)) and (priority.lessThan(Priority.fromToken(self.peek_token)))) {
            const lftPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
            lftPtr.* = leftExpression;

            self.nextToken();

            // left is partially complete like 1 from 1 + 1
            // pass its value ang get updated result like binop + , 1, 1

            leftExpression = try self.parseInfixExpressionByToken(lftPtr);
        }
        return leftExpression;
    }

    fn parsePrefixExpression(self: *Self) ParserError!ast.PrefixExpression {
        const operator = try getOperatorFromToken(self.curr_token);
        self.nextToken();
        const right = try self.parseExpression(Priority.prefix);
        // creates space on allocated memory for storing the expression
        const rtPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        // right is moved on to allocated memory here, so won't be lost on return from function
        rtPtr.* = right;
        return ast.PrefixExpression{ .operator = operator, .right = rtPtr };
    }

    fn parseInfixExpression(self: *Self, lftPtr: *ast.Expression) ParserError!ast.InfixExpression {
        const operator = try getOperatorFromToken(self.curr_token);
        const prio = Priority.fromToken(self.curr_token);

        self.nextToken();

        const right = try self.parseExpression(prio);
        const rtPtr = self.allocator.create(ast.Expression) catch return ParserError.MemoryAllocation;
        rtPtr.* = right;

        return ast.InfixExpression{ .operator = operator, .left = lftPtr, .right = rtPtr };
    }

    fn parseExpressionByPrefixToken(self: *Self) ParserError!ast.Expression {
        return switch (self.curr_token.token_type) {
            l.TokenType.IDENT => ast.Expression{ .identifier = try self.parseIdentifier() },
            l.TokenType.INT => ast.Expression{ .integer = try self.parseInteger() },
            l.TokenType.BANG, l.TokenType.MINUS => ast.Expression{ .prefixExpr = try self.parsePrefixExpression() },
            else => ParserError.InvalidToken,
        };
    }

    fn parseInfixExpressionByToken(self: *Self, lftPtr: *ast.Expression) ParserError!ast.Expression {
        return switch (self.curr_token.token_type) {
            l.TokenType.PLUS, l.TokenType.MINUS, l.TokenType.ASTERISK, l.TokenType.SLASH, l.TokenType.EQ, l.TokenType.NOT_EQ, l.TokenType.LT, l.TokenType.GT => ast.Expression{ .infixExpr = try self.parseInfixExpression(lftPtr) },
            else => ParserError.InvalidToken,
        };
    }

    fn expectPeek(self: *Self, t: l.TokenType) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        } else {
            self.nextError(t);
            return false;
        }
    }

    fn currentTokenIs(self: *Self, t: l.TokenType) bool {
        return self.curr_token.token_type == t;
    }

    fn peekTokenIs(self: *Self, t: l.TokenType) bool {
        return self.peek_token.token_type == t;
    }

    fn nextError(self: *Self, t: l.TokenType) void {
        // print on debug worry about error appends later!
        std.debug.print("Error: Expected {s}, but received {s}\n", .{ @tagName(t), @tagName(self.peek_token.token_type) });
    }
};

fn expectInteger(expected: *const ast.Integer, actual: *const ast.Integer) !void {
    try std.testing.expectEqual(expected.value, actual.value);
}

fn expectIdentifier(expected: *const ast.Identifier, actual: *const ast.Identifier) !void {
    try std.testing.expect(std.mem.eql(u8, expected.value, actual.value));
}

fn expectLetStatement(expected: *const ast.Let, actual: *const ast.Let) !void {
    try expectIdentifier(&expected.name, &actual.name);
}

fn expectReturnStatement(expected: *const ast.Return, actual: *const ast.Return) !void {
    // try expectIdentifier(&expected.name.?, &actual.name.?);
    try expectExpression(expected.value, actual.value);
}

fn expectLetStatementByStatement(expected: *const ast.Let, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .LET => |v| {
            return try expectLetStatement(expected, &v);
        },
        else => {
            try std.testing.expect(false);
        },
    }
}

fn expectReturnStatementByStatement(expected: *const ast.Return, actual: *const ast.Statement) !void {
    switch (actual.*) {
        .RETURN => |v| {
            return try expectReturnStatement(expected, &v);
        },
        else => {
            try std.testing.expect(false);
        },
    }
}

fn expectPrefixExpression(expected: *const ast.PrefixExpression, actual: *const ast.PrefixExpression) anyerror!void {
    try std.testing.expectEqual(expected.*.operator, actual.*.operator);
    try expectExpression(expected.*.right, actual.*.right);
}

fn expectInfixExpression(expected: *const ast.InfixExpression, actual: *const ast.InfixExpression) anyerror!void {
    try expectExpression(expected.*.left, actual.*.left);
    try std.testing.expectEqual(expected.*.operator, actual.*.operator);
    try expectExpression(expected.*.right, actual.*.right);
}

fn expectExpression(expected: *const ast.Expression, actual: *const ast.Expression) !void {
    switch (expected.*) {
        .integer => {
            switch (actual.*) {
                .integer => |integer| try expectInteger(&expected.*.integer, &integer),
                else => {
                    std.debug.print("expected .integer, found {}\n", .{actual});
                    try std.testing.expect(false);
                },
            }
        },
        .identifier => {
            switch (actual.*) {
                .identifier => |identifier| try expectIdentifier(&expected.*.identifier, &identifier),
                else => {
                    std.debug.print("expected .identifier, found {}\n", .{actual});
                    try std.testing.expect(false);
                },
            }
        },
        .prefixExpr => {
            switch (actual.*) {
                .prefixExpr => |prefixExpr| try expectPrefixExpression(&expected.*.prefixExpr, &prefixExpr),
                else => {
                    std.debug.print("expected .prefixExpression, found {}\n", .{actual});
                    try std.testing.expect(false);
                },
            }
        },
        .infixExpr => {
            switch (actual.*) {
                .infixExpr => |infixExpr| try expectInfixExpression(&expected.*.infixExpr, &infixExpr),
                else => {
                    std.debug.print("expected .infixExpression, found {}\n", .{actual});
                    try std.testing.expect(false);
                },
            }
        },
        else => {
            std.debug.print("unsupported {}\n", .{expected});
            try std.testing.expect(false);
        },
    }
}

test "let statements" {
    const input =
        \\ let y = 5;
        \\ let x = 10;
        \\ let x = y;
    ;

    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    var lex = l.Lexer.new(input);
    var parser = Parser.init(allocator.allocator(), &lex);
    var prog: ast.Program = try parser.parseProgram();
    defer prog.deinit();

    var expr1 = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
    var expr2 = ast.Expression{ .integer = ast.Integer{ .value = 10 } };
    var expr3 = ast.Expression{ .identifier = ast.Identifier{ .value = "y" } };

    const expected = [_]ast.Let{
        ast.Let{
            .name = ast.Identifier{ .value = "y" },
            .value = &expr1,
        },
        ast.Let{
            .name = ast.Identifier{ .value = "x" },
            .value = &expr2,
        },
        ast.Let{
            .name = ast.Identifier{ .value = "x" },
            .value = &expr3,
        },
    };

    for (expected, 0..) |exp, idx| {
        try expectLetStatementByStatement(&exp, &prog.statements.items[idx]);
    }
}

test "test return" {
    const input =
        \\ return 5;
        \\ return 10;
        \\ return x;
        \\ return 1 + 1;
    ;
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    var lex = l.Lexer.new(input);
    var parser = Parser.init(allocator.allocator(), &lex);
    var prog: ast.Program = try parser.parseProgram();
    defer prog.deinit();

    var expr1 = ast.Expression{ .integer = ast.Integer{ .value = 5 } };
    var expr2 = ast.Expression{ .integer = ast.Integer{ .value = 10 } };
    var expr3 = ast.Expression{ .identifier = ast.Identifier{ .value = "x" } };
    var expr_4_inner = ast.Expression{ .integer = ast.Integer{ .value = 1 } };
    var expr4 = ast.Expression{ .infixExpr = ast.InfixExpression{
        .left = &expr_4_inner,
        .operator = ast.Operator.plus,
        .right = &expr_4_inner,
    } };

    const expected = [_]ast.Return{
        ast.Return{
            .value = &expr1,
        },
        ast.Return{
            .value = &expr2,
        },
        ast.Return{
            .value = &expr3,
        },
        ast.Return{
            .value = &expr4,
        },
    };

    for (expected, 0..) |exp, idx| {
        try expectReturnStatementByStatement(&exp, &prog.statements.items[idx]);
    }
}
