const ast = @import("ast.zig");
const std = @import("std");
const l = @import("lexer.zig");

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
        // peek token will point at 1th index in lexer output
        parser.nextToken();
        parser.nextToken();
        return parser;
    }

    fn parseProgram(self: *Self) !ast.Program {
        var prog = ast.Program.init(self.allocator);
        while (self.curr_token.token_type != l.TokenType.EOF) {
            var stmt = self.parseStatement();
            if (stmt) |s| {
                try prog.statements.append(s);
            }
            self.nextToken();
        }
        return prog;
    }

    fn nextToken(self: *Self) void {
        self.curr_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
        // std.debug.print("Current: {s}\tPeek: {s}", .{ self.curr_token.literal, self.peek_token.literal });
    }

    fn parseStatement(self: *Self) ?ast.Statement {
        return switch (self.curr_token.token_type) {
            l.TokenType.LET => self.parseLetStatement(),
            l.TokenType.RETURN => self.parseReturnStatement(),
            else => null,
        };
    }

    fn parseReturnStatement(self: *Self) ?ast.Statement {
        var statement = ast.Return{ .token = l.Token{ .token_type = l.TokenType.RETURN, .literal = "return" }, .value = null };
        if (!self.expectPeek(l.TokenType.IDENT)) {
            return null;
        }
        return ast.Statement{
            .RETURN = statement,
        };
    }

    fn parseLetStatement(self: *Self) ?ast.Statement {
        var statement = ast.Let{
            .token = self.curr_token,
            .name = null,
            .value = null,
        };
        // next has to be identifier
        if (!self.expectPeek(l.TokenType.IDENT)) {
            return null;
        }
        statement.name = ast.Identifier{
            .token = self.curr_token,
        };
        // next has to be equals sign
        if (!self.expectPeek(l.TokenType.ASSIGN)) {
            return null;
        }
        return ast.Statement{
            .LET = statement,
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

test "let statements" {
    const input =
        \\ let x = 5;
        \\ let y = 5;
    ;

    var lex = l.Lexer.new(input);
    var parser = Parser.init(std.testing.allocator, &lex);
    var prog: ast.Program = try parser.parseProgram();
    defer prog.deinit();

    const expected = [_]l.Token{
        l.Token{ .token_type = l.TokenType.IDENT, .literal = "x" },
        l.Token{ .token_type = l.TokenType.IDENT, .literal = "y" },
    };

    for (expected, 0..) |ident, idx| {
        const stmt = prog.statements.items[idx];
        switch (stmt) {
            .LET => |v| {
                try std.testing.expect(v.name.?.token.token_type == ident.token_type);
                try std.testing.expect(std.mem.eql(u8, v.name.?.token.literal, ident.literal));
            },
            .RETURN => {
                try std.testing.expect(false);
            },
        }
    }
}

test "return statements" {
    const input =
        \\ return x;
    ;

    var lex = l.Lexer.new(input);
    var parser = Parser.init(std.testing.allocator, &lex);
    var prog: ast.Program = try parser.parseProgram();
    defer prog.deinit();

    const expected = [_]l.Token{
        l.Token{ .token_type = l.TokenType.RETURN, .literal = "return" },
    };

    for (expected, 0..) |ret, idx| {
        const stmt = prog.statements.items[idx];
        switch (stmt) {
            .LET => {
                try std.testing.expect(false);
            },
            .RETURN => |v| {
                try std.testing.expect(v.token.token_type == ret.token_type);
            },
        }
    }
}
