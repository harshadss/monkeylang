const std = @import("std");

const TokenType = enum {
    ILLEGAL,
    EOF,
    // identifiers, literals
    IDENT,
    INT,
    // operators
    ASSIGN,
    PLUS,
    // delimiter
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    //keywords
    FUNCTION,
    LET,
};

const Token = struct {
    token_type: TokenType,
    literal: []const u8,
};

const Lexer = struct {
    input: []const u8, // string input code
    position: usize,
    read_position: usize,
    ch: ?u8,

    const Self = @This();

    fn peekChar(self: *Self) ?u8 {
        if (self.read_position >= self.input.len) {
            return null;
        } else {
            return self.input[self.read_position];
        }
    }

    fn readChar(self: *Self) void {
        self.ch = self.peekChar();
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn new(input: []const u8) Lexer {
        var tmp = Self{ .input = input, .position = 0, .read_position = 0, .ch = null };
        tmp.readChar();
        return tmp;
    }

    pub fn nextToken(self: *Self) Token {
        var tok: Token = undefined;
        defer self.readChar();
        // std.debug.print("Positions: {d}\t{d}\n", .{ self.position, self.read_position });
        if (self.ch) |c| {
            switch (c) {
                '=' => {
                    tok = Token{ .token_type = TokenType.ASSIGN, .literal = "=" };
                },
                ';' => {
                    tok = Token{ .token_type = TokenType.SEMICOLON, .literal = ";" };
                },
                '(' => {
                    tok = Token{ .token_type = TokenType.LPAREN, .literal = "(" };
                },
                ')' => {
                    tok = Token{ .token_type = TokenType.RPAREN, .literal = ")" };
                },
                ',' => {
                    tok = Token{ .token_type = TokenType.COMMA, .literal = "," };
                },
                '+' => {
                    tok = Token{ .token_type = TokenType.PLUS, .literal = "+" };
                },
                '{' => {
                    tok = Token{ .token_type = TokenType.LBRACE, .literal = "{" };
                },
                '}' => {
                    tok = Token{ .token_type = TokenType.RBRACE, .literal = "}" };
                },
                else => {
                    tok = Token{ .token_type = TokenType.ILLEGAL, .literal = "" };
                },
            }
            return tok;
        } else {
            return Token{ .token_type = TokenType.EOF, .literal = "" };
        }
    }
};

test "Lexer Test Next Token" {
    const input = "=+(){};";

    var code = Lexer.new(input);

    const expected = [_]Token{
        Token{ .token_type = TokenType.ASSIGN, .literal = "=" },
        Token{ .token_type = TokenType.PLUS, .literal = "+" },
        Token{ .token_type = TokenType.LPAREN, .literal = "(" },
        Token{ .token_type = TokenType.RPAREN, .literal = ")" },
        Token{ .token_type = TokenType.LBRACE, .literal = "{" },
        Token{ .token_type = TokenType.RBRACE, .literal = "}" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },
    };
    for (expected) |exp| {
        const t = code.nextToken();
        // std.debug.print("Actual: {s}\t{s}\n", .{ @tagName(t.token_type), t.literal });
        // std.debug.print("Expected: {s}\t{s}\n", .{ @tagName(exp.token_type), exp.literal });
        try std.testing.expect(exp.token_type == t.token_type);
        try std.testing.expect(std.mem.eql(u8, exp.literal, t.literal));
    }
}

test "Empty input returns EOF" {
    const input = "";
    var code = Lexer.new(input);
    const t = code.nextToken();
    try std.testing.expect(t.token_type == TokenType.EOF);
}
