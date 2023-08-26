const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    // identifiers, literals
    IDENT,
    INT,
    // operators
    ASSIGN,
    PLUS,
    BANG,
    MINUS,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NOT_EQ,

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
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
};

pub const Token = struct {
    token_type: TokenType,
    literal: []const u8,
};

fn isLetter(ch: u8) bool {
    return (((ch >= 'a') and (ch <= 'z')) or ((ch >= 'A') and (ch <= 'Z')) or ch == '_');
}

fn isDigit(ch: u8) bool {
    return (ch >= '0') and (ch <= '9');
}

const KKV = struct { []const u8, TokenType };
const Keywords = std.ComptimeStringMap(TokenType, [_]KKV{
    .{ "fn", TokenType.FUNCTION },
    .{ "let", TokenType.LET },
    .{ "if", TokenType.IF },
    .{ "else", TokenType.ELSE },
    .{ "true", TokenType.TRUE },
    .{ "false", TokenType.FALSE },
    .{ "return", TokenType.RETURN },
});

fn lookupIdent(ident: []const u8) TokenType {
    return Keywords.get(ident) orelse TokenType.IDENT;
}

pub const Lexer = struct {
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

    fn readIdentifier(self: *Self) []const u8 {
        const posn = self.position;
        while (isLetter(self.input[self.position])) {
            self.readChar();
        }
        return self.input[posn..self.position];
    }

    fn readNumber(self: *Self) []const u8 {
        const posn = self.position;
        while (isDigit(self.input[self.position])) {
            self.readChar();
        }
        return self.input[posn..self.position];
    }

    fn skipWhiteSpace(self: *Self) void {
        while ((self.ch == ' ') or (self.ch == '\n') or (self.ch == '\t') or (self.ch == '\r')) {
            self.readChar();
        }
    }

    pub fn new(input: []const u8) Lexer {
        var tmp = Self{ .input = input, .position = 0, .read_position = 0, .ch = null };
        tmp.readChar();
        return tmp;
    }

    pub fn nextToken(self: *Self) Token {
        var tok: Token = undefined;

        // std.debug.print("Positions: {d}\t{d}\n", .{ self.position, self.read_position });
        self.skipWhiteSpace();
        if (self.ch) |c| {
            switch (c) {
                '=' => {
                    if (self.peekChar() == '=') {
                        // const ch = self.ch;
                        self.readChar();
                        tok = Token{ .token_type = TokenType.EQ, .literal = "==" };
                    } else {
                        tok = Token{ .token_type = TokenType.ASSIGN, .literal = "=" };
                    }
                },
                '+' => {
                    tok = Token{ .token_type = TokenType.PLUS, .literal = "+" };
                },
                '-' => {
                    tok = Token{ .token_type = TokenType.MINUS, .literal = "-" };
                },
                '!' => {
                    if (self.peekChar() == '=') {
                        self.readChar();
                        tok = Token{ .token_type = TokenType.NOT_EQ, .literal = "!=" };
                    } else {
                        tok = Token{ .token_type = TokenType.BANG, .literal = "!" };
                    }
                },
                '*' => {
                    tok = Token{ .token_type = TokenType.ASTERISK, .literal = "*" };
                },
                '/' => {
                    tok = Token{ .token_type = TokenType.SLASH, .literal = "/" };
                },
                '<' => {
                    tok = Token{ .token_type = TokenType.LT, .literal = "<" };
                },
                '>' => {
                    tok = Token{ .token_type = TokenType.GT, .literal = ">" };
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
                '{' => {
                    tok = Token{ .token_type = TokenType.LBRACE, .literal = "{" };
                },
                '}' => {
                    tok = Token{ .token_type = TokenType.RBRACE, .literal = "}" };
                },
                else => {
                    if (isLetter(c)) {
                        const tok_literal = self.readIdentifier();
                        const tok_type = lookupIdent(tok_literal);
                        return Token{ .token_type = tok_type, .literal = tok_literal };
                    } else if (isDigit(c)) {
                        const tok_literal = self.readNumber();
                        return Token{ .token_type = TokenType.INT, .literal = tok_literal };
                    } else {
                        tok = Token{ .token_type = TokenType.ILLEGAL, .literal = "" };
                    }
                },
            }
            self.readChar();
            return tok;
        } else {
            return Token{ .token_type = TokenType.EOF, .literal = "" };
        }
    }
};

test "Lexer Test Next Token Random" {
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

test "Lexer Test Next Token let bind" {
    const input =
        \\let five = 5;
        \\ let ten = 10;
        \\let add = fn(x, y) {
        \\ x + y;
        \\};
        \\
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5 ;
        \\
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\
        \\ 10 == 10;
        \\ 10 != 9;
    ;

    var code = Lexer.new(input);

    const expected = [_]Token{
        Token{ .token_type = TokenType.LET, .literal = "let" },
        Token{ .token_type = TokenType.IDENT, .literal = "five" },
        Token{ .token_type = TokenType.ASSIGN, .literal = "=" },
        Token{ .token_type = TokenType.INT, .literal = "5" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        Token{ .token_type = TokenType.LET, .literal = "let" },
        Token{ .token_type = TokenType.IDENT, .literal = "ten" },
        Token{ .token_type = TokenType.ASSIGN, .literal = "=" },
        Token{ .token_type = TokenType.INT, .literal = "10" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        Token{ .token_type = TokenType.LET, .literal = "let" },
        Token{ .token_type = TokenType.IDENT, .literal = "add" },
        Token{ .token_type = TokenType.ASSIGN, .literal = "=" },
        Token{ .token_type = TokenType.FUNCTION, .literal = "fn" },
        Token{ .token_type = TokenType.LPAREN, .literal = "(" },
        Token{ .token_type = TokenType.IDENT, .literal = "x" },
        Token{ .token_type = TokenType.COMMA, .literal = "," },
        Token{ .token_type = TokenType.IDENT, .literal = "y" },
        Token{ .token_type = TokenType.RPAREN, .literal = ")" },
        Token{ .token_type = TokenType.LBRACE, .literal = "{" },
        Token{ .token_type = TokenType.IDENT, .literal = "x" },
        Token{ .token_type = TokenType.PLUS, .literal = "+" },
        Token{ .token_type = TokenType.IDENT, .literal = "y" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },
        Token{ .token_type = TokenType.RBRACE, .literal = "}" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        Token{ .token_type = TokenType.LET, .literal = "let" },
        Token{ .token_type = TokenType.IDENT, .literal = "result" },
        Token{ .token_type = TokenType.ASSIGN, .literal = "=" },
        Token{ .token_type = TokenType.IDENT, .literal = "add" },
        Token{ .token_type = TokenType.LPAREN, .literal = "(" },
        Token{ .token_type = TokenType.IDENT, .literal = "five" },
        Token{ .token_type = TokenType.COMMA, .literal = "," },
        Token{ .token_type = TokenType.IDENT, .literal = "ten" },
        Token{ .token_type = TokenType.RPAREN, .literal = ")" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        Token{ .token_type = TokenType.BANG, .literal = "!" },
        Token{ .token_type = TokenType.MINUS, .literal = "-" },
        Token{ .token_type = TokenType.SLASH, .literal = "/" },
        Token{ .token_type = TokenType.ASTERISK, .literal = "*" },
        Token{ .token_type = TokenType.INT, .literal = "5" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        //5 < 10 > 5 ;
        Token{ .token_type = TokenType.INT, .literal = "5" },
        Token{ .token_type = TokenType.LT, .literal = "<" },
        Token{ .token_type = TokenType.INT, .literal = "10" },
        Token{ .token_type = TokenType.GT, .literal = ">" },
        Token{ .token_type = TokenType.INT, .literal = "5" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        Token{ .token_type = TokenType.IF, .literal = "if" },
        Token{ .token_type = TokenType.LPAREN, .literal = "(" },
        Token{ .token_type = TokenType.INT, .literal = "5" },
        Token{ .token_type = TokenType.LT, .literal = "<" },
        Token{ .token_type = TokenType.INT, .literal = "10" },
        Token{ .token_type = TokenType.RPAREN, .literal = ")" },
        Token{ .token_type = TokenType.LBRACE, .literal = "{" },
        Token{ .token_type = TokenType.RETURN, .literal = "return" },
        Token{ .token_type = TokenType.TRUE, .literal = "true" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },
        Token{ .token_type = TokenType.RBRACE, .literal = "}" },
        Token{ .token_type = TokenType.ELSE, .literal = "else" },
        Token{ .token_type = TokenType.LBRACE, .literal = "{" },
        Token{ .token_type = TokenType.RETURN, .literal = "return" },
        Token{ .token_type = TokenType.FALSE, .literal = "false" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },
        Token{ .token_type = TokenType.RBRACE, .literal = "}" },

        // 10 == 10;
        Token{ .token_type = TokenType.INT, .literal = "10" },
        Token{ .token_type = TokenType.EQ, .literal = "==" },
        Token{ .token_type = TokenType.INT, .literal = "10" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        // 10 != 9;
        Token{ .token_type = TokenType.INT, .literal = "10" },
        Token{ .token_type = TokenType.NOT_EQ, .literal = "!=" },
        Token{ .token_type = TokenType.INT, .literal = "9" },
        Token{ .token_type = TokenType.SEMICOLON, .literal = ";" },

        Token{ .token_type = TokenType.EOF, .literal = "" },
    };
    for (expected) |exp| {
        const t = code.nextToken();
        // std.debug.print("Actual: {s}\tExpected:{s}\n", .{ @tagName(t.token_type), @tagName(exp.token_type) });
        // std.debug.print("Actual: {s}\tExpected:{s}\n", .{ t.literal, exp.literal });
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
