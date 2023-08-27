const std = @import("std");
const lexer = @import("lexer.zig");
// AST object definitions

pub const Operator = enum {
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    equal,
    notEqual,
    gt,
    lt,
};

pub const Expression = union(enum) {
    identifier: Identifier,
    prefixExpr: PrefixExpression,
    infixExpr: InfixExpression,
    integer: Integer,
    boolean: Boolean,
};

pub const PrefixExpression = struct {
    operator: Operator,
    right: *const Expression,
};

pub const InfixExpression = struct {
    left: *const Expression,
    operator: Operator,
    right: *const Expression,
};

pub const Integer = struct {
    value: i64,
};

pub const Boolean = struct {
    value: bool,
};

pub const Identifier = struct {
    value: []const u8,
};

// bit of duplication with this, but so be it!
pub const StatementTypes = enum { LET, RETURN, EXPRESSION_STATEMENT };

pub const Statement = union(StatementTypes) {
    LET: Let,
    RETURN: Return,
    EXPRESSION_STATEMENT: ExpressionStatement,
};

pub const Let = struct {
    name: Identifier,
    value: *Expression,
    const Self = @This();

    pub fn print(self: Self, buf: anytype) !void {
        // just print to debug for now
        try buf.writeAll(self.token.literal);
        try buf.writeAll(self.name.?.token.literal);
        try buf.writeAll(" = ");
        try buf.writeAll(" <some value> ;\n");
    }
};

pub const Return = struct {
    value: *Expression,
    const Self = @This();
    pub fn print(self: *Self, buf: anytype) !void {
        try buf.writeAll(self.token.literal);
        try buf.writeAll("<some value>;\n");
    }
};

pub const ExpressionStatement = struct {
    token: lexer.Token,
    expression: Expression,
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: std.ArrayList(Statement),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Program {
        return Self{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }
    pub fn deinit(self: *Self) void {
        self.statements.deinit();
    }
};
