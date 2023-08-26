const std = @import("std");
const lexer = @import("lexer.zig");
// AST object definitions

pub const Expression = struct {}; // empty struct for now

pub const Identifier = struct {
    token: lexer.Token,
};

// bit of duplication with this, but so be it!
pub const StatementTypes = enum { LET, RETURN };

pub const Statement = union(StatementTypes) {
    LET: Let,
    RETURN: Return,
};

pub const Let = struct {
    name: ?Identifier,
    value: ?Expression,
    token: lexer.Token,
    const Self = @This();

    pub fn print(self: Self) !void {
        // just print to debug for now
        std.debug.print("Token Literal: {s}\tIdentifier name: {s}", .{ self.token.token_type, self.name.token.literal });
    }
};

pub const Return = struct {
    value: ?Expression,
    token: lexer.Token,
    const Self = @This();
    pub fn print(_self: *Self) !void {
        _ = _self;
        // just print to debug for now
        std.debug.print("Return statement");
    }
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
