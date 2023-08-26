const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Welcome to MonkeyLang!\n ", .{});

    var buffer: [4096]u8 = undefined;

    while (true) {
        try stdout.print(">> ", .{});

        const input = try stdin.readUntilDelimiterOrEof(&buffer, '\n');

        if (input) |inp| {
            var lex = lexer.Lexer.new(inp);
            var next_token: lexer.Token = lex.nextToken();
            while (next_token.token_type != lexer.TokenType.EOF) : (next_token = lex.nextToken()) {
                try stdout.print("Type: {s}\tLiteral: {s}\n", .{ @tagName(next_token.token_type), next_token.literal });
            }
        } else {
            break;
        }
    }
}
