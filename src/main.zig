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

test "Arraylist writer" {
    var tmp = std.ArrayList(u8).init(std.testing.allocator);
    defer tmp.deinit();
    const writer = tmp.writer();

    _ = try writer.write("123456789");
    for (0..9) |i| {
        const intt: u8 = @intCast(i);
        const char_value: u8 = '1' + intt;
        std.debug.print("{d}\n", .{tmp.items[i]});
        std.debug.print("{d}\n", .{char_value});
        try std.testing.expect(tmp.items[i] == char_value);
    }
}
