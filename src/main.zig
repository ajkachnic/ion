const std = @import("std");

const token = @import("./token.zig");

const INPUT = "a := 5";

pub fn main() !void {
    try repl();
}

fn repl() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = allocator.deinit();

    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut();

    var buffer = std.ArrayList(u8).init(allocator.allocator());
    defer buffer.deinit();

    while (true) {
        _ = try stdout.write(">> ");
        var start = buffer.items.len;
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null);

        var input = buffer.items[start..];

        var tokens = std.ArrayList(token.Token).init(allocator.allocator());
        defer tokens.deinit();

        var tokenizer = token.Tokenizer.init(input, null);
        try tokenizer.tokenize(&tokens);

        for (tokens.items) |tok| {
            std.debug.print("{}\n", .{tok});
        }
    }
}
