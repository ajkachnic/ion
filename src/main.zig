const std = @import("std");

const bytecode = @import("bytecode.zig");
const common = @import("common.zig");
const token = @import("token.zig");
const value = @import("value.zig");
const vm = @import("vm.zig");

const INPUT = "a := 5";

pub fn main() !void {
    var engine = vm.VM.init();
    defer engine.deinit();

    // engine.interpret("!(5 - 4 > 3 * 2 == !null)") catch {
    //     std.os.exit(1);
    // };
    //
    try repl();
}

fn repl() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = allocator.deinit();

    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut();

    var buffer = std.ArrayList(u8).init(allocator.allocator());
    defer buffer.deinit();

    var engine = vm.VM.init();
    defer engine.deinit();

    while (true) {
        _ = try stdout.write("\x1b[35m|>\x1b[0m ");
        var start = buffer.items.len;
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null);

        var input = buffer.items[start..];

        engine.interpret(input) catch {};
    }
}
