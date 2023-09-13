const std = @import("std");

const common = @import("common.zig");
const value = @import("value.zig");

pub const Constant = u8;

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(value.Value),
    lines: std.ArrayList(u32),

    pub fn init() Chunk {
        return Chunk{
            .code = std.ArrayList(u8).init(common.allocator),
            .constants = std.ArrayList(value.Value).init(common.allocator),
            .lines = std.ArrayList(u32).init(common.allocator),
        };
    }

    pub fn deinit(self: Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn writeByte(self: *Chunk, line: u32, byte: u8) void {
        self.code.append(byte) catch common.oom();
        self.lines.append(line) catch common.oom();
    }

    pub fn write(self: *Chunk, line: u32, opcode: Opcode, args: anytype) void {
        self.writeByte(line, @intFromEnum(opcode));

        inline for (args) |arg| {
            self.writeByte(line, arg);
        }
    }

    pub fn addConstant(self: *Chunk, v: value.Value) Constant {
        self.constants.append(v) catch common.oom();
        return @intCast(self.constants.items.len - 1);
    }
};

pub const Opcode = enum {
    pop,
    @"return",
    constant,

    true,
    false,
    null,
    equal,
    neq,
    greater,
    less,
    leq,
    geq,

    add,
    subtract,
    multiply,
    divide,
    negate,
    not,
};

pub fn disassembleChunk(chunk: Chunk, name: []const u8) !void {
    const stdout = std.io.getStdOut().writer();

    try stdout.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = try disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: Chunk, offset: usize) !usize {
    const stdout = std.io.getStdOut().writer();

    try stdout.print("{d:0>4} ", .{offset});

    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        try stdout.print("   | ", .{});
    } else {
        try stdout.print("{d: >4} ", .{chunk.lines.items[offset]});
    }

    var instruction: u8 = chunk.code.items[offset];
    return switch (@as(Opcode, @enumFromInt(instruction))) {
        .constant => constantInstruction("OP_CONSTANT", chunk, offset),
        .pop => simpleInstruction("OP_POP", offset),
        .@"return" => simpleInstruction("OP_RETURN", offset),

        .true => simpleInstruction("OP_TRUE", offset),
        .false => simpleInstruction("OP_FALSE", offset),
        .null => simpleInstruction("OP_NULL", offset),

        .equal => simpleInstruction("OP_EQUAL", offset),
        .neq => simpleInstruction("OP_NEQ", offset),
        .greater => simpleInstruction("OP_GREATER", offset),
        .less => simpleInstruction("OP_LESS", offset),
        .leq => simpleInstruction("OP_LEQ", offset),
        .geq => simpleInstruction("OP_GEQ", offset),

        .add => simpleInstruction("OP_ADD", offset),
        .subtract => simpleInstruction("OP_SUBTRACT", offset),
        .multiply => simpleInstruction("OP_MULTIPLY", offset),
        .divide => simpleInstruction("OP_DIVIDE", offset),
        .negate => simpleInstruction("OP_NEGATE", offset),
        .not => simpleInstruction("OP_NOT", offset),
    };
}

fn simpleInstruction(name: []const u8, offset: usize) !usize {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: Chunk, offset: usize) !usize {
    const stdout = std.io.getStdOut().writer();

    var constant: u8 = chunk.code.items[offset + 1];
    try stdout.print(
        "{s: <16} {d:4} '{}'\n",
        .{ name, constant, chunk.constants.items[constant] },
    );
    return offset + 2;
}
