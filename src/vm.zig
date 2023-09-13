const std = @import("std");

const bytecode = @import("bytecode.zig");
const common = @import("common.zig");
const value = @import("value.zig");

const Compiler = @import("compiler.zig");

pub const VM = struct {
    chunk: *bytecode.Chunk,
    ip: [*]u8,

    objects: ?*value.Object,

    stack: [common.STACK_MAX]value.Value,
    stackTop: [*]value.Value,

    const Error = error{
        compile_error,
        runtime_error,
    };

    pub fn init() VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
            // .chunk = chunk,
            // .ip = chunk.code.items.ptr,
            .stack = [_]value.Value{undefined} ** common.STACK_MAX,
            .stackTop = undefined,
        };
    }

    pub fn resetStack(self: *VM) void {
        self.stackTop = self.stack[0..].ptr;
    }

    fn push(self: *VM, v: value.Value) void {
        self.stackTop[0] = v;
        self.stackTop += 1;

        if (@intFromPtr(self.stackTop) - @intFromPtr(&self.stack) > common.STACK_MAX) {
            @panic("stack overflow!");
        }
    }

    fn runtimeError(self: *VM, comptime fmt: []const u8, args: anytype) Error!void {
        var stderr = std.io.getStdErr().writer();

        stderr.print("Runtime error: ", .{}) catch {};
        stderr.print(fmt, args) catch {};
        stderr.print("\n", .{}) catch {};

        var instruction = @intFromPtr(self.ip - @intFromPtr(self.chunk.code.items.ptr));
        var line = self.chunk.lines.items[instruction];

        stderr.print("[line {d}] in script\n", .{line}) catch {};

        return Error.runtime_error;
    }

    fn pop(self: *VM) value.Value {
        self.stackTop -= 1;
        return self.stackTop[0];
    }

    pub fn deinit(self: *VM) void {
        _ = self;
        // self.chunk.deinit();
    }

    pub fn interpret(self: *VM, source: []const u8) Error!void {
        self.resetStack();
        var chunk = bytecode.Chunk.init();
        defer chunk.deinit();

        var compiler = Compiler.init();

        try compiler.compile(source, &chunk);

        self.chunk = &chunk;
        self.ip = chunk.code.items.ptr;

        try self.run();
    }

    pub fn run(self: *VM) Error!void {
        while (true) {
            if (common.DEBUG_TRACE_EXECUTION) {
                _ = bytecode.disassembleInstruction(
                    self.chunk.*,
                    @intFromPtr(self.ip - @intFromPtr(self.chunk.code.items.ptr)),
                ) catch {};
                std.debug.print("          [", .{});
                var slot = @as([*]value.Value, self.stack[0..].ptr);
                while (@intFromPtr(slot) < @intFromPtr(self.stackTop)) {
                    std.debug.print(" {} ", .{slot[0]});
                    slot += 1;
                }

                std.debug.print("] \n", .{});
            }

            var instruction = self.readByte();
            switch (@as(bytecode.Opcode, @enumFromInt(instruction))) {
                .pop => {
                    std.debug.print("{}", .{self.pop()});
                },
                .@"return" => {
                    std.debug.print("{}\n", .{self.pop()});
                    break;
                },
                .constant => {
                    var constant = self.readConstant();
                    self.push(constant);
                },

                .add => try self.executeBinary(.add),
                .subtract => try self.executeBinary(.subtract),
                .multiply => try self.executeBinary(.multiply),
                .divide => try self.executeBinary(.divide),
                .negate => try self.executeUnary(.negate),
                .not => try self.executeUnary(.not),

                .equal => try self.executeBinary(.equal),
                .neq => try self.executeBinary(.neq),
                .greater => try self.executeBinary(.greater),
                .less => try self.executeBinary(.less),
                .geq => try self.executeBinary(.geq),
                .leq => try self.executeBinary(.leq),

                .true => self.push(value.Value{ .boolean = true }),
                .false => self.push(value.Value{ .boolean = false }),
                .null => self.push(value.Value.null),
            }
        }
    }

    inline fn executeUnary(self: *VM, comptime op: bytecode.Opcode) !void {
        var a = self.pop();

        switch (op) {
            .negate => {
                switch (a) {
                    .float => |v| self.push(value.Value{ .float = -v }),
                    .int => |v| self.push(value.Value{ .int = -v }),
                    else => self.runtimeError("can't negate type: {s}", .{a.typename()}) catch return Error.runtime_error,
                }
            },

            .not => self.push(value.Value{ .boolean = !a.truthy() }),
            else => unreachable,
        }
    }

    inline fn executeBinary(self: *VM, comptime op: bytecode.Opcode) !void {
        var b = self.pop();
        var a = self.pop();

        if (op == .equal) {
            return self.push(value.Value{ .boolean = a.equals(b) });
        } else if (op == .neq) {
            return self.push(value.Value{ .boolean = !a.equals(b) });
        } else if (op == .add) {
            // if (@as(value.Value.Tag, a) == .object and @as(value.Object.Tag, a.object) == .string) {
            //     if (@as(value.Value.Tag, b) == .object and @as(value.Object.Tag, b.object) == .string) {
            //
            //     }
            // }
        }

        switch (b) {
            .int => |bv| {
                switch (a) {
                    .int => |av| try self.executeBinaryInt(op, av, bv),
                    .float => |av| try self.executeBinaryFloat(op, av, @floatFromInt(bv)),
                    else => try self.runtimeError(
                        "unsupported binary operator: {} for types: {s} {s}",
                        .{ op, a.typename(), b.typename() },
                    ),
                }
            },
            .float => |bv| {
                switch (a) {
                    .int => |av| try self.executeBinaryFloat(op, @floatFromInt(av), bv),
                    .float => |av| try self.executeBinaryFloat(op, av, bv),

                    else => try self.runtimeError(
                        "unsupported binary operator: {} for types: {s} {s}",
                        .{ op, a.typename(), b.typename() },
                    ),
                }
            },
            else => try self.runtimeError(
                "unsupported binary operator: {} for types: {s} {s}",
                .{ op, a.typename(), b.typename() },
            ),
        }
    }

    inline fn executeBinaryInt(
        self: *VM,
        comptime op: bytecode.Opcode,
        a: i64,
        b: i64,
    ) Error!void {
        switch (op) {
            .add => self.push(value.Value{ .int = a + b }),
            .subtract => self.push(value.Value{ .int = a - b }),
            .multiply => self.push(value.Value{ .int = a * b }),
            .divide => {
                if (b == 0) {
                    try self.runtimeError("division by zero", .{});
                }
                self.push(value.Value{ .int = @divFloor(a, b) });
            },

            .greater => self.push(value.Value{ .boolean = a > b }),
            .less => self.push(value.Value{ .boolean = a < b }),
            .geq => self.push(value.Value{ .boolean = a >= b }),
            .leq => self.push(value.Value{ .boolean = a <= b }),

            else => unreachable,
        }
    }

    inline fn executeBinaryFloat(
        self: *VM,
        comptime op: bytecode.Opcode,
        a: f64,
        b: f64,
    ) Error!void {
        switch (op) {
            .add => self.push(value.Value{ .float = a + b }),
            .subtract => self.push(value.Value{ .float = a - b }),
            .multiply => self.push(value.Value{ .float = a * b }),
            .divide => {
                if (b == 0.0) {
                    try self.runtimeError("division by zero", .{});
                }
                self.push(value.Value{ .float = a / b });
            },

            .greater => self.push(value.Value{ .boolean = a > b }),
            .less => self.push(value.Value{ .boolean = a < b }),
            .geq => self.push(value.Value{ .boolean = a >= b }),
            .leq => self.push(value.Value{ .boolean = a <= b }),

            else => unreachable,
        }
    }

    pub fn readByte(self: *VM) u8 {
        var byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    pub fn readConstant(self: *VM) value.Value {
        return self.chunk.constants.items[self.readByte()];
    }
};
