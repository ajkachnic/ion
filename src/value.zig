const std = @import("std");

const common = @import("common.zig");

pub const ObjectString = struct {
    chars: []const u8,

    pub fn initCopy(src: []const u8) !*Object {
        var object = try common.allocator.create(ObjectString);

        var dest = try common.allocator.alloc(u8, src.len);
        std.mem.copy(dest, src);

        object.chars = dest;

        return object;
    }

    pub fn init(src: []const u8) !*ObjectString {
        var object = try common.allocator.create(ObjectString);

        object.chars = src;

        return object;
    }

    pub fn upcast(self: *ObjectString) Object {
        return Object{ .string = self };
    }

    pub fn format(
        self: *const ObjectString,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("\"", .{});

        for (self.chars) |c| {
            switch (c) {
                '\n' => try writer.print("\\n", .{}),
                '\r' => try writer.print("\\r", .{}),
                '\t' => try writer.print("\\t", .{}),
                else => try writer.print("{c}", .{c}),
            }
        }
        try writer.print("\"", .{});
    }

    pub fn equals(a: *ObjectString, b: *ObjectString) bool {
        return std.mem.eql(u8, a.chars, b.chars);
    }
};

pub const Object = union(Tag) {
    pub const Header = struct {
        next: ?*Object,
    };
    pub const Tag = enum { string };
    string: *ObjectString,

    pub fn equals(a: Object, b: Object) bool {
        switch (a) {
            .string => |av| {
                switch (b) {
                    .string => |bv| return ObjectString.equals(av, bv),
                    // else => return false,
                }
            },
            // else => return false,
        }
    }

    pub fn format(
        self: Object,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .string => |o| try writer.print("{}", .{o}),
        }
    }
};

pub const Value = union(Tag) {
    pub const Tag = enum(u8) { float, int, boolean, null, object };

    float: f64,
    int: i64,
    boolean: bool,
    null: void,
    object: Object,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .float => |f| try writer.print("{d}", .{f}),
            .int => |i| try writer.print("{d}", .{i}),
            .boolean => |b| try writer.print("{}", .{b}),
            .null => try writer.print("null", .{}),
            .object => |o| try writer.print("{}", .{o}),
        }
    }

    pub fn truthy(self: Value) bool {
        return switch (self) {
            .float => true,
            .int => true,
            .boolean => self.boolean,
            .null => false,
            .object => true,
        };
    }

    pub fn typename(self: Value) []const u8 {
        return switch (self) {
            .float => "float",
            .int => "int",
            .boolean => "boolean",
            .null => "null",
            .object => "object",
        };
    }

    pub fn equals(a: Value, b: Value) bool {
        switch (a) {
            .int => |av| switch (b) {
                .int => |bv| return av == bv,
                .float => |bv| return @as(f64, @floatFromInt(av)) == bv,
                else => return false,
            },
            .float => |av| switch (b) {
                .int => |bv| return av == @as(f64, @floatFromInt(bv)),
                .float => |bv| return av == bv,
                else => return false,
            },
            .null => {
                return b == .null;
            },
            .boolean => |av| switch (b) {
                .boolean => |bv| return av == bv,
                else => return false,
            },
            .object => |av| switch (b) {
                .object => |bv| return Object.equals(av, bv),
                else => return false,
            },
        }
    }
};
