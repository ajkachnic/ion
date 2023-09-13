const builtin = @import("builtin");
const std = @import("std");

// Global allocator
pub var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub const allocator = gpa.allocator();

pub fn deinit() void {
    gpa.deinit();
}

pub fn oom() noreturn {
    @panic("out of memory!");
}

// Debug flags
pub const DEBUG_TRACE_EXECUTION = true;
pub const DEBUG_PRINT_CODE = true;

// Parameters for the VM
//
// Many of these are limited by things like operand sizes
pub const STACK_MAX = 256;
