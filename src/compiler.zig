const std = @import("std");

const bytecode = @import("bytecode.zig");
const common = @import("common.zig");
const token = @import("token.zig");
const value = @import("value.zig");

const Compiler = @This();

const Precedence = enum {
    none,
    assignment,
    @"or",
    @"and",
    equality,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,
};

tokens: std.ArrayList(token.Token),
compilingChunk: *bytecode.Chunk,
index: usize = 0,

current: token.Token,
previous: token.Token,

pub fn init() Compiler {
    return Compiler{
        .tokens = undefined,
        .compilingChunk = undefined,
        .index = 0,
        .current = undefined,
        .previous = undefined,
    };
}

pub fn deinit(self: *Compiler) void {
    self.tokens.deinit();
}

fn currentChunk(self: *Compiler) *bytecode.Chunk {
    return self.compilingChunk;
}

fn errorAtCurrent(self: *Compiler, message: []const u8) !void {
    try self.errorAt(self.current, message);
}

fn errorAt(self: *Compiler, tok: token.Token, message: []const u8) !void {
    _ = self;
    const stderr = std.io.getStdErr().writer();

    stderr.print("{} Error", .{tok.pos}) catch {};

    if (tok.kind == .eof) {
        _ = stderr.write(" at end") catch {};
    } else {
        stderr.print(" at {s}", .{tok.string()}) catch {};
    }
    stderr.print(": {s}\n", .{message}) catch {};

    return error.compile_error;
}

fn isEOF(self: *Compiler) bool {
    return self.current.kind == .eof;
}

fn advance(self: *Compiler) void {
    self.previous = self.current;

    if (self.index + 1 < self.tokens.items.len) {
        self.index += 1;
        self.current = self.tokens.items[self.index];
    } else {
        self.current = token.Token{ .kind = .eof, .pos = self.current.pos };
    }
}

fn consume(self: *Compiler, kind: token.Kind, message: []const u8) !void {
    if (self.current.kind == kind) {
        self.advance();
        return;
    }

    return self.errorAtCurrent(message);
}

fn consumePrevious(self: *Compiler, kind: token.Kind, message: []const u8) !void {
    if (self.previous.kind == kind) {
        self.advance();
        return;
    }

    return self.errorAtCurrent(message);
}

fn emitConstant(self: *Compiler, v: value.Value) void {
    self.emit(.constant, .{self.makeConstant(v)});
}

fn makeConstant(self: *Compiler, v: value.Value) bytecode.Constant {
    var constant = self.currentChunk().addConstant(v);
    return constant;
}

fn emitByte(self: *Compiler, byte: u8) void {
    self.currentChunk().writeByte(self.previous.pos.line, byte);
}

fn emitBytes(self: *Compiler, bytes: anytype) void {
    inline for (bytes) |byte| {
        self.emitByte(byte);
    }
}

fn emit(self: *Compiler, opcode: bytecode.Opcode, bytes: anytype) void {
    self.currentChunk().write(self.previous.pos.line, opcode, bytes);
}

fn debugTokens(self: *Compiler, message: []const u8) void {
    std.debug.print("{s} ({}, {})\n", .{ message, self.previous, self.current });
}

pub fn compile(self: *Compiler, source: []const u8, chunk: *bytecode.Chunk) !void {
    var tokenizer = token.Tokenizer.init(source, null);
    var tokens = std.ArrayList(token.Token).init(common.allocator);

    defer tokens.deinit();

    tokenizer.tokenize(&tokens);

    std.debug.print("tokens: {any}\n", .{tokens.items});

    self.tokens = tokens;
    self.compilingChunk = chunk;

    self.current = self.tokens.items[0];
    self.index = 0;

    while (!self.isEOF()) {
        try self.parseExpression(false);

        try self.consume(.comma, "Expected ',' after expression");
    }

    self.endCompiler();
}

fn endCompiler(self: *Compiler) void {
    self.emit(.@"return", .{});

    if (common.DEBUG_PRINT_CODE) {
        bytecode.disassembleChunk(self.currentChunk().*, "code") catch {};
    }
}

fn parseGrouping(self: *Compiler) !void {
    try self.parseExpression(false);
    try self.consume(.rightParen, "Expect ')' after expression");
}

fn parseBinary(self: *Compiler) !void {
    var op = self.previous.kind;
    var rule = getRule(op);
    var prec = @intFromEnum(rule.precedence) + 1;
    if (prec > @intFromEnum(Precedence.call)) {
        prec = @intFromEnum(Precedence.call);
    }

    try self.parsePrecendence(@enumFromInt(prec));

    switch (op) {
        .plus => self.emit(.add, .{}),
        .minus => self.emit(.subtract, .{}),
        .times => self.emit(.multiply, .{}),
        .divide => self.emit(.divide, .{}),

        .eq => self.emit(.equal, .{}),
        .neq => self.emit(.neq, .{}),
        .less => self.emit(.less, .{}),
        .leq => self.emit(.leq, .{}),
        .greater => self.emit(.greater, .{}),
        .geq => self.emit(.geq, .{}),

        else => @panic("invariant: invalid binary operator"),
    }
}

fn parseExpression(self: *Compiler, top_level: bool) !void {
    try self.parsePrecendence(.assignment);
    if (top_level) {
        self.emit(.pop, .{});
    }
}

fn parseUnary(self: *Compiler) !void {
    var op = self.previous.kind;

    try self.parsePrecendence(.unary);

    switch (op) {
        .minus => self.emit(.negate, .{}),
        .not => self.emit(.not, .{}),
        else => @panic("invariant: invalid unary operator"),
    }
}

fn parsePrecendence(self: *Compiler, precedence: Precedence) ParseError!void {
    self.advance();
    var prefixRule = getRule(self.previous.kind).prefix orelse {
        return self.errorAtCurrent("Expected expression");
    };

    try prefixRule(self);

    while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.kind).precedence)) {
        self.advance();
        var infixRule = getRule(self.previous.kind).infix orelse {
            return self.errorAtCurrent("Expected expression");
        };

        try infixRule(self);
    }

    // self.advance();
}

fn parseUnit(self: *Compiler) !void {
    switch (self.previous.kind) {
        .numberLiteral => self.parseNumber(self.previous),
        .stringLiteral => self.parseString(self.previous),
    }
}

// parseString generates a string constant, while fixing all the escapes used in the string
fn parseString(self: *Compiler) ParseError!void {
    var tok = self.previous;
    // TODO: garbage collection will fix memory leak here
    var v = std.ArrayList(u8).init(common.allocator);

    var i: usize = 0;
    while (i < tok.payload.?.len) : (i += 1) {
        var ch = tok.payload.?[i];
        if (ch == '\\') {
            if (i + 1 >= tok.payload.?.len) {
                break;
            }

            i += 1;
            ch = tok.payload.?[i];
            switch (ch) {
                't' => v.append('\t') catch common.oom(),
                'n' => v.append('\n') catch common.oom(),
                'r' => v.append('\r') catch common.oom(),
                'x' => {
                    if (i + 2 >= tok.payload.?.len) {
                        v.append('x') catch common.oom();
                        continue;
                    }

                    var hex = std.fmt.parseUnsigned(
                        u8,
                        tok.payload.?[i + 1 .. i + 2],
                        16,
                    ) catch {
                        v.append('x') catch common.oom();
                        continue;
                    };
                    v.append(hex) catch common.oom();
                },
                else => v.append(ch) catch common.oom(),
            }
        } else {
            v.append(ch) catch common.oom();
        }
    }

    var obj = value.ObjectString.init(v.toOwnedSlice() catch common.oom()) catch common.oom();

    self.emitConstant(value.Value{ .object = obj.upcast() });
}

fn parseLiteral(self: *Compiler) ParseError!void {
    switch (self.previous.kind) {
        .falseLiteral => self.emit(.false, .{}),
        .trueLiteral => self.emit(.true, .{}),
        .nullLiteral => self.emit(.null, .{}),
        else => @panic("invariant: invalid literal"),
    }
}

fn parseNumber(self: *Compiler) ParseError!void {
    var tok = self.previous;
    var payload = tok.payload.?;

    if (std.mem.containsAtLeast(u8, payload, 1, ".")) {
        var f = std.fmt.parseFloat(f64, payload) catch {
            return self.errorAt(tok, "failed to parse float");
        };

        self.emitConstant(value.Value{ .float = f });
    } else {
        var i = std.fmt.parseInt(i64, payload, 10) catch {
            return self.errorAt(tok, "failed to parse int");
        };
        self.emitConstant(value.Value{ .int = i });
    }
}

const ParseError = error{compile_error};
const ParseFn = *const fn (*Compiler) ParseError!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,

    pub fn rule(prefix: ?ParseFn, infix: ?ParseFn, prec: Precedence) ParseRule {
        return ParseRule{
            .prefix = prefix,
            .infix = infix,
            .precedence = prec,
        };
    }
};

fn getRule(op: token.Kind) ParseRule {
    return switch (op) {
        .leftParen => ParseRule.rule(parseGrouping, null, .none),

        .minus => ParseRule.rule(parseUnary, parseBinary, .term),
        .plus => ParseRule.rule(null, parseBinary, .term),
        .divide => ParseRule.rule(null, parseBinary, .factor),
        .times => ParseRule.rule(null, parseBinary, .factor),
        .not => ParseRule.rule(parseUnary, null, .none),

        .eq => ParseRule.rule(null, parseBinary, .equality),
        .neq => ParseRule.rule(null, parseBinary, .equality),
        .less => ParseRule.rule(null, parseBinary, .comparison),
        .leq => ParseRule.rule(null, parseBinary, .comparison),
        .greater => ParseRule.rule(null, parseBinary, .comparison),
        .geq => ParseRule.rule(null, parseBinary, .comparison),

        .numberLiteral => ParseRule.rule(parseNumber, null, .none),
        .trueLiteral => ParseRule.rule(parseLiteral, null, .none),
        .falseLiteral => ParseRule.rule(parseLiteral, null, .none),
        .nullLiteral => ParseRule.rule(parseLiteral, null, .none),
        .stringLiteral => ParseRule.rule(parseString, null, .none),

        else => ParseRule.rule(null, null, .none),
    };
}
