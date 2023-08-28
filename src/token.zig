const std = @import("std");

const keywords = std.ComptimeStringMap(Kind, .{
    .{ "break", .breakKeyword },
    .{ "continue", .continueKeyword },
    .{ "if", .ifKeyword },
    .{ "else", .elseKeyword },
    .{ "for", .forKeyword },
    .{ "in", .inKeyword },
    .{ "return", .returnKeyword },

    .{ "true", .trueLiteral },
    .{ "false", .falseLiteral },
    .{ "null", .nullLiteral },
});

pub const Kind = enum {
    comment,

    // language tokens
    comma,
    dot,
    dotdot,
    colon,
    leftParen,
    rightParen,
    leftBracket,
    rightBracket,
    leftBrace,
    rightBrace,
    assign, // :=
    set, // =
    singleArrow,
    doubleArrow,

    // binary operators
    plus,
    minus,
    times,
    divide,
    modulus,
    @"and",
    @"or",
    greater,
    less,
    eq,
    geq,
    leq,
    neq,

    // unary operators
    not,
    negate,

    // keywords
    breakKeyword,
    continueKeyword,
    ifKeyword,
    elseKeyword,
    forKeyword,
    inKeyword,
    returnKeyword,

    // literals
    identifier,
    trueLiteral,
    falseLiteral,
    stringLiteral,
    numberLiteral,
    nullLiteral,
};

const Position = struct {
    line: u32,
    column: u32,
    filename: []const u8,

    pub fn format(
        self: Position,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("[{}:{}]", self.line, self.column);
    }
};

pub const Token = struct {
    kind: Kind,
    pos: Position,
    payload: ?[]const u8 = null,

    pub fn string(self: Token) []const u8 {
        return switch (self.kind) {
            .comma => ",",
            .dot => ".",
            .dotdot => "..",
            .colon => ":",
            .leftParen => "(",
            .rightParen => ")",
            .leftBracket => "[",
            .rightBracket => "]",
            .leftBrace => "{",
            .rightBrace => "}",
            .assign => ":=",
            .set => "=",
            .singleArrow => "->",
            .doubleArrow => "=>",

            .plus => "+",
            .minus => "-",
            .times => "*",
            .divide => "/",
            .modulus => "%",
            .@"and" => "and",
            .@"or" => "or",
            .greater => ">",
            .less => "<",
            .eq => "==",
            .geq => ">=",
            .leq => "<=",
            .neq => "!=",

            .not => "!",
            .negate => "-",

            .breakKeyword => "break",
            .continueKeyword => "continue",
            .ifKeyword => "if",
            .elseKeyword => "else",
            .forKeyword => "for",
            .inKeyword => "in",
            .returnKeyword => "return",

            .identifier => self.payload.?,
            .trueLiteral => "true",
            .falseLiteral => "false",
            .stringLiteral => self.payload.?,
            .numberLiteral => self.payload.?,
            .nullLiteral => "null",

            .comment => "comment",
        };
    }

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self.kind) {
            .identifier => try writer.print("var({s})", .{self.payload.?}),
            .stringLiteral => try writer.print("string({s})", .{self.payload.?}),
            .numberLiteral => try writer.print("number({s})", .{self.payload.?}),
            else => try writer.print("{s}", .{self.string()}),
        }
    }
};

pub const Tokenizer = struct {
    const This = @This();
    source: []const u8,
    filename: []const u8,

    index: usize,
    line: u32,
    column: u32,

    pub fn init(source: []const u8, filename: ?[]const u8) Tokenizer {
        return Tokenizer{
            .source = source,
            .filename = if (filename) |f| f else "unknown",
            .index = 0,
            .line = 0,
            .column = 0,
        };
    }

    fn isEOF(t: *This) bool {
        return t.index >= t.source.len;
    }

    fn next(t: *Tokenizer) u8 {
        var char = t.source[t.index];

        // ensure we don't go past EOF
        if (t.index < t.source.len) {
            t.index += 1;
        }

        if (char == '\n') {
            t.line += 1;
            t.column = 0;
        } else {
            t.column += 1;
        }

        return char;
    }

    fn peek(t: *Tokenizer) u8 {
        return t.source[t.index];
    }

    fn peekAhead(t: *Tokenizer, n: usize) ?u8 {
        if (t.index + n < t.source.len) {
            return t.source[t.index + n];
        } else {
            return null;
        }
    }

    fn readUntil(t: *Tokenizer, ch: u8) []const u8 {
        var start = t.index - 1;
        while (!t.isEOF() and t.peek() != ch) {
            _ = t.next();
        }
        return t.source[start..t.index];
    }

    fn readIdentifier(t: *Tokenizer) []const u8 {
        var start = t.index - 1;
        while (!t.isEOF()) {
            var ch = t.peek();
            if (ch == '_' or ch == '?' or isLetter(ch) or isDigit(ch)) {
                _ = t.next();
            } else {
                break;
            }
        }

        return t.source[start..t.index];
    }

    fn readNumber(t: *Tokenizer) []const u8 {
        var start = t.index - 1;
        var dot = false;
        while (!t.isEOF()) {
            var ch = t.peek();
            if (isDigit(ch)) {
                _ = t.next();
            } else if (ch == '.' and !dot) {
                // range operator
                if (t.peekAhead(1) == '.') {
                    break;
                }

                dot = true;
                _ = t.next();
            } else {
                break;
            }
        }

        return t.source[start..t.index];
    }

    fn position(t: *Tokenizer) Position {
        return Position{
            .line = t.line,
            .column = t.column,
            .filename = t.filename,
        };
    }

    fn branch(t: *Tokenizer, cond: u8, then: Kind, otherwise: Kind) Token {
        var pos = t.position();

        if (!t.isEOF() and t.peek() == cond) {
            _ = t.next();
            return Token{ .kind = then, .pos = pos };
        } else {
            return Token{ .kind = otherwise, .pos = pos };
        }
    }

    fn nextToken(t: *Tokenizer) Token {
        var ch = t.next();

        switch (ch) {
            ',' => return Token{ .kind = .comma, .pos = t.position() },
            '.' => {
                if (!t.isEOF() and t.peek() == '.') {
                    var pos = t.position();
                    _ = t.next();
                    return Token{ .kind = .dotdot, .pos = pos };
                } else {
                    return Token{ .kind = .dot, .pos = t.position() };
                }
            },
            '(' => return Token{ .kind = .leftParen, .pos = t.position() },
            ')' => return Token{ .kind = .rightParen, .pos = t.position() },
            '[' => return Token{ .kind = .leftBracket, .pos = t.position() },
            ']' => return Token{ .kind = .rightBracket, .pos = t.position() },
            '{' => return Token{ .kind = .leftBrace, .pos = t.position() },
            '}' => return Token{ .kind = .rightBrace, .pos = t.position() },

            ':' => return t.branch('=', .assign, .colon),
            '>' => return t.branch('=', .geq, .greater),
            '<' => return t.branch('=', .leq, .less),
            '!' => return t.branch('=', .neq, .not),
            '=' => {
                var pos = t.position();
                if (!t.isEOF() and t.peek() == '>') {
                    _ = t.next();
                    return Token{ .kind = .doubleArrow, .pos = pos };
                } else if (!t.isEOF() and t.peek() == '=') {
                    _ = t.next();
                    return Token{ .kind = .eq, .pos = pos };
                } else {
                    return Token{ .kind = .set, .pos = pos };
                }
            },

            '+' => return Token{ .kind = .plus, .pos = t.position() },
            '-' => return t.branch('>', .singleArrow, .minus),
            '*' => return Token{ .kind = .times, .pos = t.position() },
            '/' => return Token{ .kind = .divide, .pos = t.position() },
            '%' => return Token{ .kind = .modulus, .pos = t.position() },

            '"' => {
                var pos = t.position();
                var start = t.index - 1;

                while (!t.isEOF() and t.peek() != '"') {
                    var char = t.next();
                    if (char == '"') {
                        break;
                    } else if (ch == '\\') {
                        if (t.isEOF()) {
                            break;
                        }
                        _ = t.next();
                    }
                }

                if (t.isEOF()) {
                    return Token{ .kind = .stringLiteral, .pos = pos, .payload = t.source[start + 1 .. t.index] };
                } else {
                    _ = t.next();
                    return Token{ .kind = .stringLiteral, .pos = pos, .payload = t.source[start + 1 .. t.index - 1] };
                }
            },
            '0'...'9' => {
                var pos = t.position();
                var payload = t.readNumber();
                return Token{ .kind = .numberLiteral, .pos = pos, .payload = payload };
            },
            else => {
                var pos = t.position();
                var payload = t.readIdentifier();

                if (keywords.get(payload)) |keyword| {
                    return Token{ .kind = keyword, .pos = pos };
                }

                return Token{ .kind = .identifier, .pos = pos, .payload = payload };
            },
        }
    }

    pub fn tokenize(t: *Tokenizer, tokens: *std.ArrayList(Token)) !void {
        // check for shebang and skip it
        if (!t.isEOF() and t.peek() == '#' and t.peekAhead(1) == '!') {
            _ = t.readUntil('\n');
            if (!t.isEOF()) {
                _ = t.next();
            }
        }

        // skip whitespace
        while (!t.isEOF() and isSpace(t.peek())) {
            _ = t.next();
        }

        var last = Token{ .kind = .comma, .pos = t.position() };

        while (!t.isEOF()) {
            var ch = t.nextToken();

            // separate expressions
            if ((last.kind != .leftParen and last.kind != .leftBracket and last.kind != .leftBrace and last.kind != .comma) and (ch.kind == .identifier or ch.kind == .rightBracket or ch.kind == .rightBrace)) {
                try tokens.append(Token{ .kind = .comma, .pos = t.position() });
            }

            if (ch.kind == .comment) {
                ch = last;
            } else {
                try tokens.append(ch);
            }

            while (!t.isEOF() and isSpace(t.peek())) {
                if (t.peek() == '\n') {
                    switch (ch.kind) {
                        // if we hit a toke nthat can end a statement, insert a comma
                        .rightParen, .rightBrace, .rightBracket, .identifier, .numberLiteral, .stringLiteral, .trueLiteral, .falseLiteral, .nullLiteral => {
                            try tokens.append(Token{ .kind = .comma, .pos = t.position() });
                        },
                        else => {},
                    }
                }

                _ = t.next();
            }

            if (ch.kind != .comment) {
                last = ch;
            }
        }

        if (last.kind != .comma) {
            try tokens.append(Token{ .kind = .comma, .pos = t.position() });
        }
    }
};

fn isLetter(ch: u8) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z');
}

fn isDigit(ch: u8) bool {
    return ch >= '0' and ch <= '9';
}

fn isSpace(ch: u8) bool {
    return ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r';
}
