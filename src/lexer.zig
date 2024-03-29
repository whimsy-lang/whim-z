const std = @import("std");
const unicode = std.unicode;
const out = @import("out.zig");

pub const TokenType = enum {
    // symbols
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    comma,
    dot,
    dot_dot,
    dot_dot_equal,
    semicolon,
    colon,
    colon_colon,
    colon_equal,
    bang,
    bang_equal,
    equal,
    equal_equal,
    less,
    less_equal,
    greater,
    greater_equal,
    plus,
    plus_equal,
    minus,
    minus_equal,
    star,
    star_equal,
    slash,
    slash_equal,
    percent,
    percent_equal,
    // literals
    identifier,
    number,
    string,
    symbol,
    // keywords
    and_,
    break_,
    by,
    class,
    continue_,
    do,
    elif,
    else_,
    false,
    fn_,
    for_,
    if_,
    in,
    is,
    loop,
    nil,
    or_,
    return_,
    true,
    // ends
    class_end,
    do_end,
    fn_end,
    for_end,
    if_end,
    loop_end,
    // special
    error_,
    eof,
};

pub const Token = struct {
    type: TokenType,
    value: []const u8,
    line: u29,
};

pub const Lexer = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: u29,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Lexer, count: usize) void {
        var i: usize = 0;
        while (i < count and !self.isAtEnd()) : (i += 1) {
            self.current += unicode.utf8ByteSequenceLength(self.source[self.current]) catch {
                out.printExit("Invalid character encoding.", .{}, 1);
            };
        }
        if (self.isAtEnd()) self.current = self.source.len;
    }

    fn advanceByte(self: *Lexer, count: usize) void {
        self.current += count;
        if (self.isAtEnd()) self.current = self.source.len;
    }

    fn peek(self: *Lexer, offset: usize) u21 {
        var i: usize = 0;
        var ind = self.current;
        while (i < offset and ind < self.source.len) : (i += 1) {
            ind += unicode.utf8ByteSequenceLength(self.source[ind]) catch {
                out.printExit("Invalid character encoding.", .{}, 1);
            };
        }
        if (ind < self.source.len) {
            const len = unicode.utf8ByteSequenceLength(self.source[ind]) catch {
                out.printExit("Invalid character encoding.", .{}, 1);
            };
            return unicode.utf8Decode(self.source[ind .. ind + len]) catch {
                out.printExit("Invalid character encoding.", .{}, 1);
            };
        }
        return 0;
    }

    fn resetLength(self: *Lexer) void {
        self.start = self.current;
    }

    fn isDigit(c: u21) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u21) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isAlphaOrDigit(c: u21) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn isBinary(c: u21) bool {
        return c == '0' or c == '1' or c == '_';
    }

    fn isOctal(c: u21) bool {
        return (c >= '0' and c <= '7') or c == '_';
    }

    fn isDecimal(c: u21) bool {
        return isDigit(c) or c == '_';
    }

    fn isHex(c: u21) bool {
        return isDecimal(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
    }

    fn isSymbol(c: u21) bool {
        return switch (c) {
            '~', '!', '@', '$', '%', '^', '&', '*', '-', '+', '=' => true,
            '{', '}', '\\', '|', ';', ':', '<', '>', '.', '?', '/' => true,
            else => false,
        };
    }

    fn checkKeyword(cur_part: []const u8, rest: []const u8, token_type: TokenType) TokenType {
        if (std.mem.eql(u8, cur_part, rest)) return token_type;
        return .identifier;
    }

    // checks starting at self.current + 1, checking for rest and then !isAlphaOrDigit
    // assumes each character is a byte
    // returns a token with token_type on success or .slash
    fn checkEnd(self: *Lexer, rest: []const u8, token_type: TokenType) Token {
        const start = self.current + 1;
        const end = self.current + 1 + rest.len;
        if (end <= self.source.len and std.mem.eql(u8, self.source[start..end], rest)) {
            var end_val: u21 = 0;
            if (end < self.source.len) {
                const len = unicode.utf8ByteSequenceLength(self.source[end]) catch {
                    out.printExit("Invalid character encoding.", .{}, 1);
                };
                end_val = unicode.utf8Decode(self.source[end .. end + len]) catch {
                    out.printExit("Invalid character encoding.", .{}, 1);
                };
            }
            if (!isAlphaOrDigit(end_val)) {
                self.advanceByte(rest.len + 1);
                return self.token(token_type);
            }
        }
        return self.token(.slash);
    }

    fn token(self: *Lexer, token_type: TokenType) Token {
        return .{
            .type = token_type,
            .value = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: *Lexer, message: []const u8) Token {
        return .{
            .type = .error_,
            .value = message,
            .line = self.line,
        };
    }

    fn identifierType(self: *Lexer) TokenType {
        const cur = self.source[self.start..self.current];
        switch (cur[0]) {
            'a' => return checkKeyword(cur[1..], "nd", .and_),
            'b' => if (cur.len > 1) {
                switch (cur[1]) {
                    'r' => return checkKeyword(cur[2..], "eak", .break_),
                    'y' => if (cur.len == 2) return .by,
                    else => {},
                }
            },
            'c' => if (cur.len > 1) {
                switch (cur[1]) {
                    'l' => return checkKeyword(cur[2..], "ass", .class),
                    'o' => return checkKeyword(cur[2..], "ntinue", .continue_),
                    else => {},
                }
            },
            'd' => return checkKeyword(cur[1..], "o", .do),
            'e' => if (cur.len == 4 and cur[1] == 'l') {
                switch (cur[2]) {
                    'i' => return checkKeyword(cur[3..], "f", .elif),
                    's' => return checkKeyword(cur[3..], "e", .else_),
                    else => {},
                }
            },
            'f' => if (cur.len > 1) {
                switch (cur[1]) {
                    'a' => return checkKeyword(cur[2..], "lse", .false),
                    'n' => if (cur.len == 2) return .fn_,
                    'o' => return checkKeyword(cur[2..], "r", .for_),
                    else => {},
                }
            },
            'i' => if (cur.len == 2) {
                switch (cur[1]) {
                    'f' => return .if_,
                    'n' => return .in,
                    's' => return .is,
                    else => {},
                }
            },
            'l' => return checkKeyword(cur[1..], "oop", .loop),
            'n' => return checkKeyword(cur[1..], "il", .nil),
            'o' => return checkKeyword(cur[1..], "r", .or_),
            'r' => return checkKeyword(cur[1..], "eturn", .return_),
            't' => return checkKeyword(cur[1..], "rue", .true),
            else => {},
        }
        return .identifier;
    }

    fn identifier(self: *Lexer) Token {
        while (isAlphaOrDigit(self.peek(0))) self.advance(1);

        return self.token(self.identifierType());
    }

    fn infix(self: *Lexer) Token {
        if (!isAlpha(self.peek(0))) {
            return self.errorToken("Invalid infix operator.");
        }
        // discard `
        self.resetLength();

        self.advance(1);
        while (isAlphaOrDigit(self.peek(0))) self.advance(1);

        return self.token(.symbol);
    }

    const numDigitFn = *const fn (u21) bool;

    fn number(self: *Lexer, first: u21) Token {
        var check: numDigitFn = isDecimal;
        if (first == '0') {
            switch (self.peek(0)) {
                'b', 'B' => {
                    self.advance(1);
                    check = isBinary;
                },
                'o', 'O' => {
                    self.advance(1);
                    check = isOctal;
                },
                'x', 'X' => {
                    self.advance(1);
                    check = isHex;
                },
                else => {},
            }
        }

        while (check(self.peek(0))) self.advance(1);

        if (self.peek(0) == '.' and check(self.peek(1))) {
            // accept the . and digit
            self.advance(2);

            while (check(self.peek(0))) self.advance(1);
        }

        return self.token(.number);
    }

    fn string(self: *Lexer, first: u21) Token {
        // discard opening quote
        self.resetLength();

        while (self.peek(0) != first and !self.isAtEnd()) {
            if (self.peek(0) == '\n') self.line += 1;
            if (self.peek(0) == '\\' and self.peek(1) != 0 and self.peek(1) != '\n') self.advance(1);
            self.advance(1);
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        const tok = self.token(.string);

        // discard closing quote
        self.advance(1);
        self.resetLength();

        return tok;
    }

    fn followedByEq(cur: []const u8, tok_single: TokenType, tok_eq: TokenType) TokenType {
        if (cur.len == 1) {
            return tok_single;
        } else if (cur.len == 2 and cur[1] == '=') {
            return tok_eq;
        }
        return .symbol;
    }

    fn symbolType(self: *Lexer) TokenType {
        const cur = self.source[self.start..self.current];
        switch (cur[0]) {
            '.' => if (cur.len == 1) {
                return .dot;
            } else if (cur[1] == '.') {
                if (cur.len == 2) {
                    return .dot_dot;
                } else if (cur.len == 3 and cur[2] == '=') {
                    return .dot_dot_equal;
                }
            },
            ';' => if (cur.len == 1) return .semicolon,
            ':' => if (cur.len == 1) {
                return .colon;
            } else if (cur.len == 2) {
                switch (cur[1]) {
                    ':' => return .colon_colon,
                    '=' => return .colon_equal,
                    else => {},
                }
            },
            '!' => return followedByEq(cur, .bang, .bang_equal),
            '=' => return followedByEq(cur, .equal, .equal_equal),
            '<' => return followedByEq(cur, .less, .less_equal),
            '>' => return followedByEq(cur, .greater, .greater_equal),
            '+' => return followedByEq(cur, .plus, .plus_equal),
            '-' => return followedByEq(cur, .minus, .minus_equal),
            '%' => return followedByEq(cur, .percent, .percent_equal),
            '*' => return followedByEq(cur, .star, .star_equal),
            '/' => return followedByEq(cur, .slash, .slash_equal),
            else => {},
        }
        return .symbol;
    }

    fn symbol(self: *Lexer) Token {
        while (isSymbol(self.peek(0))) self.advance(1);

        return self.token(self.symbolType());
    }

    pub fn lexToken(self: *Lexer) Token {
        self.resetLength();

        while (!self.isAtEnd()) {
            const c = self.peek(0);
            self.advance(1);

            if (isAlpha(c)) return self.identifier();
            if (isDigit(c)) return self.number(c);

            switch (c) {
                '\n' => {
                    self.line += 1;
                    self.resetLength();
                },
                ' ', '\t', '\r' => self.resetLength(),
                '(' => return self.token(.left_paren),
                ')' => return self.token(.right_paren),
                '[' => return self.token(.left_bracket),
                ']' => return self.token(.right_bracket),
                ',' => return self.token(.comma),
                '\'', '"' => return self.string(c),
                '`' => return self.infix(),
                '#' => {
                    while (self.peek(0) != '\n' and !self.isAtEnd()) self.advance(1);
                    self.resetLength();
                },

                '/' => switch (self.peek(0)) {
                    'c' => return self.checkEnd("lass", .class_end),
                    'd' => return self.checkEnd("o", .do_end),
                    'f' => {
                        switch (self.peek(1)) {
                            'n' => if (!isAlphaOrDigit(self.peek(2))) {
                                self.advance(2);
                                return self.token(.fn_end);
                            },
                            'o' => if (self.peek(2) == 'r' and !isAlphaOrDigit(self.peek(3))) {
                                self.advance(3);
                                return self.token(.for_end);
                            },
                            else => {},
                        }
                        return self.token(.slash);
                    },
                    'i' => return self.checkEnd("f", .if_end),
                    'l' => return self.checkEnd("oop", .loop_end),
                    else => return self.symbol(),
                },
                else => {
                    if (isSymbol(c)) return self.symbol();
                    return self.errorToken("Unexpected character.");
                },
            }
        }

        return self.token(.eof);
    }
};
