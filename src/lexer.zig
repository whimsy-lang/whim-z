const std = @import("std");

pub const TokenType = enum {
    // single character tokens
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
    comma,
    dot,
    semicolon,
    underscore,
    // two character tokens
    colon_colon,
    colon_equal,
    // one or two character tokens
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
    string,
    number,
    // keywords
    and_,
    break_,
    class,
    continue_,
    do,
    elif,
    else_,
    false,
    fn_,
    for_,
    from,
    if_,
    in,
    is,
    loop,
    nil,
    or_,
    return_,
    then,
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
    line: usize,
};

pub const Lexer = struct {
    source: [:0]const u8,
    start: usize,
    current: usize,
    line: usize,

    pub fn init(source: [:0]const u8) Lexer {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn isAtEnd(self: *Lexer) bool {
        return self.source[self.current] == 0;
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn advanceMulti(self: *Lexer, count: usize) void {
        self.current += count;
    }

    fn peek(self: *Lexer) u8 {
        return self.source[self.current];
    }

    fn peekAt(self: *Lexer, offset: usize) u8 {
        return self.source[self.current + offset];
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.peek() != expected) return false;
        self.current += 1;
        return true;
    }

    fn resetLength(self: *Lexer) void {
        self.start = self.current;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn isAlphaOrDigit(c: u8) bool {
        return isAlpha(c) or isDigit(c);
    }

    fn checkKeyword(cur_part: []const u8, rest: []const u8, token_type: TokenType) TokenType {
        if (std.mem.eql(u8, cur_part, rest)) {
            return token_type;
        }
        return .identifier;
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

    fn skipBlockComment(self: *Lexer) void {
        var depth: i32 = 1;

        while (depth > 0 and !self.isAtEnd()) {
            switch (self.advance()) {
                '\n' => self.line += 1,
                '/' => if (self.peek() == '*') {
                    _ = self.advance();
                    depth += 1;
                },
                '*' => if (self.peek() == '/') {
                    _ = self.advance();
                    depth -= 1;
                },
                else => {},
            }
        }

        self.resetLength();
    }

    fn identifierType(self: *Lexer) TokenType {
        const cur = self.source[self.start..self.current];
        switch (cur[0]) {
            '_' => if (cur.len == 1) return .underscore,
            'a' => return checkKeyword(cur[1..], "nd", .and_),
            'b' => return checkKeyword(cur[1..], "reak", .break_),
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
                    'r' => return checkKeyword(cur[2..], "om", .from),
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
            't' => if (cur.len == 4) {
                switch (cur[1]) {
                    'h' => return checkKeyword(cur[2..], "en", .then),
                    'r' => return checkKeyword(cur[2..], "ue", .true),
                    else => {},
                }
            },
            else => {},
        }
        return .identifier;
    }

    fn identifier(self: *Lexer) Token {
        while (isAlphaOrDigit(self.peek())) _ = self.advance();

        return self.token(self.identifierType());
    }

    fn number(self: *Lexer) Token {
        while (isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.' and isDigit(self.peekAt(1))) {
            // accept the .
            _ = self.advance();

            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.token(.number);
    }

    fn string(self: *Lexer, first: u8) Token {
        while (self.peek() != first and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            if (self.peek() == '\\' and self.peekAt(1) != 0 and self.peekAt(1) != '\n') _ = self.advance();
            _ = self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        // closing quote
        _ = self.advance();
        return self.token(.string);
    }

    pub fn lexToken(self: *Lexer) Token {
        self.resetLength();

        while (!self.isAtEnd()) {
            const c = self.advance();

            if (isAlpha(c)) return self.identifier();
            if (isDigit(c)) return self.number();

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
                '.' => return self.token(.dot),
                ';' => return self.token(.semicolon),
                ':' => switch (self.peek()) {
                    ':' => {
                        _ = self.advance();
                        return self.token(.colon_colon);
                    },
                    '=' => {
                        _ = self.advance();
                        return self.token(.colon_equal);
                    },
                    else => return self.errorToken("Unexpected character."),
                },
                '!' => return self.token(if (self.match('=')) .bang_equal else .bang),
                '=' => return self.token(if (self.match('=')) .equal_equal else .equal),
                '<' => return self.token(if (self.match('=')) .less_equal else .less),
                '>' => return self.token(if (self.match('=')) .greater_equal else .greater),
                '+' => return self.token(if (self.match('=')) .plus_equal else .plus),
                '-' => return self.token(if (self.match('=')) .minus_equal else .minus),
                '%' => return self.token(if (self.match('=')) .percent_equal else .percent),
                '*' => switch (self.peek()) {
                    '=' => {
                        _ = self.advance();
                        return self.token(.star_equal);
                    },
                    '/' => {
                        _ = self.advance();
                        self.resetLength();
                    },
                    else => return self.token(.star),
                },
                '/' => switch (self.peek()) {
                    '=' => {
                        _ = self.advance();
                        return self.token(.slash_equal);
                    },
                    '/' => {
                        _ = self.advance();
                        while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                        self.resetLength();
                    },
                    '*' => {
                        _ = self.advance();
                        self.skipBlockComment();
                    },
                    'c' => {
                        if (self.peekAt(1) == 'l' and
                            self.peekAt(2) == 'a' and
                            self.peekAt(3) == 's' and
                            self.peekAt(4) == 's' and
                            !isAlphaOrDigit(self.peekAt(5)))
                        {
                            self.advanceMulti(5);
                            return self.token(.class_end);
                        }
                        return self.token(.slash);
                    },
                    'd' => {
                        if (self.peekAt(1) == 'o' and !isAlphaOrDigit(self.peekAt(2))) {
                            self.advanceMulti(2);
                            return self.token(.do_end);
                        }
                        return self.token(.slash);
                    },
                    'f' => {
                        switch (self.peekAt(1)) {
                            'n' => if (!isAlphaOrDigit(self.peekAt(2))) {
                                self.advanceMulti(2);
                                return self.token(.fn_end);
                            },
                            'o' => if (self.peekAt(2) == 'r' and !isAlphaOrDigit(self.peekAt(3))) {
                                self.advanceMulti(3);
                                return self.token(.for_end);
                            },
                            else => {},
                        }
                        return self.token(.slash);
                    },
                    'i' => {
                        if (self.peekAt(1) == 'f' and !isAlphaOrDigit(self.peekAt(2))) {
                            self.advanceMulti(2);
                            return self.token(.if_end);
                        }
                        return self.token(.slash);
                    },
                    'l' => {
                        if (self.peekAt(1) == 'o' and
                            self.peekAt(2) == 'o' and
                            self.peekAt(3) == 'p' and
                            !isAlphaOrDigit(self.peekAt(4)))
                        {
                            self.advanceMulti(4);
                            return self.token(.loop_end);
                        }
                        return self.token(.slash);
                    },
                    else => return self.token(.slash),
                },
                '\'', '"' => return self.string(c),
                else => return self.errorToken("Unexpected character."),
            }
        }

        return self.token(.eof);
    }
};
