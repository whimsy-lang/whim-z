const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

pub const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
};

const Precedence = enum {
    none,
    // or
    or_,
    // and
    and_,
    // == !=
    equality,
    // < > <= >=
    comparison,
    // + -
    term,
    // * / %
    factor,
    // ! -
    unary,
    // . () []
    call,
};

const PrimaryParseFn = *const fn (*Vm) bool;
const ParseFn = *const fn (*Vm) void;

pub const Compiler = struct {
    encountered_identifier: ?[]const u8,

    fn errorAt(vm: *Vm, token: *Token, message: []const u8) void {
        if (vm.parser.panic_mode) return;
        vm.parser.panic_mode = true;

        std.debug.print("[line {d}] Error", .{token.line});

        if (token.type == .eof) {
            std.debug.print(" at end", .{});
        } else if (token.type != .error_) {
            std.debug.print(" at '{s}'", .{token.value});
        }

        std.debug.print(": {s}\n", .{message});
        vm.parser.had_error = true;
    }

    fn error_(vm: *Vm, message: []const u8) void {
        errorAt(vm, &vm.parser.previous, message);
    }

    fn errorAtCurrent(vm: *Vm, message: []const u8) void {
        errorAt(vm, &vm.parser.current, message);
    }

    fn advance(vm: *Vm) void {
        vm.parser.previous = vm.parser.current;

        while (true) {
            vm.parser.current = vm.lexer.lexToken();
            if (vm.parser.current.type != .error_) break;

            errorAtCurrent(vm, vm.parser.current.value);
        }
    }

    fn consume(vm: *Vm, expected: TokenType, message: []const u8) void {
        if (vm.parser.current.type == expected) {
            advance(vm);
            return;
        }

        errorAtCurrent(vm, message);
    }

    fn check(vm: *Vm, expected: TokenType) bool {
        return vm.parser.current.type == expected;
    }

    fn match(vm: *Vm, expected: TokenType) bool {
        if (!check(vm, expected)) return false;
        advance(vm);
        return true;
    }

    fn emitReturn(vm: *Vm) void {
        vm.emitOp(.return_);
    }

    fn makeConstant(vm: *Vm, value: Value) u8 {
        const constant = vm.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            error_(vm, "Too many constants in one chunk.");
            return 0;
        }
        return @intCast(u8, constant);
    }

    fn emitConstant(vm: *Vm, value: Value) void {
        vm.emitOpByte(.constant, makeConstant(vm, value));
    }

    fn endCompiler(vm: *Vm) void {
        emitReturn(vm);
        if (debug.print_code and !vm.parser.had_error) {
            debug.disassembleChunk(vm.currentChunk(), "code");
        }
    }

    fn identifierConstant(vm: *Vm, name: *Token) u8 {
        return makeConstant(vm, Value.string(ObjString.copy(vm, name.value)));
    }

    fn defineGlobal(vm: *Vm, global: u8, constant: bool) void {
        vm.emitOpByte(if (constant) .define_global_const else .define_global_var, global);
    }

    fn getPrefixPrimary(tok_type: TokenType) ?PrimaryParseFn {
        return switch (tok_type) {
            .left_paren => groupingPrimary,
            .identifier => variablePrimary,
            else => null,
        };
    }

    fn getInfixPrimary(tok_type: TokenType) ?PrimaryParseFn {
        return switch (tok_type) {
            // .left_paren => callPrimary,
            // .dot => dotPrimary,
            else => null,
        };
    }

    fn getPrefix(tok_type: TokenType) ?ParseFn {
        return switch (tok_type) {
            .left_paren => grouping,
            .bang, .minus => unary,
            .identifier => variable,
            .string => string,
            .number => number,
            // .class => class,
            .false, .nil, .true => literal,
            // .fn_ => function,
            else => null,
        };
    }

    fn getInfix(tok_type: TokenType) ?ParseFn {
        return switch (tok_type) {
            // .left_paren => call,
            // .dot => dot,
            .bang_equal, .equal_equal => binary,
            .less, .less_equal, .greater, .greater_equal => binary,
            .plus, .minus, .star, .slash, .percent => binary,
            // .and_ => andOp,
            // .or_ => orOp,
            else => null,
        };
    }

    fn getPrecedence(tok_type: TokenType) Precedence {
        return switch (tok_type) {
            .left_paren, .dot => .call,
            .bang_equal, .equal_equal => .equality,
            .less, .less_equal, .greater, .greater_equal => .comparison,
            .plus, .minus => .term,
            .star, .slash, .percent => .factor,
            .and_ => .and_,
            .or_ => .or_,
            else => .none,
        };
    }

    fn binary(vm: *Vm) void {
        const op_type = vm.parser.previous.type;
        const precedence = getPrecedence(op_type);
        parsePrecedence(vm, @intToEnum(Precedence, @enumToInt(precedence) + 1));

        switch (op_type) {
            .bang_equal => vm.emitOp(.not_equal),
            .equal_equal => vm.emitOp(.equal),
            .greater => vm.emitOp(.greater),
            .greater_equal => vm.emitOp(.greater_equal),
            .less => vm.emitOp(.less),
            .less_equal => vm.emitOp(.less_equal),
            .plus => vm.emitOp(.add),
            .minus => vm.emitOp(.subtract),
            .star => vm.emitOp(.multiply),
            .slash => vm.emitOp(.divide),
            .percent => vm.emitOp(.modulus),
            else => unreachable,
        }
    }

    fn groupingPrimary(vm: *Vm) bool {
        grouping(vm);
        return false;
    }

    fn grouping(vm: *Vm) void {
        expression(vm);
        consume(vm, .right_paren, "Expect ')' after expression.");
    }

    fn literal(vm: *Vm) void {
        switch (vm.parser.previous.type) {
            .false => vm.emitOp(.false),
            .nil => vm.emitOp(.nil),
            .true => vm.emitOp(.true),
            else => unreachable,
        }
    }

    fn number(vm: *Vm) void {
        const value = std.fmt.parseFloat(f64, vm.parser.previous.value) catch 0;
        emitConstant(vm, Value.number(value));
    }

    fn string(vm: *Vm) void {
        const value = vm.parser.previous.value;
        emitConstant(vm, Value.string(ObjString.copyEscape(vm, value[1..(value.len - 1)])));
    }

    fn unary(vm: *Vm) void {
        const op_type = vm.parser.previous.type;

        // compile the operand
        parsePrecedence(vm, .unary);

        // emit the operator instruction
        switch (op_type) {
            .bang => vm.emitOp(.not),
            .minus => vm.emitOp(.negate),
            else => unreachable,
        }
    }

    fn variablePrimary(vm: *Vm) bool {
        switch (vm.parser.current.type) {
            .colon_colon, .colon_equal => {
                // declaration

                const constant = vm.parser.current.type == .colon_colon;

                vm.compiler.encountered_identifier = vm.parser.previous.value;

                const arg = identifierConstant(vm, &vm.parser.previous);
                advance(vm); // accept :: :=
                expression(vm);
                defineGlobal(vm, arg, constant);

                return true;
            },
            .equal, .plus_equal, .minus_equal, .star_equal, .slash_equal, .percent_equal => {
                // assignment

                vm.compiler.encountered_identifier = vm.parser.previous.value;

                // global
                const arg = identifierConstant(vm, &vm.parser.previous);

                const op: OpCode = switch (vm.parser.current.type) {
                    .plus_equal => .add_set_global,
                    .minus_equal => .subtract_set_global,
                    .star_equal => .multiply_set_global,
                    .slash_equal => .divide_set_global,
                    .percent_equal => .modulus_set_global,
                    else => .set_global,
                };

                advance(vm); // accept = += -= *= /= %=
                expression(vm);
                vm.emitOpByte(op, arg);
                return true;
            },
            else => {},
        }
        // not an assignment, continue parsing the expression
        variable(vm);
        return false;
    }

    fn variable(vm: *Vm) void {
        var name = vm.parser.previous;
        const arg = identifierConstant(vm, &name);
        vm.emitOpByte(.get_global, arg);
    }

    fn parseInfixPrecedence(vm: *Vm, precedence: Precedence) void {
        while (@enumToInt(precedence) <= @enumToInt(getPrecedence(vm.parser.current.type))) {
            advance(vm);
            const infix = getInfix(vm.parser.previous.type);
            infix.?(vm);
        }
    }

    fn parsePrecedenceFromPrevious(vm: *Vm, precedence: Precedence) void {
        const prefix = getPrefix(vm.parser.previous.type);
        if (prefix == null) {
            error_(vm, "Expect expression.");
            return;
        }

        prefix.?(vm);

        parseInfixPrecedence(vm, precedence);
    }

    fn parsePrecedence(vm: *Vm, precedence: Precedence) void {
        advance(vm);
        parsePrecedenceFromPrevious(vm, precedence);
    }

    fn expression(vm: *Vm) void {
        parsePrecedence(vm, .or_);
    }

    fn expressionFromPrevious(vm: *Vm) void {
        parsePrecedenceFromPrevious(vm, .or_);
    }

    fn expressionStatement(vm: *Vm) void {
        advance(vm);
        const primary_prefix = getPrefixPrimary(vm.parser.previous.type);
        if (primary_prefix == null) {
            expressionFromPrevious(vm);
            vm.emitOp(.pop);
            return;
        }

        var done = primary_prefix.?(vm);

        // parse the primary expression, checking for assignment
        while (!done and @enumToInt(Precedence.call) <= @enumToInt(getPrecedence(vm.parser.current.type))) {
            advance(vm);
            const primary_infix = getInfixPrimary(vm.parser.previous.type);
            done = primary_infix.?(vm);
        }

        // parse any other parts of the expression
        if (!done) {
            parseInfixPrecedence(vm, .or_);
            vm.emitOp(.pop);
        }
    }

    fn statement(vm: *Vm) void {
        vm.compiler.encountered_identifier = null;

        if (match(vm, .semicolon)) {
            // empty statement
        } else {
            expressionStatement(vm);
        }

        if (vm.parser.panic_mode) synchronize(vm);
    }

    fn synchronize(vm: *Vm) void {
        vm.parser.panic_mode = false;

        while (vm.parser.current.type != .eof) {
            switch (vm.parser.previous.type) {
                .class_end, .do_end, .fn_end, .for_end, .if_end, .loop_end, .semicolon => return,
                else => {},
            }

            switch (vm.parser.current.type) {
                .break_, .class, .continue_, .do, .fn_, .for_, .if_, .loop, .return_ => return,
                else => {},
            }

            advance(vm);
        }
    }

    pub fn compile(vm: *Vm, source: [:0]const u8, chunk: *Chunk) bool {
        vm.lexer = Lexer.init(source);
        vm.compilingChunk = chunk;

        vm.parser.had_error = false;
        vm.parser.panic_mode = false;

        advance(vm);

        while (!match(vm, .eof)) {
            statement(vm);
        }

        endCompiler(vm);
        return !vm.parser.had_error;
    }
};
