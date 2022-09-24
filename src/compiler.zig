const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
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
    primary,
};

pub const Compiler = struct {
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

    fn emitReturn(vm: *Vm) void {
        vm.emitOp(.return_);
    }

    fn makeConstant(vm: *Vm, value: Value) u8 {
        const constant = vm.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            error_(vm, "Too many constants in one chunk.");
            return 0;
        }
        return @as(u8, constant);
    }

    fn emitConstant(vm: *Vm, value: Value) void {
        vm.emitOpByte(.constant, makeConstant(vm, value));
    }

    fn endCompiler(vm: *Vm) void {
        emitReturn(vm);
    }

    fn expression(vm: *Vm) void {
        parsePrecedence(vm, .or_);
    }

    fn grouping(vm: *Vm) void {
        expression(vm);
        consume(vm, .right_paren, "Expect ')' after expression.");
    }

    fn number(vm: *Vm) void {
        const value = std.fmt.parseFloat(f64, vm.parser.previous.value) catch 0;
        emitConstant(vm, value);
    }

    fn unary(vm: *Vm) void {
        const op_type = vm.parser.previous.type;

        // compile the operand
        parsePrecedence(vm, .unary);

        // emit the operator instruction
        switch (op_type) {
            .minus => vm.emitOp(.negate),
            else => unreachable,
        }
    }

    fn parsePrecedence(vm: *Vm, precedence: Precedence) void {
        _ = vm;
        _ = precedence;
    }

    pub fn compile(vm: *Vm, source: [:0]const u8, chunk: *Chunk) bool {
        vm.lexer = Lexer.init(source);
        vm.compilingChunk = chunk;

        vm.parser.had_error = false;
        vm.parser.panic_mode = false;

        advance(vm);
        expression(vm);
        consume(vm, .eof, "Expect end of expression.");
        endCompiler(vm);
        return !vm.parser.had_error;
    }
};
