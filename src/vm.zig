const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Compiler = @import("compiler.zig").Compiler;
const Parser = @import("compiler.zig").Parser;
const debug = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const GcAllocator = @import("memory.zig").GcAllocater;
const Value = @import("value.zig").Value;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const Vm = struct {
    const Self = @This();

    const stack_max = 256;

    parent_allocator: Allocator,
    gc: GcAllocator,
    allocator: Allocator,

    chunk: *Chunk,
    ip: [*]u8,
    stack: [stack_max]Value,
    stack_top: [*]Value,

    lexer: Lexer,
    parser: Parser,
    compilingChunk: *Chunk,

    pub fn init(self: *Self, allocator: Allocator) void {
        self.parent_allocator = allocator;
        self.gc = GcAllocator.init(allocator);
        self.allocator = self.gc.allocator();
        self.resetStack();
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    fn resetStack(self: *Self) void {
        self.stack_top = &self.stack;
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        const instruction = @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr) - 1;
        const line = self.chunk.lines.items[instruction];
        std.debug.print("[line {d}] in script\n", .{line});
        self.resetStack();
    }

    fn isFalsey(value: Value) bool {
        return value.is(.nil) or (value.is(.bool) and !value.as.bool);
    }

    fn peek(self: *Self, offset: usize) Value {
        return (self.stack_top - (offset + 1))[0];
    }

    pub fn push(self: *Self, value: Value) void {
        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *Self) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    pub fn currentChunk(self: *Self) *Chunk {
        return self.compilingChunk;
    }

    pub fn emitByte(self: *Self, byte: u8) void {
        self.currentChunk().write(byte, self.parser.previous.line);
    }

    pub fn emitOp(self: *Self, op: OpCode) void {
        self.currentChunk().write(@enumToInt(op), self.parser.previous.line);
    }

    pub fn emitOpByte(self: *Self, op: OpCode, byte: u8) void {
        self.emitOp(op);
        self.emitByte(byte);
    }

    pub fn interpret(self: *Self, source: [:0]const u8) InterpretResult {
        var chunk = Chunk.init(self.allocator);
        defer chunk.deinit();

        if (!Compiler.compile(self, source, &chunk)) {
            return .compile_error;
        }

        self.chunk = &chunk;
        self.ip = self.chunk.code.items.ptr;

        const result = self.run();

        return result;
    }

    // todo - compare performance to increment and then returning self.ip[-1]
    fn readByte(self: *Self) u8 {
        const value = self.ip[0];
        self.ip += 1;
        return value;
    }

    fn readConstant(self: *Self) Value {
        return self.chunk.constants.items[self.readByte()];
    }

    fn run(self: *Self) InterpretResult {
        while (true) {
            if (debug.trace_execution) {
                std.debug.print("          ", .{});
                var slot: [*]Value = &self.stack;
                while (@ptrToInt(slot) < @ptrToInt(self.stack_top)) : (slot += 1) {
                    std.debug.print("[ ", .{});
                    slot[0].print();
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(self.chunk, @ptrToInt(self.ip) - @ptrToInt(self.chunk.code.items.ptr));
            }

            const instruction = self.readByte();
            switch (@intToEnum(OpCode, instruction)) {
                .constant => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .nil => self.push(Value.nil()),
                .true => self.push(Value.boolean(true)),
                .false => self.push(Value.boolean(false)),
                .equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.boolean(a.equal(b)));
                },
                .not_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.boolean(!a.equal(b)));
                },
                .greater => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.boolean(a > b));
                },
                .greater_equal => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.boolean(a >= b));
                },
                .less => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.boolean(a < b));
                },
                .less_equal => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.boolean(a <= b));
                },
                .add => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.number(a + b));
                },
                .subtract => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.number(a - b));
                },
                .multiply => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.number(a * b));
                },
                .divide => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.number(a / b));
                },
                .modulus => {
                    if (!self.peek(0).is(.number) or !self.peek(1).is(.number)) {
                        self.runtimeError("Operands must be numbers.", .{});
                        return .runtime_error;
                    }
                    const b = self.pop().as.number;
                    const a = self.pop().as.number;
                    self.push(Value.number(@mod(a, b)));
                },
                .negate => {
                    if (!self.peek(0).is(.number)) {
                        self.runtimeError("Operand must be a number.", .{});
                        return .runtime_error;
                    }
                    const top = self.stack_top - 1;
                    top[0].as.number = -top[0].as.number;
                },
                .not => self.push(Value.boolean(isFalsey(self.pop()))),
                .return_ => {
                    self.pop().print();
                    std.debug.print("\n", .{});
                    return .ok;
                },
                else => return .runtime_error,
            }
        }
    }
};
