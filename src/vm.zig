const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Compiler = @import("compiler.zig").Compiler;
const Parser = @import("compiler.zig").Parser;
const debug = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const GcAllocator = @import("memory.zig").GcAllocater;
const value_lib = @import("value.zig");
const Value = value_lib.Value;

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
        self.currentChunk().writeOp(op, self.parser.previous.line);
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

    const BinaryOpFn = *const fn (Value, Value) Value;

    fn binaryOp(self: *Self, binary_op: BinaryOpFn) void {
        const b = self.pop();
        const a = self.pop();
        self.push(binary_op(a, b));
    }

    const BinOps = struct {
        fn add(a: Value, b: Value) Value {
            return a + b;
        }

        fn subtract(a: Value, b: Value) Value {
            return a - b;
        }

        fn multiply(a: Value, b: Value) Value {
            return a * b;
        }

        fn divide(a: Value, b: Value) Value {
            return a / b;
        }

        fn modulus(a: Value, b: Value) Value {
            return @mod(a, b);
        }
    };

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
                    value_lib.printValue(slot[0]);
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
                .add => self.binaryOp(BinOps.add),
                .subtract => self.binaryOp(BinOps.subtract),
                .multiply => self.binaryOp(BinOps.multiply),
                .divide => self.binaryOp(BinOps.divide),
                .modulus => self.binaryOp(BinOps.modulus),
                .negate => {
                    const top = self.stack_top - 1;
                    top[0] = -top[0];
                },
                .return_ => {
                    value_lib.printValue(self.pop());
                    std.debug.print("\n", .{});
                    return .ok;
                },
                else => return .runtime_error,
            }
        }
    }
};
