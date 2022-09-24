const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
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

    chunk: *Chunk,
    ip: [*]u8,
    stack: [stack_max]Value,
    stack_top: [*]Value,

    pub fn init(self: *Self) void {
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

    pub fn interpret(self: *Self, chunk: *Chunk) InterpretResult {
        self.chunk = chunk;
        self.ip = chunk.code.items.ptr;
        return self.run();
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
                    return InterpretResult.ok;
                },
                else => return InterpretResult.runtime_error,
            }
        }
    }
};
