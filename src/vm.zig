const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Compiler = @import("compiler.zig").Compiler;
const Parser = @import("compiler.zig").Parser;
const debug = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const Map = @import("map.zig").Map;
const ValueContainer = @import("map.zig").ValueContainer;
const GcAllocator = @import("memory.zig").GcAllocater;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const Vm = struct {
    const Self = @This();
    const u8_count = std.math.maxInt(u8) + 1;
    const frames_max = 64;
    const stack_max = frames_max * u8_count;

    const CallFrame = struct {
        function: *ObjFunction,
        ip: [*]u8,
        slots: [*]Value,

        // todo - compare performance to increment and then returning self.ip[-1]
        fn readByte(self: *CallFrame) u8 {
            const value = self.ip[0];
            self.ip += 1;
            return value;
        }

        fn readShort(self: *CallFrame) u16 {
            const value = (@as(u16, self.ip[0]) << 8) | (self.ip[1]);
            self.ip += 2;
            return value;
        }

        fn readConstant(self: *CallFrame) Value {
            return self.function.chunk.constants.items[self.readByte()];
        }

        fn readString(self: *CallFrame) *ObjString {
            return self.readConstant().asString();
        }
    };

    parent_allocator: Allocator,
    gc: GcAllocator,
    allocator: Allocator,
    objects: std.ArrayList(Value),
    globals: Map,
    strings: Map,

    frames: [frames_max]CallFrame,
    frame_count: usize,
    stack: [stack_max]Value,
    stack_top: [*]Value,

    lexer: Lexer,
    parser: Parser,
    compiler: *Compiler,

    pub fn init(self: *Self, allocator: Allocator) void {
        self.parent_allocator = allocator;
        self.gc = GcAllocator.init(self.parent_allocator);
        self.allocator = self.gc.allocator();
        self.objects = std.ArrayList(Value).init(self.parent_allocator);
        self.globals = Map.init(self.allocator);
        self.strings = Map.init(self.allocator);
        self.resetStack();
    }

    pub fn deinit(self: *Self) void {
        GcAllocator.freeObjects(self);
        self.globals.deinit();
        self.strings.deinit();
    }

    pub fn registerObject(self: *Self, object: Value) void {
        self.objects.append(object) catch {
            std.debug.print("Could not allocate memory to track object.", .{});
            std.process.exit(1);
        };
    }

    fn resetStack(self: *Self) void {
        self.stack_top = &self.stack;
        self.frame_count = 0;
    }

    fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        const frame = &self.frames[self.frame_count - 1];
        const instruction = @ptrToInt(frame.ip) - @ptrToInt(frame.function.chunk.code.items.ptr) - 1;
        const line = frame.function.chunk.lines.items[instruction];
        std.debug.print("[line {d}] in script\n", .{line});
        self.resetStack();
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
        return &self.compiler.function.?.chunk;
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

    const NumBinaryOp = struct {
        const NumBinaryOpFn = *const fn (f64, f64) Value;

        // todo - test if inlining or comptime for op_fn makes a difference
        fn run(vm: *Vm, op_fn: NumBinaryOpFn) bool {
            if (!vm.peek(0).is(.number) or !vm.peek(1).is(.number)) {
                vm.runtimeError("Operands must be numbers.", .{});
                return false;
            }
            const b = vm.pop().asNumber();
            const a = vm.pop().asNumber();
            vm.push(op_fn(a, b));
            return true;
        }

        fn greater(a: f64, b: f64) Value {
            return Value.boolean(a > b);
        }

        fn greaterEqual(a: f64, b: f64) Value {
            return Value.boolean(a >= b);
        }

        fn less(a: f64, b: f64) Value {
            return Value.boolean(a < b);
        }

        fn lessEqual(a: f64, b: f64) Value {
            return Value.boolean(a <= b);
        }

        fn subtract(a: f64, b: f64) Value {
            return Value.number(a - b);
        }

        fn multiply(a: f64, b: f64) Value {
            return Value.number(a * b);
        }

        fn divide(a: f64, b: f64) Value {
            return Value.number(a / b);
        }

        fn modulus(a: f64, b: f64) Value {
            return Value.number(@rem(a, b));
        }
    };

    const GlobalNumAssignBinaryOp = struct {
        const GlobalNumAssignBinaryOpFn = *const fn (*ValueContainer, f64) void;

        // todo - test if inlining or comptime for op_fn makes a difference
        fn run(vm: *Vm, frame: *CallFrame, op_fn: GlobalNumAssignBinaryOpFn) bool {
            const name = frame.readString();
            var value: *ValueContainer = undefined;
            if (!vm.globals.getPtr(name, &value)) {
                vm.runtimeError("Undefined variable '{s}'.", .{name.chars});
                return false;
            }
            if (value.constant) {
                vm.runtimeError("Global '{s}' is constant.", .{name.chars});
                return false;
            }
            if (!value.value.is(.number) or !vm.peek(0).is(.number)) {
                vm.runtimeError("Operands must be numbers.", .{});
                return false;
            }
            op_fn(value, vm.pop().asNumber());
            return true;
        }

        fn subtract(a: *ValueContainer, b: f64) void {
            a.value.as.number -= b;
        }

        fn multiply(a: *ValueContainer, b: f64) void {
            a.value.as.number *= b;
        }

        fn divide(a: *ValueContainer, b: f64) void {
            a.value.as.number /= b;
        }

        fn modulus(a: *ValueContainer, b: f64) void {
            a.value.as.number = @rem(a.value.asNumber(), b);
        }
    };

    const LocalNumAssignBinaryOp = struct {
        const LocalNumAssignBinaryOpFn = *const fn (*Value, f64) void;

        // todo - compare performance with inlining and comptime
        fn run(vm: *Vm, frame: *CallFrame, op_fn: LocalNumAssignBinaryOpFn) bool {
            const index = frame.readByte();
            const value = &frame.slots[index];
            if (!value.is(.number) or !vm.peek(0).is(.number)) {
                vm.runtimeError("Operands must be numbers.", .{});
                return false;
            }
            op_fn(value, vm.pop().asNumber());
            return true;
        }

        fn subtract(a: *Value, b: f64) void {
            a.as.number -= b;
        }

        fn multiply(a: *Value, b: f64) void {
            a.as.number *= b;
        }

        fn divide(a: *Value, b: f64) void {
            a.as.number /= b;
        }

        fn modulus(a: *Value, b: f64) void {
            a.as.number = @rem(a.asNumber(), b);
        }
    };

    fn concatenate(self: *Self) void {
        const b = self.peek(0).asString();
        const a = self.peek(1).asString();

        const strings = [_][]const u8{ a.chars, b.chars };

        const heap_chars = std.mem.concat(self.allocator, u8, &strings) catch {
            std.debug.print("Could not allocate memory for string.", .{});
            std.process.exit(1);
        };

        const result = ObjString.take(self, heap_chars);

        _ = self.pop();
        _ = self.pop();
        self.push(Value.string(result));
    }

    fn concatValue(self: *Self, a: *ObjString) *ObjString {
        const b = self.peek(0).asString();

        const strings = [_][]const u8{ a.chars, b.chars };

        const heap_chars = std.mem.concat(self.allocator, u8, &strings) catch {
            std.debug.print("Could not allocate memory for string.", .{});
            std.process.exit(1);
        };

        const result = ObjString.take(self, heap_chars);

        _ = self.pop();
        return result;
    }

    pub fn interpret(self: *Self, source: [:0]const u8) InterpretResult {
        const function = Compiler.compile(self, source);
        if (function == null) return .compile_error;

        self.push(Value.function(function.?));
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.function = function.?;
        frame.ip = function.?.chunk.code.items.ptr;
        frame.slots = self.stack_top;

        return self.run();
    }

    fn run(self: *Self) InterpretResult {
        const frame = &self.frames[self.frame_count - 1];

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
                _ = debug.disassembleInstruction(&frame.function.chunk, @ptrToInt(frame.ip) - @ptrToInt(frame.function.chunk.code.items.ptr));
            }

            const instruction = frame.readByte();
            switch (@intToEnum(OpCode, instruction)) {
                .constant => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },
                .nil => self.push(Value.nil()),
                .true => self.push(Value.boolean(true)),
                .false => self.push(Value.boolean(false)),
                .pop => _ = self.pop(),
                .define_global_const => {
                    const name = frame.readString();
                    if (!self.globals.add(name, self.peek(0), true)) {
                        self.runtimeError("Global '{s}' already exists.", .{name.chars});
                        return .runtime_error;
                    }
                    _ = self.pop();
                },
                .define_global_var => {
                    const name = frame.readString();
                    if (!self.globals.add(name, self.peek(0), false)) {
                        self.runtimeError("Global '{s}' already exists.", .{name.chars});
                        return .runtime_error;
                    }
                    _ = self.pop();
                },
                .get_global => {
                    const name = frame.readString();
                    var value: Value = undefined;
                    if (!self.globals.get(name, &value)) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .runtime_error;
                    }
                    self.push(value);
                },
                .set_global => {
                    const name = frame.readString();
                    var value: *ValueContainer = undefined;
                    if (!self.globals.getPtr(name, &value)) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .runtime_error;
                    }
                    if (value.constant) {
                        self.runtimeError("Global '{s}' is constant.", .{name.chars});
                        return .runtime_error;
                    }
                    value.value = self.pop();
                },
                .add_set_global => {
                    const name = frame.readString();
                    var value: *ValueContainer = undefined;
                    if (!self.globals.getPtr(name, &value)) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .runtime_error;
                    }
                    if (value.constant) {
                        self.runtimeError("Global '{s}' is constant.", .{name.chars});
                        return .runtime_error;
                    }
                    if (value.value.is(.number) and self.peek(0).is(.number)) {
                        value.value.as.number += self.pop().asNumber();
                    } else if (value.value.is(.string) and self.peek(0).is(.string)) {
                        value.value.as.string = self.concatValue(value.value.asString());
                    } else {
                        self.runtimeError("Operands must both be numbers or strings.", .{});
                        return .runtime_error;
                    }
                },
                .subtract_set_global => if (!GlobalNumAssignBinaryOp.run(self, frame, GlobalNumAssignBinaryOp.subtract)) return .runtime_error,
                .multiply_set_global => if (!GlobalNumAssignBinaryOp.run(self, frame, GlobalNumAssignBinaryOp.multiply)) return .runtime_error,
                .divide_set_global => if (!GlobalNumAssignBinaryOp.run(self, frame, GlobalNumAssignBinaryOp.divide)) return .runtime_error,
                .modulus_set_global => if (!GlobalNumAssignBinaryOp.run(self, frame, GlobalNumAssignBinaryOp.modulus)) return .runtime_error,
                .get_local => {
                    const index = frame.readByte();
                    self.push(frame.slots[index]);
                },
                .set_local => {
                    const index = frame.readByte();
                    frame.slots[index] = self.pop();
                },
                .add_set_local => {
                    const index = frame.readByte();
                    const value = &frame.slots[index];
                    if (value.is(.number) and self.peek(0).is(.number)) {
                        value.as.number += self.pop().asNumber();
                    } else if (value.is(.string) and self.peek(0).is(.string)) {
                        value.as.string = self.concatValue(value.asString());
                    } else {
                        self.runtimeError("Operands must both be numbers or strings.", .{});
                        return .runtime_error;
                    }
                },
                .subtract_set_local => if (!LocalNumAssignBinaryOp.run(self, frame, LocalNumAssignBinaryOp.subtract)) return .runtime_error,
                .multiply_set_local => if (!LocalNumAssignBinaryOp.run(self, frame, LocalNumAssignBinaryOp.multiply)) return .runtime_error,
                .divide_set_local => if (!LocalNumAssignBinaryOp.run(self, frame, LocalNumAssignBinaryOp.divide)) return .runtime_error,
                .modulus_set_local => if (!LocalNumAssignBinaryOp.run(self, frame, LocalNumAssignBinaryOp.modulus)) return .runtime_error,
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
                .greater => if (!NumBinaryOp.run(self, NumBinaryOp.greater)) return .runtime_error,
                .greater_equal => if (!NumBinaryOp.run(self, NumBinaryOp.greaterEqual)) return .runtime_error,
                .less => if (!NumBinaryOp.run(self, NumBinaryOp.less)) return .runtime_error,
                .less_equal => if (!NumBinaryOp.run(self, NumBinaryOp.lessEqual)) return .runtime_error,
                .add => {
                    if (self.peek(0).is(.number) and self.peek(1).is(.number)) {
                        const b = self.pop().asNumber();
                        const a = self.pop().asNumber();
                        self.push(Value.number(a + b));
                    } else if (self.peek(0).is(.string) and self.peek(1).is(.string)) {
                        self.concatenate();
                    } else {
                        self.runtimeError("Operands must both be numbers or strings.", .{});
                        return .runtime_error;
                    }
                },
                .subtract => if (!NumBinaryOp.run(self, NumBinaryOp.subtract)) return .runtime_error,
                .multiply => if (!NumBinaryOp.run(self, NumBinaryOp.multiply)) return .runtime_error,
                .divide => if (!NumBinaryOp.run(self, NumBinaryOp.divide)) return .runtime_error,
                .modulus => if (!NumBinaryOp.run(self, NumBinaryOp.modulus)) return .runtime_error,
                .negate => {
                    if (!self.peek(0).is(.number)) {
                        self.runtimeError("Operand must be a number.", .{});
                        return .runtime_error;
                    }
                    self.push(Value.number(-self.pop().asNumber()));
                },
                .not => self.push(Value.boolean(self.pop().isFalsey())),
                .jump => {
                    const offset = frame.readShort();
                    frame.ip += offset;
                },
                .jump_back => {
                    const offset = frame.readShort();
                    frame.ip -= offset;
                },
                .jump_if_true => {
                    const offset = frame.readShort();
                    if (!self.peek(0).isFalsey()) frame.ip += offset;
                },
                .jump_if_false => {
                    const offset = frame.readShort();
                    if (self.peek(0).isFalsey()) frame.ip += offset;
                },
                .jump_if_true_pop => {
                    const offset = frame.readShort();
                    if (!self.pop().isFalsey()) frame.ip += offset;
                },
                .jump_if_false_pop => {
                    const offset = frame.readShort();
                    if (self.pop().isFalsey()) frame.ip += offset;
                },
                .return_ => {
                    // exit interpreter
                    return .ok;
                },
                else => return .runtime_error,
            }
        }
    }
};
