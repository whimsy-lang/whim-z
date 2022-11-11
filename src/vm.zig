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
const NativeFn = @import("object.zig").NativeFn;
const ObjBoundMethod = @import("object.zig").ObjBoundMethod;
const ObjClass = @import("object.zig").ObjClass;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjInstance = @import("object.zig").ObjInstance;
const ObjNative = @import("object.zig").ObjNative;
const ObjString = @import("object.zig").ObjString;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const Value = @import("value.zig").Value;

pub const InterpretResult = enum {
    ok,
    compile_error,
    runtime_error,
};

pub const Vm = struct {
    const u8_count = std.math.maxInt(u8) + 1;
    const frames_max = 64;
    const stack_max = frames_max * u8_count;

    const CallFrame = struct {
        closure: *ObjClosure,
        ip: [*]u8,
        slots: [*]Value,
        pop_one: bool,

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
            return self.closure.function.chunk.constants.items[self.readByte()];
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
    open_upvalues: ?*ObjUpvalue,

    empty_string: ?*ObjString,
    init_string: ?*ObjString,
    type_string: ?*ObjString,
    super_string: ?*ObjString,

    frames: [frames_max]CallFrame,
    frame_count: usize,
    stack: [stack_max]Value,
    stack_top: [*]Value,

    lexer: Lexer,
    parser: Parser,
    compiler: ?*Compiler,

    pub fn init(self: *Vm, allocator: Allocator) void {
        self.parent_allocator = allocator;
        self.gc = GcAllocator.init(self);
        self.allocator = self.gc.allocator();
        self.objects = std.ArrayList(Value).init(self.parent_allocator);

        self.compiler = null;

        self.resetStack();
        self.globals = Map.init(self.allocator);
        self.strings = Map.init(self.allocator);

        self.empty_string = null;
        self.init_string = null;
        self.type_string = null;
        self.super_string = null;

        self.empty_string = ObjString.copy(self, "");
        self.init_string = ObjString.copy(self, "init");
        self.type_string = ObjString.copy(self, "type");
        self.super_string = ObjString.copy(self, "super");

        self.defineNative("print", nativePrint);
        self.defineNative("time", nativeTime);
    }

    pub fn deinit(self: *Vm) void {
        self.globals.deinit();
        self.strings.deinit();

        self.empty_string = null;
        self.init_string = null;
        self.type_string = null;
        self.super_string = null;

        GcAllocator.freeObjects(self);
        self.gc.deinit();
    }

    fn nativePrint(values: []Value) Value {
        for (values) |value| {
            value.print();
        }
        std.debug.print("\n", .{});
        return Value.nil();
    }

    fn nativeTime(values: []Value) Value {
        _ = values;
        const time = @intToFloat(f64, std.time.nanoTimestamp()) / std.time.ns_per_s;
        return Value.number(time);
    }

    pub fn registerObject(self: *Vm, object: Value) void {
        self.objects.append(object) catch {
            std.debug.print("Could not allocate memory to track object.", .{});
            std.process.exit(1);
        };
    }

    fn resetStack(self: *Vm) void {
        self.stack_top = &self.stack;
        self.frame_count = 0;
        self.open_upvalues = null;
    }

    fn runtimeError(self: *Vm, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        var i: isize = @intCast(isize, self.frame_count) - 1;
        while (i >= 0) : (i -= 1) {
            const frame = &self.frames[@intCast(usize, i)];
            const function = frame.closure.function;
            const instruction = @ptrToInt(frame.ip) - @ptrToInt(function.chunk.code.items.ptr) - 1;
            std.debug.print("[line {d}] in ", .{function.chunk.lines.items[instruction]});
            if (function.name == null) {
                std.debug.print("{s}\n", .{if (i == 0) "script" else "fn()"});
            } else {
                std.debug.print("{s}()\n", .{function.name.?.chars});
            }
        }

        self.resetStack();
    }

    fn defineNative(self: *Vm, name: []const u8, native_fn: NativeFn) void {
        self.push(Value.string(ObjString.copy(self, name)));
        self.push(Value.native(ObjNative.init(self, native_fn)));
        _ = self.globals.set(self.peek(1).asString(), self.peek(0));
        _ = self.pop();
        _ = self.pop();
    }

    fn peek(self: *Vm, offset: usize) Value {
        return (self.stack_top - (offset + 1))[0];
    }

    pub fn push(self: *Vm, value: Value) void {
        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *Vm) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    pub fn currentChunk(self: *Vm) *Chunk {
        return &self.compiler.?.function.?.chunk;
    }

    pub fn emitByte(self: *Vm, byte: u8) void {
        self.currentChunk().write(byte, self.parser.previous.line);
    }

    pub fn emitOp(self: *Vm, op: OpCode) void {
        self.currentChunk().write(@enumToInt(op), self.parser.previous.line);
    }

    pub fn emitOpByte(self: *Vm, op: OpCode, byte: u8) void {
        self.emitOp(op);
        self.emitByte(byte);
    }

    fn call(self: *Vm, closure: *ObjClosure, arg_count: u8, pop_one: bool) bool {
        if (arg_count != closure.function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.", .{ closure.function.arity, arg_count });
            return false;
        }

        if (self.frame_count == frames_max) {
            self.runtimeError("Stack overflow.", .{});
            return false;
        }

        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = closure.function.chunk.code.items.ptr;
        frame.slots = self.stack_top - arg_count;
        frame.pop_one = pop_one;
        return true;
    }

    fn callValue(self: *Vm, callee: Value, arg_count: u8) bool {
        switch (callee.getType()) {
            .bound_method => {
                const bound = callee.asBoundMethod();
                (self.stack_top - (arg_count + 1))[0] = bound.receiver;
                return self.call(bound.method, arg_count + 1, false);
            },
            .class => {
                var class: ?*ObjClass = callee.asClass();
                (self.stack_top - (arg_count + 1))[0] = Value.instance(ObjInstance.init(self, class.?));

                var initializer: Value = undefined;
                while (class) |cl| {
                    if (cl.fields.get(self.init_string.?, &initializer)) {
                        return self.call(initializer.asClosure(), arg_count + 1, false);
                    }
                    class = cl.super;
                }

                if (arg_count != 0) {
                    self.runtimeError("Expected 0 arguments but got {d}.", .{arg_count});
                    return false;
                }

                return true;
            },
            .closure => return self.call(callee.asClosure(), arg_count, true),
            .native => {
                const native = callee.asNative().function;
                const result = native((self.stack_top - arg_count)[0..arg_count]);
                self.stack_top -= arg_count + 1;
                self.push(result);
                return true;
            },
            else => {},
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn invoke(self: *Vm, name: *ObjString, arg_count: u8) bool {
        const receiver = self.peek(arg_count);

        if (receiver.is(.instance)) {
            const instance = receiver.asInstance();

            var value: Value = undefined;
            if (instance.fields.get(name, &value)) {
                (self.stack_top - (arg_count + 1))[0] = value;
                return self.callValue(value, arg_count);
            }

            var current: ?*ObjClass = instance.type;
            var method: Value = undefined;
            while (current) |cur| {
                if (cur.fields.get(name, &method)) {
                    return self.call(method.asClosure(), arg_count + 1, false);
                }
                current = cur.super;
            }

            self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return false;
        } else if (receiver.is(.class)) {
            var class: ?*ObjClass = receiver.asClass();

            var value: Value = undefined;
            while (class) |cl| {
                if (cl.fields.get(name, &value)) {
                    (self.stack_top - (arg_count + 1))[0] = value;
                    return self.callValue(value, arg_count);
                }
                class = cl.super;
            }

            self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return false;
        }
        self.runtimeError("Only classes and instances have properties.", .{});
        return false;
    }

    fn captureUpvalue(self: *Vm, local: *Value) *ObjUpvalue {
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.open_upvalues;
        while (upvalue != null and @ptrToInt(upvalue.?.location) > @ptrToInt(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) {
            return upvalue.?;
        }

        const created_upvalue = ObjUpvalue.init(self, local);
        created_upvalue.next = upvalue;

        if (prev_upvalue == null) {
            self.open_upvalues = created_upvalue;
        } else {
            prev_upvalue.?.next = created_upvalue;
        }

        return created_upvalue;
    }

    fn closeUpvalues(self: *Vm, last: [*]Value) void {
        while (self.open_upvalues != null and @ptrToInt(self.open_upvalues.?.location) >= @ptrToInt(last)) {
            const upvalue = self.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
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

        fn remainder(a: f64, b: f64) Value {
            return Value.number(@rem(a, b));
        }
    };

    fn concatenate(self: *Vm) void {
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

    fn defineProperty(self: *Vm, name: *ObjString, object: Value, value: Value, constant: bool) bool {
        var fields: *Map = undefined;
        if (object.is(.instance)) {
            const instance = object.asInstance();
            fields = &instance.fields;

            if (name == self.type_string) {
                if (value.is(.class)) {
                    instance.type = value.asClass();
                } else {
                    self.runtimeError("Instance type must be a class.", .{});
                    return false;
                }
            }
        } else if (object.is(.class)) {
            const class = object.asClass();
            fields = &class.fields;

            if (name == self.super_string) {
                if (value.is(.class)) {
                    const super = value.asClass();
                    if (class != super) {
                        class.super = super;
                    } else {
                        self.runtimeError("Class cannot be its own superclass.", .{});
                        return false;
                    }
                } else {
                    self.runtimeError("Superclass must be a class.", .{});
                    return false;
                }
            }
        } else {
            self.runtimeError("Only classes and instances have properties.", .{});
            return false;
        }

        if (!fields.add(name, value, constant)) {
            self.runtimeError("Property '{s}' already exists.", .{name.chars});
            return false;
        }

        return true;
    }

    fn getProperty(self: *Vm, name: *ObjString, object: Value, do_pop: bool) bool {
        var class: ?*ObjClass = null;
        var bind = false;
        if (object.is(.instance)) {
            const instance = object.asInstance();

            if (name == self.type_string) {
                if (do_pop) _ = self.pop();
                self.push(Value.class(instance.type));
                return true;
            }

            var value: Value = undefined;
            if (instance.fields.get(name, &value)) {
                if (do_pop) _ = self.pop();
                self.push(value);
                return true;
            }
            class = instance.type;
            bind = true;
        } else if (object.is(.class)) {
            class = object.asClass();
        } else {
            self.runtimeError("Only classes and instances have properties.", .{});
            return false;
        }

        while (class) |cl| {
            if (name == self.super_string and cl.super != null) {
                if (do_pop) _ = self.pop();
                self.push(Value.class(cl.super.?));
                return true;
            }

            var value: Value = undefined;
            if (cl.fields.get(name, &value)) {
                if (bind and value.is(.closure)) {
                    // bind method
                    const bound = ObjBoundMethod.init(self, object, value.asClosure());
                    if (do_pop) _ = self.pop();
                    self.push(Value.boundMethod(bound));
                } else {
                    if (do_pop) _ = self.pop();
                    self.push(value);
                }
                return true;
            }
            class = cl.super;
        }

        self.runtimeError("Undefined property '{s}'.", .{name.chars});
        return false;
    }

    fn setProperty(self: *Vm, name: *ObjString, object: Value, value: Value) bool {
        var current: *ValueContainer = undefined;
        var found = false;
        var class: ?*ObjClass = null;
        if (object.is(.instance)) {
            const instance = object.asInstance();
            if (instance.fields.getPtr(name, &current)) {
                found = true;
                if (name == self.type_string) {
                    if (value.is(.class)) {
                        instance.type = value.asClass();
                    } else {
                        self.runtimeError("Instance type must be a class.", .{});
                        return false;
                    }
                }
            }
            class = instance.type;
        } else if (object.is(.class)) {
            class = object.asClass();
        }

        while (!found and class != null) {
            if (class.?.fields.getPtr(name, &current)) {
                found = true;
                if (name == self.super_string) {
                    if (value.is(.class)) {
                        const super = value.asClass();
                        if (class != super) {
                            class.?.super = super;
                        } else {
                            self.runtimeError("Class cannot be its own superclass.", .{});
                            return false;
                        }
                    } else {
                        self.runtimeError("Superclass must be a class.", .{});
                        return false;
                    }
                }
            }
            class = class.?.super;
        }

        if (!found) {
            self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return false;
        }
        if (current.constant) {
            self.runtimeError("Property '{s}' is constant.", .{name.chars});
            return false;
        }

        current.value = value;
        return true;
    }

    fn run(self: *Vm) InterpretResult {
        var frame = &self.frames[self.frame_count - 1];

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
                _ = debug.disassembleInstruction(&frame.closure.function.chunk, @ptrToInt(frame.ip) - @ptrToInt(frame.closure.function.chunk.code.items.ptr));
            }

            const instruction = frame.readByte();
            const op = @intToEnum(OpCode, instruction);
            switch (op) {
                .constant => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },
                .nil => self.push(Value.nil()),
                .true => self.push(Value.boolean(true)),
                .false => self.push(Value.boolean(false)),
                .num_n1 => self.push(Value.number(-1)),
                .num_0 => self.push(Value.number(0)),
                .num_1 => self.push(Value.number(1)),
                .num_2 => self.push(Value.number(2)),
                .num_3 => self.push(Value.number(3)),
                .num_4 => self.push(Value.number(4)),
                .num_5 => self.push(Value.number(5)),
                .num_6 => self.push(Value.number(6)),
                .num_7 => self.push(Value.number(7)),
                .num_8 => self.push(Value.number(8)),
                .pop => _ = self.pop(),
                .define_global_const, .define_global_var => {
                    const name = frame.readString();
                    if (!self.globals.add(name, self.peek(0), op == .define_global_const)) {
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
                .get_local_0, .get_local_1, .get_local_2, .get_local_3, .get_local_4, .get_local_5, .get_local_6, .get_local_7, .get_local_8, .get_local_9, .get_local_10, .get_local_11, .get_local_12, .get_local_13, .get_local_14, .get_local_15 => {
                    const index = @enumToInt(op) - @enumToInt(OpCode.get_local_0);
                    self.push(frame.slots[index]);
                },
                .get_local => {
                    const index = frame.readByte();
                    self.push(frame.slots[index]);
                },
                .set_local_0, .set_local_1, .set_local_2, .set_local_3, .set_local_4, .set_local_5, .set_local_6, .set_local_7, .set_local_8, .set_local_9, .set_local_10, .set_local_11, .set_local_12, .set_local_13, .set_local_14, .set_local_15 => {
                    const index = @enumToInt(op) - @enumToInt(OpCode.set_local_0);
                    frame.slots[index] = self.pop();
                },
                .set_local => {
                    const index = frame.readByte();
                    frame.slots[index] = self.pop();
                },
                .get_upvalue => {
                    const index = frame.readByte();
                    self.push(frame.closure.upvalues[index].?.location.*);
                },
                .set_upvalue => {
                    const index = frame.readByte();
                    frame.closure.upvalues[index].?.location.* = self.pop();
                },
                .define_property_const, .define_property_const_pop, .define_property_var, .define_property_var_pop => {
                    const name = frame.readString();
                    const constant = (op == .define_property_const) or (op == .define_property_const_pop);
                    const do_pop = (op == .define_property_const_pop) or (op == .define_property_var_pop);
                    if (!self.defineProperty(name, self.peek(1), self.peek(0), constant)) {
                        return .runtime_error;
                    }
                    _ = self.pop();
                    if (do_pop) _ = self.pop();
                },
                .get_property, .get_property_pop => {
                    const name = frame.readString();
                    if (!self.getProperty(name, self.peek(0), op == .get_property_pop)) {
                        return .runtime_error;
                    }
                },
                .set_property => {
                    const name = frame.readString();
                    if (!self.setProperty(name, self.peek(1), self.peek(0))) {
                        return .runtime_error;
                    }
                    _ = self.pop();
                    _ = self.pop();
                },
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
                .remainder => if (!NumBinaryOp.run(self, NumBinaryOp.remainder)) return .runtime_error,
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
                .call => {
                    const arg_count = frame.readByte();
                    if (!self.callValue(self.peek(arg_count), arg_count)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .invoke => {
                    const name = frame.readString();
                    const arg_count = frame.readByte();
                    if (!self.invoke(name, arg_count)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .closure => {
                    const function = frame.readConstant().asFunction();
                    const closure = ObjClosure.init(self, function);
                    self.push(Value.closure(closure));

                    var i: usize = 0;
                    while (i < closure.upvalues.len) : (i += 1) {
                        const is_local = frame.readByte();
                        const index = frame.readByte();
                        if (is_local == 1) {
                            closure.upvalues[i] = self.captureUpvalue(&frame.slots[index]);
                        } else {
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }
                },
                .close_upvalue => {
                    self.closeUpvalues(self.stack_top - 1);
                    _ = self.pop();
                },
                .return_ => {
                    var new_top = frame.slots;
                    if (frame.pop_one) new_top -= 1;

                    const result = self.pop();
                    self.closeUpvalues(new_top);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return .ok;
                    }

                    self.stack_top = new_top;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                },
                .class => self.push(Value.class(ObjClass.init(self, frame.readString()))),
            }
        }
    }

    pub fn interpret(self: *Vm, source: [:0]const u8) InterpretResult {
        const function = Compiler.compile(self, source);
        if (function == null) return .compile_error;

        self.push(Value.function(function.?));
        const closure = ObjClosure.init(self, function.?);
        _ = self.pop();
        self.push(Value.closure(closure));
        _ = self.call(closure, 0, true);

        return self.run();
    }
};
