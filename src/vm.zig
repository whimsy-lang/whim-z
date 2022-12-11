const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Compiler = @import("compiler.zig").Compiler;
const Parser = @import("compiler.zig").Parser;
const debug = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const GcAllocator = @import("memory.zig").GcAllocater;
const NativeFn = @import("object.zig").NativeFn;
const ObjClass = @import("object.zig").ObjClass;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjInstance = @import("object.zig").ObjInstance;
const ObjList = @import("object.zig").ObjList;
const ObjMap = @import("object.zig").ObjMap;
const ObjNative = @import("object.zig").ObjNative;
const ObjRange = @import("object.zig").ObjRange;
const ObjSet = @import("object.zig").ObjSet;
const ObjString = @import("object.zig").ObjString;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const whimsy_std = @import("std.zig");
const StringMap = @import("string_map.zig").StringMap;
const Value = @import("value.zig").Value;
const ValueContainer = @import("value.zig").ValueContainer;

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
    globals: StringMap,
    strings: StringMap,
    open_upvalues: ?*ObjUpvalue,

    empty_string: ?*ObjString,
    init_string: ?*ObjString,
    type_string: ?*ObjString,
    super_string: ?*ObjString,

    bool_class: ?*ObjClass,
    class_class: ?*ObjClass,
    function_class: ?*ObjClass,
    list_class: ?*ObjClass,
    map_class: ?*ObjClass,
    nil_class: ?*ObjClass,
    number_class: ?*ObjClass,
    range_class: ?*ObjClass,
    set_class: ?*ObjClass,
    string_class: ?*ObjClass,

    frames: [frames_max]CallFrame,
    frame_count: usize,
    stack: [stack_max]Value,
    stack_top: [*]Value,
    has_native_error: bool,

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
        self.globals = StringMap.init(self.allocator);
        self.strings = StringMap.init(self.allocator);

        // strings
        self.empty_string = null;
        self.init_string = null;
        self.type_string = null;
        self.super_string = null;

        // classes
        self.bool_class = null;
        self.class_class = null;
        self.function_class = null;
        self.list_class = null;
        self.map_class = null;
        self.nil_class = null;
        self.number_class = null;
        self.range_class = null;
        self.set_class = null;
        self.string_class = null;

        self.empty_string = ObjString.copy(self, "");
        self.init_string = ObjString.copy(self, "init");
        self.type_string = ObjString.copy(self, "type");
        self.super_string = ObjString.copy(self, "super");

        whimsy_std.register(self);
    }

    pub fn deinit(self: *Vm) void {
        self.globals.deinit();
        self.strings.deinit();

        // strings
        self.empty_string = null;
        self.init_string = null;
        self.type_string = null;
        self.super_string = null;

        // classes
        self.bool_class = null;
        self.class_class = null;
        self.function_class = null;
        self.list_class = null;
        self.map_class = null;
        self.nil_class = null;
        self.number_class = null;
        self.range_class = null;
        self.set_class = null;
        self.string_class = null;

        GcAllocator.freeObjects(self);
        self.gc.deinit();
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

    pub fn peek(self: *Vm, offset: usize) Value {
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

    pub fn nativeError(self: *Vm, comptime fmt: []const u8, args: anytype) Value {
        self.has_native_error = true;
        const chars = std.fmt.allocPrint(self.allocator, fmt, args) catch {
            std.debug.print("Could not allocate memory for error.", .{});
            std.process.exit(1);
        };
        return Value.string(ObjString.take(self, chars));
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
            .class => {
                var class: ?*ObjClass = callee.asClass();
                if (class == self.list_class) {
                    const list = ObjList.init(self);
                    (self.stack_top - (arg_count + 1))[0] = Value.list(list);
                    list.items.appendSlice((self.stack_top - arg_count)[0..arg_count]) catch {
                        std.debug.print("Could not allocate memory for list.", .{});
                        std.process.exit(1);
                    };
                    self.stack_top -= arg_count;
                    return true;
                }
                if (class == self.map_class) {
                    if (arg_count != 0) {
                        self.runtimeError("Expected 0 arguments but got {d}.", .{arg_count});
                        return false;
                    }
                    const map = ObjMap.init(self);
                    (self.stack_top - 1)[0] = Value.map(map);
                    return true;
                }
                if (class == self.set_class) {
                    const set = ObjSet.init(self);
                    (self.stack_top - (arg_count + 1))[0] = Value.set(set);
                    for ((self.stack_top - arg_count)[0..arg_count]) |val| {
                        _ = set.items.add(val, Value.nil(), true);
                    }
                    self.stack_top -= arg_count;
                    return true;
                }
                if (class == self.bool_class or
                    class == self.class_class or
                    class == self.function_class or
                    class == self.nil_class or
                    class == self.number_class or
                    class == self.range_class or
                    class == self.string_class)
                {
                    self.runtimeError("Cannot use an initializer on a primitive type.", .{});
                    return false;
                }

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
                self.has_native_error = false;
                const native = callee.asNative().function;
                const result = native(self, (self.stack_top - arg_count)[0..arg_count]);
                self.stack_top -= arg_count + 1;
                self.push(result);
                if (self.has_native_error) {
                    self.runtimeError("{s}", .{result.asString().chars});
                    return false;
                }
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
                    return self.call(method.asClosure(), arg_count, true);
                }
                current = cur.super;
            }

            self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return false;
        } else if (receiver.hasStdClass()) {
            const std_class = receiver.stdClass(self);
            var class: ?*ObjClass = if (receiver.is(.class)) receiver.asClass() else std_class;

            // type
            if (name == self.type_string) {
                const value = Value.class(std_class);
                (self.stack_top - (arg_count + 1))[0] = value;
                return self.callValue(value, arg_count);
            }

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

    fn checkIs(self: *Vm) bool {
        const b = self.pop();
        const a = self.pop();

        if (!b.is(.class)) {
            self.runtimeError("Right operand of 'is' must be a class.", .{});
            return false;
        }

        const target = b.asClass();
        var class: ?*ObjClass = null;
        if (a.is(.instance)) {
            class = a.asInstance().type;
        } else if (a.is(.class)) {
            class = a.asClass();
            // myClass is std.class
            if (target == self.class_class) {
                self.push(Value.boolean(true));
                return true;
            }
        } else if (a.hasStdClass()) {
            class = a.stdClass(self);
        } else {
            self.runtimeError("Left operand of 'is' must by a class or instance.", .{});
            return false;
        }

        while (class) |cl| {
            if (cl == target) {
                self.push(Value.boolean(true));
                return true;
            }
            class = cl.super;
        }

        self.push(Value.boolean(false));
        return true;
    }

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

    fn defineOnValue(self: *Vm, object: Value, key: Value, value: Value, constant: bool) bool {
        return switch (object.getType()) {
            .class => defineOnStringMap(self, &object.asClass().fields, key, value, constant),
            .instance => defineOnStringMap(self, &object.asInstance().fields, key, value, constant),
            .map => defineOnMap(self, object.asMap(), key, value, constant),
            else => {
                self.runtimeError("Only classes, instances, and maps have properties.", .{});
                return false;
            },
        };
    }

    fn defineOnMap(self: *Vm, map: *ObjMap, key: Value, value: Value, constant: bool) bool {
        if (!map.items.add(key, value, constant)) {
            self.runtimeError("Map already contains key.", .{});
            return false;
        }
        return true;
    }

    fn defineOnStringMap(self: *Vm, str_map: *StringMap, key: Value, value: Value, constant: bool) bool {
        if (!key.is(.string)) {
            self.runtimeError("Key must be a string.", .{});
            return false;
        }

        const key_str = key.asString();
        if (!str_map.add(key_str, value, constant)) {
            self.runtimeError("Property '{s}' already exists.", .{key_str.chars});
            return false;
        }

        return true;
    }

    fn getOnValue(self: *Vm, object: Value, key: Value, pop_count: usize) bool {
        return switch (object.getType()) {
            .list => getOnList(self, object.asList(), key, pop_count),
            .map => getOnMap(self, object.asMap(), key, pop_count),
            .set => getOnSet(self, object.asSet(), key, pop_count),
            .string => getOnString(self, object.asString(), key, pop_count),
            else => {
                // class/instance
                if (!key.is(.string)) {
                    self.runtimeError("Class and instance keys must be a string.", .{});
                    return false;
                }

                const key_str = key.asString();

                var class: ?*ObjClass = null;
                if (object.is(.instance)) {
                    const instance = object.asInstance();
                    var value: Value = undefined;
                    if (instance.fields.get(key_str, &value)) {
                        self.stack_top -= pop_count;
                        self.push(value);
                        return true;
                    }
                    class = instance.type;
                } else if (object.hasStdClass()) {
                    const std_class = object.stdClass(self);
                    class = if (object.is(.class)) object.asClass() else std_class;

                    // type
                    if (key_str == self.type_string) {
                        self.stack_top -= pop_count;
                        self.push(Value.class(std_class));
                        return true;
                    }
                } else {
                    self.runtimeError("Only classes and instances have properties.", .{});
                    return false;
                }

                while (class) |cl| {
                    var value: Value = undefined;
                    if (cl.fields.get(key_str, &value)) {
                        self.stack_top -= pop_count;
                        self.push(value);
                        return true;
                    }
                    class = cl.super;
                }

                self.runtimeError("Undefined property '{s}'.", .{key_str.chars});
                return false;
            },
        };
    }

    fn getOnList(self: *Vm, list: *ObjList, key: Value, pop_count: usize) bool {
        return switch (key.getType()) {
            .number => {
                const index = @floatToInt(isize, key.asNumber());
                if (index < 0 or index >= list.items.items.len) {
                    self.runtimeError("Index {d} is out of bounds (0-{d}).", .{ index, list.items.items.len - 1 });
                    return false;
                }
                self.stack_top -= pop_count;
                self.push(list.items.items[@intCast(usize, index)]);
                return true;
            },
            .range => {
                const range = key.asRange();
                if (range.start.is(.number) and range.step == 1) {
                    const start = @floatToInt(isize, range.start.asNumber());
                    var end = @floatToInt(isize, range.end.asNumber());
                    if (range.inclusive) end += 1;

                    if (start < 0 or start > list.items.items.len) {
                        self.runtimeError("Start {d} is out of bounds (0-{d}).", .{ start, list.items.items.len });
                        return false;
                    }
                    if (end < 0 or end > list.items.items.len) {
                        self.runtimeError("End {d} is out of bounds (0-{d}).", .{ end, list.items.items.len });
                        return false;
                    }
                    if (end < start) {
                        self.runtimeError("End {d} is before start {d}.", .{ end, start });
                        return false;
                    }

                    const ustart = @intCast(usize, start);
                    const uend = @intCast(usize, end);

                    const new_list = ObjList.init(self);
                    self.push(Value.list(new_list));
                    new_list.items.appendSlice(list.items.items[ustart..uend]) catch {
                        std.debug.print("Could not allocate memory for list.", .{});
                        std.process.exit(1);
                    };

                    self.stack_top -= (pop_count + 1);
                    self.push(Value.list(new_list));
                    return true;
                } else {
                    self.runtimeError("Only numeric ranges with a step of 1 can be used to index a list.", .{});
                    return false;
                }
            },
            else => {
                self.runtimeError("Only numbers and ranges can be used to index a list.", .{});
                return false;
            },
        };
    }

    fn getOnMap(self: *Vm, map: *ObjMap, key: Value, pop_count: usize) bool {
        var value: Value = undefined;
        if (!map.items.get(key, &value)) {
            self.runtimeError("Map does not contain key.", .{});
            return false;
        }
        self.stack_top -= pop_count;
        self.push(value);
        return true;
    }

    fn getOnSet(self: *Vm, set: *ObjSet, key: Value, pop_count: usize) bool {
        var value: Value = undefined;
        const found = set.items.get(key, &value);
        self.stack_top -= pop_count;
        self.push(Value.boolean(found));
        return true;
    }

    fn getOnString(self: *Vm, string: *ObjString, key: Value, pop_count: usize) bool {
        return switch (key.getType()) {
            .number => {
                const index = @floatToInt(isize, key.asNumber());

                if (index < 0 or index >= string.chars.len) {
                    self.runtimeError("Index {d} is out of bounds (0-{d}).", .{ index, string.chars.len - 1 });
                    return false;
                }

                self.stack_top -= pop_count;
                const uindex = @intCast(usize, index);
                self.push(Value.string(ObjString.copy(self, string.chars[uindex .. uindex + 1])));
                return true;
            },
            .range => {
                const range = key.asRange();
                if (range.start.is(.number) and range.step == 1) {
                    const start = @floatToInt(isize, range.start.asNumber());
                    var end = @floatToInt(isize, range.end.asNumber());
                    if (range.inclusive) end += 1;

                    if (start < 0 or start > string.chars.len) {
                        self.runtimeError("Start {d} is out of bounds (0-{d}).", .{ start, string.chars.len });
                        return false;
                    }
                    if (end < 0 or end > string.chars.len) {
                        self.runtimeError("End {d} is out of bounds (0-{d}).", .{ end, string.chars.len });
                        return false;
                    }
                    if (end < start) {
                        self.runtimeError("End {d} is before start {d}.", .{ end, start });
                        return false;
                    }

                    const ustart = @intCast(usize, start);
                    const uend = @intCast(usize, end);

                    self.stack_top -= pop_count;
                    self.push(Value.string(ObjString.copy(self, string.chars[ustart..uend])));
                    return true;
                } else {
                    self.runtimeError("Only numeric ranges with a step of 1 can be used to index a string.", .{});
                    return false;
                }
            },
            else => {
                self.runtimeError("Only numbers and ranges can be used to index a string.", .{});
                return false;
            },
        };
    }

    fn setOnValue(self: *Vm, object: Value, key: Value, value: Value) bool {
        return switch (object.getType()) {
            .list => setOnList(self, object.asList(), key, value),
            .map => setOnMap(self, object.asMap(), key, value),
            else => {
                // class/instance
                if (!key.is(.string)) {
                    self.runtimeError("Class and instance keys must be a string.", .{});
                    return false;
                }

                const key_str = key.asString();

                var vc: *ValueContainer = undefined;
                var found = false;
                var class: ?*ObjClass = null;
                if (object.is(.instance)) {
                    const instance = object.asInstance();
                    if (instance.fields.getPtr(key_str, &vc)) {
                        found = true;
                    }
                    class = instance.type;
                } else if (object.is(.class)) {
                    class = object.asClass();
                } else {
                    self.runtimeError("Only classes and instances have properties.", .{});
                    return false;
                }

                while (!found and class != null) {
                    if (class.?.fields.getPtr(key_str, &vc)) {
                        found = true;
                    }
                    class = class.?.super;
                }

                if (!found) {
                    self.runtimeError("Undefined property '{s}'.", .{key_str.chars});
                    return false;
                }
                if (vc.constant) {
                    self.runtimeError("Property '{s}' is constant.", .{key_str.chars});
                    return false;
                }

                vc.value = value;
                return true;
            },
        };
    }

    fn setOnList(self: *Vm, list: *ObjList, key: Value, value: Value) bool {
        if (key.is(.number)) {
            const index = @floatToInt(isize, key.asNumber());
            if (index < 0 or index >= list.items.items.len) {
                self.runtimeError("Index {d} is out of bounds (0-{d}).", .{ index, list.items.items.len - 1 });
                return false;
            }
            list.items.items[@intCast(usize, index)] = value;
            return true;
        }

        self.runtimeError("List index must be a number.", .{});
        return false;
    }

    fn setOnMap(self: *Vm, map: *ObjMap, key: Value, value: Value) bool {
        var vc: *ValueContainer = undefined;
        if (!map.items.getPtr(key, &vc)) {
            self.runtimeError("Map does not contain key.", .{});
            return false;
        }
        if (vc.constant) {
            self.runtimeError("Map item is constant.", .{});
            return false;
        }
        vc.value = value;

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

                .dup => self.push(self.peek(0)),
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

                .get_local => {
                    const index = frame.readByte();
                    self.push(frame.slots[index]);
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

                .define_const, .define_const_pop, .define_var, .define_var_pop => {
                    const constant = (op == .define_const) or (op == .define_const_pop);
                    const pop_count: usize = if (op == .define_const_pop or op == .define_var_pop) 3 else 2;
                    if (!self.defineOnValue(self.peek(2), self.peek(1), self.peek(0), constant)) {
                        return .runtime_error;
                    }
                    self.stack_top -= pop_count;
                },
                .get, .get_pop => {
                    const pop_count: usize = if (op == .get_pop) 2 else 0;
                    if (!self.getOnValue(self.peek(1), self.peek(0), pop_count)) {
                        return .runtime_error;
                    }
                },
                .set => {
                    if (!self.setOnValue(self.peek(2), self.peek(1), self.peek(0))) {
                        return .runtime_error;
                    }
                    self.stack_top -= 3;
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
                .is => if (!checkIs(self)) return .runtime_error,
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
                .class => {
                    var super: ?*ObjClass = null;
                    if (self.peek(0).is(.class)) {
                        super = self.peek(0).asClass();
                        if (super == self.bool_class or
                            super == self.class_class or
                            super == self.function_class or
                            super == self.list_class or
                            super == self.map_class or
                            super == self.nil_class or
                            super == self.number_class or
                            super == self.range_class or
                            super == self.set_class or
                            super == self.string_class)
                        {
                            self.runtimeError("Cannot inherit from a builtin type.", .{});
                            return .runtime_error;
                        }
                    } else if (!self.peek(0).is(.nil)) {
                        self.runtimeError("Superclass must be a class or nil.", .{});
                        return .runtime_error;
                    }

                    const class = ObjClass.init(self, frame.readString(), super);
                    _ = self.pop();
                    self.push(Value.class(class));
                },
                .iterate_check => {
                    const offset = frame.readShort();

                    // stack: [object being iterated over] [index]
                    // if index is valid, push the current value onto the stack
                    // if index is not valid, jump by offset

                    const obj = self.peek(1);
                    const index = self.peek(0).asNumber();
                    const uindex = @floatToInt(usize, index);
                    switch (obj.getType()) {
                        .list => {
                            const list = obj.asList();
                            if (uindex < list.items.items.len) {
                                self.push(list.items.items[uindex]);
                            } else {
                                frame.ip += offset;
                            }
                        },
                        .number => {
                            if (index < obj.asNumber()) {
                                self.push(self.peek(0));
                            } else {
                                frame.ip += offset;
                            }
                        },
                        .range => {
                            const range = obj.asRange();
                            if (range.start.is(.number)) {
                                const val = range.start.asNumber() + index * range.step;
                                const end = range.end.asNumber();
                                if ((range.step > 0 and val < end) or (range.step < 0 and val > end) or (range.inclusive and val == end)) {
                                    self.push(Value.number(val));
                                } else {
                                    frame.ip += offset;
                                }
                            } else {
                                const val = @intCast(u8, range.start.asString().chars[0] + @floatToInt(isize, index * range.step));
                                const end = range.end.asString().chars[0];
                                if ((range.step > 0 and val < end) or (range.step < 0 and val > end) or (range.inclusive and val == end)) {
                                    const next_char = [_]u8{val};
                                    self.push(Value.string(ObjString.copy(self, &next_char)));
                                } else {
                                    frame.ip += offset;
                                }
                            }
                        },
                        .string => {
                            const str = obj.asString();
                            if (uindex < str.chars.len) {
                                self.push(Value.string(ObjString.copy(self, str.chars[uindex .. uindex + 1])));
                            } else {
                                frame.ip += offset;
                            }
                        },
                        else => {
                            self.runtimeError("Only lists, numbers, ranges, and strings can be iterated on.", .{});
                            return .runtime_error;
                        },
                    }
                },
                .iterate_next => {
                    // stack: [object being iterated over] [index]
                    self.push(Value.number(self.pop().asNumber() + 1));

                    const offset = frame.readShort();
                    frame.ip -= offset;
                },
                .list => {
                    const count = frame.readByte();
                    const list = ObjList.init(self);
                    self.push(Value.list(list));
                    list.items.appendSlice((self.stack_top - (count + 1))[0..count]) catch {
                        std.debug.print("Could not allocate memory for list.", .{});
                        std.process.exit(1);
                    };
                    self.stack_top -= (count + 1);
                    self.push(Value.list(list));
                },
                .map => self.push(Value.map(ObjMap.init(self))),
                .map_with_const, .map_with_var => {
                    const val = self.peek(0);
                    const key = self.peek(1);
                    const map = ObjMap.init(self);
                    self.push(Value.map(map));
                    _ = map.items.add(key, val, op == .map_with_const);
                    self.stack_top -= 3;
                    self.push(Value.map(map));
                },
                .range, .range_inclusive => {
                    const end = self.peek(0);
                    const start = self.peek(1);
                    if ((start.is(.number) and end.is(.number)) or
                        (start.is(.string) and end.is(.string) and start.asString().chars.len == 1 and end.asString().chars.len == 1))
                    {
                        const range = ObjRange.init(self, start, end, 1, op == .range_inclusive);
                        self.stack_top -= 2;
                        self.push(Value.range(range));
                    } else {
                        self.runtimeError("Start and end must both be numbers or strings of length 1.", .{});
                        return .runtime_error;
                    }
                },
                .range_step, .range_inclusive_step => {
                    const step = self.peek(0);
                    const end = self.peek(1);
                    const start = self.peek(2);
                    if (((start.is(.number) and end.is(.number)) or
                        (start.is(.string) and end.is(.string) and start.asString().chars.len == 1 and end.asString().chars.len == 1)) and step.is(.number))
                    {
                        if (step.asNumber() == 0) {
                            self.runtimeError("Step cannot be 0.", .{});
                            return .runtime_error;
                        }
                        const range = ObjRange.init(self, start, end, step.asNumber(), op == .range_inclusive_step);
                        self.stack_top -= 3;
                        self.push(Value.range(range));
                    } else {
                        self.runtimeError("Start and end must both be numbers or strings of length 1, and step must be a number.", .{});
                        return .runtime_error;
                    }
                },
                .new_set => {
                    const count = frame.readByte();
                    const set = ObjSet.init(self);
                    self.push(Value.set(set));
                    for ((self.stack_top - (count + 1))[0..count]) |val| {
                        _ = set.items.add(val, Value.nil(), true);
                    }
                    self.stack_top -= (count + 1);
                    self.push(Value.set(set));
                },
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
