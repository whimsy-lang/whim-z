const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Compiler = @import("compiler.zig").Compiler;
const Parser = @import("compiler.zig").Parser;
const debug = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const GcAllocator = @import("memory.zig").GcAllocater;
const NativeFn = @import("object.zig").NativeFn;
const Object = @import("object.zig").Object;
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
const out = @import("out.zig");
const whimsy_std = @import("std.zig");
const StringMap = @import("string_map.zig").StringMap;
const value = @import("value.zig");
const Value = value.Value;
const ValueContainer = value.ValueContainer;
const vle = @import("vle.zig");

pub const version = "Whimsy 0.1";

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

        fn readByte(self: *CallFrame) u8 {
            const val = self.ip[0];
            self.ip += 1;
            return val;
        }

        fn readShort(self: *CallFrame) u16 {
            const val = (@as(u16, self.ip[0]) << 8) | (self.ip[1]);
            self.ip += 2;
            return val;
        }

        fn readNum(self: *CallFrame) u29 {
            return vle.getIncrement(&self.ip);
        }

        fn readConstant(self: *CallFrame) Value {
            return self.closure.function.chunk.constants.items[self.readNum()];
        }

        fn readString(self: *CallFrame) *ObjString {
            return value.asString(self.readConstant());
        }
    };

    parent_allocator: Allocator,
    gc: GcAllocator,
    allocator: Allocator,
    objects: std.ArrayList(*Object),
    globals: StringMap,
    strings: StringMap,
    open_upvalues: ?*ObjUpvalue,

    empty_string: ?*ObjString,
    init_string: ?*ObjString,
    type_string: ?*ObjString,
    super_string: ?*ObjString,

    greater_string: ?*ObjString,
    greater_equal_string: ?*ObjString,
    less_string: ?*ObjString,
    less_equal_string: ?*ObjString,
    add_string: ?*ObjString,
    subtract_string: ?*ObjString,
    multiply_string: ?*ObjString,
    divide_string: ?*ObjString,
    remainder_string: ?*ObjString,

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
        self.objects = std.ArrayList(*Object).init(self.parent_allocator);

        self.compiler = null;

        self.resetStack();
        self.globals = StringMap.init(self.allocator);
        self.strings = StringMap.init(self.allocator);

        // strings
        self.empty_string = null;
        self.init_string = null;
        self.type_string = null;
        self.super_string = null;

        self.greater_string = null;
        self.greater_equal_string = null;
        self.less_string = null;
        self.less_equal_string = null;
        self.add_string = null;
        self.subtract_string = null;
        self.multiply_string = null;
        self.divide_string = null;
        self.remainder_string = null;

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

        self.greater_string = ObjString.copy(self, ">");
        self.greater_equal_string = ObjString.copy(self, ">=");
        self.less_string = ObjString.copy(self, "<");
        self.less_equal_string = ObjString.copy(self, "<=");
        self.add_string = ObjString.copy(self, "+");
        self.subtract_string = ObjString.copy(self, "-");
        self.multiply_string = ObjString.copy(self, "*");
        self.divide_string = ObjString.copy(self, "/");
        self.remainder_string = ObjString.copy(self, "%");

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

        self.greater_string = null;
        self.greater_equal_string = null;
        self.less_string = null;
        self.less_equal_string = null;
        self.add_string = null;
        self.subtract_string = null;
        self.multiply_string = null;
        self.divide_string = null;
        self.remainder_string = null;

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

        self.freeObjects();
        self.gc.deinit();
    }

    pub fn registerObject(self: *Vm, object: *Object) void {
        self.objects.append(object) catch {
            out.printExit("Could not allocate memory to track object.", .{}, 1);
        };
    }

    pub fn collectGarbage(self: *Vm) void {
        const before = self.gc.bytes_allocated;
        if (debug.log_gc) {
            out.printlnColor("-- gc begin", .{}, 0, 0xff, 0);
        }

        self.markRoots();
        self.gc.traceReferences();
        self.strings.removeWhite();
        self.sweep();

        self.gc.next_gc = self.gc.bytes_allocated * GcAllocator.heap_grow_factor;

        if (debug.log_gc) {
            out.printlnColor("-- gc end | collected {d} bytes (from {d} to {d}) next at {d}", .{ before - self.gc.bytes_allocated, before, self.gc.bytes_allocated, self.gc.next_gc }, 0, 0xff, 0);
        }
    }

    fn markRoots(self: *Vm) void {
        var slot: [*]Value = &self.stack;
        while (@ptrToInt(slot) < @ptrToInt(self.stack_top)) : (slot += 1) {
            value.mark(slot[0], self);
        }

        var i: usize = 0;
        while (i < self.frame_count) : (i += 1) {
            self.frames[i].closure.obj.mark(self);
        }

        var upvalue = self.open_upvalues;
        while (upvalue) |up| {
            up.obj.mark(self);
            upvalue = up.next;
        }

        self.globals.mark(self);

        Compiler.markRoots(self);

        if (self.empty_string) |s| s.obj.mark(self);
        if (self.init_string) |s| s.obj.mark(self);
        if (self.type_string) |s| s.obj.mark(self);
        if (self.super_string) |s| s.obj.mark(self);

        if (self.greater_string) |s| s.obj.mark(self);
        if (self.greater_equal_string) |s| s.obj.mark(self);
        if (self.less_string) |s| s.obj.mark(self);
        if (self.less_equal_string) |s| s.obj.mark(self);
        if (self.add_string) |s| s.obj.mark(self);
        if (self.subtract_string) |s| s.obj.mark(self);
        if (self.multiply_string) |s| s.obj.mark(self);
        if (self.divide_string) |s| s.obj.mark(self);
        if (self.remainder_string) |s| s.obj.mark(self);

        if (self.bool_class) |c| c.obj.mark(self);
        if (self.class_class) |c| c.obj.mark(self);
        if (self.function_class) |c| c.obj.mark(self);
        if (self.list_class) |c| c.obj.mark(self);
        if (self.map_class) |c| c.obj.mark(self);
        if (self.nil_class) |c| c.obj.mark(self);
        if (self.number_class) |c| c.obj.mark(self);
        if (self.range_class) |c| c.obj.mark(self);
        if (self.set_class) |c| c.obj.mark(self);
        if (self.string_class) |c| c.obj.mark(self);
    }

    fn sweep(self: *Vm) void {
        var i: usize = 0;
        while (i < self.objects.items.len) {
            if (self.objects.items[i].is_marked) {
                self.objects.items[i].is_marked = false;
                i += 1;
            } else {
                const unreached = self.objects.swapRemove(i);
                self.freeObject(unreached);
            }
        }
    }

    fn freeObjects(self: *Vm) void {
        for (self.objects.items) |object| {
            self.freeObject(object);
        }
    }

    fn freeObject(self: *Vm, object: *Object) void {
        if (debug.log_gc) {
            out.print("free {any}: ", .{object.type});
            object.print();
            out.println("", .{});
        }
        switch (object.type) {
            .class => {
                const class = object.asClass();
                class.deinit();
                self.allocator.destroy(class);
            },
            .closure => {
                const closure = object.asClosure();
                self.allocator.free(closure.upvalues);
                self.allocator.destroy(closure);
            },
            .function => {
                const function = object.asFunction();
                function.deinit();
                self.allocator.destroy(function);
            },
            .instance => {
                const instance = object.asInstance();
                instance.deinit();
                self.allocator.destroy(instance);
            },
            .list => {
                const list = object.asList();
                list.deinit();
                self.allocator.destroy(list);
            },
            .map => {
                const map = object.asMap();
                map.deinit();
                self.allocator.destroy(map);
            },
            .native => self.allocator.destroy(object.asNative()),
            .range => self.allocator.destroy(object.asRange()),
            .set => {
                const set = object.asSet();
                set.deinit();
                self.allocator.destroy(set);
            },
            .string => {
                const string = object.asString();
                string.deinit(self.allocator);
                self.allocator.destroy(string);
            },
            .upvalue => self.allocator.destroy(object.asUpvalue()),
        }
    }

    fn resetStack(self: *Vm) void {
        self.stack_top = &self.stack;
        self.frame_count = 0;
        self.open_upvalues = null;
    }

    fn runtimeError(self: *Vm, comptime fmt: []const u8, args: anytype) void {
        out.println(fmt, args);

        var i: isize = @intCast(isize, self.frame_count) - 1;
        while (i >= 0) : (i -= 1) {
            const frame = &self.frames[@intCast(usize, i)];
            const function = frame.closure.function;
            const instruction = @ptrToInt(frame.ip) - @ptrToInt(function.chunk.code.items.ptr) - 1;
            out.printColor("[line {d}]", .{function.chunk.getLine(instruction)}, 0xff, 0, 0);
            out.print(" in ", .{});
            if (function.name == null) {
                out.println("{s}", .{if (i == 0) "script" else "fn()"});
            } else {
                out.println("{s}()", .{function.name.?.chars});
            }
        }

        self.resetStack();
    }

    pub fn peek(self: *Vm, offset: usize) Value {
        return (self.stack_top - (offset + 1))[0];
    }

    pub fn push(self: *Vm, val: Value) void {
        self.stack_top[0] = val;
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

    pub fn emitNum(self: *Vm, num: u29) void {
        self.currentChunk().writeVle(num, self.parser.previous.line);
    }

    pub fn emitOpNum(self: *Vm, op: OpCode, num: u29) void {
        self.emitOp(op);
        self.emitNum(num);
    }

    pub fn nativeError(self: *Vm, comptime fmt: []const u8, args: anytype) Value {
        self.has_native_error = true;
        const chars = std.fmt.allocPrint(self.allocator, fmt, args) catch {
            out.printExit("Could not allocate memory for error.", .{}, 1);
        };
        return value.string(ObjString.take(self, chars));
    }

    fn call(self: *Vm, closure: *ObjClosure, arg_count: u29, pop_one: bool) bool {
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

    fn binaryOp(self: *Vm, op: *ObjString) bool {
        const b = self.pop();
        self.push(self.peek(0));
        self.push(b);
        return self.invoke(op, 2);
    }

    fn callValue(self: *Vm, callee: Value, arg_count: u29) bool {
        if (value.isObject(callee)) {
            const obj = value.asObject(callee);
            switch (obj.type) {
                .class => {
                    var class: ?*ObjClass = obj.asClass();
                    if (class == self.list_class) {
                        const list = ObjList.init(self);
                        (self.stack_top - (arg_count + 1))[0] = value.list(list);
                        list.items.appendSlice((self.stack_top - arg_count)[0..arg_count]) catch {
                            out.printExit("Could not allocate memory for list.", .{}, 1);
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
                        (self.stack_top - 1)[0] = value.map(map);
                        return true;
                    }
                    if (class == self.set_class) {
                        const set = ObjSet.init(self);
                        (self.stack_top - (arg_count + 1))[0] = value.set(set);
                        for ((self.stack_top - arg_count)[0..arg_count]) |val| {
                            _ = set.items.add(val, value.nil(), true);
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

                    (self.stack_top - (arg_count + 1))[0] = value.instance(ObjInstance.init(self, class.?));

                    var initializer: Value = undefined;
                    while (class) |cl| {
                        if (cl.fields.get(self.init_string.?, &initializer)) {
                            return self.call(value.asClosure(initializer), arg_count + 1, false);
                        }
                        class = cl.super;
                    }

                    if (arg_count != 0) {
                        self.runtimeError("Expected 0 arguments but got {d}.", .{arg_count});
                        return false;
                    }

                    return true;
                },
                .closure => return self.call(obj.asClosure(), arg_count, true),
                .native => {
                    self.has_native_error = false;
                    const native = obj.asNative().function;
                    const result = native(self, (self.stack_top - arg_count)[0..arg_count]);
                    self.stack_top -= arg_count + 1;
                    self.push(result);
                    if (self.has_native_error) {
                        self.runtimeError("{s}", .{value.asString(result).chars});
                        return false;
                    }
                    return true;
                },
                else => {},
            }
        }
        self.runtimeError("Can only call functions and classes.", .{});
        return false;
    }

    fn invoke(self: *Vm, name: *ObjString, arg_count: u29) bool {
        const receiver = self.peek(arg_count);

        if (value.isObjType(receiver, .instance)) {
            const instance = value.asInstance(receiver);

            var val: Value = undefined;
            if (instance.fields.get(name, &val)) {
                (self.stack_top - (arg_count + 1))[0] = val;
                return self.callValue(val, arg_count);
            }

            var current: ?*ObjClass = instance.type;
            while (current) |cur| {
                if (cur.fields.get(name, &val)) {
                    return self.callValue(val, arg_count);
                }
                current = cur.super;
            }

            self.runtimeError("Undefined property '{s}'.", .{name.chars});
            return false;
        } else if (value.hasStdClass(receiver)) {
            const std_class = value.stdClass(receiver, self);
            var class: ?*ObjClass = if (value.isObjType(receiver, .class)) value.asClass(receiver) else std_class;

            // type
            if (name == self.type_string) {
                const val = value.class(std_class);
                (self.stack_top - (arg_count + 1))[0] = val;
                return self.callValue(val, arg_count);
            }

            var val: Value = undefined;
            while (class) |cl| {
                if (cl.fields.get(name, &val)) {
                    (self.stack_top - (arg_count + 1))[0] = val;
                    return self.callValue(val, arg_count);
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

    fn checkIs(self: *Vm) bool {
        const b = self.pop();
        const a = self.pop();

        if (!value.isObjType(b, .class)) {
            self.runtimeError("Right operand of 'is' must be a class.", .{});
            return false;
        }

        const target = value.asClass(b);
        var class: ?*ObjClass = null;
        if (value.isObjType(a, .instance)) {
            class = value.asInstance(a).type;
        } else if (value.isObjType(a, .class)) {
            class = value.asClass(a);
        } else if (value.hasStdClass(a)) {
            class = value.stdClass(a, self);
        } else {
            self.runtimeError("Left operand of 'is' must by a class or instance.", .{});
            return false;
        }

        while (class) |cl| {
            if (cl == target) {
                self.push(value.boolean(true));
                return true;
            }
            class = cl.super;
        }

        self.push(value.boolean(false));
        return true;
    }

    fn concatenate(self: *Vm) void {
        const b = value.asString(self.peek(0));
        const a = value.asString(self.peek(1));

        const strings = [_][]const u8{ a.chars, b.chars };

        const heap_chars = std.mem.concat(self.allocator, u8, &strings) catch {
            out.printExit("Could not allocate memory for string.", .{}, 1);
        };

        const result = ObjString.take(self, heap_chars);

        _ = self.pop();
        _ = self.pop();
        self.push(value.string(result));
    }

    const NormalizedRangeResult = struct {
        valid: bool,
        start: usize,
        end: usize,
    };

    fn normalizeRange(self: *Vm, start: isize, end: isize, inclusive: bool, length: isize) NormalizedRangeResult {
        var res = NormalizedRangeResult{ .valid = false, .start = 0, .end = 0 };
        if (start < -length or start > length) {
            self.runtimeError("Start {d} is out of bounds ({d} to {d}).", .{ start, -length, length });
            return res;
        }
        if (inclusive and end >= length) {
            self.runtimeError("End {d} is out of bounds ({d} to {d}).", .{ end, -length, length - 1 });
            return res;
        }
        if (end < -length or end > length) {
            self.runtimeError("End {d} is out of bounds ({d} to {d}).", .{ end, -length, length });
            return res;
        }
        res.start = @intCast(usize, if (start < 0) start + length else start);
        res.end = @intCast(usize, if (end < 0) end + length else end);
        if (res.start > res.end) {
            self.runtimeError("Start {d} is after end {d}.", .{ start, end });
            return res;
        }
        if (inclusive) res.end += 1;
        res.valid = true;
        return res;
    }

    fn defineOnValue(self: *Vm, target: Value, key: Value, val: Value, constant: bool) bool {
        if (value.isObject(target)) {
            const obj = value.asObject(target);
            switch (obj.type) {
                .class => return self.defineOnStringMap(&obj.asClass().fields, key, val, constant),
                .instance => return self.defineOnStringMap(&obj.asInstance().fields, key, val, constant),
                .map => return self.defineOnMap(obj.asMap(), key, val, constant),
                else => {},
            }
        }
        self.runtimeError("Only classes, instances, and maps have properties.", .{});
        return false;
    }

    fn defineOnMap(self: *Vm, map: *ObjMap, key: Value, val: Value, constant: bool) bool {
        if (!map.items.add(key, val, constant)) {
            self.runtimeError("Map already contains key.", .{});
            return false;
        }
        return true;
    }

    fn defineOnStringMap(self: *Vm, str_map: *StringMap, key: Value, val: Value, constant: bool) bool {
        if (!value.isObjType(key, .string)) {
            self.runtimeError("Key must be a string.", .{});
            return false;
        }

        const key_str = value.asString(key);
        if (!str_map.add(key_str, val, constant)) {
            self.runtimeError("Property '{s}' already exists.", .{key_str.chars});
            return false;
        }

        return true;
    }

    fn getOnValue(self: *Vm, target: Value, key: Value, pop_count: usize) bool {
        const is_type = value.isObjType(key, .string) and value.asString(key) == self.type_string;

        if (!is_type and value.isObject(target)) {
            const obj = value.asObject(target);
            switch (obj.type) {
                .list => return self.getOnList(obj.asList(), key, pop_count),
                .map => return self.getOnMap(obj.asMap(), key, pop_count),
                .set => return self.getOnSet(obj.asSet(), key, pop_count),
                .string => return self.getOnString(obj.asString(), key, pop_count),
                else => {},
            }
        }

        // class/instance
        if (!value.isObjType(key, .string)) {
            self.runtimeError("Class and instance keys must be a string.", .{});
            return false;
        }

        const key_str = value.asString(key);

        var class: ?*ObjClass = null;
        if (value.isObjType(target, .instance)) {
            const instance = value.asInstance(target);
            var val: Value = undefined;
            if (instance.fields.get(key_str, &val)) {
                self.stack_top -= pop_count;
                self.push(val);
                return true;
            }
            class = instance.type;
        } else if (value.hasStdClass(target)) {
            const std_class = value.stdClass(target, self);
            class = if (value.isObjType(target, .class)) value.asClass(target) else std_class;

            // type
            if (is_type) {
                self.stack_top -= pop_count;
                self.push(value.class(std_class));
                return true;
            }
        } else {
            self.runtimeError("Only classes and instances have properties.", .{});
            return false;
        }

        while (class) |cl| {
            var val: Value = undefined;
            if (cl.fields.get(key_str, &val)) {
                self.stack_top -= pop_count;
                self.push(val);
                return true;
            }
            class = cl.super;
        }

        self.runtimeError("Undefined property '{s}'.", .{key_str.chars});
        return false;
    }

    fn getOnList(self: *Vm, list: *ObjList, key: Value, pop_count: usize) bool {
        if (value.isNumber(key)) {
            var index = @floatToInt(isize, value.asNumber(key));
            const len = @intCast(isize, list.items.items.len);
            if (index < -len or index >= len) {
                self.runtimeError("Index {d} is out of bounds ({d} to {d}).", .{ index, -len, len - 1 });
                return false;
            }
            if (index < 0) index += len;
            self.stack_top -= pop_count;
            self.push(list.items.items[@intCast(usize, index)]);
            return true;
        }
        if (value.isObjType(key, .range)) {
            const range = value.asRange(key);
            if (range.step == 1) {
                const start = @floatToInt(isize, range.start);
                const end = @floatToInt(isize, range.end);
                const len = @intCast(isize, list.items.items.len);

                const norm = self.normalizeRange(start, end, range.inclusive, len);
                if (!norm.valid) return false;

                const new_list = ObjList.init(self);
                self.push(value.list(new_list));
                new_list.items.appendSlice(list.items.items[norm.start..norm.end]) catch {
                    out.printExit("Could not allocate memory for list.", .{}, 1);
                };

                self.stack_top -= (pop_count + 1);
                self.push(value.list(new_list));
                return true;
            }
            self.runtimeError("Only ranges with a step of 1 can be used to index a list.", .{});
            return false;
        }
        self.runtimeError("Only numbers and ranges can be used to index a list.", .{});
        return false;
    }

    fn getOnMap(self: *Vm, map: *ObjMap, key: Value, pop_count: usize) bool {
        var val: Value = undefined;
        if (!map.items.get(key, &val)) {
            self.runtimeError("Map does not contain key.", .{});
            return false;
        }
        self.stack_top -= pop_count;
        self.push(val);
        return true;
    }

    fn getOnSet(self: *Vm, set: *ObjSet, key: Value, pop_count: usize) bool {
        var val: Value = undefined;
        const found = set.items.get(key, &val);
        self.stack_top -= pop_count;
        self.push(value.boolean(found));
        return true;
    }

    fn getOnString(self: *Vm, string: *ObjString, key: Value, pop_count: usize) bool {
        if (value.isNumber(key)) {
            var index = @floatToInt(isize, value.asNumber(key));

            if (index < -string.length or index >= string.length) {
                self.runtimeError("Index {d} is out of bounds ({d} to {d}).", .{ index, -string.length, string.length - 1 });
                return false;
            }

            if (index < 0) index += string.length;
            const uindex = @intCast(usize, index);
            const br = string.byteRange(uindex, uindex + 1);

            self.stack_top -= pop_count;
            self.push(value.string(ObjString.copy(self, string.chars[br.start..br.end])));
            return true;
        }
        if (value.isObjType(key, .range)) {
            const range = value.asRange(key);
            if (range.step == 1) {
                const start = @floatToInt(isize, range.start);
                const end = @floatToInt(isize, range.end);

                const norm = self.normalizeRange(start, end, range.inclusive, string.length);
                if (!norm.valid) return false;

                const br = string.byteRange(norm.start, norm.end);
                self.stack_top -= pop_count;
                self.push(value.string(ObjString.copy(self, string.chars[br.start..br.end])));
                return true;
            }
            self.runtimeError("Only ranges with a step of 1 can be used to index a string.", .{});
            return false;
        }
        self.runtimeError("Only numbers and ranges can be used to index a string.", .{});
        return false;
    }

    fn setOnValue(self: *Vm, target: Value, key: Value, val: Value) bool {
        if (value.isObject(target)) {
            switch (value.asObject(target).type) {
                .list => return self.setOnList(value.asList(target), key, val),
                .map => return self.setOnMap(value.asMap(target), key, val),
                else => {},
            }
        }

        // class/instance
        if (!value.isObjType(key, .string)) {
            self.runtimeError("Class and instance keys must be a string.", .{});
            return false;
        }

        const key_str = value.asString(key);

        var vc: *ValueContainer = undefined;
        var found = false;
        var class: ?*ObjClass = null;
        if (value.isObjType(target, .instance)) {
            const instance = value.asInstance(target);
            if (instance.fields.getPtr(key_str, &vc)) {
                found = true;
            }
            class = instance.type;
        } else if (value.isObjType(target, .class)) {
            class = value.asClass(target);
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

        vc.value = val;
        return true;
    }

    fn setOnList(self: *Vm, list: *ObjList, key: Value, val: Value) bool {
        if (value.isNumber(key)) {
            var index = @floatToInt(isize, value.asNumber(key));
            const len = @intCast(isize, list.items.items.len);
            if (index < -len or index >= len) {
                self.runtimeError("Index {d} is out of bounds ({d} to {d}).", .{ index, -len, len - 1 });
                return false;
            }
            if (index < 0) index += len;
            list.items.items[@intCast(usize, index)] = val;
            return true;
        }
        if (value.isObjType(key, .range)) {
            const range = value.asRange(key);
            if (range.step == 1) {
                const start = @floatToInt(isize, range.start);
                const end = @floatToInt(isize, range.end);
                const len = @intCast(isize, list.items.items.len);

                const norm = self.normalizeRange(start, end, range.inclusive, len);
                if (!norm.valid) return false;

                const new_items = if (value.isObjType(val, .list)) value.asList(val).items.items else &[_]Value{val};
                list.items.replaceRange(norm.start, norm.end - norm.start, new_items) catch {
                    out.printExit("Could not allocate memory for list.", .{}, 1);
                };
                return true;
            }
            self.runtimeError("Only ranges with a step of 1 can be used to index a list.", .{});
            return false;
        }

        self.runtimeError("Only numbers and ranges can be used to index a list.", .{});
        return false;
    }

    fn setOnMap(self: *Vm, map: *ObjMap, key: Value, val: Value) bool {
        var vc: *ValueContainer = undefined;
        if (!map.items.getPtr(key, &vc)) {
            self.runtimeError("Map does not contain key.", .{});
            return false;
        }
        if (vc.constant) {
            self.runtimeError("Map item is constant.", .{});
            return false;
        }
        vc.value = val;

        return true;
    }

    fn run(self: *Vm) InterpretResult {
        var frame = &self.frames[self.frame_count - 1];

        while (true) {
            if (debug.trace_execution) {
                out.print("          ", .{});
                var slot: [*]Value = &self.stack;
                while (@ptrToInt(slot) < @ptrToInt(self.stack_top)) : (slot += 1) {
                    out.print("[ ", .{});
                    value.print(slot[0]);
                    out.print(" ]", .{});
                }
                out.println("", .{});
                _ = debug.disassembleInstruction(&frame.closure.function.chunk, @ptrToInt(frame.ip) - @ptrToInt(frame.closure.function.chunk.code.items.ptr));
            }

            const instruction = frame.readByte();
            const op = @intToEnum(OpCode, instruction);
            switch (op) {
                .constant => {
                    const constant = frame.readConstant();
                    self.push(constant);
                },

                .nil => self.push(value.nil()),
                .true => self.push(value.boolean(true)),
                .false => self.push(value.boolean(false)),

                .num_n1 => self.push(value.number(-1)),
                .num_0 => self.push(value.number(0)),
                .num_1 => self.push(value.number(1)),
                .num_2 => self.push(value.number(2)),
                .num_3 => self.push(value.number(3)),
                .num_4 => self.push(value.number(4)),
                .num_5 => self.push(value.number(5)),
                .num_6 => self.push(value.number(6)),
                .num_7 => self.push(value.number(7)),
                .num_8 => self.push(value.number(8)),
                .num_9 => self.push(value.number(9)),
                .num_10 => self.push(value.number(10)),

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
                    var val: Value = undefined;
                    if (!self.globals.get(name, &val)) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .runtime_error;
                    }
                    self.push(val);
                },
                .set_global => {
                    const name = frame.readString();
                    var vc: *ValueContainer = undefined;
                    if (!self.globals.getPtr(name, &vc)) {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .runtime_error;
                    }
                    if (vc.constant) {
                        self.runtimeError("Global '{s}' is constant.", .{name.chars});
                        return .runtime_error;
                    }
                    vc.value = self.pop();
                },

                .get_local => {
                    const index = frame.readNum();
                    self.push(frame.slots[index]);
                },
                .set_local => {
                    const index = frame.readNum();
                    frame.slots[index] = self.pop();
                },

                .get_upvalue => {
                    const index = frame.readNum();
                    self.push(frame.closure.upvalues[index].?.location.*);
                },
                .set_upvalue => {
                    const index = frame.readNum();
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

                .get_by_const, .get_by_const_pop => {
                    const key = frame.readConstant();
                    const pop_count: usize = if (op == .get_by_const_pop) 1 else 0;
                    if (!self.getOnValue(self.peek(0), key, pop_count)) {
                        return .runtime_error;
                    }
                },
                .set_by_const => {
                    const key = frame.readConstant();
                    if (!self.setOnValue(self.peek(1), key, self.peek(0))) {
                        return .runtime_error;
                    }
                    self.stack_top -= 2;
                },

                .equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(value.boolean(a == b));
                },
                .not_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(value.boolean(a != b));
                },
                .greater => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.boolean(a > b));
                    } else if (self.binaryOp(self.greater_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .greater_equal => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.boolean(a >= b));
                    } else if (self.binaryOp(self.greater_equal_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .less => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.boolean(a < b));
                    } else if (self.binaryOp(self.less_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .less_equal => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.boolean(a <= b));
                    } else if (self.binaryOp(self.less_equal_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .is => if (!self.checkIs()) return .runtime_error,
                .add => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.number(a + b));
                    } else if (value.isObjType(self.peek(0), .string) and value.isObjType(self.peek(1), .string)) {
                        self.concatenate();
                    } else if (self.binaryOp(self.add_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .subtract => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.number(a - b));
                    } else if (self.binaryOp(self.subtract_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .multiply => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.number(a * b));
                    } else if (self.binaryOp(self.multiply_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .divide => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.number(a / b));
                    } else if (self.binaryOp(self.divide_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },
                .remainder => {
                    if (value.isNumber(self.peek(0)) and value.isNumber(self.peek(1))) {
                        const b = value.asNumber(self.pop());
                        const a = value.asNumber(self.pop());
                        self.push(value.number(@rem(a, b)));
                    } else if (self.binaryOp(self.remainder_string.?)) {
                        frame = &self.frames[self.frame_count - 1];
                    } else {
                        return .runtime_error;
                    }
                },

                .negate => {
                    if (!value.isNumber(self.peek(0))) {
                        self.runtimeError("Operand must be a number.", .{});
                        return .runtime_error;
                    }
                    self.push(value.number(-value.asNumber(self.pop())));
                },
                .not => self.push(value.boolean(value.isFalsey(self.pop()))),

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
                    if (!value.isFalsey(self.peek(0))) frame.ip += offset;
                },
                .jump_if_false => {
                    const offset = frame.readShort();
                    if (value.isFalsey(self.peek(0))) frame.ip += offset;
                },
                .jump_if_false_pop => {
                    const offset = frame.readShort();
                    if (value.isFalsey(self.pop())) frame.ip += offset;
                },

                .binary_op => {
                    const binop = frame.readString();
                    if (!self.binaryOp(binop)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .call => {
                    const arg_count = frame.readNum();
                    if (!self.callValue(self.peek(arg_count), arg_count)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },
                .invoke => {
                    const name = frame.readString();
                    const arg_count = frame.readNum();
                    if (!self.invoke(name, arg_count)) {
                        return .runtime_error;
                    }
                    frame = &self.frames[self.frame_count - 1];
                },

                .closure => {
                    const function = value.asFunction(frame.readConstant());
                    const closure = ObjClosure.init(self, function);
                    self.push(value.closure(closure));

                    var i: usize = 0;
                    while (i < closure.upvalues.len) : (i += 1) {
                        const is_local = frame.readByte();
                        const index = frame.readNum();
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
                    if (value.isObjType(self.peek(0), .class)) {
                        super = value.asClass(self.peek(0));
                        if (super == self.bool_class or
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
                    } else if (!value.isNil(self.peek(0))) {
                        self.runtimeError("Superclass must be a class or nil.", .{});
                        return .runtime_error;
                    }

                    const class = ObjClass.init(self, frame.readString(), super, false);
                    _ = self.pop();
                    self.push(value.class(class));
                },
                .iterate_check => {
                    const offset = frame.readShort();

                    // stack: [object being iterated over] [index]
                    // if index is valid, push the current value onto the stack
                    // if index is not valid, jump by offset

                    const iter = self.peek(1);
                    const index = value.asNumber(self.peek(0));
                    const uindex = @floatToInt(usize, index);

                    if (value.isNumber(iter)) {
                        if (index < value.asNumber(iter)) {
                            self.push(self.peek(0));
                        } else {
                            frame.ip += offset;
                        }
                    } else if (value.isObject(iter)) {
                        const obj = value.asObject(iter);
                        switch (obj.type) {
                            .list => {
                                const list = obj.asList();
                                if (uindex < list.items.items.len) {
                                    self.push(list.items.items[uindex]);
                                } else {
                                    frame.ip += offset;
                                }
                            },
                            .range => {
                                const range = obj.asRange();
                                const val = range.start + index * range.step;
                                const positive = range.step > 0;
                                if ((positive and val < range.end) or
                                    (!positive and val > range.end) or
                                    (range.inclusive and val == range.end))
                                {
                                    self.push(value.number(val));
                                } else {
                                    frame.ip += offset;
                                }
                            },
                            .string => {
                                const str = obj.asString();

                                var byte_idx: usize = 0;
                                var length: usize = 0;
                                var i: usize = 0;
                                while (i < str.chars.len) : (length += 1) {
                                    if (length == uindex) byte_idx = i;
                                    i += unicode.utf8ByteSequenceLength(str.chars[i]) catch {
                                        out.printExit("Invalid character encoding.", .{}, 1);
                                    };
                                }

                                if (uindex < length) {
                                    const ch_len = unicode.utf8ByteSequenceLength(str.chars[byte_idx]) catch {
                                        out.printExit("Invalid character encoding.", .{}, 1);
                                    };
                                    self.push(value.string(ObjString.copy(self, str.chars[byte_idx .. byte_idx + ch_len])));
                                } else {
                                    frame.ip += offset;
                                }
                            },
                            else => {
                                self.runtimeError("Only lists, numbers, ranges, and strings can be iterated on.", .{});
                                return .runtime_error;
                            },
                        }
                    } else {
                        self.runtimeError("Only lists, numbers, ranges, and strings can be iterated on.", .{});
                        return .runtime_error;
                    }
                },
                .iterate_next => {
                    // stack: [object being iterated over] [index]
                    self.push(value.number(value.asNumber(self.pop()) + 1));

                    const offset = frame.readShort();
                    frame.ip -= offset;
                },
                .list => {
                    const count = frame.readNum();
                    const list = ObjList.init(self);
                    self.push(value.list(list));
                    list.items.appendSlice((self.stack_top - (count + 1))[0..count]) catch {
                        out.printExit("Could not allocate memory for list.", .{}, 1);
                    };
                    self.stack_top -= (count + 1);
                    self.push(value.list(list));
                },
                .map => self.push(value.map(ObjMap.init(self))),
                .map_with_const, .map_with_var => {
                    const val = self.peek(0);
                    const key = self.peek(1);
                    const map = ObjMap.init(self);
                    self.push(value.map(map));
                    _ = map.items.add(key, val, op == .map_with_const);
                    self.stack_top -= 3;
                    self.push(value.map(map));
                },
                .range, .range_inclusive => {
                    const end = self.peek(0);
                    const start = self.peek(1);
                    if (value.isNumber(start) and value.isNumber(end)) {
                        const range = ObjRange.init(self, value.asNumber(start), value.asNumber(end), 1, op == .range_inclusive);
                        self.stack_top -= 2;
                        self.push(value.range(range));
                    } else {
                        self.runtimeError("Start and end must both be numbers.", .{});
                        return .runtime_error;
                    }
                },
                .range_step, .range_inclusive_step => {
                    const step = self.peek(0);
                    const end = self.peek(1);
                    const start = self.peek(2);
                    if (value.isNumber(start) and value.isNumber(end) and value.isNumber(step)) {
                        if (value.asNumber(step) == 0) {
                            self.runtimeError("Step cannot be 0.", .{});
                            return .runtime_error;
                        }
                        const range = ObjRange.init(self, value.asNumber(start), value.asNumber(end), value.asNumber(step), op == .range_inclusive_step);
                        self.stack_top -= 3;
                        self.push(value.range(range));
                    } else {
                        self.runtimeError("Start, end, and step must all be numbers.", .{});
                        return .runtime_error;
                    }
                },
                .new_set => {
                    const count = frame.readNum();
                    const set = ObjSet.init(self);
                    self.push(value.set(set));
                    for ((self.stack_top - (count + 1))[0..count]) |val| {
                        _ = set.items.add(val, value.nil(), true);
                    }
                    self.stack_top -= (count + 1);
                    self.push(value.set(set));
                },
            }
        }
    }

    pub fn interpret(self: *Vm, source: []const u8) InterpretResult {
        const function = Compiler.compile(self, source);
        if (function == null) return .compile_error;

        self.push(value.function(function.?));
        const closure = ObjClosure.init(self, function.?);
        _ = self.pop();
        self.push(value.closure(closure));
        _ = self.call(closure, 0, true);

        return self.run();
    }
};
