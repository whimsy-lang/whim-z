const std = @import("std");
const unicode = std.unicode;
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const debug = @import("debug.zig");
const Map = @import("map.zig").Map;
const StringMap = @import("string_map.zig").StringMap;
const value = @import("value.zig");
const Value = value.Value;
const Vm = @import("vm.zig").Vm;

pub const ObjectType = enum {
    class,
    closure,
    function,
    instance,
    list,
    map,
    native,
    range,
    set,
    string,
    upvalue,
};

pub const Object = struct {
    type: ObjectType,
    is_marked: bool = false,

    pub fn is(self: *Object, obj_type: ObjectType) bool {
        return self.type == obj_type;
    }

    pub fn asClass(self: *Object) *ObjClass {
        return @fieldParentPtr(ObjClass, "obj", self);
    }

    pub fn asClosure(self: *Object) *ObjClosure {
        return @fieldParentPtr(ObjClosure, "obj", self);
    }

    pub fn asFunction(self: *Object) *ObjFunction {
        return @fieldParentPtr(ObjFunction, "obj", self);
    }

    pub fn asInstance(self: *Object) *ObjInstance {
        return @fieldParentPtr(ObjInstance, "obj", self);
    }

    pub fn asList(self: *Object) *ObjList {
        return @fieldParentPtr(ObjList, "obj", self);
    }

    pub fn asMap(self: *Object) *ObjMap {
        return @fieldParentPtr(ObjMap, "obj", self);
    }

    pub fn asNative(self: *Object) *ObjNative {
        return @fieldParentPtr(ObjNative, "obj", self);
    }

    pub fn asRange(self: *Object) *ObjRange {
        return @fieldParentPtr(ObjRange, "obj", self);
    }

    pub fn asSet(self: *Object) *ObjSet {
        return @fieldParentPtr(ObjSet, "obj", self);
    }

    pub fn asString(self: *Object) *ObjString {
        return @fieldParentPtr(ObjString, "obj", self);
    }

    pub fn asUpvalue(self: *Object) *ObjUpvalue {
        return @fieldParentPtr(ObjUpvalue, "obj", self);
    }

    pub fn debugPrint(self: *Object) void {
        switch (self.type) {
            .class => if (self.asClass().name) |name| {
                std.debug.print("class {s}", .{name.chars});
            } else {
                std.debug.print("anon class", .{});
            },
            .closure => self.asClosure().function.print(),
            .function => self.asFunction().print(),
            .instance => if (self.asInstance().type.name) |name| {
                std.debug.print("{s} inst", .{name.chars});
            } else {
                std.debug.print("anon inst", .{});
            },
            .list => {
                std.debug.print("(", .{});
                var first = true;
                for (self.asList().items.items) |item| {
                    if (first) {
                        first = false;
                    } else {
                        std.debug.print(", ", .{});
                    }
                    value.debugPrint(item);
                }
                std.debug.print(")", .{});
            },
            .map => {
                std.debug.print("[", .{});
                var first = true;
                for (self.asMap().items.entries) |entry| {
                    if (!value.isEmpty(entry.key)) {
                        if (first) {
                            first = false;
                        } else {
                            std.debug.print(", ", .{});
                        }
                        value.debugPrint(entry.key);
                        std.debug.print(" {s} ", .{if (entry.value.constant) "::" else ":="});
                        value.debugPrint(entry.value.value);
                    }
                }
                std.debug.print("]", .{});
            },
            .native => std.debug.print("<native fn>", .{}),
            .range => {
                const r = self.asRange();
                std.debug.print("{d}{s}{d}", .{ r.start, if (r.inclusive) "..=" else "..", r.end });
                if (r.step != 1) std.debug.print(" by {d}", .{r.step});
            },
            .set => {
                std.debug.print("[", .{});
                var first = true;
                for (self.asSet().items.entries) |entry| {
                    if (!value.isEmpty(entry.key)) {
                        if (first) {
                            first = false;
                        } else {
                            std.debug.print(", ", .{});
                        }
                        value.debugPrint(entry.key);
                    }
                }
                std.debug.print("]", .{});
            },
            .string => std.debug.print("{s}", .{self.asString().chars}),
            .upvalue => std.debug.print("upvalue", .{}),
        }
    }

    pub fn mark(self: *Object, vm: *Vm) void {
        if (self.is_marked) return;

        if (debug.log_gc) {
            std.debug.print("mark {any}: ", .{self.type});
            self.debugPrint();
            std.debug.print("\n", .{});
        }

        self.is_marked = true;

        // don't bother queueing up strings, ranges, or native functions
        // since they do not have references to check
        if (self.is(.string) or self.is(.range) or self.is(.native)) return;

        vm.gc.gray_stack.append(self) catch {
            std.debug.print("Could not allocate memory for garbage collection.", .{});
            std.process.exit(1);
        };
    }

    fn markArray(arr: *std.ArrayList(Value), vm: *Vm) void {
        for (arr.items) |val| value.mark(val, vm);
    }

    pub fn blacken(self: *Object, vm: *Vm) void {
        if (debug.log_gc) {
            std.debug.print("blacken {any}: ", .{self.type});
            self.debugPrint();
            std.debug.print("\n", .{});
        }

        switch (self.type) {
            .class => {
                const cl = self.asClass();
                if (cl.name) |name| name.obj.mark(vm);
                if (cl.super) |super| super.obj.mark(vm);
                cl.fields.mark(vm);
            },
            .closure => {
                const clos = self.asClosure();
                clos.function.obj.mark(vm);
                for (clos.upvalues) |upval| {
                    if (upval) |up| up.obj.mark(vm);
                }
            },
            .function => {
                const func = self.asFunction();
                if (func.name) |name| name.obj.mark(vm);
                markArray(&func.chunk.constants, vm);
            },
            .instance => {
                const inst = self.asInstance();
                inst.type.obj.mark(vm);
                inst.fields.mark(vm);
            },
            .list => markArray(&self.asList().items, vm),
            .map => self.asMap().items.mark(vm),
            .set => self.asSet().items.mark(vm),
            .upvalue => value.mark(self.asUpvalue().closed, vm),
            else => unreachable,
        }
    }
};

pub const ObjClass = struct {
    obj: Object,
    name: ?*ObjString,
    super: ?*ObjClass,
    fields: StringMap,

    pub fn init(vm: *Vm, class_name: *ObjString, super_class: ?*ObjClass, base: bool) *ObjClass {
        if (debug.log_gc) {
            std.debug.print("allocate for class\n", .{});
        }
        const class = vm.allocator.create(ObjClass) catch {
            std.debug.print("Could not allocate memory for class.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&class.obj);

        vm.push(value.class(class));

        class.obj = .{ .type = .class };

        class.name = if (class_name != vm.empty_string) class_name else null;
        class.super = if (!base and super_class == null) vm.class_class.? else super_class;

        class.fields = StringMap.init(vm.allocator);
        _ = class.fields.add(vm.super_string.?, if (class.super) |s| value.class(s) else value.nil(), true);
        _ = class.fields.add(vm.type_string.?, value.nil(), true);

        _ = vm.pop();

        return class;
    }

    pub fn deinit(self: *ObjClass) void {
        self.fields.deinit();
    }
};

pub const ObjClosure = struct {
    obj: Object,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn init(vm: *Vm, func: *ObjFunction) *ObjClosure {
        if (debug.log_gc) {
            std.debug.print("allocate for closure\n", .{});
        }
        const upvalues = vm.allocator.alloc(?*ObjUpvalue, func.upvalue_count) catch {
            std.debug.print("Could not allocate memory for closure.", .{});
            std.process.exit(1);
        };
        var i: usize = 0;
        while (i < func.upvalue_count) : (i += 1) {
            upvalues[i] = null;
        }

        const closure = vm.allocator.create(ObjClosure) catch {
            std.debug.print("Could not allocate memory for closure.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&closure.obj);

        closure.obj = .{ .type = .closure };

        closure.function = func;
        closure.upvalues = upvalues;
        return closure;
    }
};

pub const ObjFunction = struct {
    obj: Object,
    arity: usize,
    upvalue_count: usize,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn init(vm: *Vm) *ObjFunction {
        if (debug.log_gc) {
            std.debug.print("allocate for function\n", .{});
        }
        const function = vm.allocator.create(ObjFunction) catch {
            std.debug.print("Could not allocate memory for function.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&function.obj);

        vm.push(value.function(function));

        function.obj = .{ .type = .function };

        function.arity = 0;
        function.upvalue_count = 0;
        function.name = null;
        function.chunk = Chunk.init(vm.allocator);

        _ = vm.pop();

        return function;
    }

    pub fn deinit(self: *ObjFunction) void {
        self.chunk.deinit();
    }

    pub fn print(self: *ObjFunction) void {
        if (self.name != null) {
            std.debug.print("<fn {s}>", .{self.name.?.chars});
        } else {
            std.debug.print("<script>", .{});
        }
    }
};

pub const ObjInstance = struct {
    obj: Object,
    type: *ObjClass,
    fields: StringMap,

    pub fn init(vm: *Vm, class: *ObjClass) *ObjInstance {
        if (debug.log_gc) {
            std.debug.print("allocate for instance\n", .{});
        }
        const instance = vm.allocator.create(ObjInstance) catch {
            std.debug.print("Could not allocate memory for instance.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&instance.obj);

        vm.push(value.instance(instance));

        instance.obj = .{ .type = .instance };

        instance.type = class;
        instance.fields = StringMap.init(vm.allocator);
        _ = instance.fields.add(vm.type_string.?, value.class(class), true);

        _ = vm.pop();

        return instance;
    }

    pub fn deinit(self: *ObjInstance) void {
        self.fields.deinit();
    }
};

pub const ObjList = struct {
    obj: Object,
    items: std.ArrayList(Value),

    pub fn init(vm: *Vm) *ObjList {
        if (debug.log_gc) {
            std.debug.print("allocate for list\n", .{});
        }
        const list = vm.allocator.create(ObjList) catch {
            std.debug.print("Could not allocate memory for list.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&list.obj);

        vm.push(value.list(list));

        list.obj = .{ .type = .list };

        list.items = std.ArrayList(Value).init(vm.allocator);

        _ = vm.pop();

        return list;
    }

    pub fn deinit(self: *ObjList) void {
        self.items.deinit();
    }
};

pub const ObjMap = struct {
    obj: Object,
    items: Map,

    pub fn init(vm: *Vm) *ObjMap {
        if (debug.log_gc) {
            std.debug.print("allocate for map\n", .{});
        }
        const map = vm.allocator.create(ObjMap) catch {
            std.debug.print("Could not allocate memory for map.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&map.obj);

        vm.push(value.map(map));

        map.obj = .{ .type = .map };

        map.items = Map.init(vm.allocator);

        _ = vm.pop();

        return map;
    }

    pub fn deinit(self: *ObjMap) void {
        self.items.deinit();
    }
};

pub const NativeFn = *const fn (*Vm, []Value) Value;

pub const ObjNative = struct {
    obj: Object,
    function: NativeFn,

    pub fn init(vm: *Vm, native_fn: NativeFn) *ObjNative {
        if (debug.log_gc) {
            std.debug.print("allocate for native fn\n", .{});
        }
        const func = vm.allocator.create(ObjNative) catch {
            std.debug.print("Could not allocate memory for function.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&func.obj);

        func.obj = .{ .type = .native };

        func.function = native_fn;
        return func;
    }
};

pub const ObjRange = struct {
    obj: Object,
    start: f64,
    end: f64,
    step: f64,
    inclusive: bool,

    pub fn init(vm: *Vm, start_val: f64, end_val: f64, step_val: f64, inclusive_val: bool) *ObjRange {
        if (debug.log_gc) {
            std.debug.print("allocate for range\n", .{});
        }
        const range = vm.allocator.create(ObjRange) catch {
            std.debug.print("Could not allocate memory for range.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&range.obj);

        range.obj = .{ .type = .range };

        range.start = start_val;
        range.end = end_val;
        range.step = step_val;
        range.inclusive = inclusive_val;
        return range;
    }
};

pub const ObjSet = struct {
    obj: Object,
    items: Map,

    pub fn init(vm: *Vm) *ObjSet {
        if (debug.log_gc) {
            std.debug.print("allocate for set\n", .{});
        }
        const set = vm.allocator.create(ObjSet) catch {
            std.debug.print("Could not allocate memory for set.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&set.obj);

        vm.push(value.set(set));

        set.obj = .{ .type = .set };

        set.items = Map.init(vm.allocator);

        _ = vm.pop();

        return set;
    }

    pub fn deinit(self: *ObjSet) void {
        self.items.deinit();
    }
};

pub const ObjString = struct {
    obj: Object,
    chars: []const u8,
    length: isize,
    hash: u32,

    fn init(vm: *Vm, chars: []const u8, hash: u32) *ObjString {
        if (debug.log_gc) {
            std.debug.print("allocate for string\n", .{});
        }
        const string = vm.allocator.create(ObjString) catch {
            std.debug.print("Could not allocate memory for string.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&string.obj);

        vm.push(value.string(string));

        string.obj = .{ .type = .string };

        var len: isize = 0;
        var i: usize = 0;
        while (i < chars.len) : (len += 1) {
            i += unicode.utf8ByteSequenceLength(chars[i]) catch {
                std.debug.print("Invalid character encoding.", .{});
                std.process.exit(1);
            };
        }

        string.chars = chars;
        string.length = len;
        string.hash = hash;
        _ = vm.strings.set(string, value.nil());

        _ = vm.pop();

        return string;
    }

    pub fn deinit(self: *ObjString, allocator: Allocator) void {
        allocator.free(self.chars);
    }

    const Range = struct {
        start: usize,
        end: usize,
    };

    // get byte range based on utf8 indices
    pub fn byteRange(self: *ObjString, start: usize, end: usize) Range {
        var br = Range{ .start = 0, .end = 0 };

        var i: usize = 0;
        while (br.start < self.chars.len) : (i += 1) {
            if (i == start) break;
            br.start += unicode.utf8ByteSequenceLength(self.chars[br.start]) catch {
                std.debug.print("Invalid character encoding.", .{});
                std.process.exit(1);
            };
        }

        br.end = br.start;
        while (br.end < self.chars.len) : (i += 1) {
            if (i == end) break;
            br.end += unicode.utf8ByteSequenceLength(self.chars[br.end]) catch {
                std.debug.print("Invalid character encoding.", .{});
                std.process.exit(1);
            };
        }

        return br;
    }

    // get utf8 character index from byte index
    pub fn utf8Index(self: *ObjString, b_index: usize) ?usize {
        var index: usize = 0;
        var i: usize = 0;
        while (i < self.chars.len) : (index += 1) {
            if (i == b_index) return index;
            i += unicode.utf8ByteSequenceLength(self.chars[i]) catch {
                std.debug.print("Invalid character encoding.", .{});
                std.process.exit(1);
            };
        }
        return null;
    }

    pub fn take(vm: *Vm, chars: []const u8) *ObjString {
        const hash = value.calcHash(chars);
        const interned = vm.strings.findString(chars, hash);
        if (interned) |intr| {
            vm.allocator.free(chars);
            return intr;
        }

        return init(vm, chars, hash);
    }

    pub fn copy(vm: *Vm, chars: []const u8) *ObjString {
        const hash = value.calcHash(chars);
        const interned = vm.strings.findString(chars, hash);
        if (interned) |intr| return intr;

        const heap_chars = vm.allocator.alloc(u8, chars.len) catch {
            std.debug.print("Could not allocate memory for string.", .{});
            std.process.exit(1);
        };
        std.mem.copy(u8, heap_chars, chars);
        return init(vm, heap_chars, hash);
    }

    pub fn copyEscape(vm: *Vm, chars: []const u8) *ObjString {
        // count actual characters
        var escaped_len: usize = 0;
        var i: usize = 0;
        while (i < chars.len) : (i += 1) {
            if (chars[i] == '\\') i += 1;
            escaped_len += 1;
        }

        // use the base copy string if no escaped characters
        if (escaped_len == chars.len) return copy(vm, chars);

        // allocate the actual length
        const heap_chars = vm.allocator.alloc(u8, escaped_len) catch {
            std.debug.print("Could not allocate memory for string.", .{});
            std.process.exit(1);
        };
        var index: usize = 0;
        i = 0;
        while (i < chars.len) : (i += 1) {
            if (chars[i] == '\\') {
                i += 1;
                heap_chars[index] = switch (chars[i]) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    else => chars[i],
                };
            } else {
                heap_chars[index] = chars[i];
            }
            index += 1;
        }
        return take(vm, heap_chars);
    }
};

pub const ObjUpvalue = struct {
    obj: Object,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn init(vm: *Vm, slot: *Value) *ObjUpvalue {
        if (debug.log_gc) {
            std.debug.print("allocate for upvalue\n", .{});
        }
        const upvalue = vm.allocator.create(ObjUpvalue) catch {
            std.debug.print("Could not allocate memory for upvalue.", .{});
            std.process.exit(1);
        };
        vm.registerObject(&upvalue.obj);

        upvalue.obj = .{ .type = .upvalue };

        upvalue.location = slot;
        upvalue.closed = value.nil();
        upvalue.next = null;
        return upvalue;
    }
};
