const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const debug = @import("debug.zig");
const Map = @import("map.zig").Map;
const StringMap = @import("string_map.zig").StringMap;
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

pub const ObjClass = struct {
    name: ?*ObjString,
    super: ?*ObjClass,
    fields: StringMap,
    is_marked: bool,

    pub fn init(vm: *Vm, class_name: *ObjString, super_class: ?*ObjClass) *ObjClass {
        if (debug.log_gc) {
            std.debug.print("allocate for class\n", .{});
        }
        const class = vm.allocator.create(ObjClass) catch {
            std.debug.print("Could not allocate memory for class.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.class(class));

        class.name = if (class_name != vm.empty_string) class_name else null;
        class.super = super_class;
        class.fields = StringMap.init(vm.allocator);
        _ = class.fields.add(vm.super_string.?, if (super_class) |s| Value.class(s) else Value.nil(), true);
        _ = class.fields.add(vm.type_string.?, Value.nil(), true);
        class.is_marked = false;
        return class;
    }

    pub fn deinit(self: *ObjClass) void {
        self.fields.deinit();
    }
};

pub const ObjClosure = struct {
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,
    is_marked: bool,

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
        vm.registerObject(Value.closure(closure));

        closure.function = func;
        closure.upvalues = upvalues;
        closure.is_marked = false;
        return closure;
    }
};

pub const ObjFunction = struct {
    arity: usize,
    upvalue_count: usize,
    chunk: Chunk,
    name: ?*ObjString,
    is_marked: bool,

    pub fn init(vm: *Vm) *ObjFunction {
        if (debug.log_gc) {
            std.debug.print("allocate for function\n", .{});
        }
        const function = vm.allocator.create(ObjFunction) catch {
            std.debug.print("Could not allocate memory for function.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.function(function));

        function.arity = 0;
        function.upvalue_count = 0;
        function.name = null;
        function.chunk = Chunk.init(vm.allocator);
        function.is_marked = false;
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
    type: *ObjClass,
    fields: StringMap,
    is_marked: bool,

    pub fn init(vm: *Vm, class: *ObjClass) *ObjInstance {
        if (debug.log_gc) {
            std.debug.print("allocate for instance\n", .{});
        }
        const instance = vm.allocator.create(ObjInstance) catch {
            std.debug.print("Could not allocate memory for instance.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.instance(instance));

        instance.type = class;
        instance.fields = StringMap.init(vm.allocator);
        _ = instance.fields.add(vm.type_string.?, Value.class(class), true);
        instance.is_marked = false;
        return instance;
    }

    pub fn deinit(self: *ObjInstance) void {
        self.fields.deinit();
    }
};

pub const ObjList = struct {
    items: std.ArrayList(Value),
    is_marked: bool,

    pub fn init(vm: *Vm) *ObjList {
        if (debug.log_gc) {
            std.debug.print("allocate for list\n", .{});
        }
        const list = vm.allocator.create(ObjList) catch {
            std.debug.print("Could not allocate memory for list.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.list(list));

        list.items = std.ArrayList(Value).init(vm.allocator);
        list.is_marked = false;
        return list;
    }

    pub fn deinit(self: *ObjList) void {
        self.items.deinit();
    }
};

pub const ObjMap = struct {
    items: Map,
    is_marked: bool,

    pub fn init(vm: *Vm) *ObjMap {
        if (debug.log_gc) {
            std.debug.print("allocate for map\n", .{});
        }
        const map = vm.allocator.create(ObjMap) catch {
            std.debug.print("Could not allocate memory for map.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.map(map));

        map.items = Map.init(vm.allocator);
        map.is_marked = false;
        return map;
    }

    pub fn deinit(self: *ObjMap) void {
        self.items.deinit();
    }
};

pub const NativeFn = *const fn (*Vm, []Value) Value;

pub const ObjNative = struct {
    function: NativeFn,
    is_marked: bool,

    pub fn init(vm: *Vm, native_fn: NativeFn) *ObjNative {
        if (debug.log_gc) {
            std.debug.print("allocate for native fn\n", .{});
        }
        const func = vm.allocator.create(ObjNative) catch {
            std.debug.print("Could not allocate memory for function.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.native(func));

        func.function = native_fn;
        func.is_marked = false;
        return func;
    }
};

pub const ObjRange = struct {
    start: Value,
    end: Value,
    step: f64,
    inclusive: bool,
    is_marked: bool,

    pub fn init(vm: *Vm, start_val: Value, end_val: Value, step_val: f64, inclusive_val: bool) *ObjRange {
        if (debug.log_gc) {
            std.debug.print("allocate for range\n", .{});
        }
        const range = vm.allocator.create(ObjRange) catch {
            std.debug.print("Could not allocate memory for range.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.range(range));

        range.start = start_val;
        range.end = end_val;
        range.step = step_val;
        range.inclusive = inclusive_val;
        range.is_marked = false;
        return range;
    }
};

pub const ObjSet = struct {
    items: Map,
    is_marked: bool,

    pub fn init(vm: *Vm) *ObjSet {
        if (debug.log_gc) {
            std.debug.print("allocate for set\n", .{});
        }
        const set = vm.allocator.create(ObjSet) catch {
            std.debug.print("Could not allocate memory for set.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.set(set));

        set.items = Map.init(vm.allocator);
        set.is_marked = false;
        return set;
    }

    pub fn deinit(self: *ObjSet) void {
        self.items.deinit();
    }
};

pub const ObjString = struct {
    chars: []const u8,
    hash: u32,
    is_marked: bool,

    fn init(vm: *Vm, chars: []const u8, hash: u32) *ObjString {
        if (debug.log_gc) {
            std.debug.print("allocate for string\n", .{});
        }
        const string = vm.allocator.create(ObjString) catch {
            std.debug.print("Could not allocate memory for string.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.string(string));

        string.chars = chars;
        string.hash = hash;
        string.is_marked = false;

        vm.push(Value.string(string));
        _ = vm.strings.set(string, Value.nil());
        _ = vm.pop();

        return string;
    }

    pub fn deinit(self: *ObjString, allocator: Allocator) void {
        allocator.free(self.chars);
    }

    pub fn take(vm: *Vm, chars: []const u8) *ObjString {
        const hash = Value.calcHash(chars);
        const interned = vm.strings.findString(chars, hash);
        if (interned) |intr| {
            vm.allocator.free(chars);
            return intr;
        }

        return init(vm, chars, hash);
    }

    pub fn copy(vm: *Vm, chars: []const u8) *ObjString {
        const hash = Value.calcHash(chars);
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
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
    is_marked: bool,

    pub fn init(vm: *Vm, slot: *Value) *ObjUpvalue {
        if (debug.log_gc) {
            std.debug.print("allocate for upvalue\n", .{});
        }
        const upvalue = vm.allocator.create(ObjUpvalue) catch {
            std.debug.print("Could not allocate memory for upvalue.", .{});
            std.process.exit(1);
        };
        vm.registerObject(Value.upvalue(upvalue));

        upvalue.location = slot;
        upvalue.closed = Value.nil();
        upvalue.next = null;
        upvalue.is_marked = false;
        return upvalue;
    }
};
