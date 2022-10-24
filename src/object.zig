const std = @import("std");
const Allocator = std.mem.Allocator;

const Chunk = @import("chunk.zig").Chunk;
const debug = @import("debug.zig");
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

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

pub const NativeFn = *const fn ([]Value) Value;

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

        _ = vm.strings.set(string, Value.nil());

        return string;
    }

    pub fn deinit(self: *ObjString, allocator: Allocator) void {
        allocator.free(self.chars);
    }

    pub fn take(vm: *Vm, chars: []const u8) *ObjString {
        const hash = calcHash(chars);
        const interned = vm.strings.findString(chars, hash);
        if (interned) |intr| {
            vm.allocator.free(chars);
            return intr;
        }

        return init(vm, chars, hash);
    }

    pub fn copy(vm: *Vm, chars: []const u8) *ObjString {
        const hash = calcHash(chars);
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

    fn calcHash(chars: []const u8) u32 {
        var hash: u32 = 2166136261;
        var i: usize = 0;
        while (i < chars.len) : (i += 1) {
            hash ^= chars[i];
            hash *%= 16777619;
        }
        return hash;
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
