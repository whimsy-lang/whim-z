const std = @import("std");
const Allocator = std.mem.Allocator;

const Compiler = @import("compiler.zig").Compiler;
const debug = @import("debug.zig");
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

pub const GcAllocater = struct {
    const heap_grow_factor = 2;

    vm: *Vm,
    bytes_allocated: usize,
    next_gc: usize,
    gray_stack: std.ArrayList(Value),

    pub fn init(vm: *Vm) GcAllocater {
        return .{
            .vm = vm,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
            .gray_stack = std.ArrayList(Value).init(vm.parent_allocator),
        };
    }

    pub fn deinit(self: *GcAllocater) void {
        self.gray_stack.deinit();
    }

    pub fn allocator(self: *GcAllocater) Allocator {
        return Allocator.init(self, alloc, resize, free);
    }

    fn alloc(self: *GcAllocater, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) error{OutOfMemory}![]u8 {
        if ((self.bytes_allocated + len > self.next_gc) or debug.stress_gc) {
            try collectGarbage(self.vm);
        }
        const out = try self.vm.parent_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
        const before = self.bytes_allocated;
        self.bytes_allocated += out.len;
        if (debug.log_gc) {
            std.debug.print("  {d} -> {d}\n", .{ before, self.bytes_allocated });
        }
        return out;
    }

    fn resize(self: *GcAllocater, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
        if (new_len > buf.len) {
            if ((self.bytes_allocated + (new_len - buf.len) > self.next_gc) or debug.stress_gc) {
                collectGarbage(self.vm) catch {
                    return null;
                };
            }
        }

        if (self.vm.parent_allocator.rawResize(buf, buf_align, new_len, len_align, ret_addr)) |resized_len| {
            const before = self.bytes_allocated;
            if (resized_len > buf.len) {
                self.bytes_allocated += resized_len - buf.len;
            } else {
                self.bytes_allocated -= buf.len - resized_len;
            }
            if (debug.log_gc) {
                std.debug.print("  {d} -> {d}\n", .{ before, self.bytes_allocated });
            }
            return resized_len;
        }

        return null;
    }

    fn free(self: *GcAllocater, buf: []u8, buf_align: u29, ret_addr: usize) void {
        self.vm.parent_allocator.rawFree(buf, buf_align, ret_addr);
        const before = self.bytes_allocated;
        self.bytes_allocated -= buf.len;
        if (debug.log_gc) {
            std.debug.print("  {d} -> {d}\n", .{ before, self.bytes_allocated });
        }
    }

    fn markRoots(vm: *Vm) void {
        var slot: [*]Value = &vm.stack;
        while (@ptrToInt(slot) < @ptrToInt(vm.stack_top)) : (slot += 1) {
            slot[0].mark(vm);
        }

        var i: usize = 0;
        while (i < vm.frame_count) : (i += 1) {
            Value.closure(vm.frames[i].closure).mark(vm);
        }

        var upvalue = vm.open_upvalues;
        while (upvalue) |up| {
            Value.upvalue(up).mark(vm);
            upvalue = up.next;
        }

        vm.globals.mark(vm);

        Compiler.markRoots(vm);

        if (vm.empty_string) |s| Value.string(s).mark(vm);
        if (vm.init_string) |s| Value.string(s).mark(vm);
        if (vm.type_string) |s| Value.string(s).mark(vm);
        if (vm.super_string) |s| Value.string(s).mark(vm);

        if (vm.bool_class) |c| Value.class(c).mark(vm);
        if (vm.class_class) |c| Value.class(c).mark(vm);
        if (vm.function_class) |c| Value.class(c).mark(vm);
        if (vm.list_class) |c| Value.class(c).mark(vm);
        if (vm.nil_class) |c| Value.class(c).mark(vm);
        if (vm.number_class) |c| Value.class(c).mark(vm);
        if (vm.range_class) |c| Value.class(c).mark(vm);
        if (vm.string_class) |c| Value.class(c).mark(vm);
    }

    fn traceReferences(vm: *Vm) void {
        while (vm.gc.gray_stack.items.len > 0) {
            const val = vm.gc.gray_stack.pop();
            val.blacken(vm);
        }
    }

    fn sweep(vm: *Vm) void {
        var i: usize = 0;
        while (i < vm.objects.items.len) {
            if (vm.objects.items[i].getMarked()) {
                vm.objects.items[i].setMarked(false);
                i += 1;
            } else {
                const unreached = vm.objects.swapRemove(i);
                freeObject(vm, unreached);
            }
        }
    }

    fn collectGarbage(vm: *Vm) !void {
        const before = vm.gc.bytes_allocated;
        if (debug.log_gc) {
            std.debug.print("-- gc begin\n", .{});
        }

        markRoots(vm);
        traceReferences(vm);
        vm.strings.removeWhite();
        sweep(vm);

        vm.gc.next_gc = vm.gc.bytes_allocated * heap_grow_factor;

        if (debug.log_gc) {
            std.debug.print("-- gc end | collected {d} bytes (from {d} to {d}) next at {d}\n", .{ before - vm.gc.bytes_allocated, before, vm.gc.bytes_allocated, vm.gc.next_gc });
        }
    }

    pub fn freeObjects(vm: *Vm) void {
        for (vm.objects.items) |object| {
            freeObject(vm, object);
        }
    }

    fn freeObject(vm: *Vm, object: Value) void {
        if (debug.log_gc) {
            std.debug.print("free {any}\n", .{object.getType()});
        }
        switch (object.getType()) {
            .class => {
                const class = object.asClass();
                class.deinit();
                vm.allocator.destroy(class);
            },
            .closure => {
                const closure = object.asClosure();
                vm.allocator.free(closure.upvalues);
                vm.allocator.destroy(closure);
            },
            .function => {
                const function = object.asFunction();
                function.deinit();
                vm.allocator.destroy(function);
            },
            .instance => {
                const instance = object.asInstance();
                instance.deinit();
                vm.allocator.destroy(instance);
            },
            .list => {
                const list = object.asList();
                list.deinit();
                vm.allocator.destroy(list);
            },
            .native => vm.allocator.destroy(object.asNative()),
            .range => vm.allocator.destroy(object.asRange()),
            .string => {
                const string = object.asString();
                string.deinit(vm.allocator);
                vm.allocator.destroy(string);
            },
            .upvalue => vm.allocator.destroy(object.asUpvalue()),
            else => unreachable,
        }
    }
};
