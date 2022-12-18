const std = @import("std");
const Allocator = std.mem.Allocator;

const Compiler = @import("compiler.zig").Compiler;
const debug = @import("debug.zig");
const Object = @import("object.zig").Object;
const ObjString = @import("object.zig").ObjString;
const value = @import("value.zig");
const Value = value.Value;
const Vm = @import("vm.zig").Vm;

pub const GcAllocater = struct {
    const heap_grow_factor = 2;

    vm: *Vm,
    bytes_allocated: usize,
    next_gc: usize,
    gray_stack: std.ArrayList(*Object),

    pub fn init(vm: *Vm) GcAllocater {
        return .{
            .vm = vm,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
            .gray_stack = std.ArrayList(*Object).init(vm.parent_allocator),
        };
    }

    pub fn deinit(self: *GcAllocater) void {
        self.gray_stack.deinit();
    }

    pub fn allocator(self: *GcAllocater) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, log2_ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self = @ptrCast(*GcAllocater, @alignCast(@alignOf(GcAllocater), ctx));

        if ((self.bytes_allocated + len > self.next_gc) or debug.stress_gc) {
            collectGarbage(self.vm);
        }
        const result = self.vm.parent_allocator.rawAlloc(len, log2_ptr_align, ret_addr);
        if (result == null) {
            std.debug.print("alloc failure\n", .{});
            std.process.exit(1);
        } else {
            const before = self.bytes_allocated;
            self.bytes_allocated += len;
            if (debug.log_gc) {
                std.debug.print("  {d} -> {d}\n", .{ before, self.bytes_allocated });
            }
        }
        return result;
    }

    fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self = @ptrCast(*GcAllocater, @alignCast(@alignOf(GcAllocater), ctx));

        if (new_len > buf.len) {
            if ((self.bytes_allocated + (new_len - buf.len) > self.next_gc) or debug.stress_gc) {
                collectGarbage(self.vm);
            }
        }

        if (self.vm.parent_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr)) {
            const before = self.bytes_allocated;
            if (new_len > buf.len) {
                self.bytes_allocated += new_len - buf.len;
            } else {
                self.bytes_allocated -= buf.len - new_len;
            }
            if (debug.log_gc) {
                std.debug.print("  {d} -> {d}\n", .{ before, self.bytes_allocated });
            }
            return true;
        }

        return false;
    }

    fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
        const self = @ptrCast(*GcAllocater, @alignCast(@alignOf(GcAllocater), ctx));

        self.vm.parent_allocator.rawFree(buf, log2_buf_align, ret_addr);
        const before = self.bytes_allocated;
        self.bytes_allocated -= buf.len;
        if (debug.log_gc) {
            std.debug.print("  {d} -> {d}\n", .{ before, self.bytes_allocated });
        }
    }

    fn markRoots(vm: *Vm) void {
        var slot: [*]Value = &vm.stack;
        while (@ptrToInt(slot) < @ptrToInt(vm.stack_top)) : (slot += 1) {
            value.mark(slot[0], vm);
        }

        var i: usize = 0;
        while (i < vm.frame_count) : (i += 1) {
            vm.frames[i].closure.obj.mark(vm);
        }

        var upvalue = vm.open_upvalues;
        while (upvalue) |up| {
            up.obj.mark(vm);
            upvalue = up.next;
        }

        vm.globals.mark(vm);

        Compiler.markRoots(vm);

        if (vm.empty_string) |s| s.obj.mark(vm);
        if (vm.init_string) |s| s.obj.mark(vm);
        if (vm.type_string) |s| s.obj.mark(vm);
        if (vm.super_string) |s| s.obj.mark(vm);

        if (vm.greater_string) |s| s.obj.mark(vm);
        if (vm.greater_equal_string) |s| s.obj.mark(vm);
        if (vm.less_string) |s| s.obj.mark(vm);
        if (vm.less_equal_string) |s| s.obj.mark(vm);
        if (vm.add_string) |s| s.obj.mark(vm);
        if (vm.subtract_string) |s| s.obj.mark(vm);
        if (vm.multiply_string) |s| s.obj.mark(vm);
        if (vm.divide_string) |s| s.obj.mark(vm);
        if (vm.remainder_string) |s| s.obj.mark(vm);

        if (vm.bool_class) |c| c.obj.mark(vm);
        if (vm.class_class) |c| c.obj.mark(vm);
        if (vm.function_class) |c| c.obj.mark(vm);
        if (vm.list_class) |c| c.obj.mark(vm);
        if (vm.map_class) |c| c.obj.mark(vm);
        if (vm.nil_class) |c| c.obj.mark(vm);
        if (vm.number_class) |c| c.obj.mark(vm);
        if (vm.range_class) |c| c.obj.mark(vm);
        if (vm.set_class) |c| c.obj.mark(vm);
        if (vm.string_class) |c| c.obj.mark(vm);
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
            if (vm.objects.items[i].is_marked) {
                vm.objects.items[i].is_marked = false;
                i += 1;
            } else {
                const unreached = vm.objects.swapRemove(i);
                freeObject(vm, unreached);
            }
        }
    }

    fn collectGarbage(vm: *Vm) void {
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

    fn freeObject(vm: *Vm, object: *Object) void {
        if (debug.log_gc) {
            std.debug.print("free {any}: ", .{object.type});
            object.print();
            std.debug.print("\n", .{});
        }
        switch (object.type) {
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
            .map => {
                const map = object.asMap();
                map.deinit();
                vm.allocator.destroy(map);
            },
            .native => vm.allocator.destroy(object.asNative()),
            .range => vm.allocator.destroy(object.asRange()),
            .set => {
                const set = object.asSet();
                set.deinit();
                vm.allocator.destroy(set);
            },
            .string => {
                const string = object.asString();
                string.deinit(vm.allocator);
                vm.allocator.destroy(string);
            },
            .upvalue => vm.allocator.destroy(object.asUpvalue()),
        }
    }
};
