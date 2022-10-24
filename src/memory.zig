const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("debug.zig");
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

pub const GcAllocater = struct {
    vm: *Vm,
    bytes_allocated: usize,
    next_gc: usize,

    pub fn init(vm: *Vm) GcAllocater {
        return .{
            .vm = vm,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
        };
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
            slot[0].mark();
        }

        vm.globals.mark();
    }

    fn collectGarbage(vm: *Vm) !void {
        if (debug.log_gc) {
            std.debug.print("-- gc begin\n", .{});
        }

        markRoots(vm);

        if (debug.log_gc) {
            std.debug.print("-- gc end\n", .{});
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
            .native => vm.allocator.destroy(object.asNative()),
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
