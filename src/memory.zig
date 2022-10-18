const std = @import("std");
const Allocator = std.mem.Allocator;

const debug = @import("debug.zig");
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

pub const GcAllocater = struct {
    const Self = @This();

    parent_allocator: Allocator,
    bytes_allocated: usize,
    next_gc: usize,

    pub fn init(parent_allocator: Allocator) Self {
        return .{
            .parent_allocator = parent_allocator,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
        };
    }

    pub fn allocator(self: *Self) Allocator {
        return Allocator.init(self, alloc, resize, free);
    }

    fn alloc(self: *Self, len: usize, ptr_align: u29, len_align: u29, ret_addr: usize) error{OutOfMemory}![]u8 {
        if ((self.bytes_allocated + len > self.next_gc) or debug.stress_gc) {
            try self.collectGarbage();
        }
        const out = try self.parent_allocator.rawAlloc(len, ptr_align, len_align, ret_addr);
        self.bytes_allocated += out.len;
        return out;
    }

    fn resize(self: *Self, buf: []u8, buf_align: u29, new_len: usize, len_align: u29, ret_addr: usize) ?usize {
        if (new_len > buf.len) {
            if ((self.bytes_allocated + (new_len - buf.len) > self.next_gc) or debug.stress_gc) {
                self.collectGarbage() catch {
                    return null;
                };
            }
        }

        if (self.parent_allocator.rawResize(buf, buf_align, new_len, len_align, ret_addr)) |resized_len| {
            if (resized_len > buf.len) {
                self.bytes_allocated += resized_len - buf.len;
            } else {
                self.bytes_allocated -= buf.len - resized_len;
            }
            return resized_len;
        }

        return null;
    }

    fn free(self: *Self, buf: []u8, buf_align: u29, ret_addr: usize) void {
        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
        self.bytes_allocated -= buf.len;
    }

    fn collectGarbage(self: *Self) !void {
        _ = self;
    }

    pub fn freeObjects(vm: *Vm) void {
        for (vm.objects.items) |object| {
            freeObject(vm, object);
        }
    }

    fn freeObject(vm: *Vm, object: Value) void {
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
