const std = @import("std");
const Allocator = std.mem.Allocator;

const GcAllocator = @import("memory.zig").GcAllocater;
const ObjString = @import("object.zig").ObjString;
const out = @import("out.zig");
const value = @import("value.zig");
const Value = value.Value;
const ValueContainer = value.ValueContainer;
const Vm = @import("vm.zig").Vm;

pub const Map = struct {
    const Entry = struct {
        key: Value,
        value: ValueContainer,
    };

    const max_load = 0.75;

    allocator: Allocator,
    count: usize,
    entries: []Entry,

    pub fn init(allocator: Allocator) Map {
        return .{
            .allocator = allocator,
            .count = 0,
            .entries = &[_]Entry{},
        };
    }

    pub fn deinit(self: *Map) void {
        self.allocator.free(self.entries);
        self.count = 0;
        self.entries = &[_]Entry{};
    }

    // map optimizations depend on capacity being a power of 2
    pub fn growCapacity(capacity: usize) usize {
        return if (capacity < 8) 8 else capacity * 2;
    }

    fn adjustCapacity(self: *Map, capacity: usize) void {
        const entries = self.allocator.alloc(Entry, capacity) catch {
            out.printExit("Could not allocate memory for map.", .{}, 1);
        };
        for (entries) |*entry| {
            entry.key = value.empty();
            entry.value = .{ .value = value.empty() };
        }

        self.count = 0;
        for (self.entries) |entry| {
            if (value.isEmpty(entry.key)) continue;

            const dest = findEntry(entries, entry.key);
            dest.key = entry.key;
            dest.value = entry.value;
            self.count += 1;
        }

        self.allocator.free(self.entries);
        self.entries = entries;
    }

    fn findEntry(entries: []Entry, key: Value) *Entry {
        var index = value.hash(key) & (entries.len - 1);
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];
            if (value.isEmpty(entry.key)) {
                if (value.isEmpty(entry.value.value)) {
                    // empty entry
                    return if (tombstone != null) tombstone.? else entry;
                } else {
                    // found a tombstone
                    if (tombstone == null) tombstone = entry;
                }
            } else if (entry.key == key) {
                // found the key
                return entry;
            }

            index = (index + 1) & (entries.len - 1);
        }
    }

    fn ensureCapacity(self: *Map) void {
        if (@intToFloat(f64, self.count + 1) > @intToFloat(f64, self.entries.len) * max_load) {
            const capacity = growCapacity(self.entries.len);
            self.adjustCapacity(capacity);
        }
    }

    // adds an item if it doesn't already exist, and returns whether the add succeeded
    pub fn add(self: *Map, key: Value, val: Value, constant: bool) bool {
        self.ensureCapacity();

        const entry = findEntry(self.entries, key);
        const is_new_key = value.isEmpty(entry.key);
        if (is_new_key) {
            // only increment if it's a new key and not a tombstone
            if (value.isEmpty(entry.value.value)) self.count += 1;
            entry.key = key;
            entry.value.value = val;
            entry.value.constant = constant;
        }

        return is_new_key;
    }

    pub fn get(self: *Map, key: Value, val: *Value) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (value.isEmpty(entry.key)) return false;

        val.* = entry.value.value;
        return true;
    }

    pub fn getPtr(self: *Map, key: Value, vc: **ValueContainer) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (value.isEmpty(entry.key)) return false;

        vc.* = &entry.value;
        return true;
    }

    pub fn set(self: *Map, key: Value, val: Value) bool {
        self.ensureCapacity();

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key.is(.empty);
        // only increment if it's a new key and not a tombstone
        if (is_new_key and entry.value.value.is(.empty)) self.count += 1;

        entry.key = key;
        entry.value.value = val;
        return is_new_key;
    }

    pub fn delete(self: *Map, key: Value) bool {
        if (self.count == 0) return false;

        // find the entry
        const entry = findEntry(self.entries, key);
        if (value.isEmpty(entry.key)) return false;

        // tombstone
        entry.key = value.empty();
        return true;
    }

    pub fn mark(self: *Map, vm: *Vm) void {
        for (self.entries) |entry| {
            value.mark(entry.key, vm);
            value.mark(entry.value.value, vm);
        }
    }
};
