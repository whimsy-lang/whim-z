const std = @import("std");
const Allocator = std.mem.Allocator;

const GcAllocator = @import("memory.zig").GcAllocater;
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const ValueContainer = @import("value.zig").ValueContainer;
const Vm = @import("vm.zig").Vm;

pub const StringMap = struct {
    const Entry = struct {
        key: ?*ObjString,
        value: ValueContainer,
    };

    const max_load = 0.75;

    allocator: Allocator,
    count: usize,
    entries: []Entry,

    pub fn init(allocator: Allocator) StringMap {
        return .{
            .allocator = allocator,
            .count = 0,
            .entries = &[_]Entry{},
        };
    }

    pub fn deinit(self: *StringMap) void {
        self.allocator.free(self.entries);
        self.count = 0;
        self.entries = &[_]Entry{};
    }

    // map optimizations depend on capacity being a power of 2
    pub fn growCapacity(capacity: usize) usize {
        return if (capacity < 8) 8 else capacity * 2;
    }

    fn adjustCapacity(self: *StringMap, capacity: usize) void {
        const entries = self.allocator.alloc(Entry, capacity) catch {
            std.debug.print("Could not allocate memory for string map.", .{});
            std.process.exit(1);
        };
        for (entries) |*entry| {
            entry.key = null;
            entry.value = .{ .value = Value.empty() };
        }

        self.count = 0;
        for (self.entries) |entry| {
            if (entry.key == null) continue;

            const dest = findEntry(entries, entry.key.?);
            dest.key = entry.key;
            dest.value = entry.value;
            self.count += 1;
        }

        self.allocator.free(self.entries);
        self.entries = entries;
    }

    fn findEntry(entries: []Entry, key: *ObjString) *Entry {
        var index = key.hash & (entries.len - 1);
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.value.is(.empty)) {
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

    pub fn findString(self: *StringMap, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) return null;

        var index = hash & (self.entries.len - 1);
        while (true) {
            const entry = &self.entries[index];
            if (entry.key == null) {
                // stop if we find an empty non-tombstone entry
                if (entry.value.value.is(.empty)) return null;
            } else if (entry.key.?.hash == hash and std.mem.eql(u8, entry.key.?.chars, chars)) {
                // found string
                return entry.key;
            }

            index = (index + 1) & (self.entries.len - 1);
        }
    }

    fn ensureCapacity(self: *StringMap) void {
        if (@intToFloat(f64, self.count + 1) > @intToFloat(f64, self.entries.len) * max_load) {
            const capacity = growCapacity(self.entries.len);
            self.adjustCapacity(capacity);
        }
    }

    // adds an item if it doesn't already exist, and returns whether the add succeeded
    pub fn add(self: *StringMap, key: *ObjString, value: Value, constant: bool) bool {
        self.ensureCapacity();

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        if (is_new_key) {
            // only increment if it's a new key and not a tombstone
            if (entry.value.value.is(.empty)) self.count += 1;
            entry.key = key;
            entry.value.value = value;
            entry.value.constant = constant;
        }

        return is_new_key;
    }

    pub fn get(self: *StringMap, key: *ObjString, value: *Value) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        value.* = entry.value.value;
        return true;
    }

    pub fn getPtr(self: *StringMap, key: *ObjString, value: **ValueContainer) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        value.* = &entry.value;
        return true;
    }

    pub fn set(self: *StringMap, key: *ObjString, value: Value) bool {
        self.ensureCapacity();

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        // only increment if it's a new key and not a tombstone
        if (is_new_key and entry.value.value.is(.empty)) self.count += 1;

        entry.key = key;
        entry.value.value = value;
        return is_new_key;
    }

    fn delete(self: *StringMap, key: *ObjString) bool {
        if (self.count == 0) return false;

        // find the entry
        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        // tombstone
        entry.key = null;
        entry.value.value = Value.boolean(true);
        return true;
    }

    pub fn mark(self: *StringMap, vm: *Vm) void {
        for (self.entries) |entry| {
            if (entry.key) |key| key.obj.mark(vm);
            entry.value.value.mark(vm);
        }
    }

    pub fn removeWhite(self: *StringMap) void {
        for (self.entries) |entry| {
            if (entry.key != null and !entry.key.?.obj.is_marked) {
                _ = self.delete(entry.key.?);
            }
        }
    }
};
