const std = @import("std");
const Allocator = std.mem.Allocator;

const GcAllocator = @import("memory.zig").GcAllocater;
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;

pub const ValueContainer = struct {
    value: Value,
    constant: bool = false,
};

pub const Map = struct {
    const Entry = struct {
        key: ?*ObjString,
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
            std.debug.print("Could not allocate memory for map.", .{});
            std.process.exit(1);
        };
        for (entries) |*entry| {
            entry.key = null;
            entry.value = .{ .value = Value.nil() };
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
        var index = key.hash % entries.len;
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.value.is(.nil)) {
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

            index = (index + 1) % entries.len;
        }
    }

    pub fn findString(self: *Map, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) return null;

        var index = hash % self.entries.len;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key == null) {
                // stop if we find an empty non-tombstone entry
                if (entry.value.value.is(.nil)) return null;
            } else if (entry.key.?.hash == hash and std.mem.eql(u8, entry.key.?.chars, chars)) {
                // found string
                return entry.key;
            }

            index = (index + 1) % self.entries.len;
        }
    }

    fn ensureCapacity(self: *Map) void {
        if (@intToFloat(f64, self.count + 1) > @intToFloat(f64, self.entries.len) * max_load) {
            const capacity = growCapacity(self.entries.len);
            self.adjustCapacity(capacity);
        }
    }

    // adds an item if it doesn't already exist, and returns whether the add succeeded
    pub fn add(self: *Map, key: *ObjString, value: Value, constant: bool) bool {
        self.ensureCapacity();

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        if (is_new_key) {
            // only increment if it's a new key and not a tombstone
            if (entry.value.value.is(.nil)) self.count += 1;
            entry.key = key;
            entry.value.value = value;
            entry.value.constant = constant;
        }

        return is_new_key;
    }

    pub fn get(self: *Map, key: *ObjString, value: *Value) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        value.* = entry.value.value;
        return true;
    }

    pub fn getPtr(self: *Map, key: *ObjString, value: **ValueContainer) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        value.* = &entry.value;
        return true;
    }

    pub fn set(self: *Map, key: *ObjString, value: Value) bool {
        self.ensureCapacity();

        const entry = findEntry(self.entries, key);
        const is_new_key = entry.key == null;
        // only increment if it's a new key and not a tombstone
        if (is_new_key and entry.value.value.is(.nil)) self.count += 1;

        entry.key = key;
        entry.value.value = value;
        return is_new_key;
    }

    fn delete(self: *Map, key: *ObjString) bool {
        if (self.count == 0) return false;

        // find the entry
        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        // tombstone
        entry.key = null;
        entry.value.value = Value.boolean(true);
        return true;
    }

    pub fn mark(self: *Map) void {
        for (self.entries) |entry| {
            if (entry.key) |key| {
                Value.string(key).mark();
            }
            entry.value.value.mark();
        }
    }
};
