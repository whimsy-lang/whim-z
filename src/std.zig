const std = @import("std");

const NativeFn = @import("object.zig").NativeFn;
const ObjClass = @import("object.zig").ObjClass;
const ObjNative = @import("object.zig").ObjNative;
const ObjString = @import("object.zig").ObjString;
const value = @import("value.zig");
const Value = value.Value;
const version = @import("vm.zig").version;
const Vm = @import("vm.zig").Vm;

pub fn register(vm: *Vm) void {
    // std
    const std_class = defineClass(vm, "std");
    defineNative(vm, std_class, "assert", n_std_assert);
    defineNative(vm, std_class, "error", n_std_error);
    defineNative(vm, std_class, "print", n_std_print);
    defineNative(vm, std_class, "time", n_std_time);
    defineProperty(vm, std_class, "version", value.string(ObjString.copy(vm, version)));

    // std.bool
    vm.bool_class = defineInnerClass(vm, std_class, "bool");

    // std.class
    vm.class_class = defineInnerClass(vm, std_class, "class");

    // std.function
    vm.function_class = defineInnerClass(vm, std_class, "function");

    // std.list
    vm.list_class = defineInnerClass(vm, std_class, "list");
    defineNative(vm, vm.list_class.?, "add", n_std_list_add);
    defineNative(vm, vm.list_class.?, "length", n_std_list_length);
    defineNative(vm, vm.list_class.?, "remove", n_std_list_remove);

    // std.map
    vm.map_class = defineInnerClass(vm, std_class, "map");
    defineNative(vm, vm.map_class.?, "length", n_std_map_length);
    defineNative(vm, vm.map_class.?, "remove", n_std_map_remove);

    // std.nil
    vm.nil_class = defineInnerClass(vm, std_class, "nil");

    // std.number
    vm.number_class = defineInnerClass(vm, std_class, "number");

    // std.range
    vm.range_class = defineInnerClass(vm, std_class, "range");

    // std.set
    vm.set_class = defineInnerClass(vm, std_class, "set");
    defineNative(vm, vm.set_class.?, "add", n_std_set_add);
    defineNative(vm, vm.set_class.?, "length", n_std_set_length);
    defineNative(vm, vm.set_class.?, "remove", n_std_set_remove);

    // std.string
    vm.string_class = defineInnerClass(vm, std_class, "string");
    defineNative(vm, vm.string_class.?, "length", n_std_string_length);
}

fn defineClass(vm: *Vm, name: []const u8) *ObjClass {
    vm.push(value.string(ObjString.copy(vm, name)));
    const class = ObjClass.init(vm, value.asString(vm.peek(0)), null);
    vm.push(value.class(class));
    _ = vm.globals.add(value.asString(vm.peek(1)), value.class(class), true);
    _ = vm.pop();
    _ = vm.pop();
    return class;
}

fn defineInnerClass(vm: *Vm, outer: *ObjClass, name: []const u8) *ObjClass {
    vm.push(value.string(ObjString.copy(vm, name)));
    const inner = ObjClass.init(vm, value.asString(vm.peek(0)), null);
    vm.push(value.class(inner));
    _ = outer.fields.add(value.asString(vm.peek(1)), value.class(inner), true);
    _ = vm.pop();
    _ = vm.pop();
    return inner;
}

fn defineNative(vm: *Vm, class: *ObjClass, name: []const u8, native_fn: NativeFn) void {
    vm.push(value.string(ObjString.copy(vm, name)));
    vm.push(value.native(ObjNative.init(vm, native_fn)));
    _ = class.fields.add(value.asString(vm.peek(1)), vm.peek(0), true);
    _ = vm.pop();
    _ = vm.pop();
}

fn defineProperty(vm: *Vm, class: *ObjClass, name: []const u8, val: Value) void {
    vm.push(val);
    vm.push(value.string(ObjString.copy(vm, name)));
    _ = class.fields.add(value.asString(vm.peek(0)), vm.peek(1), true);
    _ = vm.pop();
    _ = vm.pop();
}

fn n_std_assert(vm: *Vm, values: []Value) Value {
    if (values.len == 0) {
        return vm.nativeError("std.assert takes a condition and an optional message", .{});
    }
    if (value.isFalsey(values[0])) {
        if (values.len == 2 and value.isObjType(values[1], .string)) {
            return vm.nativeError("std.assert: {s}", .{value.asString(values[1]).chars});
        }
        return vm.nativeError("std.assert", .{});
    }
    return value.nil();
}

fn n_std_error(vm: *Vm, values: []Value) Value {
    if (values.len == 1 and value.isObjType(values[0], .string)) {
        return vm.nativeError("std.error: {s}", .{value.asString(values[0]).chars});
    }
    return vm.nativeError("std.error", .{});
}

fn n_std_print(vm: *Vm, values: []Value) Value {
    _ = vm;
    for (values) |val| {
        value.print(val);
    }
    std.debug.print("\n", .{});
    return value.nil();
}

fn n_std_time(vm: *Vm, values: []Value) Value {
    if (values.len != 0) {
        return vm.nativeError("std.time takes 0 arguments", .{});
    }
    const time = @intToFloat(f64, std.time.nanoTimestamp()) / std.time.ns_per_s;
    return value.number(time);
}

fn n_std_list_add(vm: *Vm, values: []Value) Value {
    if (values.len < 2) {
        return vm.nativeError("std.list.add takes a list and at least one item to add", .{});
    }
    if (!value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.add's first argument must be a list", .{});
    }
    value.asList(values[0]).items.appendSlice(values[1..]) catch {
        std.debug.print("Could not allocate memory for list.", .{});
        std.process.exit(1);
    };
    return values[0];
}

fn n_std_list_length(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.length takes a list", .{});
    }
    return value.number(@intToFloat(f64, value.asList(values[0]).items.items.len));
}

fn n_std_list_remove(vm: *Vm, values: []Value) Value {
    if (values.len != 2) {
        return vm.nativeError("std.list.remove takes a list and the index to remove", .{});
    }
    if (!value.isObjType(values[0], .list) or !value.isNumber(values[1])) {
        return vm.nativeError("std.list.remove arguments must be a list and number", .{});
    }
    return value.asList(values[0]).items.orderedRemove(@floatToInt(usize, value.asNumber(values[1])));
}

fn n_std_map_length(vm: *Vm, values: []Value) Value {
    var length: usize = 0;
    if (values.len != 1 or !value.isObjType(values[0], .map)) {
        return vm.nativeError("std.map.length takes a map", .{});
    }
    for (value.asMap(values[0]).items.entries) |entry| {
        if (!value.isEmpty(entry.key)) {
            length += 1;
        }
    }
    return value.number(@intToFloat(f64, length));
}

fn n_std_map_remove(vm: *Vm, values: []Value) Value {
    if (values.len < 2) {
        return vm.nativeError("std.map.remove takes a map and at least one key to remove", .{});
    }
    if (!value.isObjType(values[0], .map)) {
        return vm.nativeError("std.map.remove's first argument must be a map", .{});
    }
    const map = value.asMap(values[0]);
    for (values[1..]) |val| {
        _ = map.items.delete(val);
    }
    return values[0];
}

fn n_std_set_add(vm: *Vm, values: []Value) Value {
    if (values.len < 2) {
        return vm.nativeError("std.set.add takes a set and at least one item to add", .{});
    }
    if (!value.isObjType(values[0], .set)) {
        return vm.nativeError("std.set.add's first argument must be a set", .{});
    }
    const set = value.asSet(values[0]);
    for (values[1..]) |val| {
        _ = set.items.add(val, value.nil(), true);
    }
    return values[0];
}

fn n_std_set_length(vm: *Vm, values: []Value) Value {
    var length: usize = 0;
    if (values.len != 1 or !value.isObjType(values[0], .set)) {
        return vm.nativeError("std.set.length takes a set", .{});
    }
    for (value.asSet(values[0]).items.entries) |entry| {
        if (!value.isEmpty(entry.key)) {
            length += 1;
        }
    }
    return value.number(@intToFloat(f64, length));
}

fn n_std_set_remove(vm: *Vm, values: []Value) Value {
    if (values.len < 2) {
        return vm.nativeError("std.set.remove takes a set and at least one item to remove", .{});
    }
    if (!value.isObjType(values[0], .set)) {
        return vm.nativeError("std.set.remove's first argument must be a set", .{});
    }
    const set = value.asSet(values[0]);
    for (values[1..]) |val| {
        _ = set.items.delete(val);
    }
    return values[0];
}

fn n_std_string_length(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.length takes a string", .{});
    }
    return value.number(@intToFloat(f64, value.asString(values[0]).length));
}
