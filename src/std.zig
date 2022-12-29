const std = @import("std");

const NativeFn = @import("object.zig").NativeFn;
const ObjClass = @import("object.zig").ObjClass;
const ObjNative = @import("object.zig").ObjNative;
const ObjString = @import("object.zig").ObjString;
const value = @import("value.zig");
const Value = value.Value;
const version = @import("vm.zig").version;
const Vm = @import("vm.zig").Vm;

pub const lib = @embedFile("std.whim");

pub fn register(vm: *Vm) void {
    // std
    const std_class = defineClass(vm, "std");
    defineNative(vm, std_class, "assert", n_std_assert);
    defineNative(vm, std_class, "error", n_std_error);
    defineNative(vm, std_class, "gc_collect", n_std_gc_collect);
    defineNative(vm, std_class, "print", n_std_print);
    defineNative(vm, std_class, "time", n_std_time);
    defineProperty(vm, std_class, "version", value.string(ObjString.copy(vm, version)));

    // std.bool
    vm.bool_class = defineInnerClass(vm, std_class, "bool");
    defineNative(vm, vm.bool_class.?, "to_string", n_std_bool_to_string);

    // std.class
    vm.class_class = defineInnerClass(vm, std_class, "class");
    defineNative(vm, vm.class_class.?, "to_string", n_std_class_to_string);

    // std.function
    vm.function_class = defineInnerClass(vm, std_class, "function");
    defineNative(vm, vm.function_class.?, "to_string", n_std_function_to_string);

    // std.list
    vm.list_class = defineInnerClass(vm, std_class, "list");
    defineNative(vm, vm.list_class.?, "add", n_std_list_add);
    defineNative(vm, vm.list_class.?, "index_of", n_std_list_index_of);
    defineNative(vm, vm.list_class.?, "last_index_of", n_std_list_last_index_of);
    defineNative(vm, vm.list_class.?, "length", n_std_list_length);
    defineNative(vm, vm.list_class.?, "remove", n_std_list_remove);

    // std.map
    vm.map_class = defineInnerClass(vm, std_class, "map");
    defineNative(vm, vm.map_class.?, "length", n_std_map_length);
    defineNative(vm, vm.map_class.?, "remove", n_std_map_remove);

    const math_class = defineInnerClass(vm, std_class, "math");
    defineNative(vm, math_class, "max", n_std_math_max);
    defineNative(vm, math_class, "min", n_std_math_min);
    defineProperty(vm, math_class, "pi", value.number(3.141592653589793238462643383279502884));

    // std.nil
    vm.nil_class = defineInnerClass(vm, std_class, "nil");
    defineNative(vm, vm.nil_class.?, "to_string", n_std_nil_to_string);

    // std.number
    vm.number_class = defineInnerClass(vm, std_class, "number");
    defineNative(vm, vm.number_class.?, "abs", n_std_number_abs);
    defineNative(vm, vm.number_class.?, "acos", n_std_number_acos);
    defineNative(vm, vm.number_class.?, "asin", n_std_number_asin);
    defineNative(vm, vm.number_class.?, "atan", n_std_number_atan);
    defineNative(vm, vm.number_class.?, "ceiling", n_std_number_ceiling);
    defineNative(vm, vm.number_class.?, "cos", n_std_number_cos);
    defineNative(vm, vm.number_class.?, "floor", n_std_number_floor);
    defineNative(vm, vm.number_class.?, "log", n_std_number_log);
    defineNative(vm, vm.number_class.?, "pow", n_std_number_pow);
    defineNative(vm, vm.number_class.?, "sin", n_std_number_sin);
    defineNative(vm, vm.number_class.?, "sqrt", n_std_number_sqrt);
    defineNative(vm, vm.number_class.?, "tan", n_std_number_tan);
    defineNative(vm, vm.number_class.?, "to_degrees", n_std_number_to_degrees);
    defineNative(vm, vm.number_class.?, "to_radians", n_std_number_to_radians);
    defineProperty(vm, vm.number_class.?, "max", value.number(std.math.floatMax(f64)));
    defineProperty(vm, vm.number_class.?, "min", value.number(-std.math.floatMax(f64)));

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
    const class = ObjClass.init(vm, value.asString(vm.peek(0)), null, true);
    vm.push(value.class(class));
    _ = vm.globals.add(value.asString(vm.peek(1)), value.class(class), true);
    _ = vm.pop();
    _ = vm.pop();
    return class;
}

fn defineInnerClass(vm: *Vm, outer: *ObjClass, name: []const u8) *ObjClass {
    vm.push(value.string(ObjString.copy(vm, name)));
    const inner = ObjClass.init(vm, value.asString(vm.peek(0)), null, true);
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

fn n_std_gc_collect(vm: *Vm, values: []Value) Value {
    if (values.len != 0) {
        return vm.nativeError("std.gc_collect takes 0 arguments", .{});
    }
    vm.collectGarbage();
    return value.nil();
}

fn n_std_math_max(vm: *Vm, values: []Value) Value {
    if (values.len == 0) {
        return vm.nativeError("std.math.max takes numbers as arguments", .{});
    }
    var max = -std.math.floatMax(f64);
    for (values) |val| {
        if (!value.isNumber(val)) {
            return vm.nativeError("std.math.max takes numbers as arguments", .{});
        }
        const num = value.asNumber(val);
        if (num > max) max = num;
    }
    return value.number(max);
}

fn n_std_math_min(vm: *Vm, values: []Value) Value {
    if (values.len == 0) {
        return vm.nativeError("std.math.min takes numbers as arguments", .{});
    }
    var min = std.math.floatMax(f64);
    for (values) |val| {
        if (!value.isNumber(val)) {
            return vm.nativeError("std.math.min takes numbers as arguments", .{});
        }
        const num = value.asNumber(val);
        if (num < min) min = num;
    }
    return value.number(min);
}

fn n_std_number_abs(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.abs takes a number", .{});
    }
    const num = value.asNumber(values[0]);
    return value.number(if (num < 0) -num else num);
}

fn n_std_number_acos(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.acos takes a number", .{});
    }
    return value.number(std.math.acos(value.asNumber(values[0])));
}

fn n_std_number_asin(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.asin takes a number", .{});
    }
    return value.number(std.math.asin(value.asNumber(values[0])));
}

fn n_std_number_atan(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.atan takes a number", .{});
    }
    return value.number(std.math.atan(value.asNumber(values[0])));
}

fn n_std_number_ceiling(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.ceiling takes a number", .{});
    }
    return value.number(std.math.ceil(value.asNumber(values[0])));
}

fn n_std_number_cos(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.cos takes a number", .{});
    }
    return value.number(std.math.cos(value.asNumber(values[0])));
}

fn n_std_number_floor(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.floor takes a number", .{});
    }
    return value.number(std.math.floor(value.asNumber(values[0])));
}

fn n_std_number_log(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isNumber(values[0]) or !value.isNumber(values[1])) {
        return vm.nativeError("std.number.log takes two numbers (value, base)", .{});
    }
    return value.number(std.math.log(f64, value.asNumber(values[1]), value.asNumber(values[0])));
}

fn n_std_number_pow(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isNumber(values[0]) or !value.isNumber(values[1])) {
        return vm.nativeError("std.number.pow takes two numbers (value, power)", .{});
    }
    return value.number(std.math.pow(f64, value.asNumber(values[0]), value.asNumber(values[1])));
}

fn n_std_number_sin(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.sin takes a number", .{});
    }
    return value.number(std.math.sin(value.asNumber(values[0])));
}

fn n_std_number_sqrt(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.sqrt takes a number", .{});
    }
    return value.number(std.math.sqrt(value.asNumber(values[0])));
}

fn n_std_number_tan(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.tan takes a number", .{});
    }
    return value.number(std.math.tan(value.asNumber(values[0])));
}

fn n_std_number_to_degrees(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.to_degrees takes a number", .{});
    }
    return value.number(std.math.radiansToDegrees(f64, value.asNumber(values[0])));
}

fn n_std_number_to_radians(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.to_radians takes a number", .{});
    }
    return value.number(std.math.degreesToRadians(f64, value.asNumber(values[0])));
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

fn n_std_bool_to_string(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isBool(values[0])) {
        return vm.nativeError("std.bool.to_string takes a boolean", .{});
    }
    return value.string(ObjString.copy(vm, if (value.asBool(values[0])) "true" else "false"));
}

fn n_std_class_to_string(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .class)) {
        return vm.nativeError("std.class.to_string takes a class", .{});
    }
    const name = if (value.asClass(values[0]).name) |n| n.chars else "anonymous";
    const chars = std.fmt.allocPrint(vm.allocator, "class {s}", .{name}) catch {
        std.debug.print("Could not allocate memory for string.", .{});
        std.process.exit(1);
    };
    return value.string(ObjString.take(vm, chars));
}

fn n_std_function_to_string(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .closure)) {
        return vm.nativeError("std.function.to_string takes a function", .{});
    }
    const name = if (value.asClosure(values[0]).function.name) |n| n.chars else "anon fn";
    const chars = std.fmt.allocPrint(vm.allocator, "{s}()", .{name}) catch {
        std.debug.print("Could not allocate memory for string.", .{});
        std.process.exit(1);
    };
    return value.string(ObjString.take(vm, chars));
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

fn n_std_list_index_of(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.index_of takes a list and the value to find", .{});
    }
    for (value.asList(values[0]).items.items) |v, i| {
        if (v == values[1]) return value.number(@intToFloat(f64, i));
    }
    return value.number(-1);
}

fn n_std_list_last_index_of(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.last_index_of takes a list and the value to find", .{});
    }
    const list = value.asList(values[0]);
    var i = @intCast(isize, list.items.items.len) - 1;
    while (i >= 0) : (i -= 1) {
        if (list.items.items[@intCast(usize, i)] == values[1]) {
            return value.number(@intToFloat(f64, i));
        }
    }
    return value.number(-1);
}

fn n_std_list_length(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.length takes a list", .{});
    }
    return value.number(@intToFloat(f64, value.asList(values[0]).items.items.len));
}

fn n_std_list_remove(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .list) or !value.isNumber(values[1])) {
        return vm.nativeError("std.list.remove takes a list and the index to remove", .{});
    }
    return value.asList(values[0]).items.orderedRemove(@floatToInt(usize, value.asNumber(values[1])));
}

fn n_std_map_length(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .map)) {
        return vm.nativeError("std.map.length takes a map", .{});
    }
    var length: usize = 0;
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

fn n_std_nil_to_string(vm: *Vm, values: []Value) Value {
    _ = values;
    return value.string(ObjString.copy(vm, "nil"));
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
    if (values.len != 1 or !value.isObjType(values[0], .set)) {
        return vm.nativeError("std.set.length takes a set", .{});
    }
    var length: usize = 0;
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
