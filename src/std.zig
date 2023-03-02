const std = @import("std");
const unicode = std.unicode;

const NativeFn = @import("object.zig").NativeFn;
const ObjClass = @import("object.zig").ObjClass;
const ObjList = @import("object.zig").ObjList;
const ObjNative = @import("object.zig").ObjNative;
const ObjSet = @import("object.zig").ObjSet;
const ObjString = @import("object.zig").ObjString;
const out = @import("out.zig");
const value = @import("value.zig");
const Value = value.Value;
const version = @import("vm.zig").version;
const Vm = @import("vm.zig").Vm;

const lib = @embedFile("std.whim");

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
    defineNative(vm, vm.bool_class.?, "to_string", n_std_to_string);

    // std.class
    vm.class_class = defineInnerClass(vm, std_class, "class");
    defineNative(vm, vm.class_class.?, "to_string", n_std_to_string);

    // std.function
    vm.function_class = defineInnerClass(vm, std_class, "function");
    defineNative(vm, vm.function_class.?, "to_string", n_std_to_string);

    // std.list
    vm.list_class = defineInnerClass(vm, std_class, "list");
    defineNative(vm, vm.list_class.?, "add", n_std_list_add);
    defineNative(vm, vm.list_class.?, "index_of", n_std_list_index_of);
    defineNative(vm, vm.list_class.?, "join", n_std_list_join);
    defineNative(vm, vm.list_class.?, "last_index_of", n_std_list_last_index_of);
    defineNative(vm, vm.list_class.?, "length", n_std_list_length);
    defineNative(vm, vm.list_class.?, "pop", n_std_list_pop);
    defineNative(vm, vm.list_class.?, "remove", n_std_list_remove);
    defineNative(vm, vm.list_class.?, "reverse", n_std_list_reverse);
    defineNative(vm, vm.list_class.?, "to_set", n_std_list_to_set);
    defineNative(vm, vm.list_class.?, "to_string", n_std_to_string);

    // std.map
    vm.map_class = defineInnerClass(vm, std_class, "map");
    defineNative(vm, vm.map_class.?, "keys", n_std_map_keys);
    defineNative(vm, vm.map_class.?, "length", n_std_map_length);
    defineNative(vm, vm.map_class.?, "remove", n_std_map_remove);
    defineNative(vm, vm.map_class.?, "to_string", n_std_to_string);
    defineNative(vm, vm.map_class.?, "values", n_std_map_values);

    // std.math
    const math_class = defineInnerClass(vm, std_class, "math");
    defineNative(vm, math_class, "max", n_std_math_max);
    defineNative(vm, math_class, "min", n_std_math_min);
    defineProperty(vm, math_class, "pi", value.number(3.141592653589793238462643383279502884));

    // std.nil
    vm.nil_class = defineInnerClass(vm, std_class, "nil");
    defineNative(vm, vm.nil_class.?, "to_string", n_std_to_string);

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
    defineProperty(vm, vm.number_class.?, "max", value.number(std.math.floatMax(f64)));
    defineProperty(vm, vm.number_class.?, "min", value.number(-std.math.floatMax(f64)));
    defineNative(vm, vm.number_class.?, "pow", n_std_number_pow);
    defineNative(vm, vm.number_class.?, "sin", n_std_number_sin);
    defineNative(vm, vm.number_class.?, "sqrt", n_std_number_sqrt);
    defineNative(vm, vm.number_class.?, "tan", n_std_number_tan);
    defineNative(vm, vm.number_class.?, "to_char", n_std_number_to_char);
    defineNative(vm, vm.number_class.?, "to_degrees", n_std_number_to_degrees);
    defineNative(vm, vm.number_class.?, "to_radians", n_std_number_to_radians);
    defineNative(vm, vm.number_class.?, "to_string", n_std_to_string);

    // std.range
    vm.range_class = defineInnerClass(vm, std_class, "range");
    defineNative(vm, vm.range_class.?, "to_string", n_std_to_string);
    defineNative(vm, vm.range_class.?, "values", n_std_range_values);

    // std.set
    vm.set_class = defineInnerClass(vm, std_class, "set");
    defineNative(vm, vm.set_class.?, "add", n_std_set_add);
    defineNative(vm, vm.set_class.?, "length", n_std_set_length);
    defineNative(vm, vm.set_class.?, "remove", n_std_set_remove);
    defineNative(vm, vm.set_class.?, "to_string", n_std_to_string);
    defineNative(vm, vm.set_class.?, "values", n_std_set_values);

    // std.string
    vm.string_class = defineInnerClass(vm, std_class, "string");
    defineNative(vm, vm.string_class.?, "char_to_number", n_std_string_char_to_number);
    defineNative(vm, vm.string_class.?, "chars", n_std_string_chars);
    defineNative(vm, vm.string_class.?, "ends_with", n_std_string_ends_with);
    defineNative(vm, vm.string_class.?, "index_of", n_std_string_index_of);
    defineNative(vm, vm.string_class.?, "last_index_of", n_std_string_last_index_of);
    defineNative(vm, vm.string_class.?, "length", n_std_string_length);
    defineNative(vm, vm.string_class.?, "repeat", n_std_string_repeat);
    defineNative(vm, vm.string_class.?, "split", n_std_string_split);
    defineNative(vm, vm.string_class.?, "starts_with", n_std_string_starts_with);
    defineNative(vm, vm.string_class.?, "to_lower", n_std_string_to_lower);
    defineNative(vm, vm.string_class.?, "to_string", n_std_to_string);
    defineNative(vm, vm.string_class.?, "to_upper", n_std_string_to_upper);
    defineNative(vm, vm.string_class.?, "trim", n_std_string_trim);
    defineNative(vm, vm.string_class.?, "trim_end", n_std_string_trim_end);
    defineNative(vm, vm.string_class.?, "trim_start", n_std_string_trim_start);

    // load the parts implemented in Whimsy
    const result = vm.interpret(lib);
    if (result == .compile_error) out.printExit("", .{}, 65);
    if (result == .runtime_error) out.printExit("", .{}, 70);
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

fn n_std_list_add(vm: *Vm, values: []Value) Value {
    if (values.len < 2 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.add takes a list and at least one item to add", .{});
    }
    value.asList(values[0]).items.appendSlice(values[1..]) catch {
        out.printExit("Could not allocate memory for list.", .{}, 1);
    };
    return values[0];
}

fn n_std_list_index_of(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.index_of takes a list and the value to find", .{});
    }
    const index = std.mem.indexOfScalar(Value, value.asList(values[0]).items.items, values[1]);
    return value.number(if (index) |i| @intToFloat(f64, i) else -1);
}

fn n_std_list_join(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .list) or !value.isObjType(values[1], .string)) {
        return vm.nativeError("std.list.join takes a list and a string", .{});
    }
    const list = value.asList(values[0]);
    const sep = value.asString(values[1]);

    const slices = vm.allocator.alloc([]const u8, list.items.items.len) catch {
        out.printExit("Could not allocate memory for slices.", .{}, 1);
    };
    defer vm.allocator.free(slices);

    for (0..list.items.items.len) |i| {
        const item = list.items.items[i];
        if (!value.isObjType(item, .string)) {
            return vm.nativeError("std.list.join can only join strings", .{});
        }
        slices[i] = value.asString(item).chars;
    }

    const heap_chars = std.mem.join(vm.allocator, sep.chars, slices) catch {
        out.printExit("Could not allocate memory for string.", .{}, 1);
    };

    return value.string(ObjString.take(vm, heap_chars));
}

fn n_std_list_last_index_of(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.last_index_of takes a list and the value to find", .{});
    }
    const index = std.mem.lastIndexOfScalar(Value, value.asList(values[0]).items.items, values[1]);
    return value.number(if (index) |i| @intToFloat(f64, i) else -1);
}

fn n_std_list_length(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.length takes a list", .{});
    }
    return value.number(@intToFloat(f64, value.asList(values[0]).items.items.len));
}

fn n_std_list_pop(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.pop takes a list", .{});
    }
    return value.asList(values[0]).items.pop();
}

fn n_std_list_remove(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .list) or !value.isNumber(values[1])) {
        return vm.nativeError("std.list.remove takes a list and the index to remove", .{});
    }
    return value.asList(values[0]).items.orderedRemove(@floatToInt(usize, value.asNumber(values[1])));
}

fn n_std_list_reverse(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.reverse takes a list", .{});
    }
    const list = value.asList(values[0]);
    const new_list = ObjList.init(vm);
    vm.push(value.list(new_list));
    new_list.items.appendSlice(list.items.items) catch {
        out.printExit("Could not allocate memory for list.", .{}, 1);
    };
    std.mem.reverse(Value, new_list.items.items);
    return vm.pop();
}

fn n_std_list_to_set(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .list)) {
        return vm.nativeError("std.list.to_set takes a list", .{});
    }
    const set = ObjSet.init(vm);
    vm.push(value.set(set));

    for (value.asList(values[0]).items.items) |i| {
        _ = set.items.add(i, value.nil(), true);
    }

    return vm.pop();
}

fn n_std_map_keys(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .map)) {
        return vm.nativeError("std.map.keys takes a map", .{});
    }
    const list = ObjList.init(vm);
    vm.push(value.list(list));

    for (value.asMap(values[0]).items.entries) |entry| {
        if (!value.isEmpty(entry.key)) {
            list.items.append(entry.key) catch {
                out.printExit("Could not allocate memory for list.", .{}, 1);
            };
        }
    }

    return vm.pop();
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
    if (values.len < 2 or !value.isObjType(values[0], .map)) {
        return vm.nativeError("std.map.remove takes a map and at least one key to remove", .{});
    }
    const map = value.asMap(values[0]);
    for (values[1..]) |val| {
        _ = map.items.delete(val);
    }
    return values[0];
}

fn n_std_map_values(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .map)) {
        return vm.nativeError("std.map.values takes a map", .{});
    }
    const list = ObjList.init(vm);
    vm.push(value.list(list));

    for (value.asMap(values[0]).items.entries) |entry| {
        if (!value.isEmpty(entry.key)) {
            list.items.append(entry.value.value) catch {
                out.printExit("Could not allocate memory for list.", .{}, 1);
            };
        }
    }

    return vm.pop();
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

fn n_std_number_to_char(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isNumber(values[0])) {
        return vm.nativeError("std.number.to_char takes a number", .{});
    }
    var buf: [4]u8 = undefined;
    const len = unicode.utf8Encode(@floatToInt(u21, value.asNumber(values[0])), &buf) catch {
        out.printExit("Unable to encode character.", .{}, 1);
    };
    return value.string(ObjString.copy(vm, buf[0..len]));
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
    for (values) |val| {
        out.print("{s}", .{value.toString(val, vm).chars});
    }
    out.println("", .{});
    out.flush();
    return value.nil();
}

fn n_std_range_values(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .range)) {
        return vm.nativeError("std.range.values takes a range", .{});
    }
    const range = value.asRange(values[0]);
    const positive = range.step > 0;
    const list = ObjList.init(vm);
    vm.push(value.list(list));

    var index: f64 = 0;
    while (true) : (index += 1) {
        const val = range.start + index * range.step;
        if ((positive and val < range.end) or
            (!positive and val > range.end) or
            (range.inclusive and val == range.end))
        {
            list.items.append(value.number(val)) catch {
                out.printExit("Could not allocate memory for list.", .{}, 1);
            };
        } else {
            break;
        }
    }

    return vm.pop();
}

fn n_std_set_add(vm: *Vm, values: []Value) Value {
    if (values.len < 2 or !value.isObjType(values[0], .set)) {
        return vm.nativeError("std.set.add takes a set and at least one item to add", .{});
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
    if (values.len < 2 or !value.isObjType(values[0], .set)) {
        return vm.nativeError("std.set.remove takes a set and at least one item to remove", .{});
    }
    const set = value.asSet(values[0]);
    for (values[1..]) |val| {
        _ = set.items.delete(val);
    }
    return values[0];
}

fn n_std_set_values(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .set)) {
        return vm.nativeError("std.set.values takes a set", .{});
    }
    const list = ObjList.init(vm);
    vm.push(value.list(list));

    for (value.asSet(values[0]).items.entries) |entry| {
        if (!value.isEmpty(entry.key)) {
            list.items.append(entry.key) catch {
                out.printExit("Could not allocate memory for list.", .{}, 1);
            };
        }
    }

    return vm.pop();
}

fn n_std_string_char_to_number(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .string) or value.asString(values[0]).length != 1) {
        return vm.nativeError("std.string.char_to_number takes a string with length 1", .{});
    }
    const val = unicode.utf8Decode(value.asString(values[0]).chars) catch {
        out.printExit("Invalid character encoding.", .{}, 1);
    };
    return value.number(@intToFloat(f64, val));
}

fn n_std_string_chars(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.chars takes a string", .{});
    }
    const str = value.asString(values[0]);
    const list = ObjList.init(vm);
    vm.push(value.list(list));

    var i: usize = 0;
    while (i < str.chars.len) {
        const len = unicode.utf8ByteSequenceLength(str.chars[i]) catch {
            out.printExit("Invalid character encoding.", .{}, 1);
        };
        vm.push(value.string(ObjString.copy(vm, str.chars[i .. i + len])));
        list.items.append(vm.peek(0)) catch {
            out.printExit("Could not allocate memory for list.", .{}, 1);
        };
        _ = vm.pop();
        i += len;
    }

    return vm.pop();
}

fn n_std_string_ends_with(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .string) or !value.isObjType(values[1], .string)) {
        return vm.nativeError("std.string.ends_with takes two strings", .{});
    }
    const res = std.mem.endsWith(u8, value.asString(values[0]).chars, value.asString(values[1]).chars);
    return value.boolean(res);
}

fn n_std_string_index_of(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .string) or !value.isObjType(values[1], .string)) {
        return vm.nativeError("std.string.index_of takes two strings", .{});
    }
    const str = value.asString(values[0]);
    const search = value.asString(values[1]);
    const b_index = std.mem.indexOf(u8, str.chars, search.chars);
    if (b_index) |bi| {
        const index = str.utf8Index(bi);
        if (index) |i| return value.number(@intToFloat(f64, i));
    }
    return value.number(-1);
}

fn n_std_string_last_index_of(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .string) or !value.isObjType(values[1], .string)) {
        return vm.nativeError("std.string.last_index_of takes two strings", .{});
    }
    const str = value.asString(values[0]);
    const search = value.asString(values[1]);
    const b_index = std.mem.lastIndexOf(u8, str.chars, search.chars);
    if (b_index) |bi| {
        const index = str.utf8Index(bi);
        if (index) |i| return value.number(@intToFloat(f64, i));
    }
    return value.number(-1);
}

fn n_std_string_length(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.length takes a string", .{});
    }
    return value.number(@intToFloat(f64, value.asString(values[0]).length));
}

fn n_std_string_repeat(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .string) or !value.isNumber(values[1])) {
        return vm.nativeError("std.string.repeat takes a string and number", .{});
    }
    const str = value.asString(values[0]);
    const count = @floatToInt(usize, value.asNumber(values[1]));
    const heap_chars = vm.allocator.alloc(u8, str.chars.len * count) catch {
        out.printExit("Could not allocate memory for string.", .{}, 1);
    };
    for (0..count) |i| {
        std.mem.copy(u8, heap_chars[i * str.chars.len ..], str.chars);
    }
    return value.string(ObjString.take(vm, heap_chars));
}

fn n_std_string_split(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .string) or !value.isObjType(values[1], .string)) {
        return vm.nativeError("std.string.split takes two strings", .{});
    }
    const str = value.asString(values[0]);
    const delim = value.asString(values[1]);
    const list = ObjList.init(vm);
    vm.push(value.list(list));

    var iter = std.mem.split(u8, str.chars, delim.chars);
    while (iter.next()) |item| {
        vm.push(value.string(ObjString.copy(vm, item)));
        list.items.append(vm.peek(0)) catch {
            out.printExit("Could not allocate memory for list.", .{}, 1);
        };
        _ = vm.pop();
    }

    return vm.pop();
}

fn n_std_string_starts_with(vm: *Vm, values: []Value) Value {
    if (values.len != 2 or !value.isObjType(values[0], .string) or !value.isObjType(values[1], .string)) {
        return vm.nativeError("std.string.starts_with takes two strings", .{});
    }
    const res = std.mem.startsWith(u8, value.asString(values[0]).chars, value.asString(values[1]).chars);
    return value.boolean(res);
}

fn n_std_string_to_lower(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.to_lower takes a string", .{});
    }
    const heap_chars = std.ascii.allocLowerString(vm.allocator, value.asString(values[0]).chars) catch {
        out.printExit("Could not allocate memory for string.", .{}, 1);
    };
    return value.string(ObjString.take(vm, heap_chars));
}

fn n_std_string_to_upper(vm: *Vm, values: []Value) Value {
    if (values.len != 1 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.to_upper takes a string", .{});
    }
    const heap_chars = std.ascii.allocUpperString(vm.allocator, value.asString(values[0]).chars) catch {
        out.printExit("Could not allocate memory for string.", .{}, 1);
    };
    return value.string(ObjString.take(vm, heap_chars));
}

fn n_std_string_trim(vm: *Vm, values: []Value) Value {
    if (values.len == 0 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.trim takes a string and optional strings to trim", .{});
    }
    // trim whitespace
    if (values.len == 1) {
        const ws = " \t\r\n";
        const trimmed = std.mem.trim(u8, value.asString(values[0]).chars, ws);
        return value.string(ObjString.copy(vm, trimmed));
    }
    // custom trim
    var trims = vm.allocator.alloc([]const u8, values.len - 1) catch {
        out.printExit("Could not allocate memory for trim.", .{}, 1);
    };
    defer vm.allocator.free(trims);
    for (values[1..], 0..) |val, i| {
        if (!value.isObjType(val, .string)) {
            return vm.nativeError("std.string.trim takes a string and optional strings to trim", .{});
        }
        trims[i] = value.asString(val).chars;
    }
    var trimmed = trim_start(value.asString(values[0]).chars, trims);
    trimmed = trim_end(trimmed, trims);
    return value.string(ObjString.copy(vm, trimmed));
}

fn n_std_string_trim_end(vm: *Vm, values: []Value) Value {
    if (values.len == 0 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.trim_end takes a string and optional strings to trim", .{});
    }
    // trim whitespace
    if (values.len == 1) {
        const ws = " \t\r\n";
        const trimmed = std.mem.trimRight(u8, value.asString(values[0]).chars, ws);
        return value.string(ObjString.copy(vm, trimmed));
    }
    // custom trim
    var trims = vm.allocator.alloc([]const u8, values.len - 1) catch {
        out.printExit("Could not allocate memory for trim.", .{}, 1);
    };
    defer vm.allocator.free(trims);
    for (values[1..], 0..) |val, i| {
        if (!value.isObjType(val, .string)) {
            return vm.nativeError("std.string.trim_end takes a string and optional strings to trim", .{});
        }
        trims[i] = value.asString(val).chars;
    }
    const trimmed = trim_end(value.asString(values[0]).chars, trims);
    return value.string(ObjString.copy(vm, trimmed));
}

fn n_std_string_trim_start(vm: *Vm, values: []Value) Value {
    if (values.len == 0 or !value.isObjType(values[0], .string)) {
        return vm.nativeError("std.string.trim_start takes a string and optional strings to trim", .{});
    }
    // trim whitespace
    if (values.len == 1) {
        const ws = " \t\r\n";
        const trimmed = std.mem.trimLeft(u8, value.asString(values[0]).chars, ws);
        return value.string(ObjString.copy(vm, trimmed));
    }
    // custom trim
    var trims = vm.allocator.alloc([]const u8, values.len - 1) catch {
        out.printExit("Could not allocate memory for trim.", .{}, 1);
    };
    defer vm.allocator.free(trims);
    for (values[1..], 0..) |val, i| {
        if (!value.isObjType(val, .string)) {
            return vm.nativeError("std.string.trim_start takes a string and optional strings to trim", .{});
        }
        trims[i] = value.asString(val).chars;
    }
    const trimmed = trim_start(value.asString(values[0]).chars, trims);
    return value.string(ObjString.copy(vm, trimmed));
}

fn trim_start(str: []const u8, trims: []const []const u8) []const u8 {
    var res = str;
    var trimming = true;
    while (trimming) {
        trimming = false;
        for (trims) |trim| {
            while (std.mem.startsWith(u8, res, trim)) {
                res = res[trim.len..];
                trimming = true;
            }
        }
    }
    return res;
}

fn trim_end(str: []const u8, trims: []const []const u8) []const u8 {
    var res = str;
    var trimming = true;
    while (trimming) {
        trimming = false;
        for (trims) |trim| {
            while (std.mem.endsWith(u8, res, trim)) {
                res = res[0 .. res.len - trim.len];
                trimming = true;
            }
        }
    }
    return res;
}

fn n_std_time(vm: *Vm, values: []Value) Value {
    if (values.len != 0) {
        return vm.nativeError("std.time takes 0 arguments", .{});
    }
    const time = @intToFloat(f64, std.time.nanoTimestamp()) / std.time.ns_per_s;
    return value.number(time);
}

fn n_std_to_string(vm: *Vm, values: []Value) Value {
    if (values.len != 1) {
        return vm.nativeError("to_string takes one argument", .{});
    }
    return value.string(value.toString(values[0], vm));
}
