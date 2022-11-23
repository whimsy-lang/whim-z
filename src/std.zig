const std = @import("std");

const NativeFn = @import("object.zig").NativeFn;
const ObjClass = @import("object.zig").ObjClass;
const ObjNative = @import("object.zig").ObjNative;
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

pub fn register(vm: *Vm) void {
    // std
    vm.std_class = defineClass(vm, "std");
    defineNative(vm, vm.std_class.?, "print", n_std_print);
    defineNative(vm, vm.std_class.?, "time", n_std_time);

    // std.list
    vm.list_class = defineInnerClass(vm, vm.std_class.?, "list");
    defineNative(vm, vm.list_class.?, "len", n_std_list_len);
}

fn defineClass(vm: *Vm, name: []const u8) *ObjClass {
    vm.push(Value.string(ObjString.copy(vm, name)));
    const class = ObjClass.init(vm, vm.peek(0).asString());
    vm.push(Value.class(class));
    _ = vm.globals.add(vm.peek(1).asString(), Value.class(class), true);
    _ = vm.pop();
    _ = vm.pop();
    return class;
}

fn defineInnerClass(vm: *Vm, outer: *ObjClass, name: []const u8) *ObjClass {
    vm.push(Value.string(ObjString.copy(vm, name)));
    const inner = ObjClass.init(vm, vm.peek(0).asString());
    vm.push(Value.class(inner));
    _ = outer.fields.add(vm.peek(1).asString(), Value.class(inner), true);
    _ = vm.pop();
    _ = vm.pop();
    return inner;
}

fn defineNative(vm: *Vm, class: *ObjClass, name: []const u8, native_fn: NativeFn) void {
    vm.push(Value.string(ObjString.copy(vm, name)));
    vm.push(Value.native(ObjNative.init(vm, native_fn)));
    _ = class.fields.add(vm.peek(1).asString(), vm.peek(0), true);
    _ = vm.pop();
    _ = vm.pop();
}

fn n_std_print(values: []Value) Value {
    for (values) |value| {
        value.print();
    }
    std.debug.print("\n", .{});
    return Value.nil();
}

fn n_std_time(values: []Value) Value {
    _ = values;
    const time = @intToFloat(f64, std.time.nanoTimestamp()) / std.time.ns_per_s;
    return Value.number(time);
}

fn n_std_list_len(values: []Value) Value {
    return Value.number(@intToFloat(f64, values[0].asList().items.items.len));
}
