const std = @import("std");

const debug = @import("debug.zig");
const Object = @import("object.zig").Object;
const ObjectType = @import("object.zig").ObjectType;
const ObjClass = @import("object.zig").ObjClass;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjInstance = @import("object.zig").ObjInstance;
const ObjList = @import("object.zig").ObjList;
const ObjMap = @import("object.zig").ObjMap;
const ObjNative = @import("object.zig").ObjNative;
const ObjRange = @import("object.zig").ObjRange;
const ObjSet = @import("object.zig").ObjSet;
const ObjString = @import("object.zig").ObjString;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const out = @import("out.zig");
const Vm = @import("vm.zig").Vm;

const quiet_nan: u64 = 0b01111111_11111100_00000000_00000000_00000000_00000000_00000000_00000000;
const sign_bit: u64 = 0b10000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000;
const obj_tag = sign_bit | quiet_nan;
const empty_val = quiet_nan;
const nil_val = quiet_nan | 1;
const true_val = quiet_nan | 2;
const false_val = quiet_nan | 3;

pub const ValueContainer = struct {
    value: Value,
    constant: bool = false,
};

pub const Value = u64;

// initializers

pub fn boolean(val: bool) Value {
    return if (val) true_val else false_val;
}

pub fn class(val: *ObjClass) Value {
    return object(&val.obj);
}

pub fn closure(val: *ObjClosure) Value {
    return object(&val.obj);
}

pub fn empty() Value {
    return empty_val;
}

pub fn function(val: *ObjFunction) Value {
    return object(&val.obj);
}

pub fn instance(val: *ObjInstance) Value {
    return object(&val.obj);
}

pub fn list(val: *ObjList) Value {
    return object(&val.obj);
}

pub fn map(val: *ObjMap) Value {
    return object(&val.obj);
}

pub fn native(val: *ObjNative) Value {
    return object(&val.obj);
}

pub fn nil() Value {
    return nil_val;
}

pub fn number(val: f64) Value {
    return @ptrCast(*const u64, &val).*;
}

pub fn object(val: *Object) Value {
    return obj_tag | @ptrCast(*const u64, &val).*;
}

pub fn range(val: *ObjRange) Value {
    return object(&val.obj);
}

pub fn set(val: *ObjSet) Value {
    return object(&val.obj);
}

pub fn string(val: *ObjString) Value {
    return object(&val.obj);
}

pub fn upvalue(val: *ObjUpvalue) Value {
    return object(&val.obj);
}

// checks

pub fn isBool(val: Value) bool {
    return val == true_val or val == false_val;
}

pub fn isEmpty(val: Value) bool {
    return val == empty_val;
}

pub fn isNil(val: Value) bool {
    return val == nil_val;
}

pub fn isNumber(val: Value) bool {
    return (val & quiet_nan) != quiet_nan;
}

pub fn isObject(val: Value) bool {
    return (val & obj_tag) == obj_tag;
}

pub fn isObjType(val: Value, obj_type: ObjectType) bool {
    return isObject(val) and asObject(val).type == obj_type;
}

// casts

pub fn asBool(val: Value) bool {
    return val == true_val;
}

pub fn asClass(val: Value) *ObjClass {
    return asObject(val).asClass();
}

pub fn asClosure(val: Value) *ObjClosure {
    return asObject(val).asClosure();
}

pub fn asFunction(val: Value) *ObjFunction {
    return asObject(val).asFunction();
}

pub fn asInstance(val: Value) *ObjInstance {
    return asObject(val).asInstance();
}

pub fn asList(val: Value) *ObjList {
    return asObject(val).asList();
}

pub fn asMap(val: Value) *ObjMap {
    return asObject(val).asMap();
}

pub fn asNative(val: Value) *ObjNative {
    return asObject(val).asNative();
}

pub fn asNumber(val: Value) f64 {
    return @ptrCast(*const f64, &val).*;
}

pub fn asObject(val: Value) *Object {
    const addr = val & ~(obj_tag);
    return @ptrCast(*const *Object, &addr).*;
}

pub fn asRange(val: Value) *ObjRange {
    return asObject(val).asRange();
}

pub fn asSet(val: Value) *ObjSet {
    return asObject(val).asSet();
}

pub fn asString(val: Value) *ObjString {
    return asObject(val).asString();
}

pub fn asUpvalue(val: Value) *ObjUpvalue {
    return asObject(val).asUpvalue();
}

pub fn isFalsey(val: Value) bool {
    return val == nil_val or val == false_val;
}

pub fn hasStdClass(val: Value) bool {
    if (isObject(val)) {
        return switch (asObject(val).type) {
            .instance, .upvalue => false,
            else => true,
        };
    }
    return !isEmpty(val);
}

pub fn stdClass(val: Value, vm: *Vm) *ObjClass {
    if (isBool(val)) return vm.bool_class.?;
    if (isNil(val)) return vm.nil_class.?;
    if (isNumber(val)) return vm.number_class.?;
    if (isObject(val)) return switch (asObject(val).type) {
        .class => vm.class_class.?,
        .closure, .function, .native => vm.function_class.?,
        .list => vm.list_class.?,
        .map => vm.map_class.?,
        .range => vm.range_class.?,
        .set => vm.set_class.?,
        .string => vm.string_class.?,
        else => unreachable,
    };
    unreachable;
}

pub fn print(val: Value) void {
    if (isBool(val)) {
        if (asBool(val)) {
            out.printColor("true", .{}, .blue);
        } else {
            out.printColor("false", .{}, .blue);
        }
    } else if (isNil(val)) {
        out.printColor("nil", .{}, .dark_red);
    } else if (isNumber(val)) {
        out.printColor("{d}", .{asNumber(val)}, .dark_green);
    } else if (isObject(val)) {
        asObject(val).print();
    } else {
        out.print("empty", .{});
    }
}

pub fn toString(val: Value, vm: *Vm) *ObjString {
    if (isBool(val)) return ObjString.copy(vm, if (asBool(val)) "true" else "false");
    if (isNil(val)) return ObjString.copy(vm, "nil");
    if (isNumber(val)) {
        const chars = std.fmt.allocPrint(vm.allocator, "{d}", .{asNumber(val)}) catch {
            out.printExit("Could not allocate memory for string.", .{}, 1);
        };
        return ObjString.take(vm, chars);
    }
    if (isObject(val)) return asObject(val).toString(vm);
    return ObjString.copy(vm, "empty");
}

pub fn mark(val: Value, vm: *Vm) void {
    if (isObject(val)) asObject(val).mark(vm);
}

pub fn hash(val: Value) u32 {
    if (isBool(val)) return calcPtrHash(&asBool(val));
    if (isNil(val)) return 0;
    if (isNumber(val)) return calcPtrHash(&asNumber(val));
    if (isObject(val)) return switch (asObject(val).type) {
        .class => calcPtrHash(&asClass(val)),
        .closure => calcPtrHash(&asClosure(val)),
        .function => calcPtrHash(&asFunction(val)),
        .instance => calcPtrHash(&asInstance(val)),
        .list => calcPtrHash(&asList(val)),
        .map => calcPtrHash(&asMap(val)),
        .native => calcPtrHash(&asNative(val)),
        .range => calcPtrHash(&asRange(val)),
        .set => calcPtrHash(&asSet(val)),
        .string => asString(val).hash,
        .upvalue => calcPtrHash(&asUpvalue(val)),
    };
    unreachable;
}

fn calcPtrHash(ptr: anytype) u32 {
    const bytes = std.mem.asBytes(ptr);
    return calcHash(bytes);
}

pub fn calcHash(bytes: []const u8) u32 {
    var result: u32 = 2166136261;
    var i: usize = 0;
    while (i < bytes.len) : (i += 1) {
        result ^= bytes[i];
        result *%= 16777619;
    }
    return result;
}
