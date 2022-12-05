const std = @import("std");

const debug = @import("debug.zig");
const ObjClass = @import("object.zig").ObjClass;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjInstance = @import("object.zig").ObjInstance;
const ObjList = @import("object.zig").ObjList;
const ObjNative = @import("object.zig").ObjNative;
const ObjRange = @import("object.zig").ObjRange;
const ObjString = @import("object.zig").ObjString;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const Vm = @import("vm.zig").Vm;

pub const ValueContainer = struct {
    value: Value,
    constant: bool = false,
};

pub const ValueType = enum {
    empty,

    bool,
    nil,
    number,

    class,
    closure,
    function,
    instance,
    list,
    native,
    range,
    string,
    upvalue,
};

pub const Value = struct {
    as: union(ValueType) {
        empty: void,

        bool: bool,
        nil: void,
        number: f64,

        class: *ObjClass,
        closure: *ObjClosure,
        function: *ObjFunction,
        instance: *ObjInstance,
        list: *ObjList,
        native: *ObjNative,
        range: *ObjRange,
        string: *ObjString,
        upvalue: *ObjUpvalue,
    },

    pub fn getType(self: Value) ValueType {
        return self.as;
    }

    pub fn boolean(value: bool) Value {
        return .{ .as = .{ .bool = value } };
    }

    pub fn class(value: *ObjClass) Value {
        return .{ .as = .{ .class = value } };
    }

    pub fn closure(value: *ObjClosure) Value {
        return .{ .as = .{ .closure = value } };
    }

    pub fn empty() Value {
        return .{ .as = .empty };
    }

    pub fn function(value: *ObjFunction) Value {
        return .{ .as = .{ .function = value } };
    }

    pub fn instance(value: *ObjInstance) Value {
        return .{ .as = .{ .instance = value } };
    }

    pub fn list(value: *ObjList) Value {
        return .{ .as = .{ .list = value } };
    }

    pub fn native(value: *ObjNative) Value {
        return .{ .as = .{ .native = value } };
    }

    pub fn nil() Value {
        return .{ .as = .nil };
    }

    pub fn number(value: f64) Value {
        return .{ .as = .{ .number = value } };
    }

    pub fn range(value: *ObjRange) Value {
        return .{ .as = .{ .range = value } };
    }

    pub fn string(value: *ObjString) Value {
        return .{ .as = .{ .string = value } };
    }

    pub fn upvalue(value: *ObjUpvalue) Value {
        return .{ .as = .{ .upvalue = value } };
    }

    pub fn is(self: Value, val_type: ValueType) bool {
        return self.getType() == val_type;
    }

    pub fn asBool(self: Value) bool {
        return self.as.bool;
    }

    pub fn asClass(self: Value) *ObjClass {
        return self.as.class;
    }

    pub fn asClosure(self: Value) *ObjClosure {
        return self.as.closure;
    }

    pub fn asFunction(self: Value) *ObjFunction {
        return self.as.function;
    }

    pub fn asInstance(self: Value) *ObjInstance {
        return self.as.instance;
    }

    pub fn asList(self: Value) *ObjList {
        return self.as.list;
    }

    pub fn asNative(self: Value) *ObjNative {
        return self.as.native;
    }

    pub fn asNumber(self: Value) f64 {
        return self.as.number;
    }

    pub fn asRange(self: Value) *ObjRange {
        return self.as.range;
    }

    pub fn asString(self: Value) *ObjString {
        return self.as.string;
    }

    pub fn asUpvalue(self: Value) *ObjUpvalue {
        return self.as.upvalue;
    }

    pub fn isFalsey(self: Value) bool {
        return self.is(.nil) or (self.is(.bool) and !self.asBool());
    }

    pub fn equal(self: Value, other: Value) bool {
        if (!self.is(other.getType())) return false;
        return switch (self.getType()) {
            .empty => true,

            .bool => self.asBool() == other.asBool(),
            .nil => true,
            .number => self.asNumber() == other.asNumber(),

            .class => self.asClass() == other.asClass(),
            .closure => self.asClosure() == other.asClosure(),
            .function => self.asFunction() == other.asFunction(),
            .instance => self.asInstance() == other.asInstance(),
            .list => self.asList() == other.asList(),
            .native => self.asNative() == other.asNative(),
            .range => self.asRange() == other.asRange(),
            .string => self.asString() == other.asString(),
            .upvalue => self.asUpvalue() == other.asUpvalue(),
        };
    }

    pub fn hasStdClass(self: Value) bool {
        return switch (self.getType()) {
            .empty, .instance, .upvalue => false,
            else => true,
        };
    }

    pub fn stdClass(self: Value, vm: *Vm) *ObjClass {
        return switch (self.getType()) {
            .bool => vm.bool_class.?,
            .class => vm.class_class.?,
            .closure, .function, .native => vm.function_class.?,
            .list => vm.list_class.?,
            .nil => vm.nil_class.?,
            .number => vm.number_class.?,
            .range => vm.range_class.?,
            .string => vm.string_class.?,

            else => unreachable,
        };
    }

    pub fn print(self: Value) void {
        switch (self.getType()) {
            .empty => std.debug.print("empty", .{}),

            .bool => std.debug.print("{s}", .{if (self.asBool()) "true" else "false"}),
            .nil => std.debug.print("nil", .{}),
            .number => std.debug.print("{d}", .{self.asNumber()}),

            .class => if (self.asClass().name) |name| {
                std.debug.print("class {s}", .{name.chars});
            } else {
                std.debug.print("anon class", .{});
            },
            .closure => self.asClosure().function.print(),
            .function => self.asFunction().print(),
            .instance => if (self.asInstance().type.name) |name| {
                std.debug.print("{s} inst", .{name.chars});
            } else {
                std.debug.print("anon inst", .{});
            },
            .list => {
                std.debug.print("(", .{});
                var first = true;
                for (self.asList().items.items) |item| {
                    if (first) {
                        first = false;
                    } else {
                        std.debug.print(", ", .{});
                    }
                    item.print();
                }
                std.debug.print(")", .{});
            },
            .native => std.debug.print("<native fn>", .{}),
            .range => {
                const r = self.asRange();
                r.start.print();
                std.debug.print("{s}", .{if (r.inclusive) "..=" else ".."});
                r.end.print();
            },
            .string => std.debug.print("{s}", .{self.asString().chars}),
            .upvalue => std.debug.print("upvalue", .{}),
        }
    }

    pub fn mark(self: Value, vm: *Vm) void {
        if (self.getMarked()) return;

        if (debug.log_gc) {
            std.debug.print("mark {any}: ", .{self.getType()});
            self.print();
            std.debug.print("\n", .{});
        }

        self.setMarked(true);

        // don't bother queueing up strings or native functions since they
        // do not have references to check
        if (self.is(.string) or self.is(.native)) return;

        vm.gc.gray_stack.append(self) catch {
            std.debug.print("Could not allocate memory for garbage collection.", .{});
            std.process.exit(1);
        };
    }

    fn markArray(arr: *std.ArrayList(Value), vm: *Vm) void {
        for (arr.items) |val| val.mark(vm);
    }

    pub fn blacken(self: Value, vm: *Vm) void {
        if (debug.log_gc) {
            std.debug.print("blacken {any}: ", .{self.getType()});
            self.print();
            std.debug.print("\n", .{});
        }

        switch (self.getType()) {
            .class => {
                const clas = self.asClass();
                if (clas.name) |name| Value.string(name).mark(vm);
                if (clas.super) |super| Value.class(super).mark(vm);
                clas.fields.mark(vm);
            },
            .closure => {
                const clos = self.asClosure();
                Value.function(clos.function).mark(vm);
                for (clos.upvalues) |upval| {
                    if (upval) |up| Value.upvalue(up).mark(vm);
                }
            },
            .function => {
                const func = self.asFunction();
                if (func.name) |name| Value.string(name).mark(vm);
                markArray(&func.chunk.constants, vm);
            },
            .instance => {
                const inst = self.asInstance();
                Value.class(inst.type).mark(vm);
                inst.fields.mark(vm);
            },
            .list => markArray(&self.asList().items, vm),
            .range => {
                const r = self.asRange();
                r.start.mark(vm);
                r.end.mark(vm);
            },
            .upvalue => self.asUpvalue().closed.mark(vm),
            else => unreachable,
        }
    }

    pub fn getMarked(self: Value) bool {
        return switch (self.getType()) {
            .class => self.asClass().is_marked,
            .closure => self.asClosure().is_marked,
            .function => self.asFunction().is_marked,
            .instance => self.asInstance().is_marked,
            .list => self.asList().is_marked,
            .native => self.asNative().is_marked,
            .range => self.asRange().is_marked,
            .string => self.asString().is_marked,
            .upvalue => self.asUpvalue().is_marked,

            else => true,
        };
    }

    pub fn setMarked(self: Value, mark_val: bool) void {
        switch (self.getType()) {
            .class => self.asClass().is_marked = mark_val,
            .closure => self.asClosure().is_marked = mark_val,
            .function => self.asFunction().is_marked = mark_val,
            .instance => self.asInstance().is_marked = mark_val,
            .list => self.asList().is_marked = mark_val,
            .native => self.asNative().is_marked = mark_val,
            .range => self.asRange().is_marked = mark_val,
            .string => self.asString().is_marked = mark_val,
            .upvalue => self.asUpvalue().is_marked = mark_val,

            else => unreachable,
        }
    }

    pub fn hash(self: Value) u32 {
        // std.debug.print("hashing: ", .{});
        // self.print();
        // std.debug.print("\n", .{});

        return switch (self.getType()) {
            .empty => unreachable,

            .bool => calcPtrHash(&self.asBool()),
            .nil => 0,
            .number => calcPtrHash(&self.asNumber()),

            .class => calcPtrHash(&self.asClass()),
            .closure => calcPtrHash(&self.asClosure()),
            .function => calcPtrHash(&self.asFunction()),
            .instance => calcPtrHash(&self.asInstance()),
            .list => calcPtrHash(&self.asList()),
            .native => calcPtrHash(&self.asNative()),
            .range => calcPtrHash(&self.asRange()),
            .string => self.asString().hash,
            .upvalue => calcPtrHash(&self.asUpvalue()),
        };
    }

    fn calcPtrHash(ptr: anytype) u32 {
        const bytes = std.mem.asBytes(ptr);
        return calcHash(bytes);
    }

    pub fn calcHash(bytes: []const u8) u32 {
        // for (bytes) |b| {
        //     std.debug.print("{d} ", .{b});
        // }
        // std.debug.print("\n", .{});

        var result: u32 = 2166136261;
        var i: usize = 0;
        while (i < bytes.len) : (i += 1) {
            result ^= bytes[i];
            result *%= 16777619;
        }
        return result;
    }
};
