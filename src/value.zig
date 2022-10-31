const std = @import("std");

const debug = @import("debug.zig");
const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjNative = @import("object.zig").ObjNative;
const ObjString = @import("object.zig").ObjString;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const Vm = @import("vm.zig").Vm;

pub const ValueType = enum {
    bool,
    nil,
    number,

    closure,
    function,
    native,
    string,
    upvalue,
};

pub const Value = struct {
    as: union(ValueType) {
        bool: bool,
        nil: void,
        number: f64,

        closure: *ObjClosure,
        function: *ObjFunction,
        native: *ObjNative,
        string: *ObjString,
        upvalue: *ObjUpvalue,
    },

    pub fn getType(self: Value) ValueType {
        return self.as;
    }

    pub fn boolean(value: bool) Value {
        return .{ .as = .{ .bool = value } };
    }

    pub fn closure(value: *ObjClosure) Value {
        return .{ .as = .{ .closure = value } };
    }

    pub fn function(value: *ObjFunction) Value {
        return .{ .as = .{ .function = value } };
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

    pub fn asClosure(self: Value) *ObjClosure {
        return self.as.closure;
    }

    pub fn asFunction(self: Value) *ObjFunction {
        return self.as.function;
    }

    pub fn asNative(self: Value) *ObjNative {
        return self.as.native;
    }

    pub fn asNumber(self: Value) f64 {
        return self.as.number;
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
            .bool => self.asBool() == other.asBool(),
            .nil => true,
            .number => self.asNumber() == other.asNumber(),

            .closure => self.asClosure() == other.asClosure(),
            .function => self.asFunction() == other.asFunction(),
            .native => self.asNative() == other.asNative(),
            .string => self.asString() == other.asString(),
            .upvalue => self.asUpvalue() == other.asUpvalue(),
        };
    }

    pub fn print(self: Value) void {
        switch (self.getType()) {
            .bool => std.debug.print("{s}", .{if (self.asBool()) "true" else "false"}),
            .nil => std.debug.print("nil", .{}),
            .number => std.debug.print("{d}", .{self.asNumber()}),

            .closure => self.asClosure().function.print(),
            .function => self.asFunction().print(),
            .native => std.debug.print("<native fn>", .{}),
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

        // don't bother queueing up native functions or strings since they
        // do not have references to check
        if (self.getType() == .native or self.getType() == .string) return;

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
            .closure => {
                const clos = self.asClosure();
                Value.function(clos.function).mark(vm);
                for (clos.upvalues) |upval| {
                    if (upval) |up| Value.upvalue(up).mark(vm);
                }
            },
            .function => {
                const func = self.asFunction();
                if (func.name) |name| {
                    Value.string(name).mark(vm);
                }
                markArray(&func.chunk.constants, vm);
            },
            .upvalue => self.asUpvalue().closed.mark(vm),
            else => unreachable,
        }
    }

    pub fn getMarked(self: Value) bool {
        return switch (self.getType()) {
            .closure => self.asClosure().is_marked,
            .function => self.asFunction().is_marked,
            .native => self.asNative().is_marked,
            .string => self.asString().is_marked,
            .upvalue => self.asUpvalue().is_marked,

            else => true,
        };
    }

    pub fn setMarked(self: Value, mark_val: bool) void {
        switch (self.getType()) {
            .closure => self.asClosure().is_marked = mark_val,
            .function => self.asFunction().is_marked = mark_val,
            .native => self.asNative().is_marked = mark_val,
            .string => self.asString().is_marked = mark_val,
            .upvalue => self.asUpvalue().is_marked = mark_val,

            else => unreachable,
        }
    }
};
