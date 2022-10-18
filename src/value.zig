const std = @import("std");

const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjNative = @import("object.zig").ObjNative;
const ObjString = @import("object.zig").ObjString;
const ObjUpvalue = @import("object.zig").ObjUpvalue;

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
};
