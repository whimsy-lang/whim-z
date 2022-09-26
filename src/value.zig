const std = @import("std");

const ObjString = @import("object.zig").ObjString;

pub const ValueType = enum {
    bool,
    nil,
    number,

    string,
};

pub const Value = struct {
    as: union(ValueType) {
        bool: bool,
        nil: void,
        number: f64,

        string: *ObjString,
    },

    pub fn getType(self: Value) ValueType {
        return self.as;
    }

    pub fn boolean(value: bool) Value {
        return .{ .as = .{ .bool = value } };
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

    pub fn is(self: Value, val_type: ValueType) bool {
        return self.getType() == val_type;
    }

    pub fn asBool(self: Value) bool {
        return self.as.bool;
    }

    pub fn asNum(self: Value) f64 {
        return self.as.number;
    }

    pub fn asString(self: Value) *ObjString {
        return self.as.string;
    }

    pub fn isFalsey(self: Value) bool {
        return self.is(.nil) or (self.is(.bool) and !self.asBool());
    }

    pub fn equal(self: Value, other: Value) bool {
        if (!self.is(other.getType())) return false;
        return switch (self.getType()) {
            .bool => self.asBool() == other.asBool(),
            .nil => true,
            .number => self.asNum() == other.asNum(),

            .string => self.asString() == other.asString(),
        };
    }

    pub fn print(self: Value) void {
        switch (self.getType()) {
            .bool => std.debug.print("{s}", .{if (self.asBool()) "true" else "false"}),
            .nil => std.debug.print("nil", .{}),
            .number => std.debug.print("{d}", .{self.asNum()}),

            .string => std.debug.print("{s}", .{self.asString().chars}),
        }
    }
};
