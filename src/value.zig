const std = @import("std");

pub const ValueType = enum {
    bool,
    nil,
    number,
};

pub const Value = struct {
    as: union(ValueType) {
        bool: bool,
        nil: void,
        number: f64,
    },
    constant: bool = false,

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

    pub fn is(self: Value, val_type: ValueType) bool {
        return self.getType() == val_type;
    }

    pub fn isConst(self: Value) bool {
        return self.constant;
    }

    pub fn asBool(self: Value) bool {
        return self.as.bool;
    }

    pub fn asNum(self: Value) f64 {
        return self.as.number;
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
        };
    }

    pub fn print(self: Value) void {
        switch (self.getType()) {
            .bool => std.debug.print("{s}", .{if (self.asBool()) "true" else "false"}),
            .nil => std.debug.print("nil", .{}),
            .number => std.debug.print("{d}", .{self.asNum()}),
        }
    }
};
