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
        return self.as == val_type;
    }

    pub fn equal(self: Value, other: Value) bool {
        if (@enumToInt(self.as) != @enumToInt(other.as)) return false;
        return switch (self.as) {
            .bool => self.as.bool == other.as.bool,
            .nil => true,
            .number => self.as.number == other.as.number,
        };
    }

    pub fn print(self: Value) void {
        switch (self.as) {
            .bool => std.debug.print("{s}", .{if (self.as.bool) "true" else "false"}),
            .nil => std.debug.print("nil", .{}),
            .number => std.debug.print("{d}", .{self.as.number}),
        }
    }
};
