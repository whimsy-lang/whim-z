// Variable-length encoding of unsigned integers.

const std = @import("std");

pub fn add(list: *std.ArrayList(u8), value: u29) !void {
    if (value > std.math.maxInt(u21)) {
        try list.appendSlice(&[_]u8{ @intCast(u8, value & 0x7f) | 0x80, @intCast(u8, (value >> 7) & 0x7f) | 0x80, @intCast(u8, (value >> 14) & 0x7f) | 0x80, @intCast(u8, value >> 21) });
    } else if (value > std.math.maxInt(u14)) {
        try list.appendSlice(&[_]u8{ @intCast(u8, value & 0x7f) | 0x80, @intCast(u8, (value >> 7) & 0x7f) | 0x80, @intCast(u8, value >> 14) });
    } else if (value > std.math.maxInt(u7)) {
        try list.appendSlice(&[_]u8{ @intCast(u8, value & 0x7f) | 0x80, @intCast(u8, value >> 7) });
    } else {
        try list.append(@intCast(u8, value));
    }
}

pub fn get(arr: [*]const u8) u29 {
    if (arr[0] < 0x80) return arr[0];
    if (arr[1] < 0x80) return (arr[0] & 0x7f) | (@intCast(u29, arr[1]) << 7);
    if (arr[2] < 0x80) return (arr[0] & 0x7f) | (@intCast(u29, arr[1] & 0x7f) << 7) | (@intCast(u29, arr[2]) << 14);
    return (arr[0] & 0x7f) | (@intCast(u29, arr[1] & 0x7f) << 7) | (@intCast(u29, arr[2] & 0x7f) << 14) | (@intCast(u29, arr[3]) << 21);
}

pub fn getIncrement(array: *[*]u8) u29 {
    const arr = array.*;
    array.* += valueLength(arr);
    return get(arr);
}

pub fn at(arr: []const u8, index: usize) u29 {
    var curIndex: usize = 0;
    var i: usize = 0;
    while (i < arr.len) {
        if (curIndex == index) return get(arr[i..].ptr);
        i += valueLength(arr[i..].ptr);
        curIndex += 1;
    }
    return 0;
}

pub fn sumTo(arr: []const u8, index: usize) u29 {
    var sum: u29 = 0;
    var curIndex: usize = 0;
    var i: usize = 0;
    while (i < arr.len) {
        sum += get(arr[i..].ptr);
        if (curIndex == index) return sum;
        i += valueLength(arr[i..].ptr);
        curIndex += 1;
    }
    return 0;
}

pub fn valueLength(arr: [*]const u8) usize {
    if (arr[0] < 0x80) return 1;
    if (arr[1] < 0x80) return 2;
    if (arr[2] < 0x80) return 3;
    return 4;
}
