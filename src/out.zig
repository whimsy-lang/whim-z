const std = @import("std");

var stdout_file: std.fs.File.Writer = undefined;
var buffered_writer: std.io.BufferedWriter(4096, std.fs.File.Writer) = undefined;
var stdout: std.io.BufferedWriter(4096, std.fs.File.Writer).Writer = undefined;

const Color = enum {
    blue,
    dark_green,
    dark_red,
    gray,
    green,
    orange,
    pink,
    purple,
    red,
    teal,
    yellow,

    fn val(self: Color) struct { u8, u8, u8 } {
        return switch (self) {
            .blue => .{ 0, 0x80, 0xff },
            .dark_green => .{ 0, 0x80, 0 },
            .dark_red => .{ 0xa0, 0, 0 },
            .gray => .{ 0x60, 0x60, 0x60 },
            .green => .{ 0, 0xff, 0 },
            .orange => .{ 0xff, 0x80, 0x40 },
            .pink => .{ 0xff, 0x80, 0xff },
            .purple => .{ 0x80, 0x40, 0xa0 },
            .red => .{ 0xff, 0, 0 },
            .teal => .{ 0, 0xa0, 0xa0 },
            .yellow => .{ 0xff, 0xff, 0x80 },
        };
    }
};

pub fn init() void {
    stdout_file = std.io.getStdOut().writer();
    buffered_writer = std.io.bufferedWriter(stdout_file);
    stdout = buffered_writer.writer();
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
    stdout.print(fmt, args) catch {
        std.debug.print("Could not print to stdout.", .{});
        std.process.exit(1);
    };
}

pub fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt ++ "\n", args);
}

pub fn printColor(comptime fmt: []const u8, args: anytype, c: Color) void {
    color(c);
    print(fmt, args);
    reset();
}

pub fn printlnColor(comptime fmt: []const u8, args: anytype, c: Color) void {
    color(c);
    print(fmt, args);
    reset();
    println("", .{});
}

pub fn printExit(comptime fmt: []const u8, args: anytype, status: u8) noreturn {
    print(fmt, args);
    flush();
    std.process.exit(status);
}

pub fn flush() void {
    buffered_writer.flush() catch {
        std.debug.print("Could not flush stdout.", .{});
        std.process.exit(1);
    };
}

pub fn color(c: Color) void {
    print("\x1b[38;2;{d};{d};{d}m", c.val());
}

pub fn bgColor(c: Color) void {
    print("\x1b[48;2;{d};{d};{d}m", c.val());
}

pub fn reset() void {
    ansiCode(0);
}

pub fn bold() void {
    ansiCode(1);
}

pub fn italic() void {
    ansiCode(3);
}

pub fn underline() void {
    ansiCode(4);
}

pub fn strikethrough() void {
    ansiCode(9);
}

fn ansiCode(code: u8) void {
    print("\x1b[{d}m", .{code});
}
