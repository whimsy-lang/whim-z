const std = @import("std");

var stdout_file: std.fs.File.Writer = undefined;
var buffered_writer: std.io.BufferedWriter(4096, std.fs.File.Writer) = undefined;
var stdout: std.io.BufferedWriter(4096, std.fs.File.Writer).Writer = undefined;

const set_color_str = "\x1b[38;2;{d};{d};{d}m";
const set_bg_color_str = "\x1b[48;2;{d};{d};{d}m";
const reset_str = "\x1b[0m";

pub fn init() void {
    stdout_file = std.io.getStdOut().writer();
    buffered_writer = std.io.bufferedWriter(stdout_file);
    stdout = buffered_writer.writer();
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
    stdout.print(fmt, args) catch {
        printExit("Could not print to stdout.", .{}, 1);
    };
}

pub fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt ++ "\n", args);
}

pub fn printColor(comptime fmt: []const u8, args: anytype, r: u8, g: u8, b: u8) void {
    print(set_color_str ++ fmt ++ reset_str, .{ r, g, b } ++ args);
}

pub fn printlnColor(comptime fmt: []const u8, args: anytype, r: u8, g: u8, b: u8) void {
    print(set_color_str ++ fmt ++ reset_str ++ "\n", .{ r, g, b } ++ args);
}

pub fn printExit(comptime fmt: []const u8, args: anytype, status: u8) void {
    std.debug.print(fmt, args);
    std.process.exit(status);
}

pub fn flush() void {
    buffered_writer.flush() catch {
        printExit("Could not flush stdout.", .{}, 1);
    };
}

pub fn color(r: u8, g: u8, b: u8) void {
    print(set_color_str, .{ r, g, b });
}

pub fn bgColor(r: u8, g: u8, b: u8) void {
    print(set_bg_color_str, .{ r, g, b });
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
