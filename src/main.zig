const std = @import("std");
const Allocator = std.mem.Allocator;

const out = @import("out.zig");
const version = @import("vm.zig").version;
const Vm = @import("vm.zig").Vm;

pub fn main() !void {
    out.init();
    defer out.flush();
    out.println("{s}", .{version});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var arg_list = std.ArrayList([]const u8).init(allocator);
    defer arg_list.deinit();

    while (args.next()) |arg| try arg_list.append(arg);

    var valid = false;
    if (arg_list.items.len >= 2 and std.mem.eql(u8, arg_list.items[1], "repl")) {
        var vm: Vm = undefined;
        vm.init(allocator);
        defer vm.deinit();
        valid = true;
        parseFlags(arg_list.items[2..]);
        try repl(&vm);
    } else if (arg_list.items.len >= 3 and std.mem.eql(u8, arg_list.items[1], "run")) {
        var vm: Vm = undefined;
        vm.init(allocator);
        defer vm.deinit();
        valid = true;
        parseFlags(arg_list.items[3..]);
        try runFile(allocator, &vm, arg_list.items[2]);
    }
    if (!valid) {
        out.printExit("Usage: whim [command] [flags]\n  Commands: repl, run [file]\n  Flags: -no-style", .{}, 64);
    }
}

fn parseFlags(flags: []const []const u8) void {
    for (flags) |flag| {
        if (std.mem.eql(u8, flag, "-no-style")) {
            out.no_style = true;
        }
    }
}

fn repl(vm: *Vm) !void {
    const stdin_file = std.io.getStdIn().reader();
    var reader = std.io.bufferedReader(stdin_file);
    const stdin = reader.reader();

    var buffer: [1024]u8 = undefined;

    while (true) {
        out.printColor("> ", .{}, .yellow);
        out.flush();

        if (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            _ = vm.interpret(line);
        }
    }
}

fn runFile(allocator: Allocator, vm: *Vm, path: []const u8) !void {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    const result = vm.interpret(source);

    if (result == .compile_error) out.printExit("", .{}, 65);
    if (result == .runtime_error) out.printExit("", .{}, 70);
}
