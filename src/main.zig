const std = @import("std");
const Allocator = std.mem.Allocator;

const out = @import("out.zig");
const version = @import("vm.zig").version;
const Vm = @import("vm.zig").Vm;

pub fn main() !void {
    out.init();
    out.println("{s}", .{version});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var argList = std.ArrayList([]const u8).init(allocator);
    defer argList.deinit();

    while (args.next()) |arg| try argList.append(arg);

    var vm: Vm = undefined;
    vm.init(allocator);
    defer vm.deinit();

    if (argList.items.len == 1) {
        try repl(&vm);
    } else if (argList.items.len == 2) {
        try runFile(allocator, &vm, argList.items[1]);
    } else {
        out.printExit("Usage: whim [path]", .{}, 64);
    }

    out.flush();
}

fn repl(vm: *Vm) !void {
    const stdin_file = std.io.getStdIn().reader();
    var reader = std.io.bufferedReader(stdin_file);
    const stdin = reader.reader();

    var buffer: [1024]u8 = undefined;

    while (true) {
        out.print("> ", .{});
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
