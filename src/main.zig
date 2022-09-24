const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const debug = @import("debug.zig");
const OpCode = @import("chunk.zig").OpCode;
const GcAllocator = @import("memory.zig").GcAllocater;
const Vm = @import("vm.zig").Vm;

pub fn main() void {
    std.debug.print("Whimsy v0.1\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var gc = GcAllocator.init(gpa.allocator());

    var vm: Vm = undefined;
    vm.init();
    var chunk = Chunk.init(gc.allocator());

    const constant = chunk.addConstant(8);
    chunk.writeOp(OpCode.constant, 123);
    chunk.write(@intCast(u8, constant), 123);

    const c2 = chunk.addConstant(3);
    chunk.writeOp(OpCode.constant, 124);
    chunk.write(@intCast(u8, c2), 124);

    chunk.writeOp(OpCode.subtract, 124);

    chunk.writeOp(OpCode.return_, 125);
    debug.disassembleChunk(&chunk, "test chunk");
    _ = vm.interpret(&chunk);
    vm.deinit();
    chunk.deinit();

    std.process.exit(0);
}
