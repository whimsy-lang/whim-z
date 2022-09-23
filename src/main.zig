const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const debug = @import("debug.zig");
const OpCode = @import("chunk.zig").OpCode;
const GcAllocator = @import("memory.zig").GcAllocater;

pub fn main() void {
    std.debug.print("Whimsy v0.1\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var gc = GcAllocator.init(gpa.allocator());
    var chunk = Chunk.init(gc.allocator());

    const constant = chunk.addConstant(1.2);
    chunk.writeOp(OpCode.constant, 123);
    chunk.write(@intCast(u8, constant), 123);

    chunk.writeOp(OpCode.return_, 123);
    debug.disassembleChunk(&chunk, "test chunk");
    chunk.deinit();

    std.process.exit(0);
}
