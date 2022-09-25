const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");

pub const print_code = true;
pub const trace_execution = true;
pub const stress_gc = false;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{chunk.lines.items[offset]});
    }

    const instruction = chunk.code.items[offset];
    return switch (@intToEnum(OpCode, instruction)) {
        .constant => constantInstruction("constant", chunk, offset),
        .nil => simpleInstruction("nil", offset),
        .true => simpleInstruction("true", offset),
        .false => simpleInstruction("false", offset),
        .equal => simpleInstruction("equal", offset),
        .not_equal => simpleInstruction("not equal", offset),
        .greater => simpleInstruction("greater", offset),
        .greater_equal => simpleInstruction("greater equal", offset),
        .less => simpleInstruction("less", offset),
        .less_equal => simpleInstruction("less equal", offset),
        .add => simpleInstruction("add", offset),
        .subtract => simpleInstruction("subtract", offset),
        .multiply => simpleInstruction("multiply", offset),
        .divide => simpleInstruction("divide", offset),
        .modulus => simpleInstruction("modulus", offset),
        .negate => simpleInstruction("negate", offset),
        .not => simpleInstruction("not", offset),
        .return_ => simpleInstruction("return", offset),
        else => offset + 1,
    };
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s: <20} {d:4} '", .{ name, constant });
    chunk.constants.items[constant].print();
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
