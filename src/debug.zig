const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");

pub const print_code = true;
pub const trace_execution = false;
pub const stress_gc = false;
pub const log_gc = false;

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
        .num_n1 => simpleInstruction("num -1", offset),
        .num_0 => simpleInstruction("num 0", offset),
        .num_1 => simpleInstruction("num 1", offset),
        .num_2 => simpleInstruction("num 2", offset),
        .num_3 => simpleInstruction("num 3", offset),
        .num_4 => simpleInstruction("num 4", offset),
        .num_5 => simpleInstruction("num 5", offset),
        .num_6 => simpleInstruction("num 6", offset),
        .num_7 => simpleInstruction("num 7", offset),
        .num_8 => simpleInstruction("num 8", offset),
        .pop => simpleInstruction("pop", offset),
        .define_global_const => constantInstruction("def const global", chunk, offset),
        .define_global_var => constantInstruction("def var global", chunk, offset),
        .get_global => constantInstruction("get global", chunk, offset),
        .set_global => constantInstruction("set global", chunk, offset),
        .get_local => byteInstruction("get local", chunk, offset),
        .set_local => byteInstruction("set local", chunk, offset),
        .get_upvalue => byteInstruction("get upvalue", chunk, offset),
        .set_upvalue => byteInstruction("set upvalue", chunk, offset),
        .define_property_const => constantInstruction("def const prop", chunk, offset),
        .define_property_const_pop => constantInstruction("def const prop (pop)", chunk, offset),
        .define_property_var => constantInstruction("def var prop", chunk, offset),
        .define_property_var_pop => constantInstruction("def var prop (pop)", chunk, offset),
        .get_property => constantInstruction("get prop", chunk, offset),
        .get_property_pop => constantInstruction("get prop (pop)", chunk, offset),
        .set_property => constantInstruction("set prop", chunk, offset),
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
        .remainder => simpleInstruction("remainder", offset),
        .negate => simpleInstruction("negate", offset),
        .not => simpleInstruction("not", offset),
        .jump => jumpInstruction("jump", 1, chunk, offset),
        .jump_back => jumpInstruction("jump back", -1, chunk, offset),
        .jump_if_true => jumpInstruction("jump if true", 1, chunk, offset),
        .jump_if_false => jumpInstruction("jump if false", 1, chunk, offset),
        .jump_if_true_pop => jumpInstruction("jump if true (pop)", 1, chunk, offset),
        .jump_if_false_pop => jumpInstruction("jump if false (pop)", 1, chunk, offset),
        .call => byteInstruction("call", chunk, offset),
        .invoke => invokeInstruction("invoke", chunk, offset),
        .closure => closureInstruction("closure", chunk, offset),
        .close_upvalue => simpleInstruction("close upvalue", offset),
        .return_ => simpleInstruction("return", offset),
        .class => constantInstruction("class", chunk, offset),
    };
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const index = chunk.code.items[offset + 1];
    std.debug.print("{s: <20} {d:4}\n", .{ name, index });
    return offset + 2;
}

fn closureInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var cur_offset = offset + 1;
    const constant = chunk.code.items[cur_offset];
    cur_offset += 1;
    std.debug.print("{s: <20} {d:4} ", .{ name, constant });
    chunk.constants.items[constant].print();
    std.debug.print("\n", .{});

    const function = chunk.constants.items[constant].asFunction();
    var i: usize = 0;
    while (i < function.upvalue_count) : (i += 1) {
        const is_local = chunk.code.items[cur_offset];
        cur_offset += 1;
        const index = chunk.code.items[cur_offset];
        cur_offset += 1;
        std.debug.print("{d:0>4}      > {s} {d}\n", .{ cur_offset - 2, if (is_local == 1) "local" else "upvalue", index });
    }

    return cur_offset;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    std.debug.print("{s: <20} {d:4} '", .{ name, constant });
    chunk.constants.items[constant].print();
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn invokeInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    const arg_count = chunk.code.items[offset + 2];
    std.debug.print("{s: <20} {d:4} '", .{ name, constant });
    chunk.constants.items[constant].print();
    std.debug.print("' ({d})\n", .{arg_count});
    return offset + 3;
}

fn jumpInstruction(name: []const u8, sign: isize, chunk: *Chunk, offset: usize) usize {
    var jump = @as(u16, chunk.code.items[offset + 1]) << 8;
    jump |= chunk.code.items[offset + 2];
    std.debug.print("{s: <20} {d:4} -> {d}\n", .{ name, offset, @intCast(isize, offset) + 3 + sign * jump });
    return offset + 3;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
