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
        .num_9 => simpleInstruction("num 9", offset),
        .num_10 => simpleInstruction("num 10", offset),

        .dup => simpleInstruction("dup", offset),
        .pop => simpleInstruction("pop", offset),

        .define_global_const => constantInstruction("def const global", chunk, offset),
        .define_global_var => constantInstruction("def var global", chunk, offset),
        .get_global => constantInstruction("get global", chunk, offset),
        .set_global => constantInstruction("set global", chunk, offset),

        .get_local => byteInstruction("get local", chunk, offset),

        .set_local_0 => simpleInstruction("set local 0", offset),
        .set_local_1 => simpleInstruction("set local 1", offset),
        .set_local_2 => simpleInstruction("set local 2", offset),
        .set_local_3 => simpleInstruction("set local 3", offset),
        .set_local_4 => simpleInstruction("set local 4", offset),
        .set_local_5 => simpleInstruction("set local 5", offset),
        .set_local_6 => simpleInstruction("set local 6", offset),
        .set_local_7 => simpleInstruction("set local 7", offset),
        .set_local_8 => simpleInstruction("set local 8", offset),
        .set_local_9 => simpleInstruction("set local 9", offset),
        .set_local_10 => simpleInstruction("set local 10", offset),
        .set_local_11 => simpleInstruction("set local 11", offset),
        .set_local_12 => simpleInstruction("set local 12", offset),
        .set_local_13 => simpleInstruction("set local 13", offset),
        .set_local_14 => simpleInstruction("set local 14", offset),
        .set_local_15 => simpleInstruction("set local 15", offset),
        .set_local => byteInstruction("set local", chunk, offset),

        .get_upvalue_0 => simpleInstruction("get upvalue 0", offset),
        .get_upvalue_1 => simpleInstruction("get upvalue 1", offset),
        .get_upvalue_2 => simpleInstruction("get upvalue 2", offset),
        .get_upvalue_3 => simpleInstruction("get upvalue 3", offset),
        .get_upvalue => byteInstruction("get upvalue", chunk, offset),

        .set_upvalue_0 => simpleInstruction("set upvalue 0", offset),
        .set_upvalue_1 => simpleInstruction("set upvalue 1", offset),
        .set_upvalue_2 => simpleInstruction("set upvalue 2", offset),
        .set_upvalue_3 => simpleInstruction("set upvalue 3", offset),
        .set_upvalue => byteInstruction("set upvalue", chunk, offset),

        .define_const_by_const => constantInstruction("def const by c", chunk, offset),
        .define_const_by_const_pop => constantInstruction("def const by c (pop)", chunk, offset),
        .define_var_by_const => constantInstruction("def var by c", chunk, offset),
        .define_var_by_const_pop => constantInstruction("def var by c (pop)", chunk, offset),
        .define_super => simpleInstruction("def super", offset),
        .get_by_const => constantInstruction("get by c", chunk, offset),
        .get_by_const_pop => constantInstruction("get by c (pop)", chunk, offset),
        .set_by_const => constantInstruction("set by c", chunk, offset),

        .define_const => simpleInstruction("def const", offset),
        .define_const_pop => simpleInstruction("def const (pop)", offset),
        .define_var => simpleInstruction("def var", offset),
        .define_var_pop => simpleInstruction("def var (pop)", offset),
        .get => simpleInstruction("get", offset),
        .get_pop => simpleInstruction("get (pop)", offset),
        .set => simpleInstruction("set", offset),

        .equal => simpleInstruction("equal", offset),
        .not_equal => simpleInstruction("not equal", offset),
        .greater => simpleInstruction("greater", offset),
        .greater_equal => simpleInstruction("greater equal", offset),
        .less => simpleInstruction("less", offset),
        .less_equal => simpleInstruction("less equal", offset),
        .is => simpleInstruction("is", offset),
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
        .jump_if_false_pop => jumpInstruction("jump if false (pop)", 1, chunk, offset),

        .call_0 => simpleInstruction("call (0)", offset),
        .call_1 => simpleInstruction("call (1)", offset),
        .call_2 => simpleInstruction("call (2)", offset),
        .call_3 => simpleInstruction("call (3)", offset),
        .call_4 => simpleInstruction("call (4)", offset),
        .call_5 => simpleInstruction("call (5)", offset),
        .call_6 => simpleInstruction("call (6)", offset),
        .call_7 => simpleInstruction("call (7)", offset),
        .call_8 => simpleInstruction("call (8)", offset),
        .call_9 => simpleInstruction("call (9)", offset),
        .call_10 => simpleInstruction("call (10)", offset),
        .call_11 => simpleInstruction("call (11)", offset),
        .call_12 => simpleInstruction("call (12)", offset),
        .call_13 => simpleInstruction("call (13)", offset),
        .call_14 => simpleInstruction("call (14)", offset),
        .call_15 => simpleInstruction("call (15)", offset),
        .call_16 => simpleInstruction("call (16)", offset),
        .call => byteInstruction("call", chunk, offset),

        .invoke_0 => constantInstruction("invoke (0)", chunk, offset),
        .invoke_1 => constantInstruction("invoke (1)", chunk, offset),
        .invoke_2 => constantInstruction("invoke (2)", chunk, offset),
        .invoke_3 => constantInstruction("invoke (3)", chunk, offset),
        .invoke_4 => constantInstruction("invoke (4)", chunk, offset),
        .invoke_5 => constantInstruction("invoke (5)", chunk, offset),
        .invoke_6 => constantInstruction("invoke (6)", chunk, offset),
        .invoke_7 => constantInstruction("invoke (7)", chunk, offset),
        .invoke_8 => constantInstruction("invoke (8)", chunk, offset),
        .invoke_9 => constantInstruction("invoke (9)", chunk, offset),
        .invoke_10 => constantInstruction("invoke (10)", chunk, offset),
        .invoke_11 => constantInstruction("invoke (11)", chunk, offset),
        .invoke_12 => constantInstruction("invoke (12)", chunk, offset),
        .invoke_13 => constantInstruction("invoke (13)", chunk, offset),
        .invoke_14 => constantInstruction("invoke (14)", chunk, offset),
        .invoke_15 => constantInstruction("invoke (15)", chunk, offset),
        .invoke_16 => constantInstruction("invoke (16)", chunk, offset),
        .invoke => invokeInstruction("invoke", chunk, offset),

        .closure => closureInstruction("closure", chunk, offset),
        .close_upvalue => simpleInstruction("close upvalue", offset),
        .return_ => simpleInstruction("return", offset),
        .class => constantInstruction("class", chunk, offset),
        .iterate_check => jumpInstruction("iterate check", 1, chunk, offset),
        .iterate_next => jumpInstruction("iterate next", -1, chunk, offset),
        .list => byteInstruction("list", chunk, offset),
        .map => simpleInstruction("map", offset),
        .map_with_const => simpleInstruction("map with const", offset),
        .map_with_var => simpleInstruction("map with var", offset),
        .range => simpleInstruction("range", offset),
        .range_inclusive => simpleInstruction("range incl", offset),
        .range_step => simpleInstruction("range w step", offset),
        .range_inclusive_step => simpleInstruction("range incl w step", offset),
        .new_set => byteInstruction("new set", chunk, offset),
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
