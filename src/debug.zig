const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");

pub const print_code = true;
pub const trace_execution = true;
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
        .pop => simpleInstruction("pop", offset),
        .define_global_const => constantInstruction("def const global", chunk, offset),
        .define_global_var => constantInstruction("def var global", chunk, offset),
        .get_global => constantInstruction("get global", chunk, offset),
        .set_global => constantInstruction("set global", chunk, offset),
        .get_local_0 => simpleInstruction("get local 0", offset),
        .get_local_1 => simpleInstruction("get local 1", offset),
        .get_local_2 => simpleInstruction("get local 2", offset),
        .get_local_3 => simpleInstruction("get local 3", offset),
        .get_local_4 => simpleInstruction("get local 4", offset),
        .get_local_5 => simpleInstruction("get local 5", offset),
        .get_local_6 => simpleInstruction("get local 6", offset),
        .get_local_7 => simpleInstruction("get local 7", offset),
        .get_local_8 => simpleInstruction("get local 8", offset),
        .get_local_9 => simpleInstruction("get local 9", offset),
        .get_local_10 => simpleInstruction("get local 10", offset),
        .get_local_11 => simpleInstruction("get local 11", offset),
        .get_local_12 => simpleInstruction("get local 12", offset),
        .get_local_13 => simpleInstruction("get local 13", offset),
        .get_local_14 => simpleInstruction("get local 14", offset),
        .get_local_15 => simpleInstruction("get local 15", offset),
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
        .define_property_const => constantInstruction("def const prop", chunk, offset),
        .define_property_const_pop => constantInstruction("def const prop (pop)", chunk, offset),
        .define_property_var => constantInstruction("def var prop", chunk, offset),
        .define_property_var_pop => constantInstruction("def var prop (pop)", chunk, offset),
        .define_super => simpleInstruction("def super", offset),
        .get_property => constantInstruction("get prop", chunk, offset),
        .get_property_pop => constantInstruction("get prop (pop)", chunk, offset),
        .set_property => constantInstruction("set prop", chunk, offset),
        .define_indexer_const => simpleInstruction("def indexer const", offset),
        .define_indexer_var => simpleInstruction("def indexer var", offset),
        .get_indexer => simpleInstruction("get indexer", offset),
        .get_indexer_pop => simpleInstruction("get indexer (pop)", offset),
        .set_indexer => simpleInstruction("set indexer", offset),
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
        .list => byteInstruction("list", chunk, offset),
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
