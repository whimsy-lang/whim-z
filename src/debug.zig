const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const value = @import("value.zig");
const vle = @import("vle.zig");

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
    if (offset > 0 and chunk.getLine(offset) == chunk.getLine(offset - 1)) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:4} ", .{chunk.getLine(offset)});
    }

    const instruction = chunk.code.items[offset];
    return switch (@intToEnum(OpCode, instruction)) {
        .constant => constantInstruction("constant", chunk, offset),
        .nil => simpleInstruction("nil", offset),
        .true => simpleInstruction("true", offset),
        .false => simpleInstruction("false", offset),

        .dup => simpleInstruction("dup", offset),
        .pop => simpleInstruction("pop", offset),

        .define_global_const => constantInstruction("def const global", chunk, offset),
        .define_global_var => constantInstruction("def var global", chunk, offset),
        .get_global => constantInstruction("get global", chunk, offset),
        .set_global => constantInstruction("set global", chunk, offset),

        .get_local => numInstruction("get local", chunk, offset),
        .set_local => numInstruction("set local", chunk, offset),

        .get_upvalue => numInstruction("get upvalue", chunk, offset),
        .set_upvalue => numInstruction("set upvalue", chunk, offset),

        .define_const => simpleInstruction("def const", offset),
        .define_const_pop => simpleInstruction("def const (pop)", offset),
        .define_var => simpleInstruction("def var", offset),
        .define_var_pop => simpleInstruction("def var (pop)", offset),
        .get => simpleInstruction("get", offset),
        .get_pop => simpleInstruction("get (pop)", offset),
        .set => simpleInstruction("set", offset),

        .get_by_const => constantInstruction("get by const", chunk, offset),
        .get_by_const_pop => constantInstruction("get by const (pop)", chunk, offset),
        .set_by_const => constantInstruction("set by const", chunk, offset),

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

        .call => numInstruction("call", chunk, offset),
        .invoke => invokeInstruction("invoke", chunk, offset),

        .closure => closureInstruction("closure", chunk, offset),
        .close_upvalue => simpleInstruction("close upvalue", offset),
        .return_ => simpleInstruction("return", offset),
        .class => constantInstruction("class", chunk, offset),
        .iterate_check => jumpInstruction("iterate check", 1, chunk, offset),
        .iterate_next => jumpInstruction("iterate next", -1, chunk, offset),
        .list => numInstruction("list", chunk, offset),
        .map => simpleInstruction("map", offset),
        .map_with_const => simpleInstruction("map with const", offset),
        .map_with_var => simpleInstruction("map with var", offset),
        .range => simpleInstruction("range", offset),
        .range_inclusive => simpleInstruction("range incl", offset),
        .range_step => simpleInstruction("range w step", offset),
        .range_inclusive_step => simpleInstruction("range incl w step", offset),
        .new_set => numInstruction("new set", chunk, offset),
    };
}

fn closureInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var cur_offset = offset + 1;
    const constant = vle.get(chunk.code.items.ptr + cur_offset);
    cur_offset += vle.valueLength(chunk.code.items.ptr + cur_offset);
    std.debug.print("{s: <20} {d:4} ", .{ name, constant });
    value.print(chunk.constants.items[constant]);
    std.debug.print("\n", .{});

    const function = value.asFunction(chunk.constants.items[constant]);
    var i: usize = 0;
    while (i < function.upvalue_count) : (i += 1) {
        const is_local = chunk.code.items[cur_offset];
        cur_offset += 1;
        const index = vle.get(chunk.code.items.ptr + cur_offset);
        cur_offset += vle.valueLength(chunk.code.items.ptr + cur_offset);
        std.debug.print("{d:0>4}      > {s} {d}\n", .{ cur_offset - 2, if (is_local == 1) "local" else "upvalue", index });
    }

    return cur_offset;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = vle.get(chunk.code.items.ptr + offset + 1);
    std.debug.print("{s: <20} {d:4} '", .{ name, constant });
    value.print(chunk.constants.items[constant]);
    std.debug.print("'\n", .{});
    return offset + vle.valueLength(chunk.code.items.ptr + offset + 1) + 1;
}

fn invokeInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    var cur_offset = offset + 1;
    const constant = vle.get(chunk.code.items.ptr + cur_offset);
    cur_offset += vle.valueLength(chunk.code.items.ptr + cur_offset);
    const arg_count = vle.get(chunk.code.items.ptr + cur_offset);
    cur_offset += vle.valueLength(chunk.code.items.ptr + cur_offset);
    std.debug.print("{s: <20} {d:4} '", .{ name, constant });
    value.print(chunk.constants.items[constant]);
    std.debug.print("' ({d})\n", .{arg_count});
    return cur_offset;
}

fn jumpInstruction(name: []const u8, sign: isize, chunk: *Chunk, offset: usize) usize {
    var jump = @as(u16, chunk.code.items[offset + 1]) << 8;
    jump |= chunk.code.items[offset + 2];
    std.debug.print("{s: <20} {d:4} -> {d}\n", .{ name, offset, @intCast(isize, offset) + 3 + sign * jump });
    return offset + 3;
}

fn numInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const index = vle.get(chunk.code.items.ptr + offset + 1);
    std.debug.print("{s: <20} {d:4}\n", .{ name, index });
    return offset + vle.valueLength(chunk.code.items.ptr + offset + 1) + 1;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
