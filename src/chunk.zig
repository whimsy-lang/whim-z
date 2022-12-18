const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("value.zig").Value;
const vle = @import("vle.zig");
const Vm = @import("vm.zig").Vm;

pub const OpCode = enum(u8) {
    constant,

    nil,
    true,
    false,

    num_n1,
    num_0,
    num_1,
    num_2,
    num_3,
    num_4,
    num_5,
    num_6,
    num_7,
    num_8,
    num_9,
    num_10,

    dup,
    pop,

    define_global_const,
    define_global_var,
    get_global,
    set_global,

    get_local,
    set_local,

    get_upvalue,
    set_upvalue,

    define_const,
    define_const_pop,
    define_var,
    define_var_pop,
    get,
    get_pop,
    set,

    get_by_const,
    get_by_const_pop,
    set_by_const,

    equal,
    not_equal,
    greater,
    greater_equal,
    less,
    less_equal,
    is,
    add,
    subtract,
    multiply,
    divide,
    remainder,

    negate,
    not,

    jump,
    jump_back,
    jump_if_true,
    jump_if_false,
    jump_if_false_pop,

    call,
    invoke,

    closure,
    close_upvalue,
    return_,
    class,
    iterate_check,
    iterate_next,
    list,
    map,
    map_with_const,
    map_with_var,
    range,
    range_inclusive,
    range_step,
    range_inclusive_step,
    new_set,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    // variable-length encoding of the difference from the previous line number
    lines: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    current_line: u29,

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(u8).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
            .current_line = 0,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: u29) void {
        self.code.append(byte) catch {
            std.debug.print("Could not add byte to chunk.", .{});
            std.process.exit(1);
        };
        vle.add(&self.lines, line - self.current_line) catch {
            std.debug.print("Could not add line to chunk.", .{});
            std.process.exit(1);
        };
        self.current_line = line;
    }

    pub fn writeVle(self: *Chunk, val: u29, line: u29) void {
        var i = self.code.items.len;
        vle.add(&self.code, val) catch {
            std.debug.print("Could not add value to chunk.", .{});
            std.process.exit(1);
        };
        while (i < self.code.items.len) : (i += 1) {
            vle.add(&self.lines, line - self.current_line) catch {
                std.debug.print("Could not add line to chunk.", .{});
                std.process.exit(1);
            };
            self.current_line = line;
        }
    }

    pub fn getLine(self: *Chunk, index: usize) u29 {
        return vle.sumTo(self.lines.items, index);
    }

    pub fn getAddConstant(self: *Chunk, vm: *Vm, val: Value) usize {
        for (self.constants.items) |v, i| {
            if (val == v) return i;
        }

        vm.push(val);
        self.constants.append(val) catch {
            std.debug.print("Could not add constant to chunk.", .{});
            std.process.exit(1);
        };
        _ = vm.pop();

        return self.constants.items.len - 1;
    }
};
