const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("value.zig").Value;
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

    pop,

    define_global_const,
    define_global_var,
    get_global,
    set_global,

    get_local_0,
    get_local_1,
    get_local_2,
    get_local_3,
    get_local_4,
    get_local_5,
    get_local_6,
    get_local_7,
    get_local_8,
    get_local_9,
    get_local_10,
    get_local_11,
    get_local_12,
    get_local_13,
    get_local_14,
    get_local_15,
    get_local,

    set_local_0,
    set_local_1,
    set_local_2,
    set_local_3,
    set_local_4,
    set_local_5,
    set_local_6,
    set_local_7,
    set_local_8,
    set_local_9,
    set_local_10,
    set_local_11,
    set_local_12,
    set_local_13,
    set_local_14,
    set_local_15,
    set_local,

    get_upvalue_0,
    get_upvalue_1,
    get_upvalue_2,
    get_upvalue_3,
    get_upvalue,

    set_upvalue_0,
    set_upvalue_1,
    set_upvalue_2,
    set_upvalue_3,
    set_upvalue,

    define_property_const,
    define_property_const_pop,
    define_property_var,
    define_property_var_pop,
    get_property,
    get_property_pop,
    set_property,

    equal,
    not_equal,
    greater,
    greater_equal,
    less,
    less_equal,
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
    jump_if_true_pop,
    jump_if_false_pop,

    call_0,
    call_1,
    call_2,
    call_3,
    call_4,
    call_5,
    call_6,
    call_7,
    call_8,
    call_9,
    call_10,
    call_11,
    call_12,
    call_13,
    call_14,
    call_15,
    call_16,
    call,

    invoke_0,
    invoke_1,
    invoke_2,
    invoke_3,
    invoke_4,
    invoke_5,
    invoke_6,
    invoke_7,
    invoke_8,
    invoke_9,
    invoke_10,
    invoke_11,
    invoke_12,
    invoke_13,
    invoke_14,
    invoke_15,
    invoke_16,
    invoke,

    closure,
    close_upvalue,
    return_,
    class,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    lines: std.ArrayList(usize),
    constants: std.ArrayList(Value),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, byte: u8, line: usize) void {
        self.code.append(byte) catch {
            std.debug.print("Could not add byte to chunk.", .{});
            std.process.exit(1);
        };
        self.lines.append(line) catch {
            std.debug.print("Could not add line to chunk.", .{});
            std.process.exit(1);
        };
    }

    pub fn getAddConstant(self: *Chunk, vm: *Vm, value: Value) usize {
        for (self.constants.items) |val, ind| {
            if (value.equal(val)) return ind;
        }

        vm.push(value);
        self.constants.append(value) catch {
            std.debug.print("Could not add constant to chunk.", .{});
            std.process.exit(1);
        };
        _ = vm.pop();

        return self.constants.items.len - 1;
    }
};
