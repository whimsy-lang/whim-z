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
    pop,
    define_global_const,
    define_global_var,
    get_global,
    set_global,
    get_local,
    set_local,
    get_upvalue,
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
    call,
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
