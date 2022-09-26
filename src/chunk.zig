const std = @import("std");
const Allocator = std.mem.Allocator;

const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    constant,
    nil,
    true,
    false,
    pop,
    define_global_const,
    define_global_var,
    get_global,
    set_global,
    add_set_global,
    subtract_set_global,
    multiply_set_global,
    divide_set_global,
    modulus_set_global,
    get_local,
    set_local,
    add_set_local,
    subtract_set_local,
    multiply_set_local,
    divide_set_local,
    modulus_set_local,
    get_upvalue,
    set_upvalue,
    add_set_upvalue,
    subtract_set_upvalue,
    multiply_set_upvalue,
    divide_set_upvalue,
    modulus_set_upvalue,
    define_property_const,
    define_property_const_pop,
    define_property_var,
    define_property_var_pop,
    get_property,
    set_property,
    add_set_property,
    subtract_set_property,
    multiply_set_property,
    divide_set_property,
    modulus_set_property,
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
    modulus,
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
    anon_class,
};

pub const Chunk = struct {
    const Self = @This();

    code: std.ArrayList(u8),
    lines: std.ArrayList(usize),
    constants: std.ArrayList(Value),

    pub fn init(allocator: Allocator) Self {
        return .{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(usize).init(allocator),
            .constants = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) void {
        self.code.append(byte) catch {
            std.debug.print("Could not add byte to chunk.", .{});
            std.process.exit(1);
        };
        self.lines.append(line) catch {
            std.debug.print("Could not add line to chunk.", .{});
            std.process.exit(1);
        };
    }

    pub fn addConstant(self: *Self, value: Value) usize {
        self.constants.append(value) catch {
            std.debug.print("Could not add constant to chunk.", .{});
            std.process.exit(1);
        };
        return self.constants.items.len - 1;
    }
};