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
    get_global_ptr,
    set_global,
    get_local,
    get_local_ptr,
    set_local,
    get_upvalue,
    get_upvalue_ptr,
    set_upvalue,
    define_property_const,
    define_property_const_pop,
    define_property_var,
    define_property_var_pop,
    get_property,
    get_property_ptr,
    set_property,
    add_set,
    subtract_set,
    multiply_set,
    divide_set,
    modulus_set,
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
