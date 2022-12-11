const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

pub const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
};

const Precedence = enum {
    none,
    or_, // or
    and_, // and
    equality, // == !=
    comparison, // < > <= >= is
    range, // .. ..=
    term, // + -
    factor, // * / %
    unary, // ! -
    call, // . : () []
    primary,
};

const PrimaryParseFn = *const fn (*Vm) bool;
const ParseFn = *const fn (*Vm) void;

pub const Compiler = struct {
    const u8_count = std.math.maxInt(u8) + 1;
    const max_loop = 32;

    const Local = struct {
        name: Token,
        constant: bool,
        depth: isize,
        is_captured: bool,
    };

    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    const Loop = struct {
        start: usize,
        exit: isize,
        depth: isize,
        iterator: bool,
    };

    const FunctionType = enum {
        function,
        initializer,
        script,
    };

    enclosing: ?*Compiler,
    function: ?*ObjFunction,
    fn_type: FunctionType,

    locals: [u8_count]Local,
    local_count: usize,
    upvalues: [u8_count]Upvalue,
    loops: [max_loop]Loop,
    loop_count: usize,
    scope_depth: isize,

    encountered_identifier: ?[]const u8,
    is_method: bool,

    pub fn init(self: *Compiler, vm: *Vm, fn_type: FunctionType) void {
        self.enclosing = vm.compiler;
        self.function = null;
        self.fn_type = fn_type;

        self.local_count = 0;
        self.loop_count = 0;
        self.scope_depth = 0;
        self.function = ObjFunction.init(vm);

        vm.compiler = self;
    }

    pub fn markRoots(vm: *Vm) void {
        var compiler = vm.compiler;
        while (compiler) |comp| {
            if (comp.function) |func| {
                Value.function(func).mark(vm);
            }
            compiler = comp.enclosing;
        }
    }

    fn errorAt(vm: *Vm, token: *Token, message: []const u8) void {
        if (vm.parser.panic_mode) return;
        vm.parser.panic_mode = true;

        std.debug.print("[line {d}] Error", .{token.line});

        if (token.type == .eof) {
            std.debug.print(" at end", .{});
        } else if (token.type != .error_) {
            std.debug.print(" at '{s}'", .{token.value});
        }

        std.debug.print(": {s}\n", .{message});
        vm.parser.had_error = true;
    }

    fn error_(vm: *Vm, message: []const u8) void {
        errorAt(vm, &vm.parser.previous, message);
    }

    fn errorAtCurrent(vm: *Vm, message: []const u8) void {
        errorAt(vm, &vm.parser.current, message);
    }

    fn advance(vm: *Vm) void {
        vm.parser.previous = vm.parser.current;

        while (true) {
            vm.parser.current = vm.lexer.lexToken();
            if (vm.parser.current.type != .error_) break;

            errorAtCurrent(vm, vm.parser.current.value);
        }
    }

    fn consume(vm: *Vm, expected: TokenType, message: []const u8) void {
        if (vm.parser.current.type == expected) {
            advance(vm);
            return;
        }

        errorAtCurrent(vm, message);
    }

    fn isDottedIdentifier(tok_type: TokenType) bool {
        return switch (tok_type) {
            .identifier, .string => true,
            .and_, .break_, .by, .class, .continue_, .do, .elif, .else_, .false, .fn_, .for_, .if_, .in, .is, .loop, .nil, .or_, .return_, .then, .true => true,

            else => false,
        };
    }

    fn consumeDottedIdentifier(vm: *Vm, message: []const u8) void {
        if (isDottedIdentifier(vm.parser.current.type)) {
            advance(vm);
            return;
        }

        errorAtCurrent(vm, message);
    }

    fn check(vm: *Vm, expected: TokenType) bool {
        return vm.parser.current.type == expected;
    }

    fn match(vm: *Vm, expected: TokenType) bool {
        if (!check(vm, expected)) return false;
        advance(vm);
        return true;
    }

    fn emitJump(vm: *Vm, instruction: OpCode) usize {
        vm.emitOp(instruction);
        vm.emitByte(0xff);
        vm.emitByte(0xff);
        return vm.currentChunk().code.items.len - 2;
    }

    fn patchJump(vm: *Vm, offset: usize) void {
        // -2 to adjust for the bytecode for the jump offset itself
        const jump = vm.currentChunk().code.items.len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            error_(vm, "Too much code to jump over.");
        }

        vm.currentChunk().code.items[offset] = @intCast(u8, (jump >> 8) & 0xff);
        vm.currentChunk().code.items[offset + 1] = @intCast(u8, jump & 0xff);
    }

    fn emitSet(vm: *Vm, op: OpCode, index: u8) void {
        if (op == .set_upvalue) {
            const max = @enumToInt(OpCode.set_upvalue_3) - @enumToInt(OpCode.set_upvalue_0);
            if (index <= max) {
                vm.emitByte(@enumToInt(OpCode.set_upvalue_0) + index);
                return;
            }
        }
        vm.emitOpByte(op, index);
    }

    fn emitLoop(vm: *Vm, op: OpCode, loop_start: usize) void {
        vm.emitOp(op);

        const offset = vm.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) error_(vm, "Loop body too large.");

        vm.emitByte(@intCast(u8, (offset >> 8) & 0xff));
        vm.emitByte(@intCast(u8, offset & 0xff));
    }

    fn emitReturn(vm: *Vm) void {
        if (vm.compiler.?.fn_type == .initializer) {
            vm.emitOpByte(.get_local, 0);
        } else {
            vm.emitOp(.nil);
        }
        vm.emitOp(.return_);
    }

    fn makeConstant(vm: *Vm, value: Value) u8 {
        const constant = vm.currentChunk().getAddConstant(vm, value);
        if (constant > std.math.maxInt(u8)) {
            error_(vm, "Too many constants in one chunk.");
            return 0;
        }
        return @intCast(u8, constant);
    }

    fn emitConstant(vm: *Vm, value: Value) void {
        vm.emitOpByte(.constant, makeConstant(vm, value));
    }

    fn emitNumber(vm: *Vm, value: f64) void {
        if (value == -1) {
            vm.emitOp(.num_n1);
        } else if (value == 0) {
            vm.emitOp(.num_0);
        } else if (value == 1) {
            vm.emitOp(.num_1);
        } else if (value == 2) {
            vm.emitOp(.num_2);
        } else if (value == 3) {
            vm.emitOp(.num_3);
        } else if (value == 4) {
            vm.emitOp(.num_4);
        } else if (value == 5) {
            vm.emitOp(.num_5);
        } else if (value == 6) {
            vm.emitOp(.num_6);
        } else if (value == 7) {
            vm.emitOp(.num_7);
        } else if (value == 8) {
            vm.emitOp(.num_8);
        } else if (value == 9) {
            vm.emitOp(.num_9);
        } else if (value == 10) {
            vm.emitOp(.num_10);
        } else {
            emitConstant(vm, Value.number(value));
        }
    }

    fn parseNum(str: []const u8) f64 {
        var index: usize = 0;
        var base: f64 = 10;
        var result: f64 = 0;
        if (str.len >= 2 and str[0] == '0') {
            switch (str[1]) {
                'b', 'B' => {
                    index = 2;
                    base = 2;
                },
                'o', 'O' => {
                    index = 2;
                    base = 8;
                },
                'x', 'X' => {
                    index = 2;
                    base = 16;
                },
                else => {},
            }
        }

        var place: f64 = 0; // place after the decimal
        var fractional_div = base;
        while (index < str.len) : (index += 1) {
            const c = str[index];
            switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    const val = @intToFloat(f64, switch (c) {
                        '0'...'9' => c - '0',
                        'a'...'f' => c - 'a' + 10,
                        'A'...'F' => c - 'A' + 10,
                        else => unreachable,
                    });
                    if (place == 0) {
                        result *= base;
                        result += val;
                    } else {
                        result += val / fractional_div;
                        place += 1;
                        fractional_div *= base;
                    }
                },
                '.' => place = 1,
                else => {},
            }
        }
        return result;
    }

    fn endCompiler(vm: *Vm) *ObjFunction {
        emitReturn(vm);
        const func = vm.compiler.?.function.?;

        if (debug.print_code and !vm.parser.had_error) {
            debug.disassembleChunk(vm.currentChunk(), if (func.name != null) func.name.?.chars else "<script>");
        }

        vm.compiler = vm.compiler.?.enclosing;
        return func;
    }

    fn beginScope(vm: *Vm) void {
        vm.compiler.?.scope_depth += 1;
    }

    fn endScope(vm: *Vm) void {
        var comp = vm.compiler.?;
        comp.scope_depth -= 1;

        while (comp.local_count > 0 and comp.locals[comp.local_count - 1].depth > comp.scope_depth) {
            if (comp.locals[comp.local_count - 1].is_captured) {
                vm.emitOp(.close_upvalue);
            } else {
                vm.emitOp(.pop);
            }
            comp.local_count -= 1;
        }
    }

    fn scopePop(vm: *Vm, depth: isize) void {
        const comp = vm.compiler.?;
        var i = @intCast(isize, comp.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            if (comp.locals[@intCast(usize, i)].depth < depth) return;
            if (comp.locals[@intCast(usize, i)].is_captured) {
                vm.emitOp(.close_upvalue);
            } else {
                vm.emitOp(.pop);
            }
        }
    }

    fn identifierConstant(vm: *Vm, name: *Token) u8 {
        return makeConstant(vm, Value.string(ObjString.copy(vm, name.value)));
    }

    fn addLocal(vm: *Vm, identifier: Token, constant: bool) void {
        var comp = vm.compiler.?;
        if (comp.local_count == u8_count) {
            error_(vm, "Too many local variables in block.");
            return;
        }

        var local = &comp.locals[comp.local_count];
        comp.local_count += 1;
        local.name = identifier;
        local.constant = constant;
        local.depth = -1;
        local.is_captured = false;
    }

    fn markInitialized(vm: *Vm) void {
        var comp = vm.compiler.?;
        comp.locals[comp.local_count - 1].depth = comp.scope_depth;
    }

    fn declareLocal(vm: *Vm, identifier: *Token, constant: bool) void {
        if (vm.compiler.?.resolveLocal(identifier) != -1) {
            error_(vm, "A variable with this name already exists.");
        }
        if (vm.compiler.?.resolveUpvalue(vm, identifier) != -1) {
            error_(vm, "A variable with this name already exists.");
        }

        addLocal(vm, identifier.*, constant);
    }

    fn placeholderLocal(vm: *Vm) void {
        var comp = vm.compiler.?;
        if (comp.local_count == u8_count) {
            error_(vm, "Too many local variables in block.");
            return;
        }

        var local = &comp.locals[comp.local_count];
        comp.local_count += 1;
        local.name = Token{ .type = .identifier, .value = "", .line = 0 };
        local.constant = false;
        local.depth = comp.scope_depth;
        local.is_captured = false;
    }

    fn resolveLocal(self: *Compiler, identifier: *Token) isize {
        var i = @intCast(isize, self.local_count) - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.locals[@intCast(usize, i)];
            if (std.mem.eql(u8, identifier.value, local.name.value)) {
                return if (local.depth == -1) -1 else i;
            }
        }
        return -1;
    }

    fn addUpvalue(self: *Compiler, vm: *Vm, index: u8, is_local: bool) usize {
        const upvalue_count = self.function.?.upvalue_count;

        var i: usize = 0;
        while (i < upvalue_count) : (i += 1) {
            const upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count == u8_count) {
            error_(vm, "Too many closure variables in function.");
            return 0;
        }

        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        self.function.?.upvalue_count += 1;
        return upvalue_count;
    }

    fn resolveUpvalue(self: *Compiler, vm: *Vm, identifier: *Token) isize {
        if (self.enclosing == null) return -1;

        const local = self.enclosing.?.resolveLocal(identifier);
        if (local != -1) {
            self.enclosing.?.locals[@intCast(usize, local)].is_captured = true;
            return @intCast(isize, self.addUpvalue(vm, @intCast(u8, local), true));
        }

        const upvalue = self.enclosing.?.resolveUpvalue(vm, identifier);
        if (upvalue != -1) {
            return @intCast(isize, self.addUpvalue(vm, @intCast(u8, upvalue), false));
        }

        return -1;
    }

    fn getUpvalueLocal(vm: *Vm, upvalue_index: usize) *Local {
        var comp = vm.compiler.?;
        var index = upvalue_index;
        while (!comp.upvalues[index].is_local) {
            index = comp.upvalues[index].index;
            comp = comp.enclosing.?;
        }
        return &comp.enclosing.?.locals[comp.upvalues[index].index];
    }

    fn defineGlobal(vm: *Vm, global: u8, constant: bool) void {
        vm.emitOpByte(if (constant) .define_global_const else .define_global_var, global);
    }

    fn defineProperty(vm: *Vm, name: u8, constant: bool, pop: bool) void {
        if (pop) {
            vm.emitOpByte(if (constant) .define_const_by_const_pop else .define_var_by_const_pop, name);
        } else {
            vm.emitOpByte(if (constant) .define_const_by_const else .define_var_by_const, name);
        }
    }

    fn defineIndexer(vm: *Vm, constant: bool, pop: bool) void {
        if (pop) {
            vm.emitOp(if (constant) .define_const_pop else .define_var_pop);
        } else {
            vm.emitOp(if (constant) .define_const else .define_var);
        }
    }

    fn getPrefixPrimary(tok_type: TokenType) ?PrimaryParseFn {
        return switch (tok_type) {
            .left_paren => groupingPrimary,
            .identifier => variablePrimary,
            else => null,
        };
    }

    fn getInfixPrimary(tok_type: TokenType) ?PrimaryParseFn {
        return switch (tok_type) {
            .left_paren => callPrimary,
            .dot => dotPrimary,
            .colon => methodPrimary,
            .left_bracket => indexerPrimary,
            else => null,
        };
    }

    fn getPrefix(tok_type: TokenType) ?ParseFn {
        return switch (tok_type) {
            .left_paren => grouping,
            .left_bracket => bracketExpression,
            .bang => not,
            .minus => negate,
            .identifier => variable,
            .string => string,
            .number => number,
            .class => class,
            .false, .nil, .true => literal,
            .fn_ => function,
            .if_ => ifExpression,
            else => null,
        };
    }

    fn getInfix(tok_type: TokenType) ?ParseFn {
        return switch (tok_type) {
            .left_paren => call,
            .dot => dot,
            .colon => method,
            .left_bracket => indexer,
            .bang_equal, .equal_equal => binary,
            .less, .less_equal, .greater, .greater_equal, .is => binary,
            .dot_dot, .dot_dot_equal => range,
            .plus, .minus, .star, .slash, .percent => binary,
            .and_ => andOp,
            .or_ => orOp,
            else => null,
        };
    }

    fn getPrecedence(tok_type: TokenType) Precedence {
        return switch (tok_type) {
            .left_paren, .dot, .colon, .left_bracket => .call,
            .bang_equal, .equal_equal => .equality,
            .less, .less_equal, .greater, .greater_equal, .is => .comparison,
            .dot_dot, .dot_dot_equal => .range,
            .plus, .minus => .term,
            .star, .slash, .percent => .factor,
            .and_ => .and_,
            .or_ => .or_,
            else => .none,
        };
    }

    fn argumentList(vm: *Vm) u8 {
        var arg_count: u8 = 0;
        if (!check(vm, .right_paren)) {
            while (true) {
                expression(vm);
                if (arg_count == std.math.maxInt(u8)) {
                    error_(vm, "Can't have more than 255 arguments.");
                }
                arg_count += 1;
                if (!(match(vm, .comma) and !check(vm, .right_paren))) break;
            }
        }
        consume(vm, .right_paren, "Expect ')' after arguments.");
        return arg_count;
    }

    fn andOp(vm: *Vm) void {
        const end_jump = emitJump(vm, .jump_if_false);

        vm.emitOp(.pop);
        parsePrecedence(vm, .and_);

        patchJump(vm, end_jump);
    }

    fn binary(vm: *Vm) void {
        const op_type = vm.parser.previous.type;
        const precedence = getPrecedence(op_type);
        parsePrecedence(vm, @intToEnum(Precedence, @enumToInt(precedence) + 1));

        switch (op_type) {
            .bang_equal => vm.emitOp(.not_equal),
            .equal_equal => vm.emitOp(.equal),
            .greater => vm.emitOp(.greater),
            .greater_equal => vm.emitOp(.greater_equal),
            .less => vm.emitOp(.less),
            .less_equal => vm.emitOp(.less_equal),
            .is => vm.emitOp(.is),
            .plus => vm.emitOp(.add),
            .minus => vm.emitOp(.subtract),
            .star => vm.emitOp(.multiply),
            .slash => vm.emitOp(.divide),
            .percent => vm.emitOp(.remainder),
            else => unreachable,
        }
    }

    fn bracketExpression(vm: *Vm) void {
        var count: usize = 0;
        var map = false;
        if (!check(vm, .right_bracket)) {
            while (true) {
                expression(vm);
                if (check(vm, .colon_colon) or check(vm, .colon_equal)) {
                    const constant = vm.parser.current.type == .colon_colon;
                    advance(vm); // accept :: :=
                    expression(vm);

                    if (count == 0) {
                        // first map item
                        map = true;
                        vm.emitOp(if (constant) .map_with_const else .map_with_var);
                    } else if (map) {
                        // map item
                        defineIndexer(vm, constant, false);
                    } else {
                        error_(vm, "Sets cannot contain key/value pairs.");
                    }
                } else if (map) {
                    error_(vm, "All map entries must be a key/value pair.");
                } else if (count == std.math.maxInt(u8)) {
                    // set item, check count
                    error_(vm, "Can't have more than 255 starting set items.");
                }

                count += 1;
                if (!(match(vm, .comma) and !check(vm, .right_bracket))) break;
            }
        }
        consume(vm, .right_bracket, "Expect ']' after expression.");

        if (count == 0) {
            vm.emitOp(.map);
        } else if (!map) {
            vm.emitOpByte(.new_set, @intCast(u8, count));
        }
    }

    fn callPrimary(vm: *Vm) bool {
        call(vm);
        return false;
    }

    fn call(vm: *Vm) void {
        const max = @enumToInt(OpCode.call_16) - @enumToInt(OpCode.call_0);
        const arg_count = argumentList(vm);
        if (arg_count <= max) {
            vm.emitByte(@enumToInt(OpCode.call_0) + arg_count);
        } else {
            vm.emitOpByte(.call, arg_count);
        }
    }

    fn classField(vm: *Vm) void {
        consumeDottedIdentifier(vm, "Expect field name.");
        const name = identifierConstant(vm, &vm.parser.previous);

        switch (vm.parser.current.type) {
            .colon_colon, .colon_equal => {
                const constant = vm.parser.current.type == .colon_colon;

                vm.compiler.?.encountered_identifier = vm.parser.previous.value;
                vm.compiler.?.is_method = true;

                advance(vm); // accept :: :=
                expression(vm);
                defineProperty(vm, name, constant, false);
            },
            else => error_(vm, "Expect '::' or ':=' declaration."),
        }
    }

    fn class(vm: *Vm) void {
        vm.emitOp(.class);
        if (vm.compiler.?.encountered_identifier) |name| {
            const name_const = makeConstant(vm, Value.string(ObjString.copy(vm, name)));
            vm.emitByte(name_const);
        } else {
            const name_const = makeConstant(vm, Value.string(vm.empty_string.?));
            vm.emitByte(name_const);
        }

        if (match(vm, .is)) {
            expression(vm);
            vm.emitOp(.define_super);
        }

        while (!check(vm, .class_end) and !check(vm, .eof)) {
            classField(vm);
        }

        consume(vm, .class_end, "Expect '/class' after block.");
    }

    fn dotHelper(vm: *Vm, name: u8) void {
        if (match(vm, .left_paren)) {
            const max = @enumToInt(OpCode.invoke_16) - @enumToInt(OpCode.invoke_0);
            const arg_count = argumentList(vm);
            if (arg_count <= max) {
                vm.emitByte(@enumToInt(OpCode.invoke_0) + arg_count);
                vm.emitByte(name);
            } else {
                vm.emitOpByte(.invoke, name);
                vm.emitByte(arg_count);
            }
        } else {
            vm.emitOpByte(.get_by_const_pop, name);
        }
    }

    fn dotPrimary(vm: *Vm) bool {
        consumeDottedIdentifier(vm, "Expect property name after '.'.");
        const name = identifierConstant(vm, &vm.parser.previous);

        const op_type = vm.parser.current.type;
        switch (op_type) {
            .colon_colon, .colon_equal => {
                // declaration
                const constant = op_type == .colon_colon;

                vm.compiler.?.encountered_identifier = vm.parser.previous.value;
                vm.compiler.?.is_method = false;

                advance(vm); // accept :: :=
                expression(vm);
                defineProperty(vm, name, constant, true);

                return true;
            },
            .equal, .plus_equal, .minus_equal, .star_equal, .slash_equal, .percent_equal => {
                // assignment
                vm.compiler.?.encountered_identifier = vm.parser.previous.value;
                vm.compiler.?.is_method = false;

                if (op_type != .equal) {
                    // emit get
                    vm.emitOpByte(.get_by_const, name);
                }

                advance(vm); // accept = += -= *= /= %=
                expression(vm);

                // emit op
                switch (op_type) {
                    .plus_equal => vm.emitOp(.add),
                    .minus_equal => vm.emitOp(.subtract),
                    .star_equal => vm.emitOp(.multiply),
                    .slash_equal => vm.emitOp(.divide),
                    .percent_equal => vm.emitOp(.remainder),
                    else => {},
                }

                // emit set
                vm.emitOpByte(.set_by_const, name);

                return true;
            },
            else => {},
        }
        // not an assignment, so get the property
        dotHelper(vm, name);
        return false;
    }

    fn dot(vm: *Vm) void {
        consumeDottedIdentifier(vm, "Expect property name after '.'.");
        const name = identifierConstant(vm, &vm.parser.previous);
        dotHelper(vm, name);
    }

    fn function(vm: *Vm) void {
        const is_method = vm.compiler.?.is_method;
        var compiler: Compiler = undefined;
        compiler.init(vm, .function);
        if (vm.compiler.?.enclosing.?.encountered_identifier) |name| {
            compiler.function.?.name = ObjString.copy(vm, name);

            if (is_method and compiler.function.?.name == vm.init_string) {
                compiler.fn_type = .initializer;
            }
        }
        beginScope(vm);

        consume(vm, .left_paren, "Expect '(' after fn.");
        if (!check(vm, .right_paren)) {
            while (true) {
                vm.compiler.?.function.?.arity += 1;
                if (vm.compiler.?.function.?.arity > std.math.maxInt(u8)) {
                    errorAtCurrent(vm, "Can't have more than 255 parameters.");
                }
                consume(vm, .identifier, "Expect parameter name.");
                declareLocal(vm, &vm.parser.previous, true);
                markInitialized(vm);
                if (!(match(vm, .comma) and !check(vm, .right_paren))) break;
            }
        }
        consume(vm, .right_paren, "Expect ')' after parameters.");
        block(vm, .fn_end, "Expect '/fn' after block.");

        const new_func = endCompiler(vm);
        vm.emitOpByte(.closure, makeConstant(vm, Value.function(new_func)));

        var i: usize = 0;
        while (i < new_func.upvalue_count) : (i += 1) {
            vm.emitByte(if (compiler.upvalues[i].is_local) 1 else 0);
            vm.emitByte(compiler.upvalues[i].index);
        }
    }

    fn groupingPrimary(vm: *Vm) bool {
        grouping(vm);
        return false;
    }

    fn grouping(vm: *Vm) void {
        var count: u8 = 0;
        var list = false;
        if (!check(vm, .right_paren)) {
            while (true) {
                expression(vm);
                if (count == std.math.maxInt(u8)) {
                    error_(vm, "Can't have more than 255 starting list items.");
                }
                count += 1;
                if (check(vm, .comma)) list = true;
                if (!(match(vm, .comma) and !check(vm, .right_paren))) break;
            }
        }
        consume(vm, .right_paren, "Expect ')' after expression.");

        if (count == 0 or list) {
            vm.emitOpByte(.list, count);
        }
    }

    fn ifExpression(vm: *Vm) void {
        expression(vm);

        // optional then
        _ = match(vm, .then);

        var then_jump = emitJump(vm, .jump_if_false_pop);

        expression(vm);

        var else_jump = emitJump(vm, .jump);
        patchJump(vm, then_jump);

        while (match(vm, .elif)) {
            expression(vm);

            // optional then
            _ = match(vm, .then);

            then_jump = emitJump(vm, .jump_if_false_pop);

            expression(vm);

            patchJump(vm, else_jump);
            else_jump = emitJump(vm, .jump);
            patchJump(vm, then_jump);
        }

        consume(vm, .else_, "Expect 'else' in if expression.");
        expression(vm);

        patchJump(vm, else_jump);
    }

    fn indexerPrimary(vm: *Vm) bool {
        expression(vm);
        consume(vm, .right_bracket, "Expect ']' after expression.");

        const op_type = vm.parser.current.type;
        switch (op_type) {
            .colon_colon, .colon_equal => {
                // declaration
                const constant = op_type == .colon_colon;

                advance(vm); // accept :: :=
                expression(vm);
                defineIndexer(vm, constant, true);

                return true;
            },
            .equal, .plus_equal, .minus_equal, .star_equal, .slash_equal, .percent_equal => {
                // assignment

                if (op_type != .equal) {
                    // emit get
                    vm.emitOp(.get);
                }

                advance(vm); // accept = += -= *= /= %=
                expression(vm);

                // emit op
                switch (op_type) {
                    .plus_equal => vm.emitOp(.add),
                    .minus_equal => vm.emitOp(.subtract),
                    .star_equal => vm.emitOp(.multiply),
                    .slash_equal => vm.emitOp(.divide),
                    .percent_equal => vm.emitOp(.remainder),
                    else => {},
                }

                // emit set
                vm.emitOp(.set);

                return true;
            },
            else => {},
        }
        // not an assignment, so get the indexer
        vm.emitOp(.get_pop);
        return false;
    }

    fn indexer(vm: *Vm) void {
        expression(vm);
        consume(vm, .right_bracket, "Expect ']' after expression.");
        vm.emitOp(.get_pop);
    }

    fn literal(vm: *Vm) void {
        switch (vm.parser.previous.type) {
            .false => vm.emitOp(.false),
            .nil => vm.emitOp(.nil),
            .true => vm.emitOp(.true),
            else => unreachable,
        }
    }

    fn methodPrimary(vm: *Vm) bool {
        method(vm);
        return false;
    }

    fn method(vm: *Vm) void {
        consumeDottedIdentifier(vm, "Expect method name after ':'.");
        const name = identifierConstant(vm, &vm.parser.previous);
        consume(vm, .left_paren, "Expect '(' after method name.");
        vm.emitOp(.dup);
        const max = @enumToInt(OpCode.invoke_16) - @enumToInt(OpCode.invoke_0);
        const arg_count = argumentList(vm) + 1;
        if (arg_count <= max) {
            vm.emitByte(@enumToInt(OpCode.invoke_0) + arg_count);
            vm.emitByte(name);
        } else {
            vm.emitOpByte(.invoke, name);
            vm.emitByte(arg_count);
        }
    }

    fn negate(vm: *Vm) void {
        if (match(vm, .number)) {
            const value = parseNum(vm.parser.previous.value);
            emitNumber(vm, -value);
        } else {
            // compile the operand
            parsePrecedence(vm, .unary);
            vm.emitOp(.negate);
        }
    }

    fn not(vm: *Vm) void {
        // compile the operand
        parsePrecedence(vm, .unary);
        vm.emitOp(.not);
    }

    fn number(vm: *Vm) void {
        const value = parseNum(vm.parser.previous.value);
        emitNumber(vm, value);
    }

    fn orOp(vm: *Vm) void {
        const end_jump = emitJump(vm, .jump_if_true);

        vm.emitOp(.pop);
        parsePrecedence(vm, .or_);

        patchJump(vm, end_jump);
    }

    fn range(vm: *Vm) void {
        const op_type = vm.parser.previous.type;
        const precedence = getPrecedence(op_type);
        parsePrecedence(vm, @intToEnum(Precedence, @enumToInt(precedence) + 1));

        if (match(vm, .by)) {
            expression(vm);

            switch (op_type) {
                .dot_dot => vm.emitOp(.range_step),
                .dot_dot_equal => vm.emitOp(.range_inclusive_step),
                else => unreachable,
            }
        } else {
            switch (op_type) {
                .dot_dot => vm.emitOp(.range),
                .dot_dot_equal => vm.emitOp(.range_inclusive),
                else => unreachable,
            }
        }
    }

    fn string(vm: *Vm) void {
        emitConstant(vm, Value.string(ObjString.copyEscape(vm, vm.parser.previous.value)));
    }

    fn variablePrimary(vm: *Vm) bool {
        const op_type = vm.parser.current.type;
        switch (op_type) {
            .colon_colon, .colon_equal => {
                // declaration

                const constant = op_type == .colon_colon;

                vm.compiler.?.encountered_identifier = vm.parser.previous.value;
                vm.compiler.?.is_method = false;

                if (vm.compiler.?.scope_depth > 0) {
                    // local
                    declareLocal(vm, &vm.parser.previous, constant);
                    advance(vm); // accept :: :=

                    // mark initialized if it's a function or class so it can reference itself
                    if (vm.parser.current.type == .fn_ or vm.parser.current.type == .class) {
                        markInitialized(vm);
                    }

                    expression(vm);
                    markInitialized(vm);
                } else {
                    // global
                    const arg = identifierConstant(vm, &vm.parser.previous);
                    advance(vm); // accept :: :=
                    expression(vm);
                    defineGlobal(vm, arg, constant);
                }

                return true;
            },
            .equal, .plus_equal, .minus_equal, .star_equal, .slash_equal, .percent_equal => {
                // assignment

                vm.compiler.?.encountered_identifier = vm.parser.previous.value;
                vm.compiler.?.is_method = false;

                var arg = vm.compiler.?.resolveLocal(&vm.parser.previous);
                var get_op = OpCode.get_local;
                var set_op = OpCode.set_local;

                if (arg != -1) {
                    // local
                    if (vm.compiler.?.locals[@intCast(usize, arg)].constant) {
                        error_(vm, "Local is constant.");
                    }
                } else {
                    arg = vm.compiler.?.resolveUpvalue(vm, &vm.parser.previous);
                    if (arg != -1) {
                        // upvalue
                        const local = getUpvalueLocal(vm, @intCast(usize, arg));
                        if (local.constant) {
                            error_(vm, "Local is constant.");
                        }
                        get_op = .get_upvalue;
                        set_op = .set_upvalue;
                    } else {
                        // global
                        arg = identifierConstant(vm, &vm.parser.previous);
                        get_op = .get_global;
                        set_op = .set_global;
                    }
                }

                if (op_type != .equal) {
                    vm.emitOpByte(get_op, @intCast(u8, arg));
                }

                advance(vm); // accept = += -= *= /= %=
                expression(vm);

                // emit op
                switch (op_type) {
                    .plus_equal => vm.emitOp(.add),
                    .minus_equal => vm.emitOp(.subtract),
                    .star_equal => vm.emitOp(.multiply),
                    .slash_equal => vm.emitOp(.divide),
                    .percent_equal => vm.emitOp(.remainder),
                    else => {},
                }

                emitSet(vm, set_op, @intCast(u8, arg));

                return true;
            },
            else => {},
        }
        // not an assignment, continue parsing the expression
        variable(vm);
        return false;
    }

    fn variable(vm: *Vm) void {
        var name = vm.parser.previous;
        var arg = vm.compiler.?.resolveLocal(&name);
        var op = OpCode.get_local;
        if (arg == -1) {
            arg = vm.compiler.?.resolveUpvalue(vm, &name);
            op = .get_upvalue;
            if (arg == -1) {
                arg = identifierConstant(vm, &name);
                op = .get_global;
            }
        }
        vm.emitOpByte(op, @intCast(u8, arg));
    }

    fn parseInfixPrecedence(vm: *Vm, precedence: Precedence) void {
        while (@enumToInt(precedence) <= @enumToInt(getPrecedence(vm.parser.current.type))) {
            advance(vm);
            const infix = getInfix(vm.parser.previous.type);
            infix.?(vm);
        }
    }

    fn parsePrecedenceFromPrevious(vm: *Vm, precedence: Precedence) void {
        const prefix = getPrefix(vm.parser.previous.type);
        if (prefix == null) {
            error_(vm, "Expect expression.");
            return;
        }

        prefix.?(vm);

        parseInfixPrecedence(vm, precedence);
    }

    fn parsePrecedence(vm: *Vm, precedence: Precedence) void {
        advance(vm);
        parsePrecedenceFromPrevious(vm, precedence);
    }

    fn expression(vm: *Vm) void {
        parsePrecedence(vm, .or_);
    }

    fn expressionFromPrevious(vm: *Vm) void {
        parsePrecedenceFromPrevious(vm, .or_);
    }

    fn block(vm: *Vm, end: TokenType, message: []const u8) void {
        while (!check(vm, end) and !check(vm, .eof)) {
            statement(vm);
        }
        consume(vm, end, message);
    }

    fn breakStatement(vm: *Vm) void {
        if (vm.compiler.?.loop_count == 0) {
            error_(vm, "Cannot break, not in a loop.");
            return;
        }

        expression(vm);

        const skip_jump = emitJump(vm, .jump_if_false_pop);

        const loop = &vm.compiler.?.loops[vm.compiler.?.loop_count - 1];
        scopePop(vm, loop.depth);
        if (loop.exit != -1) patchJump(vm, @intCast(usize, loop.exit));
        loop.exit = @intCast(isize, emitJump(vm, .jump));

        patchJump(vm, skip_jump);
    }

    fn continueStatement(vm: *Vm) void {
        if (vm.compiler.?.loop_count == 0) {
            error_(vm, "Cannot continue, not in a loop.");
            return;
        }

        expression(vm);

        const skip_jump = emitJump(vm, .jump_if_false_pop);

        const loop = &vm.compiler.?.loops[vm.compiler.?.loop_count - 1];
        scopePop(vm, loop.depth);
        emitLoop(vm, if (loop.iterator) .iterate_next else .jump_back, loop.start);

        patchJump(vm, skip_jump);
    }

    fn expressionStatement(vm: *Vm) void {
        advance(vm);
        const primary_prefix = getPrefixPrimary(vm.parser.previous.type);
        if (primary_prefix == null) {
            expressionFromPrevious(vm);
            vm.emitOp(.pop);
            return;
        }

        var done = primary_prefix.?(vm);

        // parse the primary expression, checking for assignment
        while (!done and @enumToInt(Precedence.call) <= @enumToInt(getPrecedence(vm.parser.current.type))) {
            advance(vm);
            const primary_infix = getInfixPrimary(vm.parser.previous.type);
            done = primary_infix.?(vm);
        }

        // parse any other parts of the expression
        if (!done) {
            parseInfixPrecedence(vm, .or_);
            vm.emitOp(.pop);
        }
    }

    fn forStatement(vm: *Vm) void {
        if (vm.compiler.?.loop_count == max_loop) {
            error_(vm, "Too many nested loops.");
            return;
        }

        beginScope(vm);

        // stack: [object being iterated over] [index] [current value]

        consume(vm, .identifier, "Expect variable name after 'for'.");
        placeholderLocal(vm);
        placeholderLocal(vm);

        beginScope(vm);

        declareLocal(vm, &vm.parser.previous, true);
        consume(vm, .in, "Expect 'in' after variable name.");

        expression(vm);
        markInitialized(vm);

        // push starting index 0 on the stack
        emitNumber(vm, 0);

        var loop = &vm.compiler.?.loops[vm.compiler.?.loop_count];
        vm.compiler.?.loop_count += 1;
        loop.start = vm.currentChunk().code.items.len;
        loop.exit = @intCast(isize, emitJump(vm, .iterate_check));
        loop.depth = vm.compiler.?.scope_depth;
        loop.iterator = true;

        while (vm.parser.current.type != .for_end and vm.parser.current.type != .eof) {
            statement(vm);
        }

        endScope(vm);

        emitLoop(vm, .iterate_next, loop.start);
        patchJump(vm, @intCast(usize, loop.exit));

        endScope(vm);

        consume(vm, .for_end, "Expect '/for' after block.");

        vm.compiler.?.loop_count -= 1;
    }

    fn ifStatement(vm: *Vm) void {
        expression(vm);

        // optional then
        _ = match(vm, .then);

        var then_jump = emitJump(vm, .jump_if_false_pop);

        beginScope(vm);
        while (vm.parser.current.type != .if_end and
            vm.parser.current.type != .elif and
            vm.parser.current.type != .else_ and
            vm.parser.current.type != .eof)
        {
            statement(vm);
        }
        endScope(vm);

        var else_jump = emitJump(vm, .jump);

        patchJump(vm, then_jump);

        while (match(vm, .elif)) {
            expression(vm);

            // optional then
            _ = match(vm, .then);

            then_jump = emitJump(vm, .jump_if_false_pop);

            beginScope(vm);
            while (vm.parser.current.type != .if_end and
                vm.parser.current.type != .elif and
                vm.parser.current.type != .else_ and
                vm.parser.current.type != .eof)
            {
                statement(vm);
            }
            endScope(vm);

            patchJump(vm, else_jump);
            else_jump = emitJump(vm, .jump);
            patchJump(vm, then_jump);
        }

        if (match(vm, .else_)) {
            beginScope(vm);
            while (vm.parser.current.type != .if_end and
                vm.parser.current.type != .eof)
            {
                statement(vm);
            }
            endScope(vm);
        }

        patchJump(vm, else_jump);

        consume(vm, .if_end, "Expect '/if' after block.");
    }

    fn loopStatement(vm: *Vm) void {
        if (vm.compiler.?.loop_count == max_loop) {
            error_(vm, "Too many nested loops.");
            return;
        }

        beginScope(vm);

        var loop = &vm.compiler.?.loops[vm.compiler.?.loop_count];
        vm.compiler.?.loop_count += 1;
        loop.start = vm.currentChunk().code.items.len;
        loop.exit = -1;
        loop.depth = vm.compiler.?.scope_depth;
        loop.iterator = false;

        while (vm.parser.current.type != .loop_end and vm.parser.current.type != .eof) {
            statement(vm);
        }

        endScope(vm);

        emitLoop(vm, .jump_back, loop.start);

        if (loop.exit != -1) patchJump(vm, @intCast(usize, loop.exit));

        consume(vm, .loop_end, "Expect '/loop' after block.");

        vm.compiler.?.loop_count -= 1;
    }

    fn returnStatement(vm: *Vm) void {
        if (vm.compiler.?.fn_type == .initializer) {
            error_(vm, "Can't return a value from an initializer.");
        }
        expression(vm);
        vm.emitOp(.return_);
    }

    fn statement(vm: *Vm) void {
        vm.compiler.?.encountered_identifier = null;

        if (match(vm, .semicolon)) {
            // empty statement
        } else if (match(vm, .break_)) {
            breakStatement(vm);
        } else if (match(vm, .continue_)) {
            continueStatement(vm);
        } else if (match(vm, .do)) {
            beginScope(vm);
            block(vm, .do_end, "Expect '/do' after block.");
            endScope(vm);
        } else if (match(vm, .for_)) {
            forStatement(vm);
        } else if (match(vm, .if_)) {
            ifStatement(vm);
        } else if (match(vm, .loop)) {
            loopStatement(vm);
        } else if (match(vm, .return_)) {
            returnStatement(vm);
        } else {
            expressionStatement(vm);
        }

        if (vm.parser.panic_mode) synchronize(vm);
    }

    fn synchronize(vm: *Vm) void {
        vm.parser.panic_mode = false;

        while (vm.parser.current.type != .eof) {
            switch (vm.parser.previous.type) {
                .class_end, .do_end, .fn_end, .for_end, .if_end, .loop_end, .semicolon => return,
                else => {},
            }

            switch (vm.parser.current.type) {
                .break_, .class, .continue_, .do, .fn_, .for_, .if_, .loop, .return_ => return,
                else => {},
            }

            advance(vm);
        }
    }

    pub fn compile(vm: *Vm, source: [:0]const u8) ?*ObjFunction {
        vm.lexer = Lexer.init(source);

        vm.parser.had_error = false;
        vm.parser.panic_mode = false;

        var compiler: Compiler = undefined;
        compiler.init(vm, .script);

        advance(vm);

        while (!match(vm, .eof)) {
            statement(vm);
        }

        const script_func = endCompiler(vm);
        return if (vm.parser.had_error) null else script_func;
    }
};
