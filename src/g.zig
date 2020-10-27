const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

//! g is a parser combinator library optimised for expressiveness.
//! What is the most intuitive way to describe a grammer in zig?
//!
//! A parser is a condtion that takes a sequence of utf-8 bytes as the input and
//! returns the result or may `Err`. A combinator is a condtion that takes zero or more
//! arguments and returns a parser.

pub const Err = error{ UnexpectedEof, TermRejected };

/// Parser for type T
pub fn Par(comptime T: anytype) type {
    return fn ([]const u8) Err!Res(T);
}

/// Result for type T a Parse
pub fn Res(comptime T: type) type {
    return struct {
        term: T,
        rest: []const u8,

        pub fn init(v: T, rest: []const u8) @This() {
            return .{ .term = v, .rest = rest };
        }
    };
}

/// Result Type of a Parser
pub fn ResType(comptime P: anytype) type {
    const ti = @typeInfo(@TypeOf(P));
    const ti_inner = switch (ti) {
        .Fn => ti,
        .Pointer => @typeInfo(@typeInfo(ti.Pointer.child).Array.child),
        else => return u8,
    };
    const rt = @typeInfo(ti_inner.Fn.return_type.?).ErrorUnion.payload;
    return @typeInfo(rt).Struct.fields[0].field_type;
}

/// `byte` holds combinators that work at the byte level. They just return a
/// slice of the input.
pub const byte = struct {
    pub fn One() Par(u8) {
        return struct {
            fn par(inp: []const u8) Err!Res(u8) {
                if (inp.len == 0) return error.UnexpectedEof;
                return Res(u8).init(inp[0], inp[1..]);
            }
        }.par;
    }

    test "byte.One" {
        const parse = One();

        var res = try parse("hello");
        testing.expectEqual(@as(u8, 'h'), res.term);
        testing.expectEqualSlices(u8, "ello", res.rest);
        res = try parse("é");
        testing.expectEqual(@as(u8, 0xC3), res.term);
        testing.expectEqualSlices(u8, &[_]u8{0xA9}, res.rest);
        testing.expectError(Err.UnexpectedEof, parse(""));
    }

    pub fn Alphabet() Par(u8) {
        return comptime Check(One(), check.IsAlpha());
    }

    test "byte.Alphabet" {
        const parse = Alphabet();

        var res = try parse("ABC");
        testing.expectEqual(@as(u8, 'A'), res.term);
        testing.expectEqualSlices(u8, "BC", res.rest);
        testing.expectError(Err.TermRejected, parse("1BC"));
        testing.expectError(Err.UnexpectedEof, parse(""));
    }

    pub fn Digit() Par(u8) {
        return comptime Check(One(), check.IsDigit());
    }

    test "byte.Digit" {
        const parse = Digit();

        var res = try parse("123");
        testing.expectEqual(@as(u8, '1'), res.term);
        testing.expectEqualSlices(u8, "23", res.rest);
        testing.expectError(Err.TermRejected, parse("a23"));
        testing.expectError(Err.UnexpectedEof, parse(""));
    }

    pub fn Alphanumeric() Par(u8) {
        return comptime OneOf(&[_]Par(u8){
            Alphabet(),
            Digit(),
        });
    }

    test "Alphanumeric" {
        const parse = Alphanumeric();

        var res = try parse("A2_");
        testing.expectEqual(@as(u8, 'A'), res.term);
        res = try parse(res.rest);
        testing.expectEqual(@as(u8, '2'), res.term);
        testing.expectError(Err.TermRejected, parse(res.rest));
    }

    pub fn Byte(comptime expected: u8) Par(u8) {
        return comptime Check(One(), check.IsByte(expected));
    }

    // many never fails
    pub fn Many(comptime P: Par(u8)) Par([]const u8) {
        return struct {
            fn par(inp: []const u8) Err!Res([]const u8) {
                const res = P(inp) catch
                    return Res([]const u8).init("", inp);

                const final_res = Many(P)(res.rest) catch
                    return Res([]const u8).init(&[_]u8{res.term}, res.rest);

                return Res([]const u8).init(
                    inp[0 .. inp.len - final_res.rest.len],
                    final_res.rest,
                );
            }
        }.par;
    }

    pub fn Whitespaces() Par([]const u8) {
        return comptime Many(OneOf(&[_]Par(u8){ Byte(' '), Byte('\t') }));
    }

    // The API of `Sequence` will change when an issue in the compiler is
    // closed. You can pass anon struct instead of an array
    pub fn Sequence(comptime pars: anytype) Par([]const u8) {
        return struct {
            fn par(inp: []const u8) Err!Res([]const u8) {
                if (pars.len == 0) return Res([]const u8).init("", inp);

                const res = try pars[0](inp);
                const final_res = try Sequence(pars[1..])(res.rest);
                return Res([]const u8).init(
                    inp[0 .. inp.len - final_res.rest.len],
                    final_res.rest,
                );
            }
        }.par;
    }
};

test "byte combinators" {
    _ = byte;
}

// The API of `OneOf` will change when an issue in the compiler is
// closed. You can pass anon struct instead of an array
pub fn OneOf(comptime pars: anytype) Par(ResType(pars)) {
    return struct {
        fn par(inp: []const u8) Err!Res(ResType(pars)) {
            if (pars.len == 0) return error.TermRejected;
            return pars[0](inp) catch OneOf(pars[1..])(inp);
        }
    }.par;
}

pub fn Condition(comptime T: anytype) type {
    return fn (T) bool;
}

pub fn Check(comptime P: anytype, comptime cond: anytype) Par(ResType(P)) {
    comptime {
        return struct {
            fn par(input: []const u8) Err!Res(ResType(P)) {
                const res = try P(input);
                if (!cond(res.term)) return error.TermRejected;
                return res;
            }
        }.par;
    }
}

pub const check = struct {
    pub fn IsAlpha() Condition(u8) {
        return comptime struct {
            fn cond(res: u8) bool {
                if ((res >= 'A' and res <= 'Z') or
                    (res >= 'a' and res <= 'z')) return true;
                return false;
            }
        }.cond;
    }

    pub fn IsDigit() Condition(u8) {
        return comptime struct {
            fn cond(res: u8) bool {
                if (res >= '0' and res <= '9') return true;
                return false;
            }
        }.cond;
    }

    pub fn IsByte(comptime expected: u8) Condition(u8) {
        return comptime struct {
            fn cond(res: u8) bool {
                if (res == expected) return true;
                return false;
            }
        }.cond;
    }

    pub fn IsEmpty() Condition([]const u8) {
        return comptime struct {
            fn cond(res: []const u8) bool {
                if (res.len == 0) return true;
                return false;
            }
        }.cond;
    }

    pub fn StartsWith(comptime expected: []const u8) Condition([]const u8) {
        return comptime struct {
            fn cond(res: []const u8) bool {
                if (mem.startsWith(u8, res, expected)) return true;
                return false;
            }
        }.cond;
    }
};

// Example //

fn Identifier() Par([]const u8) {
    comptime const condition = struct {
        fn cond(res: []const u8) bool {
            if (check.IsEmpty()(res)) return false;
            if (check.IsDigit()(res[0])) return false;
            return true;
        }
    }.cond;
    return comptime Check(byte.Many(OneOf(&[_]Par(u8){
        byte.Alphanumeric(),
        byte.Byte('_'),
    })), condition);
}

fn Token(comptime P: anytype) Par(ResType(P)) {
    return struct {
        fn par(inp: []const u8) Err!Res(ResType(P)) {
            const lw = try byte.Whitespaces()(inp);
            const res = try P(lw.rest);
            const rw = try byte.Whitespaces()(res.rest);
            return Res(ResType(P)).init(res.term, rw.rest);
        }
    }.par;
}

test "Identifier" {
    const parse = comptime Token(Identifier());
    const str =
        \\    Hello__1_23   this is the first line!
        \\And this is the second line.
    ;
    const res = try parse(str);
    testing.expectEqualSlices(u8, res.term, "Hello__1_23");
}
