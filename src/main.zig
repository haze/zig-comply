// packages
const std = @import("std");
const meta = std.meta;
const testing = std.testing;

// types
const TypeInfo = @import("builtin").TypeInfo;

pub const Options = struct {
    check_decls: bool = true,
    check_fields: bool = true,
    check_layout: bool = true,

    // when compile_warn is set to true using complie() will
    // throw a compiler error instead of a runtime boolean
    compile_warn: bool = true,

    check_function_visibility: bool = true,
    check_field_offset: bool = true,
};

fn compile_missing(comptime type_name: []const u8, comptime what: []const u8, comptime thing: []const u8) bool {
    @compileError("Struct " ++ type_name ++ " is missing " ++ what ++ " " ++ thing);
    // don't return anything, the return type exists to satisfy the parent return type to provide compiler warnings
}

fn pretty_type_mismatch_string(decl_name: []const u8, comptime source_type: type, comptime against_type: type) []const u8 {
    return decl_name ++ ": " ++ @typeName(source_type) ++ " (is actually " ++ @typeName(against_type) ++ ")";
}

// if the function decls do not match, the return type is the missing items in a string, otherwise null
fn check_for_function_mismatch(comptime decl_name: []const u8, comptime source: TypeInfo.Declaration.Data.FnDecl, comptime against: TypeInfo.Declaration.Data.FnDecl) ?[]const u8 {
    if (source.fn_type != against.fn_type)
        return pretty_type_mismatch_string(decl_name, source.fn_type, against.fn_type);
    // TODO(haze): determine if more checks here are needed
    return null;
}

fn does_data_match(
    comptime source: TypeInfo.Declaration,
    comptime against: TypeInfo.Declaration.Data,
) ?[]const u8 {
    switch (source.data) {
        .Type => |source_type| if (source_type != against.Type) return "TODO",
        .Var => |source_type| if (source_type != against.Var) return "TODO",
        .Fn => |source_fn_decl| return check_for_function_mismatch(source.name, source_fn_decl, against.Fn),
    }
    return null;
}

fn pretty_visibility(is_pub: bool) []const u8 {
    return if (is_pub) "public" else "private";
}

fn check_decls(comptime options: Options, comptime source: type, comptime against: type) bool {
    const against_type_name = @typeName(against);
    inline for (@typeInfo(source).Struct.decls) |source_decl| {
        // check to see if against even has the decl
        const source_pretty_decl_name = switch (source_decl.data) {
            .Type => "type",
            .Var => "variable",
            .Fn => "function",
        };

        if (!@hasDecl(against, source_decl.name))
            return if (options.compile_warn)
                compile_missing(against_type_name, source_pretty_decl_name, source_decl.name)
            else
                false;

        const against_decl = meta.declarationInfo(against, source_decl.name);

        // check that the two decls are the same type
        if (comptime meta.activeTag(source_decl.data) != meta.activeTag(against_decl.data))
            return if (options.compile_warn)
                compile_missing(against_type_name, @tagName(source_decl.data), source_decl.name ++ " (it's actually " ++ @tagName(against_decl.data) ++ ")")
            else
                false;

        // check decl visibility
        if (source_decl.is_pub != against_decl.is_pub and options.check_function_visibility)
            return if (options.compile_warn)
                compile_missing(against_type_name, pretty_visibility(source_decl.is_pub) ++ " " ++ source_pretty_decl_name, source_decl.name ++ " (it's actually " ++ pretty_visibility(against_decl.is_pub) ++ ")")
            else
                false;

        // check that the declarations actually match
        const data_mismatch: ?[]const u8 = does_data_match(source_decl, against_decl.data);
        if (data_mismatch) |whats_missing|
            return if (options.compile_warn)
                compile_missing(against_type_name, @tagName(source_decl.data), whats_missing)
            else
                false;
    }
    return true;
}

// returns the amount of fields in said struct
fn get_field_count(comptime T: type) comptime_int {
    return @typeInfo(T).Struct.fields.len;
}

fn digit_count(comptime digits: comptime_int) comptime_int {
    const ret = std.math.log10(digits);
    return @floatToInt(comptime_int, ret) + 1;
}

fn check_fields(comptime options: Options, comptime source: type, comptime against: type) bool {
    const against_type_name = @typeName(against);
    inline for (@typeInfo(source).Struct.fields) |source_field| {
        if (!@hasField(against, source_field.name))
            if (options.compile_warn)
                compile_missing(against_type_name, "field", source_field.name)
            else
                return false;
        const against_field = meta.fieldInfo(against, source_field.name);
        if (source_field.field_type != against_field.field_type)
            if (options.compile_warn) {
                compile_missing(against_type_name, source_field.name, pretty_type_mismatch_string(source_field.name, source_field.field_type, against_field.field_type));
            } else
                return false;

        if (options.check_field_offset) {
            const source_field_offset = source_field.offset;
            const against_field_offset = against_field.offset;
            if (source_field_offset) |source_offset| {
                if (against_field_offset) |against_offset| {
                    if (source_offset != against_offset) {
                        if (options.compile_warn) {
                            var source_buf: [digit_count(get_field_count(source))]u8 = undefined;
                            var against_buf: [digit_count(get_field_count(against))]u8 = undefined;
                            _ = std.fmt.bufPrint(&source_buf, "{}", .{source_offset}) catch unreachable;
                            _ = std.fmt.bufPrint(&against_buf, "{}", .{against_offset}) catch unreachable;
                            compile_missing(against_type_name, "field " ++ source_field.name, "with offset " ++ source_buf ++ " (is actually " ++ against_buf ++ ")");
                        } else return false;
                    }
                }
            }
        }
    }
    return true;
}

pub fn complies(comptime options: Options, comptime T: type, comptime with: type) bool {
    comptime {
        const t_type_info = @typeInfo(T);
        if (t_type_info != .Struct)
            @compileError("complies(): T (" ++ @typeName(T) ++ ") must be a struct");
        const with_type_info = @typeInfo(with);
        if (with_type_info != .Struct)
            @compileError("complies(): with (" ++ @typeName(with) ++ ") must be a struct");

        // check all decls
        if (options.check_decls and !check_decls(options, T, with))
            return false;

        if (t_type_info.Struct.layout != with_type_info.Struct.layout and options.check_field_offset)
            if (options.compile_warn)
                @compileError("Struct " ++ @typeName(with) ++ "'s layout (" ++ @tagName(meta.activeTag(with.Struct.layout)) ++ ") does not match " ++ @typeName(T) + "'s layout (" ++ @tagName(meta.activeTag(t_type_info.Struct.layout)) ++ ")")
            else
                return false;

        // check all fields
        if (options.check_fields and !check_fields(options, T, with))
            return false;

        return true;
    }
}

test "mismatch field" {
    const Skeleton = struct {
        some_field: u8,
    };

    const Flesh = struct {};
    testing.expect(!complies(.{ .compile_warn = false }, Skeleton, Flesh));
}

test "mismatch field offset" {
    const Skeleton = extern struct {
        some_field: u8,
    };

    const Flesh = extern struct {
        other: usize,
        some_field: u8,
    };
    testing.expect(!complies(.{ .compile_warn = false }, Skeleton, Flesh));
}

test "mismatch field type" {
    const Skeleton = struct {
        x: u8,
    };

    const Flesh = struct {
        x: usize,
    };
    testing.expect(!complies(.{ .compile_warn = false }, Skeleton, Flesh));
}

test "mismatch struct layout" {
    const Skeleton = packed struct {};

    const Flesh = struct {};
    testing.expect(!complies(.{ .compile_warn = false }, Skeleton, Flesh));
}

test "mismatch function" {
    const Skeleton = struct {
        fn foo() void {}
        pub fn pub_foo() void {}
    };

    const Flesh = struct {
        fn foo() u8 {
            return undefined;
        }
    };
    testing.expect(!complies(.{ .compile_warn = false }, Skeleton, Flesh));
}

test "mismatch function type" {
    const Skeleton = struct {
        fn foo() void {}
    };

    const Flesh = struct {
        fn foo() u8 {
            return undefined;
        }
    };
    testing.expect(!complies(.{ .compile_warn = false }, Skeleton, Flesh));
}

test "full compliant type" {
    const Skeleton = extern struct {
        pub const foo_str: []const u8 = undefined;
        const private_foo_str: []const u8 = undefined;
        pub fn foo() void {}
        fn private_foo() void {}
        a_field: u8,
    };

    const Flesh = extern struct {
        pub const foo_str: []const u8 = "foo-str";
        const private_foo_str: []const u8 = undefined;
        pub fn foo() void {}
        fn private_foo() void {}
        a_field: u8,
    };

    testing.expect(complies(.{}, Skeleton, Flesh));
}
