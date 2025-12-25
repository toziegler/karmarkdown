const std = @import("std");

pub const MiB: usize = 1024 * 1024;

pub fn cut(haystack: []const u8, needle: []const u8) ?struct { []const u8, []const u8 } {
    const idx = std.mem.indexOf(u8, haystack, needle) orelse return null;
    return .{ haystack[0..idx], haystack[idx + needle.len ..] };
}

pub fn cut_prefix(haystack: []const u8, prefix: []const u8) ?[]const u8 {
    if (!std.mem.startsWith(u8, haystack, prefix)) return null;
    return haystack[prefix.len..];
}
