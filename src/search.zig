const std = @import("std");

fn toLowerAscii(byte: u8) u8 {
    if (byte >= 'A' and byte <= 'Z') return byte + 32;
    return byte;
}

pub fn isSubsequence(query: []const u8, text: []const u8) bool {
    if (query.len == 0) return true;

    var qi: usize = 0;
    var ti: usize = 0;
    while (qi < query.len and ti < text.len) : (ti += 1) {
        const q = toLowerAscii(query[qi]);
        const t = toLowerAscii(text[ti]);
        if (q == t) {
            qi += 1;
        }
    }
    return qi == query.len;
}

test "isSubsequence matches in order" {
    try std.testing.expect(isSubsequence("md", "Markdown"));
    try std.testing.expect(isSubsequence("h1", "H1: Title"));
    try std.testing.expect(!isSubsequence("zx", "Markdown"));
}
