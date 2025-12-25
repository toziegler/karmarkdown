const std = @import("std");
const protocol = @import("protocol.zig");
const parser = @import("parser.zig");

pub const Symbol = struct {
    name: []const u8,
    kind: protocol.SymbolKind,
    range: protocol.Range,
};

pub const Document = struct {
    uri: []const u8,
    text: []u8,
    symbols: []Symbol,

    pub fn init(
        allocator: std.mem.Allocator,
        uri: []const u8,
        text: []const u8,
        parsed: []Symbol,
    ) !Document {
        return .{
            .uri = uri,
            .text = try allocator.dupe(u8, text),
            .symbols = parsed,
        };
    }

    pub fn deinit(self: *Document, allocator: std.mem.Allocator) void {
        allocator.free(self.text);
        for (self.symbols) |sym| allocator.free(sym.name);
        allocator.free(self.symbols);
    }
};

pub const Workspace = struct {
    allocator: std.mem.Allocator,
    docs: std.StringHashMap(Document),
    parser: parser.Parser,

    pub fn init(allocator: std.mem.Allocator) Workspace {
        return .{
            .allocator = allocator,
            .docs = std.StringHashMap(Document).init(allocator),
            .parser = .{},
        };
    }

    pub fn deinit(self: *Workspace) void {
        var it = self.docs.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
            self.allocator.free(entry.key_ptr.*);
        }
        self.docs.deinit();
    }

    pub fn upsertDocument(self: *Workspace, uri: []const u8, text: []const u8) !void {
        const symbols = try self.parser.parse(self.allocator, text);
        if (self.docs.getEntry(uri)) |entry| {
            entry.value_ptr.deinit(self.allocator);
            entry.value_ptr.* = try Document.init(self.allocator, entry.key_ptr.*, text, symbols);
            return;
        }
        const uri_owned = try self.allocator.dupe(u8, uri);
        const doc = try Document.init(self.allocator, uri_owned, text, symbols);
        try self.docs.put(uri_owned, doc);
    }

    pub fn getDocument(self: *Workspace, uri: []const u8) ?Document {
        if (self.docs.get(uri)) |doc| return doc;
        return null;
    }
};

test "workspace upsert replaces documents" {
    var ws = Workspace.init(std.testing.allocator);
    defer ws.deinit();

    try ws.upsertDocument("file:///a.md", "# Title\n");
    const doc1 = ws.getDocument("file:///a.md").?;
    try std.testing.expectEqual(@as(usize, 1), doc1.symbols.len);

    try ws.upsertDocument("file:///a.md", "# New\n");
    const doc2 = ws.getDocument("file:///a.md").?;
    try std.testing.expectEqual(@as(usize, 1), doc2.symbols.len);
    try std.testing.expect(std.mem.startsWith(u8, doc2.symbols[0].name, "H1: New"));
}
