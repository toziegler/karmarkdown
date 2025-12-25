const std = @import("std");
const protocol = @import("protocol.zig");
const parser = @import("parser.zig");

pub const Config = struct {
    extensions: std.ArrayListUnmanaged([]const u8),
    wiki_links: bool,
};

pub const Symbol = struct {
    name: []const u8,
    kind: protocol.SymbolKind,
    range: protocol.Range,
};

pub const Document = struct {
    uri: []const u8,
    text: []u8,
    symbols: []Symbol,
    headings: []parser.Heading,
    links: []parser.Link,
    link_defs: []parser.LinkDef,

    pub fn init(
        allocator: std.mem.Allocator,
        uri: []const u8,
        text: []const u8,
        doc_parser: parser.Parser,
        enable_wiki: bool,
    ) !Document {
        const owned_text = try allocator.dupe(u8, text);
        const parsed = try doc_parser.parse(allocator, owned_text, enable_wiki);
        return .{
            .uri = uri,
            .text = owned_text,
            .symbols = parsed.symbols,
            .headings = parsed.headings,
            .links = parsed.links,
            .link_defs = parsed.link_defs,
        };
    }

    pub fn deinit(self: *Document, allocator: std.mem.Allocator) void {
        allocator.free(self.text);
        for (self.symbols) |sym| allocator.free(sym.name);
        allocator.free(self.symbols);
        allocator.free(self.headings);
        allocator.free(self.links);
        allocator.free(self.link_defs);
    }
};

pub const Workspace = struct {
    allocator: std.mem.Allocator,
    docs: std.StringHashMap(Document),
    parser: parser.Parser,
    config: Config,
    roots: std.ArrayListUnmanaged([]const u8),

    pub fn init(allocator: std.mem.Allocator) Workspace {
        var ext_list: std.ArrayListUnmanaged([]const u8) = .empty;
        ext_list.append(allocator, ".md") catch {};
        ext_list.append(allocator, ".markdown") catch {};
        return .{
            .allocator = allocator,
            .docs = std.StringHashMap(Document).init(allocator),
            .parser = .{},
            .config = .{
                .extensions = ext_list,
                .wiki_links = true,
            },
            .roots = .empty,
        };
    }

    pub fn deinit(self: *Workspace) void {
        var it = self.docs.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
            self.allocator.free(entry.key_ptr.*);
        }
        self.docs.deinit();
        self.clearRoots();
        self.clearExtensions();
    }

    pub fn upsertDocument(self: *Workspace, uri: []const u8, text: []const u8) !void {
        if (self.docs.getEntry(uri)) |entry| {
            entry.value_ptr.deinit(self.allocator);
            entry.value_ptr.* = try Document.init(
                self.allocator,
                entry.key_ptr.*,
                text,
                self.parser,
                self.config.wiki_links,
            );
            return;
        }
        const uri_owned = try self.allocator.dupe(u8, uri);
        const doc = try Document.init(
            self.allocator,
            uri_owned,
            text,
            self.parser,
            self.config.wiki_links,
        );
        try self.docs.put(uri_owned, doc);
    }

    pub fn getDocument(self: *Workspace, uri: []const u8) ?Document {
        if (self.docs.get(uri)) |doc| return doc;
        return null;
    }

    pub fn removeDocument(self: *Workspace, uri: []const u8) void {
        if (self.docs.getEntry(uri)) |entry| {
            entry.value_ptr.deinit(self.allocator);
            self.allocator.free(entry.key_ptr.*);
            _ = self.docs.remove(uri);
        }
    }

    pub fn addRoot(self: *Workspace, root: []const u8) !void {
        if (self.hasRoot(root)) return;
        const owned = try self.allocator.dupe(u8, root);
        try self.roots.append(self.allocator, owned);
    }

    pub fn indexRoots(self: *Workspace) !void {
        for (self.roots.items) |root| {
            try self.indexFolder(root);
        }
    }

    pub fn setExtensions(self: *Workspace, extensions: []const []const u8) !void {
        self.clearExtensions();
        for (extensions) |ext| {
            const owned = try self.allocator.dupe(u8, ext);
            try self.config.extensions.append(self.allocator, owned);
        }
    }

    pub fn setWikiLinks(self: *Workspace, enabled: bool) void {
        self.config.wiki_links = enabled;
    }

    fn hasRoot(self: *Workspace, root: []const u8) bool {
        for (self.roots.items) |item| {
            if (std.mem.eql(u8, item, root)) return true;
        }
        return false;
    }

    fn clearRoots(self: *Workspace) void {
        for (self.roots.items) |item| {
            self.allocator.free(item);
        }
        self.roots.deinit(self.allocator);
    }

    fn clearExtensions(self: *Workspace) void {
        for (self.config.extensions.items) |item| {
            self.allocator.free(item);
        }
        self.config.extensions.deinit(self.allocator);
    }

    fn indexFolder(self: *Workspace, root: []const u8) !void {
        var dir = try std.fs.openDirAbsolute(root, .{ .iterate = true });
        defer dir.close();

        var walker = try dir.walk(self.allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!hasExtension(entry.path, self.config.extensions.items)) continue;
            try self.upsertDocumentFromPath(entry.path);
        }
    }

    pub fn upsertDocumentFromPath(self: *Workspace, path: []const u8) !void {
        const max_size = 2 * 1024 * 1024;
        const text = try std.fs.readFileAlloc(self.allocator, path, max_size);
        defer self.allocator.free(text);
        const uri = try pathToUri(self.allocator, path);
        defer self.allocator.free(uri);
        try self.upsertDocument(uri, text);
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

test "workspace indexes markdown files in roots" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "# Title\n" });
    try tmp.dir.writeFile(.{ .sub_path = "doc.txt", .data = "# Not Markdown\n" });
    try tmp.dir.makeDir("notes");
    try tmp.dir.writeFile(.{ .sub_path = "notes/sub.md", .data = "## Sub\n" });

    var ws = Workspace.init(std.testing.allocator);
    defer ws.deinit();

    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root_path);

    try ws.addRoot(root_path);
    try ws.indexRoots();

    try std.testing.expect(ws.docs.count() == 2);
}

test "workspace remove document clears entry" {
    var ws = Workspace.init(std.testing.allocator);
    defer ws.deinit();

    try ws.upsertDocument("file:///a.md", "# Title\n");
    try std.testing.expect(ws.docs.count() == 1);

    ws.removeDocument("file:///a.md");
    try std.testing.expect(ws.docs.count() == 0);
}

test "workspace respects configured extensions" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{ .sub_path = "doc.txt", .data = "# Title\n" });
    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "# Skip\n" });

    var ws = Workspace.init(std.testing.allocator);
    defer ws.deinit();

    try ws.setExtensions(&.{".txt"});

    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root_path);

    try ws.addRoot(root_path);
    try ws.indexRoots();

    try std.testing.expect(ws.docs.count() == 1);
}

fn hasExtension(path: []const u8, extensions: []const []const u8) bool {
    for (extensions) |ext| {
        if (std.mem.endsWith(u8, path, ext)) return true;
    }
    return false;
}

fn pathToUri(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "file://{s}", .{path});
}
