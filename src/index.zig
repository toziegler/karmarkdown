const std = @import("std");
const protocol = @import("protocol.zig");
const parser = @import("parser.zig");

pub const Config = struct {
    extensions: std.ArrayListUnmanaged([]const u8),
    wiki_links: bool,
    max_file_size_bytes: usize,
    excludes: std.ArrayListUnmanaged([]const u8),
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
        doc_parser: *parser.Parser,
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
        if (allocator.dupe(u8, ".md")) |owned| {
            ext_list.append(allocator, owned) catch {
                allocator.free(owned);
            };
        } else |_| {}
        if (allocator.dupe(u8, ".markdown")) |owned| {
            ext_list.append(allocator, owned) catch {
                allocator.free(owned);
            };
        } else |_| {}
        return .{
            .allocator = allocator,
            .docs = std.StringHashMap(Document).init(allocator),
            .parser = .{},
            .config = .{
                .extensions = ext_list,
                .wiki_links = true,
                .max_file_size_bytes = 2 * 1024 * 1024,
                .excludes = .empty,
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
        self.clearExcludes();
    }

    pub fn upsertDocument(self: *Workspace, uri: []const u8, text: []const u8) !void {
        if (self.docs.getEntry(uri)) |entry| {
            entry.value_ptr.deinit(self.allocator);
            entry.value_ptr.* = try Document.init(
                self.allocator,
                entry.key_ptr.*,
                text,
                &self.parser,
                self.config.wiki_links,
            );
            return;
        }
        const uri_owned = try self.allocator.dupe(u8, uri);
        const doc = try Document.init(
            self.allocator,
            uri_owned,
            text,
            &self.parser,
            self.config.wiki_links,
        );
        try self.docs.put(uri_owned, doc);
    }

    pub fn getDocument(self: *Workspace, uri: []const u8) ?Document {
        if (self.docs.get(uri)) |doc| return doc;
        return null;
    }

    pub fn getDocumentPath(self: *Workspace, path: []const u8) ?Document {
        const uri = pathToUri(self.allocator, path) catch return null;
        defer self.allocator.free(uri);
        return self.getDocument(uri);
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

    pub fn rootsSlice(self: *Workspace) []const []const u8 {
        return self.roots.items;
    }

    pub fn indexRoots(self: *Workspace) !void {
        for (self.roots.items) |root| {
            try self.indexFolder(root);
        }
    }

    pub fn indexRootsIncremental(self: *Workspace) !void {
        for (self.roots.items) |root| {
            try self.indexFolderIncremental(root);
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

    pub fn setMaxFileSize(self: *Workspace, bytes: usize) void {
        self.config.max_file_size_bytes = bytes;
    }

    pub fn setExcludes(self: *Workspace, excludes: []const []const u8) !void {
        self.clearExcludes();
        for (excludes) |pattern| {
            const owned = try self.allocator.dupe(u8, pattern);
            try self.config.excludes.append(self.allocator, owned);
        }
    }

    pub fn shouldIndexPath(self: *Workspace, path: []const u8) bool {
        if (isExcluded(path, self.config.excludes.items)) return false;
        if (!hasExtension(path, self.config.extensions.items)) return false;
        if (!fileWithinSize(path, self.config.max_file_size_bytes)) return false;
        return true;
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

    fn clearExcludes(self: *Workspace) void {
        for (self.config.excludes.items) |item| {
            self.allocator.free(item);
        }
        self.config.excludes.deinit(self.allocator);
    }

    fn indexFolder(self: *Workspace, root: []const u8) !void {
        var dir = try std.fs.openDirAbsolute(root, .{ .iterate = true });
        defer dir.close();

        var walker = try dir.walk(self.allocator);
        defer walker.deinit();

        while (true) {
            const entry_opt = walker.next() catch |err| switch (err) {
                error.AccessDenied, error.FileNotFound => continue,
                else => return err,
            };
            if (entry_opt == null) break;
            const entry = entry_opt.?;
            if (entry.kind != .file) continue;
            const full_path = try std.fs.path.join(self.allocator, &.{ root, entry.path });
            defer self.allocator.free(full_path);
            if (isExcluded(full_path, self.config.excludes.items)) continue;
            if (!hasExtension(full_path, self.config.extensions.items)) continue;
            if (!fileWithinSize(full_path, self.config.max_file_size_bytes)) continue;
            try self.upsertDocumentFromPath(full_path);
        }
    }

    fn indexFolderIncremental(self: *Workspace, root: []const u8) !void {
        var dir = try std.fs.openDirAbsolute(root, .{ .iterate = true });
        defer dir.close();

        var walker = try dir.walk(self.allocator);
        defer walker.deinit();

        while (true) {
            const entry_opt = walker.next() catch |err| switch (err) {
                error.AccessDenied, error.FileNotFound => continue,
                else => return err,
            };
            if (entry_opt == null) break;
            const entry = entry_opt.?;
            if (entry.kind != .file) continue;
            const full_path = try std.fs.path.join(self.allocator, &.{ root, entry.path });
            defer self.allocator.free(full_path);
            if (isExcluded(full_path, self.config.excludes.items)) continue;
            if (!hasExtension(full_path, self.config.extensions.items)) continue;
            if (!fileWithinSize(full_path, self.config.max_file_size_bytes)) continue;
            if (self.getDocumentPath(full_path) != null) continue;
            try self.upsertDocumentFromPath(full_path);
        }
    }

    pub fn upsertDocumentFromPath(self: *Workspace, path: []const u8) !void {
        const text = try std.fs.cwd().readFileAlloc(
            self.allocator,
            path,
            self.config.max_file_size_bytes,
        );
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

test "workspace indexing uses absolute file uris" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "# Title\n" });

    var ws = Workspace.init(std.testing.allocator);
    defer ws.deinit();

    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root_path);

    try ws.addRoot(root_path);
    try ws.indexRoots();

    var it = ws.docs.iterator();
    const entry = it.next().?;
    const expected_prefix = try std.fmt.allocPrint(std.testing.allocator, "file://{s}", .{root_path});
    defer std.testing.allocator.free(expected_prefix);
    try std.testing.expect(std.mem.startsWith(u8, entry.key_ptr.*, expected_prefix));
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

test "workspace excludes patterns and max file size" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.makeDir("skip");
    try tmp.dir.writeFile(.{ .sub_path = "skip/ignore.md", .data = "# Skip\n" });
    try tmp.dir.writeFile(.{ .sub_path = "keep.md", .data = "# Keep\n" });
    try tmp.dir.writeFile(.{ .sub_path = "big.md", .data = "# Too big\n" });

    var ws = Workspace.init(std.testing.allocator);
    defer ws.deinit();

    try ws.setExcludes(&.{"skip"});
    ws.setMaxFileSize(4);

    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root_path);

    try ws.addRoot(root_path);
    try ws.indexRoots();

    try std.testing.expect(ws.docs.count() == 0);

    ws.setMaxFileSize(1024);
    try ws.indexRoots();
    try std.testing.expect(ws.docs.count() == 1);
}

fn hasExtension(path: []const u8, extensions: []const []const u8) bool {
    for (extensions) |ext| {
        if (std.mem.endsWith(u8, path, ext)) return true;
    }
    return false;
}

fn isExcluded(path: []const u8, excludes: []const []const u8) bool {
    for (excludes) |pattern| {
        if (pattern.len == 0) continue;
        if (std.mem.containsAtLeast(u8, path, 1, pattern)) return true;
    }
    return false;
}

fn fileWithinSize(path: []const u8, max_bytes: usize) bool {
    if (max_bytes == 0) return false;
    const stat = std.fs.cwd().statFile(path) catch return false;
    return stat.size <= max_bytes;
}

fn pathToUri(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "file://{s}", .{path});
}
