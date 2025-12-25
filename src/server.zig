const std = @import("std");
const lsp = @import("lsp.zig");
const protocol = @import("protocol.zig");
const index = @import("index.zig");
const parser = @import("parser.zig");
const search = @import("search.zig");
const Snap = @import("snaptest.zig").Snap;

const WorkspaceSymbolEntry = struct {
    uri: []const u8,
    symbol: index.Symbol,
};

const Server = struct {
    allocator: std.mem.Allocator,
    workspace: index.Workspace,

    pub fn init(allocator: std.mem.Allocator) Server {
        return .{
            .allocator = allocator,
            .workspace = index.Workspace.init(allocator),
        };
    }

    pub fn deinit(self: *Server) void {
        self.workspace.deinit();
    }
};

pub fn run(allocator: std.mem.Allocator) !void {
    var server = Server.init(allocator);
    defer server.deinit();

    var stdin_buffer: [4096]u8 = undefined;
    var stdout_buffer: [4096]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdin = &stdin_reader.interface;
    const stdout = &stdout_writer.interface;

    while (true) {
        const msg = try lsp.readMessage(allocator, stdin) orelse break;
        defer allocator.free(msg);

        var parsed = try std.json.parseFromSlice(std.json.Value, allocator, msg, .{});
        defer parsed.deinit();

        const root = parsed.value;
        const method = getString(root, "method");
        if (method) |m| {
            if (std.mem.eql(u8, m, "initialize")) {
                try handleInitialize(&server, root);
                try sendInitializeResponse(stdout, root);
                continue;
            }
            if (std.mem.eql(u8, m, "shutdown")) {
                try sendNullResult(stdout, root);
                continue;
            }
            if (std.mem.eql(u8, m, "exit")) break;

            if (std.mem.eql(u8, m, "textDocument/didOpen")) {
                try handleDidOpen(&server, stdout, root);
                continue;
            }
            if (std.mem.eql(u8, m, "textDocument/didChange")) {
                try handleDidChange(&server, stdout, root);
                continue;
            }
            if (std.mem.eql(u8, m, "workspace/didChangeWatchedFiles")) {
                try handleDidChangeWatchedFiles(&server, root);
                continue;
            }
            if (std.mem.eql(u8, m, "textDocument/documentSymbol")) {
                try handleDocumentSymbol(&server, stdout, root);
                continue;
            }
            if (std.mem.eql(u8, m, "workspace/symbol")) {
                try handleWorkspaceSymbol(&server, stdout, root);
                continue;
            }
            if (std.mem.eql(u8, m, "textDocument/definition")) {
                try handleDefinition(&server, stdout, root);
                continue;
            }
            if (std.mem.eql(u8, m, "textDocument/completion")) {
                try handleCompletion(&server, stdout, root);
                continue;
            }
        }
    }
}

fn getString(root: std.json.Value, key: []const u8) ?[]const u8 {
    if (root.object.get(key)) |value| {
        if (value == .string) return value.string;
    }
    return null;
}

fn getId(root: std.json.Value) ?std.json.Value {
    if (root.object.get("id")) |value| return value;
    return null;
}

fn writeJsonValue(writer: anytype, value: std.json.Value) !void {
    switch (value) {
        .string => |text| try protocol.writeJsonString(writer, text),
        .integer => |num| try writer.print("{d}", .{num}),
        .float => |num| try writer.print("{}", .{num}),
        .number_string => |text| try writer.writeAll(text),
        .bool => |flag| try writer.writeAll(if (flag) "true" else "false"),
        .null => try writer.writeAll("null"),
        else => try writer.writeAll("null"),
    }
}

fn sendInitializeResponse(writer: anytype, root: std.json.Value) !void {
    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(std.heap.page_allocator);

    var out = payload.writer(std.heap.page_allocator);
    try out.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    if (getId(root)) |id_val| {
        try writeJsonValue(out, id_val);
    } else {
        try out.writeAll("null");
    }

    try out.writeAll(",\"result\":{\"capabilities\":{");
    try out.writeAll("\"textDocumentSync\":1,");
    try out.writeAll("\"documentSymbolProvider\":true,");
    try out.writeAll("\"workspaceSymbolProvider\":true,");
    try out.writeAll("\"completionProvider\":{\"triggerCharacters\":[\"[\",\"#\"]}");
    try out.writeAll("}}}");

    try lsp.writeMessage(writer, payload.items);
}

fn handleInitialize(server: *Server, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;

    if (params.object.get("initializationOptions")) |opts| {
        if (opts == .object) {
            try applyInitializationOptions(server, opts);
        }
    }

    if (params.object.get("workspaceFolders")) |folders| {
        if (folders == .array) {
            for (folders.array.items) |item| {
                if (item != .object) continue;
                const uri_val = item.object.get("uri") orelse continue;
                if (uri_val != .string) continue;
                if (uriToPath(server.allocator, uri_val.string)) |path| {
                    defer server.allocator.free(path);
                    try server.workspace.addRoot(path);
                }
            }
        }
    } else if (params.object.get("rootUri")) |root_uri| {
        if (root_uri == .string) {
            if (uriToPath(server.allocator, root_uri.string)) |path| {
                defer server.allocator.free(path);
                try server.workspace.addRoot(path);
            }
        }
    }

    try server.workspace.indexRoots();
}

fn applyInitializationOptions(server: *Server, opts: std.json.Value) !void {
    if (opts.object.get("extensions")) |exts| {
        if (exts == .array) {
            var list: std.ArrayListUnmanaged([]const u8) = .empty;
            defer list.deinit(server.allocator);
            for (exts.array.items) |item| {
                if (item != .string) continue;
                try list.append(server.allocator, item.string);
            }
            if (list.items.len > 0) {
                try server.workspace.setExtensions(list.items);
            }
        }
    }
    if (opts.object.get("wikiLinks")) |wl| {
        if (wl == .bool) {
            server.workspace.setWikiLinks(wl.bool);
        }
    }
    if (opts.object.get("maxFileSize")) |max_size| {
        if (max_size == .integer and max_size.integer > 0) {
            server.workspace.setMaxFileSize(@intCast(max_size.integer));
        }
    }
    if (opts.object.get("exclude")) |ex| {
        if (ex == .array) {
            var list: std.ArrayListUnmanaged([]const u8) = .empty;
            defer list.deinit(server.allocator);
            for (ex.array.items) |item| {
                if (item != .string) continue;
                try list.append(server.allocator, item.string);
            }
            if (list.items.len > 0) {
                try server.workspace.setExcludes(list.items);
            }
        }
    }
}

fn sendNullResult(writer: anytype, root: std.json.Value) !void {
    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(std.heap.page_allocator);

    var out = payload.writer(std.heap.page_allocator);
    try out.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    if (getId(root)) |id_val| {
        try writeJsonValue(out, id_val);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"result\":null}");

    try lsp.writeMessage(writer, payload.items);
}

fn handleDidOpen(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const doc = params.object.get("textDocument") orelse return;
    const uri = doc.object.get("uri") orelse return;
    const text = doc.object.get("text") orelse return;

    if (uri != .string or text != .string) return;
    try server.workspace.upsertDocument(uri.string, text.string);
    if (server.workspace.getDocument(uri.string)) |doc_val| {
        try publishDiagnostics(server, writer, doc_val);
    }
}

fn handleDidChange(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const doc = params.object.get("textDocument") orelse return;
    const uri = doc.object.get("uri") orelse return;
    const changes = params.object.get("contentChanges") orelse return;
    if (uri != .string or changes != .array) return;

    if (changes.array.items.len == 0) return;
    const last = changes.array.items[changes.array.items.len - 1];
    const text_value = last.object.get("text") orelse return;
    if (text_value != .string) return;

    try server.workspace.upsertDocument(uri.string, text_value.string);
    if (server.workspace.getDocument(uri.string)) |doc_val| {
        try publishDiagnostics(server, writer, doc_val);
    }
}

fn handleDidChangeWatchedFiles(server: *Server, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const changes = params.object.get("changes") orelse return;
    if (changes != .array) return;

    for (changes.array.items) |change| {
        if (change != .object) continue;
        const uri_val = change.object.get("uri") orelse continue;
        const type_val = change.object.get("type") orelse continue;
        if (uri_val != .string or type_val != .integer) continue;

        switch (type_val.integer) {
            1, 2 => {
                const path = uriToPath(server.allocator, uri_val.string) orelse continue;
                defer server.allocator.free(path);
                try server.workspace.upsertDocumentFromPath(path);
            },
            3 => {
                server.workspace.removeDocument(uri_val.string);
            },
            else => {},
        }
    }
}

fn handleDocumentSymbol(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const doc = params.object.get("textDocument") orelse return;
    const uri_val = doc.object.get("uri") orelse return;
    if (uri_val != .string) return;

    const doc_opt = server.workspace.getDocument(uri_val.string);
    const symbols = if (doc_opt) |doc_val| doc_val.symbols else &[_]index.Symbol{};
    try sendSymbolResult(writer, root, uri_val.string, symbols);
}

fn handleWorkspaceSymbol(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const query_val = params.object.get("query") orelse return;
    if (query_val != .string) return;

    var collected: std.ArrayList(WorkspaceSymbolEntry) = .empty;
    defer collected.deinit(server.allocator);

    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        for (entry.value_ptr.symbols) |sym| {
            if (search.isSubsequence(query_val.string, sym.name)) {
                try collected.append(server.allocator, .{ .uri = entry.key_ptr.*, .symbol = sym });
            }
        }
    }

    try sendWorkspaceSymbolResult(writer, root, collected.items);
}

fn handleDefinition(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const doc = params.object.get("textDocument") orelse return;
    const uri_val = doc.object.get("uri") orelse return;
    const pos_val = params.object.get("position") orelse return;
    if (uri_val != .string or pos_val != .object) return;

    const line_val = pos_val.object.get("line") orelse return;
    const char_val = pos_val.object.get("character") orelse return;
    if (line_val != .integer or char_val != .integer) return;

    const pos = protocol.Position{
        .line = @intCast(line_val.integer),
        .character = @intCast(char_val.integer),
    };

    const doc_opt = server.workspace.getDocument(uri_val.string);
    if (doc_opt == null) {
        try sendNullResult(writer, root);
        return;
    }
    const doc_val = doc_opt.?;

    const link_opt = findLinkAt(doc_val.links, pos);
    if (link_opt == null) {
        try sendNullResult(writer, root);
        return;
    }

    const location = try resolveLink(server, doc_val, link_opt.?);
    if (location == null) {
        try sendNullResult(writer, root);
        return;
    }

    try sendDefinitionResult(writer, root, location.?);
}

const CompletionItem = struct {
    label: []const u8,
};

const Diagnostic = struct {
    range: protocol.Range,
    message: []const u8,
    severity: u8,
};

fn handleCompletion(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const doc = params.object.get("textDocument") orelse return;
    const uri_val = doc.object.get("uri") orelse return;
    const pos_val = params.object.get("position") orelse return;
    if (uri_val != .string or pos_val != .object) return;

    const line_val = pos_val.object.get("line") orelse return;
    const char_val = pos_val.object.get("character") orelse return;
    if (line_val != .integer or char_val != .integer) return;

    const pos = protocol.Position{
        .line = @intCast(line_val.integer),
        .character = @intCast(char_val.integer),
    };

    const doc_opt = server.workspace.getDocument(uri_val.string) orelse {
        try sendNullResult(writer, root);
        return;
    };

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| server.allocator.free(item.label);
        items.deinit(server.allocator);
    }

    try collectCompletions(server, doc_opt, pos, &items);
    try sendCompletionResult(writer, root, items.items);
}

fn sendSymbolResult(
    writer: anytype,
    root: std.json.Value,
    uri: []const u8,
    symbols: []const index.Symbol,
) !void {
    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(std.heap.page_allocator);

    var out = payload.writer(std.heap.page_allocator);
    try out.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    if (getId(root)) |id_val| {
        try writeJsonValue(out, id_val);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"result\":[");

    for (symbols, 0..) |sym, idx| {
        if (idx > 0) try out.writeByte(',');
        try writeSymbolInformation(out, uri, sym);
    }
    try out.writeAll("]}");

    try lsp.writeMessage(writer, payload.items);
}

fn sendCompletionResult(
    writer: anytype,
    root: std.json.Value,
    items: []const CompletionItem,
) !void {
    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(std.heap.page_allocator);

    var out = payload.writer(std.heap.page_allocator);
    try out.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    if (getId(root)) |id_val| {
        try writeJsonValue(out, id_val);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"result\":{\"isIncomplete\":false,\"items\":[");
    for (items, 0..) |item, idx| {
        if (idx > 0) try out.writeByte(',');
        try out.writeAll("{\"label\":");
        try protocol.writeJsonString(out, item.label);
        try out.writeAll("}");
    }
    try out.writeAll("]}}");

    try lsp.writeMessage(writer, payload.items);
}

fn publishDiagnostics(
    server: *Server,
    writer: anytype,
    doc: index.Document,
) !void {
    const diagnostics = try collectDiagnostics(server, doc);
    defer {
        for (diagnostics) |diag| server.allocator.free(diag.message);
        server.allocator.free(diagnostics);
    }

    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(std.heap.page_allocator);

    var out = payload.writer(std.heap.page_allocator);
    try out.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":");
    try protocol.writeJsonString(out, doc.uri);
    try out.writeAll(",\"diagnostics\":[");
    for (diagnostics, 0..) |diag, idx| {
        if (idx > 0) try out.writeByte(',');
        try out.writeAll("{\"range\":{\"start\":{\"line\":");
        try out.print("{d}", .{diag.range.start.line});
        try out.writeAll(",\"character\":");
        try out.print("{d}", .{diag.range.start.character});
        try out.writeAll("},\"end\":{\"line\":");
        try out.print("{d}", .{diag.range.end.line});
        try out.writeAll(",\"character\":");
        try out.print("{d}", .{diag.range.end.character});
        try out.writeAll("}},\"severity\":");
        try out.print("{d}", .{diag.severity});
        try out.writeAll(",\"source\":\"huntsman\",\"message\":");
        try protocol.writeJsonString(out, diag.message);
        try out.writeAll("}");
    }
    try out.writeAll("]}}");

    try lsp.writeMessage(writer, payload.items);
}

fn collectDiagnostics(server: *Server, doc: index.Document) ![]Diagnostic {
    var list: std.ArrayListUnmanaged(Diagnostic) = .empty;
    errdefer list.deinit(server.allocator);

    for (doc.links) |link| {
        const resolved = try resolveLink(server, doc, link);
        if (resolved == null) {
            const message = try server.allocator.dupe(u8, "Unresolved link");
            try list.append(server.allocator, .{
                .range = link.range,
                .message = message,
                .severity = 2,
            });
        }
    }

    return list.toOwnedSlice(server.allocator);
}

fn sendWorkspaceSymbolResult(
    writer: anytype,
    root: std.json.Value,
    symbols: []const WorkspaceSymbolEntry,
) !void {
    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(std.heap.page_allocator);

    var out = payload.writer(std.heap.page_allocator);
    try out.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    if (getId(root)) |id_val| {
        try writeJsonValue(out, id_val);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"result\":[");

    for (symbols, 0..) |item, idx| {
        if (idx > 0) try out.writeByte(',');
        try writeSymbolInformation(out, item.uri, item.symbol);
    }
    try out.writeAll("]}");

    try lsp.writeMessage(writer, payload.items);
}

fn sendDefinitionResult(
    writer: anytype,
    root: std.json.Value,
    location: protocol.Location,
) !void {
    var payload: std.ArrayList(u8) = .empty;
    defer payload.deinit(std.heap.page_allocator);

    var out = payload.writer(std.heap.page_allocator);
    try out.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
    if (getId(root)) |id_val| {
        try writeJsonValue(out, id_val);
    } else {
        try out.writeAll("null");
    }
    try out.writeAll(",\"result\":[");
    try writeLocation(out, location);
    try out.writeAll("]}");

    try lsp.writeMessage(writer, payload.items);
}

fn writeSymbolInformation(
    writer: anytype,
    uri: []const u8,
    symbol: index.Symbol,
) !void {
    try writer.writeAll("{\"name\":");
    try protocol.writeJsonString(writer, symbol.name);
    try writer.writeAll(",\"kind\":");
    try writer.print("{d}", .{@intFromEnum(symbol.kind)});
    try writer.writeAll(",\"location\":{\"uri\":");
    try protocol.writeJsonString(writer, uri);
    try writer.writeAll(",\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{symbol.range.start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{symbol.range.start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{symbol.range.end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{symbol.range.end.character});
    try writer.writeAll("}}}}");
}

fn writeLocation(writer: anytype, location: protocol.Location) !void {
    try writer.writeAll("{\"uri\":");
    try protocol.writeJsonString(writer, location.uri);
    try writer.writeAll(",\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{location.range.start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{location.range.start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{location.range.end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{location.range.end.character});
    try writer.writeAll("}}}");
}

fn collectCompletions(
    server: *Server,
    doc: index.Document,
    pos: protocol.Position,
    items: *std.ArrayList(CompletionItem),
) !void {
    const line_slice = getLineSlice(doc.text, pos);
    const ctx = completionContext(line_slice, pos.character);

    switch (ctx) {
        .wiki => try appendWikiCompletions(server, items, server.allocator),
        .inline_anchor => try appendHeadingCompletions(doc, items, server.allocator, true),
        .inline_path => try appendPathCompletions(server, items, server.allocator),
        .general => try appendHeadingCompletions(doc, items, server.allocator, false),
    }
}

const CompletionContext = enum {
    wiki,
    inline_path,
    inline_anchor,
    general,
};

fn completionContext(line: []const u8, column: usize) CompletionContext {
    const cursor = @min(column, line.len);
    const before = line[0..cursor];

    if (inWikiContext(before)) return .wiki;
    if (inInlineAnchorContext(before)) return .inline_anchor;
    if (inInlinePathContext(before)) return .inline_path;
    return .general;
}

fn inWikiContext(line: []const u8) bool {
    const open = std.mem.lastIndexOf(u8, line, "[[") orelse return false;
    const close = std.mem.lastIndexOf(u8, line, "]]");
    return close == null or close.? < open;
}

fn inInlinePathContext(line: []const u8) bool {
    const open = std.mem.lastIndexOfScalar(u8, line, '(') orelse return false;
    const close = std.mem.lastIndexOfScalar(u8, line, ')');
    if (close != null and close.? > open) return false;
    const bracket = std.mem.lastIndexOfScalar(u8, line, ']') orelse return false;
    return bracket < open;
}

fn inInlineAnchorContext(line: []const u8) bool {
    const open = std.mem.lastIndexOfScalar(u8, line, '(') orelse return false;
    const close = std.mem.lastIndexOfScalar(u8, line, ')');
    if (close != null and close.? > open) return false;
    const hash = std.mem.lastIndexOfScalar(u8, line, '#') orelse return false;
    return hash > open;
}

fn getLineSlice(text: []const u8, pos: protocol.Position) []const u8 {
    var line_start: usize = 0;
    var current_line: usize = 0;
    var i: usize = 0;
    while (i < text.len and current_line < pos.line) : (i += 1) {
        if (text[i] == '\n') {
            current_line += 1;
            line_start = i + 1;
        }
    }
    var line_end = line_start;
    while (line_end < text.len and text[line_end] != '\n') : (line_end += 1) {}
    return text[line_start..line_end];
}

fn appendWikiCompletions(
    server: *Server,
    items: *std.ArrayList(CompletionItem),
    allocator: std.mem.Allocator,
) !void {
    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const path = uriToPath(allocator, entry.key_ptr.*) orelse continue;
        defer allocator.free(path);
        const base = stripExtension(std.fs.path.basename(path));
        const label = try allocator.dupe(u8, base);
        try items.append(allocator, .{ .label = label });
    }
}

fn appendPathCompletions(
    server: *Server,
    items: *std.ArrayList(CompletionItem),
    allocator: std.mem.Allocator,
) !void {
    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const path = uriToPath(allocator, entry.key_ptr.*) orelse continue;
        defer allocator.free(path);
        const base = std.fs.path.basename(path);
        const label = try allocator.dupe(u8, base);
        try items.append(allocator, .{ .label = label });
    }
}

fn appendHeadingCompletions(
    doc: index.Document,
    items: *std.ArrayList(CompletionItem),
    allocator: std.mem.Allocator,
    with_hash: bool,
) !void {
    for (doc.headings) |heading| {
        var buf: [256]u8 = undefined;
        const slug = slugifyInto(heading.text, &buf);
        const label = if (with_hash)
            try std.fmt.allocPrint(allocator, "#{s}", .{slug})
        else
            try std.fmt.allocPrint(allocator, "{s}", .{heading.text});
        try items.append(allocator, .{ .label = label });
    }
}

fn findLinkAt(links: []const parser.Link, pos: protocol.Position) ?parser.Link {
    for (links) |link| {
        if (posInRange(pos, link.range)) return link;
    }
    return null;
}

fn posInRange(pos: protocol.Position, range: protocol.Range) bool {
    if (pos.line < range.start.line or pos.line > range.end.line) return false;
    if (pos.line == range.start.line and pos.character < range.start.character) return false;
    if (pos.line == range.end.line and pos.character > range.end.character) return false;
    return true;
}

fn resolveLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !?protocol.Location {
    switch (link.kind) {
        .wiki => return resolveWikiLink(server, doc, link),
        .inline_link => return resolveInlineLink(server, doc, link),
        .reference => return resolveReferenceLink(server, doc, link),
    }
}

fn resolveReferenceLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !?protocol.Location {
    const label = link.target.label orelse return null;
    const norm_label = normalizeLabel(label);
    for (doc.link_defs) |def| {
        if (std.mem.eql(u8, normalizeLabel(def.label), norm_label)) {
            const resolved = parser.Link{
                .kind = .inline_link,
                .target = def.target,
                .range = link.range,
            };
            return resolveInlineLink(server, doc, resolved);
        }
    }
    return null;
}

fn resolveInlineLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !?protocol.Location {
    const path = link.target.path orelse "";
    if (std.mem.startsWith(u8, path, "http://") or std.mem.startsWith(u8, path, "https://")) {
        return null;
    }
    const target_uri = if (path.len == 0)
        doc.uri
    else
        try resolvePathUri(server, doc.uri, path) orelse return null;
    return findTargetInDoc(server, target_uri, link.target.anchor);
}

fn resolveWikiLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !?protocol.Location {
    const raw = link.target.path orelse "";
    if (raw.len == 0) {
        return findTargetInDoc(server, doc.uri, link.target.anchor);
    }

    if (looksLikePath(raw)) {
        const target_uri = try resolvePathUri(server, doc.uri, raw) orelse return null;
        return findTargetInDoc(server, target_uri, link.target.anchor);
    }

    const resolved = findDocByTitle(server, raw) orelse return null;
    return findTargetInDoc(server, resolved, link.target.anchor);
}

fn findTargetInDoc(
    server: *Server,
    uri: []const u8,
    anchor: ?[]const u8,
) !?protocol.Location {
    const doc_opt = server.workspace.getDocument(uri) orelse return null;
    if (anchor == null) {
        return .{
            .uri = uri,
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = 0, .character = 0 },
            },
        };
    }
    var target_buf: [256]u8 = undefined;
    const target = slugifyInto(anchor.?, &target_buf);
    for (doc_opt.headings) |heading| {
        var head_buf: [256]u8 = undefined;
        const head = slugifyInto(heading.text, &head_buf);
        if (std.mem.eql(u8, head, target)) {
            return .{
                .uri = uri,
                .range = heading.range,
            };
        }
    }
    return null;
}

fn resolvePathUri(server: *Server, base_uri: []const u8, path: []const u8) !?[]const u8 {
    if (std.mem.startsWith(u8, path, "file://")) return path;
    const base_path = uriToPath(server.allocator, base_uri) orelse return null;
    defer server.allocator.free(base_path);

    const base_dir = std.fs.path.dirname(base_path) orelse base_path;
    const joined = if (std.fs.path.isAbsolute(path))
        try std.fs.path.resolve(server.allocator, &.{ path })
    else
        try std.fs.path.resolve(server.allocator, &.{ base_dir, path });
    defer server.allocator.free(joined);

    const normalized = if (!hasMarkdownExtension(joined) and !std.mem.containsAtLeast(u8, joined, 1, "."))
        try std.fmt.allocPrint(server.allocator, "{s}.md", .{joined})
    else
        try server.allocator.dupe(u8, joined);
    defer server.allocator.free(normalized);

    return findDocByPath(server, normalized);
}

fn findDocByTitle(server: *Server, title: []const u8) ?[]const u8 {
    var want_buf: [256]u8 = undefined;
    const want = slugifyInto(title, &want_buf);
    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const path = uriToPath(server.allocator, entry.key_ptr.*) orelse continue;
        defer server.allocator.free(path);
        const base = std.fs.path.basename(path);
        const base_no_ext = stripExtension(base);
        var base_buf: [256]u8 = undefined;
        const base_slug = slugifyInto(base_no_ext, &base_buf);
        if (std.mem.eql(u8, base_slug, want)) {
            return entry.key_ptr.*;
        }
    }
    return null;
}

fn looksLikePath(text: []const u8) bool {
    return std.mem.containsAtLeast(u8, text, 1, "/") or std.mem.containsAtLeast(u8, text, 1, ".");
}

fn hasMarkdownExtension(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".md") or std.mem.endsWith(u8, path, ".markdown");
}

fn stripExtension(name: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, name, '.')) |idx| {
        return name[0..idx];
    }
    return name;
}

fn normalizeLabel(label: []const u8) []const u8 {
    return std.mem.trim(u8, label, " \t\n\r");
}

fn slugifyInto(text: []const u8, buf: []u8) []const u8 {
    var len: usize = 0;
    for (text) |ch| {
        var lower = ch;
        if (ch >= 'A' and ch <= 'Z') lower = ch + 32;
        if ((lower >= 'a' and lower <= 'z') or (lower >= '0' and lower <= '9')) {
            if (len < buf.len) buf[len] = lower;
            len += 1;
        } else if (lower == ' ' or lower == '-' or lower == '_') {
            if (len > 0 and buf[len - 1] != '-') {
                if (len < buf.len) buf[len] = '-';
                len += 1;
            }
        }
    }
    if (len == 0) return buf[0..0];
    var capped = @min(len, buf.len);
    while (capped > 0 and buf[capped - 1] == '-') capped -= 1;
    return buf[0..capped];
}

fn findDocByPath(server: *Server, path: []const u8) ?[]const u8 {
    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const doc_path = uriToPath(server.allocator, entry.key_ptr.*) orelse continue;
        defer server.allocator.free(doc_path);
        if (std.mem.eql(u8, doc_path, path)) return entry.key_ptr.*;
    }
    return null;
}

test "document symbol response is valid JSON" {
    const allocator = std.testing.allocator;
    var obj = std.json.ObjectMap.init(allocator);
    defer obj.deinit();
    try obj.put("id", std.json.Value{ .integer = 1 });

    const root = std.json.Value{ .object = obj };
    const symbols = [_]index.Symbol{
        .{
            .name = try allocator.dupe(u8, "H1: Title"),
            .kind = .String,
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = 0, .character = 8 },
            },
        },
    };
    defer allocator.free(symbols[0].name);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try sendSymbolResult(&out.writer, root, "file:///test.md", &symbols);
    const payload = try out.toOwnedSlice();
    defer allocator.free(payload);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    try std.testing.expectEqual(@as(usize, 1), result_val.array.items.len);
}

test "workspace symbol response is valid JSON" {
    const allocator = std.testing.allocator;
    var obj = std.json.ObjectMap.init(allocator);
    defer obj.deinit();
    try obj.put("id", std.json.Value{ .integer = 2 });

    const root = std.json.Value{ .object = obj };
    const symbol = index.Symbol{
        .name = try allocator.dupe(u8, "Link: [[Home]]"),
        .kind = .String,
        .range = .{
            .start = .{ .line = 1, .character = 0 },
            .end = .{ .line = 1, .character = 11 },
        },
    };
    defer allocator.free(symbol.name);

    const entries = [_]WorkspaceSymbolEntry{
        .{ .uri = "file:///test.md", .symbol = symbol },
    };

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try sendWorkspaceSymbolResult(&out.writer, root, &entries);
    const payload = try out.toOwnedSlice();
    defer allocator.free(payload);

    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    try std.testing.expectEqual(@as(usize, 1), result_val.array.items.len);
}

test "snapshot: document symbol response" {
    const allocator = std.testing.allocator;
    var obj = std.json.ObjectMap.init(allocator);
    defer obj.deinit();
    try obj.put("id", std.json.Value{ .integer = 1 });

    const root = std.json.Value{ .object = obj };
    const symbols = [_]index.Symbol{
        .{
            .name = try allocator.dupe(u8, "H1: Title"),
            .kind = .String,
            .range = .{
                .start = .{ .line = 0, .character = 0 },
                .end = .{ .line = 0, .character = 8 },
            },
        },
    };
    defer allocator.free(symbols[0].name);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try sendSymbolResult(&out.writer, root, "file:///test.md", &symbols);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":1,"result":[{"name":"H1: Title","kind":15,"location":{"uri":"file:///test.md","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":8}}}}]}
    ).diff(payload);
}

test "snapshot: workspace symbol response" {
    const allocator = std.testing.allocator;
    var obj = std.json.ObjectMap.init(allocator);
    defer obj.deinit();
    try obj.put("id", std.json.Value{ .integer = 2 });

    const root = std.json.Value{ .object = obj };
    const symbol = index.Symbol{
        .name = try allocator.dupe(u8, "Link: [[Home]]"),
        .kind = .String,
        .range = .{
            .start = .{ .line = 1, .character = 0 },
            .end = .{ .line = 1, .character = 11 },
        },
    };
    defer allocator.free(symbol.name);

    const entries = [_]WorkspaceSymbolEntry{
        .{ .uri = "file:///test.md", .symbol = symbol },
    };

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try sendWorkspaceSymbolResult(&out.writer, root, &entries);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":2,"result":[{"name":"Link: [[Home]]","kind":15,"location":{"uri":"file:///test.md","range":{"start":{"line":1,"character":0},"end":{"line":1,"character":11}}}}]}
    ).diff(payload);
}

fn extractPayload(message: []const u8) ?[]const u8 {
    const marker = "\r\n\r\n";
    const idx = std.mem.indexOf(u8, message, marker) orelse return null;
    return message[idx + marker.len ..];
}

test "snapshot: definition response" {
    var obj = std.json.ObjectMap.init(std.testing.allocator);
    defer obj.deinit();
    try obj.put("id", std.json.Value{ .integer = 7 });
    const root = std.json.Value{ .object = obj };

    const location = protocol.Location{
        .uri = "file:///root/doc.md",
        .range = .{
            .start = .{ .line = 1, .character = 2 },
            .end = .{ .line = 1, .character = 9 },
        },
    };

    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try sendDefinitionResult(&out.writer, root, location);
    const message = try out.toOwnedSlice();
    defer std.testing.allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":7,"result":[{"uri":"file:///root/doc.md","range":{"start":{"line":1,"character":2},"end":{"line":1,"character":9}}}]}
    ).diff(payload);
}

test "definition resolves wiki and inline links" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[[b#Heading]] and [B](b.md#Heading)",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/b.md",
        "## Heading\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;
    try std.testing.expect(doc_a.links.len >= 2);

    const wiki_loc = try resolveLink(&server, doc_a, doc_a.links[0]);
    try std.testing.expect(wiki_loc != null);
    try std.testing.expect(std.mem.eql(u8, wiki_loc.?.uri, "file:///root/dir/b.md"));

    const inline_loc = try resolveLink(&server, doc_a, doc_a.links[1]);
    try std.testing.expect(inline_loc != null);
    try std.testing.expect(std.mem.eql(u8, inline_loc.?.uri, "file:///root/dir/b.md"));
}

test "definition resolves reference links" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/a.md",
        "[ref][label]\n\n[label]: b.md#Heading\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/b.md",
        "# Heading\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/a.md").?;
    try std.testing.expect(doc_a.links.len >= 1);

    const loc = try resolveLink(&server, doc_a, doc_a.links[0]);
    try std.testing.expect(loc != null);
    try std.testing.expect(std.mem.eql(u8, loc.?.uri, "file:///root/b.md"));
}

test "completion suggests wiki, paths, and headings" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[[\n[Link](b.md#)\n# Heading One\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/b.md",
        "## Heading Two\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| std.testing.allocator.free(item.label);
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc_a, .{ .line = 0, .character = 2 }, &items);
    sortCompletionItems(items.items);
    const snap = Snap.snap_fn(".");
    const rendered = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered);
    try snap(@src(),
        \\a
        \\b
    ).diff(rendered);

    items.clearRetainingCapacity();
    try collectCompletions(&server, doc_a, .{ .line = 1, .character = 13 }, &items);
    sortCompletionItems(items.items);
    const rendered_anchor = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered_anchor);
    try snap(@src(),
        \\#heading-one
    ).diff(rendered_anchor);
}

test "diagnostics flag unresolved links" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/a.md",
        "[[missing]] and [Link](b.md)",
    );

    const doc_a = server.workspace.getDocument("file:///root/a.md").?;
    const diags = try collectDiagnostics(&server, doc_a);
    defer {
        for (diags) |diag| std.testing.allocator.free(diag.message);
        std.testing.allocator.free(diags);
    }

    const rendered = try renderDiagnostics(std.testing.allocator, diags);
    defer std.testing.allocator.free(rendered);

    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\Unresolved link @ 0:0-0:11
        \\Unresolved link @ 0:16-0:29
    ).diff(rendered);
}

test "config disables wiki links" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    server.workspace.setWikiLinks(false);
    try server.workspace.upsertDocument(
        "file:///root/a.md",
        "[[Wiki]]\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/a.md").?;
    try std.testing.expectEqual(@as(usize, 0), doc_a.links.len);
}

test "initialize options apply extensions and wiki settings" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{ .sub_path = "doc.txt", .data = "# Title\n" });
    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "# Skip\n" });
    try tmp.dir.makeDir("skip");
    try tmp.dir.writeFile(.{ .sub_path = "skip/ignore.txt", .data = "# Ignore\n" });
    try tmp.dir.writeFile(.{ .sub_path = "big.txt", .data = "# Too big\n" });
    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root_path);

    const root_uri = try std.fmt.allocPrint(std.testing.allocator, "file://{s}", .{root_path});
    defer std.testing.allocator.free(root_uri);

    var opts = std.json.ObjectMap.init(std.testing.allocator);
    var exts = std.json.Array.init(std.testing.allocator);
    try exts.append(std.json.Value{ .string = ".txt" });
    try opts.put("extensions", std.json.Value{ .array = exts });
    try opts.put("wikiLinks", std.json.Value{ .bool = false });
    try opts.put("maxFileSize", std.json.Value{ .integer = 8 });
    var exclude = std.json.Array.init(std.testing.allocator);
    try exclude.append(std.json.Value{ .string = "skip" });
    try opts.put("exclude", std.json.Value{ .array = exclude });

    var params = std.json.ObjectMap.init(std.testing.allocator);
    try params.put("rootUri", std.json.Value{ .string = root_uri });
    try params.put("initializationOptions", std.json.Value{ .object = opts });

    var root = std.json.ObjectMap.init(std.testing.allocator);
    try root.put("params", std.json.Value{ .object = params });

    const value = std.json.Value{ .object = root };
    defer deinitValue(std.testing.allocator, value);

    try handleInitialize(&server, value);

    try std.testing.expectEqual(@as(usize, 1), server.workspace.docs.count());
    try std.testing.expectEqual(@as(usize, 1), server.workspace.config.extensions.items.len);
    try std.testing.expectEqual(false, server.workspace.config.wiki_links);
}

test "didChangeWatchedFiles adds, updates, removes" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "# One\n" });
    const root_path = try tmp.dir.realpathAlloc(std.testing.allocator, ".");
    defer std.testing.allocator.free(root_path);

    const file_path = try std.fs.path.join(std.testing.allocator, &.{ root_path, "doc.md" });
    defer std.testing.allocator.free(file_path);
    const uri = try std.fmt.allocPrint(std.testing.allocator, "file://{s}", .{file_path});
    defer std.testing.allocator.free(uri);

    const change_create = try changeEventValue(std.testing.allocator, uri, 1);
    defer deinitValue(std.testing.allocator, change_create);
    try handleDidChangeWatchedFiles(&server, change_create);
    const doc_one = server.workspace.getDocument(uri).?;
    try std.testing.expect(std.mem.startsWith(u8, doc_one.symbols[0].name, "H1: One"));

    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "# Two\n" });
    const change_update = try changeEventValue(std.testing.allocator, uri, 2);
    defer deinitValue(std.testing.allocator, change_update);
    try handleDidChangeWatchedFiles(&server, change_update);
    const doc_two = server.workspace.getDocument(uri).?;
    try std.testing.expect(std.mem.startsWith(u8, doc_two.symbols[0].name, "H1: Two"));

    const change_delete = try changeEventValue(std.testing.allocator, uri, 3);
    defer deinitValue(std.testing.allocator, change_delete);
    try handleDidChangeWatchedFiles(&server, change_delete);
    try std.testing.expect(server.workspace.getDocument(uri) == null);
}

test "fuzz: symbol JSON is valid" {
    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            const max_len: usize = 2048;
            const len = @min(input.len, max_len);
            const buf = try std.testing.allocator.alloc(u8, len);
            defer std.testing.allocator.free(buf);
            sanitizeInput(buf, input[0..len]);

            var p = parser.Parser{};
            const parsed_doc = try p.parse(std.testing.allocator, buf, true);
            defer {
                for (parsed_doc.symbols) |sym| std.testing.allocator.free(sym.name);
                std.testing.allocator.free(parsed_doc.symbols);
                std.testing.allocator.free(parsed_doc.headings);
                std.testing.allocator.free(parsed_doc.links);
                std.testing.allocator.free(parsed_doc.link_defs);
            }

            var obj = std.json.ObjectMap.init(std.testing.allocator);
            defer obj.deinit();
            try obj.put("id", std.json.Value{ .integer = 1 });
            const root = std.json.Value{ .object = obj };

            var out = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer out.deinit();
            try sendSymbolResult(&out.writer, root, "file:///fuzz.md", parsed_doc.symbols);
            const message = try out.toOwnedSlice();
            defer std.testing.allocator.free(message);

            const payload = extractPayload(message) orelse return error.TestExpectedPayload;
            var parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, payload, .{});
            defer parsed.deinit();
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

test "fuzz: completion and diagnostics JSON are valid" {
    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            const max_len: usize = 2048;
            const len = @min(input.len, max_len);
            const buf = try std.testing.allocator.alloc(u8, len);
            defer std.testing.allocator.free(buf);
            sanitizeInput(buf, input[0..len]);

            var server = Server.init(std.testing.allocator);
            defer server.deinit();

            try server.workspace.upsertDocument("file:///fuzz.md", buf);
            const doc = server.workspace.getDocument("file:///fuzz.md").?;

            var items: std.ArrayList(CompletionItem) = .empty;
            defer {
                for (items.items) |item| std.testing.allocator.free(item.label);
                items.deinit(std.testing.allocator);
            }
            try collectCompletions(&server, doc, .{ .line = 0, .character = 0 }, &items);

            var out = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer out.deinit();

            var obj = std.json.ObjectMap.init(std.testing.allocator);
            defer obj.deinit();
            try obj.put("id", std.json.Value{ .integer = 1 });
            const root = std.json.Value{ .object = obj };
            try sendCompletionResult(&out.writer, root, items.items);
            const msg = try out.toOwnedSlice();
            defer std.testing.allocator.free(msg);
            const payload = extractPayload(msg) orelse return error.TestExpectedPayload;
            var parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, payload, .{});
            defer parsed.deinit();

            var diag_out = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer diag_out.deinit();
            try publishDiagnostics(&server, &diag_out.writer, doc);
            const diag_msg = try diag_out.toOwnedSlice();
            defer std.testing.allocator.free(diag_msg);
            const diag_payload = extractPayload(diag_msg) orelse return error.TestExpectedPayload;
            var parsed_diag = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, diag_payload, .{});
            defer parsed_diag.deinit();
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

fn renderCompletions(allocator: std.mem.Allocator, items: []const CompletionItem) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(allocator);
    for (items) |item| {
        try out.appendSlice(allocator, item.label);
        try out.appendSlice(allocator, "\n");
    }
    return out.toOwnedSlice(allocator);
}

fn renderDiagnostics(allocator: std.mem.Allocator, diags: []const Diagnostic) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(allocator);
    for (diags) |diag| {
        try out.appendSlice(allocator, diag.message);
        try out.appendSlice(allocator, " @ ");
        try out.writer(allocator).print(
            "{d}:{d}-{d}:{d}\n",
            .{
                diag.range.start.line,
                diag.range.start.character,
                diag.range.end.line,
                diag.range.end.character,
            },
        );
    }
    return out.toOwnedSlice(allocator);
}

fn changeEventValue(
    allocator: std.mem.Allocator,
    uri: []const u8,
    change_type: i64,
) !std.json.Value {
    var change_obj = std.json.ObjectMap.init(allocator);
    try change_obj.put("uri", std.json.Value{ .string = uri });
    try change_obj.put("type", std.json.Value{ .integer = change_type });

    var changes = std.json.Array.init(allocator);
    try changes.append(std.json.Value{ .object = change_obj });

    var params = std.json.ObjectMap.init(allocator);
    try params.put("changes", std.json.Value{ .array = changes });

    var root = std.json.ObjectMap.init(allocator);
    try root.put("params", std.json.Value{ .object = params });

    return std.json.Value{ .object = root };
}

fn deinitValue(allocator: std.mem.Allocator, value: std.json.Value) void {
    switch (value) {
        .object => |obj| {
            var it = obj.iterator();
            while (it.next()) |entry| {
                deinitValue(allocator, entry.value_ptr.*);
            }
            var mutable = obj;
            mutable.deinit();
        },
        .array => |arr| {
            for (arr.items) |item| {
                deinitValue(allocator, item);
            }
            var mutable = arr;
            mutable.deinit();
        },
        else => {},
    }
}

fn sortCompletionItems(items: []CompletionItem) void {
    std.sort.heap(CompletionItem, items, {}, struct {
        fn lessThan(_: void, a: CompletionItem, b: CompletionItem) bool {
            return std.mem.lessThan(u8, a.label, b.label);
        }
    }.lessThan);
}

fn sanitizeInput(dst: []u8, src: []const u8) void {
    for (src, 0..) |ch, i| {
        if (ch == '\n' or ch == '\t') {
            dst[i] = ch;
        } else if (ch < 0x20 or ch == 0x7f) {
            dst[i] = ' ';
        } else {
            dst[i] = ch;
        }
    }
}

fn uriToPath(allocator: std.mem.Allocator, uri: []const u8) ?[]u8 {
    const prefix = "file://";
    if (!std.mem.startsWith(u8, uri, prefix)) return null;
    const path = uri[prefix.len..];
    return allocator.dupe(u8, path) catch null;
}
