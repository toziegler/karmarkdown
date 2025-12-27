const std = @import("std");
const lsp = @import("lsp.zig");
const protocol = @import("protocol.zig");
const index = @import("index.zig");
const parser = @import("parser.zig");
const search = @import("search.zig");
const watcher = @import("watcher.zig");
const Snap = @import("snaptest.zig").Snap;

const WorkspaceSymbolEntry = struct {
    uri: []const u8,
    symbol: index.Symbol,
};

const Server = struct {
    allocator: std.mem.Allocator,
    workspace: index.Workspace,
    watcher: ?watcher.Watcher,

    pub fn init(allocator: std.mem.Allocator) Server {
        return .{
            .allocator = allocator,
            .workspace = index.Workspace.init(allocator),
            .watcher = null,
        };
    }

    pub fn deinit(self: *Server) void {
        self.workspace.deinit();
        if (self.watcher) |*w| w.deinit();
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
    const stdin_fd = std.fs.File.stdin().handle;

    while (true) {
        if (server.watcher) |*w| {
            try pollWatcher(&server, w, stdin_fd, stdout);
        }

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
            if (std.mem.eql(u8, m, "textDocument/codeAction")) {
                try handleCodeAction(&server, stdout, root);
                continue;
            }
        }
    }
}

fn pollWatcher(
    server: *Server,
    w: *watcher.Watcher,
    stdin_fd: std.fs.File.Handle,
    writer: anytype,
) !void {
    const w_fd = w.fd() orelse return;
    var fds = [_]std.posix.pollfd{
        .{ .fd = @intCast(stdin_fd), .events = std.posix.POLL.IN, .revents = 0 },
        .{ .fd = w_fd, .events = std.posix.POLL.IN, .revents = 0 },
    };
    _ = try std.posix.poll(&fds, 0);

    if ((fds[1].revents & std.posix.POLL.IN) != 0) {
        const events = try w.readEvents(server.allocator);
        defer {
            for (events) |ev| server.allocator.free(ev.path);
            server.allocator.free(events);
        }
        for (events) |ev| {
            if (!server.workspace.shouldIndexPath(ev.path) and ev.kind != .delete) continue;
            switch (ev.kind) {
                .create, .modify => {
                    try server.workspace.upsertDocumentFromPath(ev.path);
                    if (server.workspace.getDocumentPath(ev.path)) |doc_val| {
                        try publishDiagnostics(server, writer, doc_val);
                    }
                },
                .delete => {
                    const uri = try std.fmt.allocPrint(server.allocator, "file://{s}", .{ev.path});
                    defer server.allocator.free(uri);
                    server.workspace.removeDocument(uri);
                },
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
    try out.writeAll("\"codeActionProvider\":true,");
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
    try startWatcher(server);
}

fn startWatcher(server: *Server) !void {
    if (server.watcher != null) return;
    var w = watcher.Watcher.init(server.allocator) catch return;
    for (server.workspace.rootsSlice()) |root| {
        w.addRoot(root) catch {};
    }
    server.watcher = w;
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
    if (doc_opt) |doc_val| {
        const symbols = buildDocumentSymbols(server.allocator, doc_val);
        defer freeDocSymbols(server.allocator, symbols);
        try sendDocumentSymbolResult(writer, root, symbols);
    } else {
        try sendDocumentSymbolResult(writer, root, &.{});
    }
}

fn handleWorkspaceSymbol(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const query_val = params.object.get("query") orelse return;
    if (query_val != .string) return;

    try server.workspace.indexRootsIncremental();

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
    insert_text: ?[]const u8 = null,
    insert_text_format: ?u8 = null,
    filter_text: ?[]const u8 = null,
};

const Diagnostic = struct {
    range: protocol.Range,
    message: []const u8,
    severity: u8,
};

const DocSymbol = struct {
    name: []const u8,
    kind: protocol.SymbolKind,
    range: protocol.Range,
    selection_range: protocol.Range,
    children: std.ArrayListUnmanaged(DocSymbol) = .empty,
};

const LinkIssue = enum {
    none,
    missing_reference,
    missing_target,
    missing_anchor,
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
        for (items.items) |item| {
            server.allocator.free(item.label);
            if (item.insert_text) |text| server.allocator.free(text);
            if (item.filter_text) |text| server.allocator.free(text);
        }
        items.deinit(server.allocator);
    }

    try collectCompletions(server, doc_opt, pos, &items);
    try sendCompletionResult(writer, root, items.items);
}

fn handleCodeAction(server: *Server, writer: anytype, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const doc = params.object.get("textDocument") orelse return;
    const uri_val = doc.object.get("uri") orelse return;
    const range_val = params.object.get("range") orelse return;
    if (uri_val != .string) return;
    const range = parseRange(range_val) orelse return;

    const doc_opt = server.workspace.getDocument(uri_val.string) orelse {
        try sendCodeActionResult(writer, root, &.{});
        return;
    };

    var actions: std.ArrayList([]const u8) = .empty;
    defer {
        for (actions.items) |item| server.allocator.free(item);
        actions.deinit(server.allocator);
    }

    if (findLinkAt(doc_opt.links, range.start)) |link| {
        if (try buildCreateNoteAction(server, doc_opt, link)) |action| {
            try actions.append(server.allocator, action);
        }
        const fix_actions = try buildFixLinkActions(server, doc_opt, link);
        defer {
            for (fix_actions) |item| server.allocator.free(item);
            server.allocator.free(fix_actions);
        }
        for (fix_actions) |item| {
            try actions.append(server.allocator, item);
        }
    }

    if (try buildRenameNoteAction(server, doc_opt)) |action| {
        try actions.append(server.allocator, action);
    }

    if (try buildExtractSelectionAction(server, doc_opt, range)) |action| {
        try actions.append(server.allocator, action);
    }

    if (try buildCsvToTableAction(server, doc_opt, range)) |action| {
        try actions.append(server.allocator, action);
    }

    if (try buildInsertNewNoteLinkAction(server, doc_opt, range)) |action| {
        try actions.append(server.allocator, action);
    }

    if (try buildInsertFootnoteAction(server, doc_opt, range)) |action| {
        try actions.append(server.allocator, action);
    }

    if (try buildRelatedSectionAction(server, doc_opt)) |action| {
        try actions.append(server.allocator, action);
    }

    try sendCodeActionResult(writer, root, actions.items);
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

fn sendDocumentSymbolResult(
    writer: anytype,
    root: std.json.Value,
    symbols: []const DocSymbol,
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
        try writeDocumentSymbol(out, sym);
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
        if (item.filter_text) |filter_text| {
            try out.writeAll(",\"filterText\":");
            try protocol.writeJsonString(out, filter_text);
        }
        if (item.insert_text) |insert_text| {
            try out.writeAll(",\"insertText\":");
            try protocol.writeJsonString(out, insert_text);
            const format = item.insert_text_format orelse 1;
            try out.writeAll(",\"insertTextFormat\":");
            try out.print("{d}", .{format});
        }
        try out.writeAll("}");
    }
    try out.writeAll("]}}");

    try lsp.writeMessage(writer, payload.items);
}

fn sendCodeActionResult(
    writer: anytype,
    root: std.json.Value,
    actions: []const []const u8,
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
    for (actions, 0..) |action, idx| {
        if (idx > 0) try out.writeByte(',');
        try out.writeAll(action);
    }
    try out.writeAll("]}");

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
        try out.writeAll(",\"source\":\"karmarkdown\",\"message\":");
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
        const issue = try analyzeLink(server, doc, link);
        if (issue == .none) continue;
        const message = try server.allocator.dupe(u8, issueMessage(issue));
        try list.append(server.allocator, .{
            .range = link.range,
            .message = message,
            .severity = 2,
        });
    }

    if (doc.headings.len > 1) {
        var base_list: std.ArrayListUnmanaged(struct {
            heading: parser.Heading,
            base: []const u8,
        }) = .empty;
        defer {
            for (base_list.items) |item| server.allocator.free(item.base);
            base_list.deinit(server.allocator);
        }

        var counts = std.StringHashMap(usize).init(server.allocator);
        defer counts.deinit();

        for (doc.headings) |heading| {
            var buf: [256]u8 = undefined;
            const base = slugifyGfmBaseInto(heading.text, &buf);
            if (base.len == 0) continue;
            const base_owned = try server.allocator.dupe(u8, base);
            try base_list.append(server.allocator, .{ .heading = heading, .base = base_owned });
            if (counts.getEntry(base)) |entry| {
                entry.value_ptr.* += 1;
            } else {
                try counts.put(base_owned, 1);
            }
        }

        for (base_list.items) |item| {
            const count = counts.get(item.base) orelse 0;
            if (count <= 1) continue;
            const message = try std.fmt.allocPrint(
                server.allocator,
                "Duplicate heading id: {s}",
                .{item.base},
            );
            try list.append(server.allocator, .{
                .range = item.heading.range,
                .message = message,
                .severity = 2,
            });
        }
    }

    return list.toOwnedSlice(server.allocator);
}

fn analyzeLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !LinkIssue {
    switch (link.kind) {
        .wiki => return analyzeWikiLink(server, doc, link),
        .inline_link => return analyzeInlineLink(server, doc, link),
        .reference => return analyzeReferenceLink(server, doc, link),
    }
}

fn analyzeReferenceLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !LinkIssue {
    const label = link.target.label orelse return .missing_reference;
    const norm_label = normalizeLabel(label);
    for (doc.link_defs) |def| {
        if (std.mem.eql(u8, normalizeLabel(def.label), norm_label)) {
            return analyzeInlineTarget(server, doc, def.target);
        }
    }
    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        for (entry.value_ptr.link_defs) |def| {
            if (!std.mem.eql(u8, normalizeLabel(def.label), norm_label)) continue;
            return analyzeInlineTarget(server, entry.value_ptr.*, def.target);
        }
    }
    return .missing_reference;
}

fn analyzeInlineLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !LinkIssue {
    return analyzeInlineTarget(server, doc, link.target);
}

fn analyzeWikiLink(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !LinkIssue {
    const raw = link.target.path orelse "";
    const target_uri = if (raw.len == 0)
        doc.uri
    else if (looksLikePath(raw))
        (try resolvePathUri(server, doc.uri, raw) orelse return .missing_target)
    else
        (findDocByTitle(server, raw) orelse return .missing_target);

    if (link.target.anchor) |anchor| {
        if (!anchorExists(server, target_uri, anchor)) return .missing_anchor;
    }
    return .none;
}

fn analyzeInlineTarget(
    server: *Server,
    base_doc: index.Document,
    target: parser.LinkTarget,
) !LinkIssue {
    const path = target.path orelse "";
    if (std.mem.startsWith(u8, path, "http://") or std.mem.startsWith(u8, path, "https://")) {
        return .none;
    }
    const target_uri = if (path.len == 0)
        base_doc.uri
    else
        (try resolvePathUri(server, base_doc.uri, path) orelse return .missing_target);
    if (target.anchor) |anchor| {
        if (!anchorExists(server, target_uri, anchor)) return .missing_anchor;
    }
    return .none;
}

fn anchorExists(server: *Server, uri: []const u8, anchor: []const u8) bool {
    const doc_opt = server.workspace.getDocument(uri) orelse return false;
    const decoded = decodePercentOrCopy(server.allocator, anchor);
    defer if (decoded.owned) server.allocator.free(decoded.text);
    var target_buf: [256]u8 = undefined;
    const target = slugifyGfmBaseInto(decoded.text, &target_buf);
    if (target.len == 0) return false;
    const ids = buildHeadingIds(server.allocator, doc_opt.headings) catch return false;
    defer freeHeadingIds(server.allocator, ids);
    for (ids) |item| {
        if (std.mem.eql(u8, item.id, target)) return true;
    }
    return false;
}

fn issueMessage(issue: LinkIssue) []const u8 {
    return switch (issue) {
        .none => "",
        .missing_reference => "Undefined link reference",
        .missing_target => "Missing target file",
        .missing_anchor => "Missing heading anchor",
    };
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

fn writeDocumentSymbol(writer: anytype, sym: DocSymbol) !void {
    try writer.writeAll("{\"name\":");
    try protocol.writeJsonString(writer, sym.name);
    try writer.writeAll(",\"kind\":");
    try writer.print("{d}", .{@intFromEnum(sym.kind)});
    try writer.writeAll(",\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{sym.range.start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{sym.range.start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{sym.range.end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{sym.range.end.character});
    try writer.writeAll("}},\"selectionRange\":{\"start\":{\"line\":");
    try writer.print("{d}", .{sym.selection_range.start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{sym.selection_range.start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{sym.selection_range.end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{sym.selection_range.end.character});
    try writer.writeAll("}}");
    if (sym.children.items.len > 0) {
        try writer.writeAll(",\"children\":[");
        for (sym.children.items, 0..) |child, idx| {
            if (idx > 0) try writer.writeByte(',');
            try writeDocumentSymbol(writer, child);
        }
        try writer.writeAll("]");
    }
    try writer.writeAll("}");
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
    const prefix = completionPrefix(line_slice, pos.character, ctx);

    switch (ctx) {
        .wiki => try appendWikiCompletions(server, items, server.allocator, prefix),
        .inline_anchor => try appendHeadingCompletions(doc, items, server.allocator, true, prefix),
        .inline_path => try appendPathCompletions(server, doc, items, server.allocator, prefix),
        .general => {
            try appendHeadingCompletions(doc, items, server.allocator, false, prefix);
            try appendSnippetCompletions(items, server.allocator);
        },
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

fn completionPrefix(line: []const u8, column: usize, ctx: CompletionContext) []const u8 {
    const cursor = @min(column, line.len);
    const before = line[0..cursor];
    switch (ctx) {
        .wiki => {
            const open = std.mem.lastIndexOf(u8, before, "[[") orelse return "";
            const tail = before[open + 2 ..];
            if (std.mem.lastIndexOfScalar(u8, tail, '#')) |hash| {
                return tail[hash + 1 ..];
            }
            return tail;
        },
        .inline_path => {
            const open = std.mem.lastIndexOfScalar(u8, before, '(') orelse return "";
            return before[open + 1 ..];
        },
        .inline_anchor => {
            const hash = std.mem.lastIndexOfScalar(u8, before, '#') orelse return "";
            return before[hash + 1 ..];
        },
        .general => return "",
    }
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

fn parseRange(value: std.json.Value) ?protocol.Range {
    if (value != .object) return null;
    const start_val = value.object.get("start") orelse return null;
    const end_val = value.object.get("end") orelse return null;
    if (start_val != .object or end_val != .object) return null;

    const start_line = start_val.object.get("line") orelse return null;
    const start_char = start_val.object.get("character") orelse return null;
    const end_line = end_val.object.get("line") orelse return null;
    const end_char = end_val.object.get("character") orelse return null;
    if (start_line != .integer or start_char != .integer or end_line != .integer or end_char != .integer) return null;

    return .{
        .start = .{ .line = @intCast(start_line.integer), .character = @intCast(start_char.integer) },
        .end = .{ .line = @intCast(end_line.integer), .character = @intCast(end_char.integer) },
    };
}

const TextEdit = struct {
    range: protocol.Range,
    new_text: []const u8,
};

const max_action_selection_bytes = 64 * 1024;

const EditBucket = struct {
    uri: []const u8,
    edits: std.ArrayListUnmanaged(TextEdit) = .empty,
};

fn offsetForPosition(text: []const u8, pos: protocol.Position) usize {
    var line: usize = 0;
    var col: usize = 0;
    var i: usize = 0;
    while (i < text.len and line < pos.line) : (i += 1) {
        if (text[i] == '\n') {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    var target = i;
    while (target < text.len and col < pos.character) : (target += 1) {
        if (text[target] == '\n') break;
        col += 1;
    }
    return target;
}

fn sliceForRange(text: []const u8, range: protocol.Range) []const u8 {
    const start = offsetForPosition(text, range.start);
    const end = offsetForPosition(text, range.end);
    if (end <= start or start >= text.len) return "";
    return text[start..@min(end, text.len)];
}

fn appendWikiCompletions(
    server: *Server,
    items: *std.ArrayList(CompletionItem),
    allocator: std.mem.Allocator,
    prefix: []const u8,
) !void {
    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const path = uriToPath(allocator, entry.key_ptr.*) orelse continue;
        defer allocator.free(path);
        const base = stripExtension(std.fs.path.basename(path));
        if (!startsWithIgnoreCase(base, prefix)) continue;
        const label = try allocator.dupe(u8, base);
        try items.append(allocator, .{ .label = label });
    }
}

fn appendPathCompletions(
    server: *Server,
    doc: index.Document,
    items: *std.ArrayList(CompletionItem),
    allocator: std.mem.Allocator,
    prefix: []const u8,
) !void {
    const doc_path = uriToPath(allocator, doc.uri) orelse return;
    defer allocator.free(doc_path);
    const doc_dir = std.fs.path.dirname(doc_path) orelse doc_path;
    const parts = pathPrefixParts(prefix);
    const dir_prefix = normalizePathPrefix(parts.dir_prefix);
    const match_prefix = normalizePathPrefix(prefix);
    const title_query = parts.query;
    const lead = pathLeadingPrefix(prefix);

    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const path = uriToPath(allocator, entry.key_ptr.*) orelse continue;
        defer allocator.free(path);
        const rel = std.fs.path.relative(allocator, doc_dir, path) catch continue;
        defer allocator.free(rel);
        if (dir_prefix.len > 0 and !startsWithIgnoreCase(rel, dir_prefix)) continue;
        if (startsWithIgnoreCase(rel, match_prefix)) {
            const label = try allocator.dupe(u8, rel);
            const insert_text = try std.fmt.allocPrint(allocator, "{s}{s}", .{ lead, rel });
            const filter_text = if (prefix.len > 0) try allocator.dupe(u8, prefix) else null;
            try items.append(allocator, .{
                .label = label,
                .insert_text = insert_text,
                .filter_text = filter_text,
            });
        }

        const title = bestTitleForQuery(allocator, entry.value_ptr.*, entry.key_ptr.*, title_query) catch continue;
        defer allocator.free(title);
        if (title_query.len == 0) {
            if (dir_prefix.len == 0) continue;
        } else if (!containsIgnoreCase(title, title_query)) {
            continue;
        }

        const label = try std.fmt.allocPrint(allocator, "{s} - {s}{s}", .{ title, lead, rel });
        const insert_text = try std.fmt.allocPrint(allocator, "{s}{s}", .{ lead, rel });
        const filter_text = if (prefix.len > 0) try allocator.dupe(u8, prefix) else null;
        try items.append(allocator, .{
            .label = label,
            .insert_text = insert_text,
            .filter_text = filter_text,
        });
    }
}

fn appendHeadingCompletions(
    doc: index.Document,
    items: *std.ArrayList(CompletionItem),
    allocator: std.mem.Allocator,
    with_hash: bool,
    prefix: []const u8,
) !void {
    if (with_hash) {
        const ids = try buildHeadingIds(allocator, doc.headings);
        defer freeHeadingIds(allocator, ids);
        for (ids) |item| {
            if (!startsWithIgnoreCase(item.id, prefix)) continue;
            const label = try std.fmt.allocPrint(allocator, "#{s}", .{item.id});
            try items.append(allocator, .{ .label = label });
        }
        return;
    }

    for (doc.headings) |heading| {
        if (!startsWithIgnoreCase(heading.text, prefix)) continue;
        const label = try std.fmt.allocPrint(allocator, "{s}", .{heading.text});
        try items.append(allocator, .{ .label = label });
    }
}

fn appendSnippetCompletions(
    items: *std.ArrayList(CompletionItem),
    allocator: std.mem.Allocator,
) !void {
    const Snippet = struct {
        label: []const u8,
        text: []const u8,
    };
    const snippets = [_]Snippet{
        .{ .label = "Snippet: Code block", .text = "```$1\n$0\n```" },
        .{ .label = "Snippet: Frontmatter", .text = "---\ntags: [$1]\n---\n\n$0" },
        .{ .label = "Snippet: Link", .text = "[$1]($2)" },
        .{ .label = "Snippet: Image", .text = "![${1:alt}](${2:path})" },
        .{ .label = "Snippet: Task", .text = "- [ ] $0" },
        .{ .label = "Snippet: Table", .text = "| ${1:Col1} | ${2:Col2} |\n| --- | --- |\n| ${3:Val1} | ${4:Val2} |\n" },
        .{ .label = "Snippet: Blockquote", .text = "> $0" },
        .{ .label = "Snippet: Heading", .text = "# $0" },
        .{ .label = "Snippet: Numbered list", .text = "1. $0" },
        .{ .label = "Snippet: Horizontal rule", .text = "---\n" },
        .{ .label = "Snippet: Footnote anchor", .text = "[^${1:id}]" },
        .{ .label = "Snippet: Footnote definition", .text = "\n[^${1:id}]: $0" },
    };

    for (snippets) |snippet| {
        const label = try allocator.dupe(u8, snippet.label);
        const text = try allocator.dupe(u8, snippet.text);
        try items.append(allocator, .{
            .label = label,
            .insert_text = text,
            .insert_text_format = 2,
        });
    }
}

fn buildDocumentSymbols(
    allocator: std.mem.Allocator,
    doc: index.Document,
) []DocSymbol {
    var roots: std.ArrayListUnmanaged(DocSymbol) = .empty;
    errdefer {
        freeDocSymbolSlice(allocator, roots.items);
        roots.deinit(allocator);
    }

    const EntryKind = enum { heading, block };
    const Entry = struct {
        kind: EntryKind,
        line: usize,
        order: usize,
        index: usize,
    };

    var entries: std.ArrayListUnmanaged(Entry) = .empty;
    defer entries.deinit(allocator);

    var order: usize = 0;
    for (doc.headings, 0..) |heading, idx| {
        entries.append(allocator, .{
            .kind = .heading,
            .line = heading.range.start.line,
            .order = order,
            .index = idx,
        }) catch return &.{};
        order += 1;
    }

    for (doc.symbols, 0..) |sym, idx| {
        if (!isDocSymbolEntry(sym, doc.headings)) continue;
        entries.append(allocator, .{
            .kind = .block,
            .line = sym.range.start.line,
            .order = order,
            .index = idx,
        }) catch return &.{};
        order += 1;
    }

    std.sort.heap(Entry, entries.items, {}, struct {
        fn lessThan(_: void, a: Entry, b: Entry) bool {
            if (a.line == b.line) return a.order < b.order;
            return a.line < b.line;
        }
    }.lessThan);

    var stack: std.ArrayListUnmanaged(struct {
        list: *std.ArrayListUnmanaged(DocSymbol),
        index: usize,
        level: u8,
    }) = .empty;
    defer stack.deinit(allocator);

    var list_stack: std.ArrayListUnmanaged(struct {
        indent: usize,
        list: *std.ArrayListUnmanaged(DocSymbol),
        index: usize,
    }) = .empty;
    defer list_stack.deinit(allocator);

    for (entries.items) |entry| {
        switch (entry.kind) {
            .heading => {
                list_stack.clearRetainingCapacity();
                const heading = doc.headings[entry.index];
                while (stack.items.len > 0 and heading.level <= stack.items[stack.items.len - 1].level) {
                    _ = stack.pop();
                }

                const target_list = if (stack.items.len == 0)
                    &roots
                else
                    &stack.items[stack.items.len - 1]
                        .list.items[stack.items[stack.items.len - 1].index]
                        .children;

                const node = DocSymbol{
                    .name = headingSymbolName(doc, heading),
                    .kind = .String,
                    .range = heading.range,
                    .selection_range = heading.range,
                };
                target_list.append(allocator, node) catch return &.{};
                const idx = target_list.items.len - 1;
                stack.append(allocator, .{
                    .list = target_list,
                    .index = idx,
                    .level = heading.level,
                }) catch return &.{};
            },
            .block => {
                const sym = doc.symbols[entry.index];
                const base_list = if (stack.items.len == 0)
                    &roots
                else
                    &stack.items[stack.items.len - 1]
                        .list.items[stack.items[stack.items.len - 1].index]
                        .children;

                if (isListItemSymbol(sym)) {
                    const indent = listItemIndent(doc.text, sym.range.start.line);
                    while (list_stack.items.len > 0 and indent <= list_stack.items[list_stack.items.len - 1].indent) {
                        _ = list_stack.pop();
                    }
                    const target_list = if (list_stack.items.len > 0)
                        &list_stack.items[list_stack.items.len - 1]
                            .list.items[list_stack.items[list_stack.items.len - 1].index]
                            .children
                    else
                        base_list;
                    const node = DocSymbol{
                        .name = sym.name,
                        .kind = sym.kind,
                        .range = sym.range,
                        .selection_range = sym.range,
                    };
                    target_list.append(allocator, node) catch return &.{};
                    const idx = target_list.items.len - 1;
                    list_stack.append(allocator, .{
                        .indent = indent,
                        .list = target_list,
                        .index = idx,
                    }) catch return &.{};
                } else {
                    list_stack.clearRetainingCapacity();
                    const node = DocSymbol{
                        .name = sym.name,
                        .kind = sym.kind,
                        .range = sym.range,
                        .selection_range = sym.range,
                    };
                    base_list.append(allocator, node) catch return &.{};
                }
            },
        }
    }

    const out = roots.toOwnedSlice(allocator) catch {
        freeDocSymbolSlice(allocator, roots.items);
        roots.deinit(allocator);
        return &.{};
    };
    return out;
}

fn headingSymbolName(doc: index.Document, heading: parser.Heading) []const u8 {
    for (doc.symbols) |sym| {
        if (rangeEqual(sym.range, heading.range) and std.mem.startsWith(u8, sym.name, "H")) {
            return sym.name;
        }
    }
    return heading.text;
}

fn rangeEqual(a: protocol.Range, b: protocol.Range) bool {
    return a.start.line == b.start.line and a.start.character == b.start.character and
        a.end.line == b.end.line and a.end.character == b.end.character;
}

fn isDocSymbolEntry(sym: index.Symbol, headings: []parser.Heading) bool {
    const is_list = std.mem.startsWith(u8, sym.name, "List:");
    const is_code = std.mem.startsWith(u8, sym.name, "Code:");
    const is_link = std.mem.startsWith(u8, sym.name, "Link:");
    const is_ref = std.mem.startsWith(u8, sym.name, "Ref:");
    const is_table = std.mem.startsWith(u8, sym.name, "Table:");
    const is_tag = std.mem.startsWith(u8, sym.name, "Tag:") or
        std.mem.startsWith(u8, sym.name, "ProjectTag:");
    const is_task = std.mem.startsWith(u8, sym.name, "Task:");
    if (!is_list and !is_code and !is_link and !is_ref and !is_table and !is_tag and !is_task) return false;
    for (headings) |heading| {
        if (rangeEqual(sym.range, heading.range)) return false;
    }
    return true;
}

fn isListItemSymbol(sym: index.Symbol) bool {
    return std.mem.startsWith(u8, sym.name, "List:") or std.mem.startsWith(u8, sym.name, "Task:");
}

fn listItemIndent(text: []const u8, line: usize) usize {
    const line_slice = getLineSlice(text, .{ .line = line, .character = 0 });
    var count: usize = 0;
    while (count < line_slice.len and (line_slice[count] == ' ' or line_slice[count] == '\t')) : (count += 1) {}
    return count;
}

fn freeDocSymbols(allocator: std.mem.Allocator, symbols: []DocSymbol) void {
    if (symbols.len == 0) return;
    freeDocSymbolSlice(allocator, symbols);
    allocator.free(symbols);
}

fn freeDocSymbolSlice(allocator: std.mem.Allocator, symbols: []DocSymbol) void {
    for (symbols) |*sym| {
        freeDocSymbolSlice(allocator, sym.children.items);
        sym.children.deinit(allocator);
    }
}

fn startsWithIgnoreCase(text: []const u8, prefix: []const u8) bool {
    if (prefix.len == 0) return true;
    if (text.len < prefix.len) return false;
    for (prefix, 0..) |ch, i| {
        if (lowerAscii(text[i]) != lowerAscii(ch)) return false;
    }
    return true;
}

fn containsIgnoreCase(text: []const u8, needle: []const u8) bool {
    if (needle.len == 0) return true;
    if (text.len < needle.len) return false;
    var i: usize = 0;
    while (i + needle.len <= text.len) : (i += 1) {
        var j: usize = 0;
        while (j < needle.len) : (j += 1) {
            if (lowerAscii(text[i + j]) != lowerAscii(needle[j])) break;
        }
        if (j == needle.len) return true;
    }
    return false;
}

fn bestTitleForQuery(
    allocator: std.mem.Allocator,
    doc: index.Document,
    uri: []const u8,
    query: []const u8,
) ![]u8 {
    if (query.len > 0) {
        for (doc.headings) |heading| {
            if (containsIgnoreCase(heading.text, query)) {
                return allocator.dupe(u8, heading.text);
            }
        }
    }
    return documentTitle(allocator, doc, uri);
}

fn pathPrefixParts(prefix: []const u8) struct { dir_prefix: []const u8, query: []const u8 } {
    if (prefix.len == 0) return .{ .dir_prefix = "", .query = "" };
    if (std.mem.lastIndexOfAny(u8, prefix, "/\\")) |idx| {
        const dir_prefix = prefix[0 .. idx + 1];
        const query = if (idx + 1 < prefix.len) prefix[idx + 1 ..] else "";
        return .{ .dir_prefix = dir_prefix, .query = query };
    }
    return .{ .dir_prefix = "", .query = prefix };
}

fn normalizePathPrefix(prefix: []const u8) []const u8 {
    if (std.mem.startsWith(u8, prefix, "./")) return prefix[2..];
    if (std.mem.startsWith(u8, prefix, ".\\")) return prefix[2..];
    return prefix;
}

fn pathLeadingPrefix(prefix: []const u8) []const u8 {
    if (std.mem.startsWith(u8, prefix, "./")) return "./";
    if (std.mem.startsWith(u8, prefix, ".\\")) return ".\\";
    return "";
}

fn lowerAscii(ch: u8) u8 {
    if (ch >= 'A' and ch <= 'Z') return ch + 32;
    return ch;
}

fn findLinkAt(links: []const parser.Link, pos: protocol.Position) ?parser.Link {
    for (links) |link| {
        if (posInRange(pos, link.range)) return link;
    }
    return null;
}

fn findLinkByAnchor(links: []const parser.Link, anchor: []const u8) ?parser.Link {
    for (links) |link| {
        if (link.target.anchor) |value| {
            if (std.mem.eql(u8, value, anchor)) return link;
        }
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
    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        for (entry.value_ptr.link_defs) |def| {
            if (!std.mem.eql(u8, normalizeLabel(def.label), norm_label)) continue;
            const resolved = parser.Link{
                .kind = .inline_link,
                .target = def.target,
                .range = link.range,
            };
            return resolveInlineLink(server, entry.value_ptr.*, resolved);
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
    const decoded = decodePercentOrCopy(server.allocator, anchor.?);
    defer if (decoded.owned) server.allocator.free(decoded.text);
    var target_buf: [256]u8 = undefined;
    const target = slugifyGfmBaseInto(decoded.text, &target_buf);
    if (target.len == 0) return null;
    const ids = try buildHeadingIds(server.allocator, doc_opt.headings);
    defer freeHeadingIds(server.allocator, ids);
    for (ids) |item| {
        if (std.mem.eql(u8, item.id, target)) {
            return .{
                .uri = uri,
                .range = item.heading.range,
            };
        }
    }
    return null;
}

fn resolvePathUri(server: *Server, base_uri: []const u8, path: []const u8) !?[]const u8 {
    const normalized = normalizeLinkPath(path);
    const decoded = try decodePercent(server.allocator, normalized);
    defer if (decoded.owned) server.allocator.free(decoded.text);
    const normalized_path = decoded.text;

    if (std.mem.startsWith(u8, normalized_path, "file://")) {
        const file_path = uriToPath(server.allocator, normalized_path) orelse return null;
        defer server.allocator.free(file_path);
        if (findDocByPath(server, file_path)) |uri| return uri;
        return try ensureDocumentForPath(server, file_path);
    }

    const base_path = uriToPath(server.allocator, base_uri) orelse return null;
    defer server.allocator.free(base_path);
    const base_dir = std.fs.path.dirname(base_path) orelse base_path;

    if (std.fs.path.isAbsolute(normalized_path)) {
        if (try resolveCandidatePath(server, normalized_path)) |uri| return uri;
    } else {
        if (try resolveFromBase(server, base_dir, normalized_path)) |uri| return uri;
        for (server.workspace.rootsSlice()) |root| {
            if (try resolveFromBase(server, root, normalized_path)) |uri| return uri;
        }
    }

    return null;
}

fn resolveFromBase(server: *Server, base_dir: []const u8, path: []const u8) !?[]const u8 {
    const joined = try std.fs.path.resolve(server.allocator, &.{ base_dir, path });
    defer server.allocator.free(joined);
    return try resolveCandidatePath(server, joined);
}

fn resolveCandidatePath(server: *Server, path: []const u8) !?[]const u8 {
    const base = std.fs.path.basename(path);
    const needs_ext = !hasMarkdownExtension(path) and std.mem.indexOfScalar(u8, base, '.') == null;

    if (!needs_ext) {
        return try ensureDocumentForPath(server, path);
    }

    const md_path = try std.fmt.allocPrint(server.allocator, "{s}.md", .{path});
    defer server.allocator.free(md_path);
    if (try ensureDocumentForPath(server, md_path)) |uri| return uri;

    const markdown_path = try std.fmt.allocPrint(server.allocator, "{s}.markdown", .{path});
    defer server.allocator.free(markdown_path);
    return try ensureDocumentForPath(server, markdown_path);
}

fn ensureDocumentForPath(server: *Server, path: []const u8) !?[]const u8 {
    if (!server.workspace.shouldIndexPath(path)) return null;
    if (!pathIsFile(path)) return null;
    if (findDocByPath(server, path)) |uri| return uri;
    try server.workspace.upsertDocumentFromPath(path);
    return findDocByPath(server, path);
}

fn pathIsFile(path: []const u8) bool {
    const stat = std.fs.cwd().statFile(path) catch return false;
    return stat.kind == .file;
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
    var last_dash = false;
    for (text) |ch| {
        var lower = ch;
        if (ch >= 'A' and ch <= 'Z') lower = ch + 32;
        if ((lower >= 'a' and lower <= 'z') or (lower >= '0' and lower <= '9')) {
            if (len < buf.len) buf[len] = lower;
            len += 1;
            last_dash = false;
        } else if (lower == ' ' or lower == '-' or lower == '_') {
            if (!last_dash) {
                if (len < buf.len) buf[len] = '-';
                len += 1;
                last_dash = true;
            }
        }
    }
    if (len == 0) return buf[0..0];
    var capped = @min(len, buf.len);
    while (capped > 0 and buf[capped - 1] == '-') capped -= 1;
    return buf[0..capped];
}

const HeadingId = struct {
    heading: parser.Heading,
    id: []const u8,
};

fn buildHeadingIds(
    allocator: std.mem.Allocator,
    headings: []const parser.Heading,
) ![]HeadingId {
    var ids: std.ArrayListUnmanaged(HeadingId) = .empty;
    errdefer {
        for (ids.items) |item| allocator.free(item.id);
        ids.deinit(allocator);
    }

    var base_keys: std.ArrayListUnmanaged([]const u8) = .empty;
    defer {
        for (base_keys.items) |key| allocator.free(key);
        base_keys.deinit(allocator);
    }

    var counts = std.StringHashMap(usize).init(allocator);
    defer counts.deinit();

    for (headings) |heading| {
        var buf: [256]u8 = undefined;
        const base = slugifyGfmBaseInto(heading.text, &buf);
        if (base.len == 0) continue;

        var suffix: usize = 0;
        if (counts.getEntry(base)) |entry| {
            suffix = entry.value_ptr.*;
            entry.value_ptr.* = suffix + 1;
        } else {
            const key = try allocator.dupe(u8, base);
            try base_keys.append(allocator, key);
            try counts.put(key, 1);
            suffix = 0;
        }

        const id = if (suffix == 0)
            try allocator.dupe(u8, base)
        else
            try std.fmt.allocPrint(allocator, "{s}-{d}", .{ base, suffix });

        try ids.append(allocator, .{ .heading = heading, .id = id });
    }

    return ids.toOwnedSlice(allocator);
}

fn freeHeadingIds(allocator: std.mem.Allocator, ids: []HeadingId) void {
    for (ids) |item| allocator.free(item.id);
    allocator.free(ids);
}

fn slugifyGfmBaseInto(text: []const u8, buf: []u8) []const u8 {
    var len: usize = 0;
    var last_dash = false;
    for (text) |ch| {
        var lower = ch;
        if (ch >= 'A' and ch <= 'Z') lower = ch + 32;

        if ((lower >= 'a' and lower <= 'z') or (lower >= '0' and lower <= '9')) {
            if (len < buf.len) buf[len] = lower;
            len += 1;
            last_dash = false;
            continue;
        }

        if (lower == '_') {
            if (len < buf.len) buf[len] = lower;
            len += 1;
            last_dash = false;
            continue;
        }

        if (lower == ' ' or lower == '-') {
            if (!last_dash) {
                if (len < buf.len) buf[len] = '-';
                len += 1;
                last_dash = true;
            }
            continue;
        }
    }

    var capped = @min(len, buf.len);
    while (capped > 0 and buf[capped - 1] == '-') capped -= 1;
    return buf[0..capped];
}

fn resolveLinkTarget(server: *Server, doc: index.Document, link: parser.Link) ?parser.LinkTarget {
    switch (link.kind) {
        .wiki, .inline_link => return link.target,
        .reference => {
            const label = link.target.label orelse return null;
            const norm = normalizeLabel(label);
            for (doc.link_defs) |def| {
                if (std.mem.eql(u8, normalizeLabel(def.label), norm)) return def.target;
            }
            var it = server.workspace.docs.iterator();
            while (it.next()) |entry| {
                for (entry.value_ptr.link_defs) |def| {
                    if (std.mem.eql(u8, normalizeLabel(def.label), norm)) return def.target;
                }
            }
        },
    }
    return null;
}

fn isWebLink(path: []const u8) bool {
    return std.mem.startsWith(u8, path, "http://") or std.mem.startsWith(u8, path, "https://");
}

fn buildNotePath(allocator: std.mem.Allocator, base_dir: []const u8, raw_path: []const u8) ![]u8 {
    const joined = if (std.fs.path.isAbsolute(raw_path))
        try std.fs.path.resolve(allocator, &.{ raw_path })
    else
        try std.fs.path.resolve(allocator, &.{ base_dir, raw_path });
    defer allocator.free(joined);

    const base = std.fs.path.basename(joined);
    const needs_ext = !hasMarkdownExtension(joined) and std.mem.indexOfScalar(u8, base, '.') == null;
    if (!needs_ext) return try allocator.dupe(u8, joined);
    return try std.fmt.allocPrint(allocator, "{s}.md", .{joined});
}

fn noteTitleFromTarget(target: parser.LinkTarget, path: []const u8) []const u8 {
    if (target.label) |label| return label;
    const base = std.fs.path.basename(path);
    const title = stripExtension(base);
    return if (title.len == 0) "Note" else title;
}

fn normalizeLinkPath(path: []const u8) []const u8 {
    var trimmed = std.mem.trim(u8, path, " \t\r\n");
    if (trimmed.len >= 2) {
        const first = trimmed[0];
        const last = trimmed[trimmed.len - 1];
        if ((first == '"' and last == '"') or (first == '\'' and last == '\'') or (first == '<' and last == '>')) {
            trimmed = trimmed[1 .. trimmed.len - 1];
        }
    }
    return trimmed;
}

fn writeTextEdit(writer: anytype, edit: TextEdit) !void {
    try writer.writeAll("{\"range\":{\"start\":{\"line\":");
    try writer.print("{d}", .{edit.range.start.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{edit.range.start.character});
    try writer.writeAll("},\"end\":{\"line\":");
    try writer.print("{d}", .{edit.range.end.line});
    try writer.writeAll(",\"character\":");
    try writer.print("{d}", .{edit.range.end.character});
    try writer.writeAll("}},\"newText\":");
    try protocol.writeJsonString(writer, edit.new_text);
    try writer.writeAll("}");
}

fn buildCreateNoteAction(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) !?[]const u8 {
    const target = resolveLinkTarget(server, doc, link) orelse return null;
    if (target.path == null) return null;
    if (isWebLink(target.path.?)) return null;

    const base_path = uriToPath(server.allocator, doc.uri) orelse return null;
    defer server.allocator.free(base_path);
    const base_dir = std.fs.path.dirname(base_path) orelse base_path;

    const clean_path = normalizeLinkPath(target.path.?);
    const path = try buildNotePath(server.allocator, base_dir, clean_path);
    defer server.allocator.free(path);
    if (pathIsFile(path)) return null;

    const file_uri = try pathToUri(server.allocator, path);
    defer server.allocator.free(file_uri);

    const title = noteTitleFromTarget(target, clean_path);
    const action_title = try std.fmt.allocPrint(server.allocator, "Create note: {s}", .{title});
    defer server.allocator.free(action_title);
    const content = try std.fmt.allocPrint(server.allocator, "# {s}\n", .{title});
    defer server.allocator.free(content);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(server.allocator);
    var writer = out.writer(server.allocator);
    try writer.writeAll("{\"title\":");
    try protocol.writeJsonString(writer, action_title);
    try writer.writeAll(",\"kind\":\"quickfix\",\"edit\":{\"documentChanges\":[");
    try writer.writeAll("{\"kind\":\"create\",\"uri\":");
    try protocol.writeJsonString(writer, file_uri);
    try writer.writeAll("},");
    try writer.writeAll("{\"textDocument\":{\"uri\":");
    try protocol.writeJsonString(writer, file_uri);
    try writer.writeAll(",\"version\":null},\"edits\":[");
    try writeTextEdit(writer, .{
        .range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 0 },
        },
        .new_text = content,
    });
    try writer.writeAll("]}]}}");
    const action = try out.toOwnedSlice(server.allocator);
    return action;
}

fn buildFixLinkActions(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
) ![]const []const u8 {
    if (try analyzeLink(server, doc, link) != .missing_target) {
        return try server.allocator.alloc([]const u8, 0);
    }
    const target = resolveLinkTarget(server, doc, link) orelse {
        return try server.allocator.alloc([]const u8, 0);
    };
    if (target.path == null) return try server.allocator.alloc([]const u8, 0);

    const clean_path = normalizeLinkPath(target.path.?);
    const key_raw = normalizeTargetKey(clean_path);
    if (key_raw.len == 0) return try server.allocator.alloc([]const u8, 0);
    const key = try lowerAlloc(server.allocator, key_raw);
    defer server.allocator.free(key);

    var candidates: std.ArrayListUnmanaged(struct {
        uri: []const u8,
        score: usize,
    }) = .empty;
    defer candidates.deinit(server.allocator);

    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const base = uriBaseName(server.allocator, entry.key_ptr.*) orelse continue;
        defer server.allocator.free(base);
        const base_lower = try lowerAlloc(server.allocator, base);
        defer server.allocator.free(base_lower);
        const score = candidateScore(key, base_lower);
        if (score == 0) continue;
        candidates.append(server.allocator, .{ .uri = entry.key_ptr.*, .score = score }) catch {};
    }

    if (candidates.items.len == 0) return try server.allocator.alloc([]const u8, 0);
    std.sort.heap(@TypeOf(candidates.items[0]), candidates.items, {}, struct {
        fn lessThan(_: void, a: @TypeOf(candidates.items[0]), b: @TypeOf(candidates.items[0])) bool {
            return a.score > b.score;
        }
    }.lessThan);

    const limit = @min(candidates.items.len, 3);
    var actions: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer {
        for (actions.items) |item| server.allocator.free(item);
        actions.deinit(server.allocator);
    }

    var idx: usize = 0;
    while (idx < limit) : (idx += 1) {
        const candidate_uri = candidates.items[idx].uri;
        const replacement = try buildLinkReplacement(server, doc, link, candidate_uri);
        defer server.allocator.free(replacement);

        const candidate_name = basenameFromUri(server.allocator, candidate_uri) orelse null;
        defer if (candidate_name) |name| server.allocator.free(name);
        const title = try std.fmt.allocPrint(
            server.allocator,
            "Fix link -> {s}",
            .{candidate_name orelse "note"},
        );
        defer server.allocator.free(title);

        const action = try buildReplaceAction(server.allocator, doc.uri, link.range, replacement, title);
        try actions.append(server.allocator, action);
    }

    return actions.toOwnedSlice(server.allocator);
}

fn buildRenameNoteAction(server: *Server, doc: index.Document) !?[]const u8 {
    const heading = firstHeading(doc.headings) orelse return null;
    var buf: [256]u8 = undefined;
    const base = slugifyGfmBaseInto(heading.text, &buf);
    if (base.len == 0) return null;

    const doc_path = uriToPath(server.allocator, doc.uri) orelse return null;
    defer server.allocator.free(doc_path);
    const doc_dir = std.fs.path.dirname(doc_path) orelse doc_path;

    const new_path = try std.fs.path.join(server.allocator, &.{ doc_dir, base });
    defer server.allocator.free(new_path);
    const new_path_ext = if (!hasMarkdownExtension(new_path))
        try std.fmt.allocPrint(server.allocator, "{s}.md", .{new_path})
    else
        try server.allocator.dupe(u8, new_path);
    defer server.allocator.free(new_path_ext);

    if (std.mem.eql(u8, new_path_ext, doc_path)) return null;
    if (pathIsFile(new_path_ext)) return null;

    const new_uri = try pathToUri(server.allocator, new_path_ext);
    defer server.allocator.free(new_uri);

    var edits_map = std.StringHashMap(EditBucket).init(server.allocator);
    defer {
        var it = edits_map.iterator();
        while (it.next()) |entry| {
            for (entry.value_ptr.edits.items) |edit| server.allocator.free(edit.new_text);
            entry.value_ptr.edits.deinit(server.allocator);
        }
        edits_map.deinit();
    }

    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        const link_doc = entry.value_ptr.*;
        for (link_doc.links) |link| {
            const loc = try resolveLink(server, link_doc, link) orelse continue;
            if (!std.mem.eql(u8, loc.uri, doc.uri)) continue;
            const replacement = try buildLinkReplacement(server, link_doc, link, new_uri);
            errdefer server.allocator.free(replacement);
            try appendEdit(&edits_map, server.allocator, entry.key_ptr.*, .{
                .range = link.range,
                .new_text = replacement,
            });
        }
    }

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(server.allocator);
    var writer = out.writer(server.allocator);
    const title = try std.fmt.allocPrint(server.allocator, "Rename note to {s}", .{heading.text});
    defer server.allocator.free(title);
    try writer.writeAll("{\"title\":");
    try protocol.writeJsonString(writer, title);
    try writer.writeAll(",\"kind\":\"refactor\",\"edit\":{\"documentChanges\":[");
    try writer.writeAll("{\"kind\":\"rename\",\"oldUri\":");
    try protocol.writeJsonString(writer, doc.uri);
    try writer.writeAll(",\"newUri\":");
    try protocol.writeJsonString(writer, new_uri);
    try writer.writeAll("}");

    var edit_it = edits_map.iterator();
    while (edit_it.next()) |entry| {
        try writer.writeAll(",{\"textDocument\":{\"uri\":");
        try protocol.writeJsonString(writer, entry.key_ptr.*);
        try writer.writeAll(",\"version\":null},\"edits\":[");
        for (entry.value_ptr.edits.items, 0..) |edit, idx| {
            if (idx > 0) try writer.writeByte(',');
            try writeTextEdit(writer, edit);
        }
        try writer.writeAll("]}");
    }

    try writer.writeAll("]}}");
    const action = try out.toOwnedSlice(server.allocator);
    return action;
}

fn buildExtractSelectionAction(
    server: *Server,
    doc: index.Document,
    range: protocol.Range,
) !?[]const u8 {
    if (range.start.line == range.end.line and range.start.character == range.end.character) return null;
    const selection = sliceForRange(doc.text, range);
    if (selection.len > max_action_selection_bytes) return null;
    if (std.mem.trim(u8, selection, " \t\r\n").len == 0) return null;

    const title = selectionTitle(selection);
    const base_name = try dateIdUtc(server.allocator);
    defer server.allocator.free(base_name);

    const doc_path = uriToPath(server.allocator, doc.uri) orelse return null;
    defer server.allocator.free(doc_path);
    const doc_dir = std.fs.path.dirname(doc_path) orelse doc_path;

    const path = try uniqueNotePath(server.allocator, doc_dir, base_name);
    defer server.allocator.free(path);
    const file_uri = try pathToUri(server.allocator, path);
    defer server.allocator.free(file_uri);

    const content = try std.fmt.allocPrint(server.allocator, "# {s}\n\n{s}\n", .{ title, selection });
    defer server.allocator.free(content);

    const rel = try relativePath(server.allocator, doc.uri, file_uri);
    defer server.allocator.free(rel);
    const link_text = try std.fmt.allocPrint(server.allocator, "[{s}]({s})", .{ title, rel });
    defer server.allocator.free(link_text);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(server.allocator);
    var writer = out.writer(server.allocator);
    try writer.writeAll("{\"title\":\"Extract selection to note\",\"kind\":\"refactor.extract\",\"edit\":{\"documentChanges\":[");
    try writer.writeAll("{\"kind\":\"create\",\"uri\":");
    try protocol.writeJsonString(writer, file_uri);
    try writer.writeAll("},");
    try writer.writeAll("{\"textDocument\":{\"uri\":");
    try protocol.writeJsonString(writer, file_uri);
    try writer.writeAll(",\"version\":null},\"edits\":[");
    try writeTextEdit(writer, .{
        .range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 0 },
        },
        .new_text = content,
    });
    try writer.writeAll("]},");
    try writer.writeAll("{\"textDocument\":{\"uri\":");
    try protocol.writeJsonString(writer, doc.uri);
    try writer.writeAll(",\"version\":null},\"edits\":[");
    try writeTextEdit(writer, .{ .range = range, .new_text = link_text });
    try writer.writeAll("]}]}}");
    const action = try out.toOwnedSlice(server.allocator);
    return action;
}

fn buildCsvToTableAction(
    server: *Server,
    doc: index.Document,
    range: protocol.Range,
) !?[]const u8 {
    if (range.start.line == range.end.line and range.start.character == range.end.character) return null;
    const selection = sliceForRange(doc.text, range);
    if (selection.len > max_action_selection_bytes) return null;
    if (std.mem.trim(u8, selection, " \t\r\n").len == 0) return null;
    if (std.mem.indexOfScalar(u8, selection, ',') == null) return null;

    const table = try parseCsvTable(server.allocator, selection) orelse return null;
    defer freeCsvTable(server.allocator, table);

    const markdown = try renderCsvTable(server.allocator, table);
    defer server.allocator.free(markdown);

    const action = try buildReplaceAction(
        server.allocator,
        doc.uri,
        range,
        markdown,
        "Convert CSV to table",
    );
    return action;
}

fn buildInsertNewNoteLinkAction(
    server: *Server,
    doc: index.Document,
    range: protocol.Range,
) !?[]const u8 {
    const doc_path = uriToPath(server.allocator, doc.uri) orelse return null;
    defer server.allocator.free(doc_path);
    const doc_dir = std.fs.path.dirname(doc_path) orelse doc_path;

    const base_name = try dateIdUtc(server.allocator);
    defer server.allocator.free(base_name);

    const path = try uniqueNotePath(server.allocator, doc_dir, base_name);
    defer server.allocator.free(path);
    const file_uri = try pathToUri(server.allocator, path);
    defer server.allocator.free(file_uri);

    const link_text = if (range.start.line == range.end.line and range.start.character == range.end.character)
        base_name
    else
        selectionTitle(sliceForRange(doc.text, range));

    const content = try std.fmt.allocPrint(server.allocator, "# {s}\n", .{link_text});
    defer server.allocator.free(content);

    const rel = try relativePath(server.allocator, doc.uri, file_uri);
    defer server.allocator.free(rel);
    const link_markup = try std.fmt.allocPrint(server.allocator, "[{s}]({s})", .{ link_text, rel });
    defer server.allocator.free(link_markup);

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(server.allocator);
    var writer = out.writer(server.allocator);
    try writer.writeAll("{\"title\":\"Insert new note link\",\"kind\":\"refactor\",\"edit\":{\"documentChanges\":[");
    try writer.writeAll("{\"kind\":\"create\",\"uri\":");
    try protocol.writeJsonString(writer, file_uri);
    try writer.writeAll("},");
    try writer.writeAll("{\"textDocument\":{\"uri\":");
    try protocol.writeJsonString(writer, file_uri);
    try writer.writeAll(",\"version\":null},\"edits\":[");
    try writeTextEdit(writer, .{
        .range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 0 },
        },
        .new_text = content,
    });
    try writer.writeAll("]},");
    try writer.writeAll("{\"textDocument\":{\"uri\":");
    try protocol.writeJsonString(writer, doc.uri);
    try writer.writeAll(",\"version\":null},\"edits\":[");
    try writeTextEdit(writer, .{ .range = range, .new_text = link_markup });
    try writer.writeAll("]}]}}");
    const action = try out.toOwnedSlice(server.allocator);
    return action;
}

fn buildInsertFootnoteAction(
    server: *Server,
    doc: index.Document,
    range: protocol.Range,
) !?[]const u8 {
    const selection = sliceForRange(doc.text, range);
    const trimmed = std.mem.trim(u8, selection, " \t\r\n");
    const slug_source = if (trimmed.len > 64) trimmed[0..64] else trimmed;
    var id_owned: ?[]u8 = null;
    defer if (id_owned) |buf| server.allocator.free(buf);

    var id: []const u8 = "";
    if (slug_source.len > 0) {
        var buf: [64]u8 = undefined;
        const slug = slugifyInto(slug_source, &buf);
        if (slug.len > 0) {
            id_owned = try server.allocator.dupe(u8, slug);
            id = id_owned.?;
        }
    }
    if (id.len == 0) {
        id_owned = try dateIdUtc(server.allocator);
        id = id_owned.?;
    }

    const anchor = try std.fmt.allocPrint(server.allocator, "[^{s}]", .{id});
    defer server.allocator.free(anchor);

    const def = try std.fmt.allocPrint(server.allocator, "\n[^{s}]: ", .{id});
    defer server.allocator.free(def);

    const end_pos = documentEndPosition(doc.text);

    var edits: std.ArrayList(TextEdit) = .empty;
    defer edits.deinit(server.allocator);
    try edits.append(server.allocator, .{ .range = range, .new_text = anchor });
    try edits.append(server.allocator, .{
        .range = .{ .start = end_pos, .end = end_pos },
        .new_text = def,
    });

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(server.allocator);
    var writer = out.writer(server.allocator);
    try writer.writeAll("{\"title\":\"Insert footnote\",\"kind\":\"quickfix\",\"edit\":{\"changes\":{");
    try protocol.writeJsonString(writer, doc.uri);
    try writer.writeAll(":[");
    for (edits.items, 0..) |edit, idx| {
        if (idx > 0) try writer.writeByte(',');
        try writeTextEdit(writer, edit);
    }
    try writer.writeAll("]}}}");
    const action = try out.toOwnedSlice(server.allocator);
    return action;
}

fn buildRelatedSectionAction(
    server: *Server,
    doc: index.Document,
) !?[]const u8 {
    if (hasHeading(doc.headings, "Related")) return null;
    try server.workspace.indexRootsIncremental();

    const tags = try collectTags(server.allocator, doc);
    defer freeTagList(server.allocator, tags);

    var candidates: std.ArrayListUnmanaged(struct {
        uri: []const u8,
        title: []const u8,
        score: usize,
    }) = .empty;
    defer {
        for (candidates.items) |item| server.allocator.free(item.title);
        candidates.deinit(server.allocator);
    }

    var it = server.workspace.docs.iterator();
    while (it.next()) |entry| {
        if (std.mem.eql(u8, entry.key_ptr.*, doc.uri)) continue;
        const candidate_doc = entry.value_ptr.*;
        const score = relatedScore(server.allocator, tags, candidate_doc);
        if (score == 0) continue;
        const title = try documentTitle(server.allocator, candidate_doc, entry.key_ptr.*);
        try candidates.append(server.allocator, .{
            .uri = entry.key_ptr.*,
            .title = title,
            .score = score,
        });
    }

    if (candidates.items.len == 0) return null;
    std.sort.heap(@TypeOf(candidates.items[0]), candidates.items, {}, struct {
        fn lessThan(_: void, a: @TypeOf(candidates.items[0]), b: @TypeOf(candidates.items[0])) bool {
            if (a.score == b.score) return std.mem.lessThan(u8, a.title, b.title);
            return a.score > b.score;
        }
    }.lessThan);

    const limit = @min(candidates.items.len, 5);
    var section: std.ArrayList(u8) = .empty;
    errdefer section.deinit(server.allocator);
    var writer = section.writer(server.allocator);
    try writer.writeAll("\n## Related\n");
    var idx: usize = 0;
    while (idx < limit) : (idx += 1) {
        const item = candidates.items[idx];
        const rel = try relativePath(server.allocator, doc.uri, item.uri);
        defer server.allocator.free(rel);
        try writer.print("- [{s}]({s})\n", .{ item.title, rel });
    }

    const end_pos = documentEndPosition(doc.text);
    const action = try buildReplaceAction(
        server.allocator,
        doc.uri,
        .{ .start = end_pos, .end = end_pos },
        section.items,
        "Insert related section",
    );
    return action;
}

fn buildReplaceAction(
    allocator: std.mem.Allocator,
    uri: []const u8,
    range: protocol.Range,
    new_text: []const u8,
    title: []const u8,
) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    const writer = out.writer(allocator);
    try writer.writeAll("{\"title\":");
    try protocol.writeJsonString(writer, title);
    try writer.writeAll(",\"kind\":\"quickfix\",\"edit\":{\"changes\":{");
    try protocol.writeJsonString(writer, uri);
    try writer.writeAll(":[");
    try writeTextEdit(writer, .{ .range = range, .new_text = new_text });
    try writer.writeAll("]}}}");
    return out.toOwnedSlice(allocator);
}

fn normalizeTargetKey(path: []const u8) []const u8 {
    const base = std.fs.path.basename(path);
    return stripExtension(base);
}

fn uriBaseName(allocator: std.mem.Allocator, uri: []const u8) ?[]u8 {
    const path = uriToPath(allocator, uri) orelse return null;
    defer allocator.free(path);
    const base = stripExtension(std.fs.path.basename(path));
    return allocator.dupe(u8, base) catch null;
}

fn candidateScore(target: []const u8, candidate: []const u8) usize {
    if (std.mem.eql(u8, target, candidate)) return 100;
    if (std.mem.containsAtLeast(u8, candidate, 1, target)) return 50;
    if (search.isSubsequence(target, candidate)) return 10;
    return 0;
}

fn lowerAlloc(allocator: std.mem.Allocator, text: []const u8) ![]u8 {
    const out = try allocator.alloc(u8, text.len);
    for (text, 0..) |ch, i| {
        out[i] = lowerAscii(ch);
    }
    return out;
}

fn buildLinkReplacement(
    server: *Server,
    doc: index.Document,
    link: parser.Link,
    target_uri: []const u8,
) ![]u8 {
    const base = basenameFromUri(server.allocator, target_uri) orelse return error.MissingTarget;
    defer server.allocator.free(base);

    if (link.kind == .wiki) {
        return std.fmt.allocPrint(server.allocator, "[[{s}]]", .{base});
    }

    const label = link.target.label orelse base;
    const rel = try relativePath(server.allocator, doc.uri, target_uri);
    defer server.allocator.free(rel);
    return std.fmt.allocPrint(server.allocator, "[{s}]({s})", .{ label, rel });
}

fn basenameFromUri(allocator: std.mem.Allocator, uri: []const u8) ?[]u8 {
    return uriBaseName(allocator, uri);
}

fn relativePath(allocator: std.mem.Allocator, from_uri: []const u8, to_uri: []const u8) ![]u8 {
    const from_path = uriToPath(allocator, from_uri) orelse return error.InvalidUri;
    defer allocator.free(from_path);
    const to_path = uriToPath(allocator, to_uri) orelse return error.InvalidUri;
    defer allocator.free(to_path);
    const from_dir = std.fs.path.dirname(from_path) orelse from_path;
    return std.fs.path.relative(allocator, from_dir, to_path);
}

fn appendEdit(
    map: *std.StringHashMap(EditBucket),
    allocator: std.mem.Allocator,
    uri: []const u8,
    edit: TextEdit,
) !void {
    if (map.getEntry(uri)) |entry| {
        try entry.value_ptr.edits.append(allocator, edit);
        return;
    }
    const key = try allocator.dupe(u8, uri);
    var bucket = EditBucket{ .uri = key };
    try bucket.edits.append(allocator, edit);
    try map.put(key, bucket);
}

fn firstHeading(headings: []const parser.Heading) ?parser.Heading {
    for (headings) |heading| {
        if (heading.level == 1) return heading;
    }
    return null;
}

fn uniqueNotePath(allocator: std.mem.Allocator, base_dir: []const u8, base: []const u8) ![]u8 {
    var attempt: usize = 0;
    while (true) : (attempt += 1) {
        const suffix = if (attempt == 0)
            try std.fmt.allocPrint(allocator, "{s}.md", .{base})
        else
            try std.fmt.allocPrint(allocator, "{s}-{d}.md", .{ base, attempt });
        defer allocator.free(suffix);
        const full = try std.fs.path.join(allocator, &.{ base_dir, suffix });
        if (!pathIsFile(full)) return full;
        allocator.free(full);
    }
}

fn selectionTitle(text: []const u8) []const u8 {
    var lines = std.mem.splitScalar(u8, text, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len > 0) return trimmed;
    }
    return "Note";
}

const CsvRow = struct {
    fields: []const []const u8,
};

const CsvTable = struct {
    rows: []const CsvRow,
};

fn parseCsvTable(allocator: std.mem.Allocator, text: []const u8) !?CsvTable {
    var rows: std.ArrayListUnmanaged(CsvRow) = .empty;
    errdefer {
        for (rows.items) |row| {
            for (row.fields) |field| allocator.free(field);
            allocator.free(row.fields);
        }
        rows.deinit(allocator);
    }

    var line_iter = std.mem.splitScalar(u8, text, '\n');
    while (line_iter.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;
        const line_no_cr = std.mem.trimRight(u8, line, "\r");
        const fields = try parseCsvLine(allocator, line_no_cr);
        try rows.append(allocator, .{ .fields = fields });
    }

    if (rows.items.len == 0) return null;
    return CsvTable{ .rows = try rows.toOwnedSlice(allocator) };
}

fn parseCsvLine(allocator: std.mem.Allocator, line: []const u8) ![]const []const u8 {
    var fields: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer {
        for (fields.items) |field| allocator.free(field);
        fields.deinit(allocator);
    }

    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(allocator);

    var in_quotes = false;
    var quoted_field = false;
    var i: usize = 0;
    while (i <= line.len) : (i += 1) {
        const at_end = i == line.len;
        const ch = if (at_end) 0 else line[i];
        if (at_end or (!in_quotes and ch == ',')) {
            const raw = buf.items;
            const field_text = if (quoted_field)
                raw
            else
                std.mem.trim(u8, raw, " \t");
            const field = try allocator.dupe(u8, field_text);
            try fields.append(allocator, field);
            buf.clearRetainingCapacity();
            quoted_field = false;
            continue;
        }

        if (ch == '"' and !in_quotes) {
            in_quotes = true;
            quoted_field = true;
            continue;
        }

        if (ch == '"' and in_quotes) {
            if (i + 1 < line.len and line[i + 1] == '"') {
                try buf.append(allocator, '"');
                i += 1;
            } else {
                in_quotes = false;
            }
            continue;
        }

        try buf.append(allocator, ch);
    }

    return fields.toOwnedSlice(allocator);
}

fn freeCsvTable(allocator: std.mem.Allocator, table: CsvTable) void {
    for (table.rows) |row| {
        for (row.fields) |field| allocator.free(field);
        allocator.free(row.fields);
    }
    allocator.free(table.rows);
}

fn renderCsvTable(allocator: std.mem.Allocator, table: CsvTable) ![]u8 {
    var max_cols: usize = 0;
    for (table.rows) |row| {
        if (row.fields.len > max_cols) max_cols = row.fields.len;
    }
    if (max_cols == 0) return allocator.dupe(u8, "");

    var widths = try allocator.alloc(usize, max_cols);
    defer allocator.free(widths);
    @memset(widths, 0);

    for (table.rows) |row| {
        var col: usize = 0;
        while (col < max_cols) : (col += 1) {
            const text = if (col < row.fields.len) row.fields[col] else "";
            const width = cellDisplayWidth(text);
            if (width > widths[col]) widths[col] = width;
        }
    }

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    const writer = out.writer(allocator);

    try writeTableRow(allocator, writer, table.rows[0], widths);
    try writeTableSeparator(writer, widths);
    var idx: usize = 1;
    while (idx < table.rows.len) : (idx += 1) {
        try writeTableRow(allocator, writer, table.rows[idx], widths);
    }

    return out.toOwnedSlice(allocator);
}

fn writeTableRow(
    allocator: std.mem.Allocator,
    writer: anytype,
    row: CsvRow,
    widths: []const usize,
) !void {
    try writer.writeByte('|');
    var col: usize = 0;
    while (col < widths.len) : (col += 1) {
        const text = if (col < row.fields.len) row.fields[col] else "";
        const width = cellDisplayWidth(text);
        try writer.writeByte(' ');
        try writeEscapedCell(allocator, writer, text);
        if (width < widths[col]) {
            try writeSpaces(writer, widths[col] - width);
        }
        try writer.writeAll(" |");
    }
    try writer.writeByte('\n');
}

fn writeTableSeparator(writer: anytype, widths: []const usize) !void {
    try writer.writeByte('|');
    var col: usize = 0;
    while (col < widths.len) : (col += 1) {
        const dash_count = if (widths[col] < 3) 3 else widths[col];
        try writer.writeByte(' ');
        try writeDashes(writer, dash_count);
        try writer.writeAll(" |");
    }
    try writer.writeByte('\n');
}

fn writeEscapedCell(allocator: std.mem.Allocator, writer: anytype, text: []const u8) !void {
    _ = allocator;
    for (text) |ch| {
        switch (ch) {
            '|' => try writer.writeAll("\\|"),
            '\n' => try writer.writeByte(' '),
            '\r' => {},
            else => try writer.writeByte(ch),
        }
    }
}

fn cellDisplayWidth(text: []const u8) usize {
    var width: usize = 0;
    for (text) |ch| {
        switch (ch) {
            '|' => width += 2,
            '\r' => {},
            '\n' => width += 1,
            else => width += 1,
        }
    }
    return width;
}

fn writeSpaces(writer: anytype, count: usize) !void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeByte(' ');
    }
}

fn writeDashes(writer: anytype, count: usize) !void {
    var i: usize = 0;
    while (i < count) : (i += 1) {
        try writer.writeByte('-');
    }
}

fn documentEndPosition(text: []const u8) protocol.Position {
    var line: usize = 0;
    var col: usize = 0;
    for (text) |ch| {
        if (ch == '\n') {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .character = col };
}

fn hasHeading(headings: []const parser.Heading, title: []const u8) bool {
    for (headings) |heading| {
        if (std.ascii.eqlIgnoreCase(heading.text, title)) return true;
    }
    return false;
}

fn collectTags(allocator: std.mem.Allocator, doc: index.Document) ![]const []const u8 {
    var tags: std.ArrayListUnmanaged([]const u8) = .empty;
    errdefer {
        for (tags.items) |tag| allocator.free(tag);
        tags.deinit(allocator);
    }
    for (doc.symbols) |sym| {
        const prefix = if (std.mem.startsWith(u8, sym.name, "Tag: "))
            "Tag: "
        else if (std.mem.startsWith(u8, sym.name, "ProjectTag: "))
            "ProjectTag: "
        else
            continue;
        const raw = sym.name[prefix.len..];
        const lower = try lowerAlloc(allocator, raw);
        try tags.append(allocator, lower);
    }
    return tags.toOwnedSlice(allocator);
}

fn freeTagList(allocator: std.mem.Allocator, tags: []const []const u8) void {
    for (tags) |tag| allocator.free(tag);
    allocator.free(tags);
}

fn relatedScore(allocator: std.mem.Allocator, tags: []const []const u8, doc: index.Document) usize {
    var score: usize = 0;
    if (tags.len > 0) {
        var tag_set = std.StringHashMap(void).init(allocator);
        defer tag_set.deinit();
        for (tags) |tag| {
            _ = tag_set.put(tag, {}) catch {};
        }
        for (doc.symbols) |sym| {
            const prefix = if (std.mem.startsWith(u8, sym.name, "Tag: "))
                "Tag: "
            else if (std.mem.startsWith(u8, sym.name, "ProjectTag: "))
                "ProjectTag: "
            else
                continue;
            const raw = sym.name[prefix.len..];
            const lower = lowerAlloc(allocator, raw) catch continue;
            defer allocator.free(lower);
            if (tag_set.contains(lower)) score += 2;
        }
    }
    return score;
}

fn documentTitle(allocator: std.mem.Allocator, doc: index.Document, uri: []const u8) ![]u8 {
    if (firstHeading(doc.headings)) |heading| {
        return allocator.dupe(u8, heading.text);
    }
    const base = basenameFromUri(allocator, uri) orelse return error.MissingTarget;
    return base;
}

fn dateIdUtc(allocator: std.mem.Allocator) ![]u8 {
    const ts = std.time.timestamp();
    const secs: u64 = if (ts < 0) 0 else @intCast(ts);
    const epoch_seconds = std.time.epoch.EpochSeconds{ .secs = secs };
    const day = epoch_seconds.getEpochDay().calculateYearDay();
    const month_day = day.calculateMonthDay();
    const day_seconds = epoch_seconds.getDaySeconds();

    const year: u32 = @intCast(day.year);
    const month: u32 = @intCast(@intFromEnum(month_day.month) + 1);
    const dom: u32 = @intCast(month_day.day_index + 1);
    const hour: u32 = @intCast(day_seconds.getHoursIntoDay());
    const minute: u32 = @intCast(day_seconds.getMinutesIntoHour());
    const second: u32 = @intCast(day_seconds.getSecondsIntoMinute());

    return std.fmt.allocPrint(
        allocator,
        "{d:0>4}-{d:0>2}-{d:0>2}-{d:0>2}{d:0>2}{d:0>2}",
        .{ year, month, dom, hour, minute, second },
    );
}

fn pathToUri(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "file://{s}", .{path});
}

const DecodedText = struct {
    text: []const u8,
    owned: bool,
};

fn decodePercentOrCopy(allocator: std.mem.Allocator, text: []const u8) DecodedText {
    return decodePercent(allocator, text) catch .{ .text = text, .owned = false };
}

fn decodePercent(allocator: std.mem.Allocator, text: []const u8) !DecodedText {
    if (std.mem.indexOfScalar(u8, text, '%') == null) {
        return .{ .text = text, .owned = false };
    }

    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    var i: usize = 0;
    while (i < text.len) {
        if (text[i] == '%' and i + 2 < text.len) {
            if (hexValue(text[i + 1])) |hi| {
                if (hexValue(text[i + 2])) |lo| {
                    try out.append(allocator, (hi << 4) | lo);
                    i += 3;
                    continue;
                }
            }
        }
        try out.append(allocator, text[i]);
        i += 1;
    }

    return .{ .text = try out.toOwnedSlice(allocator), .owned = true };
}

fn hexValue(ch: u8) ?u8 {
    if (ch >= '0' and ch <= '9') return ch - '0';
    if (ch >= 'a' and ch <= 'f') return ch - 'a' + 10;
    if (ch >= 'A' and ch <= 'F') return ch - 'A' + 10;
    return null;
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
    var root_sym = DocSymbol{
        .name = "H1: Title",
        .kind = .String,
        .range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 8 },
        },
        .selection_range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 8 },
        },
    };
    try root_sym.children.append(allocator, .{
        .name = "List: -",
        .kind = .String,
        .range = .{
            .start = .{ .line = 2, .character = 0 },
            .end = .{ .line = 3, .character = 6 },
        },
        .selection_range = .{
            .start = .{ .line = 2, .character = 0 },
            .end = .{ .line = 3, .character = 6 },
        },
    });
    defer root_sym.children.deinit(allocator);
    const symbols = [_]DocSymbol{root_sym};

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try sendDocumentSymbolResult(&out.writer, root, &symbols);
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

test "code action creates missing note" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/doc.md", "[Missing](missing.md)\n");
    const doc = server.workspace.getDocument("file:///root/doc.md").?;

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/doc.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    const range = try rangeValue(allocator, doc.links[0].range);
    try params.put("range", range);

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 10 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    try std.testing.expect(result_val.array.items.len > 0);
}

test "snapshot: code action create note" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/doc.md", "[Missing](missing.md)\n");
    const doc = server.workspace.getDocument("file:///root/doc.md").?;

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/doc.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    const range = try rangeValue(allocator, doc.links[0].range);
    try params.put("range", range);

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 20 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":20,"result":[{"title":"Create note: Missing","kind":"quickfix","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/missing.md"},{"textDocument":{"uri":"file:///root/missing.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# Missing\n"}]}]}}]}
    ).diff(payload);
}

test "code action fixes broken link" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/doc.md", "[Note](missing.md)\n");
    try server.workspace.upsertDocument("file:///root/notes.md", "# Notes\n");
    const doc = server.workspace.getDocument("file:///root/doc.md").?;

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/doc.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    const range = try rangeValue(allocator, doc.links[0].range);
    try params.put("range", range);

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 11 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    try std.testing.expect(result_val.array.items.len > 0);
}

test "snapshot: code action fix broken link" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/doc.md", "[Note](missing.md)\n");
    try server.workspace.upsertDocument("file:///root/notes.md", "# Notes\n");
    const doc = server.workspace.getDocument("file:///root/doc.md").?;

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/doc.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    const range = try rangeValue(allocator, doc.links[0].range);
    try params.put("range", range);

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 21 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":21,"result":[{"title":"Fix link -> notes","kind":"quickfix","edit":{"changes":{"file:///root/doc.md":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":18}},"newText":"[Note](notes.md)"}]}}}]}
    ).diff(payload);
}

test "code action renames note and updates links" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "# Alpha\n");
    try server.workspace.upsertDocument("file:///root/b.md", "[link](a.md)\n");
    const doc = server.workspace.getDocument("file:///root/a.md").?;

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    const range = try rangeValue(allocator, doc.headings[0].range);
    try params.put("range", range);

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 12 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    try std.testing.expect(result_val.array.items.len > 0);
}

test "snapshot: code action rename note" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "# Alpha\n");
    try server.workspace.upsertDocument("file:///root/b.md", "[link](a.md)\n");
    const doc = server.workspace.getDocument("file:///root/a.md").?;

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    const range = try rangeValue(allocator, doc.headings[0].range);
    try params.put("range", range);

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 22 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":22,"result":[{"title":"Rename note to Alpha","kind":"refactor","edit":{"documentChanges":[{"kind":"rename","oldUri":"file:///root/a.md","newUri":"file:///root/alpha.md"},{"textDocument":{"uri":"file:///root/b.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":12}},"newText":"[link](alpha.md)"}]}]}}]}
    ).diff(payload);
}

test "code action extracts selection" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "Line one\nLine two\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 8 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 13 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    try std.testing.expect(result_val.array.items.len > 0);
}

test "code action inserts new note link" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "Line one\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 0 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 24 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    try std.testing.expect(result_val.array.items.len > 0);
}

test "code action inserts related section" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "---\ntags: [one, two]\n---\n");
    try server.workspace.upsertDocument("file:///root/b.md", "---\ntags: [two]\n---\n# B\n");
    try server.workspace.upsertDocument("file:///root/c.md", "---\ntags: [three]\n---\n# C\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 0 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 26 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    var found = false;
    for (result_val.array.items) |item| {
        if (item != .object) continue;
        const title_val = item.object.get("title") orelse continue;
        if (title_val != .string) continue;
        if (std.mem.eql(u8, title_val.string, "Insert related section")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "code action handles long bullet selection" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    const text =
        \\- karmarkdown
        \\    + add action selection and link 
        \\    + insert link with fuzzy search
        \\    - test the fuzz link 
        \\    - update karmarkdown version on my nvim
        \\
        \\- review matklad k-way-merge implementation
        \\    - code
        \\    - video
        \\    - maybe fix it?
        \\
        \\- hashtable design 
        \\    - interface 
        \\    - tests 
        \\    - fuzzer pass 
        \\    - faster batch ht implementation.
        \\    - performance
    ;

    try server.workspace.upsertDocument("file:///root/a.md", text);

    const end_pos = documentEndPosition(text);
    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = end_pos,
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 31 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    try std.testing.expect(result_val == .array);
}

test "code action skips large selection extract" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    const size = max_action_selection_bytes + 1024;
    const text = try allocator.alloc(u8, size);
    defer allocator.free(text);
    std.mem.set(u8, text, 'a');

    try server.workspace.upsertDocument("file:///root/a.md", text);

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = @intCast(size) },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 30 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();
    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;

    for (result_val.array.items) |item| {
        if (item != .object) continue;
        const title_val = item.object.get("title") orelse continue;
        if (title_val != .string) continue;
        try std.testing.expect(!std.mem.eql(u8, title_val.string, "Extract selection to note"));
        try std.testing.expect(!std.mem.eql(u8, title_val.string, "Convert CSV to table"));
    }
}

test "snapshot: code action insert new note link" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "Line one\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 0 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 25 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":25,"result":[{"title":"Insert new note link","kind":"refactor","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/<snap:ignore>.md"},{"textDocument":{"uri":"file:///root/<snap:ignore>.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# <snap:ignore>\n"}]},{"textDocument":{"uri":"file:///root/a.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"[<snap:ignore>](<snap:ignore>.md)"}]}]}}]}
    ).diff(payload);
}

test "snapshot: code action related section" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "---\ntags: [one, two]\n---\n");
    try server.workspace.upsertDocument("file:///root/b.md", "---\ntags: [two]\n---\n# B\n");
    try server.workspace.upsertDocument("file:///root/c.md", "---\ntags: [three]\n---\n# C\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 0 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 27 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":27,"result":[{"title":"Insert new note link","kind":"refactor","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/<snap:ignore>.md"},{"textDocument":{"uri":"file:///root/<snap:ignore>.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# <snap:ignore>\n"}]},{"textDocument":{"uri":"file:///root/a.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"[<snap:ignore>](<snap:ignore>.md)"}]}]}},{"title":"Insert related section","kind":"quickfix","edit":{"changes":{"file:///root/a.md":[{"range":{"start":{"line":1,"character":0},"end":{"line":1,"character":0}},"newText":"\n## Related\n- [B](b.md)\n"}]}}}]}
    ).diff(payload);
}

test "snapshot: code action extract selection" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "Line one\nLine two\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 8 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 23 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":23,"result":[{"title":"Extract selection to note","kind":"refactor.extract","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/<snap:ignore>.md"},{"textDocument":{"uri":"file:///root/<snap:ignore>.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# Line one\n\nLine one\n"}]},{"textDocument":{"uri":"file:///root/a.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":8}},"newText":"[Line one](<snap:ignore>.md)"}]}]}},{"title":"Insert new note link","kind":"refactor","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/<snap:ignore>.md"},{"textDocument":{"uri":"file:///root/<snap:ignore>.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# Line one\n"}]},{"textDocument":{"uri":"file:///root/a.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":8}},"newText":"[Line one](<snap:ignore>.md)"}]}]}}]}
    ).diff(payload);
}

test "snapshot: code action csv to table" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "Name,Age\nAlice,30\nBob,40\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 2, .character = 6 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 28 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":28,"result":[{"title":"Extract selection to note","kind":"refactor.extract","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/<snap:ignore>.md"},{"textDocument":{"uri":"file:///root/<snap:ignore>.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# Name,Age\n\nName,Age\nAlice,30\nBob,40\n"}]},{"textDocument":{"uri":"file:///root/a.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":2,"character":6}},"newText":"[Name,Age](<snap:ignore>.md)"}]}]}},{"title":"Convert CSV to table","kind":"quickfix","edit":{"changes":{"file:///root/a.md":[{"range":{"start":{"line":0,"character":0},"end":{"line":2,"character":6}},"newText":"| Name  | Age |\n| ----- | --- |\n| Alice | 30  |\n| Bob   | 40  |\n"}]}}},{"title":"Insert new note link","kind":"refactor","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/<snap:ignore>.md"},{"textDocument":{"uri":"file:///root/<snap:ignore>.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# Name,Age\n"}]},{"textDocument":{"uri":"file:///root/a.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":2,"character":6}},"newText":"[Name,Age](<snap:ignore>.md)"}]}]}}]}
    ).diff(payload);
}

test "snapshot: code action insert footnote" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "Line one\n");

    const range = protocol.Range{
        .start = .{ .line = 0, .character = 0 },
        .end = .{ .line = 0, .character = 0 },
    };

    var params = std.json.ObjectMap.init(allocator);
    var text_doc = std.json.ObjectMap.init(allocator);
    try text_doc.put("uri", std.json.Value{ .string = "file:///root/a.md" });
    try params.put("textDocument", std.json.Value{ .object = text_doc });
    try params.put("range", try rangeValue(allocator, range));

    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 29 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleCodeAction(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":29,"result":[{"title":"Insert new note link","kind":"refactor","edit":{"documentChanges":[{"kind":"create","uri":"file:///root/<snap:ignore>.md"},{"textDocument":{"uri":"file:///root/<snap:ignore>.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"# <snap:ignore>\n"}]},{"textDocument":{"uri":"file:///root/a.md","version":null},"edits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"[<snap:ignore>](<snap:ignore>.md)"}]}]}},{"title":"Insert footnote","kind":"quickfix","edit":{"changes":{"file:///root/a.md":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"[^<snap:ignore>]"},{"range":{"start":{"line":1,"character":0},"end":{"line":1,"character":0}},"newText":"\n[^<snap:ignore>]: "}]}}}]}
    ).diff(payload);
}

test "workspace symbol indexes new files on demand" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{ .sub_path = "old.md", .data = "# Old\n" });
    const root_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(root_path);
    try server.workspace.addRoot(root_path);
    try server.workspace.indexRoots();

    try tmp.dir.writeFile(.{ .sub_path = "new.md", .data = "# New\n" });

    var params = std.json.ObjectMap.init(allocator);
    try params.put("query", std.json.Value{ .string = "New" });
    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 3 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleWorkspaceSymbol(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    var found = false;
    for (result_val.array.items) |item| {
        if (item != .object) continue;
        const name_val = item.object.get("name") orelse continue;
        if (name_val != .string) continue;
        if (std.mem.containsAtLeast(u8, name_val.string, 1, "H1: New")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "workspace symbols include tags" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    try server.workspace.upsertDocument("file:///root/a.md", "---\ntags: [alpha, beta]\n---\n");

    var params = std.json.ObjectMap.init(allocator);
    try params.put("query", std.json.Value{ .string = "#alpha" });
    var root_obj = std.json.ObjectMap.init(allocator);
    try root_obj.put("id", std.json.Value{ .integer = 4 });
    try root_obj.put("params", std.json.Value{ .object = params });
    const root = std.json.Value{ .object = root_obj };
    defer deinitValue(allocator, root);

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try handleWorkspaceSymbol(&server, &out.writer, root);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    var parsed = try std.json.parseFromSlice(std.json.Value, allocator, payload, .{});
    defer parsed.deinit();

    const result_val = parsed.value.object.get("result") orelse return error.TestExpectedResult;
    if (result_val != .array) return error.TestExpectedArray;
    var found = false;
    for (result_val.array.items) |item| {
        if (item != .object) continue;
        const name_val = item.object.get("name") orelse continue;
        if (name_val != .string) continue;
        if (std.mem.eql(u8, name_val.string, "Tag: #alpha")) {
            found = true;
            break;
        }
    }
    try std.testing.expect(found);
}

test "snapshot: document symbol response" {
    const allocator = std.testing.allocator;
    var obj = std.json.ObjectMap.init(allocator);
    defer obj.deinit();
    try obj.put("id", std.json.Value{ .integer = 1 });

    const root = std.json.Value{ .object = obj };
    var root_sym = DocSymbol{
        .name = "H1: Title",
        .kind = .String,
        .range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 8 },
        },
        .selection_range = .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 8 },
        },
    };
    try root_sym.children.append(allocator, .{
        .name = "Code: zig",
        .kind = .String,
        .range = .{
            .start = .{ .line = 2, .character = 0 },
            .end = .{ .line = 4, .character = 3 },
        },
        .selection_range = .{
            .start = .{ .line = 2, .character = 0 },
            .end = .{ .line = 4, .character = 3 },
        },
    });
    defer root_sym.children.deinit(allocator);
    const symbols = [_]DocSymbol{root_sym};

    var out = std.Io.Writer.Allocating.init(allocator);
    defer out.deinit();
    try sendDocumentSymbolResult(&out.writer, root, &symbols);
    const message = try out.toOwnedSlice();
    defer allocator.free(message);

    const payload = extractPayload(message) orelse return error.TestExpectedPayload;
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\{"jsonrpc":"2.0","id":1,"result":[{"name":"H1: Title","kind":15,"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":8}},"selectionRange":{"start":{"line":0,"character":0},"end":{"line":0,"character":8}},"children":[{"name":"Code: zig","kind":15,"range":{"start":{"line":2,"character":0},"end":{"line":4,"character":3}},"selectionRange":{"start":{"line":2,"character":0},"end":{"line":4,"character":3}}}]}]}
    ).diff(payload);
}

test "snapshot: document symbol hierarchy" {
    const allocator = std.testing.allocator;
    var parser_instance = parser.Parser{};
    const text =
        \\# Title
        \\
        \\- [item](doc.md)
        \\  - child
        \\- [ ] todo
        \\
        \\```zig
        \\let x = 1;
        \\```
        \\
        \\## Sub
        \\
    ;
    var doc = try index.Document.init(allocator, "file:///doc.md", text, &parser_instance, true);
    defer doc.deinit(allocator);

    const symbols = buildDocumentSymbols(allocator, doc);
    defer freeDocSymbols(allocator, symbols);
    const rendered = try renderDocSymbols(allocator, symbols);
    defer allocator.free(rendered);

    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\H1: Title
        \\  List: [item](doc.md)
        \\    List: child
        \\  Task: [ ] todo
        \\  Link: [item](doc.md)
        \\  Code: zig
        \\  H2: Sub
    ).diff(rendered);
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

fn rangeValue(allocator: std.mem.Allocator, range: protocol.Range) !std.json.Value {
    var start = std.json.ObjectMap.init(allocator);
    try start.put("line", std.json.Value{ .integer = range.start.line });
    try start.put("character", std.json.Value{ .integer = range.start.character });

    var end = std.json.ObjectMap.init(allocator);
    try end.put("line", std.json.Value{ .integer = range.end.line });
    try end.put("character", std.json.Value{ .integer = range.end.character });

    var obj = std.json.ObjectMap.init(allocator);
    try obj.put("start", std.json.Value{ .object = start });
    try obj.put("end", std.json.Value{ .object = end });
    return std.json.Value{ .object = obj };
}

fn renderDocSymbols(allocator: std.mem.Allocator, symbols: []DocSymbol) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    var writer = out.writer(allocator);
    for (symbols) |sym| {
        try renderDocSymbolLine(&writer, sym, 0);
    }
    return out.toOwnedSlice(allocator);
}

fn renderDocSymbolLine(writer: anytype, sym: DocSymbol, depth: usize) !void {
    for (0..depth) |_| {
        try writer.writeAll("  ");
    }
    try writer.print("{s}\n", .{sym.name});
    for (sym.children.items) |child| {
        try renderDocSymbolLine(writer, child, depth + 1);
    }
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

test "definition resolves reference links across files" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/a.md",
        "[ref][label]\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/defs.md",
        "[label]: c.md#Heading\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/c.md",
        "## Heading\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/a.md").?;
    const loc = try resolveLink(&server, doc_a, doc_a.links[0]);
    try std.testing.expect(loc != null);
    try std.testing.expect(std.mem.eql(u8, loc.?.uri, "file:///root/c.md"));
}

test "definition resolves gfm-style heading ids" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    const text =
        \\# Hello, World!
        \\## Hello World
        \\## Hello World
        \\
        \\[one](#Hello, World!)
        \\[two](#hello-world-1)
        \\
    ;
    try server.workspace.upsertDocument("file:///doc.md", text);
    const doc = server.workspace.getDocument("file:///doc.md").?;

    const first = findLinkByAnchor(doc.links, "Hello, World!") orelse return error.TestExpectedLink;
    const second = findLinkByAnchor(doc.links, "hello-world-1") orelse return error.TestExpectedLink;

    const first_loc = try resolveLink(&server, doc, first);
    try std.testing.expect(first_loc != null);
    try std.testing.expectEqual(@as(usize, 0), first_loc.?.range.start.line);

    const second_loc = try resolveLink(&server, doc, second);
    try std.testing.expect(second_loc != null);
    try std.testing.expectEqual(@as(usize, 1), second_loc.?.range.start.line);
}

test "definition resolves percent-encoded paths" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{ .sub_path = "My File.md", .data = "# Target\n" });
    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "[Target](My%20File)\n" });

    const root_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(root_path);
    try server.workspace.addRoot(root_path);

    const doc_path = try std.fs.path.join(allocator, &.{ root_path, "doc.md" });
    defer allocator.free(doc_path);
    const doc_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{doc_path});
    defer allocator.free(doc_uri);

    const text = try std.fs.cwd().readFileAlloc(allocator, doc_path, 1024);
    defer allocator.free(text);
    try server.workspace.upsertDocument(doc_uri, text);

    const doc = server.workspace.getDocument(doc_uri).?;
    try std.testing.expect(doc.links.len > 0);

    const loc = try resolveLink(&server, doc, doc.links[0]);
    try std.testing.expect(loc != null);
    try std.testing.expect(std.mem.endsWith(u8, loc.?.uri, "My File.md"));
}

test "definition resolves relative paths with extension fallback" {
    const allocator = std.testing.allocator;
    var server = Server.init(allocator);
    defer server.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.makeDir("notes");
    try tmp.dir.writeFile(.{ .sub_path = "notes/target.markdown", .data = "# Target\n" });
    try tmp.dir.writeFile(.{ .sub_path = "doc.md", .data = "[Target](notes/target)\n" });

    const root_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(root_path);
    try server.workspace.addRoot(root_path);

    const doc_path = try std.fs.path.join(allocator, &.{ root_path, "doc.md" });
    defer allocator.free(doc_path);
    const doc_uri = try std.fmt.allocPrint(allocator, "file://{s}", .{doc_path});
    defer allocator.free(doc_uri);

    const text = try std.fs.cwd().readFileAlloc(allocator, doc_path, 1024);
    defer allocator.free(text);
    try server.workspace.upsertDocument(doc_uri, text);

    const doc = server.workspace.getDocument(doc_uri).?;
    try std.testing.expect(doc.links.len > 0);

    const loc = try resolveLink(&server, doc, doc.links[0]);
    try std.testing.expect(loc != null);
    try std.testing.expect(std.mem.endsWith(u8, loc.?.uri, "notes/target.markdown"));
}

test "completion suggests wiki, paths, and headings" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[[b\n[Link](b.md#he)\n[Title](He\n[Path](./zk/alloc\n# Heading One\n## Heading Two\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/b.md",
        "## Heading Two\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/allocator.md",
        "# Allocator\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/owner.md",
        "# Notes\n## Allocator Ownership\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| {
            std.testing.allocator.free(item.label);
            if (item.insert_text) |text| std.testing.allocator.free(text);
            if (item.filter_text) |text| std.testing.allocator.free(text);
        }
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc_a, .{ .line = 0, .character = 3 }, &items);
    sortCompletionItems(items.items);
    const snap = Snap.snap_fn(".");
    const rendered = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered);
    try snap(@src(),
        \\b
    ).diff(rendered);

    items.clearRetainingCapacity();
    try collectCompletions(&server, doc_a, .{ .line = 1, .character = 12 }, &items);
    sortCompletionItems(items.items);
    const rendered_anchor = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered_anchor);
    try snap(@src(),
        \\#heading-one
        \\#heading-two
    ).diff(rendered_anchor);

    items.clearRetainingCapacity();
    try collectCompletions(&server, doc_a, .{ .line = 1, .character = 8 }, &items);
    sortCompletionItems(items.items);
    const rendered_path = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered_path);
    try snap(@src(),
        \\b.md
    ).diff(rendered_path);

    items.clearRetainingCapacity();
    try collectCompletions(&server, doc_a, .{ .line = 2, .character = 10 }, &items);
    sortCompletionItems(items.items);
    const rendered_title_path = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered_title_path);
    try snap(@src(),
        \\Heading One - a.md
        \\Heading Two - b.md
    ).diff(rendered_title_path);

    items.clearRetainingCapacity();
    try collectCompletions(&server, doc_a, .{ .line = 3, .character = 14 }, &items);
    sortCompletionItems(items.items);
    const rendered_path_title = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered_path_title);
    try snap(@src(),
        \\Allocator - ./zk/allocator.md
        \\Allocator Ownership - ./zk/owner.md
        \\zk/allocator.md
    ).diff(rendered_path_title);
}

test "completion sets filterText for title path matches" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[Path](./zk/alloc\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/allocator.md",
        "# Allocator\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| {
            std.testing.allocator.free(item.label);
            if (item.insert_text) |text| std.testing.allocator.free(text);
            if (item.filter_text) |text| std.testing.allocator.free(text);
        }
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc_a, .{ .line = 0, .character = 14 }, &items);

    var found = false;
    for (items.items) |item| {
        if (!std.mem.eql(u8, item.label, "Allocator - ./zk/allocator.md")) continue;
        found = true;
        try std.testing.expect(item.filter_text != null);
        try std.testing.expect(std.mem.eql(u8, item.filter_text.?, "./zk/alloc"));
        try std.testing.expect(item.insert_text != null);
        try std.testing.expect(std.mem.eql(u8, item.insert_text.?, "./zk/allocator.md"));
    }
    try std.testing.expect(found);
}

test "completion matches multiple titles by query" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[Path](./zk/alloc\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/semantics.md",
        "# Allocator Semantics\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/group.md",
        "# Grouping Allocations\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| {
            std.testing.allocator.free(item.label);
            if (item.insert_text) |text| std.testing.allocator.free(text);
            if (item.filter_text) |text| std.testing.allocator.free(text);
        }
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc_a, .{ .line = 0, .character = 14 }, &items);
    sortCompletionItems(items.items);
    const rendered = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered);
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\Allocator Semantics - ./zk/semantics.md
        \\Grouping Allocations - ./zk/group.md
    ).diff(rendered);
}

test "completion matches titles by directory prefix only" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[Path](./zk/\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/semantics.md",
        "# Allocator Semantics\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/group.md",
        "# Grouping Allocations\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/other.md",
        "# Outside\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| {
            std.testing.allocator.free(item.label);
            if (item.insert_text) |text| std.testing.allocator.free(text);
            if (item.filter_text) |text| std.testing.allocator.free(text);
        }
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc_a, .{ .line = 0, .character = 12 }, &items);
    sortCompletionItems(items.items);
    const rendered = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered);
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\Allocator Semantics
        \\Grouping Allocations
        \\zk/group.md
        \\zk/semantics.md
    ).diff(rendered);
}

test "completion matches titles case-insensitively" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[Path](./zk/ALLOc\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/semantics.md",
        "# Allocator Semantics\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/group.md",
        "# Grouping Allocations\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| {
            std.testing.allocator.free(item.label);
            if (item.insert_text) |text| std.testing.allocator.free(text);
            if (item.filter_text) |text| std.testing.allocator.free(text);
        }
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc_a, .{ .line = 0, .character = 16 }, &items);
    sortCompletionItems(items.items);
    const rendered = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered);
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\Allocator Semantics - ./zk/semantics.md
        \\Grouping Allocations - ./zk/group.md
    ).diff(rendered);
}

test "completion matches subheadings when query matches" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "[Path](./zk/own\n",
    );
    try server.workspace.upsertDocument(
        "file:///root/dir/zk/owner.md",
        "# Notes\n## Allocator Ownership\n",
    );

    const doc_a = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| {
            std.testing.allocator.free(item.label);
            if (item.insert_text) |text| std.testing.allocator.free(text);
            if (item.filter_text) |text| std.testing.allocator.free(text);
        }
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc_a, .{ .line = 0, .character = 14 }, &items);
    sortCompletionItems(items.items);
    const rendered = try renderCompletions(std.testing.allocator, items.items);
    defer std.testing.allocator.free(rendered);
    const snap = Snap.snap_fn(".");
    try snap(@src(),
        \\Allocator Ownership - ./zk/owner.md
    ).diff(rendered);
}

test "completion suggests snippets" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/dir/a.md",
        "# Heading\n",
    );

    const doc = server.workspace.getDocument("file:///root/dir/a.md").?;

    var items: std.ArrayList(CompletionItem) = .empty;
    defer {
        for (items.items) |item| {
            std.testing.allocator.free(item.label);
            if (item.insert_text) |text| std.testing.allocator.free(text);
            if (item.filter_text) |text| std.testing.allocator.free(text);
        }
        items.deinit(std.testing.allocator);
    }

    try collectCompletions(&server, doc, .{ .line = 0, .character = 0 }, &items);
    var snippet_found = false;
    var code_snippet = false;
    var footnote_anchor = false;
    var footnote_def = false;
    for (items.items) |item| {
        if (!std.mem.startsWith(u8, item.label, "Snippet:")) continue;
        snippet_found = true;
        if (std.mem.eql(u8, item.label, "Snippet: Code block")) code_snippet = true;
        if (std.mem.eql(u8, item.label, "Snippet: Footnote anchor")) footnote_anchor = true;
        if (std.mem.eql(u8, item.label, "Snippet: Footnote definition")) footnote_def = true;
        try std.testing.expect(item.insert_text != null);
        try std.testing.expect(item.insert_text_format.? == 2);
    }
    try std.testing.expect(snippet_found);
    try std.testing.expect(code_snippet);
    try std.testing.expect(footnote_anchor);
    try std.testing.expect(footnote_def);
}

test "diagnostics report link issues" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    try server.workspace.upsertDocument(
        "file:///root/a.md",
        "[[missing]] [Link](b.md#missing) [Ref][label]",
    );
    try server.workspace.upsertDocument(
        "file:///root/b.md",
        "# Exists\n",
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
        \\Missing target file @ 0:0-0:11
        \\Missing heading anchor @ 0:12-0:33
        \\Undefined link reference @ 0:34-0:46
    ).diff(rendered);
}

test "diagnostics report duplicate heading ids" {
    var server = Server.init(std.testing.allocator);
    defer server.deinit();

    const text =
        \\# Title
        \\# Title
        \\
    ;
    try server.workspace.upsertDocument("file:///dup.md", text);
    const doc = server.workspace.getDocument("file:///dup.md").?;

    const diags = try collectDiagnostics(&server, doc);
    defer {
        for (diags) |diag| std.testing.allocator.free(diag.message);
        std.testing.allocator.free(diags);
    }

    try std.testing.expectEqual(@as(usize, 2), diags.len);
    try std.testing.expect(std.mem.startsWith(u8, diags[0].message, "Duplicate heading id:"));
    try std.testing.expect(std.mem.startsWith(u8, diags[1].message, "Duplicate heading id:"));
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

test "fuzz: document symbols JSON is valid" {
    const max_len = 2048;
    const max_items = 8;
    const root_uri = "file:///tmp/fuzz.md";

    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            var parser_instance = parser.Parser{};
            const len = @min(input.len, max_len);
            const buf = try std.testing.allocator.alloc(u8, len);
            defer std.testing.allocator.free(buf);
            std.mem.copyForwards(u8, buf, input[0..len]);

            const parsed_doc = try parser_instance.parse(std.testing.allocator, buf, true);
            defer {
                for (parsed_doc.symbols) |sym| std.testing.allocator.free(sym.name);
                std.testing.allocator.free(parsed_doc.symbols);
                std.testing.allocator.free(parsed_doc.headings);
                std.testing.allocator.free(parsed_doc.links);
                std.testing.allocator.free(parsed_doc.link_defs);
            }

            const doc = index.Document{
                .uri = root_uri,
                .text = buf,
                .symbols = parsed_doc.symbols,
                .headings = parsed_doc.headings,
                .links = parsed_doc.links,
                .link_defs = parsed_doc.link_defs,
            };

            const symbols = buildDocumentSymbols(std.testing.allocator, doc);
            defer freeDocSymbols(std.testing.allocator, symbols);

            const slice_len = @min(symbols.len, max_items);
            var obj = std.json.ObjectMap.init(std.testing.allocator);
            defer obj.deinit();
            try obj.put("id", std.json.Value{ .integer = 3 });
            const root = std.json.Value{ .object = obj };

            var out = std.Io.Writer.Allocating.init(std.testing.allocator);
            defer out.deinit();
            try sendDocumentSymbolResult(&out.writer, root, symbols[0..slice_len]);
            const msg = try out.toOwnedSlice();
            defer std.testing.allocator.free(msg);

            const payload = extractPayload(msg) orelse return error.TestExpectedPayload;
            var parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, payload, .{});
            defer parsed.deinit();
            if (parsed.value.object.get("result")) |result_val| {
                if (result_val != .array) return error.TestExpectedArray;
            }
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
                for (items.items) |item| {
                    std.testing.allocator.free(item.label);
                    if (item.insert_text) |text| std.testing.allocator.free(text);
                    if (item.filter_text) |text| std.testing.allocator.free(text);
                }
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

test "fuzz: link resolution stays in bounds" {
    const max_len = 2048;
    const root_uri = "file:///tmp/fuzz.md";

    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            var server = Server.init(std.testing.allocator);
            defer server.deinit();

            const len = @min(input.len, max_len);
            const buf = try std.testing.allocator.alloc(u8, len);
            defer std.testing.allocator.free(buf);
            std.mem.copyForwards(u8, buf, input[0..len]);

            try server.workspace.upsertDocument(root_uri, buf);
            const doc = server.workspace.getDocument(root_uri).?;

            for (doc.links) |link| {
                const loc = try resolveLink(&server, doc, link);
                if (loc) |resolved| {
                    _ = server.workspace.getDocument(resolved.uri);
                }
            }
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
