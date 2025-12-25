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
                try handleDidOpen(&server, root);
                continue;
            }
            if (std.mem.eql(u8, m, "textDocument/didChange")) {
                try handleDidChange(&server, root);
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
    try out.writeAll("\"workspaceSymbolProvider\":true");
    try out.writeAll("}}}");

    try lsp.writeMessage(writer, payload.items);
}

fn handleInitialize(server: *Server, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;

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

fn handleDidOpen(server: *Server, root: std.json.Value) !void {
    const params = root.object.get("params") orelse return;
    const doc = params.object.get("textDocument") orelse return;
    const uri = doc.object.get("uri") orelse return;
    const text = doc.object.get("text") orelse return;

    if (uri != .string or text != .string) return;
    try server.workspace.upsertDocument(uri.string, text.string);
}

fn handleDidChange(server: *Server, root: std.json.Value) !void {
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
