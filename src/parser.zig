const std = @import("std");
const protocol = @import("protocol.zig");
const index = @import("index.zig");
const Snap = @import("snaptest.zig").Snap;

pub const Backend = enum {
    simple,
    tree_sitter,
};

pub const Heading = struct {
    level: u8,
    text: []const u8,
    range: protocol.Range,
};

pub const LinkKind = enum {
    wiki,
    inline_link,
    reference,
};

pub const LinkTarget = struct {
    path: ?[]const u8,
    anchor: ?[]const u8,
    label: ?[]const u8,
};

pub const Link = struct {
    kind: LinkKind,
    target: LinkTarget,
    range: protocol.Range,
};

pub const LinkDef = struct {
    label: []const u8,
    target: LinkTarget,
};

pub const ParsedDoc = struct {
    symbols: []index.Symbol,
    headings: []Heading,
    links: []Link,
    link_defs: []LinkDef,
};

pub const Parser = struct {
    backend: Backend = .simple,

    pub fn parse(
        self: *Parser,
        allocator: std.mem.Allocator,
        text: []const u8,
        enable_wiki: bool,
    ) !ParsedDoc {
        return switch (self.backend) {
            .simple => parseSimple(allocator, text, enable_wiki),
            .tree_sitter => error.TreeSitterUnavailable,
        };
    }
};

const TokenKind = enum {
    Hash,
    LBracket,
    RBracket,
    LParen,
    RParen,
    DoubleLBracket,
    DoubleRBracket,
    Text,
    Newline,
    Eof,
};

const Token = struct {
    kind: TokenKind,
    start: protocol.Position,
    end: protocol.Position,
    start_offset: usize,
    end_offset: usize,
    line_start: bool,
};

const CodeSpan = struct {
    start: usize,
    end: usize,
};

const Lexer = struct {
    input: []const u8,
    index: usize,
    line: usize,
    column: usize,
    line_start: bool,
    heading_prefix: bool,

    pub fn init(input: []const u8) Lexer {
        return .{
            .input = input,
            .index = 0,
            .line = 0,
            .column = 0,
            .line_start = true,
            .heading_prefix = false,
        };
    }

    pub fn nextToken(self: *Lexer) Token {
        if (self.index >= self.input.len) {
            const pos = protocol.Position{ .line = self.line, .character = self.column };
            return .{
                .kind = .Eof,
                .start = pos,
                .end = pos,
                .start_offset = self.index,
                .end_offset = self.index,
                .line_start = self.line_start,
            };
        }

        const start_index = self.index;
        const start_line = self.line;
        const start_col = self.column;
        const at_line_start = self.line_start;
        const ch = self.input[self.index];

        if (ch == '\n') {
            self.index += 1;
            self.line += 1;
            self.column = 0;
            self.line_start = true;
            self.heading_prefix = false;
            return .{
                .kind = .Newline,
                .start = .{ .line = start_line, .character = start_col },
                .end = .{ .line = self.line, .character = self.column },
                .start_offset = start_index,
                .end_offset = self.index,
                .line_start = at_line_start,
            };
        }

        self.line_start = false;

        if (ch == '#' and (at_line_start or self.heading_prefix)) {
            self.index += 1;
            self.column += 1;
            self.heading_prefix = true;
            return .{
                .kind = .Hash,
                .start = .{ .line = start_line, .character = start_col },
                .end = .{ .line = self.line, .character = self.column },
                .start_offset = start_index,
                .end_offset = self.index,
                .line_start = at_line_start,
            };
        }

        self.heading_prefix = false;

        if (ch == '[') {
            if (self.peekChar() == '[') {
                self.index += 2;
                self.column += 2;
                return .{
                    .kind = .DoubleLBracket,
                    .start = .{ .line = start_line, .character = start_col },
                    .end = .{ .line = self.line, .character = self.column },
                    .start_offset = start_index,
                    .end_offset = self.index,
                    .line_start = at_line_start,
                };
            }
            self.index += 1;
            self.column += 1;
            return .{
                .kind = .LBracket,
                .start = .{ .line = start_line, .character = start_col },
                .end = .{ .line = self.line, .character = self.column },
                .start_offset = start_index,
                .end_offset = self.index,
                .line_start = at_line_start,
            };
        }

        if (ch == ']') {
            if (self.peekChar() == ']') {
                self.index += 2;
                self.column += 2;
                return .{
                    .kind = .DoubleRBracket,
                    .start = .{ .line = start_line, .character = start_col },
                    .end = .{ .line = self.line, .character = self.column },
                    .start_offset = start_index,
                    .end_offset = self.index,
                    .line_start = at_line_start,
                };
            }
            self.index += 1;
            self.column += 1;
            return .{
                .kind = .RBracket,
                .start = .{ .line = start_line, .character = start_col },
                .end = .{ .line = self.line, .character = self.column },
                .start_offset = start_index,
                .end_offset = self.index,
                .line_start = at_line_start,
            };
        }

        if (ch == '(') {
            self.index += 1;
            self.column += 1;
            return .{
                .kind = .LParen,
                .start = .{ .line = start_line, .character = start_col },
                .end = .{ .line = self.line, .character = self.column },
                .start_offset = start_index,
                .end_offset = self.index,
                .line_start = at_line_start,
            };
        }

        if (ch == ')') {
            self.index += 1;
            self.column += 1;
            return .{
                .kind = .RParen,
                .start = .{ .line = start_line, .character = start_col },
                .end = .{ .line = self.line, .character = self.column },
                .start_offset = start_index,
                .end_offset = self.index,
                .line_start = at_line_start,
            };
        }

        var end_index = self.index + 1;
        var end_col = self.column + 1;
        while (end_index < self.input.len) : (end_index += 1) {
            const next_ch = self.input[end_index];
            if (next_ch == '\n' or next_ch == '[' or next_ch == ']' or next_ch == '(' or next_ch == ')') {
                break;
            }
            end_col += 1;
        }

        self.index = end_index;
        self.column = end_col;
        return .{
            .kind = .Text,
            .start = .{ .line = start_line, .character = start_col },
            .end = .{ .line = self.line, .character = self.column },
            .start_offset = start_index,
            .end_offset = end_index,
            .line_start = at_line_start,
        };
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.index + 1 >= self.input.len) return 0;
        return self.input[self.index + 1];
    }
};

const ParserState = struct {
    allocator: std.mem.Allocator,
    input: []const u8,
    tokens: []Token,
    index: usize,
    symbols: std.ArrayList(index.Symbol),
    code_spans: []CodeSpan,
    inline_spans: []CodeSpan,
    headings: std.ArrayList(Heading),
    links: std.ArrayList(Link),
    link_defs: std.ArrayList(LinkDef),
    enable_wiki: bool,
};

fn parseSimple(allocator: std.mem.Allocator, text: []const u8, enable_wiki: bool) !ParsedDoc {
    var tokens_list: std.ArrayList(Token) = .empty;
    defer tokens_list.deinit(allocator);

    var lexer = Lexer.init(text);
    while (true) {
        const tok = lexer.nextToken();
        try tokens_list.append(allocator, tok);
        if (tok.kind == .Eof) break;
    }

    var state = ParserState{
        .allocator = allocator,
        .input = text,
        .tokens = tokens_list.items,
        .index = 0,
        .symbols = .empty,
        .code_spans = &.{},
        .inline_spans = &.{},
        .headings = .empty,
        .links = .empty,
        .link_defs = .empty,
        .enable_wiki = enable_wiki,
    };
    errdefer {
        for (state.symbols.items) |sym| allocator.free(sym.name);
        state.symbols.deinit(allocator);
        state.headings.deinit(allocator);
        state.links.deinit(allocator);
        state.link_defs.deinit(allocator);
    }

    const code_spans = try parseBlocksFromLines(&state);
    defer allocator.free(code_spans);
    state.code_spans = code_spans;

    const inline_spans = try parseInlineCodeSpans(text, allocator);
    defer allocator.free(inline_spans);
    state.inline_spans = inline_spans;

    parseFile(&state);
    return .{
        .symbols = try state.symbols.toOwnedSlice(allocator),
        .headings = try state.headings.toOwnedSlice(allocator),
        .links = try state.links.toOwnedSlice(allocator),
        .link_defs = try state.link_defs.toOwnedSlice(allocator),
    };
}

fn parseFile(state: *ParserState) void {
    while (!at(state, .Eof)) {
        if (skipCodeSpan(state)) continue;
        if (at(state, .Newline)) {
            _ = bump(state);
            continue;
        }
        if (atHeadingStart(state)) {
            parseHeading(state);
            continue;
        }
        if (atMarkerStart(state)) {
            parseMarker(state);
            continue;
        }
        if (atLinkStart(state)) {
            parseLink(state);
            continue;
        }
        advanceWithError(state);
    }
}

fn atHeadingStart(state: *ParserState) bool {
    if (!at(state, .Hash)) return false;
    return current(state).line_start;
}

fn atLinkStart(state: *ParserState) bool {
    if (at(state, .DoubleLBracket)) return state.enable_wiki;
    return at(state, .LBracket);
}

fn atMarkerStart(state: *ParserState) bool {
    if (!at(state, .LBracket)) return false;
    if (state.index + 2 >= state.tokens.len) return false;
    const text_tok = state.tokens[state.index + 1];
    if (text_tok.kind != .Text) return false;
    const close_tok = state.tokens[state.index + 2];
    if (close_tok.kind != .RBracket) return false;
    if (state.index + 3 < state.tokens.len and state.tokens[state.index + 3].kind == .LParen) return false;
    const raw = state.input[text_tok.start_offset..text_tok.end_offset];
    const trimmed = std.mem.trim(u8, raw, " \t");
    return trimmed.len > 1 and trimmed[0] == '@';
}

fn parseMarker(state: *ParserState) void {
    const start_tok = bump(state);
    const text_tok = current(state);
    const raw = state.input[text_tok.start_offset..text_tok.end_offset];
    const trimmed = std.mem.trim(u8, raw, " \t");
    _ = bump(state);
    const close_tok = if (at(state, .RBracket)) bump(state) else start_tok;
    if (trimmed.len <= 1) return;
    const name = std.fmt.allocPrint(state.allocator, "Tag: {s}", .{trimmed}) catch return;
    errdefer state.allocator.free(name);
    addSymbol(state, name, protocol.Range{ .start = start_tok.start, .end = close_tok.end }) catch return;
}

fn skipCodeSpan(state: *ParserState) bool {
    const offset = current(state).start_offset;
    for (state.code_spans) |span| {
        if (offset >= span.start and offset < span.end) {
            while (!at(state, .Eof) and current(state).start_offset < span.end) {
                _ = bump(state);
            }
            return true;
        }
    }
    for (state.inline_spans) |span| {
        if (offset >= span.start and offset < span.end) {
            while (!at(state, .Eof) and current(state).start_offset < span.end) {
                _ = bump(state);
            }
            return true;
        }
    }
    return false;
}

fn parseHeading(state: *ParserState) void {
    const first_hash = bump(state);
    var level: usize = 1;
    var last_hash = first_hash;

    while (at(state, .Hash) and level < 6) {
        last_hash = bump(state);
        level += 1;
    }

    var line_end_offset = last_hash.end_offset;
    var end_pos = last_hash.end;

    while (!at(state, .Eof) and !at(state, .Newline)) {
        const tok = bump(state);
        line_end_offset = tok.end_offset;
        end_pos = tok.end;
    }

    if (at(state, .Newline)) {
        const nl = current(state);
        line_end_offset = nl.start_offset;
        end_pos = nl.start;
        _ = bump(state);
    }

    if (level == 0 or level > 6) return;

    const title_slice = state.input[last_hash.end_offset..line_end_offset];
    const title = std.mem.trim(u8, title_slice, " \t");
    if (title.len == 0) return;

    const name = std.fmt.allocPrint(state.allocator, "H{d}: {s}", .{ level, title }) catch return;
    errdefer state.allocator.free(name);

    addSymbol(state, name, protocol.Range{ .start = first_hash.start, .end = end_pos }) catch return;
    state.headings.append(state.allocator, .{
        .level = @intCast(level),
        .text = title,
        .range = .{ .start = first_hash.start, .end = end_pos },
    }) catch {};
}

fn parseLink(state: *ParserState) void {
    const start_tok = current(state);
    var end_offset = start_tok.end_offset;
    var end_pos = start_tok.end;

    if (at(state, .DoubleLBracket) and state.enable_wiki) {
        _ = bump(state);
        while (!at(state, .Eof) and !at(state, .Newline) and !at(state, .DoubleRBracket)) {
            const tok = bump(state);
            end_offset = tok.end_offset;
            end_pos = tok.end;
        }
        if (at(state, .DoubleRBracket)) {
            const close_tok = bump(state);
            end_offset = close_tok.end_offset;
            end_pos = close_tok.end;
        } else {
            recoverToLineEnd(state);
        }
        addLinkSymbol(state, .wiki, start_tok.start_offset, end_offset, start_tok.start, end_pos) catch return;
        return;
    }

    if (at(state, .LBracket)) {
        var kind: LinkKind = .reference;
        _ = bump(state);
        while (!at(state, .Eof) and !at(state, .Newline) and !at(state, .RBracket)) {
            const tok = bump(state);
            end_offset = tok.end_offset;
            end_pos = tok.end;
        }
        if (!at(state, .RBracket)) {
            recoverToLineEnd(state);
            return;
        }
        const close_label = bump(state);
        end_offset = close_label.end_offset;
        end_pos = close_label.end;

        if (at(state, .LParen)) {
            kind = .inline_link;
            _ = bump(state);
            while (!at(state, .Eof) and !at(state, .Newline) and !at(state, .RParen)) {
                const tok = bump(state);
                end_offset = tok.end_offset;
                end_pos = tok.end;
            }
            if (at(state, .RParen)) {
                const close_paren = bump(state);
                end_offset = close_paren.end_offset;
                end_pos = close_paren.end;
            } else {
                recoverToLineEnd(state);
            }
        } else if (at(state, .LBracket)) {
            _ = bump(state);
            while (!at(state, .Eof) and !at(state, .Newline) and !at(state, .RBracket)) {
                const tok = bump(state);
                end_offset = tok.end_offset;
                end_pos = tok.end;
            }
            if (at(state, .RBracket)) {
                const close_ref = bump(state);
                end_offset = close_ref.end_offset;
                end_pos = close_ref.end;
            } else {
                recoverToLineEnd(state);
            }
        }

        addLinkSymbol(state, kind, start_tok.start_offset, end_offset, start_tok.start, end_pos) catch return;
    }
}

fn recoverToLineEnd(state: *ParserState) void {
    while (!at(state, .Eof) and !at(state, .Newline)) {
        _ = bump(state);
    }
    if (at(state, .Newline)) {
        _ = bump(state);
    }
}

fn addLinkSymbol(
    state: *ParserState,
    kind: LinkKind,
    start_offset: usize,
    end_offset: usize,
    start_pos: protocol.Position,
    end_pos: protocol.Position,
) !void {
    if (end_offset <= start_offset or end_offset > state.input.len) return;
    const slice = state.input[start_offset..end_offset];
    const name = try std.fmt.allocPrint(state.allocator, "Link: {s}", .{slice});
    errdefer state.allocator.free(name);
    const range = protocol.Range{ .start = start_pos, .end = end_pos };
    try addSymbol(state, name, range);
    const target = parseLinkTarget(slice, kind);
    try state.links.append(state.allocator, .{
        .kind = kind,
        .target = target,
        .range = range,
    });
}

fn addSymbol(
    state: *ParserState,
    name: []const u8,
    range: protocol.Range,
) !void {
    try state.symbols.append(state.allocator, .{
        .name = name,
        .kind = .String,
        .range = range,
    });
}

fn at(state: *ParserState, kind: TokenKind) bool {
    return current(state).kind == kind;
}

fn current(state: *ParserState) Token {
    return state.tokens[state.index];
}

fn bump(state: *ParserState) Token {
    const tok = current(state);
    if (tok.kind != .Eof) state.index += 1;
    return tok;
}

fn advanceWithError(state: *ParserState) void {
    _ = bump(state);
}

fn parseBlocksFromLines(state: *ParserState) ![]CodeSpan {
    var spans: std.ArrayList(CodeSpan) = .empty;
    errdefer spans.deinit(state.allocator);

    var line_iter = std.mem.splitScalar(u8, state.input, '\n');
    var line_index: usize = 0;
    var offset: usize = 0;

    var in_code = false;
    var code_start_line: usize = 0;
    var code_start_offset: usize = 0;
    var code_lang: []const u8 = "";
    var code_start_char: usize = 0;
    var in_frontmatter = false;
    var frontmatter_done = false;
    var collecting_tags = false;

    while (line_iter.next()) |line| : (line_index += 1) {
        const line_len = line.len;
        const line_end_offset = offset + line_len;

        if (isCodeFence(line)) |lang| {
            if (!in_code) {
                in_code = true;
                code_start_line = line_index;
                code_start_offset = offset;
                code_start_char = 0;
                code_lang = lang;
            } else {
                in_code = false;
                try addCodeSymbol(
                    state,
                    code_lang,
                    code_start_line,
                    code_start_char,
                    line_index,
                    line_len,
                );
                try spans.append(state.allocator, .{ .start = code_start_offset, .end = line_end_offset });
            }

            offset = line_end_offset + 1;
            continue;
        }

        if (in_code) {
            offset = line_end_offset + 1;
            continue;
        }

        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (!frontmatter_done and line_index == 0 and std.mem.eql(u8, trimmed, "---")) {
            in_frontmatter = true;
            offset = line_end_offset + 1;
            continue;
        }

        if (in_frontmatter) {
            if (line_index > 0 and (std.mem.eql(u8, trimmed, "---") or std.mem.eql(u8, trimmed, "..."))) {
                in_frontmatter = false;
                frontmatter_done = true;
                offset = line_end_offset + 1;
                continue;
            }
            try parseFrontmatterTagsLine(state, line, line_index, &collecting_tags);
            offset = line_end_offset + 1;
            continue;
        }

        if (parseLinkDef(line)) |link_def| {
            state.link_defs.append(state.allocator, link_def) catch {};
            if (findLinkDefStart(line)) |start_col| {
                try addReferenceDefSymbol(state, link_def.label, line_index, start_col, line_len);
            }
        }

        if (listItemInfo(line)) |info| {
            const item_text = if (info.text.len == 0) "-" else info.text;
            try addListItemSymbol(state, item_text, line_index, info.start_col, line_len);
        }

        offset = line_end_offset + 1;
    }

    if (in_code) {
        const last_line = if (line_index == 0) 0 else line_index - 1;
        const last_line_len = lastLineLength(state.input);
        try addCodeSymbol(
            state,
            code_lang,
            code_start_line,
            code_start_char,
            last_line,
            last_line_len,
        );
        try spans.append(state.allocator, .{ .start = code_start_offset, .end = state.input.len });
    }

    return spans.toOwnedSlice(state.allocator);
}

fn parseInlineCodeSpans(input: []const u8, allocator: std.mem.Allocator) ![]CodeSpan {
    var spans: std.ArrayList(CodeSpan) = .empty;
    errdefer spans.deinit(allocator);

    var i: usize = 0;
    while (i < input.len) {
        if (input[i] != '`') {
            i += 1;
            continue;
        }

        var tick_len: usize = 1;
        while (i + tick_len < input.len and input[i + tick_len] == '`') : (tick_len += 1) {}
        const start = i;

        var j = i + tick_len;
        while (j < input.len and input[j] != '\n') {
            if (input[j] == '`') {
                var run: usize = 1;
                while (j + run < input.len and input[j + run] == '`') : (run += 1) {}
                if (run == tick_len) {
                    const end = j + run;
                    try spans.append(allocator, .{ .start = start, .end = end });
                    i = end;
                    break;
                }
                j += run;
                continue;
            }
            j += 1;
        }

        if (j >= input.len or input[j] == '\n') {
            i += tick_len;
        }
    }

    return spans.toOwnedSlice(allocator);
}

fn isCodeFence(line: []const u8) ?[]const u8 {
    var i: usize = 0;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {}
    if (line.len - i < 3) return null;
    if (std.mem.startsWith(u8, line[i..], "```")) {
        const lang = std.mem.trim(u8, line[i + 3 ..], " \t");
        return lang;
    }
    return null;
}

fn addReferenceDefSymbol(
    state: *ParserState,
    label: []const u8,
    line: usize,
    start_char: usize,
    end_char: usize,
) !void {
    const name = try std.fmt.allocPrint(state.allocator, "Ref: [{s}]", .{label});
    errdefer state.allocator.free(name);
    try addSymbol(state, name, protocol.Range{
        .start = .{ .line = line, .character = start_char },
        .end = .{ .line = line, .character = end_char },
    });
}

fn findLinkDefStart(line: []const u8) ?usize {
    var i: usize = 0;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {}
    if (i >= line.len) return null;
    if (line[i] != '[') return null;
    return i;
}

fn listItemInfo(line: []const u8) ?struct {
    start_col: usize,
    text: []const u8,
} {
    var i: usize = 0;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {}
    if (i >= line.len) return null;
    if (line[i] != '-') return null;
    if (i + 1 >= line.len or (line[i + 1] != ' ' and line[i + 1] != '\t')) return null;
    var j = i + 1;
    while (j < line.len and (line[j] == ' ' or line[j] == '\t')) : (j += 1) {}
    const text = std.mem.trim(u8, line[j..], " \t");
    return .{ .start_col = i, .text = text };
}

fn addListItemSymbol(
    state: *ParserState,
    text: []const u8,
    line: usize,
    start_char: usize,
    end_char: usize,
) !void {
    const name = if (parseTask(text)) |task|
        if (task.rest.len > 0)
            try std.fmt.allocPrint(state.allocator, "Task: {s} {s}", .{ task.status, task.rest })
        else
            try std.fmt.allocPrint(state.allocator, "Task: {s}", .{task.status})
    else
        try std.fmt.allocPrint(state.allocator, "List: {s}", .{text});
    errdefer state.allocator.free(name);
    try addSymbol(state, name, protocol.Range{
        .start = .{ .line = line, .character = start_char },
        .end = .{ .line = line, .character = end_char },
    });
}

fn parseTask(text: []const u8) ?struct { status: []const u8, rest: []const u8 } {
    if (text.len < 3) return null;
    if (text[0] != '[' or text[2] != ']') return null;
    if (text[1] != ' ' and text[1] != 'x' and text[1] != 'X') return null;
    const status = text[0..3];
    var rest = text[3..];
    rest = std.mem.trimLeft(u8, rest, " \t");
    return .{ .status = status, .rest = rest };
}

fn parseFrontmatterTagsLine(
    state: *ParserState,
    line: []const u8,
    line_index: usize,
    collecting_tags: *bool,
) !void {
    var i: usize = 0;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {}
    const trimmed = line[i..];
    if (trimmed.len == 0) return;

    if (collecting_tags.*) {
        if (trimmed[0] == '-' or trimmed[0] == '*') {
            var j: usize = 1;
            while (j < trimmed.len and (trimmed[j] == ' ' or trimmed[j] == '\t')) : (j += 1) {}
            const tag = std.mem.trim(u8, trimmed[j..], " \t");
            if (tag.len > 0) {
                try addFrontmatterTagSymbol(state, tag, line_index, i, line.len);
            }
            return;
        }
        collecting_tags.* = false;
    }

    if (trimmed.len >= 5 and std.ascii.eqlIgnoreCase(trimmed[0..4], "tags") and trimmed[4] == ':') {
        const rest = std.mem.trim(u8, trimmed[5..], " \t");
        if (rest.len == 0) {
            collecting_tags.* = true;
            return;
        }
        try addTagsFromInlineValue(state, rest, line_index, i, line.len);
    }
}

fn addTagsFromInlineValue(
    state: *ParserState,
    value: []const u8,
    line_index: usize,
    start_col: usize,
    end_col: usize,
) !void {
    var slice = std.mem.trim(u8, value, " \t");
    if (slice.len >= 2 and slice[0] == '[' and slice[slice.len - 1] == ']') {
        slice = std.mem.trim(u8, slice[1 .. slice.len - 1], " \t");
    }

    if (std.mem.indexOfScalar(u8, slice, ',')) |_| {
        var it = std.mem.splitScalar(u8, slice, ',');
        while (it.next()) |part| {
            const tag = std.mem.trim(u8, part, " \t");
            if (tag.len == 0) continue;
            try addFrontmatterTagSymbol(state, tag, line_index, start_col, end_col);
        }
        return;
    }

    var ws = std.mem.splitAny(u8, slice, " \t");
    while (ws.next()) |part| {
        const tag = std.mem.trim(u8, part, " \t");
        if (tag.len == 0) continue;
        try addFrontmatterTagSymbol(state, tag, line_index, start_col, end_col);
    }
}

fn addFrontmatterTagSymbol(
    state: *ParserState,
    raw: []const u8,
    line_index: usize,
    start_col: usize,
    end_col: usize,
) !void {
    const tag = if (raw.len > 0 and raw[0] == '#') raw[1..] else raw;
    if (tag.len == 0) return;
    const name = try std.fmt.allocPrint(state.allocator, "Tag: #{s}", .{tag});
    errdefer state.allocator.free(name);
    try addSymbol(state, name, protocol.Range{
        .start = .{ .line = line_index, .character = start_col },
        .end = .{ .line = line_index, .character = end_col },
    });
}

fn addCodeSymbol(
    state: *ParserState,
    lang: []const u8,
    start_line: usize,
    start_char: usize,
    end_line: usize,
    end_char: usize,
) !void {
    const label = if (lang.len == 0) "(plain)" else lang;
    const name = try std.fmt.allocPrint(state.allocator, "Code: {s}", .{label});
    errdefer state.allocator.free(name);
    try addSymbol(state, name, protocol.Range{
        .start = .{ .line = start_line, .character = start_char },
        .end = .{ .line = end_line, .character = end_char },
    });
}

fn lastLineLength(text: []const u8) usize {
    var i: usize = text.len;
    while (i > 0) : (i -= 1) {
        if (text[i - 1] == '\n') break;
    }
    return text.len - i;
}

fn parseLinkTarget(slice: []const u8, kind: LinkKind) LinkTarget {
    switch (kind) {
        .wiki => {
            const inner = trimBrackets(slice, "[[", "]]");
            const parts = splitOnce(inner, '#');
            return .{
                .path = if (parts[0].len == 0) null else parts[0],
                .anchor = if (parts[1].len == 0) null else parts[1],
                .label = null,
            };
        },
        .inline_link => {
            const inner = trimBrackets(slice, "[", "]");
            const paren = std.mem.indexOfScalar(u8, slice, '(') orelse return .{ .path = null, .anchor = null, .label = null };
            const close = std.mem.lastIndexOfScalar(u8, slice, ')') orelse return .{ .path = null, .anchor = null, .label = null };
            if (close <= paren + 1) return .{ .path = null, .anchor = null, .label = null };
            const url = std.mem.trim(u8, slice[paren + 1 .. close], " \t");
            const parts = splitOnce(url, '#');
            return .{
                .path = if (parts[0].len == 0) null else parts[0],
                .anchor = if (parts[1].len == 0) null else parts[1],
                .label = inner,
            };
        },
        .reference => {
            const label = parseReferenceLabel(slice);
            return .{
                .path = null,
                .anchor = null,
                .label = label,
            };
        },
    }
}

fn parseReferenceLabel(slice: []const u8) []const u8 {
    const close = std.mem.indexOfScalar(u8, slice, ']') orelse return "";
    const rest = slice[close + 1 ..];
    if (std.mem.startsWith(u8, rest, "[]")) {
        return slice[1..close];
    }
    if (std.mem.startsWith(u8, rest, "[")) {
        const close_ref = std.mem.indexOfScalar(u8, rest[1..], ']') orelse return slice[1..close];
        return rest[1 .. 1 + close_ref];
    }
    return slice[1..close];
}

fn trimBrackets(slice: []const u8, open: []const u8, close: []const u8) []const u8 {
    if (!std.mem.startsWith(u8, slice, open)) return slice;
    if (!std.mem.endsWith(u8, slice, close)) return slice;
    return slice[open.len .. slice.len - close.len];
}

fn splitOnce(text: []const u8, needle: u8) [2][]const u8 {
    if (std.mem.indexOfScalar(u8, text, needle)) |idx| {
        return .{ text[0..idx], text[idx + 1 ..] };
    }
    return .{ text, "" };
}

fn parseLinkDef(line: []const u8) ?LinkDef {
    var i: usize = 0;
    while (i < line.len and (line[i] == ' ' or line[i] == '\t')) : (i += 1) {}
    if (i >= line.len or line[i] != '[') return null;
    const close = std.mem.indexOfScalar(u8, line[i + 1 ..], ']') orelse return null;
    const label = line[i + 1 .. i + 1 + close];
    const after = line[i + 1 + close + 1 ..];
    var j: usize = 0;
    while (j < after.len and (after[j] == ' ' or after[j] == '\t')) : (j += 1) {}
    if (j >= after.len or after[j] != ':') return null;
    const url = std.mem.trim(u8, after[j + 1 ..], " \t");
    if (url.len == 0) return null;
    const parts = splitOnce(url, '#');
    return .{
        .label = label,
        .target = .{
            .path = parts[0],
            .anchor = if (parts[1].len == 0) null else parts[1],
            .label = label,
        },
    };
}

test "parse simple headings and links" {
    const input =
        \\# Title
        \\## Subtitle
        \\Text with a [[Wiki Link]] and [Inline](doc.md).
        \\See [Ref][label] too.
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    try std.testing.expectEqual(@as(usize, 5), parsed.symbols.len);
    try std.testing.expect(std.mem.startsWith(u8, parsed.symbols[0].name, "H1:"));
    try std.testing.expect(std.mem.startsWith(u8, parsed.symbols[1].name, "H2:"));
    try std.testing.expect(std.mem.startsWith(u8, parsed.symbols[2].name, "Link:"));
}

test "resilient parsing keeps later headings" {
    const input =
        \\## First
        \\[broken
        \\### Second
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    try std.testing.expectEqual(@as(usize, 2), parsed.symbols.len);
    try std.testing.expect(std.mem.startsWith(u8, parsed.symbols[0].name, "H2: First"));
    try std.testing.expect(std.mem.startsWith(u8, parsed.symbols[1].name, "H3: Second"));
}

test "list blocks and code fences are symbols" {
    const input =
        \\- one
        \\- two
        \\text
        \\```zig
        \\# not heading
        \\```
        \\# Heading
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    var list_found = false;
    var code_found = false;
    var heading_found = false;
    for (parsed.symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "List: one")) list_found = true;
        if (std.mem.eql(u8, sym.name, "Code: zig")) code_found = true;
        if (std.mem.startsWith(u8, sym.name, "H1: Heading")) heading_found = true;
        if (std.mem.startsWith(u8, sym.name, "H1: not heading")) return error.TestUnexpectedHeading;
    }
    try std.testing.expect(list_found);
    try std.testing.expect(code_found);
    try std.testing.expect(heading_found);
}

test "reference link definitions are symbols" {
    const input =
        \\[label]: target.md
        \\
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    var found = false;
    for (parsed.symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "Ref: [label]")) found = true;
    }
    try std.testing.expect(found);
}

test "tags are symbols" {
    const input =
        \\---
        \\tags: [one, two]
        \\tags:
        \\  - related
        \\---
        \\#related
        \\Tag list: #skip
        \\`#skip`
        \\
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    var tag_one = false;
    var tag_two = false;
    var tag_related = false;
    var heading_related = false;
    for (parsed.symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "Tag: #one")) tag_one = true;
        if (std.mem.eql(u8, sym.name, "Tag: #two")) tag_two = true;
        if (std.mem.eql(u8, sym.name, "Tag: #related")) tag_related = true;
        if (std.mem.eql(u8, sym.name, "Tag: #skip")) return error.TestUnexpectedTag;
        if (std.mem.startsWith(u8, sym.name, "H1: related")) heading_related = true;
    }
    try std.testing.expect(tag_one);
    try std.testing.expect(tag_two);
    try std.testing.expect(tag_related);
    try std.testing.expect(heading_related);
}

test "custom markers are symbols" {
    const input =
        \\Idea [@blog] and [@Dev].
        \\`[@skip]`
        \\[@link](doc.md)
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    var blog_found = false;
    var dev_found = false;
    var link_found = false;
    for (parsed.symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "Tag: @blog")) blog_found = true;
        if (std.mem.eql(u8, sym.name, "Tag: @Dev")) dev_found = true;
        if (std.mem.startsWith(u8, sym.name, "Link: [@link]")) link_found = true;
        if (std.mem.eql(u8, sym.name, "Tag: @skip")) return error.TestUnexpectedTag;
    }
    try std.testing.expect(blog_found);
    try std.testing.expect(dev_found);
    try std.testing.expect(link_found);
}

test "tasks are symbols" {
    const input =
        \\- [ ] todo
        \\- [x] done
        \\
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    var todo_found = false;
    var done_found = false;
    for (parsed.symbols) |sym| {
        if (std.mem.eql(u8, sym.name, "Task: [ ] todo")) todo_found = true;
        if (std.mem.eql(u8, sym.name, "Task: [x] done")) done_found = true;
    }
    try std.testing.expect(todo_found);
    try std.testing.expect(done_found);
}

test "inline code spans suppress links" {
    var parser = Parser{};
    const input = "`[skip](a.md)` [ok](b.md)\n";
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    try std.testing.expectEqual(@as(usize, 1), parsed.links.len);
    try std.testing.expect(std.mem.containsAtLeast(u8, parsed.symbols[0].name, 1, "ok"));
}

test "snapshot: mixed markdown symbols" {
    const input =
        \\# Title
        \\- one
        \\- two
        \\text
        \\```zig
        \\# not heading
        \\[link](ignored)
        \\```
        \\## Subtitle
        \\Text with a [[Wiki Link]] and [Inline](doc.md).
        \\See [Ref][label] too.
    ;
    var parser = Parser{};
    const parsed = try parser.parse(std.testing.allocator, input, true);
    defer {
        for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(parsed.symbols);
        std.testing.allocator.free(parsed.headings);
        std.testing.allocator.free(parsed.links);
        std.testing.allocator.free(parsed.link_defs);
    }

    const snap = Snap.snap_fn(".");
    const rendered = try renderSymbols(std.testing.allocator, parsed.symbols);
    defer std.testing.allocator.free(rendered);

    try snap(@src(),
        \\H1: Title @ 0:0-0:7
        \\List: one @ 1:0-1:5
        \\List: two @ 2:0-2:5
        \\Code: zig @ 4:0-7:3
        \\H2: Subtitle @ 8:0-8:11
        \\Link: [[Wiki Link]] @ 9:10-9:23
        \\Link: [Inline](doc.md) @ 9:29-9:45
        \\Link: [Ref][label] @ 10:4-10:16
    ).diff(rendered);
}

test "fuzz: parser stays in bounds" {
    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            const max_len: usize = 2048;
            const len = @min(input.len, max_len);
            const buf = try std.testing.allocator.alloc(u8, len);
            defer std.testing.allocator.free(buf);

            sanitizeInput(buf, input[0..len]);

            var parser = Parser{};
            const parsed = try parser.parse(std.testing.allocator, buf, true);
            defer {
                for (parsed.symbols) |sym| std.testing.allocator.free(sym.name);
                std.testing.allocator.free(parsed.symbols);
                std.testing.allocator.free(parsed.headings);
                std.testing.allocator.free(parsed.links);
                std.testing.allocator.free(parsed.link_defs);
            }

            const line_lengths = try computeLineLengths(std.testing.allocator, buf);
            defer std.testing.allocator.free(line_lengths);

            for (parsed.symbols) |sym| {
                try validateRange(sym.range, line_lengths);
            }
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

fn renderSymbols(allocator: std.mem.Allocator, symbols: []const index.Symbol) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    defer out.deinit(allocator);

    for (symbols) |sym| {
        try out.appendSlice(allocator, sym.name);
        try out.appendSlice(allocator, " @ ");
        try out.writer(allocator).print(
            "{d}:{d}-{d}:{d}\n",
            .{
                sym.range.start.line,
                sym.range.start.character,
                sym.range.end.line,
                sym.range.end.character,
            },
        );
    }

    return out.toOwnedSlice(allocator);
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

fn computeLineLengths(allocator: std.mem.Allocator, text: []const u8) ![]usize {
    var lengths: std.ArrayListUnmanaged(usize) = .empty;
    errdefer lengths.deinit(allocator);

    var line_iter = std.mem.splitScalar(u8, text, '\n');
    while (line_iter.next()) |line| {
        try lengths.append(allocator, line.len);
    }
    if (text.len == 0) {
        try lengths.append(allocator, 0);
    }
    return lengths.toOwnedSlice(allocator);
}

fn validateRange(range: protocol.Range, line_lengths: []const usize) !void {
    if (range.start.line >= line_lengths.len) return error.TestRangeOutOfBounds;
    if (range.end.line >= line_lengths.len) return error.TestRangeOutOfBounds;
    if (range.start.character > line_lengths[range.start.line]) return error.TestRangeOutOfBounds;
    if (range.end.character > line_lengths[range.end.line]) return error.TestRangeOutOfBounds;
    if (range.end.line < range.start.line) return error.TestRangeOutOfBounds;
    if (range.end.line == range.start.line and range.end.character < range.start.character) {
        return error.TestRangeOutOfBounds;
    }
}
