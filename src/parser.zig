const std = @import("std");
const protocol = @import("protocol.zig");
const index = @import("index.zig");

pub const Backend = enum {
    simple,
    tree_sitter,
};

pub const Parser = struct {
    backend: Backend = .simple,

    pub fn parse(self: *Parser, allocator: std.mem.Allocator, text: []const u8) ![]index.Symbol {
        return switch (self.backend) {
            .simple => parseSimple(allocator, text),
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

const Lexer = struct {
    input: []const u8,
    index: usize,
    line: usize,
    column: usize,
    line_start: bool,

    pub fn init(input: []const u8) Lexer {
        return .{
            .input = input,
            .index = 0,
            .line = 0,
            .column = 0,
            .line_start = true,
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

        if (ch == '#' and at_line_start) {
            self.index += 1;
            self.column += 1;
            return .{
                .kind = .Hash,
                .start = .{ .line = start_line, .character = start_col },
                .end = .{ .line = self.line, .character = self.column },
                .start_offset = start_index,
                .end_offset = self.index,
                .line_start = at_line_start,
            };
        }

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
};

fn parseSimple(allocator: std.mem.Allocator, text: []const u8) ![]index.Symbol {
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
    };
    errdefer {
        for (state.symbols.items) |sym| allocator.free(sym.name);
        state.symbols.deinit(allocator);
    }

    parseFile(&state);
    return state.symbols.toOwnedSlice(allocator);
}

fn parseFile(state: *ParserState) void {
    while (!at(state, .Eof)) {
        if (at(state, .Newline)) {
            _ = bump(state);
            continue;
        }
        if (atHeadingStart(state)) {
            parseHeading(state);
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
    return at(state, .DoubleLBracket) or at(state, .LBracket);
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
}

fn parseLink(state: *ParserState) void {
    const start_tok = current(state);
    var end_offset = start_tok.end_offset;
    var end_pos = start_tok.end;

    if (at(state, .DoubleLBracket)) {
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
        addLinkSymbol(state, start_tok.start_offset, end_offset, start_tok.start, end_pos) catch return;
        return;
    }

    if (at(state, .LBracket)) {
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

        addLinkSymbol(state, start_tok.start_offset, end_offset, start_tok.start, end_pos) catch return;
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
    start_offset: usize,
    end_offset: usize,
    start_pos: protocol.Position,
    end_pos: protocol.Position,
) !void {
    if (end_offset <= start_offset or end_offset > state.input.len) return;
    const slice = state.input[start_offset..end_offset];
    const name = try std.fmt.allocPrint(state.allocator, "Link: {s}", .{slice});
    errdefer state.allocator.free(name);
    try addSymbol(state, name, protocol.Range{ .start = start_pos, .end = end_pos });
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

test "parse simple headings and links" {
    const input =
        \\# Title
        \\Text with a [[Wiki Link]] and [Inline](doc.md).
        \\See [Ref][label] too.
    ;
    var parser = Parser{};
    const symbols = try parser.parse(std.testing.allocator, input);
    defer {
        for (symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(symbols);
    }

    try std.testing.expectEqual(@as(usize, 4), symbols.len);
    try std.testing.expect(std.mem.startsWith(u8, symbols[0].name, "H1:"));
    try std.testing.expect(std.mem.startsWith(u8, symbols[1].name, "Link:"));
}

test "resilient parsing keeps later headings" {
    const input =
        \\## First
        \\[broken
        \\### Second
    ;
    var parser = Parser{};
    const symbols = try parser.parse(std.testing.allocator, input);
    defer {
        for (symbols) |sym| std.testing.allocator.free(sym.name);
        std.testing.allocator.free(symbols);
    }

    try std.testing.expectEqual(@as(usize, 2), symbols.len);
    try std.testing.expect(std.mem.startsWith(u8, symbols[0].name, "H2: First"));
    try std.testing.expect(std.mem.startsWith(u8, symbols[1].name, "H3: Second"));
}
