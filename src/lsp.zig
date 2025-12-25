const std = @import("std");

pub fn readMessage(
    allocator: std.mem.Allocator,
    reader: anytype,
) !?[]u8 {
    var content_length: ?usize = null;

    while (true) {
        const line = try readLine(allocator, reader) orelse return null;
        defer allocator.free(line);

        const trimmed = std.mem.trimRight(u8, line, "\r\n");
        if (trimmed.len == 0) break;

        if (std.mem.startsWith(u8, trimmed, "Content-Length:")) {
            const value = std.mem.trim(u8, trimmed["Content-Length:".len..], " ");
            content_length = try std.fmt.parseInt(usize, value, 10);
        }
    }

    const len = content_length orelse return error.MissingContentLength;
    const body = try allocator.alloc(u8, len);
    errdefer allocator.free(body);
    try reader.readSliceAll(body);
    return body;
}

pub fn writeMessage(writer: anytype, payload: []const u8) !void {
    try writer.print("Content-Length: {d}\r\n\r\n", .{payload.len});
    try writer.writeAll(payload);
    try writer.flush();
}

fn readLine(allocator: std.mem.Allocator, reader: anytype) !?[]u8 {
    var line: std.ArrayList(u8) = .empty;
    errdefer line.deinit(allocator);

    var buf: [1]u8 = undefined;
    while (true) {
        const read_len = try reader.readSliceShort(&buf);
        if (read_len == 0) {
            if (line.items.len == 0) return null;
            break;
        }
        if (buf[0] == '\n') break;
        try line.append(allocator, buf[0]);
    }
    const slice = try line.toOwnedSlice(allocator);
    return @as(?[]u8, slice);
}
