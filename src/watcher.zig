const std = @import("std");
const builtin = @import("builtin");

pub const WatchEventType = enum {
    create,
    modify,
    delete,
};

pub const WatchEvent = struct {
    path: []const u8,
    kind: WatchEventType,
    is_dir: bool,
};

pub const Watcher = if (builtin.os.tag == .linux) LinuxWatcher else StubWatcher;

const StubWatcher = struct {
    pub fn init(_: std.mem.Allocator) !StubWatcher {
        return .{};
    }

    pub fn deinit(_: *StubWatcher) void {}

    pub fn addRoot(_: *StubWatcher, _: []const u8) !void {}

    pub fn readEvents(_: *StubWatcher, _: std.mem.Allocator) ![]WatchEvent {
        return &.{};
    }

    pub fn fd(_: *StubWatcher) ?i32 {
        return null;
    }
};

const LinuxWatcher = struct {
    allocator: std.mem.Allocator,
    inotify_fd: i32,
    watches: std.AutoHashMap(i32, []const u8),

    pub fn init(allocator: std.mem.Allocator) !LinuxWatcher {
        const flags = @as(u32, @bitCast(std.posix.O{ .NONBLOCK = true, .CLOEXEC = true }));
        const inotify_fd = try std.posix.inotify_init1(flags);
        return .{
            .allocator = allocator,
            .inotify_fd = inotify_fd,
            .watches = std.AutoHashMap(i32, []const u8).init(allocator),
        };
    }

    pub fn deinit(self: *LinuxWatcher) void {
        var it = self.watches.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.*);
        }
        self.watches.deinit();
        _ = std.posix.close(self.inotify_fd);
    }

    pub fn fd(self: *LinuxWatcher) ?i32 {
        return self.inotify_fd;
    }

    pub fn addRoot(self: *LinuxWatcher, root: []const u8) !void {
        var dir = try std.fs.openDirAbsolute(root, .{ .iterate = true });
        defer dir.close();

        try self.addDirWatch(root);

        var walker = try dir.walk(self.allocator);
        defer walker.deinit();

        while (try walker.next()) |entry| {
            if (entry.kind != .directory) continue;
            try self.addDirWatch(entry.path);
        }
    }

    pub fn readEvents(self: *LinuxWatcher, allocator: std.mem.Allocator) ![]WatchEvent {
        var events: std.ArrayListUnmanaged(WatchEvent) = .empty;
        errdefer events.deinit(allocator);

        var buffer: [4096]u8 = undefined;
        const bytes_read = std.posix.read(self.inotify_fd, &buffer) catch |err| switch (err) {
            error.WouldBlock => return &.{},
            else => return err,
        };
        if (bytes_read == 0) return &.{};

        var offset: usize = 0;
        while (offset + @sizeOf(std.os.linux.inotify_event) <= bytes_read) {
            const ev: *const std.os.linux.inotify_event =
                @ptrCast(@alignCast(buffer[offset..].ptr));
            offset += @sizeOf(std.os.linux.inotify_event) + ev.len;

            const dir_path = self.watches.get(ev.wd) orelse continue;
            const name = ev.getName();
            const path = if (name) |n|
                try std.fs.path.join(allocator, &.{ dir_path, n })
            else
                try allocator.dupe(u8, dir_path);

            const is_dir = (ev.mask & IN_ISDIR) != 0;
            const kind = eventKind(ev.mask) orelse {
                allocator.free(path);
                continue;
            };

            try events.append(allocator, .{
                .path = path,
                .kind = kind,
                .is_dir = is_dir,
            });

            if (is_dir and kind == .create) {
                self.addDirWatch(path) catch {};
            }
        }

        return events.toOwnedSlice(allocator);
    }

    fn addDirWatch(self: *LinuxWatcher, path: []const u8) !void {
        const mask = IN_CREATE | IN_DELETE | IN_MODIFY | IN_MOVED_FROM | IN_MOVED_TO | IN_CLOSE_WRITE;
        const wd = std.posix.inotify_add_watch(self.inotify_fd, path, mask) catch |err| switch (err) {
            error.WatchAlreadyExists => return,
            else => return err,
        };
        if (self.watches.get(wd) != null) return;
        const owned = try self.allocator.dupe(u8, path);
        try self.watches.put(wd, owned);
    }
};

fn eventKind(mask: u32) ?WatchEventType {
    if ((mask & (IN_CREATE | IN_MOVED_TO)) != 0) return .create;
    if ((mask & (IN_DELETE | IN_MOVED_FROM)) != 0) return .delete;
    if ((mask & (IN_MODIFY | IN_CLOSE_WRITE)) != 0) return .modify;
    return null;
}

const IN_MODIFY: u32 = 0x00000002;
const IN_CLOSE_WRITE: u32 = 0x00000008;
const IN_MOVED_FROM: u32 = 0x00000040;
const IN_MOVED_TO: u32 = 0x00000080;
const IN_CREATE: u32 = 0x00000100;
const IN_DELETE: u32 = 0x00000200;
const IN_ISDIR: u32 = 0x40000000;
