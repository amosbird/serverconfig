#!/usr/bin/env luajit

local ffi = require("ffi")
local bit = require("bit")

ffi.cdef[[
typedef long ssize_t;
typedef int32_t pid_t;
typedef uint32_t uid_t;
typedef uint32_t socklen_t;

typedef struct {
    uint64_t st_dev;
    uint64_t st_ino;
    uint64_t st_nlink;
    uint32_t st_mode;
    uint32_t st_uid;
    uint8_t  _pad[112];
} linux_stat_t;

typedef struct {
    uint16_t sun_family;
    char sun_path[108];
} sockaddr_un_t;

typedef struct {
    int32_t pid;
    uint32_t uid;
    uint32_t gid;
} ucred_t;

typedef struct {
    int fd;
    short events;
    short revents;
} pollfd_t;

typedef struct {
    int64_t tv_sec;
    int64_t tv_nsec;
} timespec_t;

typedef struct { uint64_t val[16]; } sigset_t;
typedef struct { uint8_t data[128]; } signalfd_siginfo_t;

ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, const void *buf, size_t count);
int close(int fd);
int open(const char *pathname, int flags, ...);
int dup2(int oldfd, int newfd);
int pipe2(int pipefd[2], int flags);
int socket(int domain, int type, int protocol);
int connect(int sockfd, const void *addr, socklen_t addrlen);
int shutdown(int sockfd, int how);
int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
int poll(pollfd_t *fds, unsigned long nfds, int timeout);
int lstat(const char *path, linux_stat_t *buf);
int stat(const char *path, linux_stat_t *buf);
ssize_t readlink(const char *path, char *buf, size_t bufsiz);
uid_t getuid(void);
int clock_gettime(int clk_id, timespec_t *tp);
ssize_t getrandom(void *buf, size_t buflen, unsigned int flags);
int sigemptyset(sigset_t *set);
int sigaddset(sigset_t *set, int signum);
int sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
int signalfd(int fd, const sigset_t *mask, int flags);
pid_t fork(void);
int execv(const char *pathname, char *const argv[]);
int execvp(const char *file, char *const argv[]);
pid_t waitpid(pid_t pid, int *status, int options);
int kill(pid_t pid, int sig);
void _exit(int status);
char *strerror(int errnum);
]]

local C = ffi.C
local band, bor, lshift, rshift = bit.band, bit.bor, bit.lshift, bit.rshift

local MAX_NATIVE_MSG = 16 * 1024 * 1024
local MAX_BROKER_MSG = 64 * 1024
local QTILE_MAX_REPLY = 64 * 1024
local BOOKMARK_OPEN_TIMEOUT = 35.0
local DESKTOP_FOCUS_TIMEOUT = 2.0
local LEASE_MAX_LIFETIME = 60.0
local DIAGNOSTIC_LIMIT = 4096

local AF_UNIX = 1
local SOCK_STREAM = 1
local SOCK_NONBLOCK = 2048
local SOCK_CLOEXEC = 524288
local SOL_SOCKET = 1
local SO_ERROR = 4
local SO_PEERCRED = 17
local SHUT_WR = 1
local POLLIN = 1
local POLLOUT = 4
local POLLERR = 8
local POLLHUP = 16
local O_RDONLY = 0
local O_WRONLY = 1
local O_NONBLOCK = 2048
local O_CLOEXEC = 524288
local SIGINT = 2
local SIGKILL = 9
local SIGPIPE = 13
local SIGTERM = 15
local SIG_BLOCK = 0
local SIG_SETMASK = 2
local SFD_NONBLOCK = 2048
local SFD_CLOEXEC = 524288
local WNOHANG = 1
local EINTR = 4
local EAGAIN = 11
local EINPROGRESS = 115
local ENOENT = 2
local ECONNREFUSED = 111
local CLOCK_REALTIME = 0
local CLOCK_MONOTONIC = 1
local S_IFMT = 61440    -- 0170000
local S_IFSOCK = 49152  -- 0140000
local S_IFLNK = 40960   -- 0120000

local JSON_NULL = {}
local JSON_ARRAY_MT = {}
local JSON_OBJECT_MT = {}
local CANCELLED = { cancelled = true }

local ts_buf = ffi.new("timespec_t")
local function monotonic_now()
    C.clock_gettime(CLOCK_MONOTONIC, ts_buf)
    return tonumber(ts_buf.tv_sec) + tonumber(ts_buf.tv_nsec) * 1e-9
end

local function realtime_now_ms()
    C.clock_gettime(CLOCK_REALTIME, ts_buf)
    return math.floor(tonumber(ts_buf.tv_sec) * 1000 + tonumber(ts_buf.tv_nsec) / 1e6)
end

local function err_str(e)
    return ffi.string(C.strerror(e or ffi.errno()))
end

-- Minimal strict JSON decoder (RFC 8259)
local function utf8_encode(cp)
    if cp <= 0x7F then
        return string.char(cp)
    elseif cp <= 0x7FF then
        return string.char(bor(0xC0, rshift(cp, 6)), bor(0x80, band(cp, 0x3F)))
    elseif cp <= 0xFFFF then
        return string.char(
            bor(0xE0, rshift(cp, 12)),
            bor(0x80, band(rshift(cp, 6), 0x3F)),
            bor(0x80, band(cp, 0x3F))
        )
    else
        return string.char(
            bor(0xF0, rshift(cp, 18)),
            bor(0x80, band(rshift(cp, 12), 0x3F)),
            bor(0x80, band(rshift(cp, 6), 0x3F)),
            bor(0x80, band(cp, 0x3F))
        )
    end
end

local function json_decode(s)
    local len = #s
    local pos = 1
    local function skip_ws()
        local _, e = s:find("^[ \t\r\n]*", pos)
        pos = e + 1
    end
    local parse_value
    local function parse_string()
        pos = pos + 1 -- skip opening "
        local out = {}
        local start = pos
        while pos <= len do
            local b = s:byte(pos)
            if b == 34 then -- "
                if pos > start then out[#out + 1] = s:sub(start, pos - 1) end
                pos = pos + 1
                return table.concat(out)
            elseif b == 92 then -- \
                if pos > start then out[#out + 1] = s:sub(start, pos - 1) end
                pos = pos + 1
                if pos > len then error("unterminated escape") end
                local esc = s:sub(pos, pos)
                pos = pos + 1
                if esc == '"' or esc == "\\" or esc == "/" then
                    out[#out + 1] = esc
                elseif esc == "b" then out[#out + 1] = "\b"
                elseif esc == "f" then out[#out + 1] = "\f"
                elseif esc == "n" then out[#out + 1] = "\n"
                elseif esc == "r" then out[#out + 1] = "\r"
                elseif esc == "t" then out[#out + 1] = "\t"
                elseif esc == "u" then
                    local hex = s:sub(pos, pos + 3)
                    if not hex:match("^[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]$") then
                        error("invalid unicode escape")
                    end
                    pos = pos + 4
                    local cp = tonumber(hex, 16)
                    if cp >= 0xD800 and cp <= 0xDBFF then
                        if s:sub(pos, pos + 1) ~= "\\u" then error("unpaired high surrogate") end
                        local hex2 = s:sub(pos + 2, pos + 5)
                        local low = tonumber(hex2, 16)
                        if not low or low < 0xDC00 or low > 0xDFFF then
                            error("invalid low surrogate")
                        end
                        pos = pos + 6
                        cp = 0x10000 + (cp - 0xD800) * 0x400 + (low - 0xDC00)
                    elseif cp >= 0xDC00 and cp <= 0xDFFF then
                        error("unexpected low surrogate")
                    end
                    out[#out + 1] = utf8_encode(cp)
                else
                    error("invalid escape")
                end
                start = pos
            elseif b < 0x20 then
                error("unescaped control character")
            else
                pos = pos + 1
            end
        end
        error("unterminated string")
    end
    local function parse_number()
        local start = pos
        if s:byte(pos) == 45 then pos = pos + 1 end
        if pos > len then error("invalid number") end
        local b = s:byte(pos)
        if b == 48 then
            pos = pos + 1
        elseif b >= 49 and b <= 57 then
            pos = pos + 1
            while pos <= len and s:byte(pos) >= 48 and s:byte(pos) <= 57 do pos = pos + 1 end
        else
            error("invalid number")
        end
        local is_float = false
        if pos <= len and s:byte(pos) == 46 then
            is_float = true
            pos = pos + 1
            local fstart = pos
            while pos <= len and s:byte(pos) >= 48 and s:byte(pos) <= 57 do pos = pos + 1 end
            if pos == fstart then error("invalid fraction") end
        end
        if pos <= len and (s:byte(pos) == 101 or s:byte(pos) == 69) then
            is_float = true
            pos = pos + 1
            if pos <= len and (s:byte(pos) == 43 or s:byte(pos) == 45) then pos = pos + 1 end
            local estart = pos
            while pos <= len and s:byte(pos) >= 48 and s:byte(pos) <= 57 do pos = pos + 1 end
            if pos == estart then error("invalid exponent") end
        end
        local n = tonumber(s:sub(start, pos - 1))
        if not n then error("invalid number") end
        return n, not is_float
    end
    local function parse_array(depth)
        pos = pos + 1
        local arr = setmetatable({}, JSON_ARRAY_MT)
        local int_elems = {}
        skip_ws()
        if pos <= len and s:byte(pos) == 93 then pos = pos + 1 return arr, int_elems end
        while true do
            local idx = #arr + 1
            local val, is_int = parse_value(depth + 1)
            arr[idx] = val
            if is_int then int_elems[idx] = true end
            skip_ws()
            local b = s:byte(pos)
            if b == 93 then pos = pos + 1 return arr, int_elems end
            if b ~= 44 then error("expected ',' or ']'") end
            pos = pos + 1
            skip_ws()
        end
    end
    local function parse_object(depth)
        pos = pos + 1
        local obj = setmetatable({}, JSON_OBJECT_MT)
        local int_keys = {}
        skip_ws()
        if pos <= len and s:byte(pos) == 125 then pos = pos + 1 return obj, int_keys end
        while true do
            if pos > len or s:byte(pos) ~= 34 then error("expected string key") end
            local key = parse_string()
            skip_ws()
            if s:byte(pos) ~= 58 then error("expected ':'") end
            pos = pos + 1
            skip_ws()
            local val, is_int = parse_value(depth + 1)
            obj[key] = val
            if is_int then int_keys[key] = true end
            skip_ws()
            local b = s:byte(pos)
            if b == 125 then pos = pos + 1 return obj, int_keys end
            if b ~= 44 then error("expected ',' or '}'") end
            pos = pos + 1
            skip_ws()
        end
    end
    parse_value = function(depth)
        if depth > 64 then error("JSON nesting too deep") end
        skip_ws()
        if pos > len then error("unexpected end of JSON") end
        local b = s:byte(pos)
        if b == 34 then return parse_string(), false
        elseif b == 123 then return parse_object(depth)
        elseif b == 91 then return parse_array(depth)
        elseif b == 116 and s:sub(pos, pos + 3) == "true" then pos = pos + 4 return true, false
        elseif b == 102 and s:sub(pos, pos + 4) == "false" then pos = pos + 5 return false, false
        elseif b == 110 and s:sub(pos, pos + 3) == "null" then pos = pos + 4 return JSON_NULL, false
        elseif b == 45 or (b >= 48 and b <= 57) then return parse_number()
        else error("unexpected JSON token") end
    end
    local ok, val, meta = pcall(parse_value, 0)
    if not ok then return nil, nil, val end
    skip_ws()
    if pos <= len then return nil, nil, "trailing JSON content" end
    return val, meta
end

local escape_map = {
    ['"'] = '\\"', ['\\'] = '\\\\', ['\b'] = '\\b',
    ['\f'] = '\\f', ['\n'] = '\\n', ['\r'] = '\\r', ['\t'] = '\\t',
}
local function json_escape(s)
    return '"' .. s:gsub('[%z\1-\31\\"]', function(c)
        return escape_map[c] or string.format("\\u%04x", c:byte())
    end) .. '"'
end

-- Native messaging framing (little-endian uint32 length prefix on stdin/stdout)
local function read_exact_fd(fd, n)
    if n == 0 then return "" end
    local buf = ffi.new("uint8_t[?]", n)
    local got = 0
    while got < n do
        local r = C.read(fd, buf + got, n - got)
        if r == 0 then
            if got == 0 then return nil, "eof" end
            return nil, "native message ended early"
        elseif r < 0 then
            local e = ffi.errno()
            if e ~= EINTR then return nil, "read failed: " .. err_str(e) end
        else
            got = got + tonumber(r)
        end
    end
    return ffi.string(buf, n)
end

local function write_all_fd(fd, data)
    local p = ffi.cast("const uint8_t *", data)
    local len = #data
    local sent = 0
    while sent < len do
        local w = C.write(fd, p + sent, len - sent)
        if w < 0 then
            local e = ffi.errno()
            if e ~= EINTR then return nil, "write failed: " .. err_str(e) end
        else
            sent = sent + tonumber(w)
        end
    end
    return true
end

local function u32_le(n)
    return string.char(
        band(n, 0xFF),
        band(rshift(n, 8), 0xFF),
        band(rshift(n, 16), 0xFF),
        band(rshift(n, 24), 0xFF)
    )
end

local function u32_be(n)
    return string.char(
        band(rshift(n, 24), 0xFF),
        band(rshift(n, 16), 0xFF),
        band(rshift(n, 8), 0xFF),
        band(n, 0xFF)
    )
end

local function read_u32_le(s)
    local b1, b2, b3, b4 = s:byte(1, 4)
    return b1 + b2 * 256 + b3 * 65536 + b4 * 16777216
end

local function read_u32_be(s)
    local b1, b2, b3, b4 = s:byte(1, 4)
    return b1 * 16777216 + b2 * 65536 + b3 * 256 + b4
end

local function send_native_response(request_id, completion, message)
    if #message > DIAGNOSTIC_LIMIT then
        message = message:sub(#message - DIAGNOSTIC_LIMIT + 1)
    end
    local rid_json = type(request_id) == "string" and json_escape(request_id) or "null"
    local payload = string.format(
        '{"version":1,"info":"openInBrowser","request_id":%s,"completion":%s,"message":%s}',
        rid_json, json_escape(completion), json_escape(message)
    )
    write_all_fd(1, u32_le(#payload) .. payload)
end

-- Signal handling via signalfd (no Lua C-callback signal race)
local sig_fd = -1
local saved_mask = ffi.new("sigset_t")
local mask_saved = false
local cancelled_flag = false
local stdin_closed_flag = false

local function setup_signals()
    cancelled_flag = false
    stdin_closed_flag = false
    local ignore_pipe = ffi.new("sigset_t")
    C.sigemptyset(ignore_pipe)
    C.sigaddset(ignore_pipe, SIGPIPE)
    C.sigprocmask(SIG_BLOCK, ignore_pipe, nil)

    local mask = ffi.new("sigset_t")
    C.sigemptyset(mask)
    C.sigaddset(mask, SIGTERM)
    C.sigaddset(mask, SIGINT)
    C.sigprocmask(SIG_BLOCK, mask, saved_mask)
    mask_saved = true
    sig_fd = C.signalfd(-1, mask, bor(SFD_NONBLOCK, SFD_CLOEXEC))
end

local function restore_signals()
    if sig_fd >= 0 then
        C.close(sig_fd)
        sig_fd = -1
    end
    if mask_saved then
        C.sigprocmask(SIG_SETMASK, saved_mask, nil)
        mask_saved = false
    end
end

local siginfo_buf = ffi.new("signalfd_siginfo_t")
local stdin_poll_buf = ffi.new("pollfd_t[1]")
local function check_stdin_closed()
    if stdin_closed_flag then return true end
    stdin_poll_buf[0].fd = 0
    stdin_poll_buf[0].events = POLLHUP
    stdin_poll_buf[0].revents = 0
    if C.poll(stdin_poll_buf, 1, 0) > 0 and band(stdin_poll_buf[0].revents, bor(POLLHUP, POLLERR)) ~= 0 then
        stdin_closed_flag = true
        cancelled_flag = true
        return true
    end
    return false
end

local function check_cancelled()
    if cancelled_flag then return true end
    if sig_fd >= 0 then
        local r = C.read(sig_fd, siginfo_buf, ffi.sizeof("signalfd_siginfo_t"))
        if r > 0 then
            cancelled_flag = true
            return true
        end
    end
    if check_stdin_closed() then return true end
    return false
end

local function poll_fd(fd, events, deadline)
    local pfds = ffi.new("pollfd_t[3]")
    while true do
        if check_cancelled() then return nil, CANCELLED end
        local rem = deadline - monotonic_now()
        if rem <= 0 then return nil, "timeout" end
        local ms = math.ceil(rem * 1000)
        if ms > 100 then ms = 100 end
        if ms < 1 then ms = 1 end
        pfds[0].fd = fd
        pfds[0].events = events
        pfds[0].revents = 0
        pfds[1].fd = 0
        pfds[1].events = POLLHUP
        pfds[1].revents = 0
        local nfds = 2
        if sig_fd >= 0 then
            pfds[2].fd = sig_fd
            pfds[2].events = POLLIN
            pfds[2].revents = 0
            nfds = 3
        end
        local r = C.poll(pfds, nfds, ms)
        if r < 0 then
            local e = ffi.errno()
            if e ~= EINTR then return nil, "poll failed: " .. err_str(e) end
        elseif r > 0 then
            if nfds == 3 and band(pfds[2].revents, POLLIN) ~= 0 then
                check_cancelled()
                return nil, CANCELLED
            end
            if band(pfds[1].revents, bor(POLLHUP, POLLERR)) ~= 0 then
                stdin_closed_flag = true
                cancelled_flag = true
                return nil, CANCELLED
            end
            if pfds[0].revents ~= 0 then
                return pfds[0].revents
            end
        end
    end
end

-- Strict URL validation matching Python's urlsplit(url) + host.open_in_browser rules
local function validate_bookmark_url(url)
    if type(url) ~= "string" or #url == 0 or #url > 32768 then
        return nil, "invalid bookmark URL"
    end
    for i = 1, #url do
        local b = url:byte(i)
        if b <= 32 or b == 127 then
            return nil, "invalid bookmark URL"
        end
    end
    local scheme, rest = url:match("^([A-Za-z][A-Za-z0-9+.-]*):(.*)$")
    if not scheme then return nil, "unsupported bookmark URL" end
    scheme = scheme:lower()
    if (scheme ~= "http" and scheme ~= "https") or rest:sub(1, 2) ~= "//" then
        return nil, "unsupported bookmark URL"
    end
    local authority = rest:sub(3):match("^[^/?#]*")
    if not authority or authority == "" then
        return nil, "unsupported bookmark URL"
    end
    local hostport = authority:match("@([^@]*)$") or authority
    if hostport == "" then return nil, "unsupported bookmark URL" end
    local host, port_str
    if hostport:byte(1) == 91 then -- [IPv6]
        local close = hostport:find("]", 2, true)
        if not close then return nil, "invalid bookmark URL" end
        host = hostport:sub(2, close - 1)
        local after = hostport:sub(close + 1)
        if after ~= "" then
            if after:byte(1) ~= 58 then return nil, "invalid bookmark URL" end
            port_str = after:sub(2)
        end
    else
        local colon = hostport:find(":", 1, true)
        if colon then
            host = hostport:sub(1, colon - 1)
            port_str = hostport:sub(colon + 1)
        else
            host = hostport
        end
    end
    if not host or host == "" or host:find("[%[%]:]") then
        return nil, "unsupported bookmark URL"
    end
    if port_str ~= nil and port_str ~= "" then
        if not port_str:match("^[0-9]+$") then
            return nil, "invalid bookmark URL"
        end
        local port = tonumber(port_str)
        if not port or port < 0 or port > 65535 then
            return nil, "invalid bookmark URL"
        end
    end
    return true
end

-- Check whether canonical main Chrome profile is currently running
local function read_file_all(path)
    local fd = C.open(path, bor(O_RDONLY, O_CLOEXEC))
    if fd < 0 then return nil end
    local chunks = {}
    local buf = ffi.new("uint8_t[4096]")
    while true do
        local r = C.read(fd, buf, 4096)
        if r > 0 then
            chunks[#chunks + 1] = ffi.string(buf, tonumber(r))
        elseif r == 0 then
            break
        else
            if ffi.errno() ~= EINTR then C.close(fd) return nil end
        end
    end
    C.close(fd)
    return table.concat(chunks)
end

local function cmdline_matches_profile(cmdline, profile)
    if not cmdline or #cmdline == 0 then return false end
    local flag = "--user-data-dir=" .. profile
    local prev_flag = false
    local pos = 1
    local len = #cmdline
    while pos <= len do
        local nul = cmdline:find("\0", pos, true)
        local arg = nul and cmdline:sub(pos, nul - 1) or cmdline:sub(pos)
        if arg == flag or (prev_flag and arg == profile) then
            return true
        end
        prev_flag = (arg == "--user-data-dir")
        if not nul then break end
        pos = nul + 1
    end
    return false
end

local function is_main_profile_running()
    local home = os.getenv("HOME") or ""
    local profile = os.getenv("ABUSH_CHROME_PROFILE")
    if not profile or profile == "" then
        if home == "" then return false end
        profile = home .. "/.config/google-chrome-main"
    end
    local proc_root = os.getenv("ABUSH_PROC_ROOT")
    if not proc_root or proc_root == "" then proc_root = "/proc" end
    local buf = ffi.new("char[512]")
    local n = C.readlink(profile .. "/SingletonLock", buf, 511)
    if n <= 0 then return false end
    local target = ffi.string(buf, tonumber(n))
    local pid = target:match("%-([0-9]+)$")
    if not pid then return false end
    local cmdline = read_file_all(proc_root .. "/" .. pid .. "/cmdline")
    return cmdline_matches_profile(cmdline, profile)
end

-- Non-blocking Unix stream socket helpers with lstat/SO_PEERCRED checks
local function connect_unix_checked(path, deadline, check_peercred)
    if #path >= 108 then return nil, "socket path is too long" end
    local my_uid = C.getuid()
    local before = ffi.new("linux_stat_t")
    if C.lstat(path, before) ~= 0 then
        local e = ffi.errno()
        return nil, (e == ENOENT) and "ENOENT" or ("socket stat failed: " .. err_str(e))
    end
    local ftype = band(before.st_mode, S_IFMT)
    if ftype == S_IFLNK or ftype ~= S_IFSOCK or before.st_uid ~= my_uid or band(before.st_mode, 511) ~= 384 then
        return nil, "socket is unsafe"
    end

    local fd = C.socket(AF_UNIX, bor(SOCK_STREAM, SOCK_NONBLOCK, SOCK_CLOEXEC), 0)
    if fd < 0 then return nil, "socket failed: " .. err_str() end

    local addr = ffi.new("sockaddr_un_t")
    addr.sun_family = AF_UNIX
    ffi.copy(addr.sun_path, path, #path)
    local addrlen = 2 + #path + 1

    local rc = C.connect(fd, addr, addrlen)
    if rc < 0 then
        local e = ffi.errno()
        if e == EINPROGRESS or e == EAGAIN then
            local ev, perr = poll_fd(fd, POLLOUT, deadline)
            if not ev then C.close(fd) return nil, perr end
            local so_err = ffi.new("int[1]")
            local optlen = ffi.new("socklen_t[1]", 4)
            if C.getsockopt(fd, SOL_SOCKET, SO_ERROR, so_err, optlen) ~= 0 or so_err[0] ~= 0 then
                e = so_err[0] ~= 0 and so_err[0] or ffi.errno()
                C.close(fd)
                return nil, (e == ECONNREFUSED) and "ECONNREFUSED" or ("connect failed: " .. err_str(e))
            end
        else
            C.close(fd)
            return nil, (e == ECONNREFUSED or e == ENOENT) and "ECONNREFUSED" or ("connect failed: " .. err_str(e))
        end
    end

    if check_peercred then
        local cred = ffi.new("ucred_t")
        local optlen = ffi.new("socklen_t[1]", ffi.sizeof("ucred_t"))
        if C.getsockopt(fd, SOL_SOCKET, SO_PEERCRED, cred, optlen) ~= 0 or cred.uid ~= my_uid then
            C.close(fd)
            return nil, "Qtile socket peer is not owned by this user"
        end
        local after = ffi.new("linux_stat_t")
        if C.lstat(path, after) ~= 0 or before.st_dev ~= after.st_dev or before.st_ino ~= after.st_ino then
            C.close(fd)
            return nil, "Qtile socket changed during connection"
        end
    end
    return fd
end

local function sock_write_all(fd, data, deadline)
    local p = ffi.cast("const uint8_t *", data)
    local len = #data
    local sent = 0
    while sent < len do
        local w = C.write(fd, p + sent, len - sent)
        if w > 0 then
            sent = sent + tonumber(w)
        elseif w < 0 then
            local e = ffi.errno()
            if e == EAGAIN then
                local ev, perr = poll_fd(fd, POLLOUT, deadline)
                if not ev then return nil, perr end
            elseif e ~= EINTR then
                return nil, "write failed: " .. err_str(e)
            end
        end
    end
    return true
end

local function sock_read_exact(fd, n, deadline, eof_msg)
    local buf = ffi.new("uint8_t[?]", n)
    local got = 0
    while got < n do
        local r = C.read(fd, buf + got, n - got)
        if r > 0 then
            got = got + tonumber(r)
        elseif r == 0 then
            return nil, eof_msg
        else
            local e = ffi.errno()
            if e == EAGAIN then
                local ev, perr = poll_fd(fd, POLLIN, deadline)
                if not ev then return nil, perr end
            elseif e ~= EINTR then
                return nil, "read failed: " .. err_str(e)
            end
        end
    end
    return ffi.string(buf, n)
end

local function sock_read_until_eof(fd, max_bytes, deadline)
    local chunks = {}
    local total = 0
    local buf = ffi.new("uint8_t[4096]")
    while true do
        local want = math.min(4096, max_bytes + 1 - total)
        local r = C.read(fd, buf, want)
        if r > 0 then
            local rn = tonumber(r)
            total = total + rn
            if total > max_bytes then return nil, "reply is too large" end
            chunks[#chunks + 1] = ffi.string(buf, rn)
        elseif r == 0 then
            return table.concat(chunks)
        else
            local e = ffi.errno()
            if e == EAGAIN then
                local ev, perr = poll_fd(fd, POLLIN, deadline)
                if not ev then return nil, perr end
            elseif e ~= EINTR then
                return nil, "read failed: " .. err_str(e)
            end
        end
    end
end

local function desktop_socket_path()
    local injected = os.getenv("ABUSH_QTILE_SOCKET")
    if injected and injected ~= "" then
        if injected:byte(1) ~= 47 then return nil, "Qtile socket path is not absolute" end
        return injected
    end
    local xdg_cache = os.getenv("XDG_CACHE_HOME")
    if not xdg_cache or xdg_cache == "" then
        local home = os.getenv("HOME") or ""
        xdg_cache = home .. "/.cache"
    end
    local dir = xdg_cache .. "/qtile"
    local display = os.getenv("WAYLAND_DISPLAY")
    if not display or display == "" then display = os.getenv("DISPLAY") end
    if display and display ~= "" then
        return dir .. "/qtilesocket." .. display
    end
    local st = ffi.new("linux_stat_t")
    for _, d in ipairs({"wayland-0", ":0"}) do
        local p = dir .. "/qtilesocket." .. d
        if C.stat(p, st) == 0 then return p end
    end
    return nil, "Qtile socket is unavailable"
end

local function focus_browser_desktop(outer_deadline)
    local deadline = math.min(monotonic_now() + DESKTOP_FOCUS_TIMEOUT, outer_deadline)
    if deadline <= monotonic_now() then return nil, "Qtile desktop switch deadline expired" end
    local path, perr = desktop_socket_path()
    if not path then return nil, perr end

    local fd, cerr = connect_unix_checked(path, deadline, true)
    if not fd then
        if cerr == CANCELLED then return nil, CANCELLED end
        if cerr == "socket is unsafe" then return nil, "Qtile socket is unsafe" end
        if cerr == "timeout" then return nil, "Qtile desktop switch deadline expired" end
        return nil, cerr
    end

    local ok, err = sock_write_all(fd, '[[["group","f"]],"toscreen",[],{},true]', deadline)
    if not ok then
        C.close(fd)
        return nil, err == "timeout" and "Qtile desktop switch deadline expired" or err
    end
    C.shutdown(fd, SHUT_WR)
    local body, rerr = sock_read_until_eof(fd, QTILE_MAX_REPLY, deadline)
    C.close(fd)
    if not body then
        if rerr == CANCELLED then return nil, CANCELLED end
        if rerr == "timeout" then return nil, "Qtile desktop switch deadline expired" end
        if rerr == "reply is too large" then return nil, "Qtile desktop switch reply is too large" end
        return nil, rerr
    end
    local parsed, int_elems, jerr = json_decode(body)
    if jerr or getmetatable(parsed) ~= JSON_ARRAY_MT or #parsed ~= 2
        or type(parsed[1]) ~= "number" or not (int_elems and int_elems[1])
    then
        return nil, "invalid Qtile desktop switch reply"
    end
    if parsed[1] ~= 0 then
        return nil, "Qtile desktop switch command failed"
    end
    if parsed[2] ~= JSON_NULL then
        return nil, "unexpected Qtile desktop switch result"
    end
    return true
end

local function random_hex_16()
    local buf = ffi.new("uint8_t[16]")
    local got = 0
    while got < 16 do
        local r = C.getrandom(buf + got, 16 - got, 0)
        if r > 0 then
            got = got + tonumber(r)
        elseif r < 0 and ffi.errno() ~= EINTR then
            return nil
        end
    end
    local out = {}
    for i = 0, 15 do out[i + 1] = string.format("%02x", buf[i]) end
    return table.concat(out)
end

local function ordinary_socket_path()
    local injected = os.getenv("ABUSH_ORDINARY_OPEN_SOCKET")
    if injected and injected ~= "" then return injected end
    local runtime = os.getenv("XDG_RUNTIME_DIR")
    if not runtime or runtime == "" then return nil end
    return runtime .. "/abush/ordinary-open.sock"
end

-- Warm fast path: directly talk to ordinary-open.sock + Qtile socket.
-- Returns: status ("completed"|"unknown"|"fallback"), message
local function try_warm_fast_path(url)
    if not is_main_profile_running() then
        return "fallback", ""
    end
    local sock_path = ordinary_socket_path()
    if not sock_path then
        return "fallback", ""
    end

    local timeout = tonumber(os.getenv("ABUSH_ORDINARY_OPEN_TIMEOUT") or "30")
    if not timeout or timeout <= 0 or timeout > LEASE_MAX_LIFETIME then
        return "fallback", ""
    end

    local start_mono = monotonic_now()
    local deadline = start_mono + math.min(timeout, BOOKMARK_OPEN_TIMEOUT)
    local fd, cerr = connect_unix_checked(sock_path, deadline, false)
    if not fd then
        if cerr == CANCELLED then
            return "unknown", "bookmark opening was cancelled; outcome is unknown"
        end
        if cerr == "ENOENT" or cerr == "ECONNREFUSED" then
            return "fallback", ""
        end
        if cerr == "socket is unsafe" then
            return "unknown", "ordinary-open: ordinary-open socket is unsafe"
        end
        return "unknown", "ordinary-open: " .. tostring(cerr)
    end

    local ord_req_id = random_hex_16()
    if not ord_req_id then
        C.close(fd)
        return "fallback", ""
    end
    local deadline_ms = realtime_now_ms() + math.floor(timeout * 1000)
    local payload = string.format(
        '{"version":1,"request_id":"%s","operation":"ordinary_open","url":%s,"focus":true,"deadline_ms":%.0f}',
        ord_req_id, json_escape(url), deadline_ms
    )
    if #payload > MAX_BROKER_MSG then
        C.close(fd)
        return "unknown", "ordinary-open: ordinary-open request is too large"
    end

    local ok, werr = sock_write_all(fd, u32_be(#payload) .. payload, deadline)
    if not ok then
        C.close(fd)
        if werr == CANCELLED then
            return "unknown", "bookmark opening was cancelled; outcome is unknown"
        end
        return "unknown", "ordinary-open: " .. tostring(werr)
    end

    local hdr, herr = sock_read_exact(fd, 4, deadline, "ordinary-open broker closed without a completion")
    if not hdr then
        C.close(fd)
        if herr == CANCELLED then
            return "unknown", "bookmark opening was cancelled; outcome is unknown"
        end
        if herr == "timeout" then
            return "unknown", "browser completion deadline expired; outcome is unknown"
        end
        return "unknown", "ordinary-open: " .. tostring(herr)
    end
    local rlen = read_u32_be(hdr)
    if rlen > MAX_BROKER_MSG then
        C.close(fd)
        return "unknown", "ordinary-open: ordinary-open completion is too large"
    end
    local body, berr = sock_read_exact(fd, rlen, deadline, "ordinary-open broker closed without a completion")
    C.close(fd)
    if not body then
        if berr == CANCELLED then
            return "unknown", "bookmark opening was cancelled; outcome is unknown"
        end
        if berr == "timeout" then
            return "unknown", "browser completion deadline expired; outcome is unknown"
        end
        return "unknown", "ordinary-open: " .. tostring(berr)
    end

    local comp, int_keys, jerr = json_decode(body)
    if jerr or getmetatable(comp) ~= JSON_OBJECT_MT then
        return "unknown", "ordinary-open: invalid ordinary-open completion"
    end
    local key_count = 0
    for _ in pairs(comp) do key_count = key_count + 1 end
    local status = comp.completion
    if key_count ~= 4
        or comp.version ~= 1 or not (int_keys and int_keys.version)
        or comp.request_id ~= ord_req_id
        or comp.operation ~= "ordinary_open_completed"
        or (status ~= "applied" and status ~= "not_applied" and status ~= "unknown")
    then
        return "unknown", "ordinary-open: invalid ordinary-open completion"
    end
    if status ~= "applied" then
        return "unknown", "ordinary-open: ordinary-open completed as " .. status
    end
    if check_cancelled() then
        return "unknown", "bookmark opening was cancelled; outcome is unknown"
    end

    local fok, ferr = focus_browser_desktop(deadline)
    if not fok then
        if ferr == CANCELLED then
            return "unknown", "bookmark opening was cancelled; outcome is unknown"
        end
        return "unknown",
            "ordinary-open: URL opened; desktop switch failed: " .. tostring(ferr)
            .. ". Do not retry the URL automatically"
    end
    if check_cancelled() then
        return "unknown", "bookmark opening was cancelled; outcome is unknown"
    end
    return "completed", ""
end

-- Cold-start / fallback launcher: spawn scripts/chromium with bounded stderr drain & cleanup
local function stop_child(pid)
    local st = ffi.new("int[1]")
    if C.waitpid(pid, st, WNOHANG) ~= 0 then return true end
    C.kill(pid, SIGTERM)
    local term_deadline = monotonic_now() + 1.0
    local pfds = ffi.new("pollfd_t[1]")
    while monotonic_now() < term_deadline do
        if C.waitpid(pid, st, WNOHANG) ~= 0 then return true end
        C.poll(pfds, 0, 10)
    end
    C.kill(pid, SIGKILL)
    local kill_deadline = monotonic_now() + 1.0
    while monotonic_now() < kill_deadline do
        if C.waitpid(pid, st, WNOHANG) ~= 0 then return true end
        C.poll(pfds, 0, 10)
    end
    return false
end

local function run_chromium_fallback(url)
    local launcher = os.getenv("ABUSH_CHROMIUM_LAUNCHER")
    if not launcher or launcher == "" then
        launcher = "/home/amos/scripts/chromium"
    end
    local fds = ffi.new("int[2]")
    if C.pipe2(fds, bor(O_NONBLOCK, O_CLOEXEC)) ~= 0 then
        return "not_started", "pipe failed: " .. err_str()
    end

    local pid = C.fork()
    if pid < 0 then
        C.close(fds[0]); C.close(fds[1])
        return "not_started", "fork failed: " .. err_str()
    elseif pid == 0 then
        if mask_saved then C.sigprocmask(SIG_SETMASK, saved_mask, nil) end
        local devnull = C.open("/dev/null", O_RDONLY)
        if devnull >= 0 then C.dup2(devnull, 0); C.close(devnull) end
        devnull = C.open("/dev/null", O_WRONLY)
        if devnull >= 0 then C.dup2(devnull, 1); C.close(devnull) end
        C.dup2(fds[1], 2)
        C.close(fds[0]); C.close(fds[1])
        local argv = ffi.new("char *[3]")
        argv[0] = ffi.cast("char *", launcher)
        argv[1] = ffi.cast("char *", url)
        argv[2] = nil
        C.execv(launcher, argv)
        C._exit(127)
    end

    C.close(fds[1])
    local rfd = fds[0]
    local deadline = monotonic_now() + BOOKMARK_OPEN_TIMEOUT
    local diag = ""
    local pipe_open = true
    local st = ffi.new("int[1]")
    local buf = ffi.new("uint8_t[4096]")
    local exited = false

    local function append_diag(chunk)
        diag = diag .. chunk
        if #diag > DIAGNOSTIC_LIMIT then
            diag = diag:sub(#diag - DIAGNOSTIC_LIMIT + 1)
        end
    end

    while true do
        if check_cancelled() then
            stop_child(pid)
            C.close(rfd)
            return "unknown", "bookmark opening was cancelled; outcome is unknown"
        end
        local rem = deadline - monotonic_now()
        if rem <= 0 then
            stop_child(pid)
            C.close(rfd)
            return "unknown", "browser completion deadline expired; outcome is unknown"
        end
        local w = C.waitpid(pid, st, WNOHANG)
        if w == pid then
            exited = true
            break
        end
        if pipe_open then
            local pfds = ffi.new("pollfd_t[3]")
            pfds[0].fd = rfd
            pfds[0].events = POLLIN
            pfds[1].fd = 0
            pfds[1].events = POLLHUP
            local nfds = 2
            if sig_fd >= 0 then
                pfds[2].fd = sig_fd
                pfds[2].events = POLLIN
                nfds = 3
            end
            local ms = math.min(50, math.max(1, math.ceil(rem * 1000)))
            local pr = C.poll(pfds, nfds, ms)
            if pr > 0 then
                if nfds == 3 and band(pfds[2].revents, POLLIN) ~= 0 then
                    check_cancelled()
                elseif band(pfds[1].revents, bor(POLLHUP, POLLERR)) ~= 0 then
                    stdin_closed_flag = true
                    cancelled_flag = true
                elseif band(pfds[0].revents, bor(POLLIN, POLLHUP, POLLERR)) ~= 0 then
                    while true do
                        local r = C.read(rfd, buf, 4096)
                        if r > 0 then
                            append_diag(ffi.string(buf, tonumber(r)))
                        elseif r == 0 then
                            pipe_open = false
                            break
                        else
                            if ffi.errno() ~= EINTR then break end
                        end
                    end
                end
            end
        else
            local pfds = ffi.new("pollfd_t[2]")
            pfds[0].fd = 0
            pfds[0].events = POLLHUP
            local nfds = 1
            if sig_fd >= 0 then
                pfds[1].fd = sig_fd
                pfds[1].events = POLLIN
                nfds = 2
            end
            local ms = math.min(20, math.max(1, math.ceil(rem * 1000)))
            local pr = C.poll(pfds, nfds, ms)
            if pr > 0 and band(pfds[0].revents, bor(POLLHUP, POLLERR)) ~= 0 then
                stdin_closed_flag = true
                cancelled_flag = true
            end
        end
    end

    if pipe_open then
        for _ = 1, 16 do
            local r = C.read(rfd, buf, 4096)
            if r > 0 then append_diag(ffi.string(buf, tonumber(r))) else break end
        end
    end
    C.close(rfd)

    if check_cancelled() then
        return "unknown", "bookmark opening was cancelled; outcome is unknown"
    end
    if exited then
        local code = band(rshift(st[0], 8), 0xFF)
        local sig = band(st[0], 0x7F)
        if sig == 0 and code == 0 then
            return "completed", ""
        end
        diag = diag:gsub("^%s+", ""):gsub("%s+$", "")
        if diag == "" then
            diag = string.format("browser launcher exited with status %d", sig ~= 0 and -sig or code)
        end
        return "unknown", diag
    end
    return "unknown", "browser launcher failed"
end

-- Delegate non-openInBrowser rofi commands to main.py by re-feeding stdin via pipe
local function delegate_to_python(raw_frame)
    local py_host = os.getenv("ROFI_CHROME_HOST_PY")
    if not py_host or py_host == "" then
        local script = arg[0] or ""
        local dir = script:match("^(.*)/[^/]+$") or "."
        py_host = dir .. "/main.py"
    end
    local fds = ffi.new("int[2]")
    if C.pipe2(fds, 0) ~= 0 then os.exit(1) end
    local pid = C.fork()
    if pid < 0 then os.exit(1) end
    if pid == 0 then
        C.close(fds[0])
        write_all_fd(fds[1], raw_frame)
        local buf = ffi.new("uint8_t[4096]")
        while true do
            local r = C.read(0, buf, 4096)
            if r > 0 then
                if not write_all_fd(fds[1], ffi.string(buf, tonumber(r))) then break end
            elseif r == 0 then
                break
            else
                if ffi.errno() ~= EINTR then break end
            end
        end
        C.close(fds[1])
        C._exit(0)
    end
    C.close(fds[1])
    C.dup2(fds[0], 0)
    C.close(fds[0])
    local argv = ffi.new("char *[5]")
    argv[0] = ffi.cast("char *", "python3")
    argv[1] = ffi.cast("char *", "-B")
    argv[2] = ffi.cast("char *", "-S")
    argv[3] = ffi.cast("char *", py_host)
    argv[4] = nil
    C.execvp("python3", argv)
    os.exit(127)
end

local function handle_open_in_browser(msg, int_keys)
    local request_id = msg.request_id
    local key_count = 0
    for _ in pairs(msg) do key_count = key_count + 1 end
    if key_count ~= 4
        or msg.version ~= 1 or not (int_keys and int_keys.version)
        or msg.info ~= "openInBrowser"
        or type(request_id) ~= "string" or #request_id ~= 32 or not request_id:match("^[0-9a-f]+$")
    then
        send_native_response(request_id, "not_started", "invalid bookmark open request")
        return
    end
    local param = msg.param
    if getmetatable(param) ~= JSON_OBJECT_MT then
        send_native_response(request_id, "not_started", "invalid bookmark open parameters")
        return
    end
    local pcount = 0
    for _ in pairs(param) do pcount = pcount + 1 end
    if pcount ~= 1 or param.url == nil then
        send_native_response(request_id, "not_started", "invalid bookmark open parameters")
        return
    end
    local ok, uerr = validate_bookmark_url(param.url)
    if not ok then
        send_native_response(request_id, "not_started", uerr)
        return
    end

    setup_signals()
    if check_cancelled() then
        restore_signals()
        send_native_response(request_id, "unknown", "bookmark opening was cancelled before launch")
        return
    end

    local status, message = try_warm_fast_path(param.url)
    if status == "fallback" then
        status, message = run_chromium_fallback(param.url)
    end
    local was_cancelled = cancelled_flag
    restore_signals()
    send_native_response(request_id, status, message)
    return not was_cancelled
end

local function main()
    local persistent = false
    while true do
        local hdr, herr = read_exact_fd(0, 4)
        if not hdr then
            if herr ~= "eof" then io.stderr:write(herr .. "\n") end
            return
        end
        local size = read_u32_le(hdr)
        if size > MAX_NATIVE_MSG then
            io.stderr:write("native message is too large\n")
            return
        end
        local body, berr = read_exact_fd(0, size)
        if not body then
            io.stderr:write(tostring(berr) .. "\n")
            return
        end
        local msg, int_keys, jerr = json_decode(body)
        if jerr or getmetatable(msg) ~= JSON_OBJECT_MT then
            io.stderr:write("invalid native JSON: " .. tostring(jerr) .. "\n")
            return
        end
        if msg.info == "bookmarkPortInit" then
            persistent = true
            local payload = '{"version":1,"info":"bookmarkPortInit","ready":true}'
            if not write_all_fd(1, u32_le(#payload) .. payload) then
                return
            end
        elseif msg.info == "openInBrowser" then
            if handle_open_in_browser(msg, int_keys) == false or not persistent then
                return
            end
        elseif not persistent then
            delegate_to_python(hdr .. body)
            return
        else
            return
        end
    end
end

main()
