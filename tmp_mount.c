#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <sched.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

static inline int xusleep(useconds_t usec) {
    struct timespec waittime = { .tv_sec = usec / 1000000L, .tv_nsec = (usec % 1000000L) * 1000 };
    return nanosleep(&waittime, NULL);
}

static inline int write_all(int fd, const void* buf, size_t count) {
    while (count) {
        ssize_t tmp;

        errno = 0;
        tmp = write(fd, buf, count);
        if (tmp > 0) {
            count -= tmp;
            if (count)
                buf = (const void*)((const char*)buf + tmp);
        } else if (errno != EINTR && errno != EAGAIN)
            return -1;

        if (errno == EAGAIN)
            xusleep(250000);
    }
    return 0;
}

static inline __attribute__((__format__(printf, 2, 3))) int err(int ec, const char* fmt, ...) {
    va_list args;

    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n%s\n", strerror(errno));
    _exit(ec);
}

static inline __attribute__((__format__(printf, 2, 3))) int xasprintf(
    char** strp, const char* fmt, ...) {
    int ret;
    va_list args;

    va_start(args, fmt);
    ret = vasprintf(&(*strp), fmt, args);
    va_end(args);
    if (ret < 0)
        err(EXIT_FAILURE, "cannot allocate string");
    return ret;
}

static void setgroups_deny() {
    const char* file = "/proc/self/setgroups";
    const char* cmd = "deny";
    int fd = open(file, O_WRONLY);
    if (fd < 0) {
        if (errno == ENOENT)
            return;
        err(EXIT_FAILURE, "cannot open %s", file);
    }

    if (write_all(fd, cmd, strlen(cmd)))
        err(EXIT_FAILURE, "write failed %s", file);
    close(fd);
}

void map_id(const char* file, uint32_t from, uint32_t to) {
    int fd = open(file, O_WRONLY);
    if (fd < 0)
        err(EXIT_FAILURE, "cannot open %s", file);

    char* buf;
    xasprintf(&buf, "%u %u 1", from, to);
    if (write_all(fd, buf, strlen(buf)))
        err(EXIT_FAILURE, "write failed %s", file);
    free(buf);
    close(fd);
};

// ~/toolchain/bin/gcc tmp_mount.c -o /tmp/gentoo/tmp_mount
int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Usage: %s <cmd> ...\n", argv[0]);
        return 1;
    }

    char* cmd = argv[1];

    uid_t real_euid = geteuid();
    gid_t real_egid = getegid();

    if (-1 == unshare(CLONE_NEWNS | CLONE_NEWUSER))
        err(EXIT_FAILURE, "unshare failed");

    if (mount("tmpfs", "/tmp", "tmpfs", 0, NULL) != 0)
        err(EXIT_FAILURE, "mount tmp dir failed");

    setgroups_deny();
    map_id("/proc/self/uid_map", real_euid, real_euid);
    map_id("/proc/self/gid_map", real_egid, real_egid);

    if (setgid(real_egid) != 0)
        err(EXIT_FAILURE, "setgid failed %d", real_egid);

    if (setuid(real_euid) != 0)
        err(EXIT_FAILURE, "setuid failed %d", real_euid);

    execvp(cmd, argv + 1);
}
