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

int main(int argc, char* argv[]) {
    uid_t real_euid = geteuid();
    gid_t real_egid = getegid();

    uid_t uid;
    gid_t gid;
    char* tmp_dir;
    char* home_dir;
    char* cmd;
    if (real_euid == 0) {
        if (argc < 6) {
            printf("Usage: %s <uid> <gid> <tmp_dir> <home_dir> <cmd> ...\n", argv[0]);
            return 1;
        }

        char *endptr1, *endptr2;
        uid = strtol(argv[1], &endptr1, 10);
        gid = strtol(argv[2], &endptr2, 10);

        if (*endptr1 != '\0' || *endptr2 != '\0') {
            err(EXIT_FAILURE, "Parsing error: Non-integer characters detected.");
        }

        tmp_dir = argv[3];
        home_dir = argv[4];
        cmd = argv[5];
    } else {
        if (argc < 4) {
            printf("Usage: %s <tmp_dir> <home_dir> <cmd> ...\n", argv[0]);
            return 1;
        }
        tmp_dir = argv[1];
        home_dir = argv[2];
        cmd = argv[3];
        uid = real_euid;
        gid = real_egid;
    }

    if (real_euid == 0) {
        if (-1 == unshare(CLONE_NEWNS))
            err(EXIT_FAILURE, "unshare failed");
    } else {
        if (-1 == unshare(CLONE_NEWNS | CLONE_NEWUSER))
            err(EXIT_FAILURE, "unshare failed");
    }

    if (mount("none", "/", NULL, MS_REC | MS_PRIVATE, NULL) != 0)
        err(EXIT_FAILURE, "cannot change root filesystem privacy");

    if (mount(tmp_dir, "/tmp", NULL, MS_BIND, NULL) != 0)
        err(EXIT_FAILURE, "mount gentoo tmp dir failed");

    if (mount(home_dir, "/tmp/gentoo/home/amos", NULL, MS_BIND, NULL) != 0)
        err(EXIT_FAILURE, "mount gentoo home dir failed");

    if (real_euid != 0) {
        setgroups_deny();
        map_id("/proc/self/uid_map", real_euid, real_euid);
        map_id("/proc/self/gid_map", real_egid, real_egid);
    }

    if (setresgid(gid, gid, gid) != 0)
        err(EXIT_FAILURE, "setgid failed %d", gid);

    if (setresuid(uid, uid, uid) != 0)
        err(EXIT_FAILURE, "setuid failed %d", uid);

    if (real_euid != 0) {
        execvp(cmd, argv + 3);
    } else {
        execvp(cmd, argv + 5);
    }
}
