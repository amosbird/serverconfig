// SPDX-License-Identifier: GPL-2.0
// Bound how long iOA's own TCP connections may sit with unacknowledged data.
//
// Attached by systemd to ngnclient.service's cgroup (BPFProgram=sock_ops:...), so it sees every
// socket iOA.bin and SmartGateAgent open and nothing else. On each active open it sets
// TCP_USER_TIMEOUT before the SYN leaves. A connection that then has data in flight with no ACK
// for that long is aborted by the kernel with ETIMEDOUT, which iOA already handles by reconnecting.
//
// Without it the stock retransmission budget (tcp_retries2=15) keeps a dead connection "alive" for
// ten to fifteen minutes after a suspend or a network change. iOA's ngn heartbeat, the only thing
// that renews its TGT, keeps writing into such a connection, and every intranet request is refused
// with 103009 until the kernel finally gives up. See network/README.md.
//
// Loopback is skipped: iOA's GUI channels (:56100/:56101) and local resolvers cannot stall that way,
// and a deliberately idle local peer must never be torn down by a timeout meant for the WAN.

#include <linux/bpf.h>
#include <linux/in.h>
#include <linux/tcp.h>
#include <bpf/bpf_endian.h>
#include <bpf/bpf_helpers.h>

#ifndef IOA_USER_TIMEOUT_MS
#define IOA_USER_TIMEOUT_MS 30000
#endif

#define AF_INET 2
#define AF_INET6 10

static __always_inline int is_loopback(const struct bpf_sock_ops *skops)
{
    if (skops->family == AF_INET)
        return (bpf_ntohl(skops->remote_ip4) >> 24) == 127;
    if (skops->family == AF_INET6) {
        __u32 a = skops->remote_ip6[0], b = skops->remote_ip6[1];
        __u32 c = skops->remote_ip6[2], d = skops->remote_ip6[3];
        if (!a && !b && !c && d == bpf_htonl(1))
            return 1;  // ::1
        if (!a && !b && c == bpf_htonl(0xffff))
            return (bpf_ntohl(d) >> 24) == 127;  // ::ffff:127.0.0.0/104
    }
    return 0;
}

SEC("sockops")
int ioa_user_timeout(struct bpf_sock_ops *skops)
{
    int timeout = IOA_USER_TIMEOUT_MS;

    if (skops->op == BPF_SOCK_OPS_TCP_CONNECT_CB && !is_loopback(skops))
        bpf_setsockopt(skops, IPPROTO_TCP, TCP_USER_TIMEOUT, &timeout, sizeof(timeout));
    // sock_ops must return 1; anything else is reported back to the stack as a failure.
    return 1;
}

char LICENSE[] SEC("license") = "GPL";
