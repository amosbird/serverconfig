# Namespace Test Host-Zero-Impact Design

## Goal

Make `network/test-reconfigure.sh` prove that its synthetic routes, rules, interfaces, ipsets, and
iptables state never survive in the host network namespace.

## Design

Keep all mutation tests inside the existing anonymous `unshare -rn --mount` namespace. Change the
outer harness from `exec unshare` to a bounded child invocation so the outer process regains control
after the namespace exits.

Before launching the child, snapshot host sentinels for the synthetic route prefixes, policy rules,
interfaces, and test ipset names. After the child exits—successfully or not—inspect the host again.
The harness fails if the post-run state differs or if a synthetic object that was absent before is
present afterward. Preserve the child's original failure status when the host remains clean; use a
distinct failure message when host state changed.

The temporary work directory remains protected by the outer `EXIT` trap. Anonymous namespace teardown
continues to be the primary cleanup mechanism; the host check is a verification gate, not a cleanup
script that could hide leakage.

## Verification

Static tests assert that the harness runs `unshare` as a child and performs a post-run host-state
comparison. The namespace suite must pass, its temporary directory must disappear, and fresh host
checks must show no synthetic prefixes, rules, interfaces, namespaces, or `ioa_intranet` test set.
