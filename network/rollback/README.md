# Rollback artifacts

Files from the pre-iwd stack (netctl + dhcpcd + wpa_supplicant), kept solely so
`migrate.sh rollback` can restore a working network. They are no longer
deployed by `restore.sh`.

| file | restored to |
|---|---|
| `90-wired-netctl.rules` | `/etc/udev/rules.d/` — starts the netctl profile on cable plug |
| `90-amos-dhcp` | `/usr/lib/dhcpcd/dhcpcd-hooks/90-amos` — dhcpcd DNS/routing hook |

Delete this directory once the new stack has proven itself and rollback is no
longer wanted.
