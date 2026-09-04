# reloadtouchpad health module

`rmi4_touchpad_health` exposes the Synaptics RMI4 F54 normalized 16-bit image at:

```text
/sys/kernel/debug/rmi4_touchpad_normalized_image
```

`scripts/reloadtouchpad` samples this image while the touchpad is idle. It accepts an
initialization after three consecutive frames satisfy the limits measured on the local
TM3625-010, and otherwise rebinds `serio1` to `psmouse` before trying again.

## Install

```bash
tools/reloadtouchpad-health/install
```

The installer registers a DKMS module and builds it for every installed kernel that has
headers. Arch's standard DKMS pacman hooks rebuild it for future kernel upgrades.

## Uninstall

```bash
tools/reloadtouchpad-health/uninstall
```

## Notes

- The module sends only the F54 `GET_REPORT` command. It does not reset the device or write
  firmware/NVM.
- `scripts/reloadtouchpad` performs the reset and verifies `LEN040f` before rebinding.
- Keep fingers off the touchpad while checking health.
- The module mirrors private RMI4 structures because `rmi_bus.h` is not installed with kernel
  headers. Kernel API changes may require updating the module even when DKMS compilation succeeds.
