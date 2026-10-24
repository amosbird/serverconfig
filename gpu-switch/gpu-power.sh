#!/usr/bin/env bash
# gpu-power: on battery, unload an idle nvidia driver so the dGPU can drop to D3cold.
#
# This Turing dGPU has no working RTD3 under the proprietary driver: while the nvidia module is
# loaded the card sits in D0 at ~10-15W even with nothing using it. The module is blacklisted on
# the kernel cmdline, but any Vulkan/GL probe (Electron, Chrome, vulkaninfo) re-loads it through
# the setuid nvidia-modprobe. On battery we unload it again; `modprobe -r` refuses while any
# process holds /dev/nvidia*, so active prime-run/CUDA work is never interrupted.
#
# On AC nothing is touched: the driver stays loaded if something loaded it.
# D3cold without a driver needs runtime PM allowed on the card: tlp.conf RUNTIME_PM_ENABLE.
# Triggered by gpu-power.timer (every minute) and 99-gpu-power.rules (AC plug/unplug).

[[ -d /sys/bus/pci/devices/0000:01:00.0 ]] || exit 0
[[ $(cat /sys/class/power_supply/AC/online) == 1 ]] && exit 0
[[ -d /sys/module/nvidia ]] || exit 0

if modprobe -r nvidia_uvm nvidia_drm nvidia_modeset nvidia 2>/dev/null; then
    echo "<5>gpu-power: unloaded idle nvidia driver"
fi
