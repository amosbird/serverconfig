#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import os
import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
INDICATOR = ROOT / "scripts/bluetooth-indicator"
STARTUP = ROOT / "scripts/startup"

loader = importlib.machinery.SourceFileLoader("bluetooth_indicator", str(INDICATOR))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


class BluetoothIndicatorTest(unittest.TestCase):
    def test_profile_labels_are_compact(self):
        self.assertEqual(
            module.profile_details(
                "a2dp-sink", "High Fidelity Playback (A2DP Sink, codec AAC)"
            ),
            ("A2DP · AAC", "A2DP", "High quality playback"),
        )
        self.assertEqual(
            module.profile_details(
                "headset-head-unit", "Headset Head Unit (HSP/HFP, codec MSBC)"
            ),
            ("HFP · MSBC", "HFP", "Calls + microphone"),
        )

    def test_profile_is_prominent_in_icon_and_menu(self):
        source = INDICATOR.read_text()
        self.assertIn("ICON_SIZE = 96", source)
        self.assertIn("DESIGN_SIZE = 96", source)
        self.assertIn('self.icon.connect("size-changed", self.on_icon_size_changed)', source)
        self.assertIn("return True", source)
        self.assertIn('"title": "A2DP"', source)
        self.assertIn('"title": "HFP"', source)
        self.assertIn('"High quality playback"', source)
        self.assertIn('"Calls + microphone"', source)

    def test_managed_freeclip_displays_effective_route(self):
        indicator = module.BluetoothIndicator.__new__(module.BluetoothIndicator)
        for state, expected in (
            ("A2DP_READY", ("A2DP", "A2DP", "High quality playback")),
            ("HFP_READY", ("HFP", "HFP", "Calls + microphone")),
            (
                "LOCAL_FALLBACK",
                ("LOCAL", "LOCAL", "Bluetooth unavailable · using built-in audio"),
            ),
            ("A2DP_STARTING", ("···", "···", "Switching audio profile")),
        ):
            indicator.session = {"state": state}
            self.assertEqual(indicator.managed_profile(), expected)
        indicator.session = {"state": "LOCAL_FALLBACK"}
        self.assertEqual(
            indicator.effective_profile_name(module.FREECLIP_ADDRESS, "headset-head-unit"),
            "",
        )
        self.assertEqual(
            indicator.effective_profile_name("AA:BB", "headset-head-unit"),
            "headset-head-unit",
        )

    def test_indicator_is_global_but_freeclip_uses_session_manager(self):
        source = INDICATOR.read_text()
        self.assertIn('properties.get("device.bus") == "bluetooth"', source)
        self.assertIn('["pactl", "set-card-profile", card_name, profile]', source)
        self.assertIn('[BLUETOOTH_PROFILE, "a2dp"]', source)
        self.assertIn('[BLUETOOTH_PROFILE, "hfp"]', source)
        self.assertNotIn("bluetoothctl", source)
        self.assertNotIn("rfkill", source)
        self.assertNotIn("systemctl", source)

    def test_repo_scripts_are_resolved_without_path(self):
        # The systemd user environment has no ~/scripts in PATH, so the
        # indicator must reach the sibling CLI by absolute path (bzmenu is
        # a system binary and stays on PATH).
        self.assertTrue(os.path.isfile(module.BLUETOOTH_PROFILE))
        source = INDICATOR.read_text()
        self.assertNotIn('["bluetooth-profile",', source)

    def test_indicator_is_event_driven(self):
        source = INDICATOR.read_text()
        self.assertIn("signal_subscribe", source)
        self.assertIn('["pactl", "subscribe"]', source)
        self.assertIn('["pw-metadata", "-n", "default", "-m"]', source)
        self.assertIn('[BLUETOOTH_PROFILE, "status", "--json"]', source)
        self.assertIn('os.read(stream.fileno(), 65536)', source)
        self.assertIn("os.set_blocking", source)
        self.assertNotIn("stream.readline()", source)
        # One-shot timers (dead pipe respawn, failed-read retry, metadata
        # watcher recycle debounce) are fine, but there must be no periodic
        # polling of refresh().
        self.assertEqual(source.count("timeout_add"), 3)
        self.assertIn("GLib.timeout_add_seconds(2, self.respawn_event_source", source)
        self.assertIn("GLib.timeout_add_seconds(3, self.retry_refresh)", source)
        self.assertIn("GLib.timeout_add_seconds(10, self.respawn_metadata_watcher)", source)

    def test_metadata_watcher_is_recycled_on_graph_churn(self):
        # pw-metadata silently goes deaf when the "default" metadata object
        # is recreated by a WirePlumber restart; the watcher must be
        # recycled whenever the graph churns.
        source = INDICATOR.read_text()
        self.assertIn("def kick_metadata_watcher(self):", source)
        self.assertEqual(source.count("self.kick_metadata_watcher()"), 2)

    def test_failed_status_read_keeps_last_known_state(self):
        source = INDICATOR.read_text()
        self.assertIn("if session and session.get(\"state\"):", source)
        self.assertIn("return  # keep the last known cards", source)
        # FreeClip falls back to the physical card profile when the session
        # state is unreadable instead of showing LINK.
        self.assertIn("fall back to the physical profile", source)

    def test_indicator_respawns_dead_event_sources(self):
        source = INDICATOR.read_text()
        self.assertIn("respawning:", source)
        self.assertIn("faulthandler.enable()", source)

    def test_startup_launches_indicator(self):
        self.assertIn(
            'run "bluetooth-indicator" systemctl --user start bluetooth-indicator.service',
            STARTUP.read_text(),
        )
        unit = (ROOT / "systemd/bluetooth-indicator.service").read_text()
        self.assertIn("Restart=always", unit)
        self.assertIn("GDK_SCALE=2", unit)


if __name__ == "__main__":
    unittest.main()
