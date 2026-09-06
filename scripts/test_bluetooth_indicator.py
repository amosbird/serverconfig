#!/usr/bin/env python3

import importlib.machinery
import importlib.util
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
            ("A2DP · AAC", "A"),
        )
        self.assertEqual(
            module.profile_details(
                "headset-head-unit", "Headset Head Unit (HSP/HFP, codec MSBC)"
            ),
            ("HFP · MSBC", "H"),
        )

    def test_indicator_is_global_but_freeclip_uses_session_manager(self):
        source = INDICATOR.read_text()
        self.assertIn('properties.get("device.bus") == "bluetooth"', source)
        self.assertIn('["pactl", "set-card-profile", card_name, profile]', source)
        self.assertIn('["bluetooth-profile", "a2dp"]', source)
        self.assertIn('["bluetooth-profile", "hfp"]', source)
        self.assertNotIn("bluetoothctl", source)
        self.assertNotIn("rfkill", source)
        self.assertNotIn("systemctl", source)

    def test_indicator_is_event_driven(self):
        source = INDICATOR.read_text()
        self.assertIn("signal_subscribe", source)
        self.assertIn('["pactl", "subscribe"]', source)
        self.assertNotIn("timeout_add", source)

    def test_startup_launches_indicator(self):
        self.assertIn('run_bg "bluetooth-indicator" bluetooth-indicator', STARTUP.read_text())


if __name__ == "__main__":
    unittest.main()
