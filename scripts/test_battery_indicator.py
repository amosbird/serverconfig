#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
INDICATOR = ROOT / "scripts/battery-indicator"
STARTUP = ROOT / "scripts/startup"

loader = importlib.machinery.SourceFileLoader("battery_indicator", str(INDICATOR))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


class BatteryIndicatorTest(unittest.TestCase):
    def test_colour_tracks_level_and_charging_wins(self):
        self.assertEqual(module.level_color(100, False), (0.12, 0.62, 0.48))
        self.assertEqual(module.level_color(60, False), (0.12, 0.62, 0.48))
        self.assertEqual(module.level_color(59, False), (0.85, 0.62, 0.10))
        self.assertEqual(module.level_color(20, False), (0.90, 0.45, 0.10))
        self.assertEqual(module.level_color(3, False), (0.78, 0.22, 0.22))
        # A plugged-in machine must never show an alarming icon.
        self.assertEqual(module.level_color(3, True), module.CHARGING_COLOR)
        self.assertEqual(module.level_color(None, False), module.UNKNOWN_COLOR)

    def test_durations_are_human_readable(self):
        self.assertEqual(module.format_duration(0), "")
        self.assertEqual(module.format_duration(-1), "")
        self.assertEqual(module.format_duration(30), "less than a minute")
        self.assertEqual(module.format_duration(3166), "52 min")
        self.assertEqual(module.format_duration(7260), "2 h 1 min")

    def test_icon_is_hidpi_and_shows_the_percentage(self):
        source = INDICATOR.read_text()
        self.assertIn("ICON_SIZE = 96", source)
        self.assertIn("DESIGN_SIZE = 96", source)
        self.assertIn('self.icon.connect("size-changed", self.on_icon_size_changed)', source)
        self.assertIn('label = "--" if percentage is None else str(round(percentage))', source)

    def test_state_labels_cover_every_upower_state(self):
        indicator = module.BatteryIndicator.__new__(module.BatteryIndicator)
        for state, expected in (
            (1, "Charging"),
            (5, "Charging"),
            (2, "Discharging"),
            (3, "Empty"),
            (4, "Fully charged"),
            (0, "Unknown"),
        ):
            indicator.state = state
            self.assertEqual(indicator.state_label(), expected)

    def test_low_battery_warns_once_per_crossing(self):
        indicator = module.BatteryIndicator.__new__(module.BatteryIndicator)
        indicator.warned = set()
        indicator.remaining = 0
        indicator.charging = False
        fired = []
        indicator.notify = fired.append
        for percentage in (14, 13, 12):
            indicator.percentage = percentage
            indicator.warn_if_low()
        self.assertEqual(fired, [15])
        indicator.percentage = 4
        indicator.warn_if_low()
        self.assertEqual(fired, [15, 5])
        # Plugging in rearms the warnings.
        indicator.charging = True
        indicator.warn_if_low()
        self.assertEqual(indicator.warned, set())

    def test_indicator_is_event_driven_with_a_sysfs_fallback(self):
        source = INDICATOR.read_text()
        self.assertIn("signal_subscribe", source)
        self.assertIn('"PropertiesChanged"', source)
        self.assertIn("DISPLAY_DEVICE", source)
        # UPower going away must not blank the icon.
        self.assertIn('capacity = read_sysfs("capacity")', source)
        self.assertIn('status = read_sysfs("status")', source)
        self.assertIn("faulthandler.enable()", source)

    def test_startup_and_restore_deploy_the_indicator(self):
        self.assertIn(
            'run "battery-indicator" systemctl --user start battery-indicator.service',
            STARTUP.read_text(),
        )
        unit = (ROOT / "systemd/battery-indicator.service").read_text()
        self.assertIn("Restart=always", unit)
        self.assertIn("GDK_SCALE=2", unit)
        self.assertIn(
            'install -Dm644 "$DIR/systemd/battery-indicator.service"',
            (ROOT / "restore.sh").read_text(),
        )


if __name__ == "__main__":
    unittest.main()
