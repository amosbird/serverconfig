#!/usr/bin/env python3

import datetime
import importlib.machinery
import importlib.util
import pathlib
import sys
import unittest

ROOT = pathlib.Path(__file__).parents[1]
INDICATOR = ROOT / "scripts/clock-indicator"
STARTUP = ROOT / "scripts/startup"

sys.path.insert(0, str(ROOT / "scripts"))
import trayorder

loader = importlib.machinery.SourceFileLoader("clock_indicator", str(INDICATOR))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)

SHANGHAI = datetime.timezone(datetime.timedelta(hours=8))
MOMENT = datetime.datetime(2026, 9, 18, 22, 9, 30, tzinfo=SHANGHAI)


class ClockIndicatorTest(unittest.TestCase):
    def test_the_card_shows_the_time_over_the_date(self):
        self.assertEqual(module.icon_lines(MOMENT), ("22:09", "9月18日 周五"))
        self.assertEqual(
            module.icon_lines(MOMENT.replace(month=1, day=1, hour=0, minute=5)),
            ("00:05", "1月1日 周四"),
        )

    def test_the_card_spans_two_slots_and_is_handed_out_per_slot(self):
        source = INDICATOR.read_text()
        # A date needs more width than a 96 px slot, and stalonetray gives an
        # XEmbed icon exactly one slot, so the card is drawn once and split.
        self.assertIn("SLOTS = 2", source)
        self.assertIn("DESIGN_WIDTH = SLOTS * DESIGN_SIZE", source)
        self.assertIn("cairo.ImageSurface(cairo.FORMAT_ARGB32, SLOTS * size, size)", source)
        self.assertIn(
            "Gdk.pixbuf_get_from_surface(self.card(moment, size), slot * size, 0, size, size)",
            source,
        )
        # Only the outer corners are rounded, so the halves join seamlessly.
        self.assertIn(
            "self.rounded_rectangle(context, 0, 0, DESIGN_WIDTH, DESIGN_SIZE, 18)", source
        )

    def test_the_halves_dock_with_nothing_in_between(self):
        source = INDICATOR.read_text()
        dock = source.split("    def dock(self):", 1)[1].split("    def undock", 1)[0]
        # At login every indicator docks in the same instant, so a pause
        # between the halves hands one of them the slot in the middle.
        self.assertIn("for slot in range(SLOTS):", dock)
        self.assertNotIn("timeout_add", dock)

    def test_the_bar_tracks_the_day(self):
        midnight = MOMENT.replace(hour=0, minute=0, second=0)
        self.assertEqual(module.day_fraction(midnight), 0)
        self.assertEqual(module.day_fraction(midnight.replace(hour=12)), 0.5)
        self.assertAlmostEqual(module.day_fraction(MOMENT), 0.92326, places=4)

    def test_redraws_land_just_after_the_minute_flips(self):
        self.assertEqual(module.ms_to_next_minute(MOMENT.replace(second=0, microsecond=0)), 60_020)
        self.assertEqual(module.ms_to_next_minute(MOMENT), 30_020)
        # A redraw that happens a hair late must not schedule a zero timeout.
        late = MOMENT.replace(second=59, microsecond=999_000)
        self.assertEqual(module.ms_to_next_minute(late), 50)

    def test_the_tooltip_carries_what_the_icon_cannot(self):
        lines = module.tooltip_lines(MOMENT)
        self.assertEqual(lines[0], "2026-09-18 周五")
        self.assertIn("22:09:30", lines[1])
        self.assertIn("(UTC+08:00)", lines[1])
        self.assertIn("ISO week 38  ·  day 261 of 2026", lines[2])
        self.assertIn("UTC 14:09", lines[3])
        self.assertIn(f"Unix {int(MOMENT.timestamp())}", lines[3])

    def test_every_copy_format_is_unambiguous(self):
        formats = dict(module.copy_formats(MOMENT))
        self.assertEqual(formats["Copy date"], "2026-09-18")
        self.assertEqual(formats["Copy time"], "22:09:30")
        self.assertEqual(formats["Copy date and time"], "2026-09-18 22:09:30")
        self.assertEqual(formats["Copy ISO 8601"], "2026-09-18T22:09:30+08:00")
        self.assertEqual(formats["Copy Unix time"], str(int(MOMENT.timestamp())))

    def test_the_clock_holds_the_last_two_slots_of_the_row(self):
        source = INDICATOR.read_text()
        # stalonetray appends icons as applications dock, so the card only
        # stays rightmost by re-docking after them; trayorder decides when.
        self.assertIn("import trayorder", source)
        self.assertIn("self.tail = trayorder.Tail(INSTANCE, self.dock, self.undock)", source)
        self.assertIn('INSTANCE = "clock-indicator"', source)
        self.assertEqual(trayorder.ORDER[-1], "clock-indicator")
        self.assertEqual(trayorder.SLOTS["clock-indicator"], module.SLOTS)
        self.assertIn("    def undock(self):\n        self.icons = []", source)

    def test_icon_is_hidpi_and_cjk_capable(self):
        source = INDICATOR.read_text()
        self.assertIn("ICON_SIZE = 96", source)
        self.assertIn("DESIGN_SIZE = 96", source)
        self.assertIn('icon.connect("size-changed", self.on_icon_size_changed)', source)
        # Cairo's toy text API cannot draw 周五.
        self.assertIn("PangoCairo.show_layout", source)
        self.assertIn('FONT = "Noto Sans CJK SC"', source)
        self.assertIn("faulthandler.enable()", source)

    def test_a_resumed_machine_does_not_show_a_stale_time(self):
        source = INDICATOR.read_text()
        self.assertIn('"PrepareForSleep"', source)
        self.assertIn("def on_prepare_for_sleep(self", source)

    def test_startup_and_restore_deploy_the_indicator(self):
        startup = STARTUP.read_text()
        self.assertIn(
            'run "clock-indicator" systemctl --user start clock-indicator.service', startup
        )
        self.assertLess(
            startup.index('run_bg "tray" tray'),
            startup.index('run "clock-indicator"'),
        )
        unit = (ROOT / "systemd/clock-indicator.service").read_text()
        self.assertIn("Restart=always", unit)
        self.assertIn("GDK_SCALE=2", unit)
        self.assertIn(
            'install -Dm644 "$DIR/systemd/clock-indicator.service"',
            (ROOT / "restore.sh").read_text(),
        )


if __name__ == "__main__":
    unittest.main()
