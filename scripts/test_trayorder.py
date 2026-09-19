#!/usr/bin/env python3

import pathlib
import sys
import unittest

ROOT = pathlib.Path(__file__).parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

import trayorder

OTHERS = ["iOALinux", "Telegram", "flameshot", "copyq"]
TAIL = ["fcitx5-indicator", "bluetooth-indicator", "battery-indicator"]
TAIL += ["clock-indicator", "clock-indicator"]


class Tray:
    """A stand-in tray: an icon that docks lands at the end of the row."""

    def __init__(self, row):
        self.row = list(row)
        self.docks = []
        trayorder.GLib.timeout_add = lambda delay, callback: self.docks.append((delay, callback))

    def tail(self, instance):
        slots = trayorder.SLOTS[instance]

        def dock():
            self.row += [instance] * slots

        def undock():
            self.row[:] = [name for name in self.row if name != instance]

        tail = trayorder.Tail(instance, dock, undock)
        tail.row.instances = lambda: list(self.row)
        return tail

    def tails(self, instances=trayorder.ORDER):
        return [self.tail(instance) for instance in instances]

    def round(self, tails):
        """Every indicator checks, then the icons leave and come back."""
        for tail in tails:
            tail.check()
        steps = sorted(self.docks, key=lambda step: step[0])
        self.docks = []
        for _delay, callback in steps:
            callback()
        return [delay for delay, _callback in steps if delay > trayorder.DECIDE_MS]


class TrayOrderTest(unittest.TestCase):
    def setUp(self):
        timeout_add, schedule = trayorder.GLib.timeout_add, trayorder.Tail.schedule
        self.addCleanup(setattr, trayorder.GLib, "timeout_add", timeout_add)
        self.addCleanup(setattr, trayorder.Tail, "schedule", schedule)
        # The periodic check drives itself; the tests call it.
        trayorder.Tail.schedule = lambda self: None

    def test_the_agreed_order_of_the_right_hand_icons(self):
        self.assertEqual(
            trayorder.ORDER,
            ("fcitx5-indicator", "bluetooth-indicator", "battery-indicator", "clock-indicator"),
        )
        # The clock is drawn as one card across two slots.
        self.assertEqual(trayorder.SLOTS["clock-indicator"], 2)

    def test_only_the_icons_that_are_docked_are_expected(self):
        # Every indicator starts and stops on its own.
        self.assertEqual(
            trayorder.expected_tail(["copyq", "clock-indicator", "fcitx5-indicator"]),
            ["fcitx5-indicator", "clock-indicator", "clock-indicator"],
        )
        self.assertEqual(trayorder.expected_tail(["copyq", "flameshot"]), [])
        self.assertTrue(trayorder.sorted_row(OTHERS + TAIL))
        self.assertFalse(trayorder.sorted_row(OTHERS + TAIL + ["Telegram"]))

    def test_a_row_that_already_ends_in_order_is_left_alone(self):
        tray = Tray(OTHERS + TAIL)
        self.assertEqual(tray.round(tray.tails()), [])
        self.assertEqual(tray.row, OTHERS + TAIL)

    def test_a_login_race_is_sorted_out_in_one_round(self):
        # What a login leaves behind: the indicators dock in whatever order
        # they came up, and the clock's halves can even be split apart.
        tray = Tray(
            [
                "clock-indicator",
                "iOALinux",
                "battery-indicator",
                "Telegram",
                "clock-indicator",
                "bluetooth-indicator",
                "flameshot",
                "fcitx5-indicator",
                "copyq",
            ]
        )
        delays = tray.round(tray.tails())
        self.assertEqual(tray.row, ["iOALinux", "Telegram", "flameshot", "copyq"] + TAIL)
        # The icons come back spaced out, which is what puts them in ORDER.
        self.assertEqual(delays, sorted(delays))
        self.assertEqual(len(delays), len(trayorder.ORDER))

    def test_an_application_docking_later_is_overtaken(self):
        tray = Tray(OTHERS + TAIL + ["Telegram"])
        tray.round(tray.tails())
        self.assertEqual(tray.row, OTHERS + ["Telegram"] + TAIL)

    def test_indicators_that_are_not_running_leave_a_gap_in_the_order(self):
        running = ["bluetooth-indicator", "clock-indicator"]
        tray = Tray(["copyq", "clock-indicator", "clock-indicator", "bluetooth-indicator"])
        tray.round(tray.tails(running))
        self.assertEqual(tray.row, ["copyq", "bluetooth-indicator"] + ["clock-indicator"] * 2)

    def test_a_sort_in_flight_is_not_mistaken_for_a_row_to_judge(self):
        # Half a card in the row means someone is still moving.
        tray = Tray(OTHERS + ["clock-indicator", "fcitx5-indicator"])
        self.assertEqual(tray.round(tray.tails(["fcitx5-indicator"])), [])
        self.assertEqual(tray.row, OTHERS + ["clock-indicator", "fcitx5-indicator"])

    def test_icons_that_have_just_left_the_row_are_waited_for(self):
        tray = Tray(OTHERS + TAIL)
        tail = tray.tail("fcitx5-indicator")
        tray.round([tail])  # a first look, with everyone docked
        tray.row.remove("battery-indicator")
        tray.row.append("Telegram")  # the row is now out of order, too
        self.assertEqual(tray.round([tail]), [])

    def test_an_icon_the_tray_dropped_is_docked_again(self):
        # The row reads as sorted without us, which must not leave us out.
        tray = Tray(OTHERS + ["fcitx5-indicator", "bluetooth-indicator"])
        tail = tray.tail("clock-indicator")
        tray.round([tail])
        self.assertEqual(tray.row[-2:], ["clock-indicator", "clock-indicator"])

    def test_a_tray_that_keeps_its_own_order_is_left_to_it(self):
        stubborn = OTHERS + TAIL + ["Telegram"]
        tray = Tray(stubborn)
        tails = tray.tails()
        for _ in range(3):
            tray.round(tails)
            tray.row[:] = stubborn  # a tray that puts the row back as it was
        # The first round is tried; the rest of the flicker is not worth it.
        tray.round(tails)
        self.assertEqual(tray.row, stubborn)
        # A row that moved on its own is worth another try.
        tray.row[:] = OTHERS + TAIL + ["flameshot"]
        self.assertEqual(len(tray.round(tails)), len(trayorder.ORDER))

    def test_the_indicators_read_the_same_row_at_the_same_instant(self):
        self.assertLessEqual(trayorder.ms_to_next_check(), trayorder.CHECK_MS + 20)
        self.assertGreater(trayorder.ms_to_next_check(), 0)
        source = (ROOT / "scripts/trayorder.py").read_text()
        self.assertIn("int(time.time() * 1000) % CHECK_MS", source)
        # The whole sort has to be over before the next check comes round.
        self.assertLess(len(trayorder.ORDER) * trayorder.STEP_MS, trayorder.CHECK_MS)

    def test_the_row_query_survives_a_tray_that_is_gone(self):
        row = trayorder.TrayRow()

        def explode():
            raise RuntimeError("tray went away")

        row.read_instances = explode
        self.assertIsNone(row.instances())

    def test_every_indicator_holds_a_tail(self):
        for instance in trayorder.ORDER:
            source = (ROOT / "scripts" / instance).read_text()
            self.assertIn("import trayorder", source, instance)
            self.assertIn(f'INSTANCE = "{instance}"', source, instance)
            self.assertIn(
                "trayorder.Tail(INSTANCE, self.dock, self.undock)", source, instance
            )
            self.assertIn("    def dock(self):", source, instance)
            self.assertIn("    def undock(self):", source, instance)


if __name__ == "__main__":
    unittest.main()
