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
    """A stand-in tray: an icon that docks takes the lowest free slot.

    The slots an icon leaves behind go to the ones after it, but not while it
    is still on its way out, so a row only closes up between two checks.
    """

    def __init__(self, row=(), compacts=True):
        self.compacts = compacts
        self.slots = list(row)
        self.steps = []
        self.restarts = 0
        trayorder.GLib.timeout_add = lambda delay, callback: self.steps.append((delay, callback))
        trayorder.subprocess = self

    # The tray restart, as trayorder reaches for it.
    def run(self, command, **_kwargs):
        assert command == ["systemctl", "--user", "restart", trayorder.TRAY_UNIT], command
        self.slots = None  # no tray until the new one is up
        self.restarts += 1


    @property
    def row(self):
        if self.slots is None:
            return None
        return [instance for instance in self.slots if instance is not None]

    def take(self, instance, count):
        if self.slots is None:
            return
        for _ in range(count):
            if None in self.slots:
                self.slots[self.slots.index(None)] = instance
            else:
                self.slots.append(instance)

    def free(self, instance):
        if self.slots is None:
            return
        self.slots = [None if slot == instance else slot for slot in self.slots]

    def tail(self, instance):
        slots = trayorder.SLOTS[instance]
        tail = trayorder.Tail(
            instance,
            lambda: self.take(instance, slots),
            lambda: self.free(instance),
        )
        tail.row.instances = lambda: self.row
        return tail

    def tails(self, instances=trayorder.ORDER):
        return [self.tail(instance) for instance in instances]

    def round(self, tails):
        """Every indicator reads the row, then the icons move."""
        if self.compacts and self.slots:
            self.slots = [slot for slot in self.slots if slot is not None]
        for tail in tails:
            tail.check()
        steps = sorted(self.steps, key=lambda step: step[0])
        self.steps = []
        for _delay, callback in steps:
            callback()
        return [delay for delay, _callback in steps if delay > trayorder.DECIDE_MS]

    def ready(self, tails, seen=True):
        """Indicators that are docked, with the row as they last saw it."""
        for tail in tails:
            tail.docked = True
            tail.before = list(self.row) if seen and self.row is not None else None
        return tails


class TrayOrderTest(unittest.TestCase):
    def setUp(self):
        timeout_add = trayorder.GLib.timeout_add
        schedule, run = trayorder.Tail.schedule, trayorder.subprocess
        self.addCleanup(setattr, trayorder.GLib, "timeout_add", timeout_add)
        self.addCleanup(setattr, trayorder.Tail, "schedule", schedule)
        self.addCleanup(setattr, trayorder, "subprocess", run)
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

    def test_nothing_docks_before_the_applications_have_stopped_arriving(self):
        # This is the login race: every slot an application takes later is one
        # beyond ours, and there is no getting back in front of it.
        tray = Tray([])
        tails = tray.tails()
        tray.round(tails)
        self.assertEqual(tray.row, [])
        tray.slots += ["iOALinux", "flameshot"]
        tray.round(tails)
        self.assertEqual(tray.row, ["iOALinux", "flameshot"])
        tray.slots += ["copyq"]
        tray.round(tails)
        self.assertEqual(tray.row, ["iOALinux", "flameshot", "copyq"])
        # The row has not moved since the last look, so the icons take it.
        tray.round(tails)
        self.assertEqual(tray.row, ["iOALinux", "flameshot", "copyq"] + TAIL)

    def test_a_row_that_already_ends_in_order_is_left_alone(self):
        tray = Tray(OTHERS + TAIL)
        tails = tray.ready(tray.tails())
        self.assertEqual(tray.round(tails), [])
        self.assertEqual(tray.row, OTHERS + TAIL)

    def test_icons_that_docked_out_of_order_sort_themselves_out(self):
        # The slots at the end are ours, but the login race filled them in
        # the order the indicators happened to come up, halves and all.
        tray = Tray(
            [
                "iOALinux",
                "flameshot",
                "copyq",
                "clock-indicator",
                "battery-indicator",
                "clock-indicator",
                "bluetooth-indicator",
                "fcitx5-indicator",
            ]
        )
        tails = tray.ready(tray.tails())
        tray.round(tails)  # out of the row
        self.assertEqual(tray.row, ["iOALinux", "flameshot", "copyq"])
        tray.round(tails)  # the row has moved, so nothing is judged on it
        delays = tray.round(tails)  # and back, in order, after those before
        self.assertEqual(tray.row, ["iOALinux", "flameshot", "copyq"] + TAIL)
        self.assertEqual(delays, sorted(delays))
        self.assertEqual(len(delays), len(trayorder.ORDER))

    def test_indicators_that_are_not_running_leave_a_gap_in_the_order(self):
        running = ["bluetooth-indicator", "clock-indicator"]
        tray = Tray(["copyq", "clock-indicator", "clock-indicator", "bluetooth-indicator"])
        tails = tray.ready(tray.tails(running))
        for _ in range(3):
            tray.round(tails)
        self.assertEqual(tray.row, ["copyq", "bluetooth-indicator"] + ["clock-indicator"] * 2)

    def test_a_row_that_is_still_moving_is_not_judged(self):
        tray = Tray(OTHERS + TAIL)
        tails = tray.ready(tray.tails(), seen=False)
        # The first look has nothing to compare against, and by the second
        # the row has moved, so neither says anything about the order.
        self.assertEqual(tray.round(tails), [])
        tray.slots += ["Telegram"]
        self.assertEqual(tray.round(tails), [])
        self.assertEqual(tray.row, OTHERS + TAIL + ["Telegram"])

    def test_an_icon_the_tray_dropped_is_docked_again(self):
        tray = Tray(OTHERS + ["fcitx5-indicator", "bluetooth-indicator"])
        (tail,) = tray.ready([tray.tail("clock-indicator")])
        for _ in range(3):
            tray.round([tail])
        self.assertEqual(tray.row[-2:], ["clock-indicator", "clock-indicator"])

    def test_the_icons_leave_a_tray_that_is_gone_rather_than_follow_it_back(self):
        tray = Tray(OTHERS + TAIL)
        tails = tray.ready(tray.tails())
        tray.slots = None  # the tray died and took the row with it
        tray.round(tails)
        self.assertEqual([tail.docked for tail in tails], [False] * len(tails))
        tray.slots = list(OTHERS)  # a new tray, with the applications back
        tray.round(tails)
        self.assertEqual(tray.row, OTHERS)
        tray.round(tails)
        self.assertEqual(tray.row, OTHERS + TAIL)

    def test_the_tray_is_started_over_for_an_icon_that_docked_after_ours(self):
        # Slots are handed out lowest first and ours are the ones we free, so
        # nothing we do gets us past an icon that docked later.
        stubborn = ["iOALinux"] + TAIL + ["flameshot", "copyq"]
        tray = Tray(stubborn, compacts=False)
        tails = tray.ready(tray.tails())
        tray.round(tails)  # out of the row
        for _ in range(2):
            tray.round(tails)  # and back into the very slots it left
        self.assertEqual(tray.row, stubborn)
        self.assertEqual(tray.restarts, 0)
        tray.round(tails)  # the row settles, unchanged by the sort
        tray.round(tails)  # so the tray is started over
        self.assertEqual(tray.restarts, 1)
        self.assertIsNone(tray.row)
        tray.round(tails)  # with no tray, the icons wait rather than dock
        self.assertEqual([tail.docked for tail in tails], [False] * len(tails))
        tray.slots = ["iOALinux", "flameshot", "copyq"]  # the applications come back
        tray.round(tails)
        tray.round(tails)
        self.assertEqual(tray.row, ["iOALinux", "flameshot", "copyq"] + TAIL)

    def test_only_one_indicator_starts_the_tray_over(self):
        tray = Tray(["iOALinux"] + TAIL + ["copyq"], compacts=False)
        tails = tray.ready(tray.tails())
        for tail in tails:
            tail.tried = tuple(tray.row)
        tray.round(tails)
        self.assertEqual(tray.restarts, 1)
        self.assertEqual(trayorder.first_in_row(["copyq"] + TAIL), "fcitx5-indicator")
        self.assertIsNone(trayorder.first_in_row(["copyq"]))

    def test_a_tray_that_keeps_its_own_order_is_not_restarted_again_and_again(self):
        stubborn = ["iOALinux"] + TAIL + ["copyq"]
        tray = Tray(stubborn, compacts=False)
        tails = tray.ready(tray.tails())
        for tail in tails:
            tail.tried = tuple(stubborn)
        tray.round(tails)
        self.assertEqual(tray.restarts, 1)
        # The new tray hands out the same row again, which is as much as this
        # can do about it; the icons are not to start it over every check.
        for _ in range(3):
            tray.slots = list(stubborn)
            for tail in tray.ready(tails):
                tail.tried = tuple(stubborn)
            tray.round(tails)
        self.assertEqual(tray.restarts, 1)

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

    def test_every_indicator_leaves_its_icons_to_a_tail(self):
        for instance in trayorder.ORDER:
            source = (ROOT / "scripts" / instance).read_text()
            self.assertIn("import trayorder", source, instance)
            self.assertIn(f'INSTANCE = "{instance}"', source, instance)
            self.assertIn("trayorder.Tail(INSTANCE, self.dock, self.undock)", source, instance)
            self.assertIn("    def dock(self):", source, instance)
            self.assertIn("    def undock(self):", source, instance)
            # Docking before the row has settled is what loses the order.
            init = source.split("    def __init__(self):", 1)[1].split("    def ", 1)[0]
            self.assertNotIn("self.dock()", init, instance)


if __name__ == "__main__":
    unittest.main()
