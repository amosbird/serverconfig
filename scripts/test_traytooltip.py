#!/usr/bin/env python3

import pathlib
import sys
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

import traytooltip

INDICATORS = ("fcitx5-indicator", "bluetooth-indicator", "battery-indicator", "clock-indicator")


class Area:
    def __init__(self, x, y, width, height):
        self.x, self.y, self.width, self.height = x, y, width, height


class Icon:
    """A tray icon that asks before it shows a tooltip, as GTK does."""

    def __init__(self, x=100, docked=True):
        self.area = Area(x, 500, 48, 48)
        self.docked = docked
        self.has_tooltip = False
        self.handler = None

    def get_geometry(self):
        return self.docked, None, self.area, None

    def set_has_tooltip(self, value):
        self.has_tooltip = value

    def connect(self, signal, handler):
        assert signal == "query-tooltip", signal
        self.handler = handler

    def ask(self):
        """One of GTK's questions, answered with the text it is to show."""
        tooltip = Tip()
        shown = self.handler(self, 0, 0, False, tooltip)
        return tooltip.text if shown else None

    def hover(self):
        """What GTK does when it is about to show a tooltip: it asks twice.

        Once on the motion that brought the pointer here, and once more with
        nothing having moved since, and that second answer is the one that
        decides whether the tooltip is shown at all.
        """
        self.ask()
        return self.ask()


class Tip:
    def __init__(self):
        self.text = None

    def set_text(self, text):
        self.text = text


class TrayTooltipTest(unittest.TestCase):
    def setUp(self):
        pointer = traytooltip.pointer
        self.addCleanup(setattr, traytooltip, "pointer", pointer)
        self.at(2000, 2000)

    def at(self, x, y):
        traytooltip.pointer = lambda: (x, y)

    def test_an_icon_that_appears_under_a_still_pointer_shows_nothing(self):
        icon = Icon(x=100)
        tip = traytooltip.Tooltip()
        self.at(120, 520)
        tip.attach([icon])
        tip.set("Battery 50%")
        self.assertIsNone(icon.hover())

    def test_a_pointer_that_moves_onto_the_icon_is_shown_the_tooltip(self):
        icon = Icon(x=100)
        tip = traytooltip.Tooltip()
        tip.attach([icon])
        tip.set("Battery 50%")
        self.at(120, 520)
        self.assertEqual(icon.hover(), "Battery 50%")

    def test_a_row_sliding_under_the_pointer_shows_nothing(self):
        # Every icon that docks or leaves moves the whole row sideways.
        icon = Icon(x=300)
        tip = traytooltip.Tooltip()
        self.at(120, 520)
        tip.attach([icon])
        tip.set("Bluetooth on")
        icon.area.x = 100
        self.assertIsNone(icon.hover())
        # And once it has come to rest, hovering it works again.
        self.at(130, 520)
        self.assertEqual(icon.hover(), "Bluetooth on")

    def test_the_text_shown_is_the_one_the_indicator_last_set(self):
        icon = Icon(x=100)
        tip = traytooltip.Tooltip()
        tip.attach([icon])
        tip.set("Battery 50%")
        self.at(120, 520)
        self.assertEqual(icon.hover(), "Battery 50%")
        tip.set("Battery 49%")
        self.at(121, 520)
        self.assertEqual(icon.hover(), "Battery 49%")

    def test_both_halves_of_one_card_carry_the_same_tooltip(self):
        left, right = Icon(x=100), Icon(x=148)
        tip = traytooltip.Tooltip()
        tip.attach([left, right])
        tip.set("2026-09-20 周日")
        self.at(120, 520)
        self.assertEqual(left.hover(), "2026-09-20 周日")
        self.at(160, 520)
        self.assertEqual(right.hover(), "2026-09-20 周日")

    def test_docking_again_holds_the_tooltip_back_again(self):
        tip = traytooltip.Tooltip()
        tip.attach([Icon(x=100)])
        tip.set("Input method")
        self.at(120, 520)
        fresh = Icon(x=100)
        tip.attach([fresh])  # the sort made new icons, under the pointer
        self.assertIsNone(fresh.hover())

    def test_an_icon_with_no_place_in_the_row_falls_back_to_the_pointer(self):
        # A tray that will not say where an icon is cannot be asked who
        # moved, and an icon with no place of its own is not hovered anyway.
        icon = Icon(docked=False)
        tip = traytooltip.Tooltip()
        tip.attach([icon])
        tip.set("Clock")
        self.assertIsNone(icon.hover())
        self.at(120, 520)
        self.assertEqual(icon.hover(), "Clock")

    def test_a_hover_survives_being_asked_about_twice(self):
        icon = Icon(x=100)
        tip = traytooltip.Tooltip()
        tip.attach([icon])
        tip.set("Battery 50%")
        self.at(120, 520)
        self.assertEqual(icon.ask(), "Battery 50%")  # the pointer moved here
        self.assertEqual(icon.ask(), "Battery 50%")  # and is still here
        self.assertEqual(icon.ask(), "Battery 50%")

    def test_gtk_is_asked_before_every_tooltip(self):
        icon = Icon()
        traytooltip.Tooltip().attach([icon])
        self.assertTrue(icon.has_tooltip)
        self.assertIsNotNone(icon.handler)

    def test_every_indicator_hands_its_tooltips_over(self):
        for instance in INDICATORS:
            source = (ROOT / "scripts" / instance).read_text()
            self.assertIn("import traytooltip", source, instance)
            self.assertIn("traytooltip.Tooltip()", source, instance)
            self.assertIn("self.tip.attach(", source, instance)
            self.assertIn("self.tip.set(", source, instance)
            self.assertNotIn("set_tooltip_text", source, instance)


if __name__ == "__main__":
    unittest.main()
