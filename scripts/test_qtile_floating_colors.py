#!/usr/bin/env python3

import unittest
from unittest import mock

from qtile_test_support import FakeWindow, OfflineQtile


class FloatingBorderColorsTest(unittest.TestCase):
    def setUp(self):
        self.fixture = self.enterContext(OfflineQtile())
        self.config = self.fixture.load()
        self.manager = self.fixture.manager
        self.manager.rebuild()
        self.layout = self.config.floating_layout

    def window(self, name="ordinary", classes=("unknown",), role="normal"):
        window = FakeWindow(self.manager, name=name)
        window.get_wm_class = lambda: classes
        window.get_wm_role = lambda: role
        return window

    def test_designed_floating_windows_use_rainbow_colors(self):
        expected = [
            ("iOA", ("unknown",), "normal", "#F94144"),
            ("Telegram", ("TelegramDesktop",), "normal", "#70A288"),
            ("webchat", ("webchat",), "normal", "#2A9D8F"),
            ("chatgpt", ("chatgpt",), "normal", "#84A98C"),
            ("Bookmarks", ("bookmark-manager",), "normal", "#4F6D7A"),
            ("stardict", ("kitty",), "normal", "#E83E8C"),
            ("stalonetray", ("stalonetray",), "normal", "#F2CC8F"),
            ("popup", ("Google-chrome",), "pop-up", "#6A994E"),
            ("copyq", ("copyq",), "normal", "#E9C46A"),
        ]
        for name, classes, role, color in expected:
            with self.subTest(name=name):
                window = self.window(name, classes, role)
                self.assertEqual(self.layout.border_focus.get_border_for_window(window), color)
                self.assertEqual(self.layout.border_normal.get_border_for_window(window), color)

    def test_borderless_utility_windows_have_zero_width(self):
        for title, classes in (("float", ("kitty",)), ("dtpick", ("kitty",)),
                               ("urxvt_scratchpad", ("kitty",)), ("shot", ("flameshot",))):
            with self.subTest(title=title):
                self.assertEqual(self.layout.border_width.get_border_for_window(
                    self.window(title, classes)), 0)
        self.assertEqual(self.layout.border_width.get_border_for_window(self.window()), 8)

    def test_reload_keeps_conditional_border_width_numeric(self):
        patches = self.fixture.module("patches")
        width = self.layout.border_width
        self.assertIsInstance(width, int)
        self.assertEqual(width + 2, 10)
        window = self.window("float", ("kitty",))
        window._place = mock.Mock()
        # A previous config generation may have left a nonnumeric private cache.
        window._old_bw = object()
        patches.new_place(window, 1, 2, 100, 200, width, self.layout.border_focus)
        self.assertEqual(window._place.call_args.args, (1, 2, 116, 216, 0, "#FFB300"))
        self.assertEqual(window._old_bw, 0)
        # Another generation's conditional object is accepted by capability, not identity.
        other = type("OtherGeneration", (int,), {
            "default": 8, "get_border_for_window": lambda self, window: 0})(8)
        patches.new_place(window, 1, 2, 100, 200, other, "red")
        self.assertEqual(window._place.call_args.args, (1, 2, 100, 200, 0, "red"))

    def test_undesigned_floating_windows_keep_original_yellow(self):
        for window in (self.window(), self.window("dtpick", ("kitty",))):
            self.assertEqual(self.layout.border_focus.get_border_for_window(window), "#FFB300")
            self.assertEqual(self.layout.border_normal.get_border_for_window(window), "#FFB300")


if __name__ == "__main__":
    unittest.main()
