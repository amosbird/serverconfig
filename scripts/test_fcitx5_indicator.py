#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
INDICATOR = ROOT / "scripts/fcitx5-indicator"
STARTUP = ROOT / "scripts/startup"

loader = importlib.machinery.SourceFileLoader("fcitx5_indicator", str(INDICATOR))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


def state(**overrides):
    defaults = dict(running=True, state=module.STATE_ACTIVE, im="rime", name="Rime")
    return module.State(**{**defaults, **overrides})


class Fcitx5IndicatorTest(unittest.TestCase):
    def test_the_label_says_what_the_next_keystroke_types(self):
        self.assertEqual(module.appearance(state())[0], "中")
        # rime stays engaged in ASCII mode, but it types English.
        self.assertEqual(module.appearance(state(ascii_mode=True))[0], "EN")
        self.assertEqual(
            module.appearance(state(state=module.STATE_INACTIVE, im="keyboard-us"))[0], "EN"
        )
        self.assertEqual(module.appearance(module.State())[0], "—")

    def test_the_colour_separates_engaged_from_inactive_from_absent(self):
        self.assertEqual(module.appearance(state())[2], module.CHINESE_COLOR)
        self.assertEqual(module.appearance(state(ascii_mode=True))[2], module.ASCII_COLOR)
        self.assertEqual(
            module.appearance(state(state=module.STATE_INACTIVE))[2], module.INACTIVE_COLOR
        )
        self.assertEqual(module.appearance(module.State())[2], module.ABSENT_COLOR)

    def test_the_engine_name_fits_under_the_label(self):
        self.assertEqual(module.short_name("keyboard-us"), "us")
        self.assertEqual(module.short_name("rime"), "rime")
        self.assertEqual(module.short_name(""), "")
        self.assertEqual(module.appearance(state(im="keyboard-us"))[1], "us")

    def test_the_tooltip_explains_every_state(self):
        self.assertEqual(module.state_label(state()), "Active · Chinese")
        self.assertEqual(module.state_label(state(ascii_mode=True)), "Active · ASCII mode")
        self.assertEqual(
            module.state_label(state(state=module.STATE_INACTIVE)), "Inactive · English"
        )
        self.assertEqual(
            module.state_label(state(state=module.STATE_NO_CONTEXT)), "No input context"
        )
        self.assertEqual(module.state_label(module.State()), "fcitx5 is not running")
        lines = module.tooltip_lines(state(group="Default", layout="us", schema="double_pinyin"))
        self.assertEqual(lines[0], "Input method  Rime")
        self.assertIn("Group  Default  ·  layout us", lines)
        self.assertIn("Schema  double_pinyin", lines)
        # A missing group or schema drops the line instead of showing a blank.
        self.assertEqual(len(module.tooltip_lines(state())), 2)

    def test_only_a_changed_state_redraws(self):
        self.assertEqual(state().key(), state().key())
        self.assertNotEqual(state().key(), state(ascii_mode=True).key())
        self.assertNotEqual(state().key(), state(schema="luna_pinyin").key())

    def test_polling_never_starts_fcitx5_and_presence_is_event_driven(self):
        source = INDICATOR.read_text()
        self.assertIn("Gio.DBusCallFlags.NO_AUTO_START", source)
        self.assertIn('"NameOwnerChanged"', source)
        self.assertIn("def refresh_owner(self)", source)
        self.assertIn("if not self.owner:", source)
        self.assertIn("POLL_MS = 400", source)
        self.assertIn("faulthandler.enable()", source)

    def test_icon_is_hidpi_and_cjk_capable(self):
        source = INDICATOR.read_text()
        self.assertIn("ICON_SIZE = 96", source)
        self.assertIn("DESIGN_SIZE = 96", source)
        self.assertIn('self.icon.connect("size-changed", self.on_icon_size_changed)', source)
        # Cairo's toy text API cannot draw 中.
        self.assertIn("PangoCairo.show_layout", source)
        self.assertIn('FONT = "Noto Sans CJK SC"', source)

    def test_startup_and_restore_deploy_the_indicator(self):
        self.assertIn(
            'run "fcitx5-indicator" systemctl --user start fcitx5-indicator.service',
            STARTUP.read_text(),
        )
        unit = (ROOT / "systemd/fcitx5-indicator.service").read_text()
        self.assertIn("Restart=always", unit)
        self.assertIn("GDK_SCALE=2", unit)
        self.assertIn(
            'install -Dm644 "$DIR/systemd/fcitx5-indicator.service"',
            (ROOT / "restore.sh").read_text(),
        )


if __name__ == "__main__":
    unittest.main()
