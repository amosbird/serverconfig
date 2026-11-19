#!/usr/bin/env python3
"""Offline regression coverage against the installed Qtile lifecycle."""

import importlib
import sys
import unittest
from types import SimpleNamespace
from unittest import mock

from libqtile import hook
from libqtile.core.manager import Qtile
from libqtile.core.state import QtileState
from libqtile.scratchpad import DropDownToggler

from qtile_test_support import (
    FakeWindow, NATIVE_GET_STATE, NATIVE_RELOAD, NATIVE_RESTORE,
    OfflineQtile, hook_counts, window_effects,
)


class NativeFixtureTest(unittest.TestCase):
    def test_native_restore_really_exposes_the_hidden_window_flash(self):
        with OfflineQtile() as fixture:
            fixture.load()
            manager = fixture.manager
            manager.rebuild()
            window = FakeWindow(manager, group="scratchpad")
            scratchpad = manager.groups_map["scratchpad"]
            manager.trace.clear()
            NATIVE_RESTORE(scratchpad, [("chatgpt", window.wid, False)], False)
            effects = [event[0] for event in window_effects(manager)]
            self.assertIn("window.place", effects)
            self.assertIn("window.bring_to_front", effects)
            moves = [event[2] for event in window_effects(manager)
                     if event[0] == "window.togroup"]
            self.assertEqual(moves, ["scratchpad", None, "scratchpad"])
            self.assertEqual(window.group.name, "scratchpad")

    def test_native_reload_sequences_two_config_evaluations_around_hook_clear(self):
        with OfflineQtile() as fixture:
            fixture.load()
            manager = fixture.manager
            manager.rebuild()
            sentinel = mock.Mock()
            hook.subscribe.client_new(sentinel)
            manager.trace.clear()
            with mock.patch.object(manager.config, "load", wraps=manager.config.load) as load:
                NATIVE_RELOAD(manager)
            self.assertEqual(load.call_count, 2)
            self.assertNotIn(sentinel, hook.subscriptions.get("qtile", {}).get("client_new", []))
            events = [event[0] for event in manager.trace]
            self.assertLess(events.index("native.finalize"), events.index("native.load_config.adapter"))
            self.assertLess(events.index("native.load_config.adapter"),
                            events.index("fixture.rebuild_groups_and_screens"))
            self.assertIn("native.screen.set_group", events)
            self.assertIn("native.focus_screen", events)
            self.assertEqual(window_effects(manager), [])

    def test_fixture_restores_hooks_classes_modules_and_x11_dispatch(self):
        from libqtile.backend.x11 import core
        original_reload = Qtile.reload_config
        original_handlers = core.EVENT_TO_HANDLER.copy()
        original_ignored = core._IGNORED_EVENTS.copy()
        original_hooks = hook_counts()
        original_config = sys.modules.get("config")
        with OfflineQtile() as fixture:
            fixture.load()
            Qtile.fixture_only_attribute = True
            hook.clear()
        self.assertIs(Qtile.reload_config, original_reload)
        self.assertFalse(hasattr(Qtile, "fixture_only_attribute"))
        self.assertEqual(core.EVENT_TO_HANDLER, original_handlers)
        self.assertEqual(core._IGNORED_EVENTS, original_ignored)
        self.assertEqual(hook_counts(), original_hooks)
        self.assertIs(sys.modules.get("config"), original_config)


class X11ReloadLifecycleTest(unittest.TestCase):
    def test_native_x11_rebuild_preserves_all_existing_window_membership_and_frames(self):
        import contextlib
        from libqtile.backend.x11 import core, window as xwindow
        from libqtile.config import ScreenRect
        from libqtile.group import _Group

        with OfflineQtile() as fixture:
            fixture.load()
            manager = fixture.manager
            manager.rebuild()
            manager._drag = None
            manager.core.name = "x11"
            manager.core.masked = contextlib.nullcontext
            manager.core.clear_focus = lambda: manager.trace.append(("native.clear_focus",))
            fixture.module("patches").install(manager)
            manager.groups = []
            manager.groups_map = {}
            for definition in manager.config.groups:
                if definition.name == "scratchpad":
                    from libqtile.scratchpad import ScratchPad
                    group = ScratchPad("scratchpad", definition.dropdowns)
                else:
                    group = _Group(definition.name)
                group._configure(manager.config.layouts, manager.config.floating_layout, manager)
                manager.groups.append(group)
                manager.groups_map[group.name] = group
            from qtile_test_support import FakeScreen
            manager.screens = [FakeScreen(manager, index, manager.groups_map[name])
                               for index, name in enumerate(("h", "f"))]
            for screen in manager.screens:
                screen.get_rect = lambda screen=screen: ScreenRect(
                    screen.x, screen.y, screen.width, screen.height)
            manager.current_screen = manager.screens[1]
            windows = [FakeWindow(manager, index + 501, group=name, focused=focused)
                       for index, (name, focused) in enumerate((
                           ("scratchpad", False), ("h", False), ("f", True), ("c", False)))]
            for window in windows:
                window.fullscreen = False
                window.can_steal_focus = True
            tiled = FakeWindow(manager, 505, group="c", focused=False)
            tiled.floating = False
            tiled.fullscreen = False
            tiled.can_steal_focus = True
            group_c = manager.groups_map["c"]
            group_c.tiled_windows.add(tiled)
            for layout in group_c.layouts:
                layout.add_client(tiled)
            windows.append(tiled)
            module = fixture.module("scratchpads")
            module.attach_existing(manager.groups_map["scratchpad"], "stardict", windows[0])
            module.attach_existing(manager.groups_map["scratchpad"], "ioa", windows[1])
            before = [window.snapshot() for window in windows]
            fake_core = SimpleNamespace(
                qtile=manager, update_client_lists=mock.Mock(), fire_screen_change=mock.Mock(),
                conn=SimpleNamespace(enable_screen_change_notifications=mock.Mock()))
            def rebuild(initial=False):
                manager.config.load()
                manager.groups, manager.groups_map = [], {}
                for definition in manager.config.groups:
                    if definition.name == "scratchpad":
                        from libqtile.scratchpad import ScratchPad
                        group = ScratchPad("scratchpad", definition.dropdowns)
                    else:
                        group = _Group(definition.name)
                    group._configure(manager.config.layouts, manager.config.floating_layout, manager)
                    manager.groups.append(group)
                    manager.groups_map[group.name] = group
                manager.screens = [FakeScreen(manager, index, manager.groups_map[name])
                                   for index, name in enumerate(("h", "f"))]
                for screen in manager.screens:
                    screen.get_rect = lambda screen=screen: ScreenRect(
                        screen.x, screen.y, screen.width, screen.height)
                manager.current_screen = manager.screens[0]
                manager._state.apply(manager)
                # Module reload replaces imported classes, so patch only at dispatch.
                with mock.patch.object(fixture.module("patches"), "Window", FakeWindow):
                    core.Core.on_config_load(fake_core, False)
                hook.fire("startup")
                for screen in manager.screens:
                    screen.group.layout_all()
                manager._state = None
            manager.load_config = rebuild
            manager.trace.clear()
            with mock.patch.object(fixture.module("patches"), "Window", FakeWindow):
                manager.reload_config()
            self.assertEqual(fake_core.update_client_lists.call_count, 1)
            self.assertEqual([window.snapshot() for window in windows], before)
            effects = window_effects(manager)
            self.assertTrue(all(event[0] == "window.focus" and event[1] == windows[2].wid
                                for event in effects), manager.trace)
            for window in windows:
                self.assertIs(window.group, manager.groups_map[window.group.name])
                self.assertIn(window, window.group.windows)
            self.assertEqual(manager.current_group.name, "f")
            self.assertIs(manager.current_window, windows[2])
            self.assertIn(tiled, manager.groups_map["c"].layout.clients.clients)
            self.assertEqual(manager.groups_map["scratchpad"].dropdowns["stardict"].window,
                             windows[0])


class ReloadLifecycleTest(unittest.TestCase):
    def setUp(self):
        self.fixture = self.enterContext(OfflineQtile())
        self.manager = self.fixture.manager
        self.entry = self.fixture.load()
        # Import the real split modules; do not strip AST nodes or replace Qtile.
        self.runtime = self.fixture.module("runtime")
        self.scratchpads = self.fixture.module("scratchpads")
        self.patches = self.fixture.module("patches")
        self.manager.rebuild()
        self.manager.trace.clear()

    def attach(self, window, name="chatgpt", previous=None):
        scratchpad = self.manager.groups_map["scratchpad"]
        dropdown = self.scratchpads.attach_existing(scratchpad, name, window, previous=previous)
        self.assertIsInstance(dropdown, DropDownToggler)
        self.assertIs(scratchpad.dropdowns[name], dropdown)
        return dropdown

    def test_passive_attach_and_actual_reload_state_matrix(self):
        cases = (
            ("hidden", "scratchpad", False, 0),
            ("shown", "h", True, 0),
            ("shown_unfocused", "h", False, 0),
            ("custom_geometry", "h", True, 0),
            ("other_group", "other", False, 0),
            ("secondary_screen", "2", False, 0),
            ("secondary_screen_active", "2", True, 1),
        )
        for label, group, focused, current_screen in cases:
            with self.subTest(case=label), OfflineQtile() as fixture:
                fixture.load()
                manager = fixture.manager
                manager.rebuild()
                manager.current_screen = manager.screens[current_screen]
                window = FakeWindow(manager, group=group, focused=focused)
                window.x, window.y, window.width, window.height = 1933, 117, 731, 509
                before = window.snapshot()
                module = fixture.module("scratchpads")
                manager.trace.clear()
                dropdown = module.attach_existing(
                    manager.groups_map["scratchpad"], "chatgpt", window
                )
                dropdown.geometry = (0.13, 0.21, 0.51, 0.62)
                self.assertEqual(window_effects(manager), [], manager.trace)
                self.assertEqual(window.snapshot(), before)
                manager.reload_config()
                restored = manager.groups_map["scratchpad"].dropdowns["chatgpt"]
                self.assertIs(restored.window, window)
                self.assertEqual(restored.geometry, (0.13, 0.21, 0.51, 0.62))
                self.assertEqual(window.snapshot(), before)
                self.assertEqual(window_effects(manager), [], manager.trace)
                native_events = [event[0] for event in manager.trace]
                self.assertIn("native.finalize", native_events)
                self.assertIn("fixture.rebuild_groups_and_screens", native_events)
                self.assertIn("native.screen.set_group", native_events)
                self.assertIn("native.focus_screen", native_events)
                self.assertFalse(any(event[0] == "spawn" for event in manager.trace))

    def test_native_restart_state_stays_serializable_and_uses_native_match_restore(self):
        window = FakeWindow(self.manager, group="scratchpad")
        self.attach(window)
        scratchpad = self.manager.groups_map["scratchpad"]
        state = QtileState(self.manager, restart=True)
        self.assertEqual(state.scratchpads["scratchpad"], NATIVE_GET_STATE(scratchpad))
        self.assertEqual(state.scratchpads["scratchpad"], [("chatgpt", window.wid, False)])
        self.manager.rebuild()
        state.apply(self.manager)
        scratchpad = self.manager.groups_map["scratchpad"]
        self.assertIn("chatgpt", scratchpad._spawned)
        self.assertIn("chatgpt", scratchpad._to_hide)
        self.assertEqual(scratchpad.dropdowns, {})
        self.assertEqual(window_effects(self.manager), [])

    def test_overlapping_matchers_cannot_give_one_window_two_owners(self):
        from libqtile.config import Match
        window = FakeWindow(self.manager, name="shared")
        scratchpad = self.manager.groups_map["scratchpad"]
        for name in ("ioa", "stardict"):
            scratchpad._dropdownconfig[name].match = Match(title="shared")
        self.scratchpads.recover_windows(self.manager)
        self.scratchpads.recover_windows(self.manager)
        owners = [name for name, dropdown in scratchpad.dropdowns.items()
                  if dropdown.window is window]
        self.assertEqual(len(owners), 1)
        other = "stardict" if owners[0] == "ioa" else "ioa"
        with self.assertRaisesRegex(ValueError, "belongs|bound"):
            self.scratchpads.adopt_window(self.manager, other)
        self.assertEqual(window_effects(self.manager), [])

    def test_cold_managed_discovery_is_passive_before_startup(self):
        self.runtime.get_state(self.manager)["initializing"] = True
        window = FakeWindow(self.manager, name="iOA", group="2")
        before = window.snapshot()
        hook.fire("client_managed", window)
        self.assertIs(self.manager.groups_map["scratchpad"].dropdowns["ioa"].window, window)
        self.assertEqual(window.snapshot(), before)
        self.assertEqual(window_effects(self.manager), [])

    def test_pending_launch_survives_reload_without_respawn(self):
        scratchpad = self.manager.groups_map["scratchpad"]
        scratchpad.dropdown_toggle("chatgpt")
        pending = scratchpad._spawned["chatgpt"]
        scratchpad._to_hide.append("chatgpt")
        self.manager.trace.clear()
        self.manager.reload_config()
        restored = self.manager.groups_map["scratchpad"]
        self.assertIs(restored._spawned["chatgpt"], pending)
        self.assertIn("chatgpt", restored._to_hide)
        restored.dropdown_toggle("chatgpt")
        self.assertFalse(any(event[0] == "spawn" for event in self.manager.trace))
        self.assertEqual(window_effects(self.manager), [])
        callbacks = hook.subscriptions.get("qtile", {}).get("client_new", [])
        self.assertIn(restored.on_client_new, callbacks)
        self.assertNotIn(scratchpad.on_client_new, callbacks)

    def test_native_first_migration_preserves_pending_dropdown_and_shell(self):
        import types
        from libqtile.config import Match
        from libqtile.scratchpad import ScratchPad
        from qtile_test_support import FakeManager

        with OfflineQtile() as fixture:
            manager = fixture.manager
            sys.path.insert(0, str(manager.config.file_path.rsplit("/", 1)[0]))
            declarations = fixture.module("declarations")
            legacy = ScratchPad("scratchpad", declarations.build_dropdowns(declarations.load()))
            legacy.qtile = manager
            manager.groups = [group for group in manager.groups if group.name != "scratchpad"]
            manager.groups.append(legacy)
            manager.groups_map["scratchpad"] = legacy
            legacy.dropdown_toggle("chatgpt")
            pending = legacy._spawned["chatgpt"]
            shell_pending = (Match(net_wm_pid=999), 2)
            legacy_module = types.ModuleType("config")
            legacy_module.__file__ = manager.config.file_path
            legacy_module.__spec__ = importlib.util.spec_from_file_location(
                "config", manager.config.file_path)
            legacy_module.shell = SimpleNamespace(shell=None, _spawned=shell_pending)
            sys.modules["config"] = legacy_module
            # Enter the native function already resolved by the previous config.
            manager.trace.clear()
            NATIVE_RELOAD(manager)
            restored = manager.groups_map["scratchpad"]
            self.assertIs(restored._spawned["chatgpt"], pending)
            holder = fixture.module("scratchpads").shell_holder(manager)
            self.assertIs(holder._spawned, shell_pending)
            restored.dropdown_toggle("chatgpt")
            holder.toggle_left()
            self.assertFalse(any(event[0] == "spawn" for event in manager.trace))
            self.assertIn(restored.on_client_new,
                          hook.subscriptions.get("qtile", {}).get("client_new", []))

    def test_recycled_window_id_cannot_restore_an_old_object(self):
        old = FakeWindow(self.manager, group="scratchpad")
        self.attach(old)
        replacement = FakeWindow(self.manager, wid=old.wid, group="other")
        before = replacement.snapshot()
        self.manager.trace.clear()
        self.manager.reload_config()
        dropdown = self.manager.groups_map["scratchpad"].dropdowns.get("chatgpt")
        self.assertIsNone(dropdown)
        self.assertEqual(replacement.snapshot(), before)
        self.assertEqual(window_effects(self.manager), [])

    def test_previous_toggler_for_another_object_is_rejected(self):
        old = FakeWindow(self.manager, group="scratchpad")
        previous = self.attach(old)
        replacement = FakeWindow(self.manager, wid=old.wid, group="other")
        scratchpad = self.manager.groups_map["scratchpad"]
        scratchpad.dropdowns.clear()
        self.manager.trace.clear()
        try:
            result = self.scratchpads.attach_existing(
                scratchpad, "chatgpt", replacement, previous=previous
            )
        except (ValueError, RuntimeError):
            result = None
        self.assertIsNone(result)
        self.assertNotIn("chatgpt", scratchpad.dropdowns)
        self.assertEqual(window_effects(self.manager), [])

    def test_shell_duplicate_recovery_is_passive_and_idempotent(self):
        first = FakeWindow(self.manager, wid=201, name="urxvt_scratchpad")
        second = FakeWindow(self.manager, wid=202, name="urxvt_scratchpad", focused=True)
        holder = self.scratchpads.ShellHolder()
        before = (first.snapshot(), second.snapshot())
        holder.recover([first, second])
        chosen = holder.shell.window
        holder.recover([second, first])
        self.assertIs(holder.shell.window, chosen)
        self.assertEqual((first.snapshot(), second.snapshot()), before)
        self.assertEqual(window_effects(self.manager), [])
        self.assertFalse(any(event[0] == "spawn" for event in self.manager.trace))

    def test_config_double_evaluation_has_no_patch_chains_or_hook_leaks(self):
        self.assertIsNot(Qtile.reload_config, NATIVE_RELOAD)
        wrapper = Qtile.reload_config
        self.manager.reload_config()
        expected = hook_counts()
        state = self.runtime.get_state(self.manager)
        marker = object()
        state["offline_fixture_marker"] = marker
        for _ in range(3):
            self.manager.trace.clear()
            self.manager.reload_config()
            self.assertIs(Qtile.reload_config, wrapper)
            self.assertTrue(hook_counts())
            callbacks = hook.subscriptions.get("qtile", {})
            installed_runtime = self.fixture.module("runtime")
            installed_desktop = self.fixture.module("desktop")
            for event, callback in (
                ("startup", installed_runtime.on_startup),
                ("client_managed", installed_runtime.after_window_created),
                ("client_killed", installed_runtime.window_killed),
                ("setgroup", installed_desktop.show_browser_tabs),
                ("client_focus", installed_runtime.client_focused),
            ):
                self.assertIn(callback, callbacks.get(event, []), event)
            self.assertEqual(hook_counts(), expected)
            self.assertIs(self.fixture.module("runtime").get_state(self.manager), state)
            self.assertIs(state["offline_fixture_marker"], marker)
            self.assertEqual(sum(event[0] == "native.finalize" for event in self.manager.trace), 1)
            self.assertEqual(window_effects(self.manager), [])

    def test_delayed_input_callback_is_fenced_after_reload(self):
        bindings = self.fixture.module("bindings")
        query = SimpleNamespace(reply=lambda: SimpleNamespace(keys=[0] * 32))
        self.manager.core.qtile = self.manager
        self.manager.core.conn = SimpleNamespace(
            keysym_to_keycode=lambda keysym: [42],
            conn=SimpleNamespace(core=SimpleNamespace(QueryKeymap=lambda: query)),
        )
        command = bindings.defer_inputstr("0", "offline text")
        command.args[0](self.manager)
        bindings.handle_key_release(self.manager.core, SimpleNamespace(detail=42))
        self.assertTrue(self.manager.timers, "release must schedule the deferred callback")
        timers = self.manager.timers[:]
        self.manager.reload_config()
        self.manager.trace.clear()
        for timer in timers:
            timer.fire_even_if_cancelled()
        self.assertFalse(any(event[0] == "spawn" for event in self.manager.trace))
        self.assertEqual(window_effects(self.manager), [])

    def test_preflight_import_failure_has_no_window_or_native_rebuild_effects(self):
        window = FakeWindow(self.manager, group="h", focused=True)
        self.attach(window)
        before = window.snapshot()
        self.manager.trace.clear()
        with mock.patch.object(importlib, "reload", side_effect=ImportError("offline preflight")):
            self.manager.reload_config()
        self.assertEqual(window.snapshot(), before)
        self.assertEqual(window_effects(self.manager), [])
        self.assertFalse(any(event[0] in {
            "native.finalize", "native.ungrab_keys", "native.load_config.adapter"
        } for event in self.manager.trace), self.manager.trace)


if __name__ == "__main__":
    unittest.main()
