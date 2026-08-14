#!/usr/bin/env python3
import importlib.machinery
import importlib.util
import pathlib
import unittest
from unittest import mock


SCRIPT = pathlib.Path(__file__).with_name("rofi-hister-blocks")
loader = importlib.machinery.SourceFileLoader("rofi_hister_blocks", str(SCRIPT))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


class RofiHisterTest(unittest.TestCase):
    def test_hister_query_strips_fts_syntax(self):
        self.assertEqual(module.build_hister_query('foo "bar" baz*'), "foo* bar* baz*")

    def test_hister_json_accepts_trailing_comma_from_hister_2(self):
        self.assertEqual(
            module.parse_hister_json('[{"title":"T","url":"https://example.com"},\n]'),
            [{"title": "T", "url": "https://example.com"}],
        )

    def test_format_lines_escapes_pango_markup(self):
        line = module.format_lines(
            [("hist", "A < B & C", "https://example.com/?a=1&b=2", "")]
        )[0]
        self.assertEqual(
            line["text"],
            "[hist] A &lt; B &amp; C  <i>example.com/?a=1&amp;b=2</i>",
        )

    def test_format_lines_uses_compact_index_metadata(self):
        line = module.format_lines(
            [("tab", "Title", "https://example.com/long", "target")]
        )[0]
        self.assertEqual(line["data"], "0")
        self.assertNotIn("icon", line)

    def test_launcher_sets_blocks_native_prompt(self):
        launcher = SCRIPT.with_name("rofi-hister").read_text()
        self.assertIn("-blocks-prompt 'Web❯'", launcher)
        self.assertNotIn("-display-blocks", launcher)

    def test_launcher_preloads_backend_before_starting_rofi(self):
        launcher = SCRIPT.with_name("rofi-hister").read_text()
        self.assertIn("coproc BACKEND", launcher)
        self.assertLess(launcher.index("read -r initial"), launcher.index("rofi -modi"))
        self.assertNotIn("-blocks-wrap", launcher)

    def test_launcher_closes_rofi_when_backend_exits(self):
        launcher = SCRIPT.with_name("rofi-hister").read_text()
        self.assertIn("wait -n -p finished", launcher)
        self.assertIn('kill "$rofi_pid" "$backend_pid"', launcher)

    def test_launcher_waits_for_focus_before_backend_can_exit(self):
        launcher = SCRIPT.with_name("rofi-hister").read_text()
        self.assertIn("focus_done", launcher)
        self.assertLess(launcher.index("read -r focused"), launcher.index("touch \"$focus_done\""))
        self.assertIn('for _ in {1..100}', launcher)

    def test_launcher_uses_persistent_qtile_ipc_helper(self):
        launcher = SCRIPT.with_name("rofi-hister").read_text()
        self.assertIn("rofi-hister-qtile", launcher)
        self.assertNotIn("xdotool set_desktop", launcher)

    def test_restore_installs_hister_browser_and_rofi_plugins(self):
        root = SCRIPT.parent.parent
        policy = (root / "chromium" / "extensions-policy.json").read_text()
        restore = (root / "restore.sh").read_text()
        self.assertIn("jpgfhlaplofoaempbhliigmjbpofeghk", policy)
        self.assertIn('"RestoreOnStartup": 1', policy)
        self.assertNotIn("aocepclkpgckjeikiphffdlileoaceec", policy)
        self.assertIn("rofi-blocks-git", restore)
        self.assertIn("extensions-policy.json", restore)
        gui_restore = restore[restore.index("if [[ -n $GUI ]]") :]
        self.assertIn("rofi-chrome/host/main.py", gui_restore)
        self.assertIn("io.github.amosbird.rofi.chrome.json", gui_restore)
        self.assertNotIn("github.com/amosbird/rofi-chrome/releases", gui_restore)
        self.assertNotIn("curl", gui_restore)
        self.assertNotIn("tar -xz", gui_restore)
        self.assertNotIn("ROFI_CHROME_VERSION", gui_restore)
        self.assertNotIn("rofi_chrome_tmp", gui_restore)
        self.assertNotIn("SHA256SUMS", gui_restore)
        self.assertNotIn("unzip", gui_restore)

    def test_chromium_uses_store_extension_without_loading_unpackaged_copy(self):
        launcher = SCRIPT.with_name("chromium").read_text()
        self.assertNotIn("--load-extension", launcher)

    def test_chromium_reuses_default_profile_and_switches_to_browser_group(self):
        root = SCRIPT.parent.parent
        launcher = SCRIPT.with_name("chromium").read_text()
        qtile_config = (root / ".config/qtile/config.py").read_text()
        self.assertIn('qtile cmd-obj -o group f -f toscreen', launcher)
        self.assertNotIn("--user-data-dir", launcher)
        self.assertIn(
            "chrome-extension://jpgfhlaplofoaempbhliigmjbpofeghk/download.html",
            qtile_config,
        )
        self.assertNotIn("aocepclkpgckjeikiphffdlileoaceec", qtile_config)

    def test_chromium_integration_names_and_launcher(self):
        self.assertEqual(module.BROWSER_CDP_PORT, 9222)
        self.assertTrue(hasattr(module, "get_chromium_tabs"))
        self.assertTrue(hasattr(module, "activate_chromium_tab"))
        with mock.patch.object(module.subprocess, "Popen") as popen:
            module.launch_url("https://example.com")
        popen.assert_called_once_with(
            ["/home/amos/scripts/chromium", "https://example.com"],
            stdout=module.subprocess.DEVNULL,
            stderr=module.subprocess.DEVNULL,
        )

    def test_get_tabs_ignores_iframe_targets(self):
        targets = [
            {"id": "frame", "type": "iframe", "title": "Frame", "url": "https://x"},
            {"id": "page", "type": "page", "title": "Page", "url": "https://x"},
        ]
        with mock.patch.object(module, "cdp_request", return_value=targets):
            self.assertEqual(module.get_chromium_tabs(), [("Page", "https://x", "page")])

    def test_cdp_request_treats_text_response_as_success(self):
        class Response:
            status = 200

            @staticmethod
            def read():
                return b"Target activated"

        class Connection:
            def request(self, method, path):
                pass

            def getresponse(self):
                return Response()

            def close(self):
                pass

        self.assertTrue(module.cdp_request("/json/activate/id", Connection()))

    def test_open_tab_activates_target_before_requesting_launcher_focus(self):
        calls = []
        with (
            mock.patch.object(
                module, "activate_chromium_tab", side_effect=lambda target: calls.append("tab") or True
            ),
            mock.patch.object(
                module, "focus_browser_group", side_effect=lambda: calls.append("desktop")
            ),
        ):
            module.open_entry(
                {"kind": "tab", "url": "https://example.com", "target_id": "target"}
            )
        self.assertEqual(calls, ["tab", "desktop"])

    def test_backend_requests_launcher_to_focus_qtile_group(self):
        with mock.patch.object(module, "send") as send:
            module.focus_browser_group()
        send.assert_called_once_with({"command": "focus-browser"})

    def test_build_tab_entries_keeps_duplicate_urls(self):
        open_tabs = [
            ("First", "https://example.com", "target-1"),
            ("Second", "https://example.com", "target-2"),
        ]
        self.assertEqual(
            module.build_tab_entries("", open_tabs),
            [
                ("tab", "First", "https://example.com", "target-1"),
                ("tab", "Second", "https://example.com", "target-2"),
            ],
        )

    def test_full_text_history_hit_promotes_all_matching_open_tabs(self):
        open_tabs = [
            ("First", "https://example.com/article", "target-1"),
            ("Second", "https://example.com/article", "target-2"),
        ]
        history = [("Needle in page body", "https://example.com/article")]
        self.assertEqual(
            module.merge_results([], history, [], open_tabs),
            [
                ("tab", "First", "https://example.com/article", "target-1"),
                ("tab", "Second", "https://example.com/article", "target-2"),
            ],
        )

    def test_merge_results_keeps_tabs_first_and_deduplicates_history(self):
        tabs = [("tab", "Open", "https://example.com", "target-1")]
        history = [
            ("Old open", "https://example.com"),
            ("Other", "https://other.example"),
        ]
        self.assertEqual(
            module.merge_results(tabs, history, []),
            tabs + [("hist", "Other", "https://other.example", "")],
        )


if __name__ == "__main__":
    unittest.main()
