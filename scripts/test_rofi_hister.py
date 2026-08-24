#!/usr/bin/env python3
import importlib.machinery
import importlib.util
import json
import pathlib
import socket
import subprocess
import tempfile
import unittest
from unittest import mock


SCRIPT = pathlib.Path(__file__).with_name("rofi-hister-blocks")
loader = importlib.machinery.SourceFileLoader("rofi_hister_blocks", str(SCRIPT))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)

REMOTE = pathlib.Path(__file__).with_name("chromium-remote")
remote_loader = importlib.machinery.SourceFileLoader("chromium_remote", str(REMOTE))
remote_spec = importlib.util.spec_from_loader(remote_loader.name, remote_loader)
remote = importlib.util.module_from_spec(remote_spec)
remote_loader.exec_module(remote)


class FakeSocket:
    def __init__(self, ack=b"ACK"):
        self.ack = ack
        self.connected = None
        self.sent = None
        self.shutdown_mode = None
        self.timeout = None

    def __enter__(self):
        return self

    def __exit__(self, *args):
        pass

    def settimeout(self, timeout):
        self.timeout = timeout

    def connect(self, path):
        self.connected = path

    def sendall(self, data):
        self.sent = data

    def shutdown(self, mode):
        self.shutdown_mode = mode

    def recv(self, size):
        return self.ack


class RofiHisterTest(unittest.TestCase):
    def test_hister_query_strips_fts_syntax(self):
        self.assertEqual(module.build_hister_query('foo "bar" baz*'), "foo* bar* baz*")

    def test_hister_json_accepts_trailing_comma_from_hister_2(self):
        self.assertEqual(
            module.parse_hister_json('[{"title":"T","url":"https://example.com"},\n]'),
            [{"title": "T", "url": "https://example.com"}],
        )

    def test_load_chromium_bookmarks_flattens_folders(self):
        tree = {
            "roots": {
                "bookmark_bar": {
                    "type": "folder",
                    "name": "Bookmarks bar",
                    "children": [
                        {
                            "type": "folder",
                            "name": "Docs",
                            "children": [
                                {
                                    "type": "url",
                                    "name": "Chromium API",
                                    "url": "https://developer.chrome.com/",
                                }
                            ],
                        }
                    ],
                }
            }
        }
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "Bookmarks"
            path.write_text(json.dumps(tree))
            self.assertEqual(
                module.load_chromium_bookmarks(path),
                [("Chromium API", "https://developer.chrome.com/", "Bookmarks bar/Docs")],
            )

    def test_missing_or_malformed_bookmarks_are_ignored(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "Bookmarks"
            self.assertEqual(module.load_chromium_bookmarks(path), [])
            path.write_text("{")
            self.assertEqual(module.load_chromium_bookmarks(path), [])

    def test_bookmark_search_matches_folder_and_precedes_history(self):
        bookmarks = [
            ("Chromium API", "https://developer.chrome.com/", "Bookmarks bar/Docs")
        ]
        history = [
            ("Chromium History", "https://example.com/chromium"),
            ("Saved page in history", "https://developer.chrome.com/"),
        ]
        self.assertEqual(
            module.merge_results(
                [],
                history,
                [],
                bookmark_entries=module.build_bookmark_entries("docs", bookmarks),
            ),
            [
                (
                    "bm",
                    "Chromium API",
                    "https://developer.chrome.com/",
                    "Bookmarks bar/Docs",
                ),
                ("hist", "Chromium History", "https://example.com/chromium", ""),
            ],
        )

    def test_empty_query_does_not_show_bookmarks(self):
        bookmarks = [("Chromium API", "https://developer.chrome.com/", "Docs")]
        self.assertEqual(module.build_bookmark_entries("", bookmarks), [])

    def test_bookmark_mode_shows_all_bookmarks_for_empty_query(self):
        bookmarks = [
            ("Chromium API", "https://developer.chrome.com/", "Docs"),
            ("Python", "https://python.org/", "Languages"),
        ]
        self.assertEqual(
            module.build_bookmark_entries("", bookmarks, show_all=True),
            [
                ("bm", "Chromium API", "https://developer.chrome.com/", "Docs"),
                ("bm", "Python", "https://python.org/", "Languages"),
            ],
        )

    def test_format_lines_shows_bookmark_folder(self):
        line = module.format_lines(
            [("bm", "Chromium API", "https://developer.chrome.com/", "Docs/Chrome")]
        )[0]
        self.assertEqual(
            line["text"],
            "[bm] Chromium API  <i>developer.chrome.com/ · Docs/Chrome</i>",
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
        self.assertIn("prompt=Web❯", launcher)
        self.assertIn('-blocks-prompt "$prompt"', launcher)
        self.assertNotIn("-display-blocks", launcher)

    def test_launcher_binds_ctrl_tab_to_bookmark_mode_toggle(self):
        launcher = SCRIPT.with_name("rofi-hister").read_text()
        qtile = SCRIPT.parent.parent.joinpath(".config/qtile/config.py").read_text()
        self.assertIn("-kb-mode-next 'Shift+Right'", launcher)
        self.assertIn("-kb-custom-1 'Control+Tab'", launcher)
        self.assertNotIn("rofi-hister --bookmarks", qtile)

    def test_backend_handles_custom_key_as_mode_toggle(self):
        backend = SCRIPT.read_text()
        self.assertIn('elif name == "custom key" and value == "1":', backend)
        self.assertIn('"prompt": "Bookmarks❯" if bookmark_mode else "Web❯"', backend)

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
        native_manifest = (root / "rofi-chrome" / "io.github.amosbird.rofi.chrome.json").read_text()
        self.assertIn("jpgfhlaplofoaempbhliigmjbpofeghk", native_manifest)
        self.assertIn("aocepclkpgckjeikiphffdlileoaceec", native_manifest)
        host = (root / "rofi-chrome" / "host" / "main.py").read_text()
        self.assertIn("openInBrowser", host)
        self.assertIn('["xdg-open", url]', host)
        self.assertNotIn("rofi-browser-blocklist", host)
        self.assertNotIn('"copyDownload"', host)
        self.assertNotIn('["fcp"', host)
        self.assertIn('"RestoreOnStartup": 1', policy)
        self.assertNotIn('"BookmarkBarEnabled"', policy)
        self.assertNotIn("aocepclkpgckjeikiphffdlileoaceec", policy)
        self.assertIn("rofi-chrome-mode", restore)
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

    def test_luakit_delegates_desktop_switching_to_chromium_wrapper(self):
        launcher = SCRIPT.with_name("luakit").read_text()
        self.assertNotIn("qtile cmd-obj", launcher)
        self.assertIn('"$HOME"/scripts/chromium', launcher)

    def test_chromium_xkeysnail_tab_shortcuts(self):
        config = SCRIPT.parent.parent.joinpath("xkeysnail.py").read_text()
        chromium = config.split(
            're.compile(r"^(Chromium|Google-chrome|chatgpt|webchat)$")', 1
        )[1].split(
            '"Chromium Emacs-like keys"', 1
        )[0]
        self.assertIn('K("C-r"): K("C-Shift-t")', chromium)
        self.assertIn('re.compile(r"^(Chromium|Google-chrome|chatgpt|webchat)$")', config)
        self.assertIn('K("LM-w"): K("C-w")', chromium)
        self.assertNotIn('K("C-t"):', chromium)

    def test_chromium_remote_sends_native_singleton_message(self):
        with tempfile.TemporaryDirectory() as directory:
            profile = pathlib.Path(directory) / "profile"
            socket_dir = pathlib.Path(directory) / "socket"
            profile.mkdir()
            socket_dir.mkdir()
            socket_path = socket_dir / "SingletonSocket"
            socket_path.touch()
            (profile / "SingletonSocket").symlink_to(socket_path)
            (profile / "SingletonCookie").symlink_to("cookie")
            (socket_dir / "SingletonCookie").symlink_to("cookie")
            fake_socket = FakeSocket()
            with mock.patch.object(remote.socket, "socket", return_value=fake_socket):
                self.assertTrue(remote.forward(profile, ["baidu.com"], cwd="/tmp/work"))
        self.assertEqual(fake_socket.connected, str(socket_path))
        self.assertEqual(
            fake_socket.sent,
            b"START\0/tmp/work\0/usr/bin/google-chrome-stable\0baidu.com",
        )
        self.assertEqual(fake_socket.shutdown_mode, socket.SHUT_WR)
        self.assertEqual(fake_socket.timeout, 1)

    def test_chromium_remote_rejects_cookie_mismatch(self):
        with tempfile.TemporaryDirectory() as directory:
            profile = pathlib.Path(directory) / "profile"
            socket_dir = pathlib.Path(directory) / "socket"
            profile.mkdir()
            socket_dir.mkdir()
            socket_path = socket_dir / "SingletonSocket"
            socket_path.touch()
            (profile / "SingletonSocket").symlink_to(socket_path)
            (profile / "SingletonCookie").symlink_to("profile-cookie")
            (socket_dir / "SingletonCookie").symlink_to("socket-cookie")
            with mock.patch.object(remote.socket, "socket") as socket_factory:
                self.assertFalse(remote.forward(profile, ["baidu.com"]))
        socket_factory.assert_not_called()

    def test_chromium_remote_rechecks_cookie_after_connecting(self):
        with tempfile.TemporaryDirectory() as directory:
            profile = pathlib.Path(directory) / "profile"
            socket_dir = pathlib.Path(directory) / "socket"
            profile.mkdir()
            socket_dir.mkdir()
            socket_path = socket_dir / "SingletonSocket"
            socket_path.touch()
            (profile / "SingletonSocket").symlink_to(socket_path)
            (profile / "SingletonCookie").symlink_to("cookie")
            (socket_dir / "SingletonCookie").symlink_to("cookie")
            fake_socket = FakeSocket()
            original_readlink = remote.os.readlink
            reads = 0

            def replace_cookie_after_connect(path):
                nonlocal reads
                result = original_readlink(path)
                if pathlib.Path(path) == profile / "SingletonCookie":
                    reads += 1
                    if reads == 2:
                        return "replaced-cookie"
                return result

            with (
                mock.patch.object(remote.socket, "socket", return_value=fake_socket),
                mock.patch.object(remote.os, "readlink", side_effect=replace_cookie_after_connect),
            ):
                self.assertFalse(remote.forward(profile, ["baidu.com"]))
        self.assertIsNone(fake_socket.sent)

    def test_chromium_remote_falls_back_when_forwarding_fails(self):
        with mock.patch.object(remote, "forward", return_value=False):
            self.assertEqual(remote.main(["baidu.com"]), 1)

    def test_runai_bypasses_chromium_wrapper(self):
        launcher = SCRIPT.with_name("runai").read_text()
        self.assertIn("exec /usr/bin/google-chrome-stable", launcher)
        self.assertNotIn("exec /usr/bin/chromium", launcher)
        self.assertNotIn("\nchromium ", launcher)
        self.assertNotIn("\n# chromium ", launcher)

    def test_chromium_uses_installed_extensions(self):
        launcher = SCRIPT.with_name("chromium").read_text()
        self.assertIn('if chromium-remote "$@"', launcher)
        self.assertIn("exec /usr/bin/google-chrome-stable", launcher)
        self.assertNotIn("--load-extension", launcher)
        self.assertNotIn("--disable-extensions-except", launcher)

    def test_rofi_chrome_mode_switches_policy_without_loading_extensions(self):
        root = SCRIPT.parent.parent
        switcher = root / "scripts/rofi-chrome-mode"
        source = switcher.read_text()
        restore = (root / "restore.sh").read_text()
        self.assertIn("dev|store|status|id", source)
        self.assertIn("MODE_FILE", source)
        self.assertNotIn("--load-extension", source)
        self.assertIn('"$DIR/scripts/rofi-chrome-mode" install-policy', restore)
        self.assertIn("sudo install -d -m 755 /etc/opt /etc/opt/chrome", restore)

        with tempfile.TemporaryDirectory() as temp:
            env = {"HOME": temp, "XDG_STATE_HOME": f"{temp}/state"}
            dev_policy = json.loads(
                subprocess.run(
                    [switcher, "policy"], env=env, text=True, capture_output=True, check=True
                ).stdout
            )
            self.assertEqual(
                dev_policy["ExtensionSettings"]["jpgfhlaplofoaempbhliigmjbpofeghk"][
                    "installation_mode"
                ],
                "blocked",
            )
            self.assertEqual(
                dev_policy["ExtensionSettings"]["aocepclkpgckjeikiphffdlileoaceec"][
                    "installation_mode"
                ],
                "allowed",
            )
            mode_dir = pathlib.Path(env["XDG_STATE_HOME"]) / "rofi-chrome"
            mode_dir.mkdir(parents=True)
            (mode_dir / "mode").write_text("store\n")
            store_policy = json.loads(
                subprocess.run(
                    [switcher, "policy"], env=env, text=True, capture_output=True, check=True
                ).stdout
            )
            self.assertEqual(
                store_policy["ExtensionSettings"]["jpgfhlaplofoaempbhliigmjbpofeghk"][
                    "installation_mode"
                ],
                "normal_installed",
            )
            self.assertEqual(
                store_policy["ExtensionSettings"]["aocepclkpgckjeikiphffdlileoaceec"][
                    "installation_mode"
                ],
                "blocked",
            )

    def test_chromium_reuses_main_profile_and_switches_to_browser_group(self):
        root = SCRIPT.parent.parent
        launcher = SCRIPT.with_name("chromium").read_text()
        qtile_config = (root / ".config/qtile/config.py").read_text()
        self.assertIn('[[["group","f"]],"toscreen",[],{},true]', launcher)
        self.assertIn('nc -U -N "$HOME/.cache/qtile/qtilesocket.$DISPLAY"', launcher)
        self.assertNotIn("qtile cmd-obj", launcher)
        self.assertIn("--restore-last-session", launcher)
        self.assertIn("--hide-crash-restore-bubble", launcher)
        self.assertIn(
            "--disable-features=OverscrollHistoryNavigation,"
            "TouchpadOverscrollHistoryNavigation",
            launcher,
        )
        self.assertIn("--disable-smooth-scrolling", launcher)
        self.assertIn('--user-data-dir="$profile"', launcher)
        self.assertIn(
            "chrome-extension://jpgfhlaplofoaempbhliigmjbpofeghk/download.html",
            qtile_config,
        )
        self.assertIn("jpgfhlaplofoaempbhliigmjbpofeghk__bookmarks.html", qtile_config)

    def test_chromium_popups_float_and_match_telegram_geometry(self):
        root = SCRIPT.parent.parent
        qtile_config = (root / ".config/qtile/config.py").read_text()
        managed_hook = qtile_config.split("def after_window_created(client):", 1)[1]
        self.assertIn('Match(wm_class="Google-chrome", role="pop-up")', qtile_config)
        self.assertIn('client.get_wm_role() == "pop-up"', managed_hook)
        self.assertIn("int(screen.width * 0.7)", managed_hook)
        self.assertIn("int(screen.height * 0.8)", managed_hook)
        self.assertIn("int(screen.x + screen.width * 0.15)", managed_hook)
        self.assertIn("int(screen.y + screen.height * 0.1)", managed_hook)

    def test_google_chrome_is_main_browser(self):
        root = SCRIPT.parent.parent
        launcher = SCRIPT.with_name("chromium").read_text()
        remote = SCRIPT.with_name("chromium-remote").read_text()
        qtile_config = (root / ".config/qtile/config.py").read_text()
        restore = (root / "restore.sh").read_text()
        self.assertIn('profile="$HOME/.config/google-chrome-main"', launcher)
        self.assertIn("exec /usr/bin/google-chrome-stable", launcher)
        self.assertIn('PROFILE = Path.home() / ".config/google-chrome-main"', remote)
        self.assertIn("--user-data-dir=\"$profile\"", launcher)
        self.assertIn('"/usr/bin/google-chrome-stable"', remote)
        self.assertIn('Match(wm_class="Google-chrome")', qtile_config)
        self.assertIn(".config/google-chrome-main/NativeMessagingHosts", restore)
        self.assertIn("rofi-chrome-mode", restore)

    def test_default_browser_uses_chromium_wrapper(self):
        root = SCRIPT.parent.parent
        mimeapps = (root / ".config/mimeapps.list").read_text()
        desktop = (root / ".local/share/applications/chromium-amos.desktop").read_text()
        self.assertIn("x-scheme-handler/http=chromium-amos.desktop;", mimeapps)
        self.assertIn("x-scheme-handler/https=chromium-amos.desktop;", mimeapps)
        self.assertIn("text/html=chromium-amos.desktop;", mimeapps)
        self.assertIn("Exec=/home/amos/scripts/chromium %U", desktop)

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
                module,
                "activate_chromium_tab",
                side_effect=lambda target: calls.append("tab") or True,
            ),
            mock.patch.object(
                module, "focus_browser_group", side_effect=lambda: calls.append("desktop")
            ),
        ):
            module.open_entry(
                {"kind": "tab", "url": "https://example.com", "target_id": "target"}
            )
        self.assertEqual(calls, ["tab", "desktop"])

    def test_open_bookmark_activates_matching_tab(self):
        open_tabs = [("Open", "https://example.com", "target")]
        calls = []
        with (
            mock.patch.object(module, "tabs", open_tabs),
            mock.patch.object(
                module,
                "activate_chromium_tab",
                side_effect=lambda target: calls.append(target) or True,
            ),
            mock.patch.object(
                module,
                "focus_browser_group",
                side_effect=lambda: calls.append("focus"),
            ),
            mock.patch.object(module, "launch_url") as launch,
        ):
            module.open_entry({"kind": "bm", "url": "https://example.com"})
        self.assertEqual(calls, ["target", "focus"])
        launch.assert_not_called()

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
