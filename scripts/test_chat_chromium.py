#!/usr/bin/env python3

import json
import pathlib
import subprocess
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
RUNCHAT = ROOT / "scripts/runchat"
MANIFEST = ROOT / "chromium/external-links/extension/manifest.json"
CONTENT = MANIFEST.with_name("content.js")
QTILE = ROOT / ".config/qtile/config.py"
CHROMIUM = ROOT / "scripts/chromium"


class ChatChromiumTest(unittest.TestCase):
    def test_launcher_uses_one_non_kiosk_profile_with_explicit_tabs(self):
        launcher = RUNCHAT.read_text()
        self.assertIn('--disable-extensions-except="$extension"', launcher)
        self.assertIn("profile=/home/amos/.config/chrome-chat", launcher)
        self.assertIn('--user-data-dir="$profile"', launcher)
        self.assertIn("--class=webchat", launcher)
        self.assertIn("--remote-debugging-port=9224", launcher)
        self.assertNotIn("--kiosk", launcher)
        urls = (
            "https://web.whatsapp.com/",
            "https://app.slack.com/client/TUEGQCHJT/CUC7EE9N0",
            "https://discord.com/app",
        )
        positions = [launcher.index(url) for url in urls]
        self.assertEqual(positions, sorted(positions))

    def test_fixed_tab_launchers_discard_crash_sessions_only_for_new_instances(self):
        for launcher in (RUNCHAT, ROOT / "scripts/runai"):
            script = launcher.read_text()
            self.assertIn('if ! "$remote" "$profile" --check; then', script)
            self.assertIn('rm -rf "$profile/Default/Sessions"', script)
            self.assertIn('["exit_type"] = "Normal"', script)

    def test_shared_extension_keeps_all_managed_sites_internal(self):
        manifest = json.loads(MANIFEST.read_text())
        matches = manifest["content_scripts"][0]["matches"]
        self.assertEqual(matches, ["http://*/*", "https://*/*"])

        script = f"""
const {{ shouldRouteLink }} = require({json.dumps(str(CONTENT))});
const cases = [
    ['https://discord.com/app', false],
    ['https://web.whatsapp.com/', false],
    ['https://app.slack.com/client/', false],
    ['https://example.com/', true],
];
for (const [url, expected] of cases) {{
    if (shouldRouteLink('https://discord.com/app', url) !== expected) process.exit(1);
}}
"""
        subprocess.run(["node", "-e", script], check=True)

    def test_all_browser_profiles_disable_default_browser_prompt(self):
        for launcher in (CHROMIUM, RUNCHAT, ROOT / "scripts/runai"):
            self.assertIn("--no-default-browser-check", launcher.read_text())

    def test_bookmark_manager_never_bootstraps_a_hidden_browser(self):
        launcher = (ROOT / "scripts/bookmark-manager").read_text()
        self.assertNotIn("--no-startup-window", launcher)
        self.assertNotIn("/json/version", launcher)
        self.assertIn('exec /home/amos/scripts/chromium --app="$url"', launcher)

    def test_all_browser_profiles_disable_smooth_scrolling(self):
        launchers = (CHROMIUM, RUNCHAT, ROOT / "scripts/runai")
        for launcher in launchers:
            self.assertIn("--disable-smooth-scrolling", launcher.read_text())

    def test_one_time_service_worker_migration_has_been_removed(self):
        for launcher in (CHROMIUM, RUNCHAT, ROOT / "scripts/runai"):
            script = launcher.read_text()
            self.assertNotIn("router_state=", script)
            self.assertNotIn("Service Worker.stale", script)

    def test_main_chromium_loads_router_and_registers_native_host(self):
        launcher = CHROMIUM.read_text()
        self.assertIn('--disable-extensions-except="$extension,', launcher)
        self.assertIn('--load-extension="$extension,', launcher)
        self.assertIn("io.github.amosbird.browser_router.json", launcher)

    def test_main_browser_only_routes_links_into_managed_groups(self):
        script = f"""
const {{ shouldRouteLink }} = require({json.dumps(str(CONTENT))});
const cases = [
    ['https://example.com/', 'https://app.slack.com/client/', true],
    ['https://example.com/', 'https://acme.slack.com/archives/C1', true],
    ['https://example.com/', 'https://chatgpt.com/', true],
    ['https://example.com/', 'https://accounts.google.com/o/oauth2/auth', false],
    ['https://example.com/', 'https://kernel.org/', false],
];
for (const [page, link, expected] of cases) {{
    if (shouldRouteLink(page, link) !== expected) process.exit(1);
}}
"""
        subprocess.run(["node", "-e", script], check=True)

    def test_qtile_has_one_webchat_dropdown_and_no_native_discord(self):
        config = QTILE.read_text()
        self.assertIn("def show_scratchpad(name):", config)
        self.assertIn("qtile.show_scratchpad = show_scratchpad", config)
        self.assertIn('self.show_scratchpad("webchat")', RUNCHAT.read_text())
        self.assertIn('self.show_scratchpad("chatgpt")', (ROOT / "scripts/runai").read_text())
        self.assertIn('"webchat": Match(wm_class="webchat")', config)
        self.assertIn('"webchat",\n                "runchat",', config)
        self.assertNotIn('"discord",\n                "discord",', config)
        self.assertIn(
            'Key([ctrl, alt], "minus", toggle_scratchpad("webchat"))',
            config,
        )


if __name__ == "__main__":
    unittest.main()
