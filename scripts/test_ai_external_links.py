#!/usr/bin/env python3

import importlib.util
import json
import pathlib
import subprocess
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
ROUTES = ROOT / "chromium/external-links/routes.json"
HOST = ROOT / "chromium/external-links/host.py"
CONTENT = ROOT / "chromium/external-links/extension/content.js"
MANIFEST = CONTENT.with_name("manifest.json")
NATIVE_MANIFEST = ROOT / "chromium/external-links/native-host.json"
RUNAI = ROOT / "scripts/runai"


class ExternalLinkExtensionTest(unittest.TestCase):
    def test_links_only_stay_local_inside_the_same_browser_group(self):
        script = f"""
const {{ shouldRouteLink }} = require({json.dumps(str(CONTENT))});
const cases = [
    ['https://chatgpt.com/', 'https://claude.ai/new', false],
    ['https://chatgpt.com/', 'https://accounts.google.com/o/oauth2/auth', false],
    ['https://chatgpt.com/', 'https://discord.com/app', true],
    ['https://discord.com/app', 'https://app.slack.com/client/', false],
    ['https://app.slack.com/', 'https://slack.com/openid/connect/login', false],
    ['https://app.slack.com/', 'https://accounts.google.com/o/oauth2/auth', false],
    ['https://accounts.google.com/o/oauth2/auth', 'https://app.slack.com/client/', false],
    ['https://discord.com/app', 'https://chatgpt.com/', true],
    ['https://discord.com/app', 'https://example.com/', true],
    ['https://chatgpt.com/', 'mailto:test@example.com', false],
];
for (const [page, link, expected] of cases) {{
    if (shouldRouteLink(page, link) !== expected) process.exit(1);
}}
"""
        subprocess.run(["node", "-e", script], check=True)

    def test_shared_route_config_drives_host_and_extension(self):
        routes = json.loads(ROUTES.read_text())
        self.assertEqual(routes["groups"]["chat"]["debug_port"], 9224)
        self.assertIn(".slack.com", routes["groups"]["chat"]["suffixes"])
        content = CONTENT.read_text()
        routes_js = CONTENT.with_name("routes.js").read_text()
        host = HOST.read_text()
        self.assertIn("const ROUTES = {", routes_js)
        self.assertIn("ROUTES = json.loads(ROUTES_PATH.read_text())", host)
        self.assertNotIn("AI_SITES =", host)
        self.assertNotIn("exactSiteGroups", content)

    def test_background_never_closes_tabs_opened_from_managed_apps(self):
        background = CONTENT.with_name("background.js").read_text()
        self.assertNotIn("chrome.tabs.remove", background)
        self.assertNotIn("chrome.tabs.onUpdated.addListener", background)

    def test_extension_and_native_host_have_stable_matching_id(self):
        manifest = json.loads(MANIFEST.read_text())
        native_manifest = json.loads(NATIVE_MANIFEST.read_text())
        self.assertIn("key", manifest)
        self.assertNotIn("update_url", manifest)
        self.assertEqual(
            native_manifest["allowed_origins"],
            ["chrome-extension://ahonjbfnmpjgjgaabbppofphidfomgcj/"],
        )

    def test_runai_uses_installed_extension_and_registers_native_host(self):
        launcher = RUNAI.read_text()
        self.assertNotIn("--disable-extensions-except", launcher)
        self.assertNotIn("--load-extension", launcher)
        self.assertIn("exec /usr/bin/google-chrome-stable", launcher)
        self.assertIn("--kiosk", launcher)
        self.assertNotIn("--start-fullscreen", launcher)
        self.assertIn("--remote-debugging-port=9223", launcher)
        self.assertIn("profile=/home/amos/.config/chrome-chatgpt", launcher)
        self.assertIn(
            'host="$profile/NativeMessagingHosts/io.github.amosbird.browser_router.json"',
            launcher,
        )
        self.assertTrue(HOST.stat().st_mode & 0o111)

    def test_linux_external_extension_is_installed_by_restore(self):
        restore = (ROOT / "restore.sh").read_text()
        packager = (ROOT / "scripts/package-external-links").read_text()
        self.assertIn('pass show chrome/external-links.pem > "$key"', packager)
        self.assertIn('expected = "ahonjbfnmpjgjgaabbppofphidfomgcj"', packager)
        self.assertIn("public_key != manifest_key", packager)
        self.assertIn('state="$output/build-state.json"', packager)
        self.assertIn('source_hash=$(', packager)
        self.assertIn('if [[ $source_hash != "$previous_hash"', packager)
        self.assertIn("'.version = $version'", packager)
        self.assertIn('"external_crx": "$output/extension.crx"', packager)
        self.assertIn('"external_version": "$version"', packager)
        self.assertNotIn("<gupdate", packager)
        self.assertIn("package-external-links", restore)
        self.assertGreater(
            restore.index("package-external-links"),
            restore.index("git pull"),
        )
        self.assertIn("/opt/google/chrome/extensions/", restore)
        self.assertIn("package/external-extension.json", restore)
        self.assertFalse((ROOT / "chromium/extensions-policy.json").exists())
        self.assertNotIn("ExtensionInstallForcelist", restore)

    def test_shared_extension_provides_scroll_commands_for_isolated_profiles(self):
        manifest = json.loads(MANIFEST.read_text())
        background = CONTENT.with_name("background.js").read_text()
        self.assertEqual(
            manifest["commands"]["scrollToTop"]["suggested_key"]["default"],
            "Alt+Shift+Home",
        )
        self.assertEqual(
            manifest["commands"]["scrollToBottom"]["suggested_key"]["default"],
            "Alt+Shift+End",
        )
        self.assertNotIn("debugger", manifest["permissions"])
        self.assertNotIn("scripting", manifest["permissions"])
        self.assertNotIn("activeTab", manifest["permissions"])
        self.assertIn("chrome.commands.onCommand", background)
        self.assertIn('sendNativeRequest({', background)
        self.assertIn("connectNative", background)
        self.assertIn("url: tab?.url", background)
        self.assertIn("scrollQueues", background)
        self.assertNotIn("chrome.debugger", background)
        self.assertIn('"Input.dispatchKeyEvent"', HOST.read_text())
        self.assertIn('"ScrollToBeginningOfDocument"', HOST.read_text())
        self.assertIn('"ScrollToEndOfDocument"', HOST.read_text())
        self.assertNotIn("scrollCommand", CONTENT.read_text())
        self.assertNotIn("xdotool", background)
        self.assertNotIn('addEventListener("keydown"', CONTENT.read_text())

    def test_background_routes_content_script_messages(self):
        background = CONTENT.with_name("background.js").read_text()
        self.assertIn("chrome.runtime.onMessage.addListener", background)
        self.assertIn("chrome.notifications.create", background)

    def test_site_languages_are_managed_by_independent_config(self):
        manifest = json.loads(MANIFEST.read_text())
        config = json.loads(MANIFEST.with_name("site-languages.json").read_text())
        background = CONTENT.with_name("background.js").read_text()
        self.assertIn("declarativeNetRequest", manifest["permissions"])
        self.assertNotIn("declarative_net_request", manifest)
        self.assertEqual(config, {"zh-CN,zh;q=0.9": ["woa.com"]})
        self.assertIn('fetch(chrome.runtime.getURL("site-languages.json"))', background)
        self.assertIn("chrome.declarativeNetRequest.updateDynamicRules", background)
        self.assertIn("requestDomains: domains", background)

    def test_external_links_extension_only_routes_links(self):
        background = CONTENT.with_name("background.js").read_text()
        manifest = json.loads(MANIFEST.read_text())
        self.assertNotIn("chrome.downloads", background)
        self.assertNotIn("chrome.offscreen", background)
        self.assertNotIn("clipboardWrite", manifest["permissions"])
        self.assertNotIn("downloads", manifest["permissions"])
        self.assertNotIn("offscreen", manifest["permissions"])
        self.assertNotIn('message.get("command") == "key"', HOST.read_text())


class NativeHostTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        spec = importlib.util.spec_from_file_location("ai_external_links_host", HOST)
        cls.host = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.host)

    @mock.patch("subprocess.Popen")
    def test_authentication_urls_stay_in_the_calling_browser(self, popen):
        self.assertEqual(
            self.host.handle_message(
                {"url": "https://accounts.google.com/o/oauth2/auth", "sourceGroup": "chat"}
            ),
            {"ok": True},
        )
        popen.assert_not_called()

    @mock.patch("subprocess.Popen")
    def test_routes_urls_to_their_own_browser(self, popen):
        routes = {
            "https://chatgpt.com/c/1": "runai",
            "https://discord.com/channels/1": "runchat",
            "https://example.com/": "chromium",
        }
        with mock.patch.object(self.host, "reuse_tab", return_value=False):
            for url, launcher in routes.items():
                with self.subTest(url=url):
                    self.assertEqual(self.host.handle_message({"url": url}), {"ok": True})
                    popen.assert_called_with([str(pathlib.Path.home() / "scripts" / launcher), url])

    @mock.patch("subprocess.Popen")
    def test_reuses_existing_app_tab_before_launching(self, popen):
        with mock.patch.object(self.host, "reuse_tab", return_value=True) as reuse_tab:
            self.assertEqual(
                self.host.handle_message({"url": "https://app.slack.com/client/T1/C1"}),
                {"ok": True},
            )
        reuse_tab.assert_called_once_with("chat", "app.slack.com", "https://app.slack.com/client/T1/C1")
        popen.assert_not_called()

    @mock.patch("scripts.test_ai_external_links.NativeHostTest.host.scroll_page", return_value=True)
    def test_scroll_command_uses_cdp(self, scroll_page):
        self.assertEqual(
            self.host.handle_message({
                "command": "scroll",
                "direction": "scrollToBottom",
                "url": "https://example.com/page",
            }),
            {"ok": True},
        )
        scroll_page.assert_called_once_with(
            "scrollToBottom", url="https://example.com/page"
        )

    @mock.patch("scripts.test_ai_external_links.NativeHostTest.host.scroll_page", return_value=False)
    def test_invalid_scroll_command_fails(self, scroll_page):
        self.assertEqual(
            self.host.handle_message({"command": "scroll", "direction": "invalid"}),
            {"ok": False},
        )
        scroll_page.assert_called_once_with("invalid", url=None)

    @mock.patch("subprocess.run")
    def test_rejects_non_http_url(self, run):
        self.assertEqual(self.host.handle_message({"url": "file:///etc/passwd"}), {"ok": False})
        run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
