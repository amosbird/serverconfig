#!/usr/bin/env python3

import importlib.util
import io
import json
import pathlib
import shutil
import subprocess
import tempfile
import unittest
import zipfile
from html.parser import HTMLParser
from unittest import mock

ROOT = pathlib.Path(__file__).resolve().parents[1]
ROUTES = ROOT / "chromium/external-links/routes.json"
HOST = ROOT / "chromium/external-links/host.py"
CONTENT = ROOT / "chromium/external-links/extension/content.js"
MANIFEST = CONTENT.with_name("manifest.json")
NATIVE_MANIFEST = ROOT / "chromium/external-links/native-host.json"
ASSET_HELPER = ROOT / "chromium/external-links/package_assets.py"
spec = importlib.util.spec_from_file_location("package_assets", ASSET_HELPER)
package_assets = importlib.util.module_from_spec(spec)
spec.loader.exec_module(package_assets)
RUNAI = ROOT / "scripts/runai"


class ExternalLinkExtensionTest(unittest.TestCase):
    def run_node(self, script):
        result = subprocess.run(["node", "-e", script], capture_output=True, text=True, timeout=6)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_content_retries_unknown_ownership_without_reload(self):
        script = f"""
const path = {json.dumps(str(CONTENT))};
const callbacks = [];
global.ROUTES = require({json.dumps(str(CONTENT.with_name('routes.js')))}).ROUTES;
global.document = {{
    listener: null,
    addEventListener(_name, listener) {{ this.listener = listener; }},
}};
global.location = {{href: 'https://discord.com/app'}};
global.chrome = {{runtime: {{
    sent: [],
    sendMessage(message, callback) {{ this.sent.push(message); callbacks.push(callback); }},
}}}};
require(path);
if (chrome.runtime.sent.length !== 1) process.exit(1);
callbacks.shift()({{owned: null, ready: false, ok: false, route: false}});
setImmediate(() => {{
    let prevented = false;
    document.listener({{
        defaultPrevented: false, button: 0,
        target: {{closest: () => ({{href: 'https://chatgpt.com/'}})}},
        preventDefault() {{ prevented = true; }}, stopImmediatePropagation() {{}},
    }});
    if (prevented || chrome.runtime.sent.length !== 2) process.exit(2);
    callbacks.shift()({{owned: false, generation: 1, ready: true, ok: true, route: false}});
    setImmediate(() => {{
        document.listener({{
            defaultPrevented: false, button: 0,
            target: {{closest: () => ({{href: 'https://chatgpt.com/'}})}},
            preventDefault() {{ prevented = true; }}, stopImmediatePropagation() {{}},
        }});
        if (!prevented || chrome.runtime.sent.length !== 3 ||
                chrome.runtime.sent[2].url !== 'https://chatgpt.com/') process.exit(3);
    }});
}});
"""
        self.run_node(script)

    def test_content_invalidates_owned_cache_and_routes_next_click_without_reload(self):
        script = f"""
const path = {json.dumps(str(CONTENT))};
const callbacks = [];
let messageListener;
global.ROUTES = require({json.dumps(str(CONTENT.with_name('routes.js')))}).ROUTES;
global.document = {{
    listener: null,
    addEventListener(_name, listener) {{ this.listener = listener; }},
}};
global.location = {{href: 'https://discord.com/app'}};
global.chrome = {{runtime: {{
    sent: [],
    onMessage: {{addListener(listener) {{ messageListener = listener; }}}},
    sendMessage(message, callback) {{
        this.sent.push(message);
        if (callback) callbacks.push(callback);
    }},
}}}};
require(path);
callbacks.shift()({{owned: true, generation: 7, ready: true, ok: true, route: false}});
setImmediate(() => {{
    let prevented = false;
    document.listener({{
        defaultPrevented: false, button: 0,
        target: {{closest: () => ({{href: 'https://chatgpt.com/'}})}},
        preventDefault() {{ prevented = true; }}, stopImmediatePropagation() {{}},
    }});
    if (prevented || chrome.runtime.sent.length !== 1) process.exit(1);
    messageListener({{type: 'abush_ownership_changed', owned: false, generation: 8}});
    document.listener({{
        defaultPrevented: false, button: 0,
        target: {{closest: () => ({{href: 'https://chatgpt.com/'}})}},
        preventDefault() {{ prevented = true; }}, stopImmediatePropagation() {{}},
    }});
    if (!prevented || chrome.runtime.sent.length !== 2 ||
            chrome.runtime.sent[1].url !== 'https://chatgpt.com/') process.exit(2);
}});
"""
        self.run_node(script)

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
        self.run_node(script)

    def test_shared_route_config_drives_host_and_extension(self):
        routes = json.loads(ROUTES.read_text())
        self.assertEqual(routes["groups"]["chat"]["debug_port"], 9224)
        self.assertIn(".slack.com", routes["groups"]["chat"]["suffixes"])
        content = CONTENT.read_text()
        routes_js = CONTENT.with_name("routes.js").read_text()
        host = HOST.read_text()
        launcher = (ROOT / "scripts/chromium").read_text()
        self.assertIn("const ROUTES = {", routes_js)
        self.assertIn("ROUTES = json.loads(ROUTES_PATH.read_text())", host)
        self.assertIn("--remote-debugging-port=9222", launcher)
        self.assertIn("ABUSH_CHROME_DEBUG_PORT", host)
        self.assertIn('"9222"', host)
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
        worker = manifest["background"]["service_worker"]
        self.assertEqual(worker, "background.js")
        worker_path = MANIFEST.with_name(worker)
        self.assertTrue(worker_path.is_file())
        self.assertEqual(
            native_manifest["allowed_origins"],
            ["chrome-extension://ahonjbfnmpjgjgaabbppofphidfomgcj/"],
        )

    def test_v9_consent_owns_only_the_exact_enabled_tab(self):
        script = f"""
const {{assert, fixture}} = require({json.dumps(str(ROOT / 'scripts/browser_tab_groups_fixture.js'))});
(async () => {{
    const f = fixture();
    await f.ready();
    assert.equal(f.executor.state('remote').phase, 'disabled');
    assert.equal(f.executor.records.size, 0);
    assert.equal(f.tabs.size, 2);
    const record = await f.enable();
    assert.equal(f.executor.state('remote').phase, 'enabled');
    assert.equal(f.executor.ownsTab(record.tab_id), true);
    assert.equal(record.window_id, 1);
    assert.equal(f.executor.ownsTab(1), false);
    assert.equal(f.tabs.get(1).groupId, -1);
    assert.equal(f.tabs.get(record.tab_id).groupId, record.group_id);
    assert.equal(f.calls.filter(call => call.name === 'windows.create').length, 0);
    assert.deepEqual(f.local.abushOwnedMarkers, {{legacy: true}});
    await f.send({{control: 'disconnect', remote_key: 'remote'}});
    assert.equal(f.executor.ownsTab(record.tab_id), false);
    assert.equal(f.tabs.size, 3, 'disconnect must not close any tab');
}})().catch(error => {{ console.error(error); process.exitCode = 1; }});
"""
        self.run_node(script)

    def asset_fixture(self):
        common = pathlib.Path(subprocess.check_output(
            ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"],
            cwd=ROOT, text=True).strip())
        temporary_root = common.parent / ".worktrees/.tmp"
        temporary_root.mkdir(parents=True, exist_ok=True)
        temporary = tempfile.TemporaryDirectory(prefix="package-assets-", dir=temporary_root)
        self.addCleanup(temporary.cleanup)
        root = pathlib.Path(temporary.name)
        source = root / "source"
        shutil.copytree(MANIFEST.parent, source)
        return source, root / "staged"

    def asset_snapshot(self, source):
        return {str(path.relative_to(source)): path.read_bytes()
                for path in source.rglob("*") if path.is_file()}

    def test_packaged_worker_assets_are_correlated_with_exact_package_version(self):
        source, staged = self.asset_fixture()
        original = self.asset_snapshot(MANIFEST.parent)
        version = "2026.3.9.1234"
        suffix = version.replace(".", "-")
        worker = f"background-{suffix}.js"
        workspace = f"workspace-{suffix}.js"
        transport = f"playwright-transport-{suffix}.js"
        package_assets.stage_assets(source, staged, version)
        expected = dict(original)
        manifest = json.loads(expected["manifest.json"])
        manifest["version"] = version
        manifest["background"]["service_worker"] = worker
        expected["manifest.json"] = json.dumps(manifest, separators=(",", ":")).encode()
        expected[transport] = expected.pop("playwright-transport.js")
        expected[workspace] = expected.pop("workspace.js").replace(
            b'importScripts("playwright-transport.js")',
            f'importScripts("{transport}")'.encode(), 1).replace(
            b'require("./playwright-transport.js")', f'require("./{transport}")'.encode(), 1)
        expected[worker] = expected.pop("background.js").replace(
            b'importScripts("workspace.js")', f'importScripts("{workspace}")'.encode(), 1).replace(
            b'require("./workspace.js")', f'require("./{workspace}")'.encode())
        self.assertEqual(self.asset_snapshot(staged), expected)
        self.assertEqual(self.asset_snapshot(source), original)
        self.assertEqual(self.asset_snapshot(MANIFEST.parent), original)
        archive_path = staged.parent / "assets.zip"
        with zipfile.ZipFile(archive_path, "w") as archive:
            for path in staged.iterdir():
                archive.write(path, path.name)
        with zipfile.ZipFile(archive_path) as archive:
            self.assertEqual(set(archive.namelist()), set(expected))
            for name, content in expected.items():
                self.assertEqual(archive.read(name), content)
        self.assertNotIn("background.js", expected)
        self.assertNotIn("workspace.js", expected)
        self.assertNotIn("playwright-transport.js", expected)
        self.assertIn(f'importScripts("{transport}")'.encode(), expected[workspace])
        self.assertIn(f'require("./{transport}")'.encode(), expected[workspace])
        self.assertIn(b'abushBrowserAutomationV9', expected[workspace])
        self.assertIn(b'requestBootstrap(port)', expected[worker])
        self.assertNotIn(b'"workspace.js"', expected[worker])
        self.assertNotIn(b'"./workspace.js"', expected[worker])
        self.assertIn("tabGroups", manifest["permissions"])
        self.assertEqual(manifest["action"]["default_popup"], "popup.html")
        for name in ("popup.html", "popup.js", "popup.css"):
            self.assertEqual(expected[name], original[name])

        class CspParser(HTMLParser):
            def handle_starttag(parser, tag, attrs):
                attributes = dict(attrs)
                self.assertFalse(any(name.lower().startswith("on") for name, _ in attrs),
                                 f"inline event handler in {tag}")
                if tag == "script":
                    self.assertIn("src", attributes, "inline script violates extension CSP")
                    self.assertIn(attributes["src"], expected)
                if tag == "link" and attributes.get("rel") == "stylesheet":
                    self.assertIn(attributes["href"], expected)

        for name, content in expected.items():
            if name.endswith(".html"):
                CspParser().feed(content.decode())

    def test_asset_staging_command_matches_shared_function(self):
        source, staged = self.asset_fixture()
        original = self.asset_snapshot(source)
        package_assets.stage_assets(source, staged, "1.2.3.4")
        command_staged = staged.with_name("command-staged")
        subprocess.run(["python3", "-B", str(ASSET_HELPER), str(source),
                        str(command_staged), "1.2.3.4"], check=True, timeout=5)
        self.assertEqual(self.asset_snapshot(command_staged), self.asset_snapshot(staged))
        self.assertEqual(self.asset_snapshot(source), original)

    def test_asset_staging_rejects_invalid_versions_without_writing(self):
        source, staged = self.asset_fixture()
        original = self.asset_snapshot(source)
        for version in ("", "0", "0.0.0.0", "1.2.3.4.5", "01.2", "65536.1",
                        "1.-2", "1.2/../../escape", "../escape", "/escape", "1.2\n"):
            with self.subTest(version=version), self.assertRaises(ValueError):
                package_assets.stage_assets(source, staged, version)
            self.assertFalse(staged.exists())
            self.assertEqual(self.asset_snapshot(source), original)

    def test_asset_staging_rejects_existing_destinations_without_overwriting(self):
        source, staged = self.asset_fixture()
        original = self.asset_snapshot(source)
        for destination in (source, staged):
            if destination == staged:
                staged.mkdir()
                (staged / "sentinel").write_bytes(b"unchanged")
            before = self.asset_snapshot(destination)
            with self.subTest(destination=destination), self.assertRaises(FileExistsError):
                package_assets.stage_assets(source, destination, "1.2.3.4")
            self.assertEqual(self.asset_snapshot(destination), before)
        self.assertEqual(self.asset_snapshot(source), original)

    def test_asset_staging_rejects_paths_inside_source(self):
        source, staged = self.asset_fixture()
        original = self.asset_snapshot(source)
        for destination in (source / "nested", staged.parent / "source/../source/nested"):
            with self.subTest(destination=destination), self.assertRaises(ValueError):
                package_assets.stage_assets(source, destination, "1.2.3.4")
        self.assertEqual(self.asset_snapshot(source), original)
        self.assertFalse((source / "nested").exists())

    def test_asset_staging_rejects_symlink_escapes(self):
        source, staged = self.asset_fixture()
        outside = staged.parent / "outside"
        outside.write_bytes(b"unchanged")
        (source / "workspace.js").unlink()
        (source / "workspace.js").symlink_to(outside)
        with self.assertRaises(ValueError):
            package_assets.stage_assets(source, staged, "1.2.3.4")
        self.assertFalse(staged.exists())
        self.assertEqual(outside.read_bytes(), b"unchanged")
        (source / "workspace.js").unlink()
        shutil.copyfile(MANIFEST.with_name("workspace.js"), source / "workspace.js")
        original = self.asset_snapshot(source)
        staged.symlink_to(source, target_is_directory=True)
        with self.assertRaises(FileExistsError):
            package_assets.stage_assets(source, staged, "1.2.3.4")
        self.assertEqual(self.asset_snapshot(source), original)

    def test_asset_staging_rejects_noncanonical_sources_and_asset_collisions(self):
        source, staged = self.asset_fixture()
        for name, content in (
                ("manifest.json", json.dumps({"background": {"service_worker": "../escape"}})),
                ("background.js", 'importScripts("../escape")'),
                ("workspace.js", 'importScripts("../escape")'),
                ("playwright-transport-1-2-3-4.js", "unexpected transport"),
                ("workspace-1-2-3-4.js", "unexpected workspace"),
                ("background-1-2-3-4.js", "unexpected worker")):
            path = source / name
            previous = path.read_bytes() if path.exists() else None
            path.write_text(content)
            original = self.asset_snapshot(source)
            with self.subTest(name=name), self.assertRaises((ValueError, FileExistsError)):
                package_assets.stage_assets(source, staged, "1.2.3.4")
            self.assertFalse(staged.exists())
            self.assertEqual(self.asset_snapshot(source), original)
            if previous is None:
                path.unlink()
            else:
                path.write_bytes(previous)

    def test_asset_staging_missing_transport_fails_before_writes(self):
        source, staged = self.asset_fixture()
        (source / "playwright-transport.js").unlink()
        original = self.asset_snapshot(source)
        with self.assertRaises(FileNotFoundError):
            package_assets.stage_assets(source, staged, "1.2.3.4")
        self.assertFalse(staged.exists())
        self.assertEqual(self.asset_snapshot(source), original)

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
        helper = ASSET_HELPER.read_text()
        self.assertIn(
            'python3 "$root/package_assets.py" "$extension" "$work/extension" "$version"',
            packager)
        self.assertIn('manifest["version"] = version', helper)
        self.assertIn('versioned_background = background.replace(', helper)
        self.assertIn("f'importScripts(\"{versioned_workspace}\")'", helper)
        self.assertIn('(destination / "background.js").unlink()', helper)
        self.assertIn('(destination / "workspace.js").unlink()', helper)
        self.assertIn('manifest["background"]["service_worker"] = versioned_worker', helper)
        self.assertIn('versioned_workspace = f"workspace-{suffix}.js"', helper)
        self.assertIn('versioned_worker = f"background-{suffix}.js"', helper)
        self.assertIn('--pack-extension-key="$key"', packager)
        self.assertIn('header[:4] != b"Cr24"', packager)
        self.assertIn('if version != 3:', packager)
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
        workspace = CONTENT.with_name("workspace.js").read_text()
        self.assertEqual(
            manifest["commands"]["scrollToTop"]["suggested_key"]["default"],
            "Alt+Shift+Home",
        )
        self.assertEqual(
            manifest["commands"]["scrollToBottom"]["suggested_key"]["default"],
            "Alt+Shift+End",
        )
        self.assertIn("debugger", manifest["permissions"])
        self.assertIn("storage", manifest["permissions"])
        self.assertNotIn("host_permissions", manifest)
        for dangerous in ("scripting", "activeTab", "downloads", "offscreen", "clipboardWrite"):
            self.assertNotIn(dangerous, manifest["permissions"])
        self.assertIn("chrome.commands.onCommand", background)
        self.assertIn('sendNativeRequest("external_links", "scroll", {', background)
        self.assertIn("connectNative", background)
        self.assertIn("reconnect_exhausted", background)
        self.assertIn("Native browser integration could not reconnect", background)
        self.assertIn("url: tab?.url", background)
        self.assertIn("scrollQueues", background)
        self.assertIn("chrome.debugger", background)
        self.assertNotIn("isAllowedFileSchemeAccess", workspace)
        self.assertIn('"ordinary_file_navigate"', workspace)
        self.assertIn('"Input.dispatchKeyEvent"', HOST.read_text())
        self.assertIn('"ScrollToBeginningOfDocument"', HOST.read_text())
        self.assertIn('"ScrollToEndOfDocument"', HOST.read_text())
        self.assertNotIn("scrollCommand", CONTENT.read_text())
        self.assertNotIn("xdotool", background)
        self.assertNotIn('addEventListener("keydown"', CONTENT.read_text())

    def test_background_routes_content_script_messages(self):
        background = CONTENT.with_name("background.js").read_text()
        self.assertIn("runtime.onMessage.addListener", background)
        self.assertIn("chrome.notifications.create", background)

    def test_background_routes_v10_execution_and_rejects_managed_window_events(self):
        script = f"""
const {{assert, fixture, execute}} = require({json.dumps(str(ROOT / 'scripts/browser_tab_groups_fixture.js'))});
const {{registerNativeEventRouter}} = require({json.dumps(str(CONTENT.with_name('background.js')))});
(async () => {{
    const f = fixture();
    await f.ready();
    let listener;
    const errors = [];
    registerNativeEventRouter({{onEvent: fn => {{ listener = fn; }}}}, f.executor,
        async () => {{}}, message => errors.push(message));
    const before = structuredClone(f.calls);
    for (const event of ['ensure_managed_page', 'new_managed_page']) {{
        listener({{service: 'abush_browser', event: `abush_browser.${{event}}`, payload: {{}}}});
    }}
    assert.equal(errors.length, 2);
    assert(errors.every(message => /protocol v10/.test(message)));
    assert.deepEqual(f.calls, before, 'retired events must not mutate browser or consent');
    const record = await f.enable();
    const payload = execute(f);
    await listener({{service: 'abush_browser', event: 'abush_browser.execute', payload}});
    assert.equal(f.tabs.get(record.tab_id).url, payload.command.url);
    assert.equal(f.tabs.get(1).url, 'https://ordinary.test/a');
    assert.equal(f.calls.filter(call => call.name === 'debugger.sendCommand').length, 1);
    assert.equal(f.calls.filter(call => call.name === 'windows.create').length, 0);
    assert.equal(f.executor.records.size, 1);
}})().catch(error => {{ console.error(error); process.exitCode = 1; }});
"""
        self.run_node(script)

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
    def test_native_dispatcher_correlates_out_of_order_responses(self):
        background = CONTENT.with_name("background.js")
        script = f"""
const {{NativeDispatcher}} = require({json.dumps(str(background))});
global.crypto = require('crypto').webcrypto;
class Signal {{
    constructor() {{ this.listeners = []; }}
    addListener(listener) {{ this.listeners.push(listener); }}
}}
const port = {{onMessage: new Signal(), onDisconnect: new Signal(), sent: [],
    postMessage(message) {{ this.sent.push(message); }}}};
const dispatcher = new NativeDispatcher(() => port, 1000);
const first = dispatcher.request('test', 'first', {{}});
const second = dispatcher.request('test', 'second', {{}});
port.onMessage.listeners[0]({{version: 1, reply_to: port.sent[1].id, ok: true,
    payload: {{value: 2}}}});
port.onMessage.listeners[0]({{version: 1, reply_to: port.sent[0].id, ok: true,
    payload: {{value: 1}}}});
Promise.all([first, second]).then(values => {{
    if (JSON.stringify(values) !== JSON.stringify([{{value: 1}}, {{value: 2}}])) process.exit(1);
    if (dispatcher.pending.size !== 0) process.exit(2);
}}).catch(() => process.exit(3));
"""
        self.run_node(script)

    def test_ordinary_file_staging_title_wait_stops_at_request_deadline(self):
        script = f"""
const {{assert, fixture}} = require({json.dumps(str(ROOT / 'scripts/browser_tab_groups_fixture.js'))});
const {{WAL_KEY}} = require({json.dumps(str(CONTENT.with_name('workspace.js')))});
(async () => {{
    const f = fixture();
    await f.ready();
    const originalNow = Date.now;
    let now = 10000;
    let reads = 0;
    let waitingReads = 0;
    const listeners = f.api.tabs.onUpdated.listeners.length;
    Date.now = () => now;
    f.hooks['tabs.get'] = id => {{
        reads++;
        if (f.api.tabs.onUpdated.listeners.length > listeners + 1) {{
            waitingReads++;
            now = 15000;
        }}
        return {{...f.tabs.get(id), title: 'Preparing ordinary Chrome window'}};
    }};
    const payload = {{version: 1, operation: 'ordinary_open', focus: true,
        request_id: 'title-deadline', url: 'file:///home/amos/ordinary-test.html', deadline_ms: now + 5000}};
    try {{
        assert.equal(await f.executor.ordinaryOpen(payload), 'unknown');
        assert.equal(now, payload.deadline_ms);
        assert.equal(waitingReads, 1, 'readiness observation must stop at the request deadline');
        assert(reads < 6, 'document readiness must not busy-poll the tab title');
        assert.equal(f.api.tabs.onUpdated.listeners.length, listeners);
        assert.match(f.executor.lastError, /deadline expired|staging document.*timed out/);
        const entry = f.local[WAL_KEY].ordinary[payload.request_id];
        assert.equal(entry.phase, 'terminal');
        assert.equal(entry.completion, 'unknown', 'a dispatched staging tab is not false success');
        assert.equal(f.executor.ordinaryOperations.size, 0);
        assert.equal(f.calls.filter(call => ['native.prepare', 'native.activate'].includes(call.name) ||
            call.name === 'tabs.update' && call.args[1].url).length, 0);
        const completions = f.calls.filter(call => call.name === 'native.ordinary_open_completion');
        assert.equal(completions.length, 1);
        assert.equal(completions[0].args[0].completion, 'unknown');
        assert.equal(f.executor.records.size, 0);
        assert.equal(f.tabs.size, 3, 'deadline must not remove the ordinary staging tab');
    }} finally {{ Date.now = originalNow; }}
}})().catch(error => {{ console.error(error); process.exitCode = 1; }});
"""
        self.run_node(script)

    def test_native_dispatcher_uses_receiver_safe_timers_in_a_service_worker(self):
        background = CONTENT.with_name("background.js")
        script = f"""
const assert = require('assert');
const {{NativeDispatcher}} = require({json.dumps(str(background))});
global.crypto = require('crypto').webcrypto;
const scheduled = [];
global.setTimeout = function(callback, delay) {{
    'use strict';
    if (this !== globalThis) throw new TypeError('Illegal invocation');
    scheduled.push([callback, delay]);
    return scheduled.length;
}};
global.clearTimeout = function(timer) {{
    'use strict';
    if (this !== globalThis) throw new TypeError('Illegal invocation');
    assert(Number.isInteger(timer));
}};
const port = {{
    onMessage: {{addListener() {{}}}},
    onDisconnect: {{addListener() {{}}}},
    postMessage() {{}},
}};
const dispatcher = new NativeDispatcher(() => port, 1000, 1000);
const bootstrap = dispatcher.bootstrap();
assert.equal(scheduled.length, 2,
    'bootstrap and request timers must be scheduled without illegal invocation');
bootstrap.catch(() => {{}});
dispatcher.stop();
"""
        self.run_node(script)

    def test_native_dispatcher_bounds_each_ordinary_native_phase(self):
        background = CONTENT.with_name("background.js")
        script = f"""
const assert = require('assert');
const {{NativeDispatcher}} = require({json.dumps(str(background))});
global.crypto = require('crypto').webcrypto;
const delays = [];
const port = {{
    onMessage: {{addListener() {{}}}},
    onDisconnect: {{addListener() {{}}}},
    postMessage() {{}},
}};
const dispatcher = new NativeDispatcher(() => port, 3000);
dispatcher.setTimeout = (_callback, delay) => {{ delays.push(delay); return delays.length; }};
dispatcher.clearTimeout = () => {{}};
const deadline_ms = Date.now() + 30000;
dispatcher.request('ordinary_open', 'prepare', {{deadline_ms}}).catch(() => {{}});
dispatcher.request('ordinary_open', 'ordinary_file_navigate', {{deadline_ms}}).catch(() => {{}});
dispatcher.request('ordinary_open', 'activate', {{deadline_ms}}).catch(() => {{}});
assert(delays.every(delay => delay > 0 && delay <= 8000),
    `ordinary phase timeout escaped its bound: ${{JSON.stringify(delays)}}`);
dispatcher.stop();
"""
        self.run_node(script)

    def test_native_dispatcher_rejects_all_once_on_disconnect(self):
        background = CONTENT.with_name("background.js")
        script = f"""
const {{NativeDispatcher}} = require({json.dumps(str(background))});
global.crypto = require('crypto').webcrypto;
class Signal {{
    constructor() {{ this.listeners = []; }}
    addListener(listener) {{ this.listeners.push(listener); }}
}}
const port = {{onMessage: new Signal(), onDisconnect: new Signal(),
    postMessage() {{}}}};
const dispatcher = new NativeDispatcher(() => port, 1000);
let rejected = 0;
const requests = [dispatcher.request('test', 'a', {{}}), dispatcher.request('test', 'b', {{}})]
    .map(request => request.catch(() => rejected++));
port.onDisconnect.listeners[0]();
port.onDisconnect.listeners[0]();
Promise.all(requests).then(() => {{
    if (rejected !== 2 || dispatcher.pending.size !== 0) process.exit(1);
}});
"""
        self.run_node(script)

    def test_native_dispatcher_reconnects_with_bounded_backoff(self):
        background = CONTENT.with_name("background.js")
        script = f"""
const assert = require('assert');
const {{NativeDispatcher}} = require({json.dumps(str(background))});
class Signal {{
    constructor() {{ this.listeners = []; }}
    addListener(listener) {{ this.listeners.push(listener); }}
}}
function port() {{ return {{onMessage: new Signal(), onDisconnect: new Signal(),
    postMessage() {{}}, disconnect() {{}}}}; }}
const ports = [port(), port()];
let connects = 0;
const timers = [];
const dispatcher = new NativeDispatcher(() => ports[connects++], 1000);
dispatcher.setTimeout = (callback, delay) => {{
    timers.push([callback, delay]);
    return timers.length;
}};
dispatcher.clearTimeout = () => {{}};
const first = dispatcher.ensurePort();
first.onDisconnect.listeners[0]();
first.onDisconnect.listeners[0]();
assert.equal(timers.length, 1, 'disconnect storms must schedule one reconnect');
assert.equal(timers[0][1], 100);
timers.shift()[0]();
assert.equal(connects, 2);
assert.strictEqual(dispatcher.port, ports[1]);
ports[1].onDisconnect.listeners[0]();
assert.equal(timers.length, 1);
assert.equal(timers[0][1], 200, 'backoff persists until a valid native message');
timers.shift();
dispatcher.reconnectTimer = null;
const third = port();
dispatcher.port = third;
third.onMessage.addListener(message => dispatcher.dispatch(message));
third.onDisconnect.addListener(() => {{
    dispatcher.port = null;
    dispatcher.scheduleReconnect();
}});
third.onMessage.listeners[0]({{
    version: 1, event: 'ready', service: 'native_host', payload: {{}},
}});
third.onDisconnect.listeners[0]();
assert.equal(timers[0][1], 100, 'a valid native message resets backoff');
dispatcher.stop();
"""
        self.run_node(script)

    def test_native_dispatcher_marks_pre_and_post_dispatch_failures(self):
        background = CONTENT.with_name("background.js")
        script = f"""
const assert = require('assert');
const {{NativeDispatcher}} = require({json.dumps(str(background))});
function signal() {{ return {{addListener() {{}}}}; }}
const unavailable = new NativeDispatcher(() => {{ throw new Error('connect failed'); }});
const notSent = unavailable.request('ordinary_open', 'ordinary_file_navigate', {{}})
    .catch(error => error.nativeDispatch);
const uncertain = new NativeDispatcher(() => ({{
    onMessage: signal(), onDisconnect: signal(),
    postMessage() {{ throw new Error('post failed'); }},
}}));
const unknown = uncertain.request('ordinary_open', 'ordinary_file_navigate', {{}})
    .catch(error => error.nativeDispatch);
Promise.all([notSent, unknown]).then(values => {{
    assert.deepEqual(values, ['not_sent', 'unknown']);
    unavailable.stop();
    uncertain.stop();
}}).catch(() => process.exit(1));
"""
        self.run_node(script)

    def test_native_dispatcher_stops_reconnecting_after_bound(self):
        background = CONTENT.with_name("background.js")
        script = f"""
const assert = require('assert');
const {{NativeDispatcher}} = require({json.dumps(str(background))});
const timers = [];
const events = [];
const dispatcher = new NativeDispatcher(() => {{ throw new Error('offline'); }}, 1000);
dispatcher.onEvent(message => events.push(message.event));
dispatcher.maxReconnectAttempts = 3;
dispatcher.setTimeout = (callback, delay) => {{
    timers.push([callback, delay]);
    return timers.length;
}};
for (let attempt = 0; attempt < 3; attempt++) {{
    try {{ dispatcher.ensurePort(); }} catch (_) {{}}
    assert.equal(timers.length, 1);
    timers.shift();
    dispatcher.reconnectTimer = null;
}}
assert.equal(dispatcher.reconnectAttempts, 3);
dispatcher.scheduleReconnect();
dispatcher.scheduleReconnect();
assert.equal(timers.length, 0, 'bounded reconnect must stop scheduling');
assert.deepEqual(events, ['reconnect_exhausted'], 'exhaustion emits one lifecycle notice');
"""
        self.run_node(script)


class NativeHostTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        spec = importlib.util.spec_from_file_location("ai_external_links_host", HOST)
        cls.host = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.host)

    def test_envelope_validation_and_correlation(self):
        request = {
            "version": 1,
            "id": "request-1",
            "service": "abush_browser",
            "operation": "list_remotes",
            "payload": {},
        }
        self.assertEqual(self.host.validate_request(request), request)
        self.assertEqual(
            self.host.response("request-1", payload={"value": 1}),
            {"version": 1, "reply_to": "request-1", "ok": True,
             "payload": {"value": 1}},
        )
        with self.assertRaises(self.host.EnvelopeError):
            self.host.validate_request({**request, "extra": True})

    def test_native_broker_rejects_removed_window_and_candidate_operations(self):
        broker = self.host.BrowserBroker(browser={"pid": 4242, "starttime": "123"})
        with mock.patch.object(broker, "x11_binding") as binding:
            for operation in ("bind_x11_window", "verify_x11_window", "clear_x11_window",
                              "register_candidate_lease", "release_candidate_lease",
                              "arm_candidate_lease", "admit_candidate", "finish_candidate_lease"):
                with self.subTest(operation=operation), self.assertRaisesRegex(ValueError, "unsupported"):
                    broker.handle_request({
                        "version": 1, "id": operation, "service": "abush_browser",
                        "operation": operation, "payload": {},
                    })
            binding.assert_not_called()

    def test_native_host_request_failures_do_not_stop_later_requests(self):
        class Broker:
            def handle_request(self, message):
                operation = message["operation"]
                if operation == "memory":
                    raise MemoryError("fatal")
                if operation != "ok":
                    raise RuntimeError(f"{operation} failed")
                return {"operation": operation}

            def close(self):
                pass

        native = self.host.NativeBrokerHost(
            input_stream=io.BytesIO(), output_stream=io.BytesIO(),
            ordinary_open_factory=None, browser={
                "pid": 1, "starttime": "1", "profile": pathlib.Path(),
                "browser_boot_id": "test-boot",
            })
        native.broker = Broker()
        for operation in ("bind_x11_window", "verify_x11_window", "clear_x11_window",
                          "prepare"):
            native._respond({
                "version": 1, "id": operation, "service": "test",
                "operation": operation, "payload": {},
            })
            failure = native.outbound.get_nowait()
            self.assertFalse(failure["ok"])
            self.assertIn("failed", failure["error"])
            native._respond({
                "version": 1, "id": f"{operation}-next", "service": "test",
                "operation": "ok", "payload": {},
            })
            success = native.outbound.get_nowait()
            self.assertTrue(success["ok"])
            self.assertEqual(success["payload"], {"operation": "ok"})
        with self.assertRaises(MemoryError):
            native._respond({
                "version": 1, "id": "fatal", "service": "test",
                "operation": "memory", "payload": {},
            })

    def test_main_external_links_never_reuse_shared_worktab_via_cdp(self):
        with (
            mock.patch.object(self.host, "reuse_tab", return_value=True) as reuse,
            mock.patch.object(self.host, "submit_ordinary_open", return_value="applied") as submit,
            mock.patch.object(self.host, "group_for_hostname", return_value="main"),
            mock.patch.dict(self.host.ROUTES["groups"], {"main": {"launcher": "chromium", "debug_port": 9222}}),
        ):
            self.assertEqual(self.host.handle_message({"url": "https://example.test/"}), {"ok": True})
            reuse.assert_not_called()
            submit.assert_called_once_with("https://example.test/")
        with mock.patch.object(self.host.urllib.request, "urlopen") as opener:
            self.assertFalse(self.host.reuse_tab("main", "example.test", "https://example.test/"))
            opener.assert_not_called()

    def test_external_link_open_waits_for_terminal_ordinary_completion(self):
        class Ordinary:
            def submit(self, url):
                self.url = url
                return "unknown"

        ordinary = Ordinary()
        broker = self.host.BrowserBroker(ordinary_open=ordinary)
        request = {
            "version": 1,
            "id": "external-open",
            "service": "external_links",
            "operation": "open",
            "payload": {"url": "https://example.test/", "sourceUrl": ""},
        }
        with (
            mock.patch.object(self.host, "reuse_tab", return_value=False),
            self.assertRaisesRegex(ValueError, "unknown"),
        ):
            broker.handle_request(request)
        self.assertEqual(ordinary.url, "https://example.test/")

    def test_authentication_urls_stay_in_the_calling_browser(self):
        with mock.patch.object(self.host.subprocess, "Popen") as popen:
            self.assertEqual(
                self.host.handle_message(
                    {"url": "https://accounts.google.com/o/oauth2/auth", "sourceGroup": "chat"}
                ),
                {"ok": True},
            )
        popen.assert_not_called()

    def test_routes_urls_to_their_own_browser(self):
        routes = {
            "https://chatgpt.com/c/1": "runai",
            "https://discord.com/channels/1": "runchat",
            "https://example.com/": "chromium",
        }
        with (
            mock.patch.object(self.host, "reuse_tab", return_value=False),
            mock.patch.object(self.host, "submit_ordinary_open", return_value="applied") as submit,
            mock.patch.object(self.host.subprocess, "Popen") as popen,
        ):
            for url, launcher in routes.items():
                with self.subTest(url=url):
                    self.assertEqual(self.host.handle_message({"url": url}), {"ok": True})
                    if launcher == "chromium":
                        submit.assert_called_with(url)
                    else:
                        popen.assert_called_with(
                            [str(pathlib.Path.home() / "scripts" / launcher), url])

    def test_reuses_existing_app_tab_before_launching(self):
        with (
            mock.patch.object(self.host, "reuse_tab", return_value=True) as reuse_tab,
            mock.patch.object(self.host.subprocess, "Popen") as popen,
        ):
            self.assertEqual(
                self.host.handle_message({"url": "https://app.slack.com/client/T1/C1"}),
                {"ok": True},
            )
        reuse_tab.assert_called_once_with(
            "chat", "app.slack.com", "https://app.slack.com/client/T1/C1")
        popen.assert_not_called()

    def test_scroll_command_uses_cdp(self):
        with mock.patch.object(self.host, "scroll_page", return_value=True) as scroll_page:
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

    def test_invalid_scroll_command_fails(self):
        with mock.patch.object(self.host, "scroll_page", return_value=False) as scroll_page:
            self.assertEqual(
                self.host.handle_message({"command": "scroll", "direction": "invalid"}),
                {"ok": False},
            )
        scroll_page.assert_called_once_with("invalid", url=None)

    def test_rejects_non_http_url(self):
        with mock.patch.object(self.host.subprocess, "run") as run:
            self.assertEqual(
                self.host.handle_message({"url": "file:///etc/passwd"}), {"ok": False})
        run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
