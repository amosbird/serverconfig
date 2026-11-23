#!/usr/bin/env node
const {assert, clone, deferred, fixture, BOOT, runCases} =
    require("./browser_tab_groups_fixture.js");
const {NativeDispatcher, createWorkspaceInitializer, registerNativeEventRouter} =
    require("../chromium/external-links/extension/background.js");
const {AUTOMATION_KEY} = require("../chromium/external-links/extension/workspace.js");
const CAPS = {cancellation: true, owned_tab_group: true, official_engine: true, typed_output: true};
function nativeFixture() {
    const ports = [];
    const timers = new Map();
    let nextTimer = 0;
    const native = new NativeDispatcher(() => {
        const sent = [];
        const messages = [];
        const disconnects = [];
        const waiters = new Map();
        const port = {sent, onMessage: {addListener: fn => messages.push(fn)},
            onDisconnect: {addListener: fn => disconnects.push(fn)},
            postMessage(message) {
                sent.push(clone(message));
                const waiter = waiters.get(message.operation);
                if (waiter) { waiters.delete(message.operation); waiter.resolve(message); }
            },
            next(operation) {
                const existing = sent.find(message => message.operation === operation);
                if (existing) return Promise.resolve(existing);
                const waiter = deferred(); waiters.set(operation, waiter); return waiter.promise;
            },
            reply(message, payload, extra = {}) {
                for (const listener of messages) listener({version: 1, reply_to: message.id,
                    ok: true, payload: clone(payload), ...extra});
            },
            bootstrap(payload = BOOT) {
                for (const listener of messages) listener({version: 1, service: "native_host",
                    event: "bootstrap", payload: clone(payload)});
            },
            disconnect() { for (const listener of disconnects) listener(); },
        };
        ports.push(port); return port;
    });
    native.setTimeout = (fn, delay) => { const id = ++nextTimer; timers.set(id, {fn, delay}); return id; };
    native.clearTimeout = id => timers.delete(id);
    const reconnect = () => {
        const timer = native.reconnectTimer;
        assert.ok(timers.has(timer));
        const fn = timers.get(timer).fn; timers.delete(timer); fn();
        return ports.at(-1);
    };
    async function admit(port, primary = true) {
        port.bootstrap();
        const prepare = await port.next("browser_executor_prepare");
        assert.equal(prepare.service, "native_host");
        assert.deepEqual(prepare.payload, {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10});
        const connection = `native-${ports.indexOf(port)}`;
        port.reply(prepare, {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10,
            native_connection_id: connection, primary_profile: primary});
        if (!primary) return;
        const ready = await port.next("browser_executor_ready");
        assert.equal(ready.service, "native_host");
        assert.deepEqual(ready.payload, {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10,
            native_connection_id: connection, capabilities: CAPS});
        port.reply(ready, {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10,
            native_connection_id: connection, ready: true});
    }
    return {native, ports, timers, reconnect, admit};
}
const cases = [
    ["ordinary unsolicited bootstrap is identity only", async () => {
        const f = nativeFixture(); const bootstrap = f.native.bootstrap();
        const port = f.ports[0]; port.bootstrap(); await bootstrap;
        assert.deepEqual(port.sent.map(m => m.operation), ["bootstrap"]);
        const request = f.native.request("external_links", "scroll", {direction: "scrollToTop"});
        port.reply(await port.next("scroll"), {ok: true}); await request;
        assert.equal(port.sent.some(m => m.operation.startsWith("browser_executor_")), false);
        f.native.stop();
    }],
    ["overlapping admission deduplicates per bootstrap attempt", async () => {
        const f = nativeFixture(); const first = f.native.browserReady();
        assert.equal(f.native.browserReady(), first);
        await f.admit(f.ports[0]); assert.equal((await first).ready, true);
        assert.equal(f.native.browserReady(), first);
        assert.deepEqual(f.ports[0].sent.map(m => m.operation),
            ["bootstrap", "browser_executor_prepare", "browser_executor_ready"]);
        f.native.stop();
    }],
    ["cached initializer reconnect gets fresh same-boot native challenge", async () => {
        const f = nativeFixture(); const workspace = fixture(); const errors = [];
        const ready = createWorkspaceInitializer(workspace.executor, f.native, e => errors.push(e));
        const first = ready(); await f.admit(f.ports[0]); await first;
        const old = f.ports[0]; old.disconnect(); const port = f.reconnect();
        assert.equal(ready(), first);
        await f.admit(port); const result = await f.native.browserReady();
        assert.equal(result.native_connection_id, "native-1");
        assert.equal(workspace.executor.popupState().browser_executor.phase, "ready");
        assert.deepEqual(errors, []); assert.equal(workspace.local[AUTOMATION_KEY], false);
        assert.equal(workspace.executor.automationOn, false);
        f.native.stop();
    }],
    ["worker reconstruction performs admission on its own port", async () => {
        for (let i = 0; i < 2; i++) {
            const f = nativeFixture(); const workspace = fixture();
            const ready = createWorkspaceInitializer(workspace.executor, f.native, () => {});
            const initializing = ready(); await f.admit(f.ports[0]); await initializing;
            assert.equal(f.ports[0].sent.filter(m => m.operation === "browser_executor_ready").length, 1);
            f.native.stop();
        }
    }],
    ["rebuilt initializer observes already committed independent readiness status", async () => {
        const f = nativeFixture(); const admission = f.native.browserReady();
        await f.admit(f.ports[0]); await admission;
        const workspace = fixture();
        await createWorkspaceInitializer(workspace.executor, f.native, () => {})();
        assert.equal(workspace.executor.popupState().browser_executor.phase, "ready");
        assert.equal(f.ports[0].sent.filter(m => m.operation === "browser_executor_ready").length, 1);
        f.native.stop();
    }],
    ["new boot on the same port invalidates old pending preparation", async () => {
        const f = nativeFixture(); const request = f.native.browserReady(); request.catch(() => {});
        const port = f.ports[0]; port.bootstrap();
        const prepare = await port.next("browser_executor_prepare");
        port.bootstrap({...BOOT, browser_boot_id: "new-boot"});
        port.reply(prepare, {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10,
            native_connection_id: "old", primary_profile: true});
        await assert.rejects(request, /superseded/);
        assert.equal(port.sent.some(m => m.operation === "browser_executor_ready"), false);
        const fresh = port.sent.filter(m => m.operation === "browser_executor_prepare").at(-1);
        assert.equal(fresh.payload.browser_boot_id, "new-boot"); f.native.stop();
    }],
    ...["bootstrap", "prepare", "ready"].map(stage => [
        `old ${stage} completion cannot declare readiness on replacement port`, async () => {
            const f = nativeFixture(); const oldAttempt = f.native.browserReady();
            oldAttempt.catch(() => {}); const old = f.ports[0];
            let pending;
            if (stage !== "bootstrap") {
                old.bootstrap(); pending = await old.next("browser_executor_prepare");
                if (stage === "ready") {
                    old.reply(pending, {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10,
                        native_connection_id: "old", primary_profile: true});
                    pending = await old.next("browser_executor_ready");
                }
            }
            old.disconnect(); const port = f.reconnect();
            if (stage === "bootstrap") old.bootstrap();
            else old.reply(pending, stage === "prepare" ? {browser_boot_id: BOOT.browser_boot_id,
                protocol_version: 10, native_connection_id: "old", primary_profile: true} :
                {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10,
                    native_connection_id: "old", ready: true});
            await assert.rejects(oldAttempt, /disconnected|superseded/);
            assert.deepEqual(port.sent.map(m => m.operation), ["bootstrap"]);
            await f.admit(port); assert.equal((await f.native.browserReady()).native_connection_id, "native-1");
            f.native.stop();
        }]),
    ["resolved old bootstrap continuation cannot prepare on a new port", async () => {
        const f = nativeFixture(); const request = f.native.browserReady(); request.catch(() => {});
        const old = f.ports[0]; old.bootstrap(); old.disconnect(); const port = f.reconnect();
        await assert.rejects(request, /superseded/);
        assert.deepEqual(old.sent.map(m => m.operation), ["bootstrap"]);
        assert.deepEqual(port.sent.map(m => m.operation), ["bootstrap"]);
        await f.admit(port); await f.native.browserReady(); f.native.stop();
    }],
    ["pinned internal dispatch never connects or redirects an old port", async () => {
        const f = nativeFixture(); const bootstrap = f.native.bootstrap();
        const port = f.ports[0]; port.bootstrap(); await bootstrap; port.disconnect();
        await assert.rejects(f.native.dispatchRequest("native_host", "browser_executor_prepare",
            {browser_boot_id: BOOT.browser_boot_id, protocol_version: 10}, port), /superseded/);
        assert.equal(f.ports.length, 1); assert.equal(port.sent.length, 1);
        f.native.stop();
    }],
    ...[true, false].map(on => [`secondary readiness preserves ordinary routing and stored ${on}`, async () => {
        const f = nativeFixture(); const workspace = fixture({globalAutomation: true});
        workspace.local[AUTOMATION_KEY] = on;
        const errors = [];
        const ready = createWorkspaceInitializer(workspace.executor, f.native, e => errors.push(e));
        registerNativeEventRouter(f.native, workspace.executor, ready, e => errors.push(e));
        const initializing = ready(); await f.admit(f.ports[0], false); await initializing;
        assert.deepEqual(await f.native.browserReady(), {ready: false, primary_profile: false});
        assert.equal(f.ports[0].sent.some(m => m.operation === "browser_executor_ready"), false);
        assert.equal(workspace.executor.popupState().browser_executor.phase, "secondary");
        assert.match(workspace.executor.popupState().error, /primary browser profile/);
        for (let i = 0; i < 2; i++) {
            const request = f.native.request("external_links", "open", {url: "https://ordinary.test/"});
            const message = f.ports[0].sent.at(-1); f.ports[0].reply(message, {ok: true}); await request;
            await ready();
        }
        assert.equal(errors.length, 1); assert.equal(workspace.executor.restored, true);
        assert.equal(workspace.executor.automationOn, on); assert.equal(workspace.local[AUTOMATION_KEY], on);
        f.native.stop();
    }]),
    ["unavailable admission is advisory, not ordinary initialization failure", async () => {
        const f = nativeFixture(); const workspace = fixture(); const errors = [];
        const ready = createWorkspaceInitializer(workspace.executor, f.native, e => errors.push(e));
        const initialization = ready(); f.ports[0].bootstrap();
        const prepare = await f.ports[0].next("browser_executor_prepare");
        f.ports[0].reply(prepare, undefined, {ok: false, error: "unsupported operation"});
        await initialization;
        assert.equal(workspace.executor.restored, true); assert.equal(errors.length, 1);
        assert.equal(workspace.executor.popupState().browser_executor.phase, "unavailable");
        f.native.stop();
    }],
    ["identity-less legacy bootstrap cannot prepare admission", async () => {
        const f = nativeFixture(); const request = f.native.browserReady(); f.ports[0].bootstrap({});
        await assert.rejects(request, /boot is unavailable/);
        assert.deepEqual(f.ports[0].sent.map(m => m.operation), ["bootstrap"]); f.native.stop();
    }],
    ...[9, undefined].map(version => [`invalid native envelope ${version} cannot admit`, async () => {
        const f = nativeFixture(); const request = f.native.browserReady(); f.ports[0].bootstrap();
        const prepare = await f.ports[0].next("browser_executor_prepare");
        f.ports[0].reply(prepare, {browser_boot_id: BOOT.browser_boot_id, native_connection_id: "native",
            protocol_version: 10, primary_profile: true}, {version});
        await assert.rejects(request, /mismatched native response version/);
        assert.equal(f.ports[0].sent.some(m => m.operation === "browser_executor_ready"), false);
        f.native.stop();
    }]),
    ["backend capability rejection stays closed and never changes OFF", async () => {
        const f = nativeFixture(); const workspace = fixture(); const errors = [];
        const ready = createWorkspaceInitializer(workspace.executor, f.native, e => errors.push(e));
        const initialization = ready(); f.ports[0].bootstrap();
        const prepare = await f.ports[0].next("browser_executor_prepare");
        f.ports[0].reply(prepare, {browser_boot_id: BOOT.browser_boot_id, native_connection_id: "native",
            protocol_version: 10, primary_profile: true});
        const declaration = await f.ports[0].next("browser_executor_ready");
        assert.deepEqual(declaration.payload.capabilities, CAPS);
        f.ports[0].reply(declaration, undefined, {ok: false, error: "executor capabilities rejected"});
        await initialization; await assert.rejects(f.native.browserReady(), /capabilities rejected/);
        assert.equal(workspace.executor.popupState().browser_executor.phase, "unavailable");
        assert.equal(workspace.executor.automationOn, false); assert.equal(workspace.local[AUTOMATION_KEY], false);
        assert.equal(errors.length, 1); f.native.stop();
    }],
    ["bounded prepare timeout fails closed without retrying admission or effects", async () => {
        const f = nativeFixture(); const request = f.native.browserReady(); f.ports[0].bootstrap();
        await f.ports[0].next("browser_executor_prepare");
        const timer = [...f.timers.values()].find(t => t.delay === 8000); assert.ok(timer); timer.fn();
        await assert.rejects(request, /timed out/);
        await assert.rejects(f.native.browserReady(), /timed out/);
        assert.deepEqual(f.ports[0].sent.map(m => m.operation), ["bootstrap", "browser_executor_prepare"]);
        f.native.stop();
    }],
];
for (const stage of ["prepare", "ready"]) {
    const valid = stage === "prepare" ? {browser_boot_id: BOOT.browser_boot_id,
        native_connection_id: "native", protocol_version: 10, primary_profile: true} :
        {browser_boot_id: BOOT.browser_boot_id, native_connection_id: "native", protocol_version: 10, ready: true};
    const variants = [null, {}, {...valid, protocol_version: 9}, {...valid, protocol_version: "10"},
        {...valid, browser_boot_id: "foreign"}, {...valid, native_connection_id: ""},
        {...valid, extra: true}, {...valid, [stage === "prepare" ? "primary_profile" : "ready"]: 1},
        {...valid, [stage === "prepare" ? "primary_profile" : "ready"]: "true"}];
    if (stage === "ready") variants.push({...valid, native_connection_id: "foreign"}, {...valid, ready: false});
    variants.forEach((payload, index) => cases.push([`malformed ${stage} receipt ${index} is denied`, async () => {
        const f = nativeFixture(); const request = f.native.browserReady(); f.ports[0].bootstrap();
        const prepare = await f.ports[0].next("browser_executor_prepare");
        let message = prepare;
        if (stage === "ready") {
            f.ports[0].reply(prepare, {browser_boot_id: BOOT.browser_boot_id, native_connection_id: "native",
                protocol_version: 10, primary_profile: true});
            message = await f.ports[0].next("browser_executor_ready");
            assert.deepEqual(message.payload.capabilities, CAPS);
        }
        f.ports[0].reply(message, payload); await assert.rejects(request, /Invalid native/);
        if (stage === "prepare") assert.equal(f.ports[0].sent.some(m => m.operation === "browser_executor_ready"), false);
        f.native.stop();
    }]));
}
runCases(cases);
