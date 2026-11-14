#!/usr/bin/env node
// Offline exact-tab lifecycle gate; no display, host processes, or deployment required.
const {assert, clone, deferred, fixture, execute, runCases, BOOT, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const {WAL_KEY, CONSENT_KEY} = require("../chromium/external-links/extension/workspace.js");
const {NativeDispatcher, registerNativeEventRouter} =
    require("../chromium/external-links/extension/background.js");
const cases = [
    ["same boot worker restore revalidates exact tab before agreement", async () => {
        const f = fixture(); await f.ready(); const original = clone(await f.enable());
        const created = f.calls.filter(call => call.name === "tabs.create").length;
        await f.restart();
        const restored = f.executor.records.get("remote");
        assert.equal(restored.tab_id, original.tab_id); assert.equal(restored.enable_id, original.enable_id);
        assert.equal(restored.agreed, false);
        const reconnect = {...REMOTE, connection_id: "new-transport"};
        await f.executor.remoteState(reconnect);
        assert.equal(restored.agreed, true);
        await f.executor.execute(execute(f, {connection_id: "new-transport"}));
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, created);
        assert.deepEqual(f.local.abushOwnedMarkers, {legacy: true});
    }],
    ["browser restart reused IDs never adopted", async () => {
        const f = fixture(); await f.ready(); const record = await f.enable();
        const snapshot = clone([...f.tabs.values()]);
        await f.restart({...BOOT, browser_boot_id: "boot-two"});
        await f.executor.remoteState(REMOTE);
        assert.equal(f.executor.records.size, 0);
        assert.equal(f.executor.state("remote").phase, "disabled");
        assert.deepEqual([...f.tabs.values()], snapshot);
        assert.equal(f.executor.ownsTab(record.tab_id), false);
    }],
    ["same boot paused survives worker and native reconnect", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        await f.executor.control("pause", "remote");
        await f.restart(); await f.executor.remoteState(REMOTE);
        assert.equal(f.executor.state("remote").phase, "paused");
        await f.executor.invalidateRemote();
        await f.executor.remoteState({...REMOTE, connection_id: "new"});
        assert.equal(f.executor.state("remote").phase, "paused");
    }],
    ["daemon boot or fence change requires explicit resume", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        const payload = execute(f);
        const newer = {...REMOTE, executor_fence: {...REMOTE.executor_fence, epoch: 2},
            owner_display: {...REMOTE.owner_display, daemon_boot_id: "boot-new"}};
        await f.executor.remoteState(newer);
        assert.equal(f.executor.state("remote").phase, "paused");
        await assert.rejects(f.executor.execute(payload), /paused/);
        await f.executor.control("resume", "remote");
        assert.equal(f.executor.state("remote").phase, "enabled");
    }],
    ["old fence continuation cannot navigate after remote transition", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        const gate = deferred(); const entered = deferred();
        f.hooks["debugger.attach"] = async () => { entered.resolve(); await gate.promise; };
        const payload = execute(f); const pending = f.executor.execute(payload); pending.catch(() => {});
        await entered.promise;
        await f.executor.remoteState({...REMOTE, executor_fence: {executor_id: "executor", epoch: 2}});
        gate.resolve(); await assert.rejects(pending);
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 0);
        assert.equal(f.executor.state("remote").phase, "paused");
    }],
    ["close move detach replace revoke synchronously without recreate", async () => {
        for (const name of ["onRemoved", "onMoved", "onDetached", "onReplaced"]) {
            const f = fixture(); await f.ready(); const record = await f.enable(); f.listeners();
            const before = f.tabs.size;
            const jobs = name === "onReplaced" ? f.api.tabs[name].emit(99, record.tab_id) :
                f.api.tabs[name].emit(record.tab_id, {windowId: 1});
            assert.equal(f.executor.state("remote").phase, "disabled");
            await Promise.all(jobs);
            assert.equal(f.tabs.size, before);
            assert.equal(f.executor.ownsTab(record.tab_id), false);
        }
    }],
    ["unknown create is durable and never blindly retried", async () => {
        const f = fixture(); await f.ready();
        f.hooks["tabs.create"] = async () => { throw new Error("creation interrupted"); };
        const first = await f.send({control: "enable", remote_key: "remote"});
        assert.equal(first.ok, false); assert.match(first.error, /no tabs were removed/);
        delete f.hooks["tabs.create"];
        await f.restart(); await f.executor.remoteState(REMOTE);
        const second = await f.send({control: "enable", remote_key: "remote"});
        assert.equal(second.ok, false); assert.match(second.error, /Interrupted tab creation/);
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, 1);
        assert.equal(f.tabs.size, 2);
    }],
    ["restored dispatched effect reports unknown only", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        const key = f.executor.effectKey(payload);
        f.local[WAL_KEY].effects[key] = {version: 9, payload, phase: "dispatched", completion: null};
        await f.restart(); await f.executor.remoteState(REMOTE);
        assert.equal(f.executor.effects[key].completion, "unknown");
        await f.executor.execute(payload);
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 0);
    }],
    ["terminal storage failure blocks ACK and duplicate rerun", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        f.hooks["local.set"] = async value => {
            const entry = value[WAL_KEY]?.effects[f.executor.effectKey(payload)];
            if (entry?.completion) throw new Error("terminal disk failed");
        };
        await assert.rejects(f.executor.execute(payload), /terminal disk failed/);
        assert.equal(f.calls.filter(call => call.name === "native.executor_completion").length, 0);
        delete f.hooks["local.set"];
        await f.executor.execute(payload);
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 1);
    }],
    ["invalid state receipt never agrees", async () => {
        const f = fixture(); await f.ready();
        f.hooks["native.workspace_state"] = async () => { throw new Error("state rejected"); };
        assert.equal((await f.send({control: "enable", remote_key: "remote"})).ok, false);
        assert.equal(f.executor.records.get("remote").agreed, false);
        assert.equal(f.executor.state("remote").phase, "paused");
    }],
    ["cancel registers before effect and cannot report applied", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        const gate = deferred(); const entered = deferred();
        f.hooks["debugger.attach"] = async () => { entered.resolve(); await gate.promise; };
        const pending = f.executor.execute(payload); pending.catch(() => {}); await entered.promise;
        const cancel = f.executor.cancel({remote_key: "remote", connection_id: "connection",
            principal_id: "principal", request_id: "cancel", execution_request_id: payload.request_id,
            effect_id: payload.effect_id, executor_fence: payload.executor_fence});
        assert.equal(f.executor.operations.get(f.executor.effectKey(payload)).cancelled, true);
        gate.resolve(); await assert.rejects(pending); assert.equal(await cancel, "not_applied");
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 0);
    }],
    ["ordinary expired lease does not create and cold lease creates only ordinary", async () => {
        const f = fixture(); await f.ready();
        assert.equal(await f.executor.ordinaryOpen({version: 1, operation: "ordinary_open", focus: true, request_id: "expired",
            url: "https://expired.test/", deadline_ms: Date.now() - 1}), "not_applied");
        assert.equal(f.tabs.size, 2);
        f.windows.delete(1); f.tabs.delete(1);
        const lease = {version: 1, operation: "ordinary_open", focus: true, request_id: "cold",
            url: "https://cold.test/exact", deadline_ms: Date.now() + 5000};
        await f.restart({...BOOT, ordinary_launch_leases: [lease]});
        assert.equal(f.executor.records.size, 0);
        assert.equal(f.calls.filter(call => call.name === "windows.create").length, 1);
        assert.equal([...f.tabs.values()].find(tab => tab.url === lease.url).groupId, -1);
        assert.equal(f.session[CONSENT_KEY].records.length, 0);
    }],
    ["native router refuses retired protocol events", async () => {
        const f = fixture(); await f.ready(); const errors = [];
        let listener;
        registerNativeEventRouter({onEvent: fn => { listener = fn; }}, f.executor,
            async () => {}, message => errors.push(message));
        listener({service: "abush_browser", event: "abush_browser.ensure_managed_page", payload: {}});
        assert.match(errors[0], /protocol v9/); assert.equal(f.tabs.size, 2);
    }],
    ["disconnect followed by peer refresh stays disabled", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        await f.executor.control("disconnect", "remote");
        await f.executor.remoteState(REMOTE);
        assert.equal(f.executor.state("remote").phase, "disabled");
        assert.equal(f.executor.records.get("remote").agreed, false);
    }],
    ["group collapse is presentation only", async () => {
        const f = fixture(); await f.ready(); const record = await f.enable();
        await f.api.tabGroups.update(record.group_id, {collapsed: true});
        await f.executor.execute(execute(f));
        assert.equal(f.executor.state("remote").phase, "enabled");
        assert.equal(f.tabs.get(record.tab_id).active, false);
    }],
    ["pause racing initial enable persistence cannot auto resume", async () => {
        const f = fixture(); await f.ready(); const gate = deferred(); const entered = deferred();
        let blocked = false;
        f.hooks["session.set"] = async value => {
            if (!blocked && value[CONSENT_KEY]?.records.length) {
                blocked = true; entered.resolve(); await gate.promise;
            }
        };
        const enable = f.send({control: "enable", remote_key: "remote"}); await entered.promise;
        const pause = f.executor.control("pause", "remote");
        assert.equal(f.executor.state("remote").phase, "paused");
        gate.resolve(); assert.equal((await enable).ok, false); await pause;
        assert.equal(f.executor.state("remote").phase, "paused");
        assert.equal(f.executor.records.get("remote").agreed, false);
    }],
    ["exact Chrome API waits are bounded", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        f.hooks["debugger.sendCommand"] = async () => new Promise(() => {});
        const payload = execute(f);
        await assert.rejects(f.executor.execute(payload), /timed out/);
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "unknown");
        assert.equal(f.executor.operations.size, 0);
    }],
    ["native dispatcher not sent versus uncertain transport", async () => {
        const unavailable = new NativeDispatcher(() => { throw new Error("unavailable"); }, 25);
        await assert.rejects(unavailable.request("test", "test"), error => error.nativeDispatch === "not_sent");
        unavailable.stop();
        const listeners = {};
        const native = new NativeDispatcher(() => ({onMessage: {addListener: fn => { listeners.message = fn; }},
            onDisconnect: {addListener: fn => { listeners.disconnect = fn; }},
            postMessage: () => {}, disconnect: () => {}}), 25);
        await assert.rejects(native.request("test", "test"), error => error.nativeDispatch === "unknown");
        native.stop();
    }],
];
if (require.main === module) runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
module.exports = {cases};
