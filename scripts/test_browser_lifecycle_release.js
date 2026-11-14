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
        f.hooks["debugger.sendCommand"] = async () => new Promise(resolve => {
            const timer = setTimeout(resolve, 2100); timer.unref?.();
        });
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
    ["startup controls continuously fence consent read before install", async () => {
        for (const action of ["pause", "disconnect"]) {
            const f = fixture(); await f.ready(); const old = clone(await f.enable());
            const entered = deferred(); const gate = deferred();
            f.hooks["session.get.after"] = async () => { entered.resolve(); await gate.promise; };
            const worker = f.resetWorker(); await worker.acceptNativeBootstrap(BOOT);
            const restore = worker.restoreWhenReady(); await entered.promise;
            const control = f.send({control: action, remote_key: "remote"});
            await worker.sessionTail;
            assert.equal(worker.records.size, 0);
            const revokedRevision = worker.state("remote").revision;
            gate.resolve(); await restore; await control;
            assert.equal(worker.records.size, 0);
            assert.ok(worker.state("remote").revision > old.revision);
            assert.ok(worker.state("remote").revision >= revokedRevision);
            await worker.remoteState(REMOTE);
            assert.equal(worker.state("remote").phase, "disabled");
            assert.equal(worker.ownsTab(old.tab_id), false);
        }
    }],
    ["create callback revocation prevents group and authorization", async () => {
        for (const action of ["pause", "disconnect", "move"]) {
            const f = fixture(); await f.ready(); f.listeners();
            const entered = deferred(); const gate = deferred(); let created;
            f.hooks["tabs.create.after"] = async tab => { created = tab; entered.resolve(); await gate.promise; };
            const enable = f.send({control: "enable", remote_key: "remote"}); await entered.promise;
            if (action === "move") await Promise.all(f.api.tabs.onMoved.emit(created.id, {windowId: 1}));
            else await f.executor.control(action, "remote");
            gate.resolve(); assert.equal((await enable).ok, false);
            assert.equal(f.calls.filter(c => ["tabs.group", "tabGroups.update"].includes(c.name)).length, 0);
            assert.equal(f.executor.ownsTab(created.id), false);
            assert.equal(f.tabs.size, 3);
        }
    }],
    ["unknown create Disconnect releases only journal and fences late callback", async () => {
        const f = fixture(); await f.ready(); const gate = deferred(); const entered = deferred(); let old;
        f.hooks["tabs.create.after"] = async tab => { old = tab; entered.resolve(); await gate.promise; };
        const enable = f.send({control: "enable", remote_key: "remote"}); await entered.promise;
        assert.equal((await enable).ok, false);
        await f.executor.control("disconnect", "remote"); delete f.hooks["tabs.create.after"];
        const fresh = await f.enable(); assert.notEqual(fresh.tab_id, old.id);
        gate.resolve(); await f.executor.localTail;
        assert.equal(f.executor.ownsTab(old.id), false); assert.equal(f.tabs.get(old.id).groupId, -1);
        assert.equal(f.calls.filter(c => c.name === "tabs.group").length, 1);
    }],
    ["cancellation ACK loss survives restart and new transport without effects", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        await f.executor.execute(payload);
        const cancel = {remote_key: "remote", connection_id: "connection", principal_id: "principal",
            request_id: "cancel-durable", execution_request_id: payload.request_id,
            effect_id: payload.effect_id, executor_fence: payload.executor_fence};
        f.hooks["native.cancellation_completion"] = async () => { throw new Error("ACK lost"); };
        await assert.rejects(f.executor.cancel(cancel), /ACK lost/);
        delete f.hooks["native.cancellation_completion"];
        await f.restart(); await f.executor.remoteState({...REMOTE, connection_id: "retry-transport"});
        const deliveries = f.calls.filter(c => c.name === "native.cancellation_completion");
        assert.equal(deliveries.length, 2);
        assert.equal(deliveries[1].args[0].connection_id, "retry-transport");
        assert.equal(deliveries[1].args[0].request_id, cancel.request_id);
        assert.equal(await f.executor.cancel({...cancel, connection_id: "retry-transport"}), "unknown");
        assert.equal(f.calls.filter(c => c.name === "native.cancellation_completion").length, 2);
        assert.equal(f.calls.filter(c => c.name === "debugger.sendCommand").length, 1);
        await assert.rejects(f.executor.cancel({...cancel, connection_id: "stale"}), /stale/);
    }],
    ["content router ownership revisions survive lease remove and delayed reads", async () => {
        const vm = require("node:vm"); const fs = require("node:fs");
        const f = fixture(); await f.ready(); const record = await f.enable();
        const responses = []; const notifications = [];
        const context = vm.createContext({URL, Date, Promise, Number, console,
            ROUTES: {groups: {}, authSites: []}, chrome: {runtime: {
                sendMessage: (message, cb) => { responses.push({message, cb}); },
                onMessage: {addListener: fn => notifications.push(fn)},
            }}, document: {addEventListener: () => {}}, location: {href: "https://ordinary.test/"}});
        vm.runInContext(fs.readFileSync(require.resolve("../chromium/external-links/extension/content.js"), "utf8"), context);
        assert.equal(responses[0].message.query, "abush_owned_tab");
        const sender = {...f.trusted, tab: {id: record.tab_id, windowId: 1}};
        const owned = await f.send(responses[0].message, sender);
        assert.ok(owned.generation > 0); responses[0].cb(owned);
        const delayed = vm.runInContext("refreshOwnership({force: true})", context);
        await f.executor.control("disconnect", "remote");
        const released = await f.send({query: "abush_owned_tab"}, sender);
        assert.ok(released.generation > owned.generation);
        notifications[0]({type: "abush_ownership_changed", owned: false, generation: released.generation});
        responses[1].cb(owned); await delayed;
        assert.equal(vm.runInContext("ownedWorkspace", context), false);
        const fresh = await f.enable();
        const renewed = await f.send({query: "abush_owned_tab"}, {...sender, tab: {id: fresh.tab_id}});
        notifications[0]({type: "abush_ownership_changed", owned: true, generation: renewed.generation});
        assert.equal(vm.runInContext("ownedWorkspace", context), true);
        await f.executor.remoteState({...REMOTE, connection_id: "reconnected"});
        assert.ok((await f.send({query: "abush_owned_tab"}, sender)).generation >= renewed.generation);
    }],
    ["startup revocation before restore starts never reinstalls consent", async () => {
        for (const action of ["pause", "disconnect"]) {
            const f = fixture(); await f.ready(); const old = clone(await f.enable());
            const worker = f.resetWorker();
            const saved = clone(f.session[CONSENT_KEY]);
            await worker.control(action, "remote");
            f.session[CONSENT_KEY] = saved;
            await worker.acceptNativeBootstrap(BOOT); await worker.restoreWhenReady();
            assert.equal(worker.records.size, 0);
            assert.ok(worker.state("remote").revision > old.revision);
        }
    }],
    ["group callback revocation prevents label without deleting partial group", async () => {
        for (const action of ["pause", "disconnect", "move"]) {
            const f = fixture(); await f.ready(); f.listeners();
            const entered = deferred(); const gate = deferred(); let tabId;
            f.hooks["tabs.group.after"] = async (group, args) => {
                tabId = args.tabIds[0]; entered.resolve(); await gate.promise;
            };
            const enable = f.send({control: "enable", remote_key: "remote"}); await entered.promise;
            if (action === "move") await Promise.all(f.api.tabs.onMoved.emit(tabId));
            else await f.executor.control(action, "remote");
            gate.resolve(); assert.equal((await enable).ok, false);
            assert.equal(f.calls.filter(c => c.name === "tabGroups.update").length, 0);
            assert.equal(f.executor.ownsTab(tabId), false); assert.notEqual(f.tabs.get(tabId).groupId, -1);
        }
    }],
    ["native disconnect during restore cannot authorize on reconnect", async () => {
        const f = fixture(); await f.ready(); const original = clone(await f.enable());
        const entered = deferred(); const gate = deferred();
        f.hooks["session.get.after"] = async () => { entered.resolve(); await gate.promise; };
        const worker = f.resetWorker(); await worker.acceptNativeBootstrap(BOOT);
        const restore = worker.restoreWhenReady(); await entered.promise;
        await worker.invalidateRemote(); gate.resolve(); await restore;
        await worker.remoteState(REMOTE);
        assert.equal(worker.records.size, 0); assert.ok(worker.state("remote").revision > original.revision);
    }],
    ["native disconnect keeps enabled episode paused through reconnect", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        await f.executor.invalidateRemote();
        assert.equal(f.executor.state("remote").phase, "paused");
        await f.executor.remoteState({...REMOTE, connection_id: "new-native"});
        assert.equal(f.executor.state("remote").phase, "paused");
        assert.equal(f.executor.records.get("remote").agreed, false);
    }],
    ["popup late refresh cannot replace pause state or disable revocation", async () => {
        const vm = require("node:vm"); const fs = require("node:fs");
        const responses = [];
        const select = {value: "remote", addEventListener: () => {}, replaceChildren: () => {}};
        const state = {textContent: ""}; const error = {textContent: ""};
        const buttons = ["enable", "pause", "disconnect"].map(action => ({
            dataset: {action}, disabled: false, addEventListener: () => {},
        }));
        const context = vm.createContext({document: {
            getElementById: id => ({remote: select, state, error})[id],
            querySelectorAll: () => buttons, createElement: () => ({}),
        }, chrome: {runtime: {sendMessage: () => {
            const pending = deferred(); responses.push(pending); return pending.promise;
        }}}});
        vm.runInContext(fs.readFileSync(require.resolve("../chromium/external-links/extension/popup.js"), "utf8"), context);
        assert.equal(buttons[1].disabled, false); assert.equal(buttons[2].disabled, false);
        const pause = vm.runInContext("control('pause')", context);
        responses[1].resolve({ok: true, remotes: [{remote_key: "remote", display: "local",
            state: {phase: "paused"}, agreed: false}]}); await pause;
        responses[0].resolve({ok: true, remotes: [{remote_key: "remote", display: "old",
            state: {phase: "enabled"}, agreed: true}]});
        await vm.runInContext("Promise.resolve()", context);
        assert.match(state.textContent, /^paused/);
    }],
];
if (require.main === module) runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
module.exports = {cases};
