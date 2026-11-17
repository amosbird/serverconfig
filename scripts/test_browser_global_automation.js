#!/usr/bin/env node
const {assert, clone, deferred, fixture, execute, runCases, BOOT, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const {AUTOMATION_KEY, CONSENT_KEY, WAL_KEY} = require("../chromium/external-links/extension/workspace.js");
const fresh = () => fixture({globalAutomation: true});
const trace = f => [f.executor.automationOn, f.executor.state("remote").phase,
    f.executor.records.get("remote")?.agreed || false, f.tabs.get(1).active,
    f.calls.filter(c => c.name === "debugger.sendCommand").length];
const cases = [
    ...["immediate", "late"].map(timing => [
        `failed ${timing} Stop stays locally OFF across production events until retry commits`, async () => {
            const f = fresh(); await f.ready();
            await f.send({control: "start"});
            const oldPayload = execute(f);
            await f.emitNative(oldPayload);
            const gate = deferred(); const entered = deferred();
            f.hooks["local.set"] = async value => {
                if (value[AUTOMATION_KEY] !== false) return;
                entered.resolve();
                if (timing === "late") await gate.promise;
                throw new Error("Stop storage failed");
            };
            const stopping = f.send({control: "stop"}); await entered.promise;
            const response = await stopping;
            assert.equal(response.ok, false);
            assert.equal(response.automation_on, false);
            assert.equal(response.preference_pending, true);
            assert.match(response.error, /not saved.*restart/);
            if (timing === "late") {
                gate.resolve(); await f.executor.preferenceTail.catch(() => {});
            }
            assert.equal(f.executor.automationOn, false);
            assert.equal(response.stop_pending, true);
            const before = f.calls.length;
            await f.emitRemote(REMOTE);
            await Promise.all(f.api.windows.onCreated.emit({id: 99, type: "normal"}));
            await f.executor.invalidateRemote();
            await f.emitRemote({...REMOTE, connection_id: "again"});
            await f.executor.acceptNativeBootstrap({...BOOT, browser_boot_id: "new-browser"});
            await f.executor.restoreWhenReady();
            await f.emitRemote(REMOTE); await f.executor.reconcileAll();
            await f.emitNative({...oldPayload, request_id: "blocked-request", effect_id: "blocked-effect"});
            const status = await f.send({control: "status"});
            assert.equal(status.automation_on, false); assert.equal(status.stop_pending, true);
            assert.equal(f.executor.preferenceReady, false);
            assert.equal(f.local[AUTOMATION_KEY], true);
            assert.notEqual(f.executor.state("remote").phase, "enabled");
            assert.equal(f.calls.slice(before).some(c => ["tabs.create", "debugger.attach",
                "debugger.sendCommand"].includes(c.name)), false);
            delete f.hooks["local.set"];
            assert.equal((await f.send({control: "stop"})).ok, true);
            assert.equal(f.local[AUTOMATION_KEY], false);
            assert.equal(f.executor.popupState().stop_pending, false);
            await f.restart(); await f.emitRemote(REMOTE);
            assert.equal(f.executor.automationOn, false);
            assert.equal((await f.send({control: "start"})).ok, true);
            assert.equal(f.executor.recordEnabledAgreed("remote"), true);
        }]),
    ["only successful explicit ON rearms after failed Stop", async () => {
        const f = fresh(); await f.ready(); await f.send({control: "start"});
        f.hooks["local.set"] = async value => {
            if (typeof value[AUTOMATION_KEY] === "boolean") throw new Error("preference unavailable");
        };
        await f.send({control: "stop"});
        const start = await f.send({control: "start"});
        assert.equal(start.ok, false); assert.equal(start.automation_on, false);
        assert.equal(start.stop_pending, true); assert.equal(f.executor.preferenceReady, false);
        await f.emitRemote(REMOTE); await f.executor.reconcileAll();
        assert.notEqual(f.executor.state("remote").phase, "enabled");
        delete f.hooks["local.set"];
        const response = await f.send({control: "start"});
        assert.equal(response.ok, true); assert.equal(response.stop_pending, false);
        assert.equal(f.executor.recordEnabledAgreed("remote"), true);
    }],
    ["new failed OFF dominates older timed out ON completion", async () => {
        const f = fresh(); f.local[AUTOMATION_KEY] = false; await f.ready();
        const entered = deferred(); const gate = deferred();
        f.hooks["local.set"] = async value => {
            if (value[AUTOMATION_KEY] === true) { entered.resolve(); await gate.promise; }
            if (value[AUTOMATION_KEY] === false) throw new Error("Stop unavailable");
        };
        const start = f.send({control: "start"}); await entered.promise; await start;
        const stop = f.send({control: "stop"});
        gate.resolve(); await stop;
        assert.equal(f.local[AUTOMATION_KEY], true);
        assert.equal(f.executor.automationOn, false);
        assert.equal(f.executor.preferenceReady, false);
        assert.equal(f.executor.popupState().stop_pending, true);
        await f.emitRemote(REMOTE);
        assert.equal(f.calls.some(c => c.name === "tabs.create"), false);
    }],
    ...[true, false].map(oldChoice => [
        `early old browser restore ${oldChoice} cannot apply preference to newer browser generation`, async () => {
        const f = fresh(); const entered = deferred(); const gate = deferred(); let first = true;
        f.local[AUTOMATION_KEY] = oldChoice;
        await f.executor.acceptNativeBootstrap(BOOT);
        f.hooks["local.get.after"] = async () => {
            if (first) { first = false; entered.resolve(); await gate.promise; }
        };
        const oldRestore = f.executor.restoreWhenReady(); await entered.promise;
        f.local[AUTOMATION_KEY] = !oldChoice;
        await f.executor.acceptNativeBootstrap({...BOOT, browser_boot_id: "new-browser"});
        await f.executor.restoreWhenReady();
        gate.resolve(); await oldRestore;
        assert.equal(f.executor.automationOn, !oldChoice);
        await f.emitRemote(REMOTE);
        assert.equal(f.calls.some(c => c.name === "tabs.create"), false);
        assert.equal(f.executor.recordEnabledAgreed("remote"), !oldChoice);
    }]),
    ...["stop", "start"].map(control => [`failed old restore cannot replace newer ${control} choice status`, async () => {
        const f = fresh(); const entered = deferred(); const gate = deferred();
        await f.executor.acceptNativeBootstrap(BOOT);
        f.hooks["local.get"] = async () => {
            entered.resolve(); await gate.promise; throw new Error("old restore failed");
        };
        const restore = f.executor.restoreWhenReady(); restore.catch(() => {}); await entered.promise;
        await f.executor.setAutomation(control === "start");
        const status = clone(f.executor.popupState());
        gate.resolve(); await assert.rejects(restore, /old restore failed/);
        assert.deepEqual(f.executor.popupState(), status);
    }]),
    ["old boot disconnect with identical fence and connection cannot pause new boot", async () => {
        const f = fresh(); await f.ready();
        const next = {...REMOTE, owner_display: {...REMOTE.owner_display, daemon_boot_id: "next-boot"}};
        await f.emitRemote(next);
        const state = clone(f.executor.state("remote"));
        await f.emitRemote({...REMOTE, connected: false});
        assert.deepEqual(f.executor.state("remote"), state);
        assert.equal(f.executor.recordEnabledAgreed("remote"), true);
        await f.emitRemote(REMOTE);
        assert.match(f.errors.at(-1), /Old executor/);
        assert.equal(f.executor.recordEnabledAgreed("remote"), true);
    }],
    ["current alias cancellation uses complete lineage after canonical changes", async () => {
        const f = fresh(); await f.ready();
        const next = {...REMOTE, remote_key: "alias", connection_id: "alias-connection",
            owner_display: {...REMOTE.owner_display, daemon_boot_id: "next-boot"}};
        await f.emitRemote(next);
        const record = f.executor.recordFor("alias");
        const payload = execute(f, {remote_key: "alias", connection_id: next.connection_id,
            authorization: {browser_boot_id: BOOT.browser_boot_id, enable_id: record.enable_id,
                revision: record.revision}});
        const entered = deferred(); const gate = deferred();
        f.hooks["debugger.sendCommand"] = async () => { entered.resolve(); await gate.promise; };
        const running = f.emitNative(payload); await entered.promise;
        const cancelling = f.executor.cancel({remote_key: payload.remote_key,
            connection_id: payload.connection_id, principal_id: payload.principal_id,
            request_id: "cancel", effect_id: payload.effect_id, executor_fence: payload.executor_fence});
        assert.equal([...f.executor.operations.values()][0].cancelled, true);
        gate.resolve(); await running;
        assert.equal(await cancelling, "unknown");
        assert.ok(f.calls.some(c => c.name === "native.cancellation_completion" &&
            c.args[0].remote_key === "alias"));
        assert.ok(f.calls.some(c => c.name === "native.executor_completion" &&
            c.args[0].remote_key === "alias"));
    }],
    ["popup pending Stop defaults to retry rather than Start", async () => {
        const fs = require("node:fs"); const vm = require("node:vm");
        const elements = new Map();
        const element = () => ({textContent: "", disabled: false, listeners: {},
            addEventListener(name, fn) { this.listeners[name] = fn; }, replaceChildren() {}, append() {}});
        for (const id of ["toggle", "state", "error", "workbenches"]) elements.set(id, element());
        const controls = [];
        const response = {automation_on: false, preference_pending: true, stop_pending: true,
            error: "Stop not saved", remotes: []};
        vm.runInNewContext(fs.readFileSync(require.resolve("../chromium/external-links/extension/popup.js"), "utf8"), {
            document: {getElementById: id => elements.get(id), createElement: element},
            chrome: {runtime: {sendMessage: async message => { controls.push(message.control); return response; }}}});
        await new Promise(resolve => setImmediate(resolve));
        assert.equal(elements.get("toggle").textContent, "Retry Stop");
        assert.match(elements.get("state").textContent, /off locally.*not saved/);
        elements.get("toggle").listeners.click();
        await new Promise(resolve => setImmediate(resolve));
        assert.deepEqual(controls, ["status", "stop"]);
    }],
    ["daemon boot resets epoch, preserves exact tab and rejects superseded boot", async () => {
        const f = fresh(); await f.ready();
        const a = {...REMOTE, executor_fence: {executor_id: "a", epoch: 3}};
        await f.emitRemote(a);
        const original = f.executor.recordFor("remote");
        const tabId = original.tab_id; const enableId = original.enable_id;
        await f.executor.invalidateRemote();
        assert.equal(original.phase, "paused");
        const b = {...REMOTE, remote_key: "new-alias", connection_id: "b",
            executor_fence: {executor_id: "b", epoch: 1},
            owner_display: {...REMOTE.owner_display, daemon_boot_id: "boot-b"}};
        await f.emitRemote(b);
        assert.equal(f.executor.recordFor("new-alias"), original);
        assert.equal(original.tab_id, tabId); assert.equal(original.enable_id, enableId);
        assert.equal(f.executor.recordEnabledAgreed("new-alias"), true);
        const state = clone(f.executor.state("new-alias"));
        const beforeErrors = f.errors.length;
        await f.emitRemote({...a, executor_fence: {executor_id: "late-a", epoch: 4}});
        assert.match(f.errors[beforeErrors], /Old executor/);
        assert.deepEqual(f.executor.state("new-alias"), state);
        assert.equal(f.executor.recordEnabledAgreed("remote"), false);
        assert.throws(() => f.executor.selectedRemote("remote"), /connected|old executor/);
        const payload = execute(f, {remote_key: b.remote_key, connection_id: b.connection_id,
            executor_fence: b.executor_fence, authorization: {browser_boot_id: BOOT.browser_boot_id,
                enable_id: enableId, revision: state.revision}});
        await f.emitNative(payload);
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "applied");
        await assert.rejects(f.executor.execute(execute(f, {executor_fence: a.executor_fence})), /authorization|disabled/);
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 1);
        await f.executor.acceptNativeBootstrap({...BOOT, browser_boot_id: "new-browser"});
        assert.equal(f.executor.currentExecutors.size, 0);
        await f.emitRemote(a);
        assert.equal(f.executor.currentExecutor(a), true);
    }],
    ...["principal", "instance"].map(difference => [
        `distinct ${difference} has independent lineage and cannot hijack exact work`, async () => {
            const f = fresh(); await f.ready();
            await f.emitRemote({...REMOTE, executor_fence: {executor_id: "first", epoch: 3}});
            const first = f.executor.recordFor("remote");
            const other = {...REMOTE, remote_key: "other", connection_id: "other-connection",
                principal_id: difference === "principal" ? "other-principal" : REMOTE.principal_id,
                owner_display: {...REMOTE.owner_display,
                    daemon_instance_id: difference === "instance" ? "other-instance" : REMOTE.owner_display.daemon_instance_id}};
            await f.emitRemote(other);
            const second = f.executor.recordFor("other");
            assert.ok(second); assert.notEqual(first.enable_id, second.enable_id);
            assert.notDeepEqual(first.page, second.page);
            assert.equal(first.tab_id, null); assert.equal(second.tab_id, null);
            assert.equal(first.agreed, true); assert.equal(first.phase, "enabled");
            assert.equal(f.executor.recordEnabledAgreed("other"), true);
            await f.emitNative(execute(f, {remote_key: "other", connection_id: other.connection_id,
                principal_id: other.principal_id, executor_fence: other.executor_fence}));
            assert.match(f.errors.at(-1), /authorization/);
            await f.emitNative(execute(f, {executor_fence: {executor_id: "first", epoch: 3},
                authorization: {browser_boot_id: BOOT.browser_boot_id, enable_id: second.enable_id,
                    revision: second.revision}, command: {command: "navigate", page: second.page,
                    url: "https://foreign.test/"}}));
            assert.match(f.errors.at(-1), /authorization/);
            assert.equal(f.calls.some(c => c.name === "debugger.attach"), false);
        }]),
    ...["debugger.attach", "debugger.sendCommand"].map(boundary => [
        `duplicate state and window creation preserve live episode at ${boundary}`, async () => {
            const f = fresh(); await f.ready();
            const record = f.executor.records.get("remote");
            const remote = f.executor.remotes.get("remote");
            const revision = record.revision;
            const publication = record.publication;
            const entered = deferred(); const gate = deferred();
            f.hooks[boundary] = async () => { entered.resolve(); await gate.promise; };
            const running = f.executor.execute(execute(f)); running.catch(() => {}); await entered.promise;
            try {
                await f.emitRemote(clone(REMOTE));
                await Promise.all(f.api.windows.onCreated.emit({id: 99, type: "normal"}));
                assert.equal(record.agreed, true);
                assert.equal(record.revision, revision);
                assert.equal(record.publication, publication);
                assert.equal(f.executor.remotes.get("remote"), remote);
                assert.equal(f.calls.filter(c => c.name === "debugger.detach").length, 0);
            } finally { gate.resolve(); }
            assert.equal(await running, "applied");
        }]),
    ["current alias executes and delayed old disconnect cannot revoke", async () => {
        const f = fresh(); await f.ready();
        const original = f.executor.records.get("remote");
        const alias = {...REMOTE, remote_key: "alias", connection_id: "connection-b"};
        await f.emitRemote(alias);
        assert.equal(await f.executor.execute(execute(f, {remote_key: "alias", connection_id: alias.connection_id})), "applied");
        for (const override of [{principal_id: "foreign"}, {connection_id: "wrong"},
            {authorization: {browser_boot_id: BOOT.browser_boot_id, enable_id: "foreign", revision: original.revision}},
            {command: {command: "navigate", page: {...original.page, workspace_id: "foreign"}, url: "https://foreign.test/"}}]) {
            await assert.rejects(f.executor.execute(execute(f, {remote_key: "alias", connection_id: alias.connection_id, ...override})), /authorization/);
        }
        assert.ok(f.calls.some(c => c.name === "native.workspace_state" && c.args[0].remote_key === "remote" &&
            c.args[0].state.binding?.enable_id === original.enable_id));
        const next = {...alias, executor_fence: {executor_id: "next", epoch: 2}};
        await f.emitRemote(next);
        const state = f.executor.state("alias");
        await f.emitRemote({...REMOTE, connected: false});
        assert.deepEqual(f.executor.state("alias"), state);
        assert.equal(await f.executor.execute(execute(f, {remote_key: "alias", connection_id: next.connection_id,
            executor_fence: next.executor_fence, authorization: {browser_boot_id: BOOT.browser_boot_id,
                enable_id: state.binding.enable_id, revision: state.revision}, command: {
                command: "navigate", page: state.binding.page, url: "https://current.test/"}})), "applied");
        await assert.rejects(f.executor.execute(execute(f)), /authorization|disabled|paused/);
        assert.equal(f.tabs.size, 3);
        assert.equal(f.executor.ownsTab(original.tab_id), true);
        assert.ok(f.calls.some(c => c.name === "native.workspace_state" && c.args[0].remote_key === "alias"));
    }],
    ...["session.set", "debugger.detach"].map(boundary => [
        `committed ON remains observable after ${boundary} failure`, async () => {
            const f = fresh(); await f.ready();
            if (boundary === "debugger.detach") await f.executor.execute(execute(f));
            await f.send({control: "stop"});
            if (boundary === "debugger.detach") {
                await f.send({control: "start"}); await f.executor.execute(execute(f));
            }
            f.hooks[boundary] = async () => { throw new Error("injected readiness failure"); };
            const response = await f.send({control: "start"});
            assert.equal(f.local[AUTOMATION_KEY], true);
            assert.equal(f.executor.automationOn, true);
            assert.equal(response.automation_on, true);
            assert.match(response.error, /failure/);
            delete f.hooks[boundary]; await f.restart(); await f.emitRemote(REMOTE);
            assert.equal(f.executor.automationOn, true);
        }]),
    ["popup-confirmed unknown recovery preserves abandoned WAL and OFF", async () => {
        const f = fresh(); f.hooks["tabs.create"] = async () => { throw new Error("lost result"); };
        await f.ready(); await f.materialize().catch(() => {}); delete f.hooks["tabs.create"];
        assert.equal((await f.send({control: "recover", remote_key: "remote"})).ok, false);
        await f.send({control: "stop"});
        assert.equal((await f.send({control: "recover", remote_key: "remote", confirmed: true})).ok, true);
        assert.equal(f.executor.automationOn, false);
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 1);
        assert.ok(Object.values(f.local[WAL_KEY].creations).some(c => c.phase === "abandoned" && c.outcome === "unknown"));
        await f.send({control: "start"});
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 1);
        await f.materialize();
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 2);
        assert.equal(f.executor.state("remote").phase, "enabled");
    }],
    ["aliases arriving before preparation create one exact work tab", async () => {
        const f = fresh();
        await f.emitRemote(REMOTE);
        await f.emitRemote({...REMOTE, remote_key: "alias", connection_id: "b"});
        await f.executor.acceptNativeBootstrap(BOOT); await f.executor.restoreWhenReady();
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 0);
        assert.equal(f.executor.recordFor("alias"), f.executor.recordFor("remote"));
        assert.equal(await f.executor.execute(execute(f, {remote_key: "alias", connection_id: "b"})), "applied");
    }],
    ["recovery persist failure blocks fresh creation", async () => {
        const f = fresh(); f.hooks["tabs.create"] = async () => { throw new Error("lost result"); };
        await f.ready(); await f.materialize().catch(() => {}); delete f.hooks["tabs.create"];
        f.hooks["local.set"] = async value => {
            if (Object.values(value[WAL_KEY]?.creations || {}).some(c => c.phase === "abandoned")) throw new Error("abandonment not committed");
        };
        assert.equal((await f.send({control: "recover", remote_key: "remote", confirmed: true})).ok, false);
        await f.emitRemote(REMOTE); await f.send({control: "start"});
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 1);
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
    }],
    ["late abandoned create callback cannot rewrite fresh authority or WAL", async () => {
        const f = fresh(); const entered = deferred(); const gate = deferred();
        f.hooks["tabs.create.after"] = async () => { entered.resolve(); await gate.promise; };
        await f.ready();
        const preparing = f.materialize(); preparing.catch(() => {}); await entered.promise;
        const old = clone([...f.tabs.values()].find(tab => tab.id >= 10));
        await preparing.catch(() => {}); delete f.hooks["tabs.create.after"];
        assert.equal((await f.send({control: "recover", remote_key: "remote", confirmed: true})).ok, true);
        await f.materialize();
        const authority = clone(f.executor.state("remote"));
        const wal = clone(f.local[WAL_KEY]);
        gate.resolve(); await new Promise(resolve => setImmediate(resolve));
        assert.deepEqual(f.executor.state("remote"), authority);
        assert.deepEqual(f.local[WAL_KEY], wal);
        assert.deepEqual(f.tabs.get(old.id), old);
        assert.equal(f.executor.ownsTab(old.id), false);
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 2);
    }],
    ["timed out preference exposes pending and newer OFF dominates late ON", async () => {
        const f = fresh(); f.local[AUTOMATION_KEY] = false; await f.ready();
        const entered = deferred(); const gate = deferred();
        f.hooks["local.set"] = async value => {
            if (value[AUTOMATION_KEY] === true) { entered.resolve(); await gate.promise; }
        };
        const start = f.send({control: "start"}); await entered.promise;
        const response = await start;
        assert.equal(response.preference_pending, true);
        assert.equal(response.automation_on, false);
        const stop = f.send({control: "stop"}); gate.resolve(); await stop;
        assert.equal(f.local[AUTOMATION_KEY], false);
        assert.equal(f.executor.popupState().preference_pending, false);
        assert.equal(f.executor.automationOn, false);
    }],
    ...["browser-first", "workbench-first"].map(order => [
        `absent preference ${order} automatically Ready without popup`, async () => {
            const f = fresh(); const states = [trace(f)];
            if (order === "workbench-first") await f.emitRemote(REMOTE);
            states.push(trace(f));
            await f.executor.acceptNativeBootstrap(BOOT);
            await f.executor.restoreWhenReady();
            if (order === "browser-first") await f.emitRemote(REMOTE);
            states.push(trace(f));
            assert.deepEqual(states, [[false, "disabled", false, true, 0],
                [false, "disabled", false, true, 0], [true, "enabled", true, true, 0]]);
            assert.equal(f.tabs.size, 2);
            assert.equal(f.executor.recordFor("remote").tab_id, null);
            assert.equal(Object.keys(f.local[WAL_KEY].creations).length, 0);
            assert.equal(f.calls.some(c => c.name === "native.refresh_remotes"), false);
            assert.equal(f.calls.some(c => c.name === "windows.create"), false);
            assert.equal((await f.send({control: "enable", remote_key: "remote"})).ok, false);
        }]),
    ...["tabs.create", "tabs.group", "local.set", "session.set", "native.workspace_state", "debugger.attach",
        "debugger.sendCommand"].map(boundary => [`Stop during ${boundary} cannot revive authority`, async () => {
        const f = fresh();
        await f.executor.acceptNativeBootstrap(BOOT); await f.executor.restoreWhenReady();
        const executing = !["session.set", "native.workspace_state"].includes(boundary);
        if (executing) await f.executor.remoteState(REMOTE);
        const gate = deferred(); const entered = deferred(); let held = false;
        f.hooks[boundary] = async value => {
            if (held || boundary === "local.set" && !Object.keys(value[WAL_KEY]?.creations || {}).length ||
                boundary === "session.set" && value[CONSENT_KEY]?.records[0]?.phase !== "enabled" ||
                boundary === "native.workspace_state" && value.state.phase !== "enabled") return;
            held = true; entered.resolve(); await gate.promise;
        };
        const running = executing ? f.executor.execute(execute(f)) :
            f.executor.remoteState(REMOTE);
        running.catch(() => {}); await entered.promise;
        const before = trace(f);
        const stop = f.executor.setAutomation(false);
        assert.equal(f.executor.automationOn, false);
        assert.notEqual(f.executor.state("remote").phase, "enabled");
        gate.resolve(); await running.catch(() => {}); assert.equal((await stop).ok, true);
        const after = trace(f);
        assert.equal(after[0], false); assert.notEqual(after[1], "enabled");
        assert.equal(after[3], true);
        assert.equal(f.local[AUTOMATION_KEY], false);
        assert.equal(after[4], boundary === "debugger.sendCommand" ? 1 : 0);
        assert.equal(before[3], true);
        delete f.hooks[boundary];
        await f.executor.remoteState({...REMOTE, connection_id: "reconnected"});
        assert.deepEqual(trace(f), after);
    }]),
    ["Stop before browser bootstrap is durable without waiting on readiness", async () => {
        const f = fresh();
        assert.equal((await f.send({control: "stop"})).ok, true);
        await f.emitRemote(REMOTE);
        await f.executor.acceptNativeBootstrap(BOOT); await f.executor.restoreWhenReady();
        assert.deepEqual(trace(f), [false, "disabled", false, true, 0]);
        assert.equal(f.local[AUTOMATION_KEY], false);
        assert.equal(f.calls.some(c => c.name === "tabs.create"), false);
    }],
    ["Stop before stale restore read survives worker browser and transport restart", async () => {
        const f = fresh(); await f.ready();
        const gate = deferred(); const entered = deferred();
        f.hooks["local.get.after"] = async () => { entered.resolve(); await gate.promise; };
        const worker = f.resetWorker(); await worker.acceptNativeBootstrap(BOOT);
        const restore = worker.restoreWhenReady(); await entered.promise;
        const stop = f.send({control: "stop"});
        assert.equal(worker.automationOn, false);
        gate.resolve(); await restore; assert.equal((await stop).ok, true);
        delete f.hooks["local.get.after"];
        for (const boot of [BOOT, {...BOOT, browser_boot_id: "boot-two"}]) {
            await f.restart(boot); await f.executor.remoteState(REMOTE);
            await f.executor.invalidateRemote();
            await f.executor.remoteState({...REMOTE, connection_id: "again"});
            assert.equal(f.executor.automationOn, false);
            assert.notEqual(f.executor.state("remote").phase, "enabled");
        }
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 0);
    }],
    ["global ON resumes verified exact tab across ordinary reconnects", async () => {
        const f = fresh(); await f.ready(); await f.materialize(); const original = clone(f.executor.records.get("remote"));
        await f.executor.invalidateRemote();
        assert.equal(f.executor.state("remote").phase, "paused");
        await f.executor.remoteState({...REMOTE, connection_id: "new", executor_fence: {executor_id: "next", epoch: 2}});
        assert.equal(f.executor.state("remote").phase, "enabled");
        assert.equal(f.executor.records.get("remote").tab_id, original.tab_id);
        await f.restart(); await f.executor.remoteState(REMOTE);
        assert.equal(f.executor.records.get("remote").tab_id, original.tab_id);
        await f.restart({...BOOT, browser_boot_id: "new-boot"}); await f.executor.remoteState(REMOTE);
        assert.notEqual(f.executor.records.get("remote").tab_id, original.tab_id);
        assert.notEqual(f.executor.records.get("remote").enable_id, original.enable_id);
        assert.equal(f.tabs.get(original.tab_id).active, false);
    }],
    ["multiple stable workbenches and aliases never duplicate or hijack", async () => {
        const f = fresh(); await f.ready(); const first = f.executor.records.get("remote").tab_id;
        await f.executor.remoteState({...REMOTE, remote_key: "alias"});
        assert.equal(f.tabs.size, 2);
        await f.executor.remoteState({...REMOTE, remote_key: "second", workspace_id: "second-workspace"});
        assert.equal(f.tabs.size, 2);
        await f.executor.remoteState({...REMOTE, principal_id: "foreign"});
        assert.equal(f.executor.recordFor("alias").tab_id, first);
        assert.notEqual(f.executor.state("remote").phase, "enabled");
        await f.executor.invalidateRemote();
        await f.executor.remoteState({...REMOTE, remote_key: "second-alias", workspace_id: "second-workspace"});
        assert.equal(f.tabs.size, 2);
    }],
    ...["closed", "moved", "replaced"].map(action => [`human ${action} revokes and creates fresh not adopted`, async () => {
        const f = fresh(); await f.ready(); await f.materialize(); const old = f.executor.records.get("remote").tab_id;
        if (action === "closed") f.tabs.delete(old);
        if (action === "moved") f.tabs.get(old).windowId = 2;
        const revoke = Promise.all(action === "closed" ? f.api.tabs.onRemoved.emit(old) :
            action === "moved" ? f.api.tabs.onDetached.emit(old, {oldWindowId: 1}) :
            f.api.tabs.onReplaced.emit(999, old));
        assert.equal(f.executor.state("remote").phase, "disabled");
        await revoke;
        assert.equal(f.executor.state("remote").phase, "enabled");
        assert.notEqual(f.executor.records.get("remote").tab_id, old);
        assert.equal(f.executor.ownsTab(old), false);
        assert.equal(f.executor.automationOn, true);
        assert.equal(f.tabs.get(1).active, true);
    }]),
    ["missing normal window stays unavailable and reconciles later without window creation", async () => {
        const f = fresh(); f.windows.delete(1); await f.ready();
        assert.equal(f.executor.state("remote").phase, "disabled");
        assert.match(f.executor.lastError, /normal Chrome window/);
        f.windows.set(1, {id: 1, type: "normal"}); await f.executor.reconcileAll();
        assert.equal(f.executor.state("remote").phase, "enabled");
        assert.equal(f.calls.some(c => c.name === "windows.create"), false);
    }],
    ["unknown creation is explicit and never retried on Start or reconnect", async () => {
        const f = fresh(); f.hooks["tabs.create"] = async () => { throw new Error("creation result lost"); };
        await f.ready(); await f.materialize().catch(() => {}); delete f.hooks["tabs.create"];
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
        await f.send({control: "stop"}); await f.send({control: "start"});
        await f.executor.invalidateRemote(); await f.executor.remoteState(REMOTE);
        await f.restart(); await f.executor.remoteState(REMOTE);
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
        await f.restart({...BOOT, browser_boot_id: "another-boot"});
        await f.executor.remoteState({...REMOTE, remote_key: "new-alias"});
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 1);
    }],
    ["unknown navigation remains immutable across Stop Start and replay", async () => {
        const f = fresh(); await f.ready(); const payload = execute(f);
        f.hooks["debugger.sendCommand"] = async () => { throw new Error("navigation result lost"); };
        await f.executor.execute(payload).catch(() => {});
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "unknown");
        delete f.hooks["debugger.sendCommand"];
        await f.send({control: "stop"}); await f.send({control: "start"});
        assert.equal(await f.executor.execute(payload), "unknown");
        assert.equal(f.calls.filter(c => c.name === "debugger.sendCommand").length, 1);
    }],
    ["rejected missing and malformed storage fail closed", async () => {
        for (const mode of ["reject", "missing", "malformed"]) {
            const f = fresh();
            if (mode === "reject") f.hooks["local.get"] = async () => { throw new Error("storage rejected"); };
            if (mode === "missing") delete f.api.storage.local;
            if (mode === "malformed") f.local[AUTOMATION_KEY] = "yes";
            await assert.rejects(f.ready());
            assert.equal(f.executor.automationOn, false);
            assert.equal(f.calls.some(c => c.name === "tabs.create"), false);
        }
        const f = fresh(); f.local[AUTOMATION_KEY] = false; await f.ready();
        f.hooks["local.set"] = async () => { throw new Error("write rejected"); };
        assert.equal((await f.send({control: "start"})).ok, false);
        assert.equal(f.executor.automationOn, false);
        assert.equal(f.tabs.size, 2);
    }],
    ["worker restore transport alias reuses exact record", async () => {
        for (const early of [false, true]) {
            const f = fresh(); await f.ready(); await f.materialize(); const old = f.executor.records.get("remote").tab_id;
            const worker = f.resetWorker(); await worker.acceptNativeBootstrap(BOOT);
            if (early) await worker.remoteState({...REMOTE, remote_key: "alias"});
            await worker.restoreWhenReady();
            if (!early) await worker.remoteState({...REMOTE, remote_key: "alias"});
            assert.equal(worker.records.get("alias").tab_id, old);
            assert.equal(worker.records.get("alias").agreed, true);
            assert.equal(f.tabs.size, 3);
        }
    }],
    ["older Start write cannot overwrite newer Stop after bounded failure", async () => {
        const f = fresh(); f.local[AUTOMATION_KEY] = false; await f.ready();
        const entered = deferred(); const gate = deferred();
        f.hooks["local.set"] = async value => {
            if (value[AUTOMATION_KEY] === true) { entered.resolve(); await gate.promise; }
        };
        const start = f.executor.setAutomation(true); await entered.promise;
        const stop = f.executor.setAutomation(false);
        assert.equal(f.executor.automationOn, false);
        gate.resolve(); await start; await stop;
        assert.equal(f.local[AUTOMATION_KEY], false);
        assert.equal(f.executor.automationOn, false);
        assert.equal(f.calls.some(c => c.name === "tabs.create"), false);
    }],
    ["ordinary opens remain active ungrouped while automation busy and off", async () => {
        const f = fresh(); await f.ready(); await f.materialize(); const work = f.executor.records.get("remote").tab_id;
        const gate = deferred(); const entered = deferred();
        f.hooks["debugger.sendCommand"] = async () => { entered.resolve(); await gate.promise; };
        const running = f.executor.execute(execute(f)); await entered.promise;
        for (const off of [false, true]) {
            if (off) { gate.resolve(); await running; await f.send({control: "stop"}); }
            const request = {version: 1, operation: "ordinary_open", request_id: crypto.randomUUID(),
                url: "https://personal.test/", focus: true, deadline_ms: Date.now() + 30000};
            await f.executor.ordinaryOpen(request);
            const personal = [...f.tabs.values()].filter(tab => tab.url === request.url).at(-1);
            assert.equal(personal.active, true); assert.equal(personal.groupId, -1);
            assert.equal(f.executor.ownsTab(personal.id), false);
            assert.equal(f.tabs.get(work).active, false);
        }
    }],
];
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
