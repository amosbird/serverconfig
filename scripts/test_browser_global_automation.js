#!/usr/bin/env node
const {assert, clone, deferred, fixture, execute, runCases, BOOT, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const {AUTOMATION_KEY, CONSENT_KEY, WAL_KEY} = require("../chromium/external-links/extension/workspace.js");
const fresh = () => fixture({globalAutomation: true});
const trace = f => [f.executor.automationOn, f.executor.state("remote").phase,
    f.executor.records.get("remote")?.agreed || false, f.tabs.get(1).active,
    f.calls.filter(c => c.name === "debugger.sendCommand").length];
const cases = [
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
            assert.equal(f.tabs.size, 3);
            assert.equal(f.calls.some(c => c.name === "native.refresh_remotes"), false);
            assert.equal(f.calls.some(c => c.name === "windows.create"), false);
            assert.equal((await f.send({control: "enable", remote_key: "remote"})).ok, false);
        }]),
    ...["tabs.create", "tabs.group", "local.set", "session.set", "native.workspace_state", "debugger.attach",
        "debugger.sendCommand"].map(boundary => [`Stop during ${boundary} cannot revive authority`, async () => {
        const f = fresh();
        await f.executor.acceptNativeBootstrap(BOOT); await f.executor.restoreWhenReady();
        if (boundary.startsWith("debugger")) await f.executor.remoteState(REMOTE);
        const gate = deferred(); const entered = deferred(); let held = false;
        f.hooks[boundary] = async value => {
            if (held || boundary === "local.set" && !Object.keys(value[WAL_KEY]?.creations || {}).length ||
                boundary === "session.set" && value[CONSENT_KEY]?.records[0]?.phase !== "enabled" ||
                boundary === "native.workspace_state" && value.state.phase !== "enabled") return;
            held = true; entered.resolve(); await gate.promise;
        };
        const running = boundary.startsWith("debugger") ? f.executor.execute(execute(f)) :
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
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 1);
    }],
    ["global ON resumes verified exact tab across ordinary reconnects", async () => {
        const f = fresh(); await f.ready(); const original = clone(f.executor.records.get("remote"));
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
        assert.equal(f.tabs.size, 3);
        await f.executor.remoteState({...REMOTE, remote_key: "second", workspace_id: "second-workspace"});
        assert.equal(f.tabs.size, 4);
        await f.executor.remoteState({...REMOTE, principal_id: "foreign"});
        assert.equal(f.executor.records.get("remote").tab_id, first);
        assert.notEqual(f.executor.state("remote").phase, "enabled");
        await f.executor.invalidateRemote();
        await f.executor.remoteState({...REMOTE, remote_key: "second-alias", workspace_id: "second-workspace"});
        assert.equal(f.tabs.size, 4);
    }],
    ...["closed", "moved", "replaced"].map(action => [`human ${action} revokes and creates fresh not adopted`, async () => {
        const f = fresh(); await f.ready(); const old = f.executor.records.get("remote").tab_id;
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
        await f.ready(); delete f.hooks["tabs.create"];
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
            const f = fresh(); await f.ready(); const old = f.executor.records.get("remote").tab_id;
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
        const f = fresh(); await f.ready(); const work = f.executor.records.get("remote").tab_id;
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
