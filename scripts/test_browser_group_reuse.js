#!/usr/bin/env node
const {assert, clone, deferred, fixture, execute, runCases, BOOT, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const fresh = () => fixture({globalAutomation: true});
const TITLE = "Abush · local";
const record = f => f.executor.recordFor("remote");
const groups = f => { f.syncGroups(); return [...f.groups.values()]; };
const snapshot = (f, ids) => ids.map(id => clone(f.tabs.get(id)));
function seed(f, id, tabId = 1, properties = {}) {
    f.tabs.get(tabId).groupId = id;
    f.syncGroups();
    Object.assign(f.groups.get(id), {title: TITLE, color: "blue", collapsed: true}, properties);
    return clone(f.groups.get(id));
}
function unchanged(f, before) {
    assert.deepEqual(snapshot(f, before.map(tab => tab.id)), before);
    for (const tab of before) assert.equal(f.executor.ownsTab(tab.id), false);
}
function onlyExactCreatedTabsGrouped(f, start = 0) {
    const calls = f.calls.slice(start);
    const created = new Set(calls.filter(call => call.name === "tabs.create")
        .map(call => call.args[0].url));
    for (const call of calls.filter(call => call.name === "tabs.group")) {
        for (const id of call.args[0].tabIds) {
            assert.ok(created.has(f.tabs.get(id).url), `group touched preexisting tab ${id}`);
        }
    }
    for (const call of calls.filter(call => call.name === "tabs.create")) {
        assert.equal(call.args[0].active, false);
    }
    assert.equal(calls.some(call => call.name === "windows.create"), false);
}
const cases = [
    ["fixture models manual group assignments, query/get, updates and empty lifetime", async () => {
        const f = fresh();
        const before = seed(f, 80);
        assert.deepEqual(await f.api.tabGroups.query({windowId: 1, title: TITLE}), [before]);
        assert.deepEqual(await f.api.tabGroups.get(80), before);
        await f.api.tabGroups.update(80, {collapsed: false});
        assert.deepEqual(await f.api.tabGroups.get(80), {...before, collapsed: false});
        await f.api.tabs.ungroup([1]);
        assert.deepEqual(await f.api.tabGroups.query({}), []);
        await assert.rejects(f.api.tabGroups.get(80), /No group/);
    }],
    ["worker restart keeps exact tab, enablement, PageRef and group", async () => {
        const f = fresh(); await f.ready();
        const old = clone(record(f)); const before = groups(f).map(clone);
        const creates = f.calls.filter(call => call.name === "tabs.create").length;
        await f.restart(); await f.emitRemote(REMOTE);
        assert.equal(record(f).tab_id, old.tab_id);
        assert.equal(record(f).enable_id, old.enable_id);
        assert.deepEqual(record(f).page, old.page);
        assert.deepEqual(groups(f), before);
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, creates);
    }],
    ["browser reopen keeps one presentation group and never adopts restored navigated work", async () => {
        const f = fresh(); await f.ready();
        await f.emitNative(execute(f));
        const old = clone(record(f)); const start = f.calls.length;
        // A restored group can contain personal members; membership grants no authority.
        f.tabs.get(1).groupId = old.group_id;
        f.syncGroups();
        let restoredTab;
        const restored = await f.restartBrowser({beforeBootstrap: async ids => {
            assert.deepEqual(f.session, {});
            restoredTab = ids.tabIds.get(old.tab_id);
            for (const id of [restoredTab, ids.tabIds.get(1)]) {
                await Promise.all(f.api.tabs.onCreated.emit(clone(f.tabs.get(id))));
            }
        }});
        const before = snapshot(f, [...f.tabs.keys()]);
        assert.equal(f.executor.ownsTab(restoredTab), false);
        await f.emitRemote(REMOTE);
        const next = record(f);
        assert.ok(next && f.executor.recordEnabledAgreed("remote"));
        assert.notEqual(next.tab_id, restoredTab);
        assert.notEqual(next.enable_id, old.enable_id);
        assert.notDeepEqual(next.page, old.page);
        assert.equal(next.browser_boot_id, restored.boot.browser_boot_id);
        unchanged(f, before);
        assert.equal(groups(f).length, 1, "reopening must not grow one group into two");
        assert.equal(next.group_id, restored.groupIds.get(old.group_id));
        onlyExactCreatedTabsGrouped(f, start);
    }],
    ["three browser boots preserve all old tabs without group growth", async () => {
        const f = fresh(); await f.ready();
        const trace = [];
        const enableIds = new Set([record(f).enable_id]);
        for (let boot = 0; boot < 3; boot++) {
            await f.restartBrowser();
            const before = snapshot(f, [...f.tabs.keys()]);
            await f.emitRemote(REMOTE);
            const next = record(f);
            unchanged(f, before);
            assert.equal(enableIds.has(next.enable_id), false);
            enableIds.add(next.enable_id);
            trace.push({boot: next.browser_boot_id, tab: next.tab_id,
                enable: next.enable_id, page: next.page.page_id, groups: groups(f).length});
        }
        assert.deepEqual(trace.map(state => state.groups), [1, 1, 1]);
        assert.equal(new Set(trace.map(state => state.tab)).size, 3);
        assert.equal(new Set(trace.map(state => state.page)).size, 3);
    }],
    ["personal exact-title container is presentation only and retains all properties", async () => {
        const f = fresh(); const originalGroup = seed(f, 70);
        const before = snapshot(f, [1, 2]);
        await f.ready();
        unchanged(f, before);
        assert.deepEqual(f.groups.get(70), originalGroup);
        assert.equal(record(f).group_id, 70);
        assert.equal(groups(f).length, 1);
        assert.equal(f.calls.some(call => call.name === "tabGroups.update" && call.args[0] === 70), false);
        onlyExactCreatedTabsGrouped(f);
        await f.emitNative(execute(f, {command: {command: "navigate",
            page: {...record(f).page, page_id: "personal-tab-1"}, url: "https://forbidden.test/"}}));
        unchanged(f, before);
        assert.equal(f.calls.some(call => call.name === "debugger.sendCommand"), false);
    }],
    ["duplicate exact-title groups select lowest id without merging or growing", async () => {
        const f = fresh();
        f.tabs.set(3, {id: 3, windowId: 1, url: "https://personal.test/three",
            groupId: 30, active: false});
        const high = seed(f, 90); const low = seed(f, 30, 3, {color: "green"});
        const before = snapshot(f, [1, 2, 3]);
        await f.ready();
        unchanged(f, before);
        assert.equal(record(f).group_id, 30);
        assert.deepEqual(groups(f).sort((a, b) => a.id - b.id), [low, high]);
        onlyExactCreatedTabsGrouped(f);
    }],
    ["matching title in another window cannot select or move that container", async () => {
        const f = fresh();
        f.windows.set(3, {id: 3, type: "normal", focused: false});
        f.tabs.set(3, {id: 3, windowId: 3, url: "https://other-window.test/",
            groupId: 40, active: true});
        const other = seed(f, 40, 3); const before = snapshot(f, [1, 2, 3]);
        await f.ready();
        unchanged(f, before);
        assert.notEqual(record(f).group_id, 40);
        assert.equal(f.groups.get(record(f).group_id).windowId, 1);
        assert.deepEqual(f.groups.get(40), other);
        assert.equal(groups(f).length, 2);
    }],
    ["title match is exact, not prefix, case folded or workbench substring", async () => {
        const f = fresh(); seed(f, 60, 1, {title: `${TITLE} personal`});
        const before = snapshot(f, [1]); await f.ready();
        unchanged(f, before);
        assert.notEqual(record(f).group_id, 60);
        assert.equal(f.groups.get(60).title, `${TITLE} personal`);
        assert.equal(groups(f).length, 2);
    }],
    ["concurrent workbenches with same display share group but not authority", async () => {
        const f = fresh(); await f.executor.acceptNativeBootstrap(BOOT);
        await f.executor.restoreWhenReady();
        const entered = deferred(); const release = deferred(); const secondCreated = deferred();
        let first = true; let created = 0;
        f.hooks["tabs.create.after"] = async () => {
            if (++created === 2) secondCreated.resolve();
        };
        f.hooks["tabs.group"] = async () => {
            if (first) { first = false; entered.resolve(); await release.promise; }
        };
        const a = f.emitRemote(REMOTE); await entered.promise;
        const other = {...clone(REMOTE), remote_key: "other", connection_id: "other-connection",
            workspace_id: "other-workspace", principal_id: "other-principal",
            owner_display: {...REMOTE.owner_display, daemon_instance_id: "other-instance"}};
        const b = f.emitRemote(other);
        try { await secondCreated.promise; } finally { release.resolve(); }
        await Promise.all([a, b]);
        const one = record(f); const two = f.executor.recordFor("other");
        assert.ok(f.executor.recordEnabledAgreed("remote"));
        assert.ok(f.executor.recordEnabledAgreed("other"));
        assert.notEqual(one.tab_id, two.tab_id);
        assert.notEqual(one.enable_id, two.enable_id);
        assert.notDeepEqual(one.page, two.page);
        assert.equal(one.group_id, two.group_id);
        assert.equal(groups(f).length, 1);
        onlyExactCreatedTabsGrouped(f);
    }],
    ["renamed collapsed group is never relabeled during reconnect or next boot", async () => {
        const f = fresh(); await f.ready(); const old = clone(record(f));
        await f.api.tabGroups.update(old.group_id, {title: "My research", color: "red", collapsed: true});
        const renamed = clone(f.groups.get(old.group_id));
        await f.emitRemote({...REMOTE, connection_id: "reconnected"});
        assert.deepEqual(f.groups.get(old.group_id), renamed);
        const ids = await f.restartBrowser(); const start = f.calls.length;
        await f.emitRemote(REMOTE);
        assert.deepEqual(f.groups.get(ids.groupIds.get(old.group_id)), {
            ...renamed, id: ids.groupIds.get(old.group_id), windowId: ids.windowIds.get(old.window_id)});
        assert.notEqual(record(f).group_id, ids.groupIds.get(old.group_id));
        assert.equal(f.calls.slice(start).some(call => call.name === "tabGroups.update" &&
            call.args[0] === ids.groupIds.get(old.group_id)), false);
    }],
];
for (const mode of ["rejected", "timed out", "applied then receipt lost"]) {
    cases.push([`group ${mode} surfaces failure and never blindly retries unknown preparation`, async () => {
        const f = fresh(); seed(f, 75);
        const before = snapshot(f, [1, 2]);
        const held = deferred(); let attempts = 0;
        if (mode === "applied then receipt lost") {
            f.hooks["tabs.group.after"] = async () => {
                attempts++; throw new Error("group receipt lost");
            };
        } else {
            f.hooks["tabs.group"] = async () => {
                attempts++;
                if (mode === "timed out") await held.promise;
                throw new Error("group unavailable");
            };
        }
        await f.ready();
        const count = f.calls.filter(call => call.name === "tabs.create").length;
        assert.equal(attempts, 1);
        assert.equal(f.executor.recordEnabledAgreed("remote"), false);
        assert.ok(f.executor.popupState().error || f.executor.popupState().remotes.some(item => item.error));
        await f.emitRemote(REMOTE); await f.executor.reconcileAll();
        assert.equal(attempts, 1);
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, count);
        if (mode === "timed out") held.resolve();
        unchanged(f, before);
        assert.equal(f.calls.some(call => call.name === "debugger.sendCommand"), false);
    }]);
}
for (const mode of ["missing", "renamed", "wrong window"]) {
    cases.push([`candidate ${mode} after lookup cannot be blindly joined or relabeled`, async () => {
        const f = fresh(); seed(f, 65);
        let lookedUp = false;
        f.hooks["tabGroups.query.after"] = async result => {
            if (lookedUp || !result.some(group => group.id === 65)) return;
            lookedUp = true;
            if (mode === "missing") {
                for (const tab of f.tabs.values()) if (tab.groupId === 65) tab.groupId = -1;
                f.syncGroups();
            } else if (mode === "renamed") {
                f.groups.get(65).title = "Human renamed";
            } else {
                f.windows.set(3, {id: 3, type: "normal", focused: false});
                f.tabs.get(1).windowId = 3; f.groups.get(65).windowId = 3;
            }
        };
        await f.ready();
        assert.equal(lookedUp, true, "production must inspect current Chrome group state");
        assert.equal(f.calls.some(call => call.name === "tabs.group" &&
            call.args[0].groupId === 65), false);
        assert.equal(f.calls.some(call => call.name === "tabGroups.update" && call.args[0] === 65), false);
        assert.equal(f.executor.ownsTab(1), false);
        assert.equal(f.tabs.get(1).url, "https://ordinary.test/a");
    }]);
}
for (const mode of ["rejected", "timed out"]) {
    cases.push([`group lookup ${mode} is visible and cannot fall back to duplicate creation`, async () => {
        const f = fresh(); seed(f, 55); let queried = 0;
        const held = deferred();
        f.hooks["tabGroups.query"] = async () => {
            queried++;
            if (mode === "timed out") await held.promise;
            throw new Error("group lookup unavailable");
        };
        await f.ready();
        assert.equal(queried, 1);
        assert.equal(f.executor.recordEnabledAgreed("remote"), false);
        assert.equal(groups(f).length, 1);
        assert.equal(f.calls.some(call => call.name === "tabs.group"), false);
        assert.ok(f.executor.popupState().error || f.executor.popupState().remotes.some(item => item.error));
        await f.emitRemote(REMOTE); await f.executor.reconcileAll();
        assert.equal(queried, 1, "unknown preparation is not automatically repeated");
        if (mode === "timed out") held.resolve();
    }]);
}
for (const action of ["Stop", "executor replacement", "window move"]) {
    cases.push([`${action} during group admission prevents stale enabled publication`, async () => {
        const f = fresh(); seed(f, 50); const before = snapshot(f, [1, 2]);
        const entered = deferred(); const release = deferred(); let first = true;
        f.hooks["tabs.group"] = async () => {
            if (first) { first = false; entered.resolve(); await release.promise; }
        };
        const preparing = f.ready(); await entered.promise;
        const start = f.calls.length;
        let changing;
        if (action === "Stop") changing = f.send({control: "stop"});
        if (action === "executor replacement") changing = f.emitRemote({...REMOTE,
            executor_fence: {executor_id: "replacement", epoch: 2}});
        if (action === "window move") {
            const id = f.calls.find(call => call.name === "tabs.group").args[0].tabIds[0];
            f.windows.set(3, {id: 3, type: "normal", focused: false});
            f.tabs.get(id).windowId = 3; f.tabs.get(id).groupId = -1;
            changing = Promise.all(f.api.tabs.onDetached.emit(id, {oldWindowId: 1, oldPosition: 0}));
        }
        release.resolve(); await Promise.all([preparing, changing]);
        const enabled = f.calls.slice(start).filter(call => call.name === "native.workspace_state" &&
            call.args[0].state.phase === "enabled");
        if (action === "executor replacement") {
            assert.equal(enabled.some(call => call.args[0].executor_fence.executor_id === "executor"), false);
        } else {
            assert.equal(enabled.length, 0);
            assert.equal(f.executor.recordEnabledAgreed("remote"), false);
        }
        unchanged(f, before);
        assert.equal(f.calls.some(call => call.name === "debugger.sendCommand"), false);
    }]);
}
cases.push(["new personal tab inherits active work container but is isolated without touching old members", async () => {
    const f = fresh(); seed(f, 45); const before = snapshot(f, [1, 2]);
    await f.ready();
    const work = record(f); const authority = clone(f.executor.state("remote"));
    await f.api.tabs.update(work.tab_id, {active: true});
    const isolated = deferred();
    f.hooks["tabs.ungroup.after"] = async (result, ids) => {
        if (ids.some(id => id !== work.tab_id && id !== 1)) isolated.resolve();
    };
    const personal = await f.api.tabs.create({windowId: 1, url: "https://new-personal.test/", active: true});
    await isolated.promise;
    assert.equal(f.tabs.get(personal.id).groupId, -1);
    assert.equal(f.executor.ownsTab(personal.id), false);
    assert.deepEqual(f.executor.state("remote"), authority);
    // Focus is user-controlled; group membership, URL and authority are not inherited.
    for (const old of before) {
        assert.deepEqual({...f.tabs.get(old.id), active: old.active}, old);
        assert.equal(f.executor.ownsTab(old.id), false);
    }
    assert.equal(f.tabs.get(1).groupId, 45);
}]);
for (const activationFirst of [false, true]) cases.push([
    `reused container isolates exact work-origin creation with activation ${activationFirst ? "before" : "after"} creation`, async () => {
        const f = fresh(); seed(f, 44); await f.ready();
        const work = record(f); await f.api.tabs.update(work.tab_id, {active: true});
        const personal = {id: 500, windowId: 1, groupId: 44, active: true, url: "https://human.test/new"};
        f.tabs.set(personal.id, personal); f.tabs.get(work.tab_id).active = false;
        if (activationFirst) await Promise.all(f.api.tabs.onActivated.emit({tabId: personal.id, windowId: 1}));
        await Promise.all(f.api.tabs.onCreated.emit(clone(personal)));
        if (!activationFirst) await Promise.all(f.api.tabs.onActivated.emit({tabId: personal.id, windowId: 1}));
        assert.equal(personal.groupId, -1);
        assert.equal(f.tabs.get(1).groupId, 44);
        assert.equal(f.executor.ownsTab(personal.id), false);
        assert.deepEqual(f.calls.filter(call => call.name === "tabs.ungroup").map(call => call.args[0]), [[500]]);
    }]);
cases.push(["late restored group member cannot inherit cleanup authority from its container", async () => {
    const f = fresh(); seed(f, 43); await f.ready();
    const restored = {id: 501, windowId: 1, groupId: 43, active: false, url: "https://restored.test/"};
    f.tabs.set(restored.id, restored);
    const before = clone(restored);
    await Promise.all(f.api.tabs.onCreated.emit(clone(restored)));
    await Promise.all(f.api.tabs.onUpdated.emit(restored.id, {groupId: 43}, clone(restored)));
    assert.deepEqual(restored, before);
    assert.equal(f.executor.ownsTab(restored.id), false);
    assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
}]);
cases.push(["source binding never follows a later manual join to another reused container", async () => {
    const f = fresh(); seed(f, 42);
    f.tabs.set(3, {id: 3, windowId: 1, groupId: 41, active: false, url: "https://personal.test/three"});
    seed(f, 41, 3, {title: "Abush · second"});
    await f.ready();
    await f.emitRemote({...REMOTE, remote_key: "second", connection_id: "second",
        workspace_id: "second", connection_display_name: "second"});
    const work = record(f); await f.api.tabs.update(work.tab_id, {active: true});
    const personal = {id: 502, windowId: 1, groupId: -1, active: true, url: "https://personal.test/new"};
    f.tabs.set(personal.id, personal);
    await Promise.all(f.api.tabs.onCreated.emit(clone(personal)));
    personal.groupId = 41;
    await Promise.all(f.api.tabs.onUpdated.emit(personal.id, {groupId: 41}, clone(personal)));
    assert.equal(personal.groupId, 41);
    assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
}]);
cases.push(["same-boot worker retains pending creation witness for a reused group", async () => {
    const f = fresh(); seed(f, 40); await f.ready();
    const work = record(f); await f.api.tabs.update(work.tab_id, {active: true});
    const personal = {id: 503, windowId: 1, groupId: -1, active: false,
        openerTabId: work.tab_id, url: "https://personal.test/pending"};
    f.tabs.set(personal.id, personal);
    await Promise.all(f.api.tabs.onCreated.emit(clone(personal)));
    await f.executor.inheritanceTail;
    for (const candidate of f.executor.personalCandidates.values()) clearTimeout(candidate.timer);
    await f.restart(); await f.emitRemote(REMOTE);
    assert.equal(record(f).group_reused, true);
    personal.groupId = 40;
    await Promise.all(f.api.tabs.onUpdated.emit(personal.id, {groupId: 40}, clone(personal)));
    assert.equal(personal.groupId, -1);
    assert.equal(f.tabs.get(1).groupId, 40);
}]);
cases.push(["expiry cannot acquire reused-group cleanup from a newer manual membership", async () => {
    const f = fresh(); seed(f, 39); await f.ready();
    await f.api.tabs.update(record(f).tab_id, {active: true});
    const personal = {id: 504, windowId: 1, groupId: -1, active: false, url: "https://personal.test/expired"};
    f.tabs.set(personal.id, personal);
    await Promise.all(f.api.tabs.onCreated.emit(clone(personal)));
    const candidate = f.executor.personalCandidates.get(personal.id);
    personal.groupId = 39;
    await candidate.timer._onTimeout();
    assert.equal(personal.groupId, 39);
    assert.equal(f.executor.personalCandidates.has(personal.id), false);
}]);
cases.push(["late active query cannot replace a newer delivered personal activation", async () => {
    const f = fresh(); seed(f, 38); await f.ready();
    await f.api.tabs.update(record(f).tab_id, {active: true});
    const entered = deferred(); const release = deferred();
    f.hooks["tabs.query.after"] = async () => { entered.resolve(); await release.promise; };
    for (const candidate of f.executor.personalCandidates.values()) clearTimeout(candidate.timer);
    const worker = f.resetWorker(); await worker.acceptNativeBootstrap(BOOT);
    const restoring = worker.restoreWhenReady(); await entered.promise;
    await f.api.tabs.update(1, {active: true});
    release.resolve(); await restoring; delete f.hooks["tabs.query.after"];
    await f.emitRemote(REMOTE);
    const personal = await f.api.tabs.create({windowId: 1, active: true, url: "https://personal.test/after-query"});
    await f.executor.inheritanceTail;
    assert.equal(f.tabs.get(personal.id).groupId, 38);
    assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
}]);
for (const extra of [{active: false}, {active: true, openerTabId: 1}]) cases.push([
    `${extra.active ? "explicit personal opener" : "inactive late restore"} does not borrow active work provenance`, async () => {
    const f = fresh(); seed(f, 37); await f.ready();
    await f.api.tabs.update(record(f).tab_id, {active: true});
    const tab = {id: 600 + f.tabs.size, windowId: 1, groupId: 37,
        url: "https://personal.test/no-proof", ...extra};
    f.tabs.set(tab.id, tab);
    await Promise.all(f.api.tabs.onCreated.emit(clone(tab)));
    assert.equal(tab.groupId, 37);
    assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
}]);
for (const delivered of [false, true]) cases.push([
    `source manual ungroup invalidates pending cleanup (${delivered ? "delivered" : "query-only"})`, async () => {
        const f = fresh(); seed(f, 36); await f.ready();
        const work = record(f); await f.api.tabs.update(work.tab_id, {active: true});
        const tab = {id: 610, windowId: 1, groupId: -1, active: true, url: "https://personal.test/pending"};
        f.tabs.set(tab.id, tab);
        await Promise.all(f.api.tabs.onCreated.emit(clone(tab)));
        const authority = clone(f.executor.state("remote"));
        f.tabs.get(work.tab_id).groupId = -1;
        if (delivered) {
            await Promise.all(f.api.tabs.onUpdated.emit(work.tab_id, {groupId: -1}, clone(f.tabs.get(work.tab_id))));
            // Returning to the original group must not resurrect the old creation witness.
            f.tabs.get(work.tab_id).groupId = 36;
            await Promise.all(f.api.tabs.onUpdated.emit(work.tab_id, {groupId: 36}, clone(f.tabs.get(work.tab_id))));
        }
        tab.groupId = 36;
        await Promise.all(f.api.tabs.onUpdated.emit(tab.id, {groupId: 36}, clone(tab)));
        assert.equal(tab.groupId, 36);
        assert.deepEqual(f.executor.state("remote"), authority);
        assert.equal(f.executor.ownsTab(work.tab_id), true);
    }]);
for (const activationFirst of [false, true]) cases.push([
    `same-boot pre-bootstrap foreground creation retains prior work predecessor (${activationFirst})`, async () => {
        const f = fresh(); seed(f, 35); await f.ready();
        const work = clone(record(f)); await f.api.tabs.update(work.tab_id, {active: true});
        await f.executor.inheritanceTail;
        const worker = f.resetWorker();
        const tab = {id: 620, windowId: 1, groupId: 35, active: true, url: "https://personal.test/early"};
        f.tabs.set(tab.id, tab); f.tabs.get(work.tab_id).active = false;
        if (activationFirst) await Promise.all(f.api.tabs.onActivated.emit({tabId: tab.id, windowId: 1}));
        await Promise.all(f.api.tabs.onCreated.emit(clone(tab)));
        if (!activationFirst) await Promise.all(f.api.tabs.onActivated.emit({tabId: tab.id, windowId: 1}));
        assert.equal(tab.groupId, 35, "unvalidated history grants no cleanup authority");
        const isolated = deferred();
        f.hooks["tabs.ungroup.after"] = async (result, ids) => { if (ids.includes(tab.id)) isolated.resolve(); };
        try {
            await worker.acceptNativeBootstrap(BOOT); await worker.restoreWhenReady();
            await isolated.promise;
        } finally { isolated.resolve(); }
        assert.equal(tab.groupId, -1);
        assert.equal(f.tabs.get(1).groupId, 35);
    }]);
for (const action of ["activation", "removal"]) cases.push([
    `seed preserves unaffected windows during another window's ${action}`, async () => {
        const f = fresh(); seed(f, 33); await f.ready();
        await f.api.tabs.update(record(f).tab_id, {active: true});
        const worker = f.resetWorker(); await worker.acceptNativeBootstrap(BOOT);
        const entered = deferred(); const release = deferred();
        f.hooks["tabs.query.after"] = async () => { entered.resolve(); await release.promise; };
        const restoring = worker.restoreWhenReady();
        try {
            await entered.promise;
            if (action === "activation") await f.api.tabs.update(2, {active: true});
            else { f.tabs.delete(2); await Promise.all(f.api.tabs.onRemoved.emit(2, {windowId: 2})); }
        } finally { release.resolve(); }
        await restoring; delete f.hooks["tabs.query.after"];
        assert.equal(worker.activeTabs.get(1)?.tab_id, record(f).tab_id);
        if (action === "removal") assert.equal(worker.activeTabs.has(2), false);
    }]);
for (const mode of ["reject", "timeout"]) cases.push([
    `transient active seed ${mode} is advisory and observation can recover`, async () => {
        const f = fresh(); const held = deferred(); const advisories = [];
        f.executor.onAdvisoryError = error => advisories.push(error.message);
        f.hooks["tabs.query"] = async () => {
            if (mode === "timeout") await held.promise;
            throw new Error("transient active inventory failure");
        };
        try {
            await f.executor.acceptNativeBootstrap(BOOT);
            await f.executor.restoreWhenReady();
            assert.equal(f.executor.restored, true);
            assert.ok(advisories.some(message => /active|inventory|timed out/.test(message)));
        } finally { held.resolve(); delete f.hooks["tabs.query"]; }
        await f.executor.restoreWhenReady();
        assert.equal(f.executor.activeTabs.get(1)?.tab_id, 1);
        await f.emitRemote(REMOTE);
        assert.equal(f.executor.recordEnabledAgreed("remote"), true);
    }]);
for (const activationFirst of [false, true]) cases.push([
    `new-browser-boot early foreground restore never receives old predecessor (${activationFirst})`, async () => {
        const f = fresh(); seed(f, 32); await f.ready();
        await f.api.tabs.update(record(f).tab_id, {active: true});
        await f.executor.inheritanceTail;
        let early;
        await f.restartBrowser({beforeBootstrap: async ids => {
            early = {id: 9000, windowId: ids.windowIds.get(1), groupId: ids.groupIds.get(32),
                active: true, url: "https://restored.test/early"};
            f.tabs.set(early.id, early);
            if (activationFirst) await Promise.all(f.api.tabs.onActivated.emit({tabId: early.id, windowId: early.windowId}));
            await Promise.all(f.api.tabs.onCreated.emit(clone(early)));
            if (!activationFirst) await Promise.all(f.api.tabs.onActivated.emit({tabId: early.id, windowId: early.windowId}));
        }});
        assert.notEqual(early.groupId, -1);
        assert.equal(f.executor.ownsTab(early.id), false);
        assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
    }]);
cases.push(["delivered source regroup during pending inspection fences cleanup", async () => {
    const f = fresh(); seed(f, 31); await f.ready();
    const work = record(f); await f.api.tabs.update(work.tab_id, {active: true});
    const entered = deferred(); const release = deferred();
    const tab = {id: 630, windowId: 1, groupId: 31, active: true, url: "https://personal.test/race"};
    f.tabs.set(tab.id, tab);
    f.hooks["tabs.get.after"] = async (result, id) => {
        if (id === work.tab_id) { entered.resolve(); await release.promise; }
    };
    const creating = Promise.all(f.api.tabs.onCreated.emit(clone(tab)));
    try {
        await entered.promise;
        await f.api.tabs.ungroup([work.tab_id]);
        await f.api.tabs.group({tabIds: [work.tab_id], groupId: 31});
    } finally { release.resolve(); }
    await creating; delete f.hooks["tabs.get.after"];
    assert.equal(tab.groupId, 31);
    assert.equal(f.calls.some(call => call.name === "tabs.ungroup" && call.args[0].includes(tab.id)), false);
    assert.equal(f.executor.recordEnabledAgreed("remote"), true);
}]);
for (const staging of [false, true]) cases.push([
    `manual move from created group never transfers blanket guard (${staging ? "pending staging" : "record"})`, async () => {
    const f = fresh(); await f.ready();
    const work = record(f); assert.equal(work.group_reused, false);
    if (staging) f.executor.workCreations.set("pending", {tab_id: work.tab_id,
        group_id: work.group_id, group_reused: false});
    seed(f, 80);
    await f.api.tabs.group({tabIds: [work.tab_id], groupId: 80});
    await f.api.tabs.update(1, {active: true});
    const personal = await f.api.tabs.create({windowId: 1, active: false, openerTabId: 1,
        url: "https://personal.test/manual-destination"});
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(f.tabs.get(personal.id).groupId, 80);
    assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
    assert.equal(f.executor.ownsTab(personal.id), false);
}]);
for (const timing of ["pre-bootstrap", "consent read", "validation"]) cases.push([
    `delivered source ungroup/regroup fences saved witness during ${timing}`, async () => {
        const f = fresh(); seed(f, 80); await f.ready();
        const work = record(f); await f.api.tabs.update(work.tab_id, {active: true});
        const tab = {id: 640, windowId: 1, groupId: -1, active: true,
            url: "https://personal.test/saved-witness"};
        f.tabs.set(tab.id, tab);
        await Promise.all(f.api.tabs.onCreated.emit(clone(tab)));
        await f.executor.persistConsent(); await f.executor.inheritanceTail;
        assert.equal(f.executor.personalCandidates.get(tab.id).inheritance_source.tab_id, work.tab_id);
        for (const candidate of f.executor.personalCandidates.values()) clearTimeout(candidate.timer);
        const worker = f.resetWorker();
        const regroup = async () => {
            await f.api.tabs.ungroup([work.tab_id]);
            await f.api.tabs.group({tabIds: [work.tab_id], groupId: 80});
        };
        if (timing === "pre-bootstrap") await regroup();
        else {
            const hook = timing === "consent read" ? "session.get.after" : "tabs.get.after";
            f.hooks[hook] = async (result, id) => {
                if (timing === "validation" && id !== work.tab_id) return;
                delete f.hooks[hook];
                await regroup();
            };
        }
        await worker.acceptNativeBootstrap(BOOT); await worker.restoreWhenReady();
        tab.groupId = 80;
        await Promise.all(f.api.tabs.onUpdated.emit(tab.id, {groupId: 80}, clone(tab)));
        assert.equal(tab.groupId, 80);
        assert.equal(f.calls.some(call => call.name === "tabs.ungroup" && call.args[0].includes(tab.id)), false);
        assert.equal(worker.ownsTab(work.tab_id), true);
    }]);
cases.push(["pre-hydration source group fences are bounded and discarded after same-boot load", async () => {
    const f = fresh(); await f.ready();
    const work = clone(record(f)); const worker = f.resetWorker();
    for (let id = 1000; id < 1300; id++) {
        await Promise.all(f.api.tabs.onUpdated.emit(id, {groupId: -1}, {id, windowId: 1, groupId: -1}));
    }
    assert.equal(worker.sourceGroupEvents.size, 128);
    await worker.acceptNativeBootstrap(BOOT); await worker.restoreWhenReady();
    assert.equal(worker.sourceGroupEvents.size, 0);
    assert.equal(record(f).group_reused, true);
    assert.ok(record(f).group_generation > (work.group_generation || 0));
    assert.equal(record(f).enable_id, work.enable_id);
    assert.deepEqual(record(f).page, work.page);
}]);
cases.push(["manual move during group journal persistence cannot relabel a personal destination", async () => {
    const f = fresh(); seed(f, 80, 1, {title: "Personal research"});
    const personalGroup = clone(f.groups.get(80));
    const entered = deferred(); const release = deferred(); let held = false;
    f.hooks["session.set"] = async value => {
        if (held || !(value.abushBrowserInheritanceV9?.creations || []).some(item => item.stage === "grouped")) return;
        held = true; entered.resolve(); await release.promise;
    };
    const preparing = f.ready();
    try {
        await entered.promise;
        const workId = f.calls.find(call => call.name === "tabs.group").args[0].tabIds[0];
        await f.api.tabs.group({tabIds: [workId], groupId: 80});
    } finally { release.resolve(); }
    await preparing;
    assert.deepEqual(f.groups.get(80), personalGroup);
    assert.equal(f.calls.some(call => call.name === "tabGroups.update" && call.args[0] === 80), false);
    assert.equal(f.executor.recordEnabledAgreed("remote"), false);
}]);
for (const explicit of [false, true]) cases.push([
    `early unresolved ${explicit ? "opener" : "active"} context cannot survive later source regroup`, async () => {
        const f = fresh(); seed(f, 80); await f.ready();
        const work = record(f); await f.api.tabs.update(work.tab_id, {active: true});
        await f.executor.inheritanceTail;
        const worker = f.resetWorker();
        const tab = {id: 650, windowId: 1, groupId: -1, active: !explicit,
            url: "https://personal.test/early-context", ...(explicit ? {openerTabId: work.tab_id} : {})};
        f.tabs.set(tab.id, tab);
        await Promise.all(f.api.tabs.onCreated.emit(clone(tab)));
        await f.api.tabs.ungroup([work.tab_id]);
        await f.api.tabs.group({tabIds: [work.tab_id], groupId: 80});
        await worker.acceptNativeBootstrap(BOOT); await worker.restoreWhenReady();
        tab.groupId = 80;
        await Promise.all(f.api.tabs.onUpdated.emit(tab.id, {groupId: 80}, clone(tab)));
        assert.equal(tab.groupId, 80);
        assert.equal(f.calls.some(call => call.name === "tabs.ungroup" && call.args[0].includes(tab.id)), false);
    }]);
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
