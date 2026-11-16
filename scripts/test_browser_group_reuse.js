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
                f.tabs.get(1).groupId = -1; f.syncGroups();
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
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
