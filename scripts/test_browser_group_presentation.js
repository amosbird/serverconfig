#!/usr/bin/env node
const {assert, clone, deferred, fixture, execute, runCases, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const fresh = () => fixture({globalAutomation: true});
const record = f => f.executor.recordFor("remote");
const TITLE = "Abush · local";
const presentationCalls = f => f.calls.filter(call => ["tabs.create", "tabs.group", "tabs.move",
    "tabGroups.update", "tabGroups.move"].includes(call.name));
function seedPersonal(f, id, properties = {}) {
    f.tabs.set(id, {id, windowId: 1, groupId: -1, active: false,
        url: `https://personal.test/${id}`, ...properties});
}
function guardPersonalFrames(f, personalIds, from = 0) {
    for (const frame of f.presentationFrames.slice(from)) {
        assert.deepEqual(frame.strips.find(strip => strip.windowId === 1).ids
            .filter(id => personalIds.includes(id)), personalIds);
        assert.deepEqual(frame.tabs.filter(tab => tab.windowId === 1 && tab.active).map(tab => tab.id), [1]);
        assert.deepEqual(frame.windows.filter(window => window.focused).map(window => window.id), [1]);
        for (const id of personalIds) {
            assert.equal(frame.tabs.find(tab => tab.id === id).url, f.tabs.get(id).url);
        }
    }
    assert.equal(f.calls.some(call => ["windows.create", "windows.update", "tabs.move", "tabGroups.move"]
        .includes(call.name)), false);
}
function holdGroupedJournal(f) {
    const entered = deferred(); const release = deferred(); let held = false;
    f.hooks["session.set"] = async value => {
        if (held || !(value.abushBrowserInheritanceV9?.creations || []).some(item => item.stage === "grouped")) return;
        held = true; entered.resolve(); await release.promise;
    };
    return {entered, release};
}
const cases = [
    ["logical startup leaves personal strips and all group presentation unchanged", async () => {
        const f = fresh(); seedPersonal(f, 3); const before = clone(f.tabs.get(1));
        await f.ready();
        assert.deepEqual(presentationCalls(f), []);
        assert.deepEqual(f.tabs.get(1), before);
        assert.deepEqual(f.tabStrip(1), [1, 3]);
        assert.equal(record(f).tab_id, null);
    }],
    ["first navigation creates a right-edge inactive work tab and folds its fresh group once", async () => {
        const f = fresh(); seedPersonal(f, 3); seedPersonal(f, 4);
        await f.ready(); const authority = clone(f.executor.state("remote"));
        await f.materialize(); const work = record(f);
        assert.deepEqual(f.tabStrip(1), [1, 3, 4, work.tab_id]);
        const created = f.calls.find(call => call.name === "tabs.create").args[0];
        assert.ok(Number.isInteger(created.index) && created.index >= 3, "explicit right-edge creation");
        assert.equal(created.active, false);
        assert.deepEqual(f.groups.get(work.group_id), {id: work.group_id, windowId: 1,
            title: TITLE, color: "purple", collapsed: true});
        assert.deepEqual(f.executor.state("remote"), authority);
        assert.equal(f.calls.filter(call => call.name === "tabGroups.update").length, 1);
        guardPersonalFrames(f, [1, 3, 4]);
    }],
    ["right-edge creation survives a new personal tab arriving after window selection", async () => {
        const f = fresh(); await f.ready(); let seeded = false;
        f.hooks["tabs.create"] = async () => {
            if (!seeded) { seeded = true; seedPersonal(f, 3); }
        };
        await f.materialize();
        assert.deepEqual(f.tabStrip(1), [1, 3, record(f).tab_id]);
        guardPersonalFrames(f, [1, 3], f.presentationFrames.findIndex(frame => frame.name === "tabs.create"));
    }],
    ["reused expanded middle group is never moved, recolored, relabeled or folded", async () => {
        const f = fresh(); seedPersonal(f, 3, {groupId: 80}); seedPersonal(f, 4);
        f.syncGroups(); Object.assign(f.groups.get(80), {title: TITLE, color: "blue", collapsed: false});
        const before = clone(f.groups.get(80)); await f.ready(); await f.materialize();
        assert.equal(record(f).group_id, 80);
        assert.deepEqual(f.tabStrip(1), [1, 3, record(f).tab_id, 4]);
        assert.deepEqual(f.groups.get(80), before);
        assert.equal(f.calls.some(call => call.name === "tabGroups.update"), false);
        guardPersonalFrames(f, [1, 3, 4]);
    }],
    ["manual expansion and reposition survive navigation and worker or transport reconnect", async () => {
        const f = fresh(); seedPersonal(f, 3); await f.ready(); await f.materialize();
        const work = record(f); await f.api.tabGroups.update(work.group_id, {collapsed: false});
        const order = f.tabStrip(1); order.splice(order.indexOf(work.tab_id), 1); order.unshift(work.tab_id);
        await Promise.all(f.api.tabGroups.onMoved.emit(clone(f.groups.get(work.group_id))));
        const before = clone(f.groups.get(work.group_id)); const calls = presentationCalls(f).length;
        await f.executor.execute(execute(f));
        await f.emitRemote({...REMOTE, connection_id: "reconnected"});
        await f.restart(); await f.emitRemote(REMOTE);
        assert.deepEqual(f.groups.get(work.group_id), before);
        assert.deepEqual(f.tabStrip(1), [work.tab_id, 1, 3]);
        assert.equal(presentationCalls(f).length, calls);
    }],
    ["new browser boot preserves restored layout and still creates no blank work tab", async () => {
        const f = fresh(); seedPersonal(f, 3); await f.ready(); await f.materialize();
        const old = clone(record(f)); await f.api.tabGroups.update(old.group_id, {collapsed: false});
        const order = f.tabStrip(1); order.splice(order.indexOf(old.tab_id), 1); order.unshift(old.tab_id);
        const calls = presentationCalls(f).length;
        const restored = await f.restartBrowser(); await f.emitRemote(REMOTE);
        const restoredWork = restored.tabIds.get(old.tab_id);
        assert.deepEqual(f.tabStrip(restored.windowIds.get(1)), [restoredWork,
            restored.tabIds.get(1), restored.tabIds.get(3)]);
        assert.equal(f.groups.get(restored.groupIds.get(old.group_id)).collapsed, false);
        assert.equal(presentationCalls(f).length, calls);
        assert.equal(f.executor.ownsTab(restoredWork), false);
    }],
];
for (const intervention of ["active work", "personal member", "rename", "recolor", "expand", "move"]) {
    cases.push([`fresh group defaults yield to ${intervention} during journal persistence`, async () => {
        const f = fresh(); seedPersonal(f, 3); await f.ready(); const barrier = holdGroupedJournal(f);
        const preparing = f.materialize();
        try {
            await barrier.entered.promise; const work = record(f);
            if (intervention === "active work") await f.api.tabs.update(work.tab_id, {active: true});
            if (intervention === "personal member") {
                f.tabs.get(3).groupId = work.group_id;
                await Promise.all(f.api.tabs.onUpdated.emit(3, {groupId: work.group_id}, clone(f.tabs.get(3))));
            }
            if (intervention === "rename") await f.api.tabGroups.update(work.group_id, {title: "Human research"});
            if (intervention === "recolor") await f.api.tabGroups.update(work.group_id, {color: "red"});
            if (intervention === "expand") await f.api.tabGroups.update(work.group_id, {collapsed: false});
            if (intervention === "move") {
                const order = f.tabStrip(1); order.splice(order.indexOf(work.tab_id), 1); order.unshift(work.tab_id);
                await Promise.all(f.api.tabGroups.onMoved.emit(clone(f.groups.get(work.group_id))));
            }
        } finally { barrier.release.resolve(); }
        await preparing;
        assert.equal(f.calls.some(call => call.name === "tabGroups.update" && call.args[1].collapsed === true), false);
        assert.equal(f.tabs.get(1).active, intervention !== "active work");
        assert.equal(f.executor.recordEnabledAgreed("remote"), true);
        assert.equal(f.executor.ownsTab(3), false);
    }]);
}
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
