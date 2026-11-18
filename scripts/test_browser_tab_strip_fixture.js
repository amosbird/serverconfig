#!/usr/bin/env node
const {assert, clone, deferred, fixture, runCases} = require("./browser_tab_groups_fixture.js");

function fresh(options) {
    const f = fixture({inheritActiveGroup: false, ...options});
    // These are API-model tests, not executor integration tests.
    for (const value of Object.values(f.api.tabs)) {
        if (value?.listeners) value.listeners.length = 0;
    }
    for (const value of Object.values(f.api.tabGroups)) {
        if (value?.listeners) value.listeners.length = 0;
    }
    return f;
}
const create = (f, args = {}) => f.api.tabs.create({windowId: 1,
    url: "https://personal.test/", active: false, ...args});
async function coherent(f) {
    const tabs = await f.api.tabs.query({windowId: 1});
    assert.deepEqual(tabs.map(tab => tab.id), f.tabStrip(1));
    assert.deepEqual(tabs.map(tab => tab.index), tabs.map((_, index) => index));
    let unpinned = false;
    const closedGroups = new Set();
    let previous = -1;
    for (const tab of tabs) {
        if (!tab.pinned) unpinned = true;
        else assert.equal(unpinned, false, "pinned prefix");
        if (tab.groupId !== previous) {
            if (previous >= 0) closedGroups.add(previous);
            assert.equal(closedGroups.has(tab.groupId), false, "contiguous groups");
        }
        if (tab.pinned) assert.equal(tab.groupId, -1);
        previous = tab.groupId;
    }
}
const cases = [
    ["explicit insertion before active Abush group stays outside it", async () => {
        const f = fresh({inheritActiveGroup: true});
        const work = await create(f);
        const groupId = await f.api.tabs.group({tabIds: work.id});
        await f.api.tabGroups.update(groupId, {title: "Abush", color: "blue"});
        await f.api.tabs.update(work.id, {active: true});
        const metadata = clone(f.groups.get(groupId));
        const ordinary = await create(f, {index: 1, active: true});
        assert.equal(ordinary.index, 1);
        assert.equal(ordinary.groupId, -1);
        assert.deepEqual(f.tabStrip(1), [1, ordinary.id, work.id]);
        assert.deepEqual(f.groups.get(groupId), metadata);
        await coherent(f);
    }],
    ["legacy hostile append inheritance remains available and contiguous", async () => {
        const f = fresh({inheritActiveGroup: true});
        const work = await create(f);
        const groupId = await f.api.tabs.group({tabIds: work.id});
        await create(f, {index: 0});
        await f.api.tabs.update(work.id, {active: true});
        const ordinary = await create(f);
        assert.equal(ordinary.groupId, groupId);
        assert.equal(ordinary.index, f.tabStrip(1).indexOf(work.id) + 1);
        await coherent(f);
    }],
    ["realistic option disables active-group append inheritance", async () => {
        const f = fresh();
        const groupId = await f.api.tabs.group({tabIds: 1});
        const ordinary = await create(f);
        assert.equal(ordinary.groupId, -1);
        assert.equal(f.tabs.get(1).groupId, groupId);
        await coherent(f);
    }],
    ["creation shifts indices without synthetic moved events", async () => {
        const f = fresh();
        const moved = [];
        f.api.tabs.onMoved.addListener((...args) => moved.push(args));
        let eventTab;
        f.api.tabs.onCreated.addListener(tab => { eventTab = tab; });
        const created = await create(f, {index: 0});
        assert.deepEqual(eventTab, created);
        assert.equal(created.index, 0);
        assert.equal((await f.api.tabs.get(1)).index, 1);
        assert.deepEqual(moved, []);
        await coherent(f);
    }],
    ["move clamps indices, preserves selection and only emits explicit tab", async () => {
        const f = fresh();
        const a = await create(f);
        const b = await create(f);
        const moved = [];
        f.api.tabs.onMoved.addListener((id, info) => {
            assert.equal(f.tabStrip(info.windowId)[info.toIndex], id);
            moved.push([id, info.fromIndex, info.toIndex]);
        });
        assert.equal((await f.api.tabs.move(a.id, {index: 999})).index, 2);
        assert.equal((await f.api.tabs.move(a.id, {index: -1})).index, 2);
        assert.deepEqual(moved, [[a.id, 1, 2]]);
        assert.equal(f.tabs.get(1).active, true);
        assert.equal(f.tabs.get(b.id).active, false);
        await assert.rejects(f.api.tabs.move(a.id, {index: 0, windowId: 2}), /Cross-window/);
        await coherent(f);
    }],
    ["create and move respect pinned prefix", async () => {
        const f = fresh();
        const pinned = await create(f, {pinned: true, index: 999});
        const ordinary = await create(f, {index: 0});
        assert.equal(pinned.index, 0);
        assert.equal(ordinary.index, 1);
        assert.equal((await f.api.tabs.move(ordinary.id, {index: 0})).index, 1);
        assert.equal((await f.api.tabs.move(pinned.id, {index: -1})).index, 0);
        await assert.rejects(f.api.tabs.group({tabIds: pinned.id}), /pinned/);
        await coherent(f);
    }],
    ["group only relocates explicit ids and retains old personal order and metadata", async () => {
        const f = fresh();
        const a = await create(f);
        const b = await create(f);
        const c = await create(f);
        const groupId = await f.api.tabs.group({tabIds: [1, a.id]});
        await f.api.tabGroups.update(groupId, {title: "Personal", color: "red"});
        const metadata = clone(f.groups.get(groupId));
        const moved = [];
        const updates = [];
        f.api.tabs.onMoved.addListener((id, info) => moved.push([id, info]));
        f.api.tabs.onUpdated.addListener((id, change, tab) => {
            assert.equal(tab.index, f.tabStrip(1).indexOf(id));
            updates.push(id);
        });
        await f.api.tabs.group({tabIds: c.id, groupId});
        assert.deepEqual(f.tabStrip(1), [1, a.id, c.id, b.id]);
        assert.deepEqual(moved, [[c.id, {windowId: 1, fromIndex: 3, toIndex: 2}]]);
        assert.deepEqual(updates, [c.id]);
        assert.deepEqual(f.groups.get(groupId), metadata);
        await coherent(f);
    }],
    ["ungroup middle tab moves only it to group boundary with accurate event indices", async () => {
        const f = fresh();
        const a = await create(f);
        const b = await create(f);
        const groupId = await f.api.tabs.group({tabIds: [1, a.id, b.id]});
        const metadata = clone(f.groups.get(groupId));
        const moved = [];
        f.api.tabs.onMoved.addListener((id, info) => moved.push([id, info]));
        await f.api.tabs.ungroup(a.id);
        assert.deepEqual(f.tabStrip(1), [1, b.id, a.id]);
        assert.deepEqual(moved, [[a.id, {windowId: 1, fromIndex: 1, toIndex: 2}]]);
        assert.deepEqual(f.groups.get(groupId), metadata);
        await coherent(f);
    }],
    ["move cannot split unrelated group and does not change its metadata", async () => {
        const f = fresh();
        const a = await create(f);
        const b = await create(f);
        const groupId = await f.api.tabs.group({tabIds: [1, a.id]});
        const metadata = clone(f.groups.get(groupId));
        await f.api.tabs.move(b.id, {index: 1});
        assert.deepEqual(f.tabStrip(1), [1, a.id, b.id]);
        assert.equal(f.tabs.get(b.id).groupId, -1);
        assert.deepEqual(f.groups.get(groupId), metadata);
        await coherent(f);
    }],
    ["pin and unpin relocate only target and keep grouped peers contiguous", async () => {
        const f = fresh();
        const a = await create(f);
        const b = await create(f);
        const groupId = await f.api.tabs.group({tabIds: [1, a.id, b.id]});
        const metadata = clone(f.groups.get(groupId));
        assert.equal((await f.api.tabs.update(a.id, {pinned: true})).index, 0);
        assert.equal(f.tabs.get(a.id).groupId, -1);
        assert.deepEqual(f.tabStrip(1), [a.id, 1, b.id]);
        await coherent(f);
        await f.api.tabs.update(a.id, {pinned: false});
        assert.deepEqual(f.groups.get(groupId), metadata);
        await coherent(f);
    }],
    ["raw move event precedes delayed callback and is not replayed", async () => {
        const f = fresh();
        const a = await create(f);
        const entered = deferred();
        const release = deferred();
        const events = [];
        f.api.tabs.onMoved.addListener((id, info) => events.push([id, info]));
        f.hooks["tabs.move.after"] = async result => {
            entered.resolve(result);
            await release.promise;
        };
        const pending = f.api.tabs.move(a.id, {index: 0});
        try {
            assert.equal((await entered.promise).index, 0);
            assert.deepEqual(f.tabStrip(1), [a.id, 1]);
            assert.deepEqual(events, [[a.id, {windowId: 1, fromIndex: 1, toIndex: 0}]]);
        } finally {
            release.resolve();
        }
        await pending;
        assert.equal(events.length, 1);
        await coherent(f);
    }],
    ["raw mutation and events finish before delayed API callback, snapshots stay immutable", async () => {
        const f = fresh();
        const entered = deferred();
        const release = deferred();
        const events = [];
        f.api.tabs.onCreated.addListener(tab => events.push(tab));
        f.hooks["tabs.create.after"] = async result => {
            entered.resolve(result);
            await release.promise;
        };
        let settled = false;
        const pending = create(f, {index: 0}).then(result => { settled = true; return result; });
        try {
            const raw = await entered.promise;
            assert.equal(settled, false);
            assert.equal(events.length, 1);
            assert.equal((await f.api.tabs.get(raw.id)).index, 0);
            await f.api.tabs.move(raw.id, {index: -1});
            assert.equal(raw.index, 0);
            assert.equal(events[0].index, 0);
        } finally {
            release.resolve();
        }
        assert.equal((await pending).index, 0);
        await coherent(f);
    }],
];
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
