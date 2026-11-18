#!/usr/bin/env node
const {assert, clone, deferred, fixture, runCases, REMOTE, execute} =
    require("./browser_tab_groups_fixture.js");
const {bounded} = require("../chromium/external-links/extension/workspace.js");
const request = id => ({version: 1, operation: "ordinary_open", focus: true,
    request_id: id, url: `https://qq.com/?personal=${id}`, deadline_ms: Date.now() + 5000});
function seedTab(f, id, groupId = -1, extra = {}) {
    f.tabs.set(id, {id, windowId: 1, groupId, active: false, url: `https://personal.test/${id}`, ...extra});
}
function seedGroup(f, id, properties = {}) {
    f.syncGroups(); Object.assign(f.groups.get(id), {title: "Abush · local", color: "purple",
        collapsed: true, ...properties});
}
async function open(f, id) {
    const payload = request(id);
    assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
    const tab = [...f.tabs.values()].find(tab => tab.url === payload.url);
    assert.ok(tab); assert.equal(tab.groupId, -1); assert.equal(tab.active, true);
    assert.equal(f.executor.ownsTab(tab.id), false);
    return tab.id;
}
function preserved(f, old, properties) {
    for (const before of old) {
        assert.deepEqual({...f.tabs.get(before.id), active: before.active}, before);
        assert.equal(f.executor.ownsTab(before.id), false);
    }
    for (const [id, value] of properties) assert.deepEqual(f.groups.get(id), value);
    assert.equal(f.calls.some(call => call.name === "tabGroups.move"), false);
    assert.equal(f.calls.some(call => call.name === "tabGroups.update"), false);
}
const cases = [
    ["restored collapsed group remains right of ordinary tab with only logical work reservation", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3); seedTab(f, 4, 80); seedTab(f, 5, 80);
        seedGroup(f, 80); const old = clone([...f.tabs.values()]); const groups = clone([...f.groups]);
        await f.ready();
        assert.equal(f.executor.recordFor("remote").tab_id, null);
        const id = await open(f, "logical-before-restored");
        assert.deepEqual(f.tabStrip(1), [1, 3, id, 4, 5]);
        assert.equal(f.calls.find(call => call.name === "tabs.create").args[0].index, 2);
        assert.equal(f.executor.recordFor("remote").tab_id, null);
        assert.equal(f.calls.some(call => call.name === "tabs.group"), false);
        preserved(f, old, groups);
    }],
    ["earliest actual strip position wins over group ids and query result order", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 90); seedTab(f, 4);
        seedTab(f, 5, 20); seedTab(f, 6, 20); seedGroup(f, 90); seedGroup(f, 20);
        await f.ready();
        f.hooks["tabs.query.after"] = async result => { result.reverse(); };
        const id = await open(f, "duplicate-display-groups");
        assert.deepEqual(f.tabStrip(1), [1, id, 3, 4, 5, 6]);
        assert.equal(f.groups.size, 2);
    }],
    ["personal members inside a reused display group are not moved or adopted", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedTab(f, 4, 80); seedTab(f, 5);
        seedGroup(f, 80, {collapsed: false, color: "blue"});
        const old = clone([...f.tabs.values()]); const groups = clone([...f.groups]); await f.ready();
        const id = await open(f, "mixed-group");
        assert.deepEqual(f.tabStrip(1), [1, id, 3, 4, 5]);
        preserved(f, old, groups);
    }],
    ["pinned personal prefix stays ahead of new personal tabs", async () => {
        const f = fixture({globalAutomation: true}); f.tabs.get(1).pinned = true;
        seedTab(f, 3, -1, {pinned: true}); seedTab(f, 4); seedTab(f, 5, 80); seedGroup(f, 80);
        await f.ready(); const id = await open(f, "pinned-prefix");
        assert.deepEqual(f.tabStrip(1), [1, 3, 4, id, 5]);
    }],
    ["other-window display group does not influence this window", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3); seedTab(f, 4, 80, {windowId: 2});
        seedGroup(f, 80); await f.ready(); const id = await open(f, "other-window");
        assert.deepEqual(f.tabStrip(1), [1, 3, id]); assert.deepEqual(f.tabStrip(2), [2, 4]);
    }],
    ["prefix and near-match titles are not presentation boundaries", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80);
        seedGroup(f, 80, {title: "Abush · local notes"}); await f.ready();
        const id = await open(f, "near-match"); assert.deepEqual(f.tabStrip(1), [1, 3, id]);
    }],
    ["serial personal requests remain before the current group boundary", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80); await f.ready();
        const [a, b] = await Promise.all([open(f, "one"), open(f, "two")]);
        assert.deepEqual(f.tabStrip(1), [1, a, b, 3]);
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, 2);
    }],
    ["cancel during placement reads creates nothing and does not navigate", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80); await f.ready();
        const entered = deferred(); const release = deferred();
        f.hooks["tabGroups.query.after"] = async () => { entered.resolve(); await release.promise; };
        const payload = request("cancel-position"); const pending = f.executor.ordinaryOpen(payload);
        try {
            await entered.promise;
            f.executor.cancelOrdinaryOpen({version: 1, operation: "ordinary_open_cancel", request_id: payload.request_id});
        } finally { release.resolve(); }
        assert.equal(await pending, "not_applied");
        assert.equal(f.calls.some(call => call.name === "tabs.create"), false);
    }],
    ["direct personal creation is placed once without staging, navigation or work ownership", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3); seedTab(f, 4, 80); seedGroup(f, 80);
        const old = clone([...f.tabs.values()]); const groups = clone([...f.groups]); await f.ready();
        const start = f.calls.length;
        const personal = await f.api.tabs.create({windowId: 1, active: true, url: "https://rofi.test/search"});
        await f.executor.presentationTail;
        assert.deepEqual(f.tabStrip(1), [1, 3, personal.id, 4]);
        assert.equal(f.executor.ownsTab(personal.id), false);
        assert.equal(f.tabs.get(personal.id).url, "https://rofi.test/search");
        assert.equal(f.tabs.get(personal.id).active, true);
        assert.equal(f.calls.slice(start).filter(call => call.name === "tabs.move").length, 1);
        assert.equal(f.calls.slice(start).some(call => call.name.startsWith("native.") ||
            ["tabs.update", "tabs.group", "windows.create", "windows.update"].includes(call.name)), false);
        preserved(f, old, groups);
        await f.api.tabs.move(personal.id, {index: -1});
        await f.executor.reconcileAll();
        assert.deepEqual(f.tabStrip(1), [1, 3, 4, personal.id], "later human placement is not enforced");
    }],
    ["personal creation from the exact active work tab is isolated then placed before its group", async () => {
        const f = fixture({globalAutomation: true}); await f.ready(); await f.materialize();
        const work = f.executor.recordFor("remote");
        await f.api.tabs.update(work.tab_id, {active: true});
        const authority = clone(f.executor.state("remote"));
        const personal = await f.api.tabs.create({windowId: 1, active: true, url: "https://rofi.test/history"});
        await f.executor.presentationTail;
        assert.deepEqual(f.tabStrip(1), [1, personal.id, work.tab_id]);
        assert.equal(f.tabs.get(personal.id).groupId, -1);
        assert.equal(f.executor.ownsTab(personal.id), false);
        assert.deepEqual(f.executor.state("remote"), authority);
        const effects = f.calls.filter(call => ["tabs.ungroup", "tabs.move"].includes(call.name));
        assert.deepEqual(effects.map(call => call.name), ["tabs.ungroup", "tabs.move"]);
        assert.ok(effects.every(call => call.name === "tabs.move" ? call.args[0] === personal.id :
            call.args[0].length === 1 && call.args[0][0] === personal.id));
    }],
    ["inactive creations, restored members and pre-bootstrap creations never receive personal placement", async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80);
        const early = await f.api.tabs.create({windowId: 1, active: true, url: "https://restored.test/early"});
        await f.ready();
        const inactive = await f.api.tabs.create({windowId: 1, active: false, url: "https://background.test/"});
        const restored = {id: 90, windowId: 1, groupId: 80, active: true, url: "https://restored.test/member"};
        f.tabs.set(restored.id, restored);
        await Promise.all(f.api.tabs.onCreated.emit(clone(restored)));
        await f.executor.presentationTail;
        assert.equal(f.calls.some(call => call.name === "tabs.move"), false);
        assert.equal(f.tabs.get(restored.id).groupId, 80);
        assert.equal(f.executor.ownsTab(restored.id), false);
        assert.ok(f.tabStrip(1).indexOf(early.id) > f.tabStrip(1).indexOf(3));
        assert.equal(f.tabs.get(inactive.id).active, false);
    }],
];
for (const url of ["about:blank", "chrome://newtab/", "chrome-extension://rofi/history.html"]) {
    cases.push([`new personal ${url} is placed without navigation or adoption`, async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80); await f.ready();
        const personal = await f.api.tabs.create({windowId: 1, active: true, url});
        await f.executor.presentationTail;
        assert.deepEqual(f.tabStrip(1), [1, personal.id, 3]);
        assert.equal(f.tabs.get(personal.id).url, url);
        assert.equal(f.executor.ownsTab(personal.id), false);
        assert.equal(f.calls.some(call => call.name === "tabs.update"), false);
    }]);
}
for (const intervention of ["move ABA", "pin ABA", "group ABA", "navigate ABA", "activate ABA", "closed", "detached"]) {
    cases.push([`${intervention} during placement reads prevents a late personal move`, async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80); await f.ready();
        const entered = deferred(); const release = deferred();
        f.hooks["tabGroups.query.after"] = async () => { entered.resolve(); await release.promise; };
        const personal = await f.api.tabs.create({windowId: 1, active: true, url: "https://rofi.test/new"});
        try {
            await entered.promise;
            const tab = f.tabs.get(personal.id);
            if (intervention === "move ABA") {
                await f.api.tabs.move(tab.id, {index: 0}); await f.api.tabs.move(tab.id, {index: -1});
            }
            if (intervention === "pin ABA" || intervention === "group ABA" || intervention === "navigate ABA") {
                const key = intervention === "pin ABA" ? "pinned" : intervention === "group ABA" ? "groupId" : "url";
                const before = tab[key];
                for (const value of [key === "pinned" ? true : key === "groupId" ? 700 : "https://human.test/", before]) {
                    tab[key] = value;
                    await Promise.all(f.api.tabs.onUpdated.emit(tab.id, {[key]: value}, clone(tab)));
                }
            }
            if (intervention === "activate ABA") {
                await f.api.tabs.update(1, {active: true}); await f.api.tabs.update(tab.id, {active: true});
            }
            if (intervention === "closed") {
                f.tabs.delete(tab.id); await Promise.all(f.api.tabs.onRemoved.emit(tab.id, {windowId: 1}));
            }
            if (intervention === "detached") {
                tab.windowId = 2; await Promise.all(f.api.tabs.onDetached.emit(tab.id, {oldWindowId: 1}));
            }
        } finally { release.resolve(); }
        await f.executor.presentationTail;
        assert.equal(f.calls.filter(call => call.name === "tabs.move").length,
            intervention === "move ABA" ? 2 : 0);
        assert.equal(f.executor.personalPlacements.size, 0);
    }]);
}
for (const heldOperation of ["tabs.create", "tabs.ungroup"]) {
    cases.push([`ordinary placement retains serialization after a late ${heldOperation} callback`, async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80); await f.ready();
        const entered = deferred(); const release = deferred();
        f.hooks[`${heldOperation}.after`] = async () => { entered.resolve(); await release.promise; };
        const firstPayload = request(`late-${heldOperation}`);
        const first = f.executor.ordinaryOpen(firstPayload);
        await entered.promise;
        f.executor.cancelOrdinaryOpen({version: 1, operation: "ordinary_open_cancel", request_id: firstPayload.request_id});
        const second = open(f, `after-${heldOperation}`);
        await new Promise(resolve => setImmediate(resolve));
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, 1);
        delete f.hooks[`${heldOperation}.after`]; release.resolve();
        assert.equal(await first, "unknown");
        const id = await second;
        assert.ok(f.tabStrip(1).indexOf(id) < f.tabStrip(1).indexOf(3));
        assert.equal(f.calls.filter(call => call.name === "native.prepare").length, 1);
    }]);
}
cases.push(["ordinary creation cannot overtake an unresolved work-group membership effect", async () => {
    const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80); await f.ready();
    const entered = deferred(); const release = deferred();
    f.hooks["tabs.group.after"] = async () => { entered.resolve(); await release.promise; };
    const materializing = f.materialize();
    await entered.promise;
    const ordinary = open(f, "during-grouping");
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(f.calls.filter(call => call.name === "tabs.create").length, 1);
    release.resolve();
    await materializing; const id = await ordinary;
    assert.deepEqual(f.tabStrip(1), [1, id, 3, f.executor.recordFor("remote").tab_id]);
}]);
cases.push(["human tab movement while a staging read is held prevents late activation or navigation", async () => {
    const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80); await f.ready();
    const entered = deferred(); const release = deferred(); let held = false;
    f.hooks["tabs.get.after"] = async tab => {
        if (held || tab.id < 10) return;
        held = true; entered.resolve(tab.id); await release.promise;
    };
    const pending = f.executor.ordinaryOpen(request("human-staging-move"));
    const id = await entered.promise;
    await f.api.tabs.move(id, {index: 0}); await f.api.tabs.move(id, {index: -1});
    release.resolve(); assert.equal(await pending, "unknown");
    assert.equal(f.calls.some(call => call.name === "native.prepare"), false);
    assert.equal(f.calls.some(call => call.name === "tabs.update"), false);
}]);
for (const heldOperation of ["tabs.move", "tabs.ungroup"]) {
    cases.push([`personal placement timeout cannot release a raw ${heldOperation} effect or replay it`, async () => {
        const f = fixture({globalAutomation: true}); seedTab(f, 3, 80); seedGroup(f, 80);
        await f.ready();
        if (heldOperation === "tabs.ungroup") {
            await f.materialize(); await f.api.tabs.update(f.executor.recordFor("remote").tab_id, {active: true});
        }
        const originalApi = f.executor.api.bind(f.executor);
        const timedOut = deferred();
        f.executor.api = (task, label) => label === "place new personal tab before work groups" ?
            bounded(task, label, 25).catch(error => { timedOut.resolve(error); throw error; }) : originalApi(task, label);
        const entered = deferred(); const release = deferred();
        f.hooks[`${heldOperation}.after`] = async () => { entered.resolve(); await release.promise; };
        const personal = await f.api.tabs.create({windowId: 1, active: true, url: "https://rofi.test/late"});
        await entered.promise;
        assert.match((await timedOut.promise).message, /timed out/);
        const second = {...REMOTE, remote_key: "second", principal_id: "second-principal", workspace_id: "second-workspace"};
        await f.emitRemote(second); const record = f.executor.recordFor("second");
        const groupingReached = deferred();
        f.hooks["tabs.group"] = async () => { groupingReached.resolve(); };
        const materializing = f.executor.execute(execute(f, {remote_key: "second", principal_id: second.principal_id,
            authorization: {browser_boot_id: record.browser_boot_id, enable_id: record.enable_id, revision: record.revision},
            command: {command: "navigate", page: clone(record.page), url: "https://second.test/"}}));
        await new Promise(resolve => setImmediate(resolve));
        assert.equal(f.calls.filter(call => call.name === "tabs.group").length,
            heldOperation === "tabs.ungroup" ? 1 : 0, "unresolved raw personal effect holds the presentation queue");
        delete f.hooks[`${heldOperation}.after`]; release.resolve();
        await materializing; await groupingReached.promise;
        assert.equal(f.calls.filter(call => call.name === heldOperation &&
            (heldOperation === "tabs.move" ? call.args[0] === personal.id : call.args[0].includes(personal.id))).length, 1);
        await f.executor.reconcileAll();
        assert.equal(f.executor.ownsTab(personal.id), false);
    }]);
}
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
