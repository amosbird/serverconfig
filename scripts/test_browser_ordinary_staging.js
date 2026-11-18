#!/usr/bin/env node
const {assert, clone, deferred, fixture, runCases, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const {WAL_KEY, ORDINARY_PREFIX} = require("../chromium/external-links/extension/workspace.js");
const {registerNativeEventRouter} = require("../chromium/external-links/extension/background.js");
const request = (id, url = "https://qq.com/?ordinary=exact#fragment") => ({version: 1,
    operation: "ordinary_open", focus: true, request_id: id, url, deadline_ms: Date.now() + 5000});
const fresh = () => fixture({globalAutomation: true});
function opened(f) {
    return [...f.tabs.values()].find(tab => tab.url?.includes("ordinary-staging.html#") ||
        tab.pendingUrl?.includes("ordinary-staging.html#"));
}
async function commit(f, id, url, title) {
    const tab = f.tabs.get(id); Object.assign(tab, {url, ...(title ? {title} : {})});
    delete tab.pendingUrl;
    await Promise.all(f.api.tabs.onUpdated.emit(id, {url, ...(title ? {title} : {})}, clone(tab)));
}
const observedListeners = f => [f.api.tabs.onUpdated, f.api.tabs.onRemoved,
    f.api.tabs.onDetached, f.api.tabs.onActivated].map(event => event.listeners.length);
const cases = [];
for (const initial of ["empty", "blank", "pending"]) {
    cases.push([`ordinary ${initial} staging observation waits for its committed document`, async () => {
        const f = fresh(); await f.ready();
        const listeners = observedListeners(f);
        const firstRead = deferred(); const releaseRead = deferred();
        let waiting = false;
        let expected, nonce, id;
        f.hooks["tabs.create.after"] = async result => {
            expected = result.url; nonce = expected.split("#")[1]; id = result.id;
            Object.assign(f.tabs.get(id), {url: initial === "empty" ? "" : "about:blank", title: ""});
            if (initial === "pending") f.tabs.get(id).pendingUrl = expected;
        };
        f.hooks["tabs.get.after"] = async (result, tabId) => {
            if (tabId === id && !waiting &&
                    f.calls.some(call => call.name === "tabs.ungroup" && call.args[0].includes(id))) {
                waiting = true;
                firstRead.resolve();
                await releaseRead.promise;
            }
        };
        const payload = request(`pending-${initial}`);
        const result = f.executor.ordinaryOpen(payload);
        try {
            await firstRead.promise;
            assert.equal(f.calls.some(call => call.name === "native.prepare"), false);
            await commit(f, id, expected, ORDINARY_PREFIX + nonce);
        } finally { releaseRead.resolve(); }
        assert.equal(await result, "applied");
        assert.equal(f.tabs.get(id).url, payload.url);
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, 1);
        assert.equal(f.executor.ownsTab(id), false);
        assert.deepEqual(observedListeners(f), listeners);
    }]);
}
cases.push(["a synchronous staging commit between observation and wait is never lost", async () => {
    const f = fresh(); await f.ready(); let stage;
    f.hooks["tabs.create.after"] = async result => {
        stage = {id: result.id, url: result.url, title: result.title};
        Object.assign(f.tabs.get(result.id), {url: "about:blank", pendingUrl: result.url, title: ""});
    };
    let committed = false;
    f.hooks["tabs.get.after"] = async (_result, id) => {
        if (id === stage?.id && !committed) {
            committed = true; await commit(f, id, stage.url, stage.title);
        }
    };
    assert.equal(await f.executor.ordinaryOpen(request("commit-between-read-and-wait")), "applied");
}]);
cases.push(["ordinary creation cannot expand a folded Abush group before isolation", async () => {
    const f = fresh(); await f.ready(); await f.materialize();
    const work = f.executor.recordFor("remote");
    await f.api.tabs.update(work.tab_id, {active: true});
    const group = f.groups.get(work.group_id); group.collapsed = true;
    const before = clone(group); const start = f.calls.length; const frames = f.presentationFrames.length;
    const payload = request("folded-personal-open");
    assert.equal(await f.executor.ordinaryOpen(payload), "applied");
    const calls = f.calls.slice(start);
    const created = calls.find(call => call.name === "tabs.create");
    const personal = [...f.tabs.values()].find(tab => tab.url === payload.url);
    assert.equal(created.args[0].active, false);
    assert.ok(created.args[0].index > 0);
    assert.ok(f.tabStrip(1).indexOf(personal.id) < f.tabStrip(1).indexOf(work.tab_id),
        "ordinary page stays before the folded work group even after inherited-group cleanup");
    assert.ok(calls.findIndex(call => call.name === "tabs.ungroup") <
        calls.findIndex(call => call.name === "tabs.update" && call.args[1].active));
    assert.deepEqual(group, before);
    for (const frame of f.presentationFrames.slice(frames)) {
        assert.equal(frame.groups.find(item => item.id === work.group_id).collapsed, true);
        const tab = frame.tabs.find(item => item.id === personal.id);
        if (tab?.active) assert.equal(tab.groupId, -1);
    }
    assert.equal(personal.active, true); assert.equal(personal.groupId, -1);
    assert.equal(f.executor.ownsTab(personal.id), false);
    assert.equal(f.tabs.get(work.tab_id).url, "https://work.test/exact?q=one#fragment");
}]);
for (const intervention of ["foreign pending", "foreign committed", "lost activation", "moved window", "closed"]) {
    cases.push([`${intervention} while staging loads never navigates a changed personal target`, async () => {
        const f = fresh(); await f.ready(); const observed = deferred(); let stage;
        f.hooks["tabs.create.after"] = async result => {
            stage = clone(result); Object.assign(f.tabs.get(result.id),
                {url: "about:blank", pendingUrl: result.url, title: ""});
        };
        let reported = false;
        f.hooks["tabs.get.after"] = async (_result, id) => {
            if (id === stage?.id && !reported) { reported = true; observed.resolve(); }
        };
        const result = f.executor.ordinaryOpen(request(`intervention-${intervention}`));
        await observed.promise;
        const current = f.tabs.get(stage.id);
        if (intervention === "foreign pending") current.pendingUrl = "https://personal.test/not-requested";
        if (intervention === "foreign committed") {
            current.url = "https://personal.test/not-requested"; delete current.pendingUrl;
        }
        if (intervention === "lost activation") {
            current.active = false; f.tabs.get(1).active = true;
            await Promise.all(f.api.tabs.onActivated.emit({windowId: 1, tabId: 1}));
        }
        if (intervention === "moved window") current.windowId = 2;
        if (intervention === "closed") {
            f.tabs.delete(stage.id); await Promise.all(f.api.tabs.onRemoved.emit(stage.id, {windowId: 1}));
        } else await Promise.all(f.api.tabs.onUpdated.emit(stage.id,
            {url: current.url, pendingUrl: current.pendingUrl}, clone(current)));
        assert.equal(await result, "unknown");
        assert.equal(f.calls.some(call => call.name === "native.prepare"), false);
        assert.equal(f.calls.some(call => call.name === "tabs.update" && call.args[1].url), false);
    }]);
}
for (const after of ["staging", "blank", "other extension", "unrelated http"]) {
    cases.push([`accepted pending navigation returning to ${after} is never falsely applied`, async () => {
        const f = fresh(); await f.ready(); let staging;
        f.hooks["tabs.create.after"] = async result => { staging = result.url; };
        const payload = request(`reverted-${after}`);
        f.hooks["tabs.update.after"] = async (result, id, update) => {
            if (!update.url) return;
            result.url = staging; result.pendingUrl = payload.url;
            Object.assign(f.tabs.get(id), {url: staging, pendingUrl: payload.url});
        };
        f.hooks["native.activate"] = async () => {
            const tab = opened(f); delete tab.pendingUrl;
            tab.url = after === "blank" ? "about:blank" : after === "other extension" ?
                "chrome-extension://extension/ordinary-staging.html#other" : after === "unrelated http" ?
                    "https://personal.test/not-a-correlated-redirect" : staging;
        };
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        const mutations = f.calls.filter(call => ["tabs.create", "tabs.update", "native.prepare", "native.activate"]
            .includes(call.name)).length;
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        await f.restart(); await f.emitRemote(REMOTE);
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        assert.equal(f.calls.filter(call => ["tabs.create", "tabs.update", "native.prepare", "native.activate"]
            .includes(call.name)).length, mutations);
    }]);
}
cases.push(["cancellation while waiting for staging commits leaves no late native or navigation effect", async () => {
    const f = fresh(); await f.ready(); const observed = deferred(); let stage;
    f.hooks["tabs.create.after"] = async result => {
        stage = clone(result); Object.assign(f.tabs.get(result.id),
            {url: "about:blank", pendingUrl: result.url, title: ""});
    };
    let reported = false;
    f.hooks["tabs.get.after"] = async (_result, id) => {
        if (id === stage?.id && !reported) { reported = true; observed.resolve(); }
    };
    const payload = request("cancel-pending-staging");
    const pending = f.executor.ordinaryOpen(payload); await observed.promise;
    f.executor.cancelOrdinaryOpen({version: 1, operation: "ordinary_open_cancel", request_id: payload.request_id});
    assert.equal(await pending, "unknown");
    await commit(f, stage.id, stage.url, stage.title);
    assert.equal(f.calls.some(call => call.name === "native.prepare"), false);
    assert.equal(f.calls.some(call => call.name === "tabs.update" && call.args[1].url), false);
    assert.equal(f.local[WAL_KEY].ordinary[payload.request_id].completion, "unknown");
}]);
for (const timing of ["before", "during"]) {
    cases.push([`personal tab switch ${timing} native prepare fences destination navigation`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`switch-prepare-${timing}`);
        const switchTab = async () => f.api.tabs.update(1, {active: true});
        if (timing === "before") {
            f.hooks["tabs.update.after"] = async (_result, id, update) => {
                if (update.active && !update.url && id !== 1) await switchTab();
            };
        } else {
            f.hooks["native.prepare"] = switchTab;
        }
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        assert.equal(f.tabs.get(1).active, true);
        assert.equal(f.calls.some(call => call.name === "tabs.update" && call.args[1].url), false);
        assert.equal(f.calls.some(call => call.name === "native.activate"), false);
    }]);
}
for (const mode of ["completion lost", "worker restart", "native prepare failure"]) {
    cases.push([`${mode} never repeats an admitted ordinary creation`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`terminal-${mode}`);
        if (mode === "completion lost") {
            f.hooks["native.ordinary_open_completion"] = async () => { throw new Error("completion receipt lost"); };
        }
        if (mode === "native prepare failure") {
            f.hooks["native.prepare"] = async () => { throw new Error("prepare identity unavailable"); };
        }
        if (mode === "completion lost") await assert.rejects(f.executor.ordinaryOpen(payload));
        else assert.equal(await f.executor.ordinaryOpen(payload),
            mode === "native prepare failure" ? "unknown" : "applied");
        const mutations = f.calls.filter(call => ["tabs.create", "tabs.update", "native.prepare", "native.activate"]
            .includes(call.name)).length;
        delete f.hooks["native.ordinary_open_completion"];
        if (mode === "worker restart") { await f.restart(); await f.emitRemote(REMOTE); }
        assert.equal(await f.executor.ordinaryOpen(payload),
            mode === "native prepare failure" ? "unknown" : "applied");
        assert.equal(f.calls.filter(call => ["tabs.create", "tabs.update", "native.prepare", "native.activate"]
            .includes(call.name)).length, mutations);
        if (mode === "native prepare failure") {
            assert.match(f.local[WAL_KEY].ordinary[payload.request_id].error, /prepare identity/);
        }
    }]);
}
cases.push(["deadline without a staging commit drains listeners and never continues on a late commit", async () => {
    const f = fresh(); await f.ready(); const listeners = observedListeners(f); let stage;
    f.hooks["tabs.create.after"] = async result => {
        stage = clone(result); Object.assign(f.tabs.get(result.id),
            {url: "about:blank", pendingUrl: result.url, title: ""});
    };
    const payload = request("staging-deadline"); const deadline = deferred();
    const now = Date.now; let expired = false;
    f.hooks["tabs.get.after"] = async (_result, id) => {
        if (id === stage?.id && f.api.tabs.onUpdated.listeners.length > listeners[0]) deadline.resolve();
    };
    const running = f.executor.ordinaryOpen(payload);
    try {
        await deadline.promise;
        Date.now = () => expired ? payload.deadline_ms + 1 : now();
        expired = true;
        await Promise.all(f.api.tabs.onUpdated.emit(stage.id, {title: "still preparing"}, clone(f.tabs.get(stage.id))));
        assert.equal(await running, "unknown");
    } finally { Date.now = now; deadline.resolve(); }
    assert.deepEqual(observedListeners(f), listeners);
    await commit(f, stage.id, stage.url, stage.title);
    assert.equal(f.calls.some(call => call.name === "native.prepare"), false);
}]);
for (const bootstrap of [false, true]) cases.push([
    `ordinary cancellation before ${bootstrap ? "bootstrap lease" : "native event"} readiness prevents all creation`, async () => {
        const f = fresh(); const readiness = deferred(); let route;
        registerNativeEventRouter({onEvent: callback => { route = callback; }}, f.executor,
            () => readiness.promise, error => { throw new Error(error); });
        const payload = request(`cancel-before-ready-${bootstrap}`);
        let pending;
        if (bootstrap) {
            f.executor.bootstrapLeases = [payload];
        } else {
            pending = route({service: "ordinary_open", event: "ordinary_open", payload});
        }
        route({service: "ordinary_open", event: "ordinary_open_cancel", payload: {
            version: 1, operation: "ordinary_open_cancel", request_id: payload.request_id}});
        if (bootstrap) {
            await f.executor.acceptNativeBootstrap({...require("./browser_tab_groups_fixture.js").BOOT,
                ordinary_launch_leases: [payload]});
            await f.executor.restoreWhenReady();
        } else {
            readiness.resolve(); await pending;
            if (f.executor.ordinaryOperations.get(payload.request_id)) {
                await f.executor.ordinaryOperations.get(payload.request_id).done;
            }
        }
        assert.equal(f.calls.some(call => call.name === "tabs.create"), false);
        assert.equal(f.calls.some(call => call.name === "native.prepare"), false);
        assert.equal(f.executor.ordinary[payload.request_id].completion, "not_applied");
        assert.equal(f.local[WAL_KEY].ordinary_cancellations[payload.request_id], true);
    }]);
for (const intervention of ["navigation and regroup", "activation ABA", "detach ABA"]) {
    cases.push([`operation witness fences ${intervention} across asynchronous preparation`, async () => {
        const f = fresh(); await f.ready();
        if (intervention === "navigation and regroup") {
            f.hooks["tabs.create.after"] = async result => {
                const tab = f.tabs.get(result.id);
                tab.url = "https://personal.test/changed-before-create-return"; tab.groupId = 90;
                f.syncGroups();
                await Promise.all(f.api.tabs.onUpdated.emit(result.id,
                    {url: tab.url, groupId: 90}, clone(tab)));
            };
        } else {
            f.hooks["native.prepare"] = async () => {
                const tab = opened(f);
                if (intervention === "activation ABA") {
                    await f.api.tabs.update(1, {active: true});
                    await f.api.tabs.update(tab.id, {active: true});
                } else {
                    await Promise.all(f.api.tabs.onDetached.emit(tab.id, {oldWindowId: 1}));
                    await Promise.all(f.api.tabs.onAttached.emit(tab.id, {newWindowId: 1}));
                }
            };
        }
        assert.equal(await f.executor.ordinaryOpen(request(`witness-${intervention}`)), "unknown");
        if (intervention === "navigation and regroup") {
            assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
            assert.equal(f.calls.some(call => call.name === "tabs.update"), false);
            assert.equal(f.groups.has(90), true);
        }
        assert.equal(f.calls.some(call => call.name === "tabs.update" && call.args[1].url), false);
        assert.equal(f.calls.some(call => call.name === "native.activate"), false);
    }]);
}
cases.push(["manual ungroup and regroup to the inherited work container is not automatic inheritance", async () => {
    const f = fresh(); await f.ready(); await f.materialize();
    const work = f.executor.recordFor("remote"); await f.api.tabs.update(work.tab_id, {active: true});
    const start = f.calls.length;
    f.hooks["tabs.create.after"] = async result => {
        await f.api.tabs.ungroup([result.id]);
        await f.api.tabs.group({tabIds: [result.id], groupId: work.group_id});
    };
    assert.equal(await f.executor.ordinaryOpen(request("manual-inherited-group-ABA")), "unknown");
    const calls = f.calls.slice(start);
    assert.equal(calls.filter(call => call.name === "tabs.ungroup").length, 1);
    assert.equal(calls.some(call => call.name === "tabs.update"), false);
    assert.equal(calls.some(call => call.name === "native.prepare"), false);
}]);
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
