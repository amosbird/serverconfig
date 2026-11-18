#!/usr/bin/env node
const {assert, clone, fixture, deferred, runCases, REMOTE} = require("./browser_tab_groups_fixture.js");
const {WAL_KEY} = require("../chromium/external-links/extension/workspace.js");
const request = id => ({version: 1, operation: "ordinary_open", focus: true, request_id: id,
    url: `https://ordinary.test/loading?q=${id}#exact`, deadline_ms: Date.now() + 5000});
const fresh = () => fixture({globalAutomation: true});
const urlUpdates = f => f.calls.filter(call => call.name === "tabs.update" && call.args[1].url);
const mutations = f => f.calls.filter(call => /^(tabs\.(create|update|ungroup|move)|windows\.create)$/.test(call.name));
function nativeCreation(f, payload) {
    const created = f.calls.find(call => call.name === "tabs.create");
    assert.equal(created.args[0].url, payload.url, "Chrome receives the destination with tab creation");
    assert.equal(created.args[0].active, false);
    assert.deepEqual(urlUpdates(f), [], "activation never navigates the newly created document again");
    assert.equal(f.calls.some(call => ["native.prepare", "native.activate", "debugger.attach", "debugger.sendCommand"].includes(call.name)), false);
}
const cases = [
    ["ordinary web creation uses the native initial destination rather than a blank document", async () => {
        const f = fresh(); await f.ready(); const payload = request("native-create");
        assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
        nativeCreation(f, payload);
        const id = f.calls.find(call => call.name === "tabs.update").args[0];
        assert.deepEqual(f.calls.find(call => call.name === "tabs.update").args[1], {active: true});
        assert.equal(f.tabs.get(id).url, payload.url); assert.equal(f.executor.ownsTab(id), false);
    }],
    ["a native pending URL can be selected before any document or title commits", async () => {
        const f = fresh(); await f.ready(); const payload = request("pending-destination"); let id;
        f.hooks["tabs.create.after"] = async created => {
            id = created.id;
            Object.assign(created, {url: "", pendingUrl: payload.url, title: "", status: "loading"});
            Object.assign(f.tabs.get(id), clone(created));
        };
        assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
        nativeCreation(f, payload);
        assert.equal(f.tabs.get(id).url, ""); assert.equal(f.tabs.get(id).pendingUrl, payload.url);
        assert.equal(f.tabs.get(id).active, true);
        for (const frame of f.presentationFrames) {
            const tab = frame.tabs.find(tab => tab.id === id);
            if (tab?.active) assert.equal(tab.pendingUrl, payload.url);
        }
    }],
    ["a canonical committed destination is not navigated again", async () => {
        const f = fresh(); await f.ready(); const payload = {...request("canonical"), url: "https://ORDINARY.test:443"};
        f.hooks["tabs.create.after"] = async created => {
            created.url = new URL(payload.url).href; f.tabs.get(created.id).url = created.url;
        };
        assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
        nativeCreation(f, payload);
    }],
    ["slow loading never activates an inherited folded work group or changes work authority", async () => {
        const f = fresh(); await f.ready(); await f.materialize();
        const work = f.executor.recordFor("remote"); await f.api.tabs.update(work.tab_id, {active: true});
        f.groups.get(work.group_id).collapsed = true;
        const group = clone(f.groups.get(work.group_id)); const authority = clone(f.executor.state("remote"));
        const start = f.calls.length; const frames = f.presentationFrames.length; const payload = request("folded"); let id;
        f.hooks["tabs.create.after"] = async created => {
            id = created.id;
            assert.equal(created.groupId, work.group_id, "fixture keeps adversarial inheritance for native creation");
            Object.assign(created, {url: "about:blank", pendingUrl: payload.url, status: "loading"});
            Object.assign(f.tabs.get(id), clone(created));
        };
        assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
        const calls = f.calls.slice(start);
        assert.ok(calls.findIndex(call => call.name === "tabs.ungroup") < calls.findIndex(call => call.name === "tabs.update"));
        assert.equal(calls.some(call => call.name === "tabs.update" && call.args[1].url), false);
        assert.ok(f.tabStrip(1).indexOf(id) < f.tabStrip(1).indexOf(work.tab_id));
        for (const frame of f.presentationFrames.slice(frames)) {
            const tab = frame.tabs.find(tab => tab.id === id);
            if (tab?.active) { assert.equal(tab.groupId, -1); assert.equal(tab.pendingUrl, payload.url); }
            assert.deepEqual(frame.groups.find(group => group.id === work.group_id), group);
        }
        assert.deepEqual(f.executor.state("remote"), authority); assert.equal(f.executor.ownsTab(id), false);
    }],
];
for (const timing of ["before create returns", "during isolation"]) {
    cases.push([`an ambiguous redirect ${timing} is preserved without re-navigation or late activation`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`redirect-${timing}`); let id;
        const redirect = async () => {
            const tab = f.tabs.get(id); tab.url = "https://redirect.test/destination"; delete tab.pendingUrl;
            await Promise.all(f.api.tabs.onUpdated.emit(id, {url: tab.url}, clone(tab)));
        };
        f.hooks["tabs.create.after"] = async created => { id = created.id; if (timing === "before create returns") await redirect(); };
        if (timing === "during isolation") f.hooks["tabs.ungroup.after"] = redirect;
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        assert.equal(f.tabs.get(id).url, "https://redirect.test/destination");
        assert.equal(f.tabs.get(id).active, false); assert.deepEqual(urlUpdates(f), []);
        const count = mutations(f).length; await f.restart(); await f.emitRemote(REMOTE);
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        assert.equal(mutations(f).length, count);
    }]);
}
for (const field of ["url", "pendingUrl"]) {
    cases.push([`replacement ${field} supplied only in a Tab snapshot cannot regain presentation after ABA`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`tab-snapshot-${field}`); let id;
        f.hooks["tabs.create.after"] = async created => { id = created.id; };
        f.hooks["tabs.ungroup.after"] = async () => {
            const tab = f.tabs.get(id); const previous = tab[field];
            for (const value of ["https://human.test/replacement", previous]) {
                if (value === undefined) delete tab[field]; else tab[field] = value;
                await Promise.all(f.api.tabs.onUpdated.emit(id, {status: "loading"}, clone(tab)));
            }
        };
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        assert.equal(f.tabs.get(id).active, false); assert.deepEqual(urlUpdates(f), []);
    }]);
}
for (const event of ["replace", "transport", "boot"]) {
    cases.push([`${event} during a native creation fences all subsequent presentation`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`fence-${event}`); let id;
        f.hooks["tabs.create.after"] = async created => {
            id = created.id;
            if (event === "replace") await Promise.all(f.api.tabs.onReplaced.emit(500, id));
            else if (event === "transport") await f.executor.invalidateRemote();
            else await f.executor.acceptNativeBootstrap({...require("./browser_tab_groups_fixture.js").BOOT,
                browser_boot_id: "different-boot"});
        };
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        assert.deepEqual(urlUpdates(f), []); assert.equal(f.tabs.get(id).active, false);
        assert.equal(f.calls.some(call => call.name === "tabs.ungroup"), false);
        assert.equal(f.local[WAL_KEY].ordinary[payload.request_id].completion, "unknown");
    }]);
}
cases.push(["a late native creation receipt cannot start a second request or select after cancellation", async () => {
    const f = fresh(); await f.ready(); const payload = request("cancel-created");
    const entered = deferred(); const release = deferred(); let id;
    f.hooks["tabs.create.after"] = async created => { id = created.id; entered.resolve(); await release.promise; };
    const pending = f.executor.ordinaryOpen(payload);
    try {
        await entered.promise;
        f.executor.cancelOrdinaryOpen({version: 1, operation: "ordinary_open_cancel", request_id: payload.request_id});
    } finally { release.resolve(); }
    assert.equal(await pending, "unknown");
    assert.equal(f.tabs.get(id).url, payload.url, "the already-requested navigation is retained");
    assert.equal(f.tabs.get(id).active, false); assert.deepEqual(urlUpdates(f), []);
    const count = mutations(f).length;
    assert.equal(await f.executor.ordinaryOpen(payload), "unknown"); assert.equal(mutations(f).length, count);
}]);
for (const initial of ["empty", "blank"]) {
    cases.push([`a stopped native request with an ${initial} document cannot select its placeholder`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`stopped-${initial}`); let id;
        f.hooks["tabs.create.after"] = async created => {
            id = created.id;
            Object.assign(created, {url: initial === "empty" ? "" : "about:blank", pendingUrl: payload.url,
                status: "loading"});
            Object.assign(f.tabs.get(id), clone(created));
        };
        f.hooks["tabs.ungroup.after"] = async () => {
            const tab = f.tabs.get(id); delete tab.pendingUrl; tab.status = "complete";
            await Promise.all(f.api.tabs.onUpdated.emit(id, {status: "complete"}, clone(tab)));
        };
        assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
        assert.equal(f.tabs.get(id).active, false);
        assert.equal(f.tabs.get(1).active, true);
        assert.equal(f.calls.some(call => call.name === "tabs.update"), false);
        assert.deepEqual(urlUpdates(f), []);
    }]);
}
for (const event of ["transport", "boot"]) {
    cases.push([`a queued ordinary request cannot adopt the next ${event} generation`, async () => {
        const f = fresh(); await f.ready();
        const firstPayload = request(`first-${event}`); const queuedPayload = request(`queued-${event}`);
        const entered = deferred(); const release = deferred();
        f.hooks["tabs.create.after"] = async () => { entered.resolve(); await release.promise; };
        const first = f.executor.ordinaryOpen(firstPayload);
        let queued;
        try {
            await entered.promise;
            queued = f.executor.ordinaryOpen(queuedPayload);
            if (event === "transport") await f.executor.invalidateRemote();
            else await f.executor.acceptNativeBootstrap({...require("./browser_tab_groups_fixture.js").BOOT,
                browser_boot_id: "next-browser-boot"});
        } finally { release.resolve(); }
        assert.equal(await first, "unknown");
        assert.equal(await queued, "not_applied");
        assert.equal(f.calls.filter(call => call.name === "tabs.create").length, 1);
        assert.equal(f.calls.some(call => call.name === "tabs.update"), false);
    }]);
}
for (const initial of ["empty", "blank"]) {
    for (const timing of ["during final read", "cancel and resume ABA"]) {
        cases.push([`${initial} pending destination lost ${timing} cannot use a stale read to select a placeholder`, async () => {
            const f = fresh(); await f.ready(); const payload = request(`lost-${initial}-${timing}`); let id;
            f.hooks["tabs.create.after"] = async created => {
                id = created.id;
                Object.assign(created, {url: initial === "empty" ? "" : "about:blank", pendingUrl: payload.url,
                    status: "loading"});
                Object.assign(f.tabs.get(id), clone(created));
            };
            let stopped = false;
            f.hooks["tabs.get.after"] = async (_snapshot, tabId) => {
                if (tabId !== id || stopped || !f.calls.some(call => call.name === "tabs.ungroup")) return;
                stopped = true;
                const tab = f.tabs.get(id); delete tab.pendingUrl; tab.status = "complete";
                await Promise.all(f.api.tabs.onUpdated.emit(id, {status: "complete"}, clone(tab)));
                if (timing === "cancel and resume ABA") {
                    tab.pendingUrl = payload.url; tab.status = "loading";
                    await Promise.all(f.api.tabs.onUpdated.emit(id, {status: "loading"}, clone(tab)));
                }
            };
            assert.equal(await f.executor.ordinaryOpen(payload), "unknown");
            assert.equal(stopped, true); assert.equal(f.tabs.get(id).active, false);
            assert.equal(f.tabs.get(1).active, true);
            assert.equal(f.calls.some(call => call.name === "tabs.update"), false);
        }]);
    }
}
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
