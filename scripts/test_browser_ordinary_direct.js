#!/usr/bin/env node
const {assert, clone, deferred, fixture, runCases, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const {WAL_KEY, AUTOMATION_KEY} = require("../chromium/external-links/extension/workspace.js");
const request = (id, url = `https://qq.com/?ordinary=${id}#fragment`) => ({version: 1,
    operation: "ordinary_open", focus: true, request_id: id, url, deadline_ms: Date.now() + 5000});
const fresh = () => fixture({globalAutomation: true});
const effects = f => f.calls.filter(call => /^(tabs\.(create|update|ungroup|move)|windows\.create)$/.test(call.name));
function noDesktopHandshake(f, start = 0) {
    assert.equal(f.calls.slice(start).some(call =>
        ["native.prepare", "native.activate", "native.ordinary_file_navigate"].includes(call.name)), false);
}
function noVisibleStaging(f, start = 0) {
    for (const frame of f.presentationFrames.slice(start)) {
        for (const tab of frame.tabs.filter(tab => tab.active)) {
            assert.equal(/ordinary-staging\.html|__ABUSH_ORDINARY__/.test(`${tab.url} ${tab.title}`), false);
        }
    }
}
const cases = [];
for (const initial of ["empty", "pending blank"]) {
    cases.push([`${initial} ordinary creation does not need a committed preparation document`, async () => {
        const f = fresh(); await f.ready();
        f.hooks["tabs.create.after"] = async created => {
            Object.assign(f.tabs.get(created.id), {url: "", title: ""});
            if (initial === "pending blank") f.tabs.get(created.id).pendingUrl = "about:blank";
        };
        f.hooks["tabs.update.after"] = async (_result, id, update) => {
            if (update.url) delete f.tabs.get(id).pendingUrl;
        };
        const payload = request(`uncommitted-${initial}`);
        assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
        assert.equal(f.calls.filter(call => call.name === "tabs.update" && call.args[1].url === payload.url).length, 1);
        noDesktopHandshake(f); noVisibleStaging(f);
    }]);
}
for (const url of ["http://ordinary.test/exact?q=one#fragment", "https://qq.com/exact?q=one#fragment"]) {
    cases.push([`${new URL(url).protocol} opens without committing a staging page or calling desktop IPC`, async () => {
        const f = fresh(); await f.ready();
        const forbid = async () => { throw new Error("desktop IPC must not gate a web navigation"); };
        f.hooks["native.prepare"] = forbid; f.hooks["native.activate"] = forbid;
        const payload = request("without-desktop", url);
        assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
        noDesktopHandshake(f); noVisibleStaging(f);
        const created = f.calls.find(call => call.name === "tabs.create");
        assert.equal(created.args[0].url, "about:blank");
        assert.equal(created.args[0].active, false);
        const updates = f.calls.filter(call => call.name === "tabs.update");
        assert.equal(updates.length, 1);
        assert.deepEqual(updates[0].args[1], {url, active: true});
        const tab = f.tabs.get(updates[0].args[0]);
        assert.equal(tab.url, url); assert.equal(tab.groupId, -1); assert.equal(tab.active, true);
        assert.equal(f.executor.ownsTab(tab.id), false);
    }]);
}
cases.push(["navigation and activation follow exact isolation without exposing the collapsed work group", async () => {
    const f = fresh(); await f.ready(); await f.materialize();
    const work = f.executor.recordFor("remote"); await f.api.tabs.update(work.tab_id, {active: true});
    f.groups.get(work.group_id).collapsed = true;
    const group = clone(f.groups.get(work.group_id));
    const authority = clone(f.executor.state("remote")); const workUrl = f.tabs.get(work.tab_id).url;
    const start = f.calls.length; const frames = f.presentationFrames.length;
    const payload = request("folded-direct");
    assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
    const calls = f.calls.slice(start);
    const id = calls.find(call => call.name === "tabs.update" && call.args[1].url).args[0];
    assert.ok(calls.findIndex(call => call.name === "tabs.ungroup") <
        calls.findIndex(call => call.name === "tabs.update"));
    assert.ok(f.tabStrip(1).indexOf(id) < f.tabStrip(1).indexOf(work.tab_id));
    assert.deepEqual(f.groups.get(work.group_id), group);
    assert.deepEqual(f.executor.state("remote"), authority);
    assert.equal(f.tabs.get(work.tab_id).url, workUrl);
    for (const frame of f.presentationFrames.slice(frames)) {
        const tab = frame.tabs.find(tab => tab.id === id);
        if (tab?.active) {
            assert.equal(tab.groupId, -1);
            assert.equal(tab.url, payload.url, "no active preparation document precedes the destination request");
        }
        assert.equal(frame.groups.find(item => item.id === work.group_id).collapsed, true);
    }
    noDesktopHandshake(f, start); noVisibleStaging(f, frames);
}]);
cases.push(["web open remains personal while the global automation toggle is OFF", async () => {
    const f = fresh(); f.local[AUTOMATION_KEY] = false; await f.ready();
    assert.equal(await f.executor.ordinaryOpen(request("automation-off")), "applied");
    assert.equal(f.local[AUTOMATION_KEY], false);
    assert.equal(f.executor.recordFor("remote"), undefined);
    noDesktopHandshake(f);
}]);
for (const intervention of ["cancel", "navigate ABA", "pin ABA", "group ABA", "activate ABA", "move ABA", "detach ABA", "closed"]) {
    cases.push([`${intervention} during web isolation prevents a late destination request`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`isolate-${intervention}`);
        const entered = deferred(); const release = deferred(); let id;
        f.hooks["tabs.ungroup.after"] = async (_result, ids) => {
            id = ids[0]; entered.resolve(); await release.promise;
        };
        const pending = f.executor.ordinaryOpen(payload);
        try {
            await entered.promise;
            const tab = f.tabs.get(id);
            if (intervention === "cancel") f.executor.cancelOrdinaryOpen({version: 1,
                operation: "ordinary_open_cancel", request_id: payload.request_id});
            if (["navigate ABA", "pin ABA", "group ABA"].includes(intervention)) {
                const key = intervention === "navigate ABA" ? "url" : intervention === "pin ABA" ? "pinned" : "groupId";
                const initial = tab[key];
                for (const value of [key === "url" ? "https://human.test/not-requested" : key === "pinned" ? true : 90, initial]) {
                    tab[key] = value;
                    await Promise.all(f.api.tabs.onUpdated.emit(id, {[key]: value}, clone(tab)));
                }
            }
            if (intervention === "activate ABA") {
                await f.api.tabs.update(id, {active: true}); await f.api.tabs.update(1, {active: true});
            }
            if (intervention === "move ABA") {
                await f.api.tabs.move(id, {index: 0}); await f.api.tabs.move(id, {index: -1});
            }
            if (intervention === "detach ABA") {
                await Promise.all(f.api.tabs.onDetached.emit(id, {oldWindowId: 1}));
                await Promise.all(f.api.tabs.onAttached.emit(id, {newWindowId: 1}));
            }
            if (intervention === "closed") {
                f.tabs.delete(id); await Promise.all(f.api.tabs.onRemoved.emit(id, {windowId: 1}));
            }
        } finally { release.resolve(); }
        assert.equal(await pending, "unknown");
        assert.equal(f.calls.some(call => call.name === "tabs.update" && call.args[1].url), false);
        noDesktopHandshake(f);
        assert.equal(f.local[WAL_KEY].ordinary[payload.request_id].completion, "unknown");
    }]);
}
for (const failure of ["completion lost", "worker restart", "navigation unknown"]) {
    cases.push([`${failure} only redelivers the original outcome and cannot repeat a web open`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`retry-${failure}`);
        if (failure === "completion lost") {
            f.hooks["native.ordinary_open_completion"] = async () => { throw new Error("receipt lost"); };
        }
        if (failure === "navigation unknown") {
            f.hooks["tabs.update.after"] = async (_result, _id, update) => {
                if (update.url) throw new Error("destination acknowledgement lost");
            };
        }
        if (failure === "completion lost") await assert.rejects(f.executor.ordinaryOpen(payload), /receipt lost/);
        else assert.equal(await f.executor.ordinaryOpen(payload), failure === "navigation unknown" ? "unknown" : "applied");
        const count = effects(f).length; delete f.hooks["native.ordinary_open_completion"];
        if (failure === "worker restart") { await f.restart(); await f.emitRemote(REMOTE); }
        assert.equal(await f.executor.ordinaryOpen(payload), failure === "navigation unknown" ? "unknown" : "applied");
        assert.equal(effects(f).length, count);
        noDesktopHandshake(f);
    }]);
}
for (const after of ["pending destination", "blank", "other extension", "unrelated http"]) {
    cases.push([`an uncommitted navigation followed by ${after} has an exact outcome`, async () => {
        const f = fresh(); await f.ready(); const payload = request(`pending-${after}`); let id;
        f.hooks["tabs.update.after"] = async (result, tabId, update) => {
            if (!update.url) return;
            id = tabId;
            result.url = "about:blank"; result.pendingUrl = payload.url;
            Object.assign(f.tabs.get(id), {url: "about:blank", pendingUrl: payload.url});
        };
        f.hooks["tabs.get"] = async tabId => {
            if (tabId !== id || after === "pending destination") return;
            const tab = f.tabs.get(id); delete tab.pendingUrl;
            tab.url = after === "blank" ? "about:blank" : after === "other extension" ?
                "chrome-extension://extension/ordinary-staging.html#unrelated" : "https://human.test/unconfirmed-redirect";
        };
        const expected = after === "pending destination" ? "applied" : "unknown";
        assert.equal(await f.executor.ordinaryOpen(payload), expected, f.executor.lastError);
        const count = effects(f).length;
        assert.equal(await f.executor.ordinaryOpen(payload), expected);
        assert.equal(effects(f).length, count);
        noDesktopHandshake(f);
    }]);
}
cases.push(["a confirmed destination may redirect without reissuing navigation", async () => {
    const f = fresh(); await f.ready(); const payload = request("confirmed-redirect"); let id;
    f.hooks["tabs.update.after"] = async (_result, tabId, update) => {
        if (update.url) { id = tabId; f.tabs.get(id).url = "https://www.qq.com/redirected"; }
    };
    assert.equal(await f.executor.ordinaryOpen(payload), "applied", f.executor.lastError);
    assert.equal(f.tabs.get(id).url, "https://www.qq.com/redirected");
    assert.equal(f.calls.filter(call => call.name === "tabs.update").length, 1);
    noDesktopHandshake(f);
}]);
cases.push(["a deadline during isolation prevents late navigation after its callback returns", async () => {
    const f = fresh(); await f.ready(); const payload = request("expired-isolation");
    const entered = deferred(); const release = deferred(); const now = Date.now;
    f.hooks["tabs.ungroup.after"] = async () => { entered.resolve(); await release.promise; };
    const pending = f.executor.ordinaryOpen(payload);
    try {
        await entered.promise;
        Date.now = () => payload.deadline_ms + 1;
        release.resolve();
        assert.equal(await pending, "unknown");
    } finally { Date.now = now; release.resolve(); }
    assert.equal(f.calls.some(call => call.name === "tabs.update"), false);
    noDesktopHandshake(f);
}]);
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
