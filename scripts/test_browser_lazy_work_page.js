#!/usr/bin/env node
const {assert, clone, deferred, fixture, execute, runCases, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const {AUTOMATION_KEY, WAL_KEY, CONSENT_KEY} =
    require("../chromium/external-links/extension/workspace.js");
const fresh = () => fixture({globalAutomation: true});
const record = f => f.executor.recordFor("remote");
const calls = (f, name) => f.calls.filter(call => call.name === name);
const mutations = f => f.calls.filter(call =>
    /^(tabs\.(create|group|ungroup|update|remove)|tabGroups\.update|debugger\.|windows\.(create|update|remove))/.test(call.name));
const identity = value => clone({page: value.page, enable_id: value.enable_id,
    revision: value.revision, browser_boot_id: value.browser_boot_id});
const navigate = (f, url) => execute(f, {command: {command: "navigate", page: clone(record(f).page), url}});
const completion = (f, payload) => calls(f, "native.executor_completion")
    .filter(call => call.args[0].effect_id === payload.effect_id).at(-1)?.args[0];
function reserved(f) {
    assert.ok(f.executor.recordEnabledAgreed("remote"));
    assert.equal(record(f).tab_id, null);
    assert.equal(record(f).group_id, null);
}
const cases = [
    ["three Chrome restarts without Execute reserve fresh identities without physical tabs", async () => {
        const f = fresh(); await f.ready();
        const seen = new Set(); const pages = new Set(); const count = f.tabs.size;
        for (let boot = 0; boot < 4; boot++) {
            reserved(f);
            assert.equal(seen.has(record(f).enable_id), false); seen.add(record(f).enable_id);
            assert.equal(pages.has(record(f).page.page_id), false); pages.add(record(f).page.page_id);
            assert.equal(f.tabs.size, count);
            assert.deepEqual(mutations(f), []);
            assert.deepEqual(f.executor.popupState().remotes.map(row => !!row.creation_unknown), [false]);
            if (boot < 3) { await f.restartBrowser(); await f.emitRemote(REMOTE); }
        }
    }],
    ["matching personal blank and marker group members never become work pages", async () => {
        const f = fresh();
        f.tabs.get(1).url = "about:blank"; f.tabs.get(1).groupId = 70;
        f.tabs.set(3, {id: 3, windowId: 1, active: false, groupId: 70,
            url: "about:blank#abush-work-restored"});
        f.syncGroups(); Object.assign(f.groups.get(70), {title: "Abush · local", collapsed: true});
        const personal = [1, 3].map(id => clone(f.tabs.get(id))); const group = clone(f.groups.get(70));
        await f.ready(); reserved(f); assert.deepEqual(mutations(f), []);
        await f.materialize();
        assert.ok(![1, 3].includes(record(f).tab_id));
        assert.deepEqual([1, 3].map(id => f.tabs.get(id)), personal);
        assert.deepEqual(f.groups.get(70), group);
        for (const id of [1, 3]) assert.equal(f.executor.ownsTab(id), false);
    }],
    ["first authenticated navigation creates one inactive blank and preserves logical authorization", async () => {
        const f = fresh(); await f.ready(); const before = identity(record(f));
        const payload = navigate(f, "https://first.test/exact?q=1#fragment");
        await f.emitNative(payload);
        assert.deepEqual(identity(record(f)), before);
        assert.equal(calls(f, "tabs.create").length, 1);
        assert.equal(calls(f, "tabs.create")[0].args[0].windowId, 1);
        assert.equal(calls(f, "tabs.create")[0].args[0].active, false);
        assert.match(calls(f, "tabs.create")[0].args[0].url, /^about:blank(?:#abush-work-.*)?$/);
        assert.equal(calls(f, "tabs.group").length, 1);
        assert.equal(calls(f, "debugger.sendCommand").length, 1);
        assert.deepEqual(calls(f, "debugger.sendCommand")[0].args,
            [{tabId: record(f).tab_id}, "Page.navigate", {url: payload.command.url}]);
        assert.equal(f.tabs.get(record(f).tab_id).active, false);
        assert.equal(f.tabs.get(1).active, true);
        assert.equal(completion(f, payload).completion, "applied");
        const second = navigate(f, "https://second.test/"); await f.emitNative(second);
        assert.deepEqual(identity(record(f)), before);
        assert.equal(calls(f, "tabs.create").length, 1);
        assert.equal(calls(f, "tabs.group").length, 1);
        assert.equal(calls(f, "debugger.sendCommand").length, 2);
        assert.equal(f.tabs.get(record(f).tab_id).url, second.command.url);
    }],
    ...[false, true].map(used => [`worker restart ${used ? "after use" : "before use"} preserves exact reservation`, async () => {
        const f = fresh(); await f.ready();
        if (used) await f.materialize();
        const before = identity(record(f)); const tab = record(f).tab_id; const start = mutations(f).length;
        await f.restart(); await f.emitRemote(REMOTE);
        assert.ok(record(f).revision > before.revision);
        assert.deepEqual({...identity(record(f)), revision: before.revision}, before);
        assert.equal(record(f).tab_id, tab);
        assert.equal(mutations(f).length, start);
        if (!used) reserved(f);
        await f.emitNative(navigate(f, "https://after-worker.test/"));
        assert.equal(calls(f, "tabs.create").length, 1);
        if (used) assert.equal(record(f).tab_id, tab);
    }]),
    ["exact duplicate effect after worker restart only redelivers original serialized receipt", async () => {
        const f = fresh(); await f.ready(); const payload = execute(f);
        f.hooks["native.executor_completion"] = async () => { throw new Error("receipt lost"); };
        await f.emitNative(payload); const receipt = JSON.stringify(completion(f, payload));
        delete f.hooks["native.executor_completion"];
        const before = mutations(f).length;
        await f.restart(); await f.emitRemote(REMOTE);
        await f.emitNative({...clone(payload), request_id: "retry-request"});
        assert.equal(mutations(f).length, before);
        assert.equal(JSON.stringify(completion(f, payload)), receipt);
        assert.ok(calls(f, "native.executor_completion").length >= 2);
    }],
    ["concurrent first calls through an alias serialize on reservation rather than mutable key", async () => {
        const f = fresh(); await f.ready(); const entered = deferred(); const release = deferred();
        f.hooks["tabs.create.after"] = async () => { entered.resolve(); await release.promise; };
        const first = navigate(f, "https://first-concurrent.test/");
        const running = f.emitNative(first); await entered.promise;
        const alias = {...REMOTE, remote_key: "alias", connection_id: "alias-connection"};
        await f.emitRemote(alias);
        const next = f.executor.recordFor("alias");
        const second = execute(f, {remote_key: "alias", connection_id: alias.connection_id,
            authorization: {browser_boot_id: next.browser_boot_id, enable_id: next.enable_id, revision: next.revision},
            command: {command: "navigate", page: clone(next.page), url: "https://second-concurrent.test/"}});
        const queued = f.emitNative(second);
        try { assert.equal(calls(f, "tabs.create").length, 1); } finally { release.resolve(); }
        await Promise.all([running, queued]);
        assert.equal(calls(f, "tabs.create").length, 1);
        assert.equal(calls(f, "tabs.group").length, 1);
        assert.equal(calls(f, "debugger.sendCommand").length, 2);
        assert.deepEqual(calls(f, "debugger.sendCommand").map(call => call.args[2].url),
            [first.command.url, second.command.url]);
    }],
    ...["page", "revision", "connection", "fence"].map(stale => [
        `unauthenticated first Execute with stale ${stale} cannot create a page`, async () => {
            const f = fresh(); await f.ready(); const payload = execute(f);
            if (stale === "page") payload.command.page.page_id = "personal";
            if (stale === "revision") payload.authorization.revision++;
            if (stale === "connection") payload.connection_id = "unrelated";
            if (stale === "fence") payload.executor_fence.epoch++;
            await f.emitNative(payload); reserved(f);
            assert.deepEqual(mutations(f), []);
            assert.notEqual(completion(f, payload).completion, "applied");
        },
    ]),
    ["Stop persists OFF and no later boot publishes a reservation or creates a tab", async () => {
        const f = fresh(); await f.ready(); assert.equal((await f.send({control: "stop"})).ok, true);
        assert.equal(f.local[AUTOMATION_KEY], false);
        const start = f.calls.length;
        await f.restartBrowser(); await f.emitRemote(REMOTE); await f.executor.reconcileAll();
        assert.equal(f.executor.automationOn, false);
        assert.equal(f.calls.slice(start).some(call => call.name === "native.workspace_state" &&
            call.args[0].state.phase === "enabled"), false);
        assert.deepEqual(mutations(f), []);
    }],
    ["no normal window reports unavailable and a late window does not eagerly create", async () => {
        const f = fresh(); f.windows.delete(1); f.tabs.delete(1);
        await f.ready(); assert.deepEqual(mutations(f), []);
        assert.equal(f.executor.recordEnabledAgreed("remote"), false);
        assert.ok(f.executor.lastError || f.executor.popupState().error ||
            f.executor.popupState().remotes.some(row => row.error));
        f.windows.set(3, {id: 3, type: "normal", focused: true});
        await Promise.all(f.api.windows.onCreated.emit(clone(f.windows.get(3))));
        await f.executor.reconcileAll(); assert.deepEqual(mutations(f), []);
        await f.emitNative(execute(f)); assert.equal(calls(f, "tabs.create").length, 1);
    }],
    ["duplicate native and Chrome reconciliation events cannot materialize a reservation", async () => {
        const f = fresh(); await f.ready(); const before = identity(record(f));
        for (let index = 0; index < 3; index++) {
            await f.emitRemote(REMOTE);
            await Promise.all(f.api.windows.onCreated.emit(clone(f.windows.get(1))));
            await Promise.all(f.api.tabs.onCreated.emit(clone(f.tabs.get(1))));
            await Promise.all(f.api.tabs.onUpdated.emit(1, {url: "about:blank"}, clone(f.tabs.get(1))));
            await f.executor.reconcileAll();
        }
        reserved(f); assert.deepEqual(identity(record(f)), before); assert.deepEqual(mutations(f), []);
    }],
];
for (const stage of ["before intent save", "after intent save", "raw create", "raw group", "consent"]) {
    for (const action of ["Stop", "authority", "cancel"]) cases.push([
        `${action} at ${stage} fences raw late callbacks and all later navigation`, async () => {
            const f = fresh(); await f.ready();
            const entered = deferred(); const release = deferred(); let held = false;
            const hook = stage === "before intent save" ? "local.set" :
                stage === "after intent save" ? "local.set.after" :
                stage === "raw create" ? "tabs.create.after" :
                stage === "raw group" ? "tabs.group.after" : "session.set";
            f.hooks[hook] = async (value, args) => {
                const saved = stage === "after intent save" ? args : value;
                if (held) return;
                if (stage.includes("intent save") &&
                    !Object.values(saved[WAL_KEY]?.creations || {}).some(entry => entry.phase === "unknown")) return;
                if (stage === "consent" &&
                    !JSON.stringify(saved[CONSENT_KEY] || {}).includes('"materialized"')) return;
                held = true; entered.resolve(); await release.promise;
            };
            const payload = execute(f); const running = f.emitNative(payload);
            let changing;
            try {
                await entered.promise;
                if (action === "Stop") {
                    changing = f.executor.setAutomation(false);
                    assert.equal(f.executor.automationOn, false);
                } else if (action === "authority") {
                    changing = f.emitRemote({...REMOTE, executor_fence: {executor_id: "replacement", epoch: 2}});
                } else {
                    changing = f.executor.cancel({remote_key: payload.remote_key,
                        connection_id: payload.connection_id, principal_id: payload.principal_id,
                        request_id: "cancel-at-barrier", effect_id: payload.effect_id,
                        executor_fence: clone(payload.executor_fence)});
                    assert.ok([...f.executor.operations.values()].some(op => op.cancelled));
                }
            } finally { release.resolve(); }
            await Promise.all([running, changing]); delete f.hooks[hook];
            assert.equal(calls(f, "debugger.sendCommand").length, 0);
            assert.equal(calls(f, "debugger.attach").length, 0);
            assert.notEqual(completion(f, payload).completion, "applied");
            if (stage.includes("intent save")) assert.equal(calls(f, "tabs.create").length, 0);
            if (stage === "raw create") assert.equal(calls(f, "tabs.group").length, 0);
            if (stage === "raw group") assert.equal(calls(f, "tabGroups.update").length, 0);
            const before = mutations(f).length; const receipt = JSON.stringify(completion(f, payload));
            await f.emitNative(clone(payload));
            assert.equal(mutations(f).length, before);
            assert.equal(JSON.stringify(completion(f, payload)), receipt);
        },
    ]);
}
for (const stage of ["create", "group", "navigate"]) cases.push([
    `unknown ${stage} never reruns original effects or recreates for a separate request after restart`, async () => {
        const f = fresh(); await f.ready();
        const hook = stage === "create" ? "tabs.create.after" :
            stage === "group" ? "tabs.group.after" : "debugger.sendCommand.after";
        f.hooks[hook] = async () => { throw new Error("effect applied but receipt lost"); };
        const payload = execute(f); await f.emitNative(payload); delete f.hooks[hook];
        const receipt = JSON.stringify(completion(f, payload));
        assert.notEqual(completion(f, payload).completion, "applied");
        const before = mutations(f).length; const creates = calls(f, "tabs.create").length;
        await f.restart(); await f.emitRemote(REMOTE);
        await f.emitNative(clone(payload));
        assert.equal(JSON.stringify(completion(f, payload)), receipt);
        assert.equal(mutations(f).length, before);
        if (stage !== "navigate") {
            await f.emitNative(execute(f));
            assert.equal(calls(f, "tabs.create").length, creates);
            assert.equal(mutations(f).length, before);
            await f.restartBrowser(); await f.emitRemote(REMOTE);
            await f.emitNative(execute(f));
            assert.equal(calls(f, "tabs.create").length, creates);
            assert.equal(mutations(f).length, before);
        }
    },
]);
for (const restart of [false, true]) cases.push([
    `abandonment survives disabled consent failure ${restart ? "after worker restore" : "in live worker"}`, async () => {
        const f = fresh(); await f.ready();
        const oldEnable = record(f).enable_id;
        f.hooks["tabs.group.after"] = async () => { throw new Error("group receipt lost"); };
        const original = execute(f); await f.emitNative(original); delete f.hooks["tabs.group.after"];
        const partial = record(f).tab_id; const oldTab = clone(f.tabs.get(partial));
        assert.equal(record(f).preparation, "materializing");
        f.hooks["session.set"] = async () => { throw new Error("consent unavailable"); };
        await f.executor.recoverPreparation("remote", true).catch(() => {});
        assert.ok(Object.values(f.local[WAL_KEY].creations).some(entry => entry.phase === "abandoned"));
        delete f.hooks["session.set"];
        if (restart) { await f.restart(); await f.emitRemote(REMOTE); }
        else await f.executor.reconcileAll();
        reserved(f); assert.notEqual(record(f).enable_id, oldEnable);
        await f.emitNative(execute(f));
        assert.notEqual(record(f).tab_id, partial);
        assert.equal(calls(f, "tabs.create").length, 2);
        assert.equal(calls(f, "debugger.sendCommand").length, 1);
        assert.deepEqual(f.tabs.get(partial), oldTab);
        const count = mutations(f).length; await f.emitNative(original);
        assert.equal(mutations(f).length, count);
    },
]);
for (const boundary of ["before", "after"]) cases.push([
    `abandonment WAL failure ${boundary} commit stays blocked until durable recovery`, async () => {
        const f = fresh(); await f.ready();
        f.hooks["tabs.group.after"] = async () => { throw new Error("group receipt lost"); };
        await f.emitNative(execute(f)); delete f.hooks["tabs.group.after"];
        const partial = record(f).tab_id; const oldEnable = record(f).enable_id;
        const hook = boundary === "before" ? "local.set" : "local.set.after";
        f.hooks[hook] = async (value, args) => {
            const saved = boundary === "before" ? value : args;
            if (Object.values(saved[WAL_KEY]?.creations || {}).some(entry => entry.phase === "abandoned")) {
                throw new Error("abandonment receipt lost");
            }
        };
        assert.equal((await f.executor.recoverPreparation("remote", true)).ok, false);
        delete f.hooks[hook];
        await f.executor.reconcileAll();
        assert.equal(f.executor.recordEnabledAgreed("remote"), false);
        await f.restart(); await f.emitRemote(REMOTE);
        if (boundary === "before") {
            assert.equal(f.executor.recordEnabledAgreed("remote"), false);
            await f.executor.recoverPreparation("remote", true);
        }
        reserved(f); assert.notEqual(record(f).enable_id, oldEnable);
        await f.emitNative(execute(f));
        assert.notEqual(record(f).tab_id, partial);
        assert.equal(calls(f, "tabs.create").length, 2);
        assert.equal(calls(f, "debugger.sendCommand").length, 1);
    },
]);
for (const reopen of [false, true]) cases.push([
    `unused reservation follows ${reopen ? "last normal window reopening" : "another existing normal window"}`, async () => {
        const f = fresh(); await f.ready(); const before = identity(record(f));
        if (!reopen) f.windows.set(3, {id: 3, type: "normal", focused: false});
        f.windows.delete(1); f.tabs.delete(1);
        await f.executor.reconcileAll();
        if (reopen) {
            assert.equal((await f.executor.setAutomation(false)).ok, true);
            await f.executor.setAutomation(true);
            f.windows.set(3, {id: 3, type: "normal", focused: true});
            await Promise.all(f.api.windows.onCreated.emit(clone(f.windows.get(3))));
            await f.executor.reconcileAll();
        }
        assert.deepEqual(mutations(f), []);
        assert.equal(record(f).enable_id, before.enable_id);
        assert.deepEqual(record(f).page, before.page);
        if (!reopen) assert.equal(record(f).revision, before.revision);
        assert.ok(f.executor.recordEnabledAgreed("remote"));
        await f.emitNative(execute(f));
        assert.equal(calls(f, "tabs.create").length, 1);
        assert.equal(calls(f, "tabs.create")[0].args[0].windowId, 3);
        assert.equal(calls(f, "debugger.sendCommand").length, 1);
        assert.equal(calls(f, "windows.create").length, 0);
        assert.equal(calls(f, "windows.update").length, 0);
    },
]);
for (const action of ["cancel", "Stop"]) cases.push([
    `${action} before Chrome create leaves durable reusable untouched reservation`, async () => {
        const f = fresh(); await f.ready(); const before = identity(record(f));
        const entered = deferred(); const release = deferred(); let held = false;
        f.hooks["local.set.after"] = async (_, saved) => {
            if (!held && Object.values(saved[WAL_KEY]?.creations || {}).some(entry => entry.phase === "unknown")) {
                held = true; entered.resolve(); await release.promise;
            }
        };
        const original = execute(f); const running = f.emitNative(original); let changing;
        try {
            await entered.promise;
            changing = action === "Stop" ? f.executor.setAutomation(false) : f.executor.cancel({
                remote_key: original.remote_key, connection_id: original.connection_id,
                principal_id: original.principal_id, request_id: "cancel-before-create",
                effect_id: original.effect_id, executor_fence: clone(original.executor_fence)});
        } finally { release.resolve(); }
        await Promise.all([running, changing]); delete f.hooks["local.set.after"];
        assert.equal(calls(f, "tabs.create").length, 0);
        assert.equal(completion(f, original).completion, "not_applied");
        assert.ok(Object.values(f.local[WAL_KEY].creations).some(entry => entry.phase === "not_applied"));
        if (action === "Stop") {
            assert.equal(f.local[AUTOMATION_KEY], false);
            assert.equal(f.executor.recordEnabledAgreed("remote"), false);
            assert.equal((await f.executor.setAutomation(true)).ok, true);
        } else assert.deepEqual(identity(record(f)), before);
        reserved(f);
        await f.restart(); await f.emitRemote(REMOTE); reserved(f);
        await f.emitNative(execute(f));
        assert.equal(calls(f, "tabs.create").length, 1);
        assert.equal(calls(f, "debugger.sendCommand").length, 1);
        const count = mutations(f).length; await f.emitNative(original);
        assert.equal(mutations(f).length, count);
        assert.equal(completion(f, original).completion, "not_applied");
    },
]);
for (const stage of ["intent timeout", "not_applied failure", "raw timeout", "crashed intent"]) cases.push([
    `${stage} distinguishes live no-dispatch proof from unknown Chrome effects`, async () => {
        const f = fresh(); await f.ready();
        const entered = deferred(); const release = deferred(); let held = false;
        if (stage === "raw timeout") {
            f.hooks["tabs.create.after"] = async () => { entered.resolve(); await release.promise; };
        } else {
            f.hooks["local.set.after"] = async (_, saved) => {
                if (!held && Object.values(saved[WAL_KEY]?.creations || {}).some(entry => entry.phase === "unknown")) {
                    held = true; entered.resolve(); await release.promise;
                }
            };
        }
        if (stage === "not_applied failure") f.hooks["local.set"] = async saved => {
            if (Object.values(saved[WAL_KEY]?.creations || {}).some(entry => entry.phase === "not_applied")) {
                throw new Error("no-dispatch proof not saved");
            }
        };
        const originalApi = f.executor.api.bind(f.executor); let journalWrites = 0;
        f.executor.api = (task, label) => {
            if (label === "persist effect journal") journalWrites++;
            if (stage === "intent timeout" && label === "persist effect journal" && journalWrites === 2) {
                return entered.promise.then(() => { throw new Error("intent write timed out"); });
            }
            if (stage === "raw timeout" && label === "create inactive work tab") {
                task().catch(() => {});
                return entered.promise.then(() => { throw new Error("raw create timed out"); });
            }
            return originalApi(task, label);
        };
        const original = execute(f); const running = f.emitNative(original);
        await entered.promise;
        let changing;
        if (stage === "not_applied failure") changing = f.executor.cancel({
            remote_key: original.remote_key, connection_id: original.connection_id,
            principal_id: original.principal_id, request_id: "cancel-proof-failure",
            effect_id: original.effect_id, executor_fence: clone(original.executor_fence)});
        if (stage === "crashed intent") {
            const wal = clone(f.local[WAL_KEY]); const consent = clone(f.session[CONSENT_KEY]);
            // Revoke the old worker before releasing its continuation; restore crash snapshots below.
            f.executor.bumpControl("remote");
            release.resolve(); await running;
            f.local[WAL_KEY] = wal; f.session[CONSENT_KEY] = consent;
            await f.restart(); await f.emitRemote(REMOTE);
        } else {
            if (stage === "raw timeout") await running;
            release.resolve(); await running; await changing;
        }
        delete f.hooks["local.set"]; delete f.hooks["local.set.after"]; delete f.hooks["tabs.create.after"];
        if (stage !== "crashed intent") f.executor.api = originalApi;
        if (stage === "intent timeout") {
            reserved(f);
            assert.ok(Object.values(f.local[WAL_KEY].creations).some(entry => entry.phase === "not_applied"));
            await f.emitNative(execute(f));
            assert.equal(calls(f, "tabs.create").length, 1);
            assert.equal(calls(f, "debugger.sendCommand").length, 1);
        } else {
            await f.executor.reconcileAll();
            await f.emitNative(execute(f));
            assert.equal(calls(f, "debugger.sendCommand").length, 0);
            await f.restart(); await f.emitRemote(REMOTE);
            await f.emitNative(execute(f));
            assert.equal(calls(f, "tabs.create").length, stage === "raw timeout" ? 1 : 0);
            assert.equal(calls(f, "debugger.sendCommand").length, 0);
            assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
        }
        const count = mutations(f).length; await f.emitNative(original);
        assert.equal(mutations(f).length, count);
    },
]);
for (const state of ["materializing", "missing ledger", "unknown ledger", "abandoned ledger", "legacy"]) cases.push([
    `physical ${state} eligibility is checked on restore and every Execute`, async () => {
        const f = fresh(); await f.ready(); await f.materialize();
        const physical = record(f); const tabId = physical.tab_id;
        if (state === "legacy") delete physical.preparation;
        else if (state === "materializing") physical.preparation = "materializing";
        else {
            for (const [key, entry] of Object.entries(f.executor.creations)) {
                if (entry.enable_id !== physical.enable_id) continue;
                if (state === "missing ledger") delete f.executor.creations[key];
                else entry.phase = state === "unknown ledger" ? "unknown" : "abandoned";
            }
        }
        await f.executor.persistWal(); await f.executor.persistConsent();
        const count = calls(f, "debugger.sendCommand").length;
        await f.emitNative(execute(f));
        assert.equal(calls(f, "debugger.sendCommand").length, count + Number(state === "legacy"));
        if (state === "legacy") await f.executor.detach(physical);
        await f.restart(); await f.emitRemote(REMOTE);
        assert.equal(record(f)?.tab_id === tabId, state === "legacy");
        if (state === "legacy") {
            await f.emitNative(execute(f));
            assert.equal(calls(f, "tabs.create").length, 1);
            assert.equal(calls(f, "debugger.sendCommand").length, count + 2);
        }
    },
]);
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
