#!/usr/bin/env node
const {assert, clone, deferred, fixture, execute, runCases, REMOTE} =
    require("./browser_tab_groups_fixture.js");
const {WAL_KEY, RECEIPTS_KEY, CONSENT_KEY} = require("../chromium/external-links/extension/workspace.js");
const cases = [
    ["exact Execute before state ACK applies once with pending UI", async () => {
        for (const outcome of ["accept", "fail", "malformed", "pause"]) {
            const f = fixture(); await f.ready();
            const entered = deferred(); const gate = deferred();
            f.hooks["native.workspace_state"] = async payload => {
                if (payload.state.phase === "enabled") {
                    entered.resolve(); await gate.promise;
                    if (outcome === "fail") throw new Error("state ACK lost");
                    if (outcome === "malformed") return {type: "workspace_state_accepted", extra: true};
                }
            };
            const enable = f.send({control: "enable", remote_key: "remote"});
            await entered.promise;
            const payload = execute(f);
            const trace = () => [f.executor.state("remote").phase,
                f.executor.popupState().remotes[0].agreed,
                f.calls.filter(c => c.name === "debugger.sendCommand").length];
            assert.deepEqual(trace(), ["enabled", false, 0]);
            assert.equal(f.session[CONSENT_KEY].records[0].phase, "enabled");
            for (const invalid of [
                execute(f, {authorization: {...payload.authorization, revision: payload.authorization.revision - 1}}),
                execute(f, {executor_fence: {executor_id: "old", epoch: 1}}),
                {...execute(f), extra: true},
            ]) await f.emitNative(invalid);
            assert.deepEqual(trace(), ["enabled", false, 0]);
            if (outcome === "pause") await f.executor.control("pause", "remote");
            await f.emitNative(payload);
            await f.emitNative(payload);
            const expected = outcome === "pause" ? "not_applied" : "applied";
            assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, expected);
            assert.deepEqual(trace(), outcome === "pause" ? ["paused", false, 0] : ["enabled", false, 1]);
            assert.equal(f.calls.filter(c => c.name === "native.executor_completion" &&
                c.args[0].effect_id === payload.effect_id).at(-1).args[0].completion, expected);
            gate.resolve(); const response = await enable;
            assert.equal(response.ok, outcome === "accept");
            assert.deepEqual(trace(), outcome === "accept" ? ["enabled", true, 1] :
                ["paused", false, outcome === "pause" ? 0 : 1]);
            assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, expected);
        }
    }],
    ["Execute cannot bypass pending local consent persistence", async () => {
        const f = fixture(); await f.ready(); const entered = deferred(); const gate = deferred();
        f.hooks["session.set"] = async value => {
            if (value[CONSENT_KEY]?.records[0]?.phase === "enabled") {
                entered.resolve(); await gate.promise;
            }
        };
        const enable = f.send({control: "enable", remote_key: "remote"}); await entered.promise;
        await f.emitNative(execute(f));
        assert.equal(f.calls.filter(c => c.name === "debugger.sendCommand").length, 0);
        assert.equal(f.calls.filter(c => c.name === "native.workspace_state" &&
            c.args[0].state.phase === "enabled").length, 0);
        gate.resolve(); assert.equal((await enable).ok, true);
    }],
    ["native reorder insert and ungroup preserve exact tab only", async () => {
        const f = fixture(); await f.ready(); const record = await f.enable();
        const original = clone(record.page);
        await f.api.tabs.group({tabIds: [1], groupId: record.group_id});
        await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: false});
        f.api.tabs.onMoved.emit(record.tab_id, {windowId: 1, fromIndex: 2, toIndex: 0});
        await f.api.tabs.ungroup([record.tab_id]);
        assert.equal(f.executor.ownsTab(record.tab_id), true);
        assert.equal(f.executor.ownsTab(1), false);
        assert.deepEqual(record.page, original);
        assert.equal(await f.executor.execute(execute(f)), "applied");
        assert.equal(f.executor.revokedTabs.size, 0);
    }],
    ["disabled at bootstrap and peer connect", async () => {
        const f = fixture(); await f.ready();
        assert.equal(f.executor.state("remote").phase, "disabled");
        await assert.rejects(f.executor.execute(execute(f)), /disabled/);
        assert.equal(f.calls.filter(call => call.name.startsWith("debugger.")).length, 0);
        assert.equal(f.tabs.size, 2);
    }],
    ["trusted popup routing only", async () => {
        const f = fixture(); await f.ready();
        for (const sender of [{...f.trusted, id: "external"}, {...f.trusted, tab: {id: 1}},
            {...f.trusted, tab: null}, {...f.trusted, tab: false}, null,
            {...f.trusted, url: f.trusted.url + "#fake"}, {id: f.trusted.id, url: "https://content/"}]) {
            assert.equal((await f.send({control: "enable", remote_key: "remote"}, sender)).ok, false);
        }
        for (const message of [null, [], {}, {control: null, remote_key: "remote"}]) {
            assert.equal((await f.send(message)).ok, false);
        }
        assert.equal(f.tabs.size, 2);
        await f.enable();
        assert.equal(f.tabs.size, 3);
    }],
    ["exact inactive tab grouped without window effects", async () => {
        const f = fixture(); await f.ready();
        const ordinary = clone(f.tabs.get(1)); const popup = clone(f.tabs.get(2));
        const record = await f.enable();
        assert.deepEqual(f.calls.find(call => call.name === "tabs.create").args[0],
            {windowId: 1, url: `about:blank#abush-work-${record.enable_id}`,
                active: false, index: 0x7fffffff});
        assert.deepEqual(f.calls.find(call => call.name === "tabs.group").args[0], {tabIds: [record.tab_id]});
        assert.deepEqual(f.calls.find(call => call.name === "tabGroups.update").args,
            [record.group_id, {title: "Abush · local", color: "purple", collapsed: true}]);
        assert.equal(record.agreed, true);
        await f.executor.execute(execute(f));
        assert.deepEqual(f.tabs.get(1), ordinary); assert.deepEqual(f.tabs.get(2), popup);
        assert.equal(f.tabs.get(record.tab_id).active, false);
        assert.equal(f.calls.filter(call => call.name === "windows.create" || call.name === "tabs.update").length, 0);
    }],
    ["group members and popup are never owned", async () => {
        const f = fixture(); await f.ready(); const record = await f.enable();
        f.tabs.get(1).groupId = record.group_id;
        assert.equal(f.executor.ownsTab(1), false); assert.equal(f.executor.ownsTab(2), false);
        const response = await f.send({query: "abush_owned_tab"}, {...f.trusted, tab: {id: 1, windowId: 1}});
        assert.equal(response.owned, false);
        const routed = await f.send({url: "https://external.test/"}, {...f.trusted, tab: {id: 1, windowId: 1}});
        assert.equal(routed.route, true);
        await f.executor.control("disconnect", "remote");
        assert.equal(f.tabs.size, 3); assert.equal(f.tabs.get(1).groupId, record.group_id);
    }],
    ["pause closes authorization synchronously and reconnect cannot resume", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        const payload = execute(f); const gate = deferred(); const entered = deferred();
        f.hooks["session.set"] = async () => { entered.resolve(); await gate.promise; };
        const pause = f.executor.control("pause", "remote");
        assert.equal(f.executor.state("remote").phase, "paused"); await entered.promise;
        const before = f.calls.filter(call => call.name.startsWith("debugger.")).length;
        const attempt = f.executor.execute(payload); attempt.catch(() => {});
        gate.resolve(); await pause; await assert.rejects(attempt, /paused/);
        delete f.hooks["session.set"];
        assert.equal(f.calls.filter(call => call.name.startsWith("debugger.")).length, before);
        await f.executor.remoteState({...REMOTE, connection_id: "reconnect"});
        assert.equal(f.executor.state("remote").phase, "paused");
        await f.executor.control("resume", "remote");
        assert.equal(f.executor.state("remote").phase, "enabled");
    }],
    ["takeover during deferred attach prevents navigation", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        const gate = deferred(); const entered = deferred();
        f.hooks["debugger.attach"] = async () => { entered.resolve(); await gate.promise; };
        const pending = f.executor.execute(execute(f)); pending.catch(() => {}); await entered.promise;
        await f.executor.control("takeover", "remote"); gate.resolve();
        await assert.rejects(pending, /revoked|paused/);
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 0);
    }],
    ...["timeout", "pause", "disconnect", "remove"].map(action => [
        `late attachment cleanup remains owned across ${action} and failed detach`, async () => {
            const f = fixture(); await f.ready(); const record = await f.enable();
            const entered = deferred(); const attachGate = deferred();
            f.hooks["debugger.attach"] = async () => { entered.resolve(); await attachGate.promise; };
            const payload = execute(f);
            const pending = f.executor.execute(payload); pending.catch(() => {});
            await entered.promise;
            await assert.rejects(f.api.debugger.detach({tabId: record.tab_id}), /Not attached/);
            if (action === "remove") {
                await f.executor.revokeTab(record.tab_id);
                f.executor.records.delete("remote");
            } else if (action !== "timeout") {
                assert.equal((await f.send({control: action, remote_key: "remote"})).ok, true);
            }
            await assert.rejects(pending, /attach exact work tab debugger timed out/);
            assert.equal(f.executor.operations.size, 0);
            assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "not_applied");
            const token = f.executor.attachments.get(record.tab_id);
            assert.equal(token.pending, true);
            assert.equal(record.attachment, undefined);
            assert.equal(f.executor.popupState().cleanup_pending, true);
            const detachEntered = deferred(); const detachGate = deferred();
            f.hooks["debugger.detach"] = async () => {
                detachEntered.resolve(); await detachGate.promise;
            };
            attachGate.resolve(); await detachEntered.promise;
            assert.equal(f.attachedTabs.has(record.tab_id), true);
            assert.equal(token.pending, false);
            const blocked = f.send({control: ["timeout", "pause"].includes(action) ? "resume" : "enable",
                remote_key: "remote"});
            assert.equal((await blocked).ok, false);
            assert.equal(f.executor.attachments.get(record.tab_id), token);
            assert.equal(f.executor.popupState().remotes[0].cleanup_pending, true);
            if (action === "timeout") {
                await assert.rejects(f.executor.execute(execute(f)), /cleanup is still pending/);
            }
            assert.equal(f.calls.filter(c => c.name === "debugger.sendCommand").length, 0);
            detachGate.reject(new Error("late detach failed"));
            await assert.rejects(token.detaching, /late detach failed/);
            assert.equal(f.executor.attachments.get(record.tab_id), token);
            assert.match(f.executor.popupState().error, /late detach failed/);
            delete f.hooks["debugger.detach"];
            await f.executor.detach(record, token);
            assert.equal(f.executor.attachments.size, 0);
            assert.equal(f.attachedTabs.size, 0);
            assert.equal(f.executor.popupState().cleanup_pending, false);
            if (action === "pause") {
                await f.executor.control("resume", "remote");
                await f.executor.execute(execute(f));
                const newer = record.attachment;
                await f.executor.detach(record, token);
                assert.equal(record.attachment, newer);
                assert.equal(f.attachedTabs.has(record.tab_id), true);
                await f.executor.control("pause", "remote");
                assert.equal(f.attachedTabs.size, 0);
            }
        },
    ]),
    ...[false, true].map(timedOut => [
        `external detach confirmation releases cleanup after ${timedOut ? "timed-out" : "pending"} request rejects`, async () => {
            const f = fixture(); await f.ready(); const record = await f.enable();
            await f.executor.execute(execute(f));
            const entered = deferred(); const gate = deferred();
            f.hooks["debugger.detach"] = async () => { entered.resolve(); await gate.promise; };
            const pause = f.executor.control("pause", "remote"); pause.catch(() => {});
            await entered.promise;
            const token = f.executor.attachments.get(record.tab_id);
            const detaching = token.detaching;
            if (timedOut) await assert.rejects(pause, /detach work tab debugger timed out/);
            f.attachedTabs.delete(record.tab_id);
            f.executor.debuggerDetached(record.tab_id);
            assert.equal(f.executor.attachments.get(record.tab_id), token);
            assert.equal(f.executor.popupState().cleanup_pending, true);
            await assert.rejects(f.executor.execute(execute(f)), /paused/);
            gate.reject(new Error("Not attached"));
            await detaching;
            if (!timedOut) await pause;
            assert.equal(f.executor.attachments.size, 0);
            assert.equal(f.executor.popupState().cleanup_pending, false);
            delete f.hooks["debugger.detach"];
            await f.executor.control("resume", "remote");
            await f.executor.execute(execute(f));
            assert.equal(f.attachedTabs.has(record.tab_id), true);
            await f.executor.control("pause", "remote");
            assert.equal(f.attachedTabs.size, 0);
        },
    ]),
    ["rejected attachment releases obligation and successful attachment is reused", async () => {
        const f = fixture(); await f.ready(); const record = await f.enable();
        f.hooks["debugger.attach"] = async () => { throw new Error("attachment rejected"); };
        await assert.rejects(f.executor.execute(execute(f)), /attachment rejected/);
        assert.equal(f.executor.attachments.size, 0);
        assert.equal(f.attachedTabs.size, 0);
        delete f.hooks["debugger.attach"];
        await f.executor.execute(execute(f));
        await f.executor.execute(execute(f));
        assert.equal(f.calls.filter(c => c.name === "debugger.attach").length, 2);
        assert.equal(record.attached, true);
        await f.executor.control("pause", "remote");
        assert.equal(f.attachedTabs.size, 0);
        assert.equal(f.executor.attachments.size, 0);
    }],
    ["pause after dispatch fences output and records unknown", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        const entered = deferred(); const gate = deferred();
        f.hooks["debugger.sendCommand"] = async () => { entered.resolve(); await gate.promise; };
        const pending = f.executor.execute(payload); pending.catch(() => {}); await entered.promise;
        await f.executor.control("pause", "remote"); gate.resolve(); await assert.rejects(pending, /disabled|paused/);
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "unknown");
    }],
    ["ACK loss resends terminal only", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        f.hooks["native.executor_completion"] = async () => { throw new Error("ACK lost"); };
        await assert.rejects(f.executor.execute(payload), /ACK lost/);
        delete f.hooks["native.executor_completion"];
        await f.executor.execute(payload);
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 1);
        assert.ok(f.local[RECEIPTS_KEY].receipts[f.executor.effectKey(payload)]);
    }],
    ["malformed command gets no fake ACK", async () => {
        const f = fixture(); await f.ready(); await f.enable();
        await assert.rejects(f.executor.execute(execute(f, {authorization: {}})), /Invalid/);
        assert.equal(f.calls.filter(call => call.name === "native.executor_completion").length, 0);
    }],
    ["persistence before dispatch fails not applied", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        let fail = true;
        f.hooks["local.set"] = async value => {
            if (fail && value[WAL_KEY]) { fail = false; throw new Error("disk failed"); }
        };
        await assert.rejects(f.executor.execute(payload), /disk failed/);
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "not_applied");
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 0);
    }],
    ["side-effect API failure is unknown", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        f.hooks["debugger.sendCommand"] = async () => { throw new Error("lost response"); };
        await assert.rejects(f.executor.execute(payload), /lost response/);
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "unknown");
    }],
    ["ordinary exact URL new active ungrouped sibling", async () => {
        const f = fixture(); await f.ready(); const record = await f.enable();
        f.tabs.get(1).active = false; f.tabs.get(record.tab_id).active = true;
        const work = clone(f.tabs.get(record.tab_id));
        const payload = {version: 1, operation: "ordinary_open", focus: true, request_id: "ordinary",
            url: "https://ordinary.test/a?exact=yes#anchor", deadline_ms: Date.now() + 5000};
        assert.equal(await f.executor.ordinaryOpen(payload), "applied");
        const opened = [...f.tabs.values()].filter(tab => tab.url === payload.url);
        assert.equal(opened.length, 1); assert.equal(opened[0].active, true);
        assert.equal(opened[0].groupId, -1); assert.notEqual(opened[0].id, 1);
        assert.equal(f.tabs.get(record.tab_id).url, work.url);
        assert.equal(f.tabs.get(record.tab_id).groupId, work.groupId);
        await f.executor.ordinaryOpen(payload); assert.equal(f.tabs.size, 4);
    }],
    ["trusted refresh lists remotes but never enables", async () => {
        const f = fixture(); await f.ready();
        const response = await f.send({control: "refresh"});
        assert.equal(response.ok, true); assert.equal(response.remotes[0].state.phase, "disabled");
        assert.equal(f.tabs.size, 2);
    }],
    ["invalid canonical completion receipt stays pending", async () => {
        const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
        const original = f.executor.nativeRequest;
        f.executor.nativeRequest = async (service, operation, wire) => {
            const receipt = await original(service, operation, wire);
            return operation === "executor_completion" ? {...receipt, extra: true} : receipt;
        };
        await assert.rejects(f.executor.execute(payload), /Invalid completion receipt/);
        assert.equal(f.executor.receipts[f.executor.effectKey(payload)], undefined);
        f.executor.nativeRequest = original;
        await f.executor.execute(payload);
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, 1);
    }],
    ["resume validates original tab and cannot adopt same URL", async () => {
        const f = fixture(); await f.ready(); const record = await f.enable();
        await f.executor.control("pause", "remote");
        const old = clone(f.tabs.get(record.tab_id)); f.tabs.delete(record.tab_id);
        f.tabs.set(90, {...old, id: 90});
        assert.equal((await f.send({control: "resume", remote_key: "remote"})).ok, false);
        assert.equal(f.executor.records.get("remote").agreed, false);
        assert.equal(f.executor.ownsTab(90), false);
    }],
    ["ordinary cancel before create is not applied", async () => {
        const f = fixture(); await f.ready(); const gate = deferred(); const entered = deferred();
        f.hooks["windows.getAll"] = async () => { entered.resolve(); await gate.promise; };
        const payload = {version: 1, operation: "ordinary_open", focus: true, request_id: "cancel-ordinary",
            url: "https://ordinary.test/", deadline_ms: Date.now() + 5000};
        const pending = f.executor.ordinaryOpen(payload); await entered.promise;
        f.executor.cancelOrdinaryOpen({version: 1, operation: "ordinary_open_cancel", request_id: payload.request_id});
        gate.resolve(); assert.equal(await pending, "not_applied"); assert.equal(f.tabs.size, 2);
    }],
    ["ordinary file uses native exact nonce", async () => {
        const f = fixture(); await f.ready();
        assert.equal(await f.executor.ordinaryOpen({version: 1, operation: "ordinary_open", focus: true,
            request_id: "file", url: "file:///home/example/a.html", deadline_ms: Date.now() + 5000}), "applied");
        assert.equal(f.calls.filter(call => call.name === "native.ordinary_file_navigate").length, 1);
    }],
    ["CDP semantic errors and malformed responses never Applied", async () => {
        for (const response of [{frameId: "frame", errorText: "net::ERR_NAME_NOT_RESOLVED"}, {}]) {
            const f = fixture(); await f.ready(); await f.enable(); const payload = execute(f);
            f.hooks["debugger.sendCommand"] = async () => response;
            await assert.rejects(f.executor.execute(payload), /navigate|ERR|response/i);
            assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "unknown");
            delete f.hooks["debugger.sendCommand"];
            await f.restart(); await f.executor.remoteState(REMOTE);
            assert.equal(await f.executor.execute(payload), "unknown");
            assert.equal(f.calls.filter(c => c.name === "debugger.sendCommand").length, 1);
        }
    }],
    ["ordinary web requests serialize through navigation and queued cancellation", async () => {
        const f = fixture(); await f.ready(); const entered = deferred(); const gate = deferred();
        const payload = id => ({version: 1, operation: "ordinary_open", focus: true, request_id: id,
            url: `https://ordinary.test/${id}`, deadline_ms: Date.now() + 5000});
        f.hooks["tabs.create.after"] = async () => { entered.resolve(); await gate.promise; };
        const a = f.executor.ordinaryOpen(payload("A")); await entered.promise;
        const b = f.executor.ordinaryOpen(payload("B"));
        gate.resolve(); assert.equal(await a, "applied"); assert.equal(await b, "applied");
        const trace = f.calls.filter(c => ["tabs.create", "tabs.update", "native.prepare", "native.activate"].includes(c.name))
            .map(c => c.name);
        assert.deepEqual(trace, ["tabs.create", "tabs.update", "tabs.create", "tabs.update"]);
        delete f.hooks["tabs.create.after"];
        assert.equal(f.calls.some(c => c.name === "tabs.update" && c.args[1].url), false);
        const queued = f.executor.ordinaryOpen(payload("cancelled"));
        f.executor.cancelOrdinaryOpen({version: 1, operation: "ordinary_open_cancel", request_id: "cancelled"});
        assert.equal(await queued, "not_applied");
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 2);
    }],
    ["ordinary rejects malformed file activation and competing web pending URL", async () => {
        for (const mode of ["activation", "pending"]) {
            const f = fixture(); await f.ready();
            if (mode === "activation") f.hooks["native.activate"] = async () => ({});
            else f.hooks["tabs.update.after"] = async result => { f.tabs.get(result.id).pendingUrl = "https://other.test/"; };
            assert.equal(await f.executor.ordinaryOpen({version: 1, operation: "ordinary_open", focus: true,
                request_id: mode, url: mode === "activation" ? "file:///home/amos/ordinary-test.html" : "https://ordinary.test/",
                deadline_ms: Date.now() + 5000}), "unknown");
        }
    }],
    ["ordinary URL canonical pending destination and ambiguous early redirect", async () => {
        for (const mode of ["canonical", "pending", "redirect"]) {
            const f = fixture(); await f.ready();
            const requested = "https://ORDINARY.test:443";
            f.hooks["tabs.create.after"] = async created => {
                const change = mode === "canonical" ? {url: new URL(requested).href} :
                    mode === "pending" ? {url: "about:blank", pendingUrl: new URL(requested).href} :
                        {url: "https://ordinary.test/redirected"};
                Object.assign(created, change);
                Object.assign(f.tabs.get(created.id), change);
            };
            assert.equal(await f.executor.ordinaryOpen({version: 1, operation: "ordinary_open", focus: true,
                request_id: mode, url: requested, deadline_ms: Date.now() + 5000}),
            mode === "redirect" ? "unknown" : "applied");
            if (mode === "redirect") assert.ok([...f.tabs.values()].some(tab => tab.url === "https://ordinary.test/redirected"));
            assert.equal(f.calls.some(c => c.name === "tabs.update" && c.args[1].url), false);
        }
    }],
];
if (require.main === module) runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
module.exports = {cases};
