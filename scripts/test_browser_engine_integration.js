#!/usr/bin/env node
const {assert, clone, deferred, fixture, execute, runCases, REMOTE} = require("./browser_tab_groups_fixture.js");
const {WAL_KEY, AUTOMATION_KEY} = require("../chromium/external-links/extension/workspace.js");
const result = {text: "button [ref=e1]", data: {snapshot_id: "snapshot-1", refs: ["e1"]}, images: []};
function setup(f) {
    f.hooks["debugger.sendCommand"] = async (source, method) => {
        if (method === "Target.getTargetInfo") return {targetInfo: {targetId: "exact", type: "page", url: "about:blank"}};
        return {};
    };
    let sessionId;
    f.hooks["native.engine_open"] = async payload => {
        const received = deferred();
        f.hooks["native.engine_cdp"] = async wire => {
            if (wire.message.method === "Target.attachedToTarget") sessionId = wire.message.params.sessionId;
            if (wire.message.id === 1) received.resolve();
        };
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].phase, "dispatched");
        assert.equal(f.executor.engineCdp({...payload, lease_id: "lease", message: {id: 1,
            method: "Target.setAutoAttach", params: {autoAttach: true, flatten: true, waitForDebuggerOnStart: false}}}), true);
        await received.promise;
        return {lease_id: "lease"};
    };
    f.hooks["native.engine_execute"] = async payload => {
        if (payload.command.action?.type === "click" || payload.command.command === "navigate") {
            const received = deferred();
            f.hooks["native.engine_cdp"] = async wire => { if (wire.message.id === 2) received.resolve(); };
            f.executor.engineCdp({...payload, message: {id: 2, sessionId,
                method: payload.command.command === "navigate" ? "Page.navigate" : "Input.dispatchMouseEvent",
                params: payload.command.command === "navigate" ? {url: payload.command.url} : {type: "mousePressed", x: 1, y: 1}}});
            await received.promise;
        }
        return {ok: true, result: clone(result), error: null};
    };
}
const action = (f, value) => execute(f, {command: {command: "action", page: clone(f.executor.recordFor("remote").page), action: value}});
const cases = [
    ["open on exact attached blank precedes navigate and data reaches durable completion", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        const payload = execute(f); await f.executor.execute(payload);
        const names = f.calls.map(call => call.name);
        assert.ok(names.indexOf("debugger.attach") < names.indexOf("native.engine_open"));
        assert.ok(names.indexOf("native.engine_open") < names.indexOf("native.engine_execute"));
        const completion = f.calls.find(call => call.name === "native.executor_completion").args[0];
        assert.deepEqual(completion.result, result); assert.equal(completion.error, null);
        assert.equal(completion.completion, "applied");
        assert.equal(f.api.debugger.onEvent.listeners.length, 1);
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").every(call =>
            call.args[0].tabId === f.executor.recordFor("remote").tab_id), true);
        await f.executor.setAutomation(false);
        assert.equal(f.api.debugger.onEvent.listeners.length, 0);
    }],
    ["snapshot and click return original output through one journal; worker replay never executes", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        const read = action(f, {type: "snapshot"}); await f.executor.execute(read);
        const click = action(f, {type: "click", snapshot_id: "snapshot-1", ref: "e1"});
        f.hooks["native.executor_completion"] = async () => { throw new Error("lost receipt"); };
        await assert.rejects(f.executor.execute(click), /lost receipt/);
        const saved = JSON.stringify(f.local[WAL_KEY].effects[f.executor.effectKey(click)]);
        assert.equal(f.calls.filter(call => call.name === "native.engine_open").length, 1);
        assert.deepEqual(f.executor.effects[f.executor.effectKey(read)].result, result);
        delete f.hooks["native.executor_completion"];
        const count = f.calls.filter(call => call.name === "native.engine_execute").length;
        await f.executor.setAutomation(false); await f.restart(); await f.emitRemote(REMOTE);
        await f.executor.execute(click);
        assert.equal(f.calls.filter(call => call.name === "native.engine_execute").length, count);
        assert.equal(JSON.stringify(f.local[WAL_KEY].effects[f.executor.effectKey(click)]), saved);
    }],
    ["Stop racing blocked engine execute closes only lease and rejects stale or personal CDP", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        const payload = action(f, {type: "text"}); const entered = deferred(); const gate = deferred();
        f.hooks["native.engine_execute"] = async () => { entered.resolve(); await gate.promise; return {ok: true, result, error: null}; };
        const running = f.executor.execute(payload); running.catch(() => {}); await entered.promise;
        const session = [...f.executor.engineSessions.values()][0];
        const before = f.calls.filter(call => call.name === "native.engine_cdp").length;
        f.api.debugger.onEvent.emit({tabId: 1}, "Network.requestWillBeSent", {secret: "personal"});
        assert.equal(f.calls.filter(call => call.name === "native.engine_cdp").length, before);
        await f.executor.setAutomation(false); await assert.rejects(running, /revoked/);
        assert.equal(f.executor.engineCdp({...payload, lease_id: "lease", message: {id: 3, method: "Runtime.evaluate"}}), false);
        assert.equal(session.bridge.closed, true); assert.equal(f.local[AUTOMATION_KEY], false);
        assert.equal(f.executor.effects[f.executor.effectKey(payload)].completion, "not_applied");
        assert.equal(f.calls.some(call => /^(windows\.(remove|update|create)|tabs\.remove)$/.test(call.name)), false);
        gate.resolve();
    }],
    ["cancellation while engine waits exits without false applied or result", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        const payload = action(f, {type: "wait", text: "later", timeout_ms: 30000});
        const entered = deferred(); const gate = deferred();
        f.hooks["native.engine_execute"] = async () => { entered.resolve(); await gate.promise; return {ok: true, result, error: null}; };
        const running = f.executor.execute(payload); running.catch(() => {}); await entered.promise;
        const cancellation = f.executor.cancel({remote_key: payload.remote_key, connection_id: payload.connection_id,
            principal_id: payload.principal_id, executor_fence: payload.executor_fence,
            effect_id: payload.effect_id, execution_request_id: payload.request_id, request_id: "cancel"});
        await assert.rejects(running, /revoked/); assert.equal(await cancellation, "not_applied");
        const wire = f.calls.find(call => call.name === "native.cancellation_completion").args[0];
        assert.equal("result" in wire, false); assert.equal("error" in wire, false); gate.resolve();
    }],
    ["Stop during engine initialization fences late reservation before any CDP", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); await f.ready();
        const entered = deferred(); const gate = deferred();
        const payload = execute(f);
        f.hooks["native.engine_open"] = async () => { entered.resolve(); await gate.promise; return {lease_id: "late"}; };
        const running = f.executor.execute(payload); running.catch(() => {}); await entered.promise;
        await f.executor.setAutomation(false); await assert.rejects(running, /revoked/);
        assert.equal(f.executor.engineLease({...payload, lease_id: "late"}), false);
        gate.resolve(); await Promise.resolve(); await Promise.resolve();
        assert.ok(f.calls.some(call => call.name === "native.engine_close" && call.args[0].lease_id === "late"));
        assert.equal(f.calls.some(call => call.name === "native.engine_execute"), false);
        assert.equal(f.api.debugger.onEvent.listeners.length, 0);
    }],
    ["engine error after CDP input is unknown, persisted, and never retried", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        await f.executor.execute(action(f, {type: "snapshot"}));
        const original = f.hooks["native.engine_execute"];
        f.hooks["native.engine_execute"] = async payload => {
            await original(payload);
            return {ok: false, result: null, error: "Input outcome unconfirmed"};
        };
        const payload = action(f, {type: "click", snapshot_id: "snapshot-1", ref: "e1"});
        await assert.rejects(f.executor.execute(payload), /Input outcome/);
        const entry = f.local[WAL_KEY].effects[f.executor.effectKey(payload)];
        assert.equal(entry.completion, "unknown"); assert.equal(entry.result, null);
        assert.equal(entry.error, "Input outcome unconfirmed");
        const count = f.calls.filter(call => call.name === "native.engine_execute").length;
        await f.executor.execute(payload);
        assert.equal(f.calls.filter(call => call.name === "native.engine_execute").length, count);
    }],
    ...[false, true].map(dispatched => [
        `engine failure ${dispatched ? "after" : "before"} capture dispatch retires attachment before receipt`,
        async () => {
            const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
            const payload = action(f, {type: "screenshot", full_page: false});
            const captureEntered = deferred(), captureRelease = deferred();
            const detachEntered = deferred(), detachRelease = deferred(), detached = deferred();
            const receiptEntered = deferred(), receiptRelease = deferred(), lateFinished = deferred();
            const originalCommand = f.hooks["debugger.sendCommand"];
            f.hooks["debugger.sendCommand"] = async (source, method, params) => {
                if (method === "Page.getLayoutMetrics") return {contentSize: {width: 800, height: 1600},
                    cssContentSize: {width: 800, height: 1600}};
                if (method === "Page.startScreencast") {
                    queueMicrotask(() => f.api.debugger.onEvent.emit(source, "Page.screencastFrame",
                        {sessionId: 1, data: "private-frame"}));
                }
                if (method === "Page.captureScreenshot") {
                    captureEntered.resolve();
                    await captureRelease.promise;
                    return {data: "late-image-must-not-be-delivered"};
                }
                return originalCommand(source, method, params);
            };
            f.hooks["debugger.detach"] = async () => {
                detachEntered.resolve(); await detachRelease.promise; detached.resolve();
            };
            f.hooks["native.executor_completion"] = async wire => {
                if (wire.effect_id === payload.effect_id) {
                    receiptEntered.resolve(clone(wire)); await receiptRelease.promise;
                }
                return {type: "completion_accepted", request_id: wire.request_id,
                    effect_id: wire.effect_id, executor_fence: wire.executor_fence,
                    completion: wire.completion, result: wire.result, error: wire.error};
            };
            let session;
            f.hooks["native.engine_execute"] = async wire => {
                session = f.executor.engineSessions.get(f.executor.recordFor("remote"));
                if (dispatched) {
                    const sessionId = session.bridge.rootSessionId;
                    await session.bridge.handle({id: 71, sessionId,
                        method: "Runtime.callFunctionOn", params: {functionDeclaration: "() => true"}});
                    session.bridge.handle({id: 72, sessionId,
                        method: "Page.captureScreenshot", params: {format: "png", captureBeyondViewport: false,
                            clip: {x: 0, y: 0, width: 800, height: 600, scale: 1}}})
                        .then(() => lateFinished.resolve(), error => lateFinished.reject(error));
                    await captureEntered.promise;
                }
                throw new Error("Official screenshot failed");
            };
            const running = f.executor.execute(payload); running.catch(() => {});
            let replacement;
            try {
                await detachEntered.promise;
                const record = f.executor.recordFor("remote");
                const token = f.executor.attachments.get(record.tab_id);
                assert.equal(token.cleanup, true); assert.equal(record.attached, false);
                assert.equal(session.bridge.closed, true);
                assert.equal(f.calls.some(call => call.name === "native.executor_completion"), false);
                replacement = f.executor.execute(action(f, {type: "text"})); replacement.catch(() => {});
                await Promise.resolve();
                assert.equal(f.calls.filter(call => call.name === "debugger.attach").length, 1);
                assert.equal(f.calls.filter(call => call.name === "native.engine_execute").length, 1);
                assert.equal(f.executor.engineCdp({...payload, lease_id: "lease", message: {
                    id: 73, sessionId: session.bridge.rootSessionId, method: "Page.captureScreenshot",
                    params: {format: "png"}}}), false);
                detachRelease.resolve(); await detached.promise;
                const wire = await receiptEntered.promise;
                assert.equal(f.executor.attachments.has(record.tab_id), false);
                assert.equal(wire.completion, dispatched ? "unknown" : "not_applied");
                assert.equal(wire.result, null); assert.equal(wire.error, "Official screenshot failed");
                const saved = clone(f.local[WAL_KEY].effects[f.executor.effectKey(payload)]);
                assert.equal(saved.phase, "terminal"); assert.equal(saved.completion, wire.completion);
                assert.deepEqual(f.local[WAL_KEY].effects[f.executor.effectKey(payload)], saved);
                assert.equal(f.calls.filter(call => call.name === "native.executor_completion").length, 1);
                // The exact receipt is retried, never the failed screenshot or its late image.
                receiptRelease.reject(new Error("lost receipt"));
                f.hooks["native.engine_execute"] = async () => ({ok: true, result, error: null});
                await assert.rejects(running, /lost receipt/);
                await replacement;
                const nextToken = f.executor.attachments.get(record.tab_id);
                assert.ok(nextToken); assert.notEqual(nextToken, token);
                captureRelease.resolve();
                if (dispatched) await lateFinished.promise;
                else lateFinished.resolve();
                assert.equal(f.executor.attachments.get(record.tab_id), nextToken);
                assert.equal(record.attached, true);
                assert.equal(f.calls.filter(call => call.name === "debugger.detach").length, 1);
                assert.equal(f.calls.some(call => call.name === "native.engine_cdp" &&
                    call.args[0].message.id === 72), false);
                delete f.hooks["native.executor_completion"];
                const executions = f.calls.filter(call => call.name === "native.engine_execute").length;
                await f.executor.execute(payload); await f.executor.execute(payload);
                assert.equal(f.calls.filter(call => call.name === "native.engine_execute").length, executions);
                const receipts = f.calls.filter(call => call.name === "native.executor_completion" &&
                    call.args[0].effect_id === payload.effect_id);
                assert.equal(receipts.length, 2);
                assert.deepEqual(receipts[0].args[0], receipts[1].args[0]);
                assert.deepEqual(f.local[WAL_KEY].effects[f.executor.effectKey(payload)], saved);
            } finally {
                captureRelease.resolve(); detachRelease.resolve(); receiptRelease.resolve();
                await Promise.allSettled([running, replacement].filter(Boolean));
                await f.executor.setAutomation(false);
            }
        },
    ]),
    ["detach failure remains visible without replacing the durable engine outcome", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        const payload = action(f, {type: "screenshot", full_page: false});
        f.hooks["native.engine_execute"] = async () => { throw new Error("Capture not dispatched"); };
        f.hooks["debugger.detach"] = async () => { throw new Error("Detach failed"); };
        await assert.rejects(f.executor.execute(payload), /Detach failed/);
        assert.match(f.executor.popupState().error, /Detach failed/);
        assert.equal(f.executor.popupState().cleanup_pending, true);
        const saved = clone(f.local[WAL_KEY].effects[f.executor.effectKey(payload)]);
        assert.equal(saved.phase, "terminal"); assert.equal(saved.completion, "not_applied");
        assert.equal(saved.error, "Capture not dispatched"); assert.equal(saved.result, null);
        const calls = f.calls.map(call => call.name);
        assert.ok(calls.indexOf("debugger.detach") < calls.indexOf("native.executor_completion"));
        const executions = calls.filter(name => name === "native.engine_execute").length;
        await assert.rejects(f.executor.execute(action(f, {type: "text"})), /cleanup is still pending/);
        assert.equal(f.calls.filter(call => call.name === "native.engine_execute").length, executions);
        await f.executor.execute(payload);
        assert.deepEqual(f.local[WAL_KEY].effects[f.executor.effectKey(payload)], saved);
        delete f.hooks["debugger.detach"];
        await f.executor.setAutomation(false);
    }],
    ["persistent CDP response crosses A completion and B rebind without lease loss", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        const first = action(f, {type: "text"}); await f.executor.execute(first);
        const session = [...f.executor.engineSessions.values()][0];
        const sessionId = session.bridge.rootSessionId;
        const entered = deferred(), release = deferred(), replied = deferred();
        const originalCommand = f.hooks["debugger.sendCommand"];
        f.hooks["debugger.sendCommand"] = async (source, method, params) => {
            if (method === "Runtime.getProperties") {
                entered.resolve(); await release.promise; return {result: []};
            }
            return originalCommand(source, method, params);
        };
        const originalSend = f.hooks["native.engine_cdp"];
        f.hooks["native.engine_cdp"] = async wire => {
            if (wire.message.id === 81) replied.resolve(clone(wire));
            return originalSend(wire);
        };
        assert.equal(f.executor.engineCdp({...first, lease_id: "lease", message: {
            id: 81, sessionId, method: "Runtime.getProperties", params: {objectId: "old-object"}}}), true);
        await entered.promise;
        const second = action(f, {type: "console"});
        const executing = deferred(), finish = deferred();
        f.hooks["native.engine_execute"] = async () => {
            executing.resolve(); await finish.promise; return {ok: true, result, error: null};
        };
        const running = f.executor.execute(second);
        try {
            await executing.promise;
            assert.equal(f.executor.engineSessions.get(f.executor.recordFor("remote")), session);
            release.resolve();
            const response = await replied.promise;
            assert.equal(response.request_id, second.request_id);
            assert.equal(response.lease_id, "lease");
            assert.deepEqual(response.message, {id: 81, sessionId, result: {result: []}});
            const received = deferred();
            f.hooks["native.engine_cdp"] = async wire => {
                if (wire.message.id === 82) received.resolve(clone(wire));
            };
            // Host-origin A context remains valid after B is current.
            assert.equal(f.executor.engineCdp({...first, lease_id: "lease", message: {
                id: 82, sessionId, method: "Network.enable"}}), true);
            assert.deepEqual((await received.promise).message.result, {});
            for (const change of [{lease_id: "old-lease"}, {remote_key: "foreign"},
                {principal_id: "foreign"}, {connection_id: "old"},
                {executor_fence: {...first.executor_fence, epoch: 99}},
                {authorization: {...first.authorization, revision: 99}},
                {command: {...first.command, page: {...first.command.page, generation: 99}}},
                {command: {...first.command, page: {...first.command.page, page_id: "foreign"}}}]) {
                assert.equal(f.executor.engineCdp({...first, lease_id: "lease", ...change,
                    message: {id: 83, sessionId, method: "Runtime.getProperties"}}), false);
            }
            assert.equal(f.calls.filter(call => call.name === "native.engine_open").length, 1);
            assert.equal(f.calls.some(call => call.name === "native.engine_close"), false);
        } finally { release.resolve(); finish.resolve(); }
        await running;
        await f.executor.setAutomation(false);
    }],
    ["idle lease forwards observations but never executes Runtime or input mutations", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true}); setup(f); await f.ready();
        const first = action(f, {type: "text"}); await f.executor.execute(first);
        const session = [...f.executor.engineSessions.values()][0];
        const received = deferred();
        f.hooks["native.engine_cdp"] = async wire => {
            if (wire.message.method === "Runtime.consoleAPICalled") received.resolve(wire);
        };
        f.api.debugger.onEvent.emit({tabId: f.executor.recordFor("remote").tab_id},
            "Runtime.consoleAPICalled", {type: "log", args: []});
        assert.equal((await received.promise).lease_id, "lease");
        const count = f.calls.filter(call => call.name === "debugger.sendCommand").length;
        for (const [id, method] of [[91, "Runtime.evaluate"], [92, "Runtime.callFunctionOn"],
            [93, "Input.dispatchMouseEvent"], [94, "Page.navigate"]]) {
            const response = deferred();
            f.hooks["native.engine_cdp"] = async wire => {
                if (wire.message.id === id) response.resolve(wire.message);
            };
            assert.equal(f.executor.engineCdp({...first, lease_id: "lease", message: {
                id, method, sessionId: session.bridge.rootSessionId, params: {}}}), true);
            assert.match((await response.promise).error.message, /current accepted operation/);
        }
        assert.equal(f.calls.filter(call => call.name === "debugger.sendCommand").length, count);
        assert.equal(f.executor.effects[f.executor.effectKey(first)].completion, "applied");
        await f.executor.setAutomation(false);
    }],
    ["frozen v9 history and global OFF survive v10 restore", async () => {
        const f = fixture({globalAutomation: true, officialEngine: true});
        const old = {version: 9, effects: {old: {phase: "dispatched"}}};
        f.local.abushBrowserEffectsV9 = clone(old); f.local[AUTOMATION_KEY] = false;
        await f.ready(); assert.deepEqual(f.local.abushBrowserEffectsV9, old);
        assert.deepEqual(f.executor.effects, {}); assert.equal(f.tabs.size, 2);
    }],
];
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
