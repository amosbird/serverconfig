#!/usr/bin/env node

const assert = require("assert");
const {WorkspaceExecutor, BROWSER_ERROR_CODES} = require(
    "../chromium/external-links/extension/workspace.js");
const {NativeDispatcher} = require("../chromium/external-links/extension/background.js");

const fence = {executor_id: "executor", epoch: 1};
const logicalWindowId = "11111111-1111-4111-8111-111111111111";
const logicalPageId = "22222222-2222-4222-8222-222222222222";

function ensurePayload(overrides = {}) {
    return {
        remote_key: "r_12345678",
        connection_id: "c_12345678",
        principal_id: "principal-one",
        workspace_id: "workspace-authoritative",
        request_id: "request-one",
        ensure_id: "ensure-one",
        executor_fence: fence,
        expected_page: null,
        ...overrides,
    };
}

function executePayload(overrides = {}) {
    const payload = ensurePayload(overrides);
    delete payload.ensure_id;
    delete payload.workspace_id;
    delete payload.expected_page;
    return payload;
}

function deferred() {
    let resolve;
    let reject;
    const promise = new Promise((resolvePromise, rejectPromise) => {
        resolve = resolvePromise;
        reject = rejectPromise;
    });
    return {promise, resolve, reject};
}

function fixture(saved = []) {
    const calls = [];
    let nextWindow = 10;
    const windows = new Map(saved.map(record => [record.windowId, [
        {id: record.statusTabId, url: `chrome-extension://id/status.html#${record.nonce}`},
        {id: record.pageTabId, url: "about:blank"},
    ]]));
    const chrome = {
        runtime: {getURL: path => `chrome-extension://id/${path}`},
        storage: {session: {
            get: async () => ({abushWorkspaces: saved}),
            set: async value => calls.push(["persist", value]),
        }},
        windows: {
            create: async properties => {
                calls.push(["createWindow", properties]);
                const id = nextWindow++;
                windows.set(id, [{id: id + 100, url: properties.url}]);
                return {id, tabs: [{id: id + 100}]};
            },
            remove: async id => calls.push(["removeWindow", id]),
        },
        tabs: {
            query: async ({windowId}) => windows.get(windowId) || [],
            create: async properties => {
                calls.push(["createTab", properties]);
                const tab = {id: properties.windowId + 200, url: properties.url};
                windows.get(properties.windowId).push(tab);
                return tab;
            },
            update: async (id, properties) => calls.push(["update", id, properties]),
            get: async id => {
                for (const [windowId, tabs] of windows) {
                    if (tabs.some(tab => tab.id === id)) return {id, windowId};
                }
                throw new Error("missing tab");
            },
            remove: async id => calls.push(["remove", id]),
        },
        debugger: {
            attach: async target => calls.push(["attach", target]),
            detach: async target => calls.push(["detach", target]),
            sendCommand: async (...args) => calls.push(["command", ...args]),
        },
    };
    const native = async (service, operation, payload) => {
        calls.push([operation, payload]);
        return {};
    };
    return {executor: new WorkspaceExecutor(chrome, native), calls};
}

(async () => {
    {
        const {executor, calls} = fixture();
        const expectedPage = {
            window: {
                workspace_id: "workspace-authoritative",
                window_id: logicalWindowId,
                generation: 7,
            },
            page_id: logicalPageId,
            generation: 9,
        };
        const recreated = await executor.ensure(ensurePayload({expected_page: expectedPage}));
        assert.deepEqual(executor.pageRef(recreated), expectedPage,
            "empty Chrome session storage must recreate the physical window with exact refs");
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 1);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_old00000", executorFence: {...fence, epoch: 1},
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        const expectedPage = executor.pageRef(record);
        expectedPage.page_id = "different-page";
        await assert.rejects(executor.ensure(ensurePayload({expected_page: expectedPage})),
            /does not match expected page/);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "mismatched restored records must fail closed");
        assert.equal(calls.find(call => call[0] === "ensure_managed_page_completion")[1]
            .completion.code, "stale_page");
    }
    {
        const {executor, calls} = fixture();
        const [first, second] = await Promise.all([
            executor.ensure(ensurePayload()),
            executor.ensure(ensurePayload({request_id: "request-two", ensure_id: "ensure-two"})),
        ]);
        assert.strictEqual(first, second, "same principal ensures must reuse the record");
        assert.match(first.logicalWindowId, /^[0-9a-f-]{36}$/);
        assert.match(first.logicalPageId, /^[0-9a-f-]{36}$/);
        assert.notEqual(first.logicalWindowId, String(first.windowId));
        assert.notEqual(first.logicalPageId, String(first.pageTabId));
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 1);
        assert.deepEqual(calls.find(call => call[0] === "createWindow")[1], {
            url: calls.find(call => call[0] === "createWindow")[1].url,
            focused: false,
            type: "normal",
        });
        assert.equal(calls.filter(call => call[0] === "createTab").length, 1);
        assert.equal(calls.filter(call => call[0] === "bind_x11_window").length, 1);
        const completions = calls.filter(call => call[0] === "ensure_managed_page_completion");
        assert.equal(completions.length, 2);
        assert.deepEqual(completions.map(call => call[1].completion), [
            {outcome: "created", window_id: first.logicalWindowId,
                page_id: first.logicalPageId},
            {outcome: "created", window_id: first.logicalWindowId,
                page_id: first.logicalPageId},
        ]);
        assert(!calls.some(call => call[0].toLowerCase().includes("focus")));
    }
    {
        const {executor, calls} = fixture();
        const created = deferred();
        executor.chrome.windows.create = async properties => {
            calls.push(["createWindow", properties]);
            return created.promise;
        };
        const payload = ensurePayload();
        const ensuring = executor.ensure(payload);
        await Promise.resolve();
        const cancel = executor.ensureIdentity(payload);
        assert.equal(await executor.cancelEnsure(cancel), true);
        created.resolve({id: 10, tabs: [{id: 110}]});
        assert.equal(await ensuring, undefined);
        assert(calls.some(call => call[0] === "removeWindow" && call[1] === 10),
            "sole cancelled ensure must clean up its uncommitted window");
        assert.equal(executor.workspaces.size, 0);
        assert.equal(calls.filter(call => call[0] === "ensure_managed_page_completion").length, 0,
            "visible cancellation must suppress completion");
    }
    {
        const {executor, calls} = fixture();
        const created = deferred();
        const originalCreate = executor.chrome.windows.create;
        executor.chrome.windows.create = async properties => {
            await created.promise;
            return originalCreate(properties);
        };
        const firstPayload = ensurePayload();
        const secondPayload = ensurePayload({request_id: "request-two", ensure_id: "ensure-two"});
        const first = executor.ensure(firstPayload);
        const second = executor.ensure(secondPayload);
        await Promise.resolve();
        const firstCancel = executor.ensureIdentity(firstPayload);
        assert.equal(await executor.cancelEnsure(firstCancel), true);
        assert.equal(await executor.cancelEnsure({...firstCancel, request_id: "stale"}), false);
        created.resolve();
        assert.equal(await first, undefined);
        const record = await second;
        assert(record, "uncancelled waiter must receive the shared workspace");
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 1);
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "one cancelled waiter must not destroy another waiter's workspace");
        const completions = calls.filter(call => call[0] === "ensure_managed_page_completion");
        assert.deepEqual(completions.map(call => call[1].request_id), ["request-two"]);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-before-restart",
            connectionId: "c_old00000", executorFence: {...fence, epoch: 1},
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        await assert.rejects(executor.ensure(ensurePayload({
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: null,
        })), /requires an expected page/);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "restored state without an expected page must not be adopted ambiguously");
        assert.equal(record.workspaceId, "workspace-before-restart");
        assert.equal(record.connectionId, "c_old00000");
        assert.equal(record.executorFence.epoch, 1);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_old00000", executorFence: {...fence, epoch: 1},
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        const restored = await executor.ensure(ensurePayload({
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: executor.pageRef(record),
        }));
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "failover must reuse the principal window");
        assert.equal(restored.logicalWindowId, logicalWindowId);
        assert.equal(restored.logicalPageId, logicalPageId);
        assert.equal(record.connectionId, "c_12345678");
        assert.equal(record.executorFence.epoch, 2);
        const bindIndex = calls.findIndex(call => call[0] === "bind_x11_window");
        const failoverPersistIndex = calls.findLastIndex(call => call[0] === "persist");
        assert(bindIndex >= 0 && failoverPersistIndex > bindIndex,
            "failover identity must commit only after successful bind");
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_old00000", executorFence: {...fence, epoch: 1},
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") {
                assert.equal(record.workspaceId, "workspace-authoritative",
                    "bind must not expose the candidate workspace before commit");
                assert.equal(payload.workspace_id, "workspace-authoritative");
                throw new Error("bind failed");
            }
            return {};
        };
        await assert.rejects(executor.ensure(ensurePayload({
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: executor.pageRef(record),
        })), /bind failed/);
        assert.equal(record.connectionId, "c_old00000");
        assert.equal(record.executorFence.epoch, 1);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_old00000", executorFence: {...fence, epoch: 1},
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        const persist = executor.chrome.storage.session.set;
        executor.chrome.storage.session.set = async value => {
            await persist(value);
            throw new Error("persist failed");
        };
        await assert.rejects(executor.ensure(ensurePayload({
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: executor.pageRef(record),
        })), /persist failed/);
        assert.equal(record.connectionId, "c_old00000");
        assert.equal(record.executorFence.epoch, 1);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0);
    }
    {
        const legacy = {
            remoteKey: "r_12345678", connectionId: "c_12345678",
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([legacy]);
        await executor.restore();
        assert.equal(executor.workspaces.size, 0);
        assert.deepEqual(calls.find(call => call[0] === "persist")[1], {abushWorkspaces: []});
        assert(!calls.some(call => call[0] === "createWindow"), "restore must not create windows");
    }
    {
        const first = fixture();
        const one = await first.executor.ensure(ensurePayload());
        const second = await first.executor.ensure(ensurePayload({
            principal_id: "principal-two", request_id: "request-two", ensure_id: "ensure-two",
        }));
        assert.notEqual(one.windowId, second.windowId);
        assert.notEqual(one.logicalWindowId, second.logicalWindowId);
        assert.notEqual(one.logicalPageId, second.logicalPageId);
        assert.equal(first.executor.workspaces.size, 2);
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        assert.equal(executor.pageRef(record).window.workspace_id, "workspace-authoritative");
        await executor.execute({
            ...executePayload(), effect_id: "effect", command: {
                command: "navigate",
                page: executor.pageRef(record),
                url: "https://example.test/",
            },
        });
        assert(calls.some(call => call[0] === "command"));
        const completion = calls.find(call => call[0] === "executor_completion")[1];
        assert.equal(completion.principal_id, "principal-one");
        assert.equal(completion.completion, "applied");
        assert.equal(record.workspaceId, "workspace-authoritative");
        assert.equal(calls.find(call => call[0] === "bind_x11_window")[1].workspace_id,
            "workspace-authoritative");
        assert.equal(calls.find(call => call[0] === "command")[3].url,
            "https://example.test/");

        await executor.execute({
            ...executePayload({principal_id: "other"}), effect_id: "stale", command: {
                command: "navigate",
                page: executor.pageRef(record),
                url: "https://example.test/",
            },
        });
        assert.equal(calls.filter(call => call[0] === "command").length, 1);
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "not_applied");

        await executor.execute({
            ...executePayload(), effect_id: "stale-workspace", command: {
                command: "navigate",
                page: {...executor.pageRef(record), window: {
                    ...executor.windowRef(record), workspace_id: "principal-one",
                }},
                url: "https://example.test/",
            },
        });
        assert.equal(calls.filter(call => call[0] === "command").length, 1);
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "not_applied");

        await executor.execute({
            ...executePayload(), effect_id: "stale-page", command: {
                command: "navigate",
                page: {...executor.pageRef(record), generation: 0},
                url: "https://example.test/",
            },
        });
        assert.equal(calls.filter(call => call[0] === "command").length, 1);
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "not_applied");
    }
    {
        const persisted = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_12345678", executorFence: fence,
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([persisted]);
        await executor.restore();
        const restored = executor.workspaces.get(executor.key(
            persisted.remoteKey, persisted.principalId));
        assert.equal(restored.logicalWindowId, logicalWindowId);
        assert.equal(restored.logicalPageId, logicalPageId);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0);
    }
    {
        const {executor, calls} = fixture();
        let attempts = 0;
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window" && attempts++ === 0) {
                throw new Error("bind failed");
            }
            return {};
        };
        await assert.rejects(executor.ensure(ensurePayload()), /bind failed/);
        assert.equal(executor.workspaces.size, 0);
        assert(calls.some(call => call[0] === "removeWindow"));
        assert.deepEqual(calls.find(call => call[0] === "ensure_managed_page_completion")[1]
            .completion, {outcome: "rejected", code: "invalid_command"});
        assert(BROWSER_ERROR_CODES.has("invalid_command"));
        assert(!BROWSER_ERROR_CODES.has("workspace_unavailable"));
    }
    {
        const {executor} = fixture();
        executor.nativeRequest = async (_service, operation) => {
            throw new Error(operation === "ensure_managed_page_completion" ?
                "completion failed" : "bind failed");
        };
        await assert.rejects(executor.ensure(ensurePayload()), error =>
            error instanceof AggregateError && error.errors.length === 2);
    }
    {
        const {executor} = fixture();
        const payload = {...executePayload(), effect_id: "effect", command: {}};
        const operation = {
            ...executor.operationIdentity(payload), cancelled: false,
            workspaceKey: executor.key(payload.remote_key, payload.principal_id), tabId: 210,
        };
        executor.operations.set(executor.operationKey(payload), operation);
        for (const change of [
            {remote_key: "r_abcdefgh"}, {principal_id: "other"},
            {connection_id: "c_abcdefgh"}, {execution_request_id: "delayed"},
            {effect_id: "other-effect"}, {executor_fence: {...fence, epoch: 2}},
        ]) {
            executor.cancel({...executor.operationIdentity(payload),
                request_id: "cancel-request", execution_request_id: payload.request_id, ...change});
            assert.equal(operation.cancelled, false,
                `stale cancel changed operation: ${JSON.stringify(change)}`);
        }
        executor.cancel({...executor.operationIdentity(payload),
            request_id: "cancel-request", execution_request_id: payload.request_id});
        assert.equal(operation.cancelled, true);
    }
    {
        const {executor} = fixture();
        const firstKey = executor.key("r_12345678", "principal-one");
        const secondKey = executor.key("r_12345678", "principal-two");
        executor.tabOwners.set(210, firstKey);
        const first = {workspaceKey: firstKey, tabId: 210};
        const otherPrincipal = {workspaceKey: secondKey, tabId: 211};
        const otherTab = {workspaceKey: firstKey, tabId: 212};
        executor.operations.set("first", first);
        executor.operations.set("other-principal", otherPrincipal);
        executor.operations.set("other-tab", otherTab);
        await executor.debuggerDetached(210);
        assert.equal(first.uncertain, true);
        assert.equal(otherPrincipal.uncertain, undefined);
        assert.equal(otherTab.uncertain, undefined);
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const navigate = deferred();
        executor.chrome.debugger.sendCommand = async (...args) => {
            calls.push(["command", ...args]);
            return navigate.promise;
        };
        const payload = {...executePayload(), effect_id: "cancel-during", command: {
            command: "navigate", page: executor.pageRef(record), url: "https://example.test/",
        }};
        const executing = executor.execute(payload);
        while (!calls.some(call => call[0] === "command")) await Promise.resolve();
        executor.cancel({...executor.operationIdentity(payload), request_id: "cancel-request",
            execution_request_id: payload.request_id});
        navigate.resolve();
        await executing;
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "unknown", "cancellation after mutation send must not report not_applied or success");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const attach = deferred();
        executor.chrome.debugger.attach = async target => {
            calls.push(["attach", target]);
            await attach.promise;
        };
        const payload = {...executePayload(), effect_id: "cancel-before", command: {
            command: "navigate", page: executor.pageRef(record), url: "https://example.test/",
        }};
        const executing = executor.execute(payload);
        while (!calls.some(call => call[0] === "attach")) await Promise.resolve();
        executor.cancel({...executor.operationIdentity(payload), request_id: "cancel-request",
            execution_request_id: payload.request_id});
        attach.resolve();
        await executing;
        assert(!calls.some(call => call[0] === "command"));
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "not_applied");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const navigate = deferred();
        executor.chrome.debugger.sendCommand = async (...args) => {
            calls.push(["command", ...args]);
            return navigate.promise;
        };
        const payload = {...executePayload(), effect_id: "disconnect", command: {
            command: "navigate", page: executor.pageRef(record), url: "https://example.test/",
        }};
        const executing = executor.execute(payload);
        while (!calls.some(call => call[0] === "command")) await Promise.resolve();
        await executor.remoteState({remote_key: payload.remote_key,
            connection_id: payload.connection_id, connected: false});
        navigate.resolve();
        await executing;
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "unknown");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const navigate = deferred();
        executor.chrome.debugger.sendCommand = async (...args) => {
            calls.push(["command", ...args]);
            return navigate.promise;
        };
        const payload = {...executePayload(), effect_id: "cancel-after", command: {
            command: "navigate", page: executor.pageRef(record), url: "https://example.test/",
        }};
        const executing = executor.execute(payload);
        while (!calls.some(call => call[0] === "command")) await Promise.resolve();
        navigate.resolve();
        executor.cancel({...executor.operationIdentity(payload), request_id: "cancel-request",
            execution_request_id: payload.request_id});
        await executing;
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "unknown", "cancellation after navigate resolves must suppress success");
    }
    {
        const {executor, calls} = fixture();
        const firstWrite = deferred();
        let writes = 0;
        executor.chrome.storage.session.set = async value => {
            calls.push(["persist", value]);
            if (writes++ === 0) await firstWrite.promise;
        };
        const first = executor.persistRecords([{workspaceId: "older"}]);
        await Promise.resolve();
        const second = executor.persistRecords([{workspaceId: "newer"}]);
        await Promise.resolve();
        await Promise.resolve();
        assert.equal(calls.filter(call => call[0] === "persist").length, 1,
            "later persistence must wait for the earlier write");
        firstWrite.resolve();
        await Promise.all([first, second]);
        assert.deepEqual(calls.filter(call => call[0] === "persist").map(call =>
            call[1].abushWorkspaces[0].workspaceId), ["older", "newer"]);
    }
    {
        const listeners = {message: null, disconnect: null};
        const sent = [];
        const port = {
            onMessage: {addListener: listener => { listeners.message = listener; }},
            onDisconnect: {addListener: listener => { listeners.disconnect = listener; }},
            postMessage: message => sent.push(message),
        };
        const dispatcher = new NativeDispatcher(() => port, 5);
        const events = [];
        dispatcher.onEvent(message => events.push(message.event));
        await assert.rejects(dispatcher.request("abush_browser", "execute", {}), /timed out/);
        assert.deepEqual(events, ["timeout"]);
        const pending = dispatcher.request("abush_browser", "execute", {});
        listeners.disconnect();
        await assert.rejects(pending, /disconnected/);
        assert.deepEqual(events, ["timeout", "disconnect"]);
        assert.equal(sent.length, 2);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabId: 110, pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        executor.chrome.tabs.query = async () => [
            {id: 110, url: executor.statusUrl(record)},
            {id: 210, url: "about:blank"},
            {id: 211, url: "https://unexpected.test/"},
        ];
        await executor.restore();
        assert.equal(executor.workspaces.size, 0,
            "restore must require exactly one status tab and one business tab");
        assert.deepEqual(calls.find(call => call[0] === "persist")[1], {abushWorkspaces: []});
    }
    {
        const {executor, calls} = fixture();
        const persist = executor.chrome.storage.session.set;
        executor.chrome.storage.session.set = async value => {
            await persist(value);
            throw new Error("persist failed");
        };
        await assert.rejects(executor.ensure(ensurePayload()), /persist failed/);
        assert.equal(executor.workspaces.size, 0, "failed persist must roll back in-memory install");
        assert.equal(executor.windowOwners.size, 0);
        assert.equal(executor.tabOwners.size, 0);
        assert(calls.some(call => call[0] === "removeWindow"));
    }
    {
        const {executor, calls} = fixture();
        await executor.status();
        assert.deepEqual(calls.map(call => call[0]), ["refresh_remotes"]);
        assert(!calls.some(call => call[0] === "createWindow"), "status must not create windows");
    }
    console.log("workspace executor tests passed");
})().catch(error => { console.error(error); process.exitCode = 1; });
