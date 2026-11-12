#!/usr/bin/env node

const assert = require("assert");
const {WorkspaceExecutor, BROWSER_ERROR_CODES, displayTitle, STATUS_PREFIX,
    MARKER_IDS} = require(
    "../chromium/external-links/extension/workspace.js");
const {NativeDispatcher, connectWorkspaceAfterRestore} = require(
    "../chromium/external-links/extension/background.js");

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
        owner_display: {
            daemon_instance_id: "daemon-instance",
            daemon_boot_id: "daemon-boot",
            workbench_name: "workbench",
        },
        connection_display_name: "local",
        expected_page: null,
        ...overrides,
    };
}

function executePayload(overrides = {}) {
    const payload = ensurePayload(overrides);
    delete payload.ensure_id;
    delete payload.workspace_id;
    delete payload.expected_page;
    delete payload.owner_display;
    delete payload.connection_display_name;
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

function fixture(saved = [], orphanStatusNonces = [], ordinaryTabCount = 0,
                 orphanPage = false, restoredPopup = null, localState = {}) {
    const calls = [];
    if (!localState.abushOwnedMarkers) {
        localState.abushOwnedMarkers = Object.fromEntries(saved.map(record => [record.nonce, true]));
    }
    let nextWindow = 10;
    let nextTab = 1000;
    const orphaned = orphanStatusNonces.map((nonce, index) => [500 + index, [{
        id: 1500 + index,
        windowId: 500 + index,
        windowType: "normal",
        url: `chrome-extension://id/status.html#${nonce}`,
        title: `${STATUS_PREFIX}${nonce}`,
    }, ...(orphanPage ? [{id: 1600 + index, windowId: 500 + index,
        windowType: "normal", url: "about:blank"}] : [])]]);
    const restored = restoredPopup ? [[restoredPopup.windowId, [
        ...MARKER_IDS.map((marker, index) => ({
            id: restoredPopup.tabIds[index],
            windowId: restoredPopup.windowId,
            windowType: "normal",
            index,
            pinned: true,
            url: `chrome-extension://id/status.html#nonce=${restoredPopup.nonce}&marker=${marker}`,
            title: `${STATUS_PREFIX}${restoredPopup.nonce}:${marker}`,
        })),
        {id: restoredPopup.tabIds[2], windowId: restoredPopup.windowId,
            windowType: "normal", index: 2, pinned: false, url: restoredPopup.businessUrl},
    ]]] : [];
    const windows = new Map([[1, Array.from({length: Math.max(ordinaryTabCount, 1)}, (_, index) => ({
        id: 2000 + index, windowId: 1, windowType: "normal",
        url: `https://ordinary-${index}.test/`,
    }))], ...orphaned, ...restored, ...saved.map(record => [record.windowId, [
        ...record.statusTabIds.map((id, index) => ({
            id,
            windowId: record.windowId,
            windowType: "normal",
            index,
            pinned: true,
            url: `chrome-extension://id/status.html#nonce=${record.nonce}&marker=${MARKER_IDS[index]}`,
            title: `${STATUS_PREFIX}${record.nonce}:${MARKER_IDS[index]}`,
        })),
        {id: record.pageTabId, windowId: record.windowId, windowType: "normal",
            index: 2, pinned: false, url: "about:blank"},
    ]])]);
    const chrome = {
        runtime: {getURL: path => `chrome-extension://id/${path}`},
        storage: {
            session: {
                get: async () => ({abushWorkspaces: saved}),
                set: async value => calls.push(["persist", value]),
            },
            local: {
                get: async () => structuredClone(localState),
                set: async value => {
                    calls.push(["persistLocal", structuredClone(value)]);
                    Object.assign(localState, structuredClone(value));
                },
            },
        },
        windows: {
            get: async id => ({
                id,
                type: windows.get(id)[0]?.windowType || "normal",
                focused: id === 1,
            }),
            getAll: async () => [...windows.keys()].map(id => ({
                id,
                type: windows.get(id)[0]?.windowType || "normal",
                focused: id === 1,
            })),
            create: async properties => {
                calls.push(["createWindow", properties]);
                const id = nextWindow++;
                if (properties.url) {
                    const urls = Array.isArray(properties.url) ? properties.url : [properties.url];
                    const tabs = urls.map((url, index) => {
                        const tab = {id: nextTab++, windowId: id,
                            windowType: properties.type, url, index, pinned: false};
                        const params = new URLSearchParams(url.split("#")[1]);
                        tab.title = `${STATUS_PREFIX}${params.get("nonce")}:${params.get("marker")}`;
                        return tab;
                    });
                    windows.set(id, tabs);
                    return {id, tabs};
                }
                const source = [...windows.values()].find(tabs =>
                    tabs.some(tab => tab.id === properties.tabId));
                const tab = source.splice(source.findIndex(item => item.id === properties.tabId), 1)[0];
                tab.windowId = id;
                tab.windowType = properties.type;
                windows.set(id, [tab]);
                return {id, tabs: [tab]};
            },
            remove: async id => {
                calls.push(["removeWindow", id]);
                windows.delete(id);
            },
        },
        tabs: {
            query: async query => query.windowId === undefined ?
                [...windows.values()].flat() : windows.get(query.windowId) || [],
            create: async properties => {
                calls.push(["createTab", properties]);
                const windowId = properties.windowId ?? 1;
                const tab = {id: nextTab++, windowId, windowType: "normal", url: properties.url,
                    index: windows.get(windowId).length, pinned: false};
                if (properties.url?.includes("status.html#")) {
                    const params = new URLSearchParams(properties.url.split("#")[1]);
                    tab.title = `${STATUS_PREFIX}${params.get("nonce")}:${params.get("marker")}`;
                }
                windows.get(windowId).push(tab);
                return tab;
            },
            move: async (id, properties) => {
                calls.push(["move", id, properties]);
                let tab;
                for (const tabs of windows.values()) {
                    const index = tabs.findIndex(candidate => candidate.id === id);
                    if (index !== -1) tab = tabs.splice(index, 1)[0];
                }
                tab.windowId = properties.windowId;
                tab.windowType = windows.get(properties.windowId)[0].windowType;
                windows.get(properties.windowId).push(tab);
                return tab;
            },
            update: async (id, properties) => {
                calls.push(["update", id, properties]);
                for (const tabs of windows.values()) {
                    const tab = tabs.find(candidate => candidate.id === id);
                    if (!tab) continue;
                    Object.assign(tab, properties);
                    if (properties.pinned) {
                        tabs.splice(tabs.indexOf(tab), 1);
                        const pinnedCount = tabs.filter(candidate => candidate.pinned).length;
                        tabs.splice(pinnedCount, 0, tab);
                        tabs.forEach((candidate, index) => candidate.index = index);
                    }
                }
            },
            get: async id => {
                for (const [windowId, tabs] of windows) {
                    const tab = tabs.find(item => item.id === id);
                    if (tab) return {...tab, windowId};
                }
                throw new Error("missing tab");
            },
            remove: async id => {
                calls.push(["remove", id]);
                for (const tabs of windows.values()) {
                    const index = tabs.findIndex(tab => tab.id === id);
                    if (index !== -1) tabs.splice(index, 1);
                }
            },
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
    assert.equal(displayTitle("local"), "local");
    assert.equal(displayTitle("dev.example.com"), "dev.example.com");
    assert.equal(displayTitle("prod-alias"), "prod-alias");
    {
        const orphanNonce = "f".repeat(32);
        const {executor, calls} = fixture([], [orphanNonce]);
        await executor.restore();
        assert.deepEqual(calls.filter(call => call[0] === "remove"),
            [["remove", 1500]], "a marker tab in an ordinary window must not close that window");
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0);
    }
    {
        const orphanNonce = "f".repeat(32);
        const {executor, calls} = fixture([], [orphanNonce], 1, true);
        await executor.restore();
        assert.deepEqual(calls.filter(call => call[0] === "remove"),
            [["remove", 1500]], "orphan cleanup must remove only the marker tab");
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "orphan cleanup must never destroy a window containing user tabs");
    }
    {
        const {executor, calls} = fixture([], [], 1);
        executor.chrome.windows.create = async () => {
            throw new Error("window creation failed");
        };
        await assert.rejects(executor.ensure(ensurePayload()), /window creation failed/);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "a failed window create must not leave a partial marker tab");
    }
    {
        const nonce = "9".repeat(32);
        const restoredPopup = {
            windowId: 700,
            tabIds: [1700, 1701, 1702],
            nonce,
            businessUrl: "https://restored-abush.test/",
        };
        const localState = {abushOwnedMarkers: {[nonce]: true}};
        const {executor, calls} = fixture([], [], 0, false, restoredPopup, localState);
        await executor.restore();
        assert.deepEqual(calls.filter(call => call[0] === "removeWindow"),
            [["removeWindow", 700]], "restored owned popup must be removed before ensures");
    }
    {
        const nonce = "8".repeat(32);
        const restoredPopup = {
            windowId: 701,
            tabIds: [1710, 1711, 1712],
            nonce,
            businessUrl: "https://user-tab.test/",
        };
        const {executor, calls} = fixture([], [], 0, false, restoredPopup);
        await executor.restore();
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "marker topology without durable ownership must preserve the window");
        assert.equal(calls.filter(call => call[0] === "remove").length, 2);
    }
    {
        const gate = deferred();
        const order = [];
        const workspaces = {
            restoreWhenReady: async () => {
                order.push("restore-start");
                await gate.promise;
                order.push("restore-done");
            },
        };
        const ready = connectWorkspaceAfterRestore(workspaces, () => order.push("connect"));
        await Promise.resolve();
        assert.deepEqual(order, ["restore-start"]);
        gate.resolve();
        await ready;
        assert.deepEqual(order, ["restore-start", "restore-done", "connect"]);
    }
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
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
        const createWindow = calls.find(call => call[0] === "createWindow")[1];
        assert.equal(createWindow.focused, false);
        assert.equal(createWindow.type, "normal");
        assert.equal(createWindow.tabId, first.statusTabIds[0]);
        assert.equal(calls.filter(call => call[0] === "createTab").length, 3);
        assert.equal(calls.filter(call => call[0] === "move").length, 1);
        assert.equal(calls.filter(call => call[0] === "bind_x11_window").length, 1);
        assert.equal(calls.find(call => call[0] === "bind_x11_window")[1].display_title,
            "local");
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
        const cancelling = executor.cancelEnsure(cancel);
        created.resolve({id: 10, tabs: [{id: 110}]});
        assert.equal(await cancelling, true);
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
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        const restored = await executor.ensure(ensurePayload({
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: null,
        }));
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "a daemon restart with empty topology must adopt the live restored workspace");
        assert.equal(restored.logicalWindowId, logicalWindowId);
        assert.equal(restored.logicalPageId, logicalPageId);
        assert.equal(record.workspaceId, "workspace-authoritative");
        assert.equal(record.connectionId, "c_12345678");
        assert.equal(record.executorFence.epoch, 2);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_old00000", executorFence: {...fence, epoch: 1},
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([legacy]);
        await executor.restore();
        assert.equal(executor.workspaces.size, 0);
        assert.deepEqual(calls.find(call => call[0] === "persist")[1].abushWorkspaces, []);
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
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
        const record = await executor.ensure(ensurePayload());
        calls.length = 0;
        const persisted = deferred();
        executor.chrome.storage.session.set = async value => {
            calls.push(["persist", value]);
            await persisted.promise;
        };
        const payload = {...executePayload(), effect_id: "cancel-persist", command: {
            command: "navigate", page: executor.pageRef(record), url: "https://example.test/",
        }};
        const executing = executor.execute(payload);
        while (!calls.some(call => call[0] === "persist")) await Promise.resolve();
        assert(calls.some(call => call[0] === "command"),
            "navigate must commit before the persistence barrier");
        const cancelling = executor.cancel({...executor.operationIdentity(payload),
            request_id: "cancel-request", execution_request_id: payload.request_id});
        await Promise.resolve();
        assert.equal(calls.filter(call => call[0] === "executor_completion").length, 0,
            "executor completion must remain unpublished while persistence is blocked");
        assert.equal(calls.filter(call => call[0] === "cancellation_completion").length, 0,
            "cancellation acknowledgement must wait for executor completion publication");
        persisted.resolve();
        await Promise.all([executing, cancelling]);
        assert.deepEqual(calls.filter(call => call[0] === "executor_completion")
            .map(call => call[1].completion), ["unknown"],
        "late cancellation must produce exactly one executor completion");
        assert.equal(calls.filter(call => call[0] === "cancellation_completion").at(-1)[1]
            .completion, "unknown",
        "cancellation after navigate commit must never acknowledge not_applied");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        calls.length = 0;
        const payload = {...executePayload(), effect_id: "cancel-terminal", command: {
            command: "navigate", page: executor.pageRef(record), url: "https://example.test/",
        }};
        await executor.execute(payload);
        await executor.cancel({...executor.operationIdentity(payload),
            request_id: "cancel-request", execution_request_id: payload.request_id});
        assert.deepEqual(calls.filter(call => call[0] === "executor_completion")
            .map(call => call[1].completion), ["applied"],
        "late cancellation must not duplicate executor completion");
        assert.equal(calls.filter(call => call[0] === "cancellation_completion").at(-1)[1]
            .completion, "unknown",
        "terminal applied completion must remain visible to a late cancellation");
    }
    {
        const {executor, calls} = fixture();
        const firstWrite = deferred();
        let writes = 0;
        executor.chrome.storage.session.set = async value => {
            calls.push(["persist", structuredClone(value)]);
            if (writes++ === 0) await firstWrite.promise;
        };
        const recordA = {
            remoteKey: "r_aaaaaaaa", principalId: "principal-a", workspaceId: "workspace-a",
            connectionId: "c_aaaaaaaa", executorFence: fence, logicalWindowId: "window-a",
            logicalPageId: "page-a", windowGeneration: 1, pageGeneration: 1, windowId: 10,
            statusTabIds: [11, 13], pageTabId: 12, nonce: "a".repeat(32), debuggerAttached: false,
        };
        const recordB = {
            remoteKey: "r_bbbbbbbb", principalId: "principal-b", workspaceId: "workspace-b",
            connectionId: "c_bbbbbbbb", executorFence: fence, logicalWindowId: "window-b",
            logicalPageId: "page-b", windowGeneration: 1, pageGeneration: 1, windowId: 20,
            statusTabIds: [21, 23], pageTabId: 22, nonce: "b".repeat(32), debuggerAttached: false,
        };
        executor.install(recordA);
        const writeA = executor.persist();
        while (!calls.some(call => call[0] === "persist")) await Promise.resolve();
        executor.install(recordB);
        const writeB = executor.persist();
        firstWrite.resolve();
        await Promise.all([writeA, writeB]);
        const snapshots = calls.filter(call => call[0] === "persist")
            .map(call => call[1].abushWorkspaces.map(record => record.principalId));
        assert.deepEqual(snapshots, [["principal-a"], ["principal-a", "principal-b"]],
            "queued writes [A],[B] must snapshot in execution order");
        assert.deepEqual(calls.filter(call => call[0] === "persist").at(-1)[1]
            .abushWorkspaces.map(record => record.principalId),
        [...executor.workspaces.values()].map(record => record.principalId),
        "final durable state must equal the final workspace map");
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
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        executor.chrome.tabs.query = async () => [
            {id: 110, windowId: 10, index: 0, pinned: true,
                url: executor.statusUrl(record, "a")},
            {id: 111, windowId: 10, index: 1, pinned: true,
                url: executor.statusUrl(record, "b")},
            {id: 210, windowId: 10, index: 2, pinned: false, url: "about:blank"},
            {id: 211, windowId: 10, index: 3, pinned: false,
                url: "https://unexpected.test/"},
        ];
        await executor.restore();
        assert.equal(executor.workspaces.size, 0,
            "restore must require exactly two marker tabs and one business tab");
        assert.deepEqual(calls.find(call => call[0] === "persist")[1].abushWorkspaces, []);
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
        const gates = [deferred(), deferred()];
        const persisted = [];
        let write = 0;
        executor.chrome.storage.session.set = async value => {
            const index = write++;
            calls.push(["persist-start", value]);
            await gates[index].promise;
            persisted.push(value);
        };
        const firstRecord = {remoteKey: "r_one", principalId: "principal-one"};
        executor.workspaces.set("one", firstRecord);
        const first = executor.persist();
        await Promise.resolve();
        const secondRecord = {remoteKey: "r_two", principalId: "principal-two"};
        executor.workspaces.set("two", secondRecord);
        const second = executor.persist();
        await Promise.resolve();
        assert.equal(write, 1, "persist writes must be serialized");
        gates[0].resolve();
        await first;
        await Promise.resolve();
        assert.equal(write, 2);
        gates[1].resolve();
        await second;
        assert.deepEqual(persisted.map(value => value.revision), [1, 2]);
        assert.deepEqual(persisted.at(-1).abushWorkspaces.map(record => record.remoteKey),
            ["r_one", "r_two"], "newer whole-map state must win after reversed pressure");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        await executor.tabMoved(record.pageTabId, {windowId: 999});
        const report = calls.find(call => call[0] === "managed_page_moved_outside");
        assert(report, "moving a managed page must report topology before forgetting it");
        assert.deepEqual(report[1].page, executor.pageRef(record));
        assert.equal(executor.workspaces.size, 0);
    }
    {
        const {executor, calls} = fixture();
        await executor.status();
        assert.deepEqual(calls.map(call => call[0]), ["refresh_remotes"]);
        assert(!calls.some(call => call[0] === "createWindow"), "status must not create windows");
    }
    console.log("workspace executor tests passed");
})().catch(error => { console.error(error); process.exitCode = 1; });
