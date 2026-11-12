#!/usr/bin/env node

const assert = require("assert");
const {WorkspaceExecutor, BROWSER_ERROR_CODES, displayTitle, STATUS_PREFIX,
    MARKER_IDS} = require(
    "../chromium/external-links/extension/workspace.js");
const {NativeDispatcher, connectWorkspaceAfterRestore,
    registerWorkspaceWindowListeners} = require(
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
        localState.abushOwnedMarkers = Object.fromEntries(saved
            .filter(record => Number.isInteger(record.x11WindowId))
            .map(record => [record.nonce, {
            version: 2,
            nonce: record.nonce,
            window_id: record.windowId,
            status_tab_ids: [...record.statusTabIds],
            page_tab_id: record.pageTabId,
            xid: record.x11WindowId,
            logical_window_id: record.logicalWindowId,
            logical_page_id: record.logicalPageId,
            window_generation: record.windowGeneration ?? record.generation,
            page_generation: record.pageGeneration ?? record.generation,
            binding: {
                nonce: record.nonce,
                remote_key: record.remoteKey,
                workspace_id: record.workspaceId,
                generation: record.windowGeneration ?? record.generation,
                control_state: "remote",
                display_title: record.connectionDisplayName,
            },
        }]));
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
    const windows = new Map([[1, Array.from(
        {length: Math.max(ordinaryTabCount, 1)}, (_, index) => ({
        id: 2000 + index, windowId: 1, windowType: "normal",
        url: `https://ordinary-${index}.test/`, active: index === 0,
    }))], ...orphaned, ...restored, ...saved.map(record => [record.windowId, [
        ...record.statusTabIds.map((id, index) => ({
            id,
            windowId: record.windowId,
            windowType: "normal",
            index,
            pinned: true,
            url: `chrome-extension://id/status.html#nonce=${record.nonce}` +
                `&marker=${MARKER_IDS[index]}`,
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
            onRemoved: {addListener: listener => {
                calls.push(["windowRemovedListener", listener]);
            }},
            get: async id => {
                const tabs = windows.get(id);
                if (!tabs) throw new Error("missing window");
                return {
                    id,
                    type: tabs[0]?.windowType || "normal",
                    focused: id === 1,
                };
            },
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
                        tab.active = index === 0;
                        if (url.includes("ordinary-staging.html#")) {
                            tab.title = `__ABUSH_ORDINARY__:${url.split("#")[1]}`;
                        } else if (url.includes("status.html#")) {
                            const params = new URLSearchParams(url.split("#")[1]);
                            tab.title = `${STATUS_PREFIX}${params.get("nonce")}:` +
                                params.get("marker");
                        }
                        return tab;
                    });
                    windows.set(id, tabs);
                    return {id, tabs};
                }
                const source = [...windows.values()].find(tabs =>
                    tabs.some(tab => tab.id === properties.tabId));
                const index = source.findIndex(item => item.id === properties.tabId);
                const tab = source.splice(index, 1)[0];
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
                const tab = {
                    id: nextTab++, windowId, windowType: "normal", url: properties.url,
                    index: windows.get(windowId).length, pinned: false,
                    active: properties.active === true,
                };
                if (properties.url?.includes("status.html#")) {
                    const params = new URLSearchParams(properties.url.split("#")[1]);
                    tab.title = `${STATUS_PREFIX}${params.get("nonce")}:${params.get("marker")}`;
                }
                if (properties.url?.includes("ordinary-staging.html#")) {
                    tab.title = `__ABUSH_ORDINARY__:${properties.url.split("#")[1]}`;
                }
                if (properties.active) {
                    windows.get(windowId).forEach(candidate => candidate.active = false);
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
                tab.windowType = windows.get(properties.windowId)?.[0]?.windowType || "normal";
                if (!windows.has(properties.windowId)) windows.set(properties.windowId, []);
                windows.get(properties.windowId).push(tab);
                return tab;
            },
            update: async (id, properties) => {
                calls.push(["update", id, properties]);
                for (const tabs of windows.values()) {
                    const tab = tabs.find(candidate => candidate.id === id);
                    if (!tab) continue;
                    if (properties.active) {
                        tabs.forEach(candidate => candidate.active = candidate.id === id);
                    }
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
        if (operation === "bind_x11_window") {
            return {
                available: true,
                xid: 42,
                observed: {
                    WM_WINDOW_ROLE: "abush-browser",
                    _ABUSH_BINDING_NONCE: payload.nonce,
                    _ABUSH_REMOTE_ID: payload.remote_key,
                    _ABUSH_WORKSPACE_ID: payload.workspace_id,
                    _ABUSH_WINDOW_GENERATION: String(payload.generation),
                    _ABUSH_CONTROL_STATE: payload.control_state,
                    _ABUSH_DISPLAY_TITLE: payload.display_title,
                },
                qtile_moved: true,
                observed_group: "c",
            };
        }
        if (operation === "clear_x11_window") {
            return {cleared: true, xid: payload.xid, reclassified_group: "f"};
        }
        if (operation === "managed_page_moved_outside") {
            return {acknowledged: true, request_id: payload.request_id,
                result: {outcome: "page_moved_outside"}};
        }
        return {};
    };
    return {executor: new WorkspaceExecutor(chrome, native), calls};
}

(async () => {
    assert.equal(displayTitle("local"), "local");
    assert.equal(displayTitle("dev.example.com"), "dev.example.com");
    assert.equal(displayTitle("prod-alias"), "prod-alias");
    {
        const {executor, calls} = fixture([], [], 1);
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-one",
            operation: "ordinary_open",
            url: "https://ordinary-open.test/path",
            focus: true,
        });
        assert.equal(completion, "not_applied",
            "prepare failure must be not_applied before URL mutation");
        assert.equal(calls.filter(call => call[0] === "update" &&
            call[2]?.url === "https://ordinary-open.test/path").length, 0,
            "prepare failure must not navigate the staging tab");
        const staging = calls.find(call => call[0] === "createTab");
        assert.equal(staging[1].windowId, 1, "staging must always carry an exact windowId");
    }
    {
        const {executor, calls} = fixture([], [], 1);
        const originalActive = await executor.chrome.tabs.get(2000);
        assert.equal(originalActive.active, true);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") throw new Error("prepare failed");
            return {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-prepare-rollback",
            operation: "ordinary_open",
            url: "https://ordinary-open.test/rollback",
            focus: true,
        });
        assert.equal(completion, "not_applied");
        assert.equal((await executor.chrome.tabs.get(2000)).active, true,
            "prepare rollback must restore the exact original active tab");
        assert.equal(calls.filter(call => call[0] === "remove").length, 1,
            "prepare rollback must delete the staging tab");
    }
    {
        const {executor, calls} = fixture([], [], 1);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            if (operation === "activate") throw new Error("focus verification failed");
            return {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-two",
            operation: "ordinary_open",
            url: "https://ordinary-open.test/after-prepare",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "activation failure after navigation must be unknown");
        assert.equal(calls.filter(call => call[0] === "update" &&
            call[2]?.url === "https://ordinary-open.test/after-prepare").length, 1);
    }
    {
        const {executor, calls} = fixture([], [], 1);
        const originalUpdate = executor.chrome.tabs.update;
        executor.chrome.tabs.update = async (id, properties) => {
            if (properties.url === "https://ordinary-open.test/update-race") {
                throw new Error("update result lost");
            }
            return originalUpdate(id, properties);
        };
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            return {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-update-race",
            operation: "ordinary_open",
            url: "https://ordinary-open.test/update-race",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "a rejected navigation call may already have applied and must be unknown");
        assert.equal(calls.filter(call => call[0] === "remove").length, 0,
            "a navigation race must not delete the target tab");
    }
    {
        const {executor, calls} = fixture([], [], 1);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            if (operation === "ordinary_file_navigate") {
                return {...payload, completion: "applied"};
            }
            return {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file",
            operation: "ordinary_open",
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "applied");
        const navigate = calls.find(call => call[0] === "ordinary_file_navigate");
        assert.equal(navigate[1].url, "file:///home/amos/private.txt");
        assert.equal(navigate[1].xid, 42);
        assert.equal(typeof navigate[1].nonce, "string");
        assert.equal(calls.filter(call => call[0] === "update" && call[2]?.url).length, 0,
            "file navigation must never use extension tabs.update");
    }
    {
        const {executor} = fixture([], [], 1);
        executor.nativeRequest = (service, operation, payload) => {
            if (operation === "prepare") return Promise.resolve({xid: 42, nonce: payload.nonce});
            if (operation === "ordinary_file_navigate") {
                const error = new Error("native request was not sent");
                error.nativeDispatch = "not_sent";
                return Promise.reject(error);
            }
            return Promise.resolve({});
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file-before-send",
            operation: "ordinary_open",
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "not_applied",
            "synchronous failure before native dispatch must remain not_applied");
    }
    {
        const {executor} = fixture([], [], 1);
        executor.nativeRequest = (service, operation, payload) => {
            if (operation === "prepare") return Promise.resolve({xid: 42, nonce: payload.nonce});
            if (operation === "ordinary_file_navigate") {
                const error = new Error("native request dispatch became uncertain");
                error.nativeDispatch = "unknown";
                return Promise.reject(error);
            }
            return Promise.resolve({});
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file-dispatch-unknown",
            operation: "ordinary_open",
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "native dispatch failure after postMessage starts must remain unknown");
    }
    {
        const {executor, calls} = fixture([], [], 1);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            if (operation === "ordinary_file_navigate") {
                return {...payload, nonce: "f".repeat(32), completion: "applied"};
            }
            return {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file-wrong-nonce",
            operation: "ordinary_open",
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "a sent mutation with a mismatched native correlation must be unknown");
    }
    {
        const {executor, calls} = fixture([], [], 1);
        const payload = {
            version: 1,
            request_id: "ordinary-duplicate",
            operation: "ordinary_open",
            url: "https://ordinary-open.test/duplicate",
            focus: true,
        };
        const gate = deferred();
        executor.nativeRequest = async (service, operation, value) => {
            calls.push([operation, value]);
            if (operation === "prepare") {
                await gate.promise;
                return {xid: 42, nonce: value.nonce};
            }
            return {};
        };
        const first = executor.ordinaryOpen(payload);
        while (!calls.some(call => call[0] === "prepare")) await Promise.resolve();
        const duplicate = executor.ordinaryOpen({...payload});
        await assert.rejects(executor.ordinaryOpen({...payload,
            url: "https://ordinary-open.test/conflict"}), /conflicting duplicate/);
        gate.resolve();
        assert.deepEqual(await Promise.all([first, duplicate]), ["applied", "applied"]);
        assert.equal(calls.filter(call => call[0] === "prepare").length, 1);
        assert.equal(calls.filter(call => call[0] === "ordinary_open_completion").length, 1);
        await assert.rejects(executor.ordinaryOpen({...payload,
            url: "https://ordinary-open.test/completed-conflict"}), /conflicting duplicate/);
    }
    {
        const {executor, calls} = fixture([], [], 1);
        const gates = [deferred(), deferred()];
        let prepares = 0;
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") {
                const gate = gates[prepares++];
                await gate.promise;
                return {xid: 42, nonce: payload.nonce};
            }
            return {};
        };
        const first = executor.ordinaryOpen({
            version: 1, request_id: "ordinary-serialized-one",
            operation: "ordinary_open", url: "https://one.test/", focus: true,
        });
        const second = executor.ordinaryOpen({
            version: 1, request_id: "ordinary-serialized-two",
            operation: "ordinary_open", url: "https://two.test/", focus: true,
        });
        while (prepares < 1) await Promise.resolve();
        assert.equal(prepares, 1, "ordinary opens must not share staging state concurrently");
        gates[0].resolve();
        await first;
        while (prepares < 2) await Promise.resolve();
        gates[1].resolve();
        assert.equal(await second, "applied");
    }
    {
        const {executor} = fixture([], [], 2);
        const selected = await executor.selectOrdinaryWindow();
        assert.equal(selected.windowId, 1);
        assert.equal(selected.originalActiveTabId, 2000,
            "prepare rollback must capture the active tab before staging");
    }
    {
        const {executor} = fixture([], [], 1);
        executor.chrome.windows.getAll = async () => [
            {id: 1, type: "normal", focused: false},
            {id: 2, type: "normal", focused: false},
        ];
        executor.chrome.windows.getLastFocused = async () => ({id: 2, type: "normal"});
        const originalQuery = executor.chrome.tabs.query;
        executor.chrome.tabs.query = async query => query.windowId === 2 ? [{
            id: 3000, windowId: 2, windowType: "normal", active: true,
            url: "https://last-focused.test/",
        }] : originalQuery(query);
        await executor.initializeOrdinaryMru();
        assert(executor.ordinaryWindowMru.get(2) > 0,
            "service worker restart must seed ordinary MRU from getLastFocused");
        await executor.recordOrdinaryFocus(1);
        assert(executor.ordinaryWindowMru.get(1) > executor.ordinaryWindowMru.get(2),
            "focus changes after readiness must update ordinary MRU");
    }
    {
        const managed = {
            remoteKey: "r_12345678", principalId: "principal", workspaceId: "workspace",
            connectionId: "c_12345678", executorFence: fence,
            logicalWindowId, logicalPageId, windowGeneration: 1, pageGeneration: 1,
            windowId: 1, x11WindowId: 10001, statusTabIds: [3001, 3002], pageTabId: 3003,
            nonce: "a".repeat(32), connectionDisplayName: "local",
        };
        const {executor, calls} = fixture([managed], [], 0);
        const create = executor.chrome.windows.create;
        executor.chrome.windows.create = async properties => {
            await create(properties);
            throw new Error("window result lost");
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-create-race",
            operation: "ordinary_open",
            url: "https://ordinary-open.test/create-race",
            focus: true,
        });
        assert.equal(completion, "not_applied",
            "a created staging window is not_applied only after verified cleanup");
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 1);
        assert.equal(calls.filter(call => call[0] === "update" &&
            call[2]?.url === "https://ordinary-open.test/create-race").length, 0);
    }
    {
        const managed = {
            remoteKey: "r_12345678", principalId: "principal", workspaceId: "workspace",
            connectionId: "c_12345678", executorFence: fence,
            logicalWindowId, logicalPageId, windowGeneration: 1, pageGeneration: 1,
            windowId: 1, x11WindowId: 10001, statusTabIds: [3001, 3002], pageTabId: 3003,
            nonce: "d".repeat(32), connectionDisplayName: "local",
        };
        const {executor, calls} = fixture([managed], [], 0);
        executor.chrome.windows.create = async properties => {
            calls.push(["createWindow", properties]);
            return {id: 99, tabs: [{id: 1999, windowId: 99, url: properties.url,
                title: `__ABUSH_ORDINARY__:${properties.url.split("#")[1]}`}]};
        };
        const selected = await executor.selectOrdinaryWindow();
        assert.equal(selected, null);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "selection must not create a window before rollback state exists");
    }
    {
        const nonce = "4".repeat(32);
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 42, statusTabIds: [110, 111], pageTabId: 210,
            nonce, connectionDisplayName: "local",
        };
        const localState = {};
        const first = fixture([record], [], 0, false, null, localState);
        await first.executor.restore();
        first.executor.chrome.windows.remove = async id => {
            first.calls.push(["removeWindow", id]);
            throw new Error("Qtile cleanup failed");
        };
        await assert.rejects(first.executor.convergeManagedTopology(record,
            {closeWindow: true, report: false}), /cleanup was incomplete/);
        assert(localState.abushCleanupTombstones?.[0]);
        assert.equal(localState.abushCleanupTombstones[0].nonce, nonce);
        assert.equal(localState.abushCleanupTombstones[0].xid, 42,
            "failed cleanup must retain durable X11 ownership proof");
        assert.equal(localState.abushOwnedMarkers[nonce].version, 2,
            "failed physical cleanup must retain the owned marker fingerprint");

        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restore();
        assert.deepEqual(localState.abushCleanupTombstones, [],
            "restart must retry and clear a confirmed cleanup tombstone");
        assert.equal(localState.abushOwnedMarkers[nonce], undefined);
    }
    {
        const nonce = "3".repeat(32);
        const tombstone = {
            version: 1, nonce, window_id: 999, xid: null,
            status_tab_ids: [1998, 1999], page_tab_id: null, binding: null,
            errors: ["fresh cleanup failed"],
        };
        const localState = {
            abushCleanupTombstones: [tombstone],
            abushOwnedMarkers: {[nonce]: {version: 1, nonce, pending: true}},
        };
        const {executor} = fixture([], [], 0, false, null, localState);
        await executor.restore();
        assert.deepEqual(localState.abushCleanupTombstones, []);
        assert.equal(localState.abushOwnedMarkers[nonce], undefined,
            "failed fresh-candidate cleanup must converge after service-worker restart");
    }
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
        const localState = {abushOwnedMarkers: {[nonce]: {
            version: 2,
            nonce,
            window_id: 700,
            status_tab_ids: [1700, 1701],
            page_tab_id: 1702,
            xid: 7000,
            logical_window_id: logicalWindowId,
            logical_page_id: logicalPageId,
            window_generation: 1,
            page_generation: 1,
            binding: {
                nonce,
                remote_key: "r_12345678",
                workspace_id: "workspace",
                generation: 1,
                control_state: "remote",
                display_title: "local",
            },
        }}};
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
        const nonce = "7".repeat(32);
        const restoredPopup = {
            windowId: 702,
            tabIds: [1720, 1721, 1722],
            nonce,
            businessUrl: "https://single-marker.test/",
        };
        const localState = {abushOwnedMarkers: {[nonce]: true}};
        const {executor, calls} = fixture([], [], 0, false, restoredPopup, localState);
        const tabs = await executor.chrome.tabs.query({windowId: restoredPopup.windowId});
        tabs.splice(1, 1);
        tabs[1].index = 1;
        await executor.restore();
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "legacy one-marker ownership cannot prove whole-window ownership");
        assert.deepEqual(calls.filter(call => call[0] === "remove"), [["remove", 1720]],
            "unsafe shutdown residue must remove only its marker tab");
    }
    {
        const nonce = "6".repeat(32);
        const restoredPopup = {
            windowId: 703,
            tabIds: [1730, 1731, 1732],
            nonce,
            businessUrl: "https://single-marker.test/",
        };
        const {executor, calls} = fixture([], [], 0, false, restoredPopup);
        const tabs = await executor.chrome.tabs.query({windowId: restoredPopup.windowId});
        tabs.splice(1, 1);
        tabs[1].index = 1;
        await executor.restore();
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "one-marker topology without durable ownership must preserve user windows");
        assert.deepEqual(calls.filter(call => call[0] === "remove"), [["remove", 1730]]);
    }
    {
        const nonce = "5".repeat(32);
        const restoredPopup = {
            windowId: 704,
            tabIds: [1740, 1741, 1742],
            nonce,
            businessUrl: "https://attacker-business.test/",
        };
        const localState = {abushOwnedMarkers: {[nonce]: {
            version: 2,
            nonce,
            window_id: 704,
            status_tab_ids: [1740, 1741],
            page_tab_id: 999999,
            xid: 7040,
            logical_window_id: logicalWindowId,
            logical_page_id: logicalPageId,
            window_generation: 7,
            page_generation: 9,
            binding: {
                nonce,
                remote_key: "r_attacker",
                workspace_id: "workspace-attacker",
                generation: 7,
                control_state: "remote",
                display_title: "attacker",
            },
        }}};
        const {executor, calls} = fixture([], [], 0, false, restoredPopup, localState);
        await executor.restore();
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "copied markers plus a mismatched fingerprint must never destroy a user window");
        assert.deepEqual(calls.filter(call => call[0] === "remove").map(call => call[1]),
            [1740, 1741]);
        assert(calls.some(call => call[0] === "clear_x11_window" && call[1].xid === 7040),
            "unproven orphan ownership must clear verified managed X11 metadata");
        assert.equal(localState.abushOwnedMarkers[nonce], undefined);
    }
    {
        const localState = {abushRestoreHost: true};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.windows.getAll = async () => [];
        await executor.restore();
        assert.equal(localState.abushRestoreHost, false,
            "restore intent must be consumed before opening a neutral host");
        assert.deepEqual(calls.filter(call => call[0] === "createWindow").map(call => call[1]), [
            {url: "about:blank", focused: false, type: "normal"},
        ]);
        const clearIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushRestoreHost === false);
        const createIndex = calls.findIndex(call => call[0] === "createWindow");
        assert(clearIndex > createIndex,
            "verified recovery must be consumed only after host creation");
    }
    {
        const localState = {abushRestoreHost: true};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.sessions = {
            getRecentlyClosed: async query => {
                calls.push(["getRecentlyClosed", query]);
                return [
                    {tab: {sessionId: "tab-session"}, sessionId: "tab-session"},
                    {window: {tabs: [
                        {url: "chrome-extension://id/status.html#nonce=" +
                            "a".repeat(32) + "&marker=a"},
                        {url: "https://managed.test/"},
                    ]}, sessionId: "managed-window-session"},
                    {window: {tabs: [{url: "https://work.test/"}]}, sessionId: "window-session"},
                ];
            },
            restore: async sessionId => {
                calls.push(["restoreSession", sessionId]);
                return {window: {sessionId, tabs: [{url: "https://work.test/"}]}};
            },
        };
        await executor.restore();
        assert.equal(localState.abushRestoreHost, false);
        assert.deepEqual(calls.filter(call => call[0] === "restoreSession"), [
            ["restoreSession", "window-session"],
        ], "restart recovery must skip managed entries and restore the ordinary window");
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "successful session restore must not create a blank fallback window");
        const clearIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushRestoreHost === false);
        const restoreIndex = calls.findIndex(call => call[0] === "restoreSession");
        assert(clearIndex > restoreIndex,
            "verified recovery must be consumed only after session restore");
    }
    {
        const localState = {abushRestoreHost: {
            version: 1,
            sessionId: "ordinary-window-session",
            snapshot: {tabCount: 2, urls: ["https://work.test/", "https://docs.test/"]},
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.sessions = {
            getRecentlyClosed: async query => {
                calls.push(["getRecentlyClosed", query]);
                return [{window: {tabs: [{url: "chrome-extension://id/status.html#nonce=" +
                    "a".repeat(32) + "&marker=a"}]}, sessionId: "managed-window-session"}];
            },
            restore: async sessionId => {
                calls.push(["restoreSession", sessionId]);
                return {window: {sessionId, tabs: [
                    {url: "https://work.test/"}, {url: "https://docs.test/"},
                ]}};
            },
        };
        await executor.restore();
        assert.equal(localState.abushRestoreHost, false);
        assert.deepEqual(calls.filter(call => call[0] === "restoreSession"), [
            ["restoreSession", "ordinary-window-session"],
        ], "restart recovery must use the persisted exact ordinary session ID");
        assert.equal(calls.filter(call => call[0] === "getRecentlyClosed").length, 1,
            "successful exact restore may only refresh the close baseline after recovery");
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0);
    }
    {
        const localState = {abushRestoreHost: {
            version: 2, phase: "inflight", sessionId: "ordinary-window-session",
            snapshot: {tabCount: 1, urls: ["https://ordinary-0.test/"]},
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.sessions = {
            restore: async sessionId => {
                calls.push(["restoreSession", sessionId]);
                throw new Error("must not restore an already-live exact snapshot");
            },
        };
        await executor.restore();
        assert.equal(calls.filter(call => call[0] === "restoreSession").length, 0,
            "inflight restore restart must verify live target before replaying the effect");
        assert.equal(localState.abushRestoreHost, false);
    }
    {
        const snapshot = {tabCount: 2, urls: ["https://work.test/", "https://docs.test/"]};
        const localState = {abushRestoreHost: {
            version: 1, sessionId: "expired-session", snapshot,
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [
                {window: {tabs: [{url: "https://wrong.test/"}]}, sessionId: "newer-wrong"},
                {window: {tabs: snapshot.urls.map(url => ({url}))}, sessionId: "matched-session"},
                {window: {tabs: [{url: "chrome-extension://id/status.html#nonce=" +
                    "a".repeat(32) + "&marker=a"}]}, sessionId: "managed-window-session"},
            ],
            restore: async sessionId => {
                calls.push(["restoreSession", sessionId]);
                if (sessionId === "expired-session") throw new Error("expired");
                return {window: {sessionId, tabs: snapshot.urls.map(url => ({url}))}};
            },
        };
        await executor.restore();
        assert.deepEqual(calls.filter(call => call[0] === "restoreSession"), [
            ["restoreSession", "expired-session"],
            ["restoreSession", "matched-session"],
        ], "failed exact restore must fall back to the matching snapshot, not the newest window");
    }
    {
        const localState = {abushRestoreHost: true};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.windows.getAll = async () => [];
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [],
            restore: async sessionId => { calls.push(["restoreSession", sessionId]); },
        };
        await executor.restore();
        assert.equal(calls.filter(call => call[0] === "restoreSession").length, 0);
        assert.deepEqual(calls.filter(call => call[0] === "createWindow").map(call => call[1]), [
            {url: "about:blank", focused: false, type: "normal"},
        ], "missing recently closed windows must fall back to a neutral host");
    }
    {
        const localState = {abushRestoreHost: true};
        const {executor} = fixture([], [], 1, false, null, localState);
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [
                {window: {sessionId: "one", tabs: [{url: "https://one.test/"}]}},
                {window: {sessionId: "two", tabs: [{url: "https://two.test/"}]}},
            ],
            restore: async () => { throw new Error("must not choose an ambiguous snapshot"); },
        };
        await assert.rejects(executor.restore(), /ambiguous ordinary restore snapshot/);
        assert.equal(localState.abushRestoreHost.phase, "prepared",
            "ambiguous restore must fail closed without consuming its ledger");
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
        assert.deepEqual(await ready, []);
        assert.deepEqual(order, ["restore-start", "restore-done", "connect"]);
    }
    {
        const order = [];
        const workspaces = {
            restoreWhenReady: async () => { order.push("restore"); },
            drainAdvisoryErrors: () => [new Error("outbox pending")],
        };
        const errors = await connectWorkspaceAfterRestore(workspaces, async () => {
            order.push("connect-start");
            await Promise.resolve();
            order.push("connect-done");
        });
        assert.deepEqual(order, ["restore", "connect-start", "connect-done"]);
        assert.match(errors[0].message, /outbox pending/);
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
        const binding = calls.find(call => call[0] === "bind_x11_window")[1];
        assert.equal(binding.generation, 7,
            "X11 binding must use logical window generation, not page generation or a constant");
        assert.equal(executor.pageRef(recreated).generation, 9);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 1);
    }
    {
        const listeners = {};
        const moved = [];
        const removed = [];
        const failures = [];
        const lifecycle = [];
        const chromeApi = {
            windows: {
                onCreated: {addListener: listener => { listeners.windowCreated = listener; }},
                onFocusChanged: {addListener: listener => { listeners.windowFocused = listener; }},
                onRemoved: {addListener: listener => { listeners.windowRemoved = listener; }},
            },
            tabs: {
                onCreated: {addListener: listener => { listeners.tabCreated = listener; }},
                onUpdated: {addListener: listener => { listeners.tabUpdated = listener; }},
                onMoved: {addListener: listener => { listeners.tabMoved = listener; }},
                onRemoved: {addListener: listener => { listeners.tabRemoved = listener; }},
                onAttached: {addListener: listener => { listeners.tabAttached = listener; }},
                onDetached: {addListener: listener => { listeners.tabDetached = listener; }},
                onReplaced: {addListener: listener => { listeners.tabReplaced = listener; }},
            },
        };
        const ready = deferred();
        const snapshots = [];
        const workspaceMethods = {
            enqueueLifecycle: async task => {
                lifecycle.push("start");
                await task();
                lifecycle.push("done");
            },
            recordOrdinaryFocus: async id => { moved.push(["focus", id]); },
            windowRemoved: async id => { removed.push(id); },
            tabCreated: async (tab) => { moved.push(["created", tab.id]); },
            tabUpdated: async (id, info) => { moved.push(["updated", id, info]); },
            tabMoved: async (id, info) => { moved.push(["moved", id, info]); },
            tabRemoved: async (id, info) => { moved.push(["removed", id, info]); },
            tabAttached: async (id, info) => { moved.push(["attached", id, info]); },
            tabDetached: async (id, info) => { moved.push(["detached", id, info]); },
            tabReplaced: async (added, removedId) => {
                moved.push(["replaced", added, removedId]);
            },
            rememberWindowSnapshot: async id => { snapshots.push(id); },
        };
        registerWorkspaceWindowListeners(chromeApi, workspaceMethods,
            ready.promise, error => failures.push(error));
        listeners.windowCreated({id: 42, type: "normal"});
        listeners.windowFocused(42);
        listeners.tabCreated({id: 24, windowId: 42});
        listeners.tabUpdated(24, {title: "changed"}, {id: 24, windowId: 42, index: 0});
        listeners.tabMoved(24, {windowId: 42, fromIndex: 0, toIndex: 1});
        listeners.tabRemoved(24, {windowId: 42});
        listeners.tabDetached(24, {oldWindowId: 42});
        listeners.windowRemoved(42);
        listeners.tabAttached(24, {windowId: 7, newWindowId: 7});
        await Promise.resolve();
        assert.deepEqual(removed, []);
        assert.deepEqual(moved, []);
        assert.equal(typeof listeners.tabReplaced, "function",
            "tabs.onReplaced must be registered");
        ready.resolve();
        await ready.promise;
        await Promise.resolve();
        await Promise.resolve();
        assert.deepEqual(removed, [42]);
        const normalizedMoved = moved.map(value => JSON.stringify(value)).sort();
        assert.deepEqual(normalizedMoved, [
            ["created", null], ["created", 24], ["detached", 24, {oldWindowId: 42}],
            ["focus", 42], ["moved", 24, {windowId: 42, fromIndex: 0, toIndex: 1}],
            ["removed", 24, {windowId: 42}],
            ["attached", 24, {windowId: 7, newWindowId: 7}],
            ["updated", 24, {title: "changed"}],
        ].map(value => JSON.stringify(value)).sort());
        assert.deepEqual([...snapshots].sort((left, right) => left - right),
            [7, 42, 42, 42, 42, 42, 42]);
        assert.equal(lifecycle.filter(value => value === "start").length, 9,
            "all lifecycle-sensitive Chrome events must enter the serialized tail");
        listeners.tabReplaced(90, 80);
        await Promise.resolve();
        await Promise.resolve();
        assert(moved.some(call => call[0] === "replaced" && call[1] === 90 && call[2] === 80));
        assert.equal(failures.length, 0);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_old00000",
            executorFence: {...fence, epoch: 1}, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "2".repeat(32), connectionDisplayName: "local",
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        calls.length = 0;
        let binds = 0;
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") {
                binds += 1;
                if (binds === 1) {
                    const error = new Error("response timed out after side effect");
                    error.nativeDispatch = "unknown";
                    throw error;
                }
                return {
                    available: true, xid: 10010, qtile_moved: true, observed_group: "c",
                    observed: {
                        WM_WINDOW_ROLE: "abush-browser",
                        _ABUSH_BINDING_NONCE: payload.nonce,
                        _ABUSH_REMOTE_ID: payload.remote_key,
                        _ABUSH_WORKSPACE_ID: payload.workspace_id,
                        _ABUSH_WINDOW_GENERATION: String(payload.generation),
                        _ABUSH_CONTROL_STATE: payload.control_state,
                        _ABUSH_DISPLAY_TITLE: payload.display_title,
                    },
                };
            }
            return {};
        };
        await assert.rejects(executor.ensure(ensurePayload({
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: executor.pageRef(record),
        })), /response timed out/);
        assert.equal(binds, 2,
            "unknown bind completion must always serialize an old-identity rebind");
        assert.equal(record.connectionId, "c_old00000");
        assert.equal(record.executorFence.epoch, 1);
        assert.strictEqual(executor.workspaces.get(
            executor.key(record.remoteKey, record.principalId)), record);

        const originalSet = executor.chrome.storage.session.set;
        let partial = true;
        executor.chrome.storage.session.set = async value => {
            await originalSet(value);
            if (partial) {
                partial = false;
                executor.chrome.storage.session.get = async () => ({...value});
                throw new Error("storage response lost after commit");
            }
        };
        await assert.rejects(executor.ensure(ensurePayload({
            request_id: "partial-commit", ensure_id: "partial-commit",
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: executor.pageRef(record),
        })), /storage response lost/);
        assert.equal(record.connectionId, "c_old00000",
            "partial durable bind commit must rollback the in-memory old record");
        assert(calls.some(call => call[0] === "bind_x11_window" &&
            call[1].workspace_id === "workspace-authoritative"),
        "partial durable bind commit must rebind the old X11 identity");

        binds = 0;
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") {
                binds += 1;
                throw new Error(binds === 1 ? "candidate timed out" : "rollback failed");
            }
            return {};
        };
        await assert.rejects(executor.ensure(ensurePayload({
            request_id: "unknown-rebind", ensure_id: "unknown-rebind",
            connection_id: "c_12345678", executor_fence: {...fence, epoch: 2},
            expected_page: executor.pageRef(record),
        })), error => error instanceof AggregateError && error.browserCode === "unknown_effect");
        assert.equal(executor.workspaces.has(
            executor.key(record.remoteKey, record.principalId)), false,
        "unprovable rollback must quarantine the old record instead of keeping it active");
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_old00000", executorFence: {...fence, epoch: 1},
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
        while (!executor.ensureFlights.has(executor.key(payload.remote_key,
            payload.principal_id))) await Promise.resolve();
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
        while (!executor.ensureFlights.has(executor.key(firstPayload.remote_key,
            firstPayload.principal_id))) await Promise.resolve();
        const firstCancel = executor.ensureIdentity(firstPayload);
        const cancelling = executor.cancelEnsure(firstCancel);
        assert.equal(await executor.cancelEnsure({...firstCancel, request_id: "stale"}), false);
        created.resolve();
        assert.equal(await cancelling, true);
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
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
        })), error => /bind failed/.test(error.message) ||
            (error instanceof AggregateError && error.browserCode === "unknown_effect" &&
                error.errors.some(inner => /bind failed/.test(inner.message))));
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
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
        })), error => /persist failed/.test(error.message) ||
            (error instanceof AggregateError && error.browserCode === "unknown_effect" &&
                error.errors.some(inner => /persist failed/.test(inner.message))));
        assert.equal(record.connectionId, "c_old00000");
        assert.equal(record.executorFence.epoch, 1);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0);
    }
    {
        const legacy = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "1".repeat(32), connectionDisplayName: "local",
        };
        const localState = {abushOwnedMarkers: {[legacy.nonce]: true}};
        const {executor, calls} = fixture([legacy], [], 0, false, null, localState);
        await executor.restore();
        const migrated = executor.workspaces.get(executor.key(
            legacy.remoteKey, legacy.principalId));
        assert(migrated, "exact live legacy record must migrate instead of being discarded");
        assert.equal(migrated.x11WindowId, 42);
        assert.equal(localState.abushOwnedMarkers[legacy.nonce].version, 2);
        assert.deepEqual(executor.pageRef(migrated), {
            window: {workspace_id: legacy.workspaceId, window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1,
        });
        assert(calls.some(call => call[0] === "bind_x11_window"));
    }
    {
        const legacy = {
            remoteKey: "r_12345678", connectionId: "c_12345678",
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([legacy]);
        await executor.restore();
        assert.equal(executor.workspaces.size, 0);
        assert.deepEqual(calls.find(call => call[0] === "persist")[1].abushWorkspaces, []);
        assert(!calls.some(call => call[0] === "createWindow"), "restore must not create windows");
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_12345678", executorFence: fence,
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        calls.length = 0;
        await executor.windowRemoved(record.windowId);
        assert.equal(executor.workspaces.size, 0,
            "a user-closed managed window must be forgotten before Chrome exits");
        assert.equal(executor.windowOwners.has(record.windowId), false);
        assert.equal(executor.tabOwners.has(record.pageTabId), false);
        assert.deepEqual(calls.find(call => call[0] === "persist")[1].abushWorkspaces, [],
            "window close must durably remove restore ownership before marker cleanup");
        const persistIndex = calls.findIndex(call => call[0] === "persist");
        const outboxIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushTopologyOutbox));
        const markerIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushOwnedMarkers);
        assert(outboxIndex >= 0 && persistIndex > outboxIndex && markerIndex > persistIndex,
            "durable report/workspace removal must precede durable marker disown");
        assert(calls.some(call => call[0] === "detach" && call[1].tabId === record.pageTabId));
        await executor.windowRemoved(record.windowId);
        assert.equal(calls.filter(call => call[0] === "persist").length, 1,
            "duplicate window removal must be idempotent");
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_12345678", executorFence: fence,
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        calls.length = 0;
        executor.chrome.windows.getAll = async () => [
            {id: 10, type: "normal", focused: false},
        ];
        await executor.windowRemoved(record.windowId);
        assert.equal(executor.workspaces.size, 0,
            "closing the final managed window must clear ownership without a restore race");
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "a window already removed by the user must not be closed again");
        assert.deepEqual(calls.find(call => call[0] === "persist")[1].abushWorkspaces, []);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_12345678", executorFence: fence,
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const localState = {};
        const {executor, calls} = fixture([record], [], 0, false, null, localState);
        await executor.restore();
        calls.length = 0;
        executor.chrome.windows.getAll = async () => [
            {id: 1, type: "normal", focused: false},
            {id: 10, type: "normal", focused: false},
        ];
        executor.removedHostSnapshots.set(1, {
            tabCount: 1,
            urls: ["https://ordinary-0.test/"],
        });
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [
                {window: {sessionId: "wrong-ordinary-session",
                    tabs: [{url: "https://newer-but-different.test/"}]}},
                {window: {sessionId: "ordinary-host-session",
                    tabs: [{url: "https://ordinary-0.test/"}]}},
            ],
            restore: async sessionId => { calls.push(["restoreSession", sessionId]); },
        };
        await executor.windowRemoved(1);
        assert.equal(executor.workspaces.size, 0,
            "closing the ordinary host first must retire the last managed window");
        assert.deepEqual(calls.filter(call => call[0] === "createWindow").map(call => call[1]), [
            {url: "about:blank", focused: false, type: "normal"},
        ], "a neutral host must replace the closed ordinary window before managed cleanup");
        assert(calls.some(call => call[0] === "removeWindow" &&
            call[1] === record.windowId),
            "the extension must close the last managed window before Chrome records it " +
            "as last session");
        const persistIndex = calls.findIndex(call => call[0] === "persist");
        const closeIndex = calls.findIndex(call => call[0] === "removeWindow");
        assert(persistIndex >= 0 && closeIndex > persistIndex,
            "durable ownership removal must precede closing the last managed window");
        const createIndex = calls.findIndex(call => call[0] === "createWindow");
        assert(createIndex > closeIndex,
            "neutral host creation must follow managed cleanup so Chrome keeps running");
        const intentIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushRestoreHost?.sessionId === "ordinary-host-session");
        assert(intentIndex >= 0 && intentIndex < closeIndex && createIndex > intentIndex,
            "exact ordinary session ownership must commit before managed cleanup");
        assert.deepEqual(localState.abushRestoreHost, {
            version: 2,
            phase: "prepared",
            sessionId: "ordinary-host-session",
            snapshot: {tabCount: 1, urls: ["https://ordinary-0.test/"]},
        }, "the removed window snapshot must select its own session, not a newer ordinary entry");
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative",
            connectionId: "c_12345678", executorFence: fence,
            logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record], [], 1);
        await executor.restore();
        calls.length = 0;
        executor.chrome.windows.getAll = async () => [
            {id: 1, type: "normal", focused: false},
            {id: 10, type: "normal", focused: false},
            {id: 2, type: "normal", focused: false},
        ];
        await executor.windowRemoved(1);
        assert.equal(executor.workspaces.size, 1,
            "another ordinary window must keep managed browser workspaces alive");
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0);
    }
    {
        const firstRecord = {
            remoteKey: "r_aaaaaaaa", principalId: "principal-a", workspaceId: "workspace-a",
            connectionId: "c_aaaaaaaa", executorFence: fence, logicalWindowId: "window-a",
            logicalPageId: "page-a", generation: 1, windowId: 10, x11WindowId: 10010,
            statusTabIds: [110, 111], pageTabId: 210, nonce: "a".repeat(32),
        };
        const secondRecord = {
            remoteKey: "r_bbbbbbbb", principalId: "principal-b", workspaceId: "workspace-b",
            connectionId: "c_bbbbbbbb", executorFence: fence, logicalWindowId: "window-b",
            logicalPageId: "page-b", generation: 1, windowId: 20, x11WindowId: 10020,
            statusTabIds: [120, 121], pageTabId: 220, nonce: "b".repeat(32),
        };
        const {executor, calls} = fixture([firstRecord, secondRecord]);
        await executor.restore();
        calls.length = 0;
        executor.chrome.windows.getAll = async () => [
            {id: 1, type: "normal", focused: false},
            {id: 10, type: "normal", focused: false},
            {id: 20, type: "normal", focused: false},
        ];
        executor.removedHostSnapshots.set(1, {
            tabCount: 1,
            urls: ["https://ordinary-0.test/"],
        });
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [{
                window: {sessionId: "ordinary-host-session",
                    tabs: [{url: "https://ordinary-0.test/"}]},
            }],
        };
        await executor.windowRemoved(1);
        assert.equal(executor.workspaces.size, 0);
        assert.deepEqual(calls.filter(call => call[0] === "persist")
            .map(call => call[1].abushWorkspaces), [[]],
        "all managed windows must leave durable restore state in one write");
        assert.deepEqual(calls.filter(call => call[0] === "removeWindow")
            .map(call => call[1]), [10, 20]);
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
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
        const invalidResults = [
            {},
            {available: false, xid: 42, observed: {}, qtile_moved: true, observed_group: "c"},
            {available: true, xid: 0, observed: {}, qtile_moved: true, observed_group: "c"},
            {available: true, xid: 42, observed: {}, qtile_moved: false, observed_group: "c"},
            {available: true, xid: 42, observed: {}, qtile_moved: true, observed_group: "f"},
        ];
        for (const [index, result] of invalidResults.entries()) {
            const {executor, calls} = fixture();
            executor.nativeRequest = async (_service, operation, payload) => {
                calls.push([operation, payload]);
                if (operation === "bind_x11_window") return result;
                return {};
            };
            await assert.rejects(executor.ensure(ensurePayload({
                request_id: `strict-bind-${index}`,
                ensure_id: `strict-bind-${index}`,
            })), /fully verified/);
            assert.equal(executor.workspaces.size, 0);
            assert.equal(calls.filter(call => call[0] === "persist" &&
                call[1].abushWorkspaces.length > 0).length, 0,
            "invalid native bind must never commit a managed record");
            assert(!calls.some(call => call[0] === "ensure_managed_page_completion" &&
                call[1].completion.outcome === "created"));
        }
    }
    {
        const {executor, calls} = fixture();
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation !== "bind_x11_window") return {};
            return {
                available: true,
                xid: 42,
                observed: {
                    WM_WINDOW_ROLE: "abush-browser",
                    _ABUSH_BINDING_NONCE: payload.nonce,
                    _ABUSH_REMOTE_ID: payload.remote_key,
                    _ABUSH_WORKSPACE_ID: payload.workspace_id,
                    _ABUSH_WINDOW_GENERATION: String(payload.generation),
                    _ABUSH_CONTROL_STATE: payload.control_state,
                    _ABUSH_DISPLAY_TITLE: `${payload.display_title}-wrong`,
                },
                qtile_moved: true,
                observed_group: "c",
            };
        };
        await assert.rejects(executor.ensure(ensurePayload()), /fully verified/);
        assert.equal(executor.workspaces.size, 0);
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
        const {executor, calls} = fixture();
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") throw new Error("bind failed");
            return {};
        };
        executor.chrome.windows.remove = async id => {
            calls.push(["removeWindow", id]);
            throw new Error("window remove failed");
        };
        await assert.rejects(executor.ensure(ensurePayload()), error =>
            error instanceof AggregateError && error.browserCode === "unknown_effect" &&
            error.errors.some(inner => /bind failed/.test(inner.message)) &&
            error.errors.some(inner => /window remove failed/.test(inner.message)));
        const completion = calls.find(call => call[0] === "ensure_managed_page_completion")[1]
            .completion;
        assert.deepEqual(completion, {outcome: "rejected", code: "unknown_effect"},
            "failed bind cleanup must never claim a safely not-applied result");
        assert(calls.some(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushCleanupTombstones)));
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
        const disconnected = await pending.catch(error => error);
        assert.match(disconnected.message, /disconnected/);
        assert.equal(disconnected.nativeDispatch, "unknown");
        assert.deepEqual(events, ["timeout", "disconnect"]);
        assert.equal(sent.length, 2);
    }
    {
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210, nonce: "0".repeat(32),
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
        await assert.rejects(executor.ensure(ensurePayload()), error =>
            error instanceof AggregateError && error.errors.some(inner =>
                /persist failed/.test(inner.message)));
        assert.equal(executor.workspaces.size, 0,
            "failed persist must roll back in-memory install");
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
        calls.length = 0;
        const persisted = deferred();
        executor.chrome.storage.session.set = async value => {
            calls.push(["persist", structuredClone(value)]);
            await persisted.promise;
        };
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9999, windowId: record.windowId, index: 3, pinned: false});
        const converging = executor.tabCreated(tabs.at(-1));
        while (!calls.some(call => call[0] === "persist")) await Promise.resolve();
        assert.strictEqual(executor.workspaces.get(
            executor.key(record.remoteKey, record.principalId)), record,
        "persist-before-effect must retain in-memory authority until the write commits");
        assert.equal(calls.some(call => call[0] === "detach"), false);
        persisted.resolve();
        await converging;
        assert.equal(executor.workspaces.size, 0);
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        calls.length = 0;
        executor.chrome.storage.local.set = async value => {
            calls.push(["persistLocal", structuredClone(value)]);
            if (Array.isArray(value.abushTopologyOutbox)) throw new Error("local write failed");
        };
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9999, windowId: record.windowId, index: 3, pinned: false});
        await assert.rejects(executor.tabMoved(record.pageTabId, {windowId: 999}),
            /local write failed/);
        assert.strictEqual(executor.workspaces.get(
            executor.key(record.remoteKey, record.principalId)), record,
        "outbox failure must preserve the only retry source");
        assert.equal(calls.some(call => call[0] === "persist"), false);
        assert.equal(calls.some(call => call[0] === "detach"), false);
        assert.equal(calls.some(call => call[0] === "clear_x11_window"), false);
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        calls.length = 0;
        executor.chrome.storage.session.set = async value => {
            calls.push(["persist", structuredClone(value)]);
            throw new Error("session write failed");
        };
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9999, windowId: record.windowId, index: 3, pinned: false});
        await assert.rejects(executor.tabMoved(record.pageTabId, {windowId: 999}),
            /session write failed/);
        assert.strictEqual(executor.workspaces.get(
            executor.key(record.remoteKey, record.principalId)), record,
        "workspace persistence failure must not forget the active owner");
        const outboxIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushTopologyOutbox));
        const persistIndex = calls.findIndex(call => call[0] === "persist");
        assert(outboxIndex >= 0 && persistIndex > outboxIndex);
        assert.equal(calls.some(call => call[0] === "detach"), false);
        assert.equal(calls.some(call => call[0] === "clear_x11_window"), false);
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const before = executor.pageRef(record);
        calls.length = 0;
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "managed_page_moved_outside") throw new Error("daemon offline");
            if (operation === "clear_x11_window") {
                return {cleared: true, xid: payload.xid, reclassified_group: "f"};
            }
            return {};
        };
        const advisory = [];
        executor.onAdvisoryError = error => advisory.push(error);
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9999, windowId: record.windowId, index: 3, pinned: false});
        await executor.tabMoved(record.pageTabId, {windowId: 999});
        assert.equal(executor.workspaces.size, 0,
            "native report failure must not block local ownership convergence");
        const names = calls.map(call => call[0]);
        const outboxIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushTopologyOutbox) && call[1].abushTopologyOutbox.length === 1);
        const persistIndex = names.indexOf("persist");
        const detachIndex = names.indexOf("detach");
        const disownIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushOwnedMarkers);
        const clearIndex = names.indexOf("clear_x11_window");
        const reportIndex = names.indexOf("managed_page_moved_outside");
        assert(outboxIndex >= 0 && persistIndex > outboxIndex && detachIndex > persistIndex &&
            clearIndex > detachIndex && disownIndex > clearIndex && reportIndex > disownIndex,
        "durable report/removal must precede detach/cleanup/disown and terminal ACK");
        assert.deepEqual(calls.find(call => call[0] === "managed_page_moved_outside")[1].page,
            before, "pageRef must be captured before the record is forgotten");
        const outboxWrite = calls.find(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushTopologyOutbox) && call[1].abushTopologyOutbox.length === 1);
        assert(outboxWrite, "failed topology reports must remain durable in storage.local");
        assert(calls.some(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushTopologyOutbox)),
        "failed topology report must remain visible through the durable outbox");
        assert.equal(advisory.length, 1,
            "report failure must trigger a user-visible advisory callback");
    }
    {
        const {executor, calls} = fixture();
        const bindGate = deferred();
        const originalNative = executor.nativeRequest;
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") await bindGate.promise;
            return originalNative(service, operation, payload);
        };
        const ensuring = executor.ensure(ensurePayload());
        while (!calls.some(call => call[0] === "bind_x11_window")) await Promise.resolve();
        const unrelated = executor.enqueueLifecycle(() => false);
        bindGate.resolve();
        const record = await ensuring;
        await unrelated;
        assert(record, "unrelated lifecycle work must not reject an owned transaction");
    }
    {
        const {executor, calls} = fixture();
        const bindGate = deferred();
        const originalNative = executor.nativeRequest;
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") await bindGate.promise;
            return originalNative(service, operation, payload);
        };
        const ensuring = executor.ensure(ensurePayload());
        while (!calls.some(call => call[0] === "bind_x11_window")) await Promise.resolve();
        const flight = executor.ensureFlights.get(executor.key("r_12345678", "principal-one"));
        const createdWindowId = flight.createdWindowId;
        const tabs = await executor.chrome.tabs.query({windowId: createdWindowId});
        const fourth = {id: 9999, windowId: createdWindowId, index: 3, pinned: false};
        tabs.push(fourth);
        const event = executor.enqueueLifecycle(() => {
            executor.observeChromeEvent("tabCreated", {tab: fourth});
            return executor.tabCreated(fourth);
        });
        bindGate.resolve();
        await assert.rejects(ensuring, /workspace changed during bind/);
        await event;
        assert.equal(executor.workspaces.size, 0,
            "an event queued before install/commit must not leave a managed record");
    }
    {
        const {executor} = fixture();
        const record = await executor.ensure(ensurePayload());
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        const page = tabs.find(tab => tab.id === record.pageTabId);
        const detached = deferred();
        const detachEvent = executor.enqueueLifecycle(async () => {
            await detached.promise;
            return executor.tabDetached(record.pageTabId, {oldWindowId: record.windowId});
        });
        tabs.splice(tabs.indexOf(page), 1);
        page.windowId = record.windowId;
        page.index = 2;
        tabs.push(page);
        tabs.sort((left, right) => left.index - right.index);
        detached.resolve();
        await detachEvent;
        await executor.enqueueLifecycle(() =>
            executor.tabAttached(record.pageTabId, {newWindowId: record.windowId}));
        assert.strictEqual(executor.workspaces.get(
            executor.key(record.remoteKey, record.principalId)), record,
        "delayed self-generated detach must re-check exact live topology");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        calls.length = 0;
        const liveGate = deferred();
        const originalIsLive = executor.isLive.bind(executor);
        let checked = false;
        executor.isLive = async candidate => {
            const live = await originalIsLive(candidate);
            if (!checked) {
                checked = true;
                await liveGate.promise;
            }
            return live;
        };
        const reconciling = executor.tabCreated({id: 9999, windowId: record.windowId});
        while (!checked) await Promise.resolve();
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        const fourth = {id: 9999, windowId: record.windowId, index: 3, pinned: false};
        tabs.push(fourth);
        const fourthEvent = executor.enqueueLifecycle(() => executor.tabCreated(fourth));
        liveGate.resolve();
        await Promise.all([reconciling, fourthEvent]);
        assert.equal(executor.workspaces.size, 0,
            "a fourth tab arriving after an isLive check must be caught by its own event");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        calls.length = 0;
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9999, windowId: record.windowId, index: 3, pinned: false,
            url: "https://fourth-tab.test/"});
        await executor.tabCreated(tabs.at(-1));
        assert.equal(executor.workspaces.size, 0,
            "a fourth managed-window tab must trigger convergence");
        assert(calls.some(call => call[0] === "managed_page_moved_outside"));
    }
    {
        const mutations = [
            ["marker reorder", (record, tabs) => { tabs[0].index = 1; tabs[1].index = 0; }],
            ["marker unpin", (_record, tabs) => { tabs[0].pinned = false; }],
            ["marker url", (record, tabs) => { tabs[0].url = `${record.nonce}-wrong`; }],
            ["marker title", (_record, tabs) => { tabs[0].title = "wrong"; }],
            ["business pin", (record, tabs) => {
                tabs.find(tab => tab.id === record.pageTabId).pinned = true;
            }],
        ];
        for (const [name, mutate] of mutations) {
            const {executor} = fixture();
            const record = await executor.ensure(ensurePayload({
                request_id: `topology-${name}`, ensure_id: `topology-${name}`,
            }));
            const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
            mutate(record, tabs);
            await executor.tabUpdated(record.statusTabIds[0], {title: "changed"}, tabs[0]);
            assert.equal(executor.workspaces.size, 0, `${name} must converge locally`);
        }
    }
    {
        const {executor} = fixture();
        const record = await executor.ensure(ensurePayload());
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        const index = tabs.findIndex(tab => tab.id === record.pageTabId);
        tabs.splice(index, 1);
        await executor.tabRemoved(record.pageTabId, {windowId: record.windowId});
        assert.equal(executor.workspaces.size, 0,
            "business close must not leave managed ownership");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const payload = {
            ...executor.ensureIdentity(ensurePayload()),
            window_id: record.logicalWindowId,
            page_id: record.logicalPageId,
        };
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.splice(tabs.findIndex(tab => tab.id === record.pageTabId), 1);
        calls.length = 0;
        assert.equal(await executor.retireManagedPage(payload), true);
        assert.equal(executor.workspaces.size, 0);
        assert.equal(calls.filter(call =>
            call[0] === "ensure_managed_page_cancellation_completion").length, 1);
        assert.equal(await executor.retireManagedPage(payload), true,
            "duplicate retire replay must converge without waiting");
        assert.equal(calls.filter(call =>
            call[0] === "ensure_managed_page_cancellation_completion").length, 2,
        "duplicate delivery after ACK loss must replay the terminal completion");
        await assert.rejects(executor.retireManagedPage({...payload, page_id: "other"}),
            /conflicting duplicate retire request/);
    }
    {
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        const payload = {
            ...first.executor.ensureIdentity(ensurePayload()),
            window_id: record.logicalWindowId,
            page_id: record.logicalPageId,
        };
        const originalPersist = first.executor.persistRetireLedger.bind(first.executor);
        let writes = 0;
        first.executor.persistRetireLedger = async () => {
            await originalPersist();
            if (++writes === 1) throw new Error("crash after retire acceptance");
        };
        await assert.rejects(first.executor.retireManagedPage(payload),
            /crash after retire acceptance/);
        assert.equal(first.executor.workspaces.size, 1,
            "accepted-before-effect crash must preserve physical ownership for restart recovery");
        assert.equal(localState.abushRetireLedger[0].status, "accepted");
        const second = fixture([record], [], 0, false, null, localState);
        await second.executor.restore();
        assert.equal(second.executor.workspaces.size, 1,
            "restart must retain accepted retire work until a peer can receive its ACK");
        await second.executor.remoteState({
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            executor_fence: payload.executor_fence,
            connected: true,
        });
        assert.equal(second.executor.workspaces.size, 0,
            "connected peer recovery must finish the accepted retire before ACK");
        assert.equal(localState.abushRetireLedger[0].status, "completed");
        assert(second.calls.some(call =>
            call[0] === "ensure_managed_page_cancellation_completion"));
        const rotated = {...payload, connection_id: "c_new00000",
            executor_fence: {executor_id: "executor-new", epoch: 2}};
        await second.executor.remoteState({
            remote_key: rotated.remote_key,
            connection_id: rotated.connection_id,
            principal_id: rotated.principal_id,
            executor_fence: rotated.executor_fence,
            connected: true,
        });
        assert(second.calls.some(call =>
            call[0] === "ensure_managed_page_cancellation_completion" &&
            call[1].connection_id === rotated.connection_id &&
            JSON.stringify(call[1].executor_fence) ===
                JSON.stringify(rotated.executor_fence)),
        "completed retire identity must reauthorize its ACK on a new fence");
    }
    {
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        const payload = {
            ...first.executor.ensureIdentity(ensurePayload()),
            window_id: record.logicalWindowId,
            page_id: record.logicalPageId,
        };
        await first.executor.retireManagedPage(payload);
        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restore();
        second.calls.length = 0;
        assert.equal(await second.executor.retireManagedPage(payload), true);
        assert.equal(second.calls.filter(call =>
            call[0] === "ensure_managed_page_cancellation_completion").length, 1,
        "service-worker restart must replay the durable terminal retire completion");
    }
    {
        const report = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", request_id: "ack-loss",
            executor_fence: fence,
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1},
        };
        const localState = {abushTopologyOutbox: [report]};
        const first = fixture([], [], 0, false, null, localState);
        first.executor.nativeRequest = async (_service, operation, payload) => {
            first.calls.push([operation, payload]);
            throw new Error("response lost after daemon ACK");
        };
        await first.executor.restore();
        await assert.rejects(first.executor.flushTopologyOutbox(), /pending retry/);
        assert.equal(localState.abushTopologyOutbox.length, 1,
            "response loss must keep the durable request for restart retry");
        assert.equal(localState.abushTopologyOutbox[0].request_id, report.request_id);
        assert.deepEqual(localState.abushTopologyOutbox[0].page, report.page);
        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restore();
        await second.executor.flushTopologyOutbox();
        assert.deepEqual(localState.abushTopologyOutbox, []);
        assert.equal(second.calls.filter(call =>
            call[0] === "managed_page_moved_outside")[0][1].request_id, "ack-loss");
    }
    {
        const report = {
            remote_key: "r_12345678",
            connection_id: "c_12345678",
            principal_id: "principal-one",
            request_id: "topology-retry",
            executor_fence: fence,
            page: {
                window: {workspace_id: "workspace-authoritative",
                    window_id: logicalWindowId, generation: 7},
                page_id: logicalPageId,
                generation: 9,
            },
        };
        const localState = {abushTopologyOutbox: [report, {...report,
            request_id: "duplicate-correlation"}, {bad: true}]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restore();
        await executor.flushTopologyOutbox();
        assert.equal(calls.filter(call => call[0] === "managed_page_moved_outside").length, 2,
            "different correlation IDs must each receive a terminal ACK");
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "startup must drain a successfully reported durable topology outbox");
    }
    {
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9999, windowId: record.windowId, index: 3, pinned: false});
        await executor.tabMoved(record.pageTabId, {windowId: 999});
        const report = calls.find(call => call[0] === "managed_page_moved_outside");
        assert(report, "moving a managed page must report topology after local convergence");
        assert.deepEqual(report[1].page, executor.pageRef(record));
        assert.equal(executor.workspaces.size, 0);
    }
    {
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        first.executor.nativeRequest = async (_service, operation, payload) => {
            first.calls.push([operation, payload]);
            if (operation === "managed_page_moved_outside") throw new Error("offline");
            if (operation === "clear_x11_window") {
                return {cleared: true, xid: payload.xid, reclassified_group: "f"};
            }
            return {};
        };
        const tabs = await first.executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9988, windowId: record.windowId, index: 3, pinned: false});
        await first.executor.tabMoved(record.pageTabId, {windowId: 999});
        const stored = localState.abushTopologyOutbox[0];
        assert.equal(stored.cleanup.phase, "removed");
        const originalRequestId = stored.request_id;
        const originalReportId = stored.report_id;
        const rotatedFence = {executor_id: "executor-new", epoch: 2};
        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restoreTopologyOutbox();
        await second.executor.authorizeTopologyOutbox({
            remote_key: stored.remote_key,
            connection_id: "c_new00000",
            principal_id: stored.principal_id,
            executor_fence: rotatedFence,
            owner_display: ensurePayload().owner_display,
            connected: true,
        });
        await second.executor.flushTopologyOutbox();
        const resent = second.calls.find(call => call[0] === "managed_page_moved_outside")[1];
        assert.equal(resent.request_id, originalRequestId);
        assert.equal(resent.report_id, originalReportId);
        assert.equal(resent.connection_id, "c_new00000");
        assert.deepEqual(resent.executor_fence, rotatedFence);
    }
    {
        const localState = {};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        await executor.restore();
        calls.length = 0;
        const originalCreateTab = executor.chrome.tabs.create;
        const originalCreateWindow = executor.chrome.windows.create;
        const originalMove = executor.chrome.tabs.move;
        const originalUpdate = executor.chrome.tabs.update;
        const emitted = [];
        executor.chrome.tabs.create = async properties => {
            const tab = await originalCreateTab(properties);
            emitted.push(executor.observeChromeEvent("tabCreated", {tab}));
            return tab;
        };
        executor.chrome.windows.create = async properties => {
            const window = await originalCreateWindow(properties);
            emitted.push(await executor.observeWindowCreated({
                id: window.id, type: "normal", tabs: window.tabs,
            }));
            return window;
        };
        executor.chrome.tabs.move = async (tabId, properties) => {
            const tab = await executor.chrome.tabs.get(tabId);
            emitted.push(executor.observeChromeEvent("tabDetached", {
                tabId, oldWindowId: tab.windowId,
            }));
            const moved = await originalMove(tabId, properties);
            emitted.push(executor.observeChromeEvent("tabAttached", {
                tabId, newWindowId: properties.windowId,
            }));
            return moved;
        };
        executor.chrome.tabs.update = async (tabId, properties) => {
            const updated = await originalUpdate(tabId, properties);
            if (properties.pinned === true) {
                emitted.push(executor.observeChromeEvent("tabUpdated", {
                    tabId, changeInfo: {pinned: true}, tab: await executor.chrome.tabs.get(tabId),
                }));
            }
            return updated;
        };
        const record = await executor.ensure(ensurePayload({
            request_id: "real-events", ensure_id: "real-events",
        }));
        assert(record && emitted.length >= 8 && emitted.every(Boolean),
            "real create/attach/detach/pin events must be owned and reconciled by the transaction");
    }
    {
        const localState = {};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        await executor.restore();
        calls.length = 0;
        const record = await executor.ensure(ensurePayload());
        assert(record);
        const transactionWrites = calls.filter(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushPendingTransactions));
        const firstEffect = calls.findIndex(call =>
            ["createTab", "createWindow", "move", "update"].includes(call[0]));
        const firstTransaction = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushPendingTransactions?.length === 1);
        assert(firstTransaction >= 0 && firstTransaction < firstEffect,
            "pending transaction must be durable before the first physical effect");
        assert(transactionWrites.some(call => call[1].abushPendingTransactions.some(saved =>
            saved.window_id === record.windowId && saved.page_tab_id === record.pageTabId &&
            saved.xid === record.x11WindowId)),
        "write-ahead transaction must accumulate exact physical and X11 identities");
        assert.deepEqual(localState.abushPendingTransactions, [],
            "committed transaction must be consumed after durable workspace commit");
    }
    {
        const localState = {};
        const first = fixture([], [], 1, false, null, localState);
        const createGate = deferred();
        const originalCreate = first.executor.chrome.windows.create;
        first.executor.chrome.windows.create = async properties => {
            await createGate.promise;
            return originalCreate(properties);
        };
        const payload = ensurePayload();
        const ensuring = first.executor.ensure(payload);
        while (!localState.abushPendingTransactions?.some(transaction =>
            transaction.status_tab_ids.length === 2)) await Promise.resolve();
        const persisted = structuredClone(localState.abushPendingTransactions);
        createGate.resolve();
        await ensuring;
        const crashState = {abushPendingTransactions: persisted};
        const second = fixture([], [], 1, false, null, crashState);
        await second.executor.restorePendingTransactions();
        assert.deepEqual(crashState.abushPendingTransactions, [],
            "restart must consume a write-ahead transaction from before windows.create");
    }
    {
        const nonce = "c".repeat(32);
        const localState = {abushCleanupTombstones: [{
            nonce, window_id: 710, status_tab_ids: [1810, 1811], page_tab_id: 1812,
            errors: ["legacy"],
        }]};
        const restoredPopup = {windowId: 710, tabIds: [1810, 1811, 1812], nonce,
            businessUrl: "about:blank"};
        const {executor} = fixture([], [], 0, false, restoredPopup, localState);
        await executor.restoreCleanupTombstones();
        assert.deepEqual(localState.abushCleanupTombstones, [],
            "v0 cleanup proof must migrate and retry instead of being silently dropped");
    }
    {
        const {executor} = fixture();
        const record = await executor.ensure(ensurePayload());
        const oldPageTabId = record.pageTabId;
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        const oldPage = tabs.find(tab => tab.id === oldPageTabId);
        tabs.splice(tabs.indexOf(oldPage), 1);
        const replacement = {...oldPage, id: 9876, index: 2, pinned: false};
        tabs.push(replacement);
        await executor.tabReplaced(replacement.id, oldPageTabId);
        assert.equal(record.pageTabId, replacement.id);
        assert.equal(executor.tabOwners.has(oldPageTabId), false);
        assert.equal(executor.tabOwners.get(replacement.id),
            executor.key(record.remoteKey, record.principalId));
    }
    {
        const {executor, calls} = fixture();
        await executor.status();
        assert.deepEqual(calls.map(call => call[0]), ["refresh_remotes"]);
        assert(!calls.some(call => call[0] === "createWindow"), "status must not create windows");
    }
    console.log("workspace executor tests passed");
})().catch(error => { console.error(error); process.exitCode = 1; });
