#!/usr/bin/env node

const assert = require("assert");
const crypto = require("crypto").webcrypto;
global.crypto ||= crypto;
const executorHash = async value => {
    const digest = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(value));
    return [...new Uint8Array(digest)]
        .map(byte => byte.toString(16).padStart(2, "0")).join("");
};

const {WorkspaceExecutor, BROWSER_ERROR_CODES, displayTitle, STATUS_PREFIX,
    MARKER_IDS} = require(
    "../chromium/external-links/extension/workspace.js");
const {NativeDispatcher, connectWorkspaceAfterRestore, createWorkspaceInitializer,
    registerNativeEventRouter, registerWorkspaceWindowListeners, registerRuntimeMessageRouter} =
    require(
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

const WAIT_TIMEOUT_MS = 1000;
let activeCase = "startup";
let caseNumber = 0;

function beginCase() {
    const caller = new Error().stack.split("\n")[2]?.trim() || "unknown location";
    activeCase = caller.replace(/^at\s+/, "");
    caseNumber += 1;
    if (process.env.ABUSH_TEST_TRACE === "1") {
        console.error(`[workspace-test] ${caseNumber}: ${activeCase}`);
    }
}

function timeoutError(label) {
    return new Error(`${activeCase}: timed out waiting for ${label}`);
}

async function waitFor(predicate, label, timeoutMs = WAIT_TIMEOUT_MS) {
    const deadline = Date.now() + timeoutMs;
    while (!predicate()) {
        if (Date.now() >= deadline) throw timeoutError(label);
        await new Promise(resolve => setImmediate(resolve));
    }
}

async function exactWait(promise, label, timeoutMs = WAIT_TIMEOUT_MS) {
    let timer;
    try {
        return await Promise.race([
            promise,
            new Promise((_, reject) => {
                timer = setTimeout(() => reject(timeoutError(label)), timeoutMs);
            }),
        ]);
    } finally {
        if (timer !== undefined) clearTimeout(timer);
    }
}

function deferred(label = "deferred promise") {
    let resolve;
    let reject;
    const pending = new Promise((resolvePromise, rejectPromise) => {
        resolve = resolvePromise;
        reject = rejectPromise;
    });
    return {promise: exactWait(pending, label), resolve, reject};
}

function typedResponse(operation, payload) {
    const types = {
        executor_completion: "completion_accepted",
        cancellation_completion: "cancellation_completion_accepted",
        ensure_managed_page_completion: "ensure_managed_page_completion_accepted",
        ensure_managed_page_cancellation_completion: "ensure_managed_page_cancellation_accepted",
    };
    if (!types[operation]) return null;
    const id = operation.startsWith("ensure_") ? "ensure_id" : "effect_id";
    const response = {type: types[operation], request_id: payload.request_id,
        [id]: payload[id], executor_fence: payload.executor_fence};
    if (operation !== "ensure_managed_page_cancellation_completion") {
        response.completion = payload.completion;
    }
    return structuredClone(response);
}

function candidateLeaseResponse(operation, payload) {
    if (operation === "finish_candidate_lease") {
        return {finished: true, version: 1, nonce: payload.nonce, outcome: payload.outcome};
    }
    if (operation === "register_candidate_lease") {
        return {accepted: true, nonce: payload.nonce, admission_version: 2, phase: "reserved"};
    }
    if (operation === "arm_candidate_lease") {
        return {armed: true, nonce: payload.nonce, admission_version: 2, phase: "dispatched"};
    }
    if (operation === "admit_candidate") {
        return {admitted: true, nonce: payload.nonce, xid: 42, admission_version: 2,
            first_group: "__abush_quarantine"};
    }
    if (operation === "release_candidate_lease") {
        return {released: true, nonce: payload.nonce, admission_version: 2,
            phase: "aborted-resolved", denial_retained: false};
    }
    return null;
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
            sendMessage: async (id, message) => {
                calls.push(["sendTabMessage", id, structuredClone(message)]);
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
        const admission = candidateLeaseResponse(operation, payload);
        if (admission) return admission;
        if (operation === "register_candidate_lease") {
            return {accepted: true, nonce: payload.nonce, admission_version: 2, phase: "reserved"};
        }
        if (operation === "release_candidate_lease") {
            return {released: true, nonce: payload.nonce, admission_version: 2,
            phase: "aborted-resolved", denial_retained: false};
        }
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
        if (operation === "verify_x11_window") {
            return {verified: true, xid: payload.xid};
        }
        if (operation === "clear_x11_window") {
            return {cleared: true, xid: payload.xid, reclassified_group: "f"};
        }
        if (operation === "managed_page_moved_outside") {
            return {
                request_id: payload.request_id,
                report_id: payload.report_id,
                executor_fence: payload.executor_fence,
                response_delivery_id: `topology-response-${payload.report_id}`,
                result: {outcome: "already_converged"},
            };
        }
        if (operation === "topology_response_consumed") {
            return {
                request_id: payload.request_id,
                report_id: payload.report_id,
                executor_fence: payload.executor_fence,
                response_delivery_id: payload.response_delivery_id,
            };
        }
        if (operation === "retire_managed_page_completion") {
            return {
                request_id: payload.request_id,
                retire_id: payload.retire_id,
                executor_fence: payload.executor_fence,
                response_delivery_id: `retire-response-${payload.retire_id}`,
                origin_executor_fence: payload.origin_executor_fence,
                completion: payload.completion,
            };
        }
        if (operation === "retire_managed_page_ack_consumed") {
            return {
                request_id: payload.request_id,
                retire_id: payload.retire_id,
                executor_fence: payload.executor_fence,
                response_delivery_id: payload.response_delivery_id,
            };
        }
        return typedResponse(operation, payload) || {};
    };
    const executor = new WorkspaceExecutor(chrome, native);
    executor.browserBootId = "test-browser-boot";
    executor.bootstrapResolved = true;
    executor.resolveBootstrap();
    return {executor, calls};
}

async function managedEventFixture({windowFirst = false, lost = null} = {}) {
    const localState = {};
    const {executor, calls} = fixture([], [], 3, false, null, localState);
    const chrome = executor.chrome;
    const trace = [];
    const query = chrome.tabs.query;
    const create = chrome.tabs.create;
    const createWindow = chrome.windows.create;
    const move = chrome.tabs.move;
    const update = chrome.tabs.update;
    let stale = false;
    chrome.tabs.query = async properties => structuredClone((await query(properties))
        .filter(tab => !stale || !tab.pendingUrl));
    const emit = (type, payload) => {
        const owned = executor.observeChromeEvent(type, structuredClone(payload));
        trace.push([type, owned, [...executor.pendingTransactions.values()]
            .some(transaction => transaction.invalid)]);
        assert(owned, `transaction must own ${type}`);
    };
    const reindex = async windowId => {
        (await query({windowId})).forEach((tab, index) => tab.index = index);
    };
    chrome.tabs.create = async properties => {
        const tab = await create(properties);
        tab.pendingUrl = tab.url;
        tab.url = "";
        tab.status = "loading";
        const title = tab.title;
        delete tab.title;
        emit("tabCreated", {tab});
        emit("tabUpdated", {tabId: tab.id, changeInfo: {status: "loading"}, tab});
        const marker = new URLSearchParams(properties.url.split("#")[1]).get("marker");
        if (lost === `create-${marker}`) {
            stale = true;
            throw new Error("lost marker creation result");
        }
        if (lost === "observed-b" && marker === "b") {
            stale = true;
            // The API result supplies the only exact identity when query lags on pendingUrl.
            executor.pendingTransactions.values().next().value.invalid = true;
            return structuredClone(tab);
        }
        const result = structuredClone(tab);
        const settle = () => {
            tab.url = tab.pendingUrl;
            delete tab.pendingUrl;
            tab.title = title;
            tab.status = "complete";
            emit("tabUpdated", {tabId: tab.id,
                changeInfo: {url: tab.url, title, status: "complete"}, tab});
        };
        if (properties.url === "about:blank") settle();
        else setImmediate(settle);
        return result;
    };
    chrome.windows.create = async properties => {
        const source = await chrome.tabs.get(properties.tabId);
        const window = await createWindow(properties);
        await reindex(source.windowId);
        await reindex(window.id);
        const created = async () => {
            const owned = await executor.observeWindowCreated(
                structuredClone({...window, type: "normal"}));
            trace.push(["windowCreated", owned, false]);
            assert(owned);
        };
        if (windowFirst) await created();
        emit("tabDetached", {tabId: source.id, oldWindowId: source.windowId,
            oldPosition: source.index});
        emit("tabAttached", {tabId: source.id, newWindowId: window.id, newPosition: 0});
        if (!windowFirst) await created();
        if (lost === "window") throw new Error("lost window creation result");
        return structuredClone(window);
    };
    chrome.tabs.move = async (id, properties) => {
        const source = await chrome.tabs.get(id);
        emit("tabDetached", {tabId: id, oldWindowId: source.windowId,
            oldPosition: source.index});
        const result = await move(id, properties);
        await reindex(source.windowId);
        await reindex(properties.windowId);
        emit("tabAttached", {tabId: id, newWindowId: properties.windowId,
            newPosition: result.index});
        if (lost === "move") throw new Error("lost move result");
        return structuredClone(result);
    };
    chrome.tabs.update = async (id, properties) => {
        await update(id, properties);
        const tab = await chrome.tabs.get(id);
        if ("pinned" in properties) {
            emit("tabUpdated", {tabId: id, changeInfo: {pinned: properties.pinned}, tab});
            emit("tabUpdated", {tabId: id, changeInfo: {status: "complete"}, tab});
            if (lost === `pin-${tab.index}`) throw new Error("lost pin result");
        }
        return structuredClone(tab);
    };
    return {executor, calls, localState, trace,
        physicalTabs: async () => structuredClone(await query({}))};
}

(async () => {
    const testDeadline = setTimeout(() => {
        console.error(timeoutError("test completion"));
        process.exitCode = 1;
    }, 15000);
    testDeadline.unref();
    try {
    for (const operation of ["executor_completion", "cancellation_completion",
        "ensure_managed_page_completion", "ensure_managed_page_cancellation_completion"]) {
        for (const fault of ["empty", "undefined", "request_id", "id", "executor_fence", "type",
            "completion", "extra"]) {
            if (operation === "ensure_managed_page_cancellation_completion" &&
                    fault === "completion") continue;
            beginCase();
            activeCase = `${operation} rejects ${fault}`;
            const localState = {};
            const {executor} = fixture([], [], 0, false, null, localState);
            const payload = {...ensurePayload(), effect_id: "effect-receipt"};
            const ensure = operation.startsWith("ensure_");
            const cancelled = operation.includes("cancellation");
            const completion = ensure ? {outcome: "created", window_id: logicalWindowId,
                page_id: logicalPageId} : cancelled ? "unknown" : "applied";
            executor.nativeRequest = async (_service, kind, value) => {
                const receipt = typedResponse(kind, value);
                if (fault === "empty") return {};
                if (fault === "undefined") return undefined;
                if (fault === "request_id") receipt.request_id = "wrong";
                if (fault === "id") receipt[ensure ? "ensure_id" : "effect_id"] = "wrong";
                if (fault === "executor_fence") receipt.executor_fence = {...fence, epoch: 2};
                if (fault === "type") receipt.type = "wrong_accepted";
                if (fault === "completion") receipt.completion = ensure ?
                    {outcome: "rejected", code: "invalid_command"} : "not_applied";
                if (fault === "extra") receipt.extra = true;
                return receipt;
            };
            if (ensure) {
                const pending = cancelled ? executor.ensureCancellationCompleted(payload) :
                    executor.completeEnsure(payload, completion);
                await assert.rejects(pending, /receipt was not canonical/);
                assert.equal(localState.abushEnsureCompletions[
                    executor.ensureRequestKey(payload)].phase, "pending_ack");
            } else {
                const identity = {...executor.operationIdentity(payload), command: {command: "x"}};
                executor.executionLedger.set(executor.operationEffectKey(payload), {
                    version: 1, phase: cancelled ? "acked" : "terminal_pending_ack",
                    identity, completion: "applied",
                });
                if (cancelled) {
                    executor.pendingOperationCancels.set(executor.operationKey(payload), [payload]);
                    await executor.persistOperationCancels();
                }
                await executor.persistExecutionLedger();
                await executor.flushExecutionLedger();
                assert.equal(executor.executionLedger.size, 1);
                if (cancelled) assert.equal(executor.pendingOperationCancels.size, 1);
            }
            assert(executor.advisoryErrors.length > 0);
        }
    }
    {
        beginCase();
        activeCase = "unknown create retains WAL after cleanup and worker reconciliation";
        const localState = {};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        const native = executor.nativeRequest;
        let invoked = false;
        executor.chrome.windows.create = async () => {
            invoked = true;
            const wal = localState.abushPendingTransactions[0];
            assert.equal(wal.admission.nonce, wal.nonce);
            assert.equal(wal.admission.phase, "dispatched");
            assert.equal(wal.admission.chrome, "unknown");
            throw new Error("Chrome create outcome unknown");
        };
        executor.nativeRequest = async (service, operation, payload) => {
            if (operation === "release_candidate_lease" && invoked) {
                assert(calls.some(call => call[0] === "remove"),
                    "physical tab cleanup precedes native finish");
                throw new Error("candidate cleanup remains unknown; admission denial retained");
            }
            return native(service, operation, payload);
        };
        await assert.rejects(executor.ensure(ensurePayload()),
            error => error.browserCode === "unknown_effect");
        assert.equal(executor.pendingTransactions.size, 1);
        assert.equal(localState.abushPendingTransactions.length, 1);
        const restarted = fixture([], [], 0, false, null, localState).executor;
        restarted.nativeRequest = executor.nativeRequest;
        await restarted.restorePendingTransactions();
        assert.equal(restarted.pendingTransactions.size, 1);
        assert(restarted.advisoryErrors.length > 0);
    }
    for (const receipt of [
        {released: true, nonce: "a".repeat(32)},
        {released: false, nonce: "a".repeat(32), admission_version: 2,
            phase: "dispatched", denial_retained: true},
        {released: true, nonce: "a".repeat(32), admission_version: 2,
            phase: "aborted-resolved", denial_retained: true},
    ]) {
        beginCase();
        const {executor} = fixture();
        executor.nativeRequest = async () => receipt;
        await assert.rejects(executor.updateCandidateLease("release", "a".repeat(32)),
            /not fully verified/);
    }
    for (const boundary of ["reply", "before_ack_storage", "after_ack_storage"]) {
        for (const cancelled of [false, true]) {
            beginCase();
            activeCase = `ensure ${cancelled ? "cancellation" : "terminal"} loss ${boundary}`;
            const localState = {};
            const first = fixture([], [], 0, false, null, localState);
            const payload = ensurePayload();
            const completion = {outcome: "created", window_id: logicalWindowId,
                page_id: logicalPageId};
            const accepted = deferred("ensure acceptance barrier");
            const release = deferred("ensure receipt release barrier");
            const native = first.executor.nativeRequest;
            first.executor.nativeRequest = async (service, operation, value) => {
                const receipt = await native(service, operation, value);
                accepted.resolve();
                await release.promise;
                if (boundary === "reply") throw new Error("receipt lost");
                return receipt;
            };
            const set = first.executor.chrome.storage.local.set;
            first.executor.chrome.storage.local.set = async value => {
                const entry = Object.values(value.abushEnsureCompletions || {})[0];
                if (entry?.phase === "acked" && boundary !== "reply") {
                    if (boundary === "after_ack_storage") await set(value);
                    throw new Error("ack storage lost");
                }
                await set(value);
            };
            const pending = cancelled ? first.executor.ensureCancellationCompleted(payload) :
                first.executor.completeEnsure(payload, completion);
            const failed = assert.rejects(pending, /lost/);
            await accepted.promise;
            assert.equal(localState.abushEnsureCompletions[
                first.executor.ensureRequestKey(payload)].phase, "pending_ack");
            release.resolve();
            await failed;
            const second = fixture([], [], 0, false, null, localState);
            second.executor.browserBootId = "different-browser-boot";
            await second.executor.restore();
            assert.equal(localState.abushEnsureCompletions[
                second.executor.ensureRequestKey(payload)].phase, "acked");
            assert.equal(second.calls.some(call => ["createTab", "createWindow",
                "bind_x11_window"].includes(call[0])), false);
            assert.equal(await second.executor.cancelEnsure(payload), false);
            await assert.rejects(second.executor.completeEnsure(payload,
                {outcome: "rejected", code: "invalid_command"}), /conflicting durable/);
        }
    }
    {
        beginCase();
        activeCase = "late cancellation receipt loss survives restart";
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const execution = {...executePayload(), effect_id: "late-cancel-effect",
            command: {command: "invalid"}};
        await first.executor.execute(execution);
        const cancel = {...first.executor.operationIdentity(execution), request_id: "late-cancel",
            execution_request_id: execution.request_id};
        first.executor.nativeRequest = async () => undefined;
        await first.executor.cancel(cancel);
        assert.equal(first.executor.pendingOperationCancels.size, 1);
        assert.equal(localState.abushExecutionLedger[0].phase, "acked");
        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restore();
        assert.deepEqual(localState.abushExecutionLedger, []);
        assert.deepEqual(localState.abushPendingOperations, []);
        const reply = second.calls.find(call => call[0] === "cancellation_completion")[1];
        assert.equal(reply.request_id, "late-cancel");
        assert.equal(reply.completion, "not_applied");
    }
    for (const legacy of [false, true]) {
        beginCase();
        activeCase = `ensure legacy migration ${legacy}`;
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const payload = ensurePayload();
        const key = first.executor.ensureRequestKey(payload);
        const completion = {outcome: "created", window_id: logicalWindowId, page_id: logicalPageId};
        if (legacy) localState.abushEnsureCompletions = {[key]: completion};
        else await first.executor.stageEnsureCompletion(payload, completion);
        await first.executor.restoreEnsureCompletions();
        if (legacy) {
            await first.executor.flushEnsureCompletions();
            assert.equal(first.executor.ensureCompletions.get(key).phase, "pending_ack");
            assert.equal(first.calls.some(call => call[0] === "ensure_managed_page_completion"), false);
        }
        await first.executor.ensure(payload);
        assert.equal(Object.values(localState.abushEnsureCompletions)[0].phase, "acked");
        assert.equal(first.calls.some(call => ["createTab", "createWindow", "bind_x11_window"]
            .includes(call[0])), false);
    }
    {
        beginCase();
        activeCase = "receipt object field order is not identity";
        const {executor} = fixture();
        const payload = ensurePayload();
        executor.nativeRequest = async (_service, operation, value) => ({
            completion: {page_id: value.completion.page_id, window_id: value.completion.window_id,
                outcome: value.completion.outcome},
            executor_fence: {epoch: value.executor_fence.epoch,
                executor_id: value.executor_fence.executor_id},
            ensure_id: value.ensure_id, request_id: value.request_id,
            type: "ensure_managed_page_completion_accepted",
        });
        await executor.completeEnsure(payload,
            {outcome: "created", window_id: logicalWindowId, page_id: logicalPageId});
        assert.equal(executor.ensureCompletions.get(executor.ensureRequestKey(payload)).phase, "acked");
    }
    {
        beginCase();
        activeCase = "reconnect replays origin receipt with current transport";
        const localState = {};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        const payload = ensurePayload();
        const native = executor.nativeRequest;
        executor.nativeRequest = async () => { throw new Error("disconnected"); };
        await assert.rejects(executor.completeEnsure(payload,
            {outcome: "created", window_id: logicalWindowId, page_id: logicalPageId}));
        executor.nativeRequest = native;
        await executor.remoteState({...payload, connection_id: "replacement-transport",
            executor_fence: {...fence, epoch: 2}, connected: true});
        const delivery = calls.find(call => call[0] === "ensure_managed_page_completion")[1];
        assert.equal(delivery.connection_id, "replacement-transport");
        assert.deepEqual(delivery.executor_fence, fence);
        assert.equal(Object.values(localState.abushEnsureCompletions)[0].phase, "acked");
        assert.equal(calls.some(call => ["createTab", "createWindow", "bind_x11_window"]
            .includes(call[0])), false);
    }
        beginCase();
        activeCase = "display titles";
        assert.equal(displayTitle("local"), "local");
        assert.equal(displayTitle("dev.example.com"), "dev.example.com");
        assert.equal(displayTitle("prod-alias"), "prod-alias");
    for (const windowFirst of [false, true]) {
        beginCase();
        const {executor, calls, localState, trace} = await managedEventFixture({windowFirst});
        const record = await exactWait(executor.ensure(ensurePayload()), "managed event ensure");
        assert.deepEqual(record.statusTabIds, [1000, 1001]);
        assert.equal(record.pageTabId, 1002);
        assert(trace.length > 10 && trace.every(([, owned, invalid]) => owned && !invalid));
        assert.deepEqual(localState.abushOwnedMarkers[record.nonce].status_tab_ids, [1000, 1001]);
        assert.deepEqual(localState.abushPendingTransactions, []);
        assert.equal(calls.filter(call => ["remove", "removeWindow"].includes(call[0])).length, 0);
        assert.deepEqual((await executor.chrome.tabs.query({windowId: 1})).map(tab => tab.id),
            [2000, 2001, 2002]);
        assert(calls.filter(call => call[0] === "createTab")
            .every(call => call[1].url.includes("status.html#") || call[1].url === "about:blank"));
    }
    for (const lost of ["create-a", "create-b", "observed-b", "window", "move", "pin-0", "pin-1"]) {
        beginCase();
        const {executor, calls, localState, physicalTabs} = await managedEventFixture({lost});
        await assert.rejects(exactWait(executor.ensure(ensurePayload()), `lost ${lost}`));
        assert.deepEqual((await executor.chrome.tabs.query({windowId: 1})).map(tab => tab.id),
            [2000, 2001, 2002], `ordinary host survives ${lost}`);
        assert(!calls.some(call => call[0] === "removeWindow" && call[1] === 1));
        assert(!calls.some(call => call[0] === "remove" && call[1] >= 2000));
        assert.deepEqual(await executor.chrome.tabs.query({}),
            await executor.chrome.tabs.query({windowId: 1}), `no leaked candidate after ${lost}`);
        assert.deepEqual((await physicalTabs()).map(tab => tab.id), [2000, 2001, 2002],
            `no physical pendingUrl leak after ${lost}`);
        if (lost === "window") {
            assert.equal(localState.abushPendingTransactions.length, 1);
            assert.equal(localState.abushPendingTransactions[0].admission_settlement.outcome,
                "rejected", "settlement does not prove allocation rollback");
        } else {
            assert.deepEqual(localState.abushPendingTransactions, []);
        }
        assert.deepEqual(localState.abushOwnedMarkers, {});
        await exactWait(executor.lifecycleTail, `lifecycle queue after ${lost}`);
    }
    {
        beginCase();
        const {executor, calls} = fixture();
        const native = executor.nativeRequest;
        const accepted = deferred("created completion accepted");
        const reply = deferred("created completion reply");
        executor.nativeRequest = async (service, operation, payload) => {
            if (operation === "ensure_managed_page_completion") {
                calls.push([operation, structuredClone(payload)]);
                accepted.resolve();
                await reply.promise;
                throw new Error("accepted completion reply lost");
            }
            return native(service, operation, payload);
        };
        const payload = ensurePayload();
        const pending = executor.ensure(payload);
        const rejected = assert.rejects(pending, /accepted completion reply lost/);
        await accepted.promise;
        assert.equal(await exactWait(executor.cancelEnsure(payload), "completion cancel overlap"),
            false, "completion owns the terminal outcome before sending");
        reply.resolve();
        await rejected;
        assert.deepEqual(calls.filter(call => call[0] === "ensure_managed_page_completion")
            .map(call => call[1].completion.outcome), ["created"]);
        assert.equal(executor.workspaces.size, 1);
        const effects = calls.filter(call => ["createTab", "createWindow"].includes(call[0])).length;
        await assert.rejects(exactWait(executor.ensure(payload), "terminal replay"),
            /accepted completion reply lost/);
        assert.equal(calls.filter(call => ["createTab", "createWindow"].includes(call[0])).length,
            effects, "a lost terminal reply must not execute a second ensure");
        await assert.rejects(executor.completeEnsure(payload,
            {outcome: "rejected", code: "invalid_command"}), /conflicting durable/);
    }
    for (const boundary of ["ownership", "host", "preparing"]) {
        beginCase();
        const localState = {};
        const {executor} = fixture([], [], 0, false, null, localState);
        if (boundary === "ownership") {
            const own = executor.ownMarker.bind(executor);
            executor.ownMarker = async nonce => { await own(nonce); throw new Error(boundary); };
        } else if (boundary === "host") {
            executor.normalWindowId = async () => { throw new Error(boundary); };
        } else {
            const update = executor.updateEnsureTransaction.bind(executor);
            executor.updateEnsureTransaction = async (transaction, value) => {
                await update(transaction, value);
                if (value.phase === "preparing") throw new Error(boundary);
            };
        }
        await assert.rejects(exactWait(executor.ensure(ensurePayload()), boundary));
        assert.equal(executor.pendingTransactions.size, 0);
        assert.deepEqual(localState.abushOwnedMarkers, {});
    }
    {
        beginCase();
        let leaseNonce;
        const {executor, calls} = fixture();
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "register_candidate_lease") {
                leaseNonce = payload.nonce;
                return {accepted: true, nonce: "b".repeat(32)};
            }
            if (operation === "release_candidate_lease") {
                return {released: true, nonce: payload.nonce, admission_version: 2,
            phase: "aborted-resolved", denial_retained: false};
            }
            return typedResponse(operation, payload) || {};
        };
        await assert.rejects(executor.ensure(ensurePayload()),
            /candidate lease was not fully verified/);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "an unverified lease receipt must block candidate creation");
        assert.equal(calls.filter(call => call[0] === "release_candidate_lease").length, 1,
            "an uncertain register response must trigger idempotent lease release");
        assert.equal(calls.find(call => call[0] === "release_candidate_lease")[1].nonce,
            leaseNonce);
    }
    {
        beginCase();
        let listener;
        const failures = [];
        const order = [];
        let authority = null;
        const oldExecute = {
            ...executePayload(), effect_id: "old-buffered-effect", command: {command: "x"},
        };
        const replacement = {
            ...ensurePayload({connection_id: "c_replaced", executor_fence: {
                executor_id: "executor-replaced", epoch: 2,
            }}),
            connected: true,
        };
        const workspaces = {
            tail: Promise.resolve(),
            enqueueRemoteLifecycle(_remoteKey, task) {
                const next = this.tail.catch(() => {}).then(task);
                this.tail = next;
                return next;
            },
            async executeQueued(payload) {
                order.push(`execute:${payload.connection_id}`);
                if (!authority || payload.connection_id !== authority.connection_id ||
                        JSON.stringify(payload.executor_fence) !==
                            JSON.stringify(authority.executor_fence)) {
                    throw new Error("stale remote authority");
                }
                order.push("mutated");
                return {deferred: Promise.resolve()};
            },
            async remoteStateSerialized(payload) {
                authority = payload;
                order.push(`state:${payload.connection_id}`);
            },
        };
        registerNativeEventRouter({onEvent: callback => { listener = callback; }}, workspaces,
            async () => {}, message => failures.push(message));
        const hold = deferred("old buffered execute barrier");
        workspaces.tail = hold.promise;
        listener({service: "abush_browser", event: "abush_browser.execute", payload: oldExecute});
        listener({service: "abush_browser", event: "abush_browser.remote_state",
            payload: replacement});
        hold.resolve();
        await waitFor(() => order.includes(`state:${replacement.connection_id}`),
            "replacement remote state commit");
        await exactWait(workspaces.tail.catch(() => {}), "remote authority queue");
        assert.deepEqual(order, [
            `execute:${oldExecute.connection_id}`,
            `state:${replacement.connection_id}`,
        ]);
        assert.equal(order.includes("mutated"), false,
            "an old buffered frame must have zero mutation after replacement authority");
        assert(failures.some(message => /stale remote authority/.test(message)),
            "stale buffered execution must surface a retryable failure");
    }
    {
        beginCase();
        let listener;
        let authority = null;
        let mutated = 0;
        const failures = [];
        const started = deferred("old execute started");
        const release = deferred("old execute release");
        const old = {...executePayload(), effect_id: "old-running-effect", command: {command: "x"}};
        const replacement = {
            ...ensurePayload({connection_id: "c_running_replacement", executor_fence: {
                executor_id: "executor-running-replacement", epoch: 2,
            }}),
            connected: true,
        };
        const workspaces = {
            tail: Promise.resolve(),
            enqueueRemoteLifecycle(_remoteKey, task) {
                const next = this.tail.catch(() => {}).then(task);
                this.tail = next;
                return next;
            },
            async executeQueued(payload) {
                if (!authority || payload.connection_id !== authority.connection_id) {
                    throw new Error("stale remote authority");
                }
                const deferredExecution = (async () => {
                    started.resolve();
                    await release.promise;
                    if (payload.connection_id !== authority.connection_id) {
                        throw new Error("stale remote authority");
                    }
                    mutated += 1;
                })();
                return {deferred: deferredExecution};
            },
            async remoteStateSerialized(payload) {
                authority = payload;
            },
        };
        registerNativeEventRouter({onEvent: callback => { listener = callback; }}, workspaces,
            async () => {}, message => failures.push(message));
        authority = {...old, connected: true};
        listener({service: "abush_browser", event: "abush_browser.execute", payload: old});
        await started.promise;
        listener({service: "abush_browser", event: "abush_browser.remote_state",
            payload: replacement});
        await waitFor(() => authority.connection_id === replacement.connection_id,
            "replacement authority while old execute runs");
        release.resolve();
        await waitFor(() => failures.some(message => /stale remote authority/.test(message)),
            "old running execute stale completion");
        assert.equal(mutated, 0,
            "an old running execute must recheck authority before its browser mutation");
    }
    {
        beginCase();
        const {executor, calls} = fixture();
        const old = ensurePayload();
        await executor.remoteState({...old, connected: true});
        const record = await executor.ensure(old);
        calls.length = 0;
        const attachStarted = deferred("old execute debugger attach");
        const releaseAttach = deferred("old execute debugger attach release");
        executor.chrome.debugger.attach = async target => {
            calls.push(["attach", target]);
            attachStarted.resolve();
            await releaseAttach.promise;
        };
        const payload = {...executePayload(), effect_id: "old-execute-after-replacement",
            command: {command: "navigate", page: executor.pageRef(record),
                url: "https://stale-authority.test/"}};
        const executing = executor.execute(payload);
        await attachStarted.promise;
        const replacement = ensurePayload({
            connection_id: "c_execute_replacement",
            executor_fence: {executor_id: "executor-execute-replacement", epoch: 2},
        });
        await executor.remoteState({...replacement, connected: true});
        releaseAttach.resolve();
        await executing;
        assert.equal(calls.some(call => call[0] === "command"), false,
            "old execute must not mutate the browser after replacement remoteState commits");
        assert.equal(calls.find(call => call[0] === "executor_completion")[1].completion,
            "not_applied");
    }
    {
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_old00000",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "a".repeat(32), connectionDisplayName: "local",
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        const old = ensurePayload({connection_id: record.connectionId});
        const current = ensurePayload({
            connection_id: "c_current0",
            executor_fence: {executor_id: "executor-current", epoch: 2},
        });
        await executor.remoteState({...old, connected: true});
        const flight = {
            waiters: new Set(), cancelled: false, authority: executor.ensureIdentity(old),
        };
        await executor.remoteState({...current, connected: true});
        calls.length = 0;
        await assert.rejects(executor.ensureWorkspace(old, flight), /stale remote authority/);
        assert.equal(calls.some(call => call[0] === "persist" ||
            call[0] === "bind_x11_window"), false,
        "an old ensure must not persist or rebind after replacement authority commits");
        const adopted = await executor.ensure({...current, request_id: "current-ensure",
            ensure_id: "current-ensure", expected_page: executor.pageRef(record)});
        assert.equal(adopted.connectionId, current.connection_id,
            "the current authority must retain existing-workspace failover");
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-one",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
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
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        const originalActive = await executor.chrome.tabs.get(2000);
        assert.equal(originalActive.active, true);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") throw new Error("prepare failed");
            return typedResponse(operation, payload) || {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-prepare-rollback",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
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
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            if (operation === "activate") throw new Error("focus verification failed");
            return typedResponse(operation, payload) || {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-two",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
            url: "https://ordinary-open.test/after-prepare",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "activation failure after navigation must be unknown");
        assert.equal(calls.filter(call => call[0] === "update" &&
            call[2]?.url === "https://ordinary-open.test/after-prepare").length, 1);
    }
    {
        beginCase();
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
            return typedResponse(operation, payload) || {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-update-race",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
            url: "https://ordinary-open.test/update-race",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "a rejected navigation call may already have applied and must be unknown");
        assert.equal(calls.filter(call => call[0] === "remove").length, 0,
            "a navigation race must not delete the target tab");
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            if (operation === "ordinary_file_navigate") {
                return {...payload, completion: "applied"};
            }
            return typedResponse(operation, payload) || {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "applied");
        const navigate = calls.find(call => call[0] === "ordinary_file_navigate");
        assert.equal(navigate[1].url, "file:///home/amos/private.txt");
        assert.equal(navigate[1].xid, 42);
        assert(Number.isSafeInteger(navigate[1].deadline_ms));
        assert.equal(typeof navigate[1].nonce, "string");
        assert.equal(calls.filter(call => call[0] === "update" && call[2]?.url).length, 0,
            "file navigation must never use extension tabs.update");
    }
    {
        beginCase();
        const localState = {};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        await executor.restore();
        calls.length = 0;
        const payload = ensurePayload({
            request_id: "marker-created-pending-url",
            ensure_id: "marker-created-pending-url",
        });
        const nonce = "d".repeat(32);
        const transaction = await executor.beginEnsureTransaction(payload, nonce);
        transaction.hostWindowId = 1;
        const markerUrl = executor.statusUrl({nonce}, "a");
        transaction.plannedEffect = {
            kind: "tabs.create.marker", window_id: 1, marker: "a", url: markerUrl,
        };
        const owned = executor.observeChromeEvent("tabCreated", {tab: {
            id: 4000, windowId: 1, pendingUrl: markerUrl,
        }});
        assert.equal(owned, true,
            "Chrome tabCreated pendingUrl must correlate the planned marker effect");
        assert.equal(transaction.invalid, false,
            "a planned marker with pendingUrl must not invalidate its transaction");
        assert.equal(transaction.plannedEffect.tab_id, 4000,
            "the transaction must retain the exact created marker tab ID");
    }
    {
        beginCase();
        const localState = {};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        await executor.restore();
        calls.length = 0;
        let creates = 0;
        let hiddenTabId = null;
        let hideFromReconcile = false;
        const originalCreateTab = executor.chrome.tabs.create;
        const originalQueryTabs = executor.chrome.tabs.query;
        executor.chrome.tabs.query = async query => {
            const tabs = await originalQueryTabs(query);
            if (hideFromReconcile && query.windowId === undefined) {
                hideFromReconcile = false;
                return tabs.filter(tab => tab.id !== hiddenTabId);
            }
            return tabs;
        };
        executor.chrome.tabs.create = async properties => {
            creates += 1;
            const tab = await originalCreateTab(properties);
            executor.observeChromeEvent("tabCreated", {tab: {
                id: tab.id, windowId: properties.windowId, pendingUrl: properties.url,
            }});
            if (creates === 2) {
                hiddenTabId = tab.id;
                hideFromReconcile = true;
                throw new Error("second marker result lost");
            }
            return tab;
        };
        const payload = ensurePayload({
            request_id: "marker-result-loss-cleanup",
            ensure_id: "marker-result-loss-cleanup",
        });
        await assert.rejects(executor.ensure(payload), /second marker result lost/);
        const markerRemovals = calls.filter(call => call[0] === "remove").map(call => call[1]);
        assert.equal(markerRemovals.length, 2,
            "cleanup must remove both the returned and event-correlated marker tabs");
        assert(markerRemovals.includes(hiddenTabId),
            "cleanup must use the event-correlated tab ID before tabs.query sees it");
        assert.deepEqual(localState.abushPendingTransactions, [],
            "marker result loss must consume the rolled-back durable transaction");
        assert.deepEqual(localState.abushOwnedMarkers, {},
            "marker result loss must disown its pending nonce");
    }
    {
        beginCase();
        const {executor} = fixture([], [], 1);
        executor.nativeRequest = (service, operation, payload) => {
            if (operation === "prepare") return Promise.resolve({xid: 42, nonce: payload.nonce});
            if (operation === "ordinary_file_navigate") {
                const error = new Error("native request was not sent");
                error.nativeDispatch = "not_sent";
                return Promise.reject(error);
            }
            return Promise.resolve(typedResponse(operation, payload) || {});
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file-before-send",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "not_applied",
            "synchronous failure before native dispatch must remain not_applied");
    }
    {
        beginCase();
        const {executor} = fixture([], [], 1);
        executor.nativeRequest = (service, operation, payload) => {
            if (operation === "prepare") return Promise.resolve({xid: 42, nonce: payload.nonce});
            if (operation === "ordinary_file_navigate") {
                const error = new Error("native request dispatch became uncertain");
                error.nativeDispatch = "unknown";
                return Promise.reject(error);
            }
            return Promise.resolve(typedResponse(operation, payload) || {});
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file-dispatch-unknown",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "native dispatch failure after postMessage starts must remain unknown");
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            if (operation === "ordinary_file_navigate") {
                return {...payload, nonce: "f".repeat(32), completion: "applied"};
            }
            return typedResponse(operation, payload) || {};
        };
        const completion = await executor.ordinaryOpen({
            version: 1,
            request_id: "ordinary-file-wrong-nonce",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
            url: "file:///home/amos/private.txt",
            focus: true,
        });
        assert.equal(completion, "unknown",
            "a sent mutation with a mismatched native correlation must be unknown");
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        const payload = {
            version: 1,
            request_id: "ordinary-duplicate",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
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
            return typedResponse(operation, value) || {};
        };
        const first = executor.ordinaryOpen(payload);
        await waitFor(() => calls.some(call => call[0] === "prepare"),
            "ordinary duplicate prepare");
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
        beginCase();
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
            return typedResponse(operation, payload) || {};
        };
        const first = executor.ordinaryOpen({
            version: 1, request_id: "ordinary-serialized-one",
            operation: "ordinary_open", deadline_ms: Date.now() + 30000,
            url: "https://one.test/", focus: true,
        });
        const second = executor.ordinaryOpen({
            version: 1, request_id: "ordinary-serialized-two",
            operation: "ordinary_open", deadline_ms: Date.now() + 30000,
            url: "https://two.test/", focus: true,
        });
        await waitFor(() => prepares >= 1, "first serialized ordinary prepare");
        assert.equal(prepares, 1, "ordinary opens must not share staging state concurrently");
        gates[0].resolve();
        await first;
        await waitFor(() => prepares >= 2, "second serialized ordinary prepare");
        gates[1].resolve();
        assert.equal(await second, "applied");
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        const expired = {
            version: 1,
            request_id: "ordinary-expired-queued",
            operation: "ordinary_open",
            deadline_ms: Date.now() - 1,
            url: "https://expired.test/",
            focus: true,
        };
        assert.equal(await executor.ordinaryOpen(expired), "not_applied");
        assert.equal(calls.some(call => call[0] === "createTab" || call[0] === "createWindow"),
            false, "expired queued ordinary open must cause zero late navigation effects");
        assert.equal(calls.find(call => call[0] === "ordinary_open_completion")[1].completion,
            "not_applied");
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        const navigate = deferred("ordinary navigation completion");
        const originalUpdate = executor.chrome.tabs.update;
        executor.chrome.tabs.update = async (id, properties) => {
            const result = await originalUpdate(id, properties);
            if (properties.url === "https://cancel-after-navigation.test/") {
                await navigate.promise;
            }
            return result;
        };
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            return typedResponse(operation, payload) || {};
        };
        const payload = {
            version: 1,
            request_id: "ordinary-cancel-after-navigation",
            operation: "ordinary_open",
            deadline_ms: Date.now() + 30000,
            url: "https://cancel-after-navigation.test/",
            focus: true,
        };
        const opening = executor.ordinaryOpen(payload);
        await waitFor(() => calls.some(call => call[0] === "update" &&
            call[2]?.url === payload.url), "ordinary navigation dispatch");
        await executor.cancelOrdinaryOpen({version: 1, request_id: payload.request_id,
            operation: "ordinary_open_cancel"});
        assert.equal(calls.find(call => call[0] === "prepare")[1].deadline_ms,
            payload.deadline_ms);
        navigate.resolve();
        assert.equal(await opening, "unknown",
            "cancel after navigation dispatch must never report applied or not_applied");
    }
    {
        beginCase();
        const {executor} = fixture([], [], 2);
        const selected = await executor.selectOrdinaryWindow();
        assert.equal(selected.windowId, 1);
        assert.equal(selected.originalActiveTabId, 2000,
            "prepare rollback must capture the active tab before staging");
    }
    {
        beginCase();
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
        beginCase();
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
            deadline_ms: Date.now() + 30000,
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
        beginCase();
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
        beginCase();
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
        beginCase();
        const nonce = "8".repeat(32);
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
        first.executor.chrome.windows.remove = async () => {
            throw new Error("cleanup interrupted");
        };
        await assert.rejects(first.executor.convergeManagedTopology(record,
            {closeWindow: true, report: false}), /cleanup was incomplete/);
        const reused = fixture([], [], 1, false, null, localState);
        reused.executor.chrome.windows.get = async id => ({id, type: "normal", focused: true});
        reused.executor.chrome.tabs.query = async query => query.windowId === 10 ? [{
            id: 110, windowId: 10, index: 0, pinned: false,
            url: "https://ordinary-reused.test/", title: "ordinary",
        }] : [];
        await reused.executor.restoreCleanupTombstones();
        assert.equal(reused.calls.some(call => call[0] === "removeWindow" && call[1] === 10), false,
            "reused numeric window ID must never remove an ordinary window");
        assert.equal(reused.calls.some(call => call[0] === "remove" && call[1] === 110), false,
            "reused numeric tab ID must never remove a non-marker tab");
    }
    {
        beginCase();
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
        beginCase();
        const orphanNonce = "f".repeat(32);
        const {executor, calls} = fixture([], [orphanNonce]);
        await executor.restore();
        assert.deepEqual(calls.filter(call => call[0] === "remove"),
            [["remove", 1500]], "a marker tab in an ordinary window must not close that window");
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0);
    }
    {
        beginCase();
        const orphanNonce = "f".repeat(32);
        const {executor, calls} = fixture([], [orphanNonce], 1, true);
        await executor.restore();
        assert.deepEqual(calls.filter(call => call[0] === "remove"),
            [["remove", 1500]], "orphan cleanup must remove only the marker tab");
        assert.equal(calls.filter(call => call[0] === "removeWindow").length, 0,
            "orphan cleanup must never destroy a window containing user tabs");
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 1);
        executor.chrome.windows.create = async () => {
            throw new Error("window creation failed");
        };
        await assert.rejects(executor.ensure(ensurePayload()), error =>
            error.browserCode === "unknown_effect");
        assert.equal(calls.filter(call => call[0] === "register_candidate_lease").length, 1);
        assert.equal(calls.filter(call => call[0] === "release_candidate_lease").length, 0,
            "rejection cannot prove that allocation was rolled back");
        assert.equal(executor.pendingTransactions.size, 1);
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "a failed window create must not leave a partial marker tab");
    }
    {
        beginCase();
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
        assert.equal(calls.some(call => call[0] === "verify_x11_window" ||
            call[0] === "clear_x11_window" || call[0] === "removeWindow"), false,
        "cross-boot ownership must not authorize physical ID or XID mutation");
        assert.deepEqual(calls.filter(call => call[0] === "remove"), [],
            "stable managed candidates remain quarantined, not ordinary");
        assert.equal((await executor.chrome.tabs.get(1702)).url,
            restoredPopup.businessUrl);
    }
    {
        beginCase();
        const nonce = "4".repeat(32);
        const restoredPopup = {
            windowId: 705,
            tabIds: [1750, 1751, 1752],
            nonce,
            businessUrl: "https://x11-mismatch.test/",
        };
        const localState = {abushOwnedMarkers: {[nonce]: {
            version: 2,
            nonce,
            window_id: 705,
            status_tab_ids: [1750, 1751],
            page_tab_id: 1752,
            xid: 7050,
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
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "verify_x11_window") {
                return {verified: false, xid: payload.xid};
            }
            if (operation === "clear_x11_window") {
                throw new Error("unverified X11 identity must not be cleared");
            }
            return typedResponse(operation, payload) || {};
        };
        await executor.restore();
        assert.equal(calls.some(call => call[0] === "removeWindow" && call[1] === 705), false,
            "X11 mismatch must prevent whole-window removal despite matching Chrome IDs");
        assert.deepEqual(calls.filter(call => call[0] === "remove"), [],
            "cross-boot stable managed candidate must remain quarantined");
        assert.equal(calls.some(call => call[0] === "clear_x11_window"), false,
            "X11 mismatch must not clear metadata on a reused XID");
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
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
        beginCase();
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
        assert.deepEqual(calls.filter(call => call[0] === "remove"), []);
        assert.equal(calls.some(call => call[0] === "clear_x11_window"), false,
            "cross-boot orphan ownership cannot authorize a reused XID");
        assert(localState.abushOwnedMarkers[nonce],
            "stable candidate quarantine preserves durable ownership for diagnosis");
    }
    {
        beginCase();
        const localState = {abushRestoreHost: {
            version: 3, phase: "prepared", outcome: "ambiguous",
            source_boot: "previous-browser-boot", target_boot: "next",
            sessionId: null, snapshot: null, registry_revision: 1,
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.windows.getAll = async () => [];
        await executor.restore();
        assert.equal(localState.abushRestoreHost.outcome, "ambiguous",
            "ambiguous handoff must remain durable and fail closed");
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "startup without an exact handoff must never create a neutral window");
        assert.equal(calls.filter(call => call[0] === "restoreSession").length, 0);
    }
    {
        beginCase();
        const snapshot = {tabCount: 1, urls: ["https://work.test/"]};
        const localState = {abushRestoreHost: {
            version: 3, phase: "prepared", outcome: "exact",
            source_boot: "previous-browser-boot", target_boot: "next",
            sessionId: "window-session", snapshot, registry_revision: 1,
        }};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        executor.chrome.windows.getAll = async () => [];
        executor.chrome.tabs.query = async () => [];
        executor.chrome.sessions = {
            getRecentlyClosed: async query => {
                calls.push(["getRecentlyClosed", query]);
                return [{window: {sessionId: "window-session",
                    tabs: [{url: "https://work.test/"}]}}];
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
        ], "next boot must restore only its persisted exact ordinary session ID");
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0);
    }
    {
        beginCase();
        const snapshot = {tabCount: 1, urls: ["https://ordinary-0.test/"]};
        const localState = {abushRestoreHost: {
            version: 3, phase: "prepared", outcome: "exact",
            source_boot: "previous-browser-boot", target_boot: "next",
            sessionId: "auto-restored", snapshot, registry_revision: 1,
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.browserBootId = "new-browser-boot";
        executor.chrome.sessions = {
            restore: async sessionId => {
                calls.push(["restoreSession", sessionId]);
                throw new Error("must not duplicate Chrome auto restore");
            },
        };
        await executor.restore();
        assert.equal(calls.some(call => call[0] === "restoreSession"), false,
            "Chrome auto-restored exact ordinary window must consume without duplication");
        assert.equal(localState.abushRestoreHost, false);
    }
    {
        beginCase();
        const snapshot = {tabCount: 1, urls: ["https://ordinary-0.test/"]};
        const localState = {abushRestoreHost: {
            version: 3, phase: "inflight", outcome: "exact",
            source_boot: "previous-browser-boot", target_boot: "other-browser-boot",
            sessionId: "accepted-session", snapshot, registry_revision: 1,
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.browserBootId = "new-browser-boot";
        executor.chrome.sessions = {
            restore: async sessionId => {
                calls.push(["restoreSession", sessionId]);
                throw new Error("must not duplicate accepted restore after crash");
            },
        };
        await executor.restore();
        assert.equal(calls.some(call => call[0] === "restoreSession"), false);
        assert.equal(localState.abushRestoreHost, false,
            "accepted restore visible after restart must verify and consume exactly once");
    }
    {
        beginCase();
        const snapshot = {tabCount: 1, urls: ["https://ordinary-0.test/"]};
        const localState = {abushRestoreHost: {
            version: 3, phase: "inflight", outcome: "exact",
            source_boot: "previous-browser-boot", target_boot: "test-browser-boot",
            sessionId: "ordinary-window-session", snapshot, registry_revision: 1,
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
            "accepted restore must verify live target before replaying the effect");
        assert.equal(localState.abushRestoreHost, false,
            "same target boot worker restart reconciles accepted restore without replay");
    }
    {
        beginCase();
        const localState = {abushRestoreHost: {
            version: 3, phase: "inflight", outcome: "exact",
            source_boot: "previous-browser-boot", target_boot: "test-browser-boot",
            sessionId: "consumed-session",
            snapshot: {tabCount: 1, urls: ["https://accepted-not-visible.test/"]},
            registry_revision: 1,
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [],
            restore: async sessionId => {
                calls.push(["restoreSession", sessionId]);
                throw new Error("must not replay a consumed inflight session ID");
            },
        };
        await executor.restoreHost();
        assert.equal(calls.some(call => call[0] === "restoreSession"), false);
        assert.equal(calls.some(call => call[0] === "createWindow"), false);
        assert.equal(localState.abushRestoreHost.phase, "unknown",
            "same target boot must reconcile accepted but invisible effect without replay");
    }
    {
        beginCase();
        const snapshot = {tabCount: 2, urls: ["https://work.test/", "https://docs.test/"]};
        const localState = {abushRestoreHost: {
            version: 3, phase: "prepared", outcome: "exact",
            source_boot: "previous-browser-boot", target_boot: "next",
            sessionId: "expired-session", snapshot, registry_revision: 1,
        }};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        executor.chrome.windows.getAll = async () => [];
        executor.chrome.tabs.query = async () => [];
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [
                {window: {sessionId: "newer-wrong", tabs: [{url: "https://wrong.test/"}]}},
                {window: {sessionId: "matched-session",
                    tabs: snapshot.urls.map(url => ({url}))}},
            ],
            restore: async sessionId => { calls.push(["restoreSession", sessionId]); },
        };
        await executor.restore();
        assert.equal(calls.filter(call => call[0] === "restoreSession").length, 0,
            "invalid session ID must not guess another session by URLs");
        assert.equal(localState.abushRestoreHost.sessionId, "expired-session");
        assert.equal(localState.abushRestoreHost.phase, "prepared");
    }
    for (const failure of ["unavailable", "rejected", "ambiguous"]) {
        beginCase();
        const snapshot = {tabCount: 1, urls: ["https://handoff.test/"]};
        const localState = {};
        const source = fixture([], [], 1, false, null, localState);
        source.executor.browserBootId = "boot-A";
        await source.executor.markRestoreHost({sessionId: "exact-window", snapshot});
        await source.executor.restore();
        assert.equal(localState.abushRestoreHost.target_boot, "next",
            "source boot worker restart cannot claim its own handoff");
        const target = fixture([], [], 1, false, null, localState);
        target.executor.browserBootId = "boot-B";
        const entry = {window: {sessionId: "exact-window",
            tabs: snapshot.urls.map(url => ({url}))}};
        target.executor.chrome.sessions = {
            getRecentlyClosed: async () => failure === "unavailable" ? [] : [entry],
            restore: async id => {
                target.calls.push(["restoreSession", id]);
                throw new Error("restore rejected before acceptance");
            },
        };
        if (failure === "ambiguous") {
            const first = await target.executor.chrome.tabs.query({windowId: 1});
            first[0].url = snapshot.urls[0];
            await target.executor.chrome.windows.create({url: snapshot.urls[0], type: "normal"});
            await assert.rejects(exactWait(target.executor.restore(), "ambiguous restore"),
                /ambiguous live ordinary/);
        } else {
            await exactWait(target.executor.restore(), "target boot restore");
        }
        assert.equal(localState.abushRestoreHost.target_boot, "boot-B");
        const transitions = target.calls.filter(call => call[0] === "persistLocal" &&
            call[1].abushRestoreHost).map(call => call[1].abushRestoreHost);
        assert.equal(transitions[0].target_boot, "boot-B",
            "claim must precede availability, ambiguity and restore effects");
        assert(transitions.every(intent => intent.target_boot === "boot-B"));
        const later = fixture([], [], 1, false, null, localState);
        later.executor.browserBootId = "boot-C";
        later.executor.chrome.sessions = {
            getRecentlyClosed: async () => [entry],
            restore: async id => { later.calls.push(["restoreSession", id]); return entry; },
        };
        await exactWait(later.executor.restore(), "later boot restore");
        assert.equal(later.calls.some(call => call[0] === "restoreSession"), false,
            "a missed next-boot handoff must never execute on boot C");
        assert.equal(localState.abushRestoreHost.target_boot, "boot-B");
        if (failure !== "ambiguous") {
            const restart = fixture([], [], 1, false, null, localState);
            restart.executor.browserBootId = "boot-B";
            restart.executor.chrome.sessions = {
                getRecentlyClosed: async () => [entry],
                restore: async id => {
                    restart.calls.push(["restoreSession", id]);
                    return entry;
                },
            };
            await exactWait(restart.executor.restore(), "same target boot retry");
            assert.equal(restart.calls.filter(call => call[0] === "restoreSession").length, 1);
            assert.equal(localState.abushRestoreHost, false);
        }
    }
    for (const entry of [
        {sessionId: "exact-window", window: {tabs: [{url: "https://handoff.test/"}]}},
        {tab: {sessionId: "exact-window"}, window: {tabs: [{url: "https://handoff.test/"}]}},
        {sessionId: "exact-window", window: {sessionId: "", tabs: [
            {url: "https://handoff.test/"}]}},
    ]) {
        beginCase();
        const localState = {abushRestoreHost: {
            version: 3, phase: "prepared", outcome: "exact", source_boot: "boot-A",
            target_boot: "next", sessionId: "exact-window",
            snapshot: {tabCount: 1, urls: ["https://handoff.test/"]},
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [entry],
            restore: async id => calls.push(["restoreSession", id]),
        };
        await exactWait(executor.restore(), "invalid nested window identity");
        assert.equal(calls.some(call => call[0] === "restoreSession"), false);
        assert.equal(localState.abushRestoreHost.phase, "prepared");
    }
    {
        beginCase();
        const nonce = "a".repeat(32);
        const localState = {abushRestoreHost: {
            version: 3, phase: "prepared", outcome: "exact", source_boot: "boot-A",
            target_boot: "next", sessionId: "auto-restored",
            snapshot: {tabCount: 1, urls: ["https://ordinary-0.test/"]},
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        const tabs = await executor.chrome.tabs.query({windowId: 1});
        tabs.push({id: 3000, windowId: 1, url: executor.statusUrl({nonce}, "a"),
            title: `${STATUS_PREFIX}${nonce}:a`, pinned: true});
        executor.chrome.sessions = {restore: async id => calls.push(["restoreSession", id])};
        await exactWait(executor.restore(), "orphan before auto-restore reconciliation");
        assert.deepEqual(calls.filter(call => call[0] === "remove"), [["remove", 3000]]);
        assert.equal(calls.some(call => call[0] === "restoreSession" ||
            call[0] === "removeWindow" || call[0] === "update"), false);
        const removed = calls.findIndex(call => call[0] === "remove");
        const consumed = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushRestoreHost?.phase === "consumed");
        assert(consumed > removed, "orphan sanitization must precede handoff consumption");
        assert.equal(localState.abushRestoreHost, false);
        assert.deepEqual(tabs.map(tab => tab.url), ["https://ordinary-0.test/"]);
    }
    {
        beginCase();
        const nonce = "b".repeat(32);
        const old = {nonce, windowId: 1, statusTabIds: [2001, 2002], pageTabId: 2000,
            x11WindowId: 42, logicalWindowId, logicalPageId, windowGeneration: 1,
            pageGeneration: 1, remoteKey: "remote", workspaceId: "workspace",
            connectionDisplayName: "local"};
        const localState = {};
        const {executor, calls} = fixture([], [], 3, false, null, localState);
        localState.abushOwnedMarkers = {[nonce]: executor.markerOwnership(old)};
        const requestId = "c".repeat(32);
        const tabs = await executor.chrome.tabs.query({windowId: 1});
        await executor.chrome.windows.create({url: executor.ordinaryStagingUrl(requestId),
            type: "normal", focused: false});
        const before = structuredClone(tabs);
        executor.bootstrapLeases.set(requestId, {version: 1, request_id: requestId,
            operation: "ordinary_open", url: "https://requested.test/", focus: true,
            deadline_ms: Date.now() + 5000});
        await exactWait(executor.restore(), "cold lease with reused IDs");
        assert.equal(calls.some(call => call[0] === "removeWindow" && call[1] === 1 ||
            ["clear_x11_window", "verify_x11_window"].includes(call[0])), false);
        for (const id of [2000, 2002]) {
            assert.equal(calls.some(call => call[0] === "remove" && call[1] === id), false,
                "stale owned page/status IDs cannot authorize deletion of ordinary tabs");
            assert.equal((await executor.chrome.tabs.get(id)).url,
                before.find(tab => tab.id === id).url);
        }
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
        const ready = deferred("runtime ownership readiness");
        let listener;
        let owned = false;
        const chromeApi = {runtime: {onMessage: {
            addListener: value => { listener = value; },
        }}};
        registerRuntimeMessageRouter(chromeApi, {
            ownsWindow: () => owned,
        }, () => ready.promise, (_url, _source, callback) => callback({ok: true}));
        const responses = [];
        assert.equal(listener({query: "abush_owned_window"}, {tab: {windowId: 42}},
            value => responses.push(value)), true);
        assert.equal(listener({url: "https://outside.test/"}, {tab: {windowId: 42}},
            value => responses.push(value)), true);
        await Promise.resolve();
        assert.deepEqual(responses, [], "runtime messages must wait for restored ownership");
        owned = true;
        ready.resolve();
        await waitFor(() => responses.length === 2, "deferred runtime responses");
        assert.deepEqual(responses, [
            {owned: true, generation: 0, ready: true, ok: true, route: false},
            {owned: true, generation: 0, ready: true, ok: true, route: false},
        ]);
    }
    {
        beginCase();
        let listener;
        const chromeApi = {runtime: {onMessage: {
            addListener: value => { listener = value; },
        }}};
        registerRuntimeMessageRouter(chromeApi, {ownsWindow: () => false},
            () => Promise.reject(new Error("restore failed")));
        const response = await new Promise(resolve => {
            assert.equal(listener({url: "https://outside.test/"}, {tab: {windowId: 42}},
                resolve), true);
        });
        assert.deepEqual(response, {owned: null, ready: false, ok: false, route: false},
            "failed readiness must conservatively suppress routing");
    }
    {
        beginCase();
        let listener;
        let readyAttempts = 0;
        let routed = 0;
        const chromeApi = {runtime: {onMessage: {
            addListener: value => { listener = value; },
        }}};
        registerRuntimeMessageRouter(chromeApi, {ownsWindow: () => false}, async () => {
            readyAttempts += 1;
            if (readyAttempts === 1) throw new Error("restore failed");
        }, (_url, _source, callback) => {
            routed += 1;
            callback({ok: true});
        });
        const send = message => new Promise(resolve => {
            assert.equal(listener(message, {tab: {windowId: 42, url: "https://source.test/"}},
                resolve), true);
        });
        assert.deepEqual(await send({query: "abush_owned_window"}),
            {owned: null, ready: false, ok: false, route: false});
        assert.deepEqual(await send({url: "https://outside.test/"}),
            {ok: true, owned: false, generation: 0, ready: true, route: true});
        assert.equal(routed, 1, "ordinary routing must recover without a content reload");
    }
    {
        beginCase();
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
        beginCase();
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
            observeWindowCreated: async window => {
                moved.push(["observed-window", window.id]);
                return true;
            },
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
        assert.deepEqual(moved, [["observed-window", 42]]);
        assert.equal(typeof listeners.tabReplaced, "function",
            "tabs.onReplaced must be registered");
        ready.resolve();
        await ready.promise;
        await Promise.resolve();
        await Promise.resolve();
        assert.deepEqual(removed, [42]);
        const normalizedMoved = moved.map(value => JSON.stringify(value)).sort();
        assert.deepEqual(normalizedMoved, [
            ["created", 24], ["detached", 24, {oldWindowId: 42}],
            ["observed-window", 42],
            ["focus", 42], ["moved", 24, {windowId: 42, fromIndex: 0, toIndex: 1}],
            ["removed", 24, {windowId: 42}],
            ["attached", 24, {windowId: 7, newWindowId: 7}],
            ["updated", 24, {title: "changed"}],
        ].map(value => JSON.stringify(value)).sort());
        assert.deepEqual([...snapshots].sort((left, right) => left - right),
            [7, 42, 42, 42, 42, 42]);
        assert.equal(lifecycle.filter(value => value === "start").length, 8,
            "all lifecycle-sensitive Chrome events must enter the serialized tail");
        listeners.tabReplaced(90, 80);
        await Promise.resolve();
        await Promise.resolve();
        assert(moved.some(call => call[0] === "replaced" && call[1] === 90 && call[2] === 80));
        assert.equal(failures.length, 0);
    }
    {
        beginCase();
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
            return typedResponse(operation, payload) || {};
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
            return typedResponse(operation, payload) || {};
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
        beginCase();
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
        beginCase();
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
        const registerLeaseIndex = calls.findIndex(call =>
            call[0] === "register_candidate_lease");
        const createWindowIndex = calls.findIndex(call => call[0] === "createWindow");
        const releaseLeaseIndex = calls.findIndex(call =>
            call[0] === "release_candidate_lease");
        const bindIndex = calls.findIndex(call => call[0] === "bind_x11_window");
        const ownershipIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushOwnedMarkers?.[first.nonce]?.version === 2);
        const commitIndex = calls.findIndex(call => call[0] === "persist" &&
            call[1].abushWorkspaces?.some(record => record.nonce === first.nonce));
        const armIndex = calls.findIndex(call => call[0] === "arm_candidate_lease");
        const admitIndex = calls.findIndex(call => call[0] === "admit_candidate");
        assert(registerLeaseIndex < armIndex && armIndex < createWindowIndex &&
            createWindowIndex < admitIndex && admitIndex < bindIndex,
        "reserve and arm must precede physical create, exact admission must precede bind");
        const firstBusiness = calls.findIndex(call => call[0] === "createTab" &&
            call[1].url === "about:blank");
        assert(admitIndex < firstBusiness, "business tabs must wait for pre-map admission");
        assert(registerLeaseIndex >= 0 && registerLeaseIndex < createWindowIndex &&
            createWindowIndex < bindIndex && bindIndex < ownershipIndex &&
            ownershipIndex < commitIndex && commitIndex < releaseLeaseIndex,
        "candidate lease must survive verified promotion and ownership commit");
        assert.equal(calls[registerLeaseIndex][1].nonce, first.nonce);
        assert.equal(calls[releaseLeaseIndex][1].nonce, first.nonce);
        assert.equal(calls.filter(call => call[0] === "register_candidate_lease").length, 1);
        assert.equal(calls.filter(call => call[0] === "release_candidate_lease").length, 1);
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
        beginCase();
        const {executor, calls} = fixture();
        const created = deferred();
        executor.chrome.windows.create = async properties => {
            calls.push(["createWindow", properties]);
            return created.promise;
        };
        const payload = ensurePayload();
        const ensuring = executor.ensure(payload);
        await waitFor(() => executor.ensureFlights.has(executor.key(payload.remote_key,
            payload.principal_id)), "ensure flight registration");
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
        beginCase();
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
        await waitFor(() => executor.ensureFlights.has(executor.key(firstPayload.remote_key,
            firstPayload.principal_id)), "shared ensure flight registration");
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
        beginCase();
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
        beginCase();
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
        beginCase();
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
            return typedResponse(operation, payload) || {};
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
        beginCase();
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
        beginCase();
        const localState = {};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        const remoteA = ensurePayload();
        const remoteB = ensurePayload({
            remote_key: "r_87654321", connection_id: "c_87654321",
            principal_id: "principal-two", request_id: "ensure-b", ensure_id: "ensure-b",
            executor_fence: {executor_id: "executor-b", epoch: 1},
        });
        const record = await executor.ensure(remoteA);
        const retire = {
            ...executor.ensureIdentity(remoteA), request_id: "retire-interleave-a",
            retire_id: "retire-interleave-a", origin_executor_fence: remoteA.executor_fence,
            window_id: record.logicalWindowId, page_id: record.logicalPageId,
        };
        const cleanupGate = deferred("interleaved retire cleanup");
        const cleanupStarted = deferred("interleaved retire start");
        const originalCleanup = executor.convergeManagedTopology.bind(executor);
        executor.convergeManagedTopology = async (...args) => {
            cleanupStarted.resolve();
            await cleanupGate.promise;
            return originalCleanup(...args);
        };
        calls.length = 0;
        const retiring = executor.retireManagedPage(retire);
        await cleanupStarted.promise;
        await executor.remoteState({...remoteB, connected: true});
        cleanupGate.resolve();
        await retiring;
        assert.equal(executor.retireConfirmedIdentities.has(retire.retire_id), true,
            "remote B state during A retire must not steal A completion authority");
        assert.equal(calls.filter(call => call[0] ===
            "retire_managed_page_ack_consumed").length, 1);
        await executor.remoteState({remote_key: remoteB.remote_key,
            connection_id: remoteB.connection_id, connected: false});
        assert.equal(executor.remoteAuthorities.has(remoteA.remote_key), true,
            "disconnecting B must not clear A authority");
        assert.equal(executor.retireRecoveryStates.has(remoteA.remote_key), true);
    }
    {
        beginCase();
        const localState = {};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        const identity = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", request_id: "retire-deferred-delivery",
            retire_id: "retire-deferred-ack", ensure_id: "ensure-deferred-ack",
            executor_fence: fence, origin_executor_fence: fence,
            window_id: logicalWindowId, page_id: logicalPageId,
        };
        const completion = {outcome: "already_retired"};
        executor.retireCompleted.set(identity.retire_id, {identity, delivery: identity, completion});
        await executor.persistRetireLedger();
        const ack = {
            remote_key: identity.remote_key, connection_id: identity.connection_id,
            principal_id: identity.principal_id, request_id: identity.request_id,
            retire_id: identity.retire_id, executor_fence: identity.executor_fence,
            response_delivery_id: "deferred-retire-response",
            origin_executor_fence: identity.origin_executor_fence, completion,
        };
        assert.equal(await executor.retireManagedPageAck(ack), true);
        assert.equal(executor.retireDeferredAcks.size, 1,
            "ACK before authority must be durably deferred");
        assert.equal(localState.abushRetireLedger.some(entry =>
            entry.phase === "ack_deferred"), true);
        assert.equal(calls.some(call => call[0] === "retire_managed_page_ack_consumed"), false);
        await executor.remoteState({...identity, connected: true});
        assert.equal(executor.retireDeferredAcks.size, 0);
        assert.equal(calls.filter(call => call[0] ===
            "retire_managed_page_ack_consumed").length, 1,
        "remote state must consume a deferred ACK exactly once");
        await executor.remoteState({...identity, connected: true});
        assert.equal(calls.filter(call => call[0] ===
            "retire_managed_page_ack_consumed").length, 1);
    }
    {
        beginCase();
        const {executor} = fixture();
        const identity = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", request_id: "retire-wrong-ack",
            retire_id: "retire-wrong-ack", ensure_id: "ensure-wrong-ack",
            executor_fence: fence, origin_executor_fence: fence,
            window_id: logicalWindowId, page_id: logicalPageId,
        };
        const completion = {outcome: "already_retired"};
        executor.retireCompleted.set(identity.retire_id, {identity, delivery: identity, completion});
        await executor.remoteState({...identity, connected: true});
        const ack = {
            remote_key: identity.remote_key, connection_id: identity.connection_id,
            principal_id: identity.principal_id, request_id: identity.request_id,
            retire_id: identity.retire_id, executor_fence: identity.executor_fence,
            response_delivery_id: "wrong-retire-response",
            origin_executor_fence: identity.origin_executor_fence, completion,
        };
        assert.equal(await executor.retireManagedPageAck({...ack,
            retire_id: "different-stable-retire"}), false);
        assert.equal(await executor.retireManagedPageAck({...ack,
            request_id: "wrong-retire-delivery"}), false);
        assert.equal(await executor.retireManagedPageAck({...ack,
            connection_id: "c_wrong0000"}), false);
        assert.equal(await executor.retireManagedPageAck({...ack,
            executor_fence: {executor_id: "wrong", epoch: 9}}), false);
        assert.equal(executor.retireDeferredAcks.size, 0,
            "wrong ACK under known authority must be rejected, not deferred");
    }
    {
        beginCase();
        let listener;
        const failures = [];
        registerNativeEventRouter({onEvent: callback => { listener = callback; }}, {
            retireManagedPageAck: async () => false,
        }, async () => {}, message => failures.push(message));
        listener({service: "abush_browser", event: "abush_browser.retire_managed_page_ack",
            payload: {remote_key: "r_12345678"}});
        await waitFor(() => failures.length === 1, "rejected retire ACK notification");
        assert.match(failures[0], /retire acknowledgement/,
            "background must surface a rejected retire ACK for retry");
    }
    {
        beginCase();
        const identity = {
            remote_key: "r_12345678", connection_id: "c_old00000",
            principal_id: "principal-one", request_id: "retire-command-old",
            retire_id: "retire-completed-ack-replay", ensure_id: "ensure-ack-replay",
            executor_fence: fence, origin_executor_fence: fence,
            window_id: logicalWindowId, page_id: logicalPageId,
        };
        const completion = {outcome: "retired"};
        const oldAck = {
            request_id: identity.request_id, retire_id: identity.retire_id,
            executor_fence: fence, response_delivery_id: "stable-retire-delivery",
            origin_executor_fence: fence, completion,
        };
        const localState = {abushRetireLedger: [{
            version: 8, phase: "completed", identity,
            delivery: identity, completion, cleanup: null,
        }]};
        const first = fixture([], [], 0, false, null, localState);
        await first.executor.restoreRetireLedger();
        const oldAuthority = {...identity, connected: true};
        first.executor.remoteAuthorities.set(identity.remote_key, oldAuthority);
        first.executor.nativeRequest = async (_service, operation, value) => {
            first.calls.push([operation, value]);
            if (operation === "retire_managed_page_ack_consumed") {
                const error = new Error("consumed ACK lost");
                error.nativeDispatch = "unknown";
                throw error;
            }
            return typedResponse(operation, payload) || {};
        };
        const originalPersist = first.executor.persistRetireLedger.bind(first.executor);
        let pendingSnapshot = null;
        first.executor.persistRetireLedger = async () => {
            await originalPersist();
            const pending = first.executor.retireConsumedPending.get(identity.retire_id);
            if (pending && pendingSnapshot === null) pendingSnapshot = structuredClone(pending);
        };
        const oldPayload = {
            remote_key: identity.remote_key, connection_id: identity.connection_id,
            principal_id: identity.principal_id, ...oldAck,
        };
        await assert.rejects(first.executor.retireManagedPageAck(oldPayload),
            /consumed ACK lost/);
        const stableConsumedId = pendingSnapshot.consumed_request_id;
        localState.abushRetireLedger = [{version: 8, phase: "consumed_pending",
            ...pendingSnapshot}];

        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restoreRetireLedger();
        const {executor, calls} = second;
        const currentFence = {executor_id: "executor-retire-reconnected", epoch: 2};
        const authority = {
            remote_key: identity.remote_key, connection_id: "c_retire_reconnect",
            principal_id: identity.principal_id, executor_fence: currentFence, connected: true,
        };
        executor.remoteAuthorities.set(identity.remote_key, authority);
        let cleanupCount = 0;
        executor.convergeManagedTopology = async () => { cleanupCount += 1; };
        executor.nativeRequest = async (_service, operation, value) => {
            calls.push([operation, value]);
            if (operation === "retire_managed_page_ack_consumed") {
                return {
                    request_id: value.request_id, retire_id: value.retire_id,
                    executor_fence: value.executor_fence,
                    response_delivery_id: value.response_delivery_id,
                };
            }
            return typedResponse(operation, value) || {};
        };
        const replayed = {
            remote_key: identity.remote_key, connection_id: authority.connection_id,
            principal_id: identity.principal_id, request_id: "completed-ack-new-delivery",
            retire_id: identity.retire_id, executor_fence: currentFence,
            response_delivery_id: oldAck.response_delivery_id,
            origin_executor_fence: identity.origin_executor_fence, completion,
        };
        assert.equal(await executor.retireManagedPageAck(replayed), true);
        const consumed = calls.find(call => call[0] ===
            "retire_managed_page_ack_consumed")[1];
        assert.equal(consumed.request_id, stableConsumedId,
            "completed ACK replay must preserve the stable consumed request ID");
        assert.equal(consumed.delivery_request_id, replayed.request_id,
            "completed ACK consumption must correlate the current replay request ID");
        assert.equal(consumed.connection_id, authority.connection_id);
        assert.deepEqual(consumed.executor_fence, currentFence);
        assert.equal(cleanupCount, 0,
            "a confirmed local physical effect must not be cleaned up twice");
        assert(calls.some(call => call[0] === "persistLocal" &&
            call[1].abushRetireLedger?.some(entry => entry.phase === "consumed_pending" &&
                entry.consumed_request_id === stableConsumedId &&
                entry.ack.request_id === replayed.request_id &&
                entry.ack_connection_id === authority.connection_id &&
                JSON.stringify(entry.delivery.executor_fence) === JSON.stringify(currentFence))),
        "completed ACK replay correlation must be durable before consumption");
        assert.deepEqual(localState.abushRetireLedger, [],
            "completed ACK replay must reach server confirmation and deletion");

        for (const conflicting of [
            {...replayed, response_delivery_id: "conflicting-retire-delivery"},
            {...replayed, completion: {outcome: "already_retired"}},
        ]) {
            const conflictState = {abushRetireLedger: [{
                version: 8, phase: "consumed_pending", identity,
                delivery: identity, completion, ack: oldAck,
                ack_connection_id: identity.connection_id,
                consumed_request_id: "stable-retire-consumed-replay",
            }]};
            const conflict = fixture([], [], 0, false, null, conflictState);
            await conflict.executor.restoreRetireLedger();
            conflict.executor.remoteAuthorities.set(identity.remote_key, authority);
            assert.equal(await conflict.executor.retireManagedPageAck(conflicting), false,
                "conflicting canonical retire ACK replay must be rejected");
            assert.equal(conflict.calls.some(call => call[0] ===
                "retire_managed_page_ack_consumed"), false);
        }
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "d".repeat(32), connectionDisplayName: "local",
            ownerDisplay: ensurePayload().owner_display,
        };
        const localState = {abushManagedCleanupBatch: {
            version: 1,
            batch_id: "old-browser-cleanup",
            source_boot: "previous-browser-boot",
            phase: "records_removed",
            records: [{
                key: `r_12345678\0principal-one`,
                record,
                phase: "converging",
            }],
        }};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        executor.chrome.windows.get = async id => ({id, type: "normal", focused: true});
        executor.chrome.tabs.query = async query => {
            if (query.windowId === undefined || query.windowId === 10) {
                return [
                    {id: 110, windowId: 10, index: 0, pinned: false,
                        url: "https://ordinary-reused.test/one"},
                    {id: 111, windowId: 10, index: 1, pinned: false,
                        url: "https://ordinary-reused.test/two"},
                    {id: 210, windowId: 10, index: 2, pinned: false,
                        url: "https://ordinary-reused.test/three"},
                ];
            }
            return [];
        };
        await executor.restoreManagedCleanupBatch();
        executor.bootstrapLeases.set("cold-lease", {
            version: 1, request_id: "cold-lease", operation: "ordinary_open",
            url: "https://cold-ordinary.test/", focus: true, deadline_ms: Date.now() + 30000,
            bootstrapContext: {windowId: 1},
        });
        executor.startupManagedRecords = Object.values(localState.abushOwnedMarkers)
            .filter(ownership => ownership.trusted !== false);
        await executor.cleanupStaleManagedBeforeBootstrap();
        assert.equal(calls.some(call => call[0] === "removeWindow"), false,
            "cold cleanup must not install quarantined physical ownership");
        assert.equal(calls.some(call => call[0] === "remove"), false,
            "cold cleanup must not remove reused tabs from quarantined ownership");
        assert.equal(calls.some(call => call[0] === "removeWindow" && call[1] === 10), false,
            "cross-boot cleanup must not remove a window by a reused numeric ID");
        assert.equal(calls.some(call => call[0] === "remove" && [110, 111, 210].includes(call[1])),
            false, "cross-boot cleanup must not remove ordinary tabs by reused numeric IDs");
        assert.equal(localState.abushManagedCleanupBatch.records[0].phase,
            "orphan_unresolved");
        assert.match(localState.abushManagedCleanupBatch.records[0].error, /relocated/,
            "unrelocated cross-boot cleanup must retain a durable advisory");
        assert.equal(localState.abushManagedCleanupBatch.records[0].record.windowId, undefined,
            "quarantined cleanup must discard stale physical window identity");
        assert.equal(localState.abushOwnedMarkers[record.nonce].trusted, false,
            "unresolved cross-boot ownership must be durably untrusted");
        assert.equal(executor.workspaces.size, 0);
        assert.equal(executor.windowOwners.size, 0);
        assert.equal(executor.tabOwners.size, 0);
        assert(localState.abushManagedCleanupBatch.records[0].logical_tombstone?.report_id,
            "quarantined cleanup must retain a logical outbox identity for daemon convergence");
        await executor.restoreQuarantinedCleanupReports();
        assert.equal(localState.abushTopologyOutbox.length, 1);
        assert.equal(localState.abushTopologyOutbox[0].page.page_id, logicalPageId);
    }
    {
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, windowGeneration: 3,
            pageGeneration: 5, windowId: 10, x11WindowId: 10010,
            statusTabIds: [110, 111], pageTabId: 210,
            nonce: "e".repeat(32), connectionDisplayName: "local",
        };
        const localState = {abushManagedCleanupBatch: {
            version: 1, batch_id: "restored-managed-cleanup",
            source_boot: "previous-browser-boot", phase: "records_removed",
            records: [{key: `r_12345678\0principal-one`, record, phase: "converging"}],
        }};
        const restoredPopup = {
            windowId: 77, tabIds: [770, 771, 772], nonce: record.nonce,
            businessUrl: "https://managed-business.test/",
        };
        const {executor, calls} = fixture([], [], 1, false, restoredPopup, localState);
        localState.abushOwnedMarkers = {[record.nonce]: {
            version: 2, nonce: record.nonce,
            window_id: record.windowId, status_tab_ids: record.statusTabIds,
            page_tab_id: record.pageTabId, xid: record.x11WindowId,
            logical_window_id: logicalWindowId, logical_page_id: logicalPageId,
            window_generation: 3, page_generation: 5,
            binding: executor.bindingPayload(record),
        }};
        await executor.restoreManagedCleanupBatch();
        assert(calls.some(call => call[0] === "bind_x11_window"),
            "cross-boot cleanup must bind and verify the current X11 window");
        assert(calls.some(call => call[0] === "removeWindow" && call[1] === 77),
            "restored managed window with entirely new IDs must be cleaned");
        assert.equal(calls.some(call => call[0] === "removeWindow" && call[1] === 1), false,
            "similar ordinary windows must remain untouched");
        assert.equal(localState.abushManagedCleanupBatch, false);
    }
    {
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "0".repeat(32),
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
        beginCase();
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
        beginCase();
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
        assert.equal(calls.filter(call => call[0] === "createWindow").length, 0,
            "last ordinary close must never create a neutral replacement");
        assert(calls.some(call => call[0] === "removeWindow" &&
            call[1] === record.windowId),
            "the extension must close the last managed window before Chrome records it " +
            "as last session");
        const persistIndex = calls.findIndex(call => call[0] === "persist");
        const closeIndex = calls.findIndex(call => call[0] === "removeWindow");
        assert(persistIndex >= 0 && closeIndex > persistIndex,
            "durable ownership removal must precede closing the last managed window");
        const intentIndex = calls.findIndex(call => call[0] === "persistLocal" &&
            call[1].abushRestoreHost?.sessionId === "ordinary-host-session");
        assert(intentIndex >= 0 && intentIndex < closeIndex,
            "exact ordinary session ownership must commit before managed cleanup");
        assert.equal(localState.abushRestoreHost.version, 3);
        assert.equal(localState.abushRestoreHost.phase, "prepared");
        assert.equal(localState.abushRestoreHost.outcome, "exact");
        assert.equal(localState.abushRestoreHost.source_boot, "test-browser-boot");
        assert.equal(localState.abushRestoreHost.target_boot, "next");
        assert.equal(localState.abushRestoreHost.sessionId, "ordinary-host-session");
        assert.deepEqual(localState.abushRestoreHost.snapshot,
            {tabCount: 1, urls: ["https://ordinary-0.test/"]});
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "0".repeat(32),
        };
        const localState = {};
        const {executor, calls} = fixture([record], [], 0, false, null, localState);
        await executor.restore();
        calls.length = 0;
        executor.removedHostSnapshots.set(1, {
            tabCount: 1, urls: ["https://ordinary-0.test/"],
        });
        executor.windowRegistry.set(1, {
            windowId: 1, kind: "ordinary",
            snapshot: {tabCount: 1, urls: ["https://ordinary-0.test/"]},
            tabIds: [2000], revision: ++executor.windowRegistryRevision, closing: true,
        });
        executor.chrome.windows.getAll = async () => [
            {id: 10, type: "normal", focused: false},
        ];
        executor.chrome.sessions = {
            getRecentlyClosed: async () => [{window: {
                sessionId: "ordinary-host-session",
                tabs: [{url: "https://ordinary-0.test/"}],
            }}],
        };
        await executor.windowRemoved(1);
        assert.equal(calls.some(call => call[0] === "createWindow"), false,
            "last ordinary close must never create a replacement window");
        assert.equal(calls.some(call => call[0] === "restoreSession"), false,
            "windows.onRemoved must never restore in the source boot");
        assert(calls.some(call => call[0] === "removeWindow" && call[1] === 10));
        assert.equal(localState.abushRestoreHost.sessionId, "ordinary-host-session");
        assert.equal(localState.abushRestoreHost.target_boot, "next");
        assert.equal(localState.abushManagedCleanupBatch, false,
            "managed cleanup batch must converge durably");
    }
    {
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "0".repeat(32),
        };
        for (const order of ["ordinary-first", "interleaved"]) {
            const localState = {};
            const {executor, calls} = fixture([record], [], 1, false, null, localState);
            await executor.restore();
            calls.length = 0;
            const second = {
                windowId: 2, kind: "ordinary",
                snapshot: {tabCount: 1, urls: ["https://second.test/"]},
                tabIds: [3000], revision: ++executor.windowRegistryRevision,
            };
            executor.windowRegistry.set(2, second);
            executor.freezeClosingWindow(1);
            if (order === "interleaved") await Promise.resolve();
            executor.freezeClosingWindow(2);
            executor.chrome.windows.getAll = async () => [
                {id: 10, type: "normal", focused: false},
            ];
            executor.chrome.sessions = {getRecentlyClosed: async () => [
                {window: {sessionId: `${order}-one`,
                    tabs: [{url: "https://ordinary-0.test/"}]}},
                {window: {sessionId: `${order}-two`,
                    tabs: [{url: "https://second.test/"}]}},
            ]};
            const firstRemoval = executor.windowRemoved(1);
            const secondRemoval = executor.windowRemoved(2);
            await Promise.all([firstRemoval, secondRemoval]);
            assert.equal(executor.workspaces.size, 1,
                `${order} multi-ordinary close must retain managed ownership`);
            assert.equal(calls.some(call => call[0] === "removeWindow"), false,
                `${order} multi-ordinary close must perform zero managed cleanup`);
            assert.equal(localState.abushRestoreHost, undefined,
                `${order} multi-ordinary close must not capture an exact handoff`);
            const ambiguity = localState.abushCloseAmbiguities.at(-1);
            assert.deepEqual(ambiguity.windows.map(window => window.window_id).sort(), [1, 2]);
            assert.deepEqual(ambiguity.session_candidates.map(candidate =>
                candidate.session_id).sort(), [`${order}-one`, `${order}-two`].sort());
        }
    }
    {
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "0".repeat(32),
        };
        for (const order of ["ordinary-first", "managed-first", "interleaved"]) {
            const {executor, calls} = fixture([record], [], 0, false, null, {});
            await executor.restore();
            calls.length = 0;
            executor.removedHostSnapshots.set(1, {
                tabCount: 1, urls: ["https://ordinary-0.test/"],
            });
            executor.windowRegistry.set(1, {
                windowId: 1, kind: "ordinary",
                snapshot: {tabCount: 1, urls: ["https://ordinary-0.test/"]},
                tabIds: [2000], revision: ++executor.windowRegistryRevision, closing: true,
            });
            executor.chrome.windows.getAll = async () => order === "managed-first" ? [] :
                [{id: 10, type: "normal", focused: false}];
            executor.chrome.sessions = {getRecentlyClosed: async () => [{window: {
                sessionId: `menu-${order}`,
                tabs: [{url: "https://ordinary-0.test/"}],
            }}]};
            if (order === "managed-first") await executor.windowRemoved(10);
            await executor.windowRemoved(1);
            if (order === "interleaved") await executor.windowRemoved(10);
            assert.equal(calls.some(call => call[0] === "createWindow"), false,
                `${order} menu exit must create zero windows`);
            assert.equal(calls.some(call => call[0] === "restoreSession"), false,
                `${order} menu exit must restore zero sessions`);
        }
    }
    {
        beginCase();
        const {executor, calls} = fixture([], [], 2);
        await executor.restore();
        calls.length = 0;
        executor.windowRegistry.set(2, {
            windowId: 2, kind: "ordinary",
            snapshot: {tabCount: 1, urls: ["https://second.test/"]},
            tabIds: [3000], revision: ++executor.windowRegistryRevision,
        });
        executor.removedHostSnapshots.set(1, {
            tabCount: 1, urls: ["https://ordinary-0.test/"],
        });
        executor.chrome.windows.getAll = async () => [
            {id: 2, type: "normal", focused: false},
        ];
        executor.chrome.sessions = {getRecentlyClosed: async () => [{window: {
            sessionId: "closed-first", tabs: [{url: "https://ordinary-0.test/"}],
        }}]};
        await executor.windowRemoved(1);
        assert.equal(calls.some(call => call[0] === "removeWindow"), false,
            "closing one of multiple ordinary windows must not touch managed windows");
        assert.equal(calls.some(call => call[0] === "createWindow" ||
            call[0] === "restoreSession"), false);
        assert(executor.recentSessionIds.has("closed-first"),
            "multiple-ordinary close must advance the session baseline");
    }
    {
        beginCase();
        const localState = {};
        const {executor} = fixture([], [], 1, false, null, localState);
        executor.removedHostSnapshots.set(1, {
            tabCount: 1, urls: ["https://identical.test/"],
        });
        executor.recentSessionIds = new Set();
        executor.chrome.sessions = {getRecentlyClosed: async () => [
            {window: {sessionId: "same-a", tabs: [{url: "https://identical.test/"}]}},
            {window: {sessionId: "same-b", tabs: [{url: "https://identical.test/"}]}},
        ]};
        const handoff = await executor.recentlyClosedHostIntent(1);
        assert.equal(handoff.outcome, "ambiguous");
        assert.equal(handoff.sessionId, null);
        assert.equal(localState.abushCloseAmbiguities.length, 1,
            "identical concurrent closes must persist ambiguity instead of guessing");
    }
    {
        beginCase();
        const {executor} = fixture([], [], 2);
        await executor.refreshWindowRegistry();
        const before = structuredClone(executor.windowRegistry.get(1).snapshot);
        const query = deferred("in-flight snapshot query");
        executor.chrome.tabs.query = async ({windowId}) => {
            assert.equal(windowId, 1);
            return query.promise;
        };
        const remembering = executor.rememberWindowSnapshot(1);
        await Promise.resolve();
        executor.freezeClosingWindow(1);
        query.resolve([{id: 2001, windowId: 1, index: 0,
            url: "https://ordinary-1.test/"}]);
        await remembering;
        assert.deepEqual(executor.removedHostSnapshots.get(1), before,
            "a query captured before freeze must never replace the frozen full snapshot");
        assert.equal(executor.windowRegistry.get(1).closing, true);
    }
    {
        beginCase();
        const {executor} = fixture([], [], 2);
        await executor.refreshWindowRegistry();
        const before = structuredClone(executor.windowRegistry.get(1).snapshot);
        executor.freezeClosingWindow(1);
        const tabs = await executor.chrome.tabs.query({windowId: 1});
        tabs.splice(0, 1);
        await executor.tabRemoved(2000, {windowId: 1, isWindowClosing: true});
        await executor.rememberWindowSnapshot(1);
        assert.deepEqual(executor.removedHostSnapshots.get(1), before,
            "tabs.onRemoved while closing must preserve the frozen full-window snapshot");
    }
    {
        beginCase();
        const record = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "0".repeat(32),
        };
        const {executor, calls} = fixture([record]);
        await executor.restore();
        calls.length = 0;
        executor.closingManagedWindows.add(10);
        await executor.windowRemoved(10);
        assert.equal(calls.some(call => call[0] === "createWindow" ||
            call[0] === "restoreSession"), false,
        "self-generated managed removal must not re-enter ordinary close handling");
    }
    {
        beginCase();
        const managed = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 20, x11WindowId: 10020, statusTabIds: [120, 121], pageTabId: 220,
            nonce: "0".repeat(32), connectionDisplayName: "local",
        };
        const localState = {};
        const {executor, calls} = fixture([managed], [], 0, false, null, localState);
        const originalGetAll = executor.chrome.windows.getAll;
        executor.chrome.windows.getAll = async options => (await originalGetAll(options))
            .filter(window => window.id !== 1);
        const lease = {
            version: 1, request_id: "cold-request", operation: "ordinary_open",
            deadline_ms: Date.now() + 30000, url: "https://cold.test/", focus: true,
        };
        executor.bootstrapLeases.set(lease.request_id, lease);
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "prepare") return {xid: 42, nonce: payload.nonce};
            if (operation === "verify_x11_window") return {verified: true, xid: payload.xid};
            if (operation === "clear_x11_window") {
                return {cleared: true, xid: payload.xid, reclassified_group: "f"};
            }
            return typedResponse(operation, payload) || {};
        };
        await executor.restore();
        const stage = calls.findIndex(call => call[0] === "createWindow" &&
            call[1].url?.includes("ordinary-staging.html#"));
        const cleanup = calls.findIndex(call => call[0] === "removeWindow" && call[1] === 20);
        const navigation = calls.findIndex(call => call[0] === "update" &&
            call[2]?.url === lease.url);
        assert(stage >= 0 && cleanup > stage && navigation > cleanup,
            "cold lease must stage before managed cleanup, then navigate exactly once");
        assert.equal(calls.filter(call => call[0] === "update" &&
            call[2]?.url === lease.url).length, 1);
        assert.equal(localState.abushColdOrdinaryLedger.at(-1).completion, "applied");
    }
    {
        beginCase();
        const payload = {
            version: 1, request_id: "crashed-cold", operation: "ordinary_open",
            deadline_ms: Date.now() + 30000, url: "https://cold-once.test/", focus: true,
        };
        const localState = {abushColdOrdinaryLedger: [{
            version: 1,
            request_id: payload.request_id,
            url_hash: await executorHash(payload.url),
            deadline_ms: payload.deadline_ms,
            phase: "effect_started",
            completion: null,
            browser_boot_id: "test-browser-boot",
            target_window_id: 1,
            target_tab_id: 2000,
        }]};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        const tabs = await executor.chrome.tabs.query({windowId: 1});
        tabs[0].url = payload.url;
        executor.bootstrapLeases.set("crashed-cold", payload);
        await executor.executeBootstrapLeases();
        assert.equal(calls.some(call => call[0] === "createWindow" ||
            call[0] === "createTab" || call[0] === "update" && call[2]?.url), false,
        "effect-started cold request must not execute a second URL mutation after restart");
        assert.equal(localState.abushColdOrdinaryLedger[0].completion, "applied",
            "an exact live URL must reconcile a recovered effect as applied");
        assert.equal(calls.filter(call => call[0] === "ordinary_open_completion").length, 1);
    }
    {
        beginCase();
        const payload = {
            version: 1, request_id: "reconnected-effect", operation: "ordinary_open",
            deadline_ms: Date.now() + 30000, url: "https://reconnected-once.test/", focus: true,
        };
        const localState = {abushColdOrdinaryLedger: [{
            version: 1, request_id: payload.request_id,
            url_hash: await executorHash(payload.url), deadline_ms: payload.deadline_ms,
            phase: "effect_started", completion: null,
            browser_boot_id: "test-browser-boot", target_window_id: 1, target_tab_id: 2000,
        }]};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        const tabs = await executor.chrome.tabs.query({windowId: 1});
        tabs[0].url = payload.url;
        executor.restoreAttempt = {generation: 1, promise: Promise.resolve()};
        executor.restoreGeneration = 1;
        const bootstrap = {
            browser_boot_id: "test-browser-boot", browser_pid: 42,
            browser_starttime: "123", ordinary_launch_leases: [payload],
        };
        await executor.acceptNativeBootstrap(bootstrap);
        await executor.acceptNativeBootstrap(bootstrap);
        assert.equal(calls.filter(call => call[0] === "ordinary_open_completion").length, 1,
            "reconnected bootstrap must reconcile an effect-started lease exactly once");
        assert.equal(calls.some(call => call[0] === "update" && call[2]?.url === payload.url), false,
            "reconnected effect-started lease must not repeat URL mutation");
    }
    {
        beginCase();
        const managed = {
            remoteKey: "r_12345678", principalId: "principal-one",
            workspaceId: "workspace-authoritative", connectionId: "c_12345678",
            executorFence: fence, logicalWindowId, logicalPageId, generation: 1,
            windowId: 10, x11WindowId: 10010, statusTabIds: [110, 111], pageTabId: 210,
            nonce: "0".repeat(32), connectionDisplayName: "local",
        };
        const {executor, calls} = fixture([managed], [], 0);
        const originalGetAll = executor.chrome.windows.getAll;
        executor.chrome.windows.getAll = async options => (await originalGetAll(options))
            .filter(window => window.id !== 1);
        await executor.restore();
        assert.equal(calls.some(call => call[0] === "createWindow"), false,
            "stale managed-only startup without a lease must not create a neutral window");
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
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
        beginCase();
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
                const lease = candidateLeaseResponse(operation, payload);
                if (lease !== null) return lease;
                if (operation === "bind_x11_window") return result;
                return typedResponse(operation, payload) || {};
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
        beginCase();
        const {executor, calls} = fixture();
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            const lease = candidateLeaseResponse(operation, payload);
            if (lease !== null) return lease;
            if (operation !== "bind_x11_window") return typedResponse(operation, payload) || {};
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
        beginCase();
        const {executor, calls} = fixture();
        let attempts = 0;
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            const lease = candidateLeaseResponse(operation, payload);
            if (lease !== null) return lease;
            if (operation === "bind_x11_window" && attempts++ === 0) {
                throw new Error("bind failed");
            }
            return typedResponse(operation, payload) || {};
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
        beginCase();
        const {executor, calls} = fixture();
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            const lease = candidateLeaseResponse(operation, payload);
            if (lease !== null) return lease;
            if (operation === "bind_x11_window") throw new Error("bind failed");
            return typedResponse(operation, payload) || {};
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
        beginCase();
        const {executor} = fixture();
        executor.nativeRequest = async (_service, operation, payload) => {
            const lease = candidateLeaseResponse(operation, payload);
            if (lease !== null) return lease;
            throw new Error(operation === "ensure_managed_page_completion" ?
                "completion failed" : "bind failed");
        };
        await assert.rejects(executor.ensure(ensurePayload()), error =>
            error instanceof AggregateError && error.errors.length === 2);
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
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
        await waitFor(() => calls.some(call => call[0] === "command"),
            "navigate command dispatch");
        executor.cancel({...executor.operationIdentity(payload), request_id: "cancel-request",
            execution_request_id: payload.request_id});
        navigate.resolve();
        await executing;
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "unknown", "cancellation after mutation send must not report not_applied or success");
    }
    {
        beginCase();
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
        await waitFor(() => calls.some(call => call[0] === "attach"),
            "debugger attach dispatch");
        executor.cancel({...executor.operationIdentity(payload), request_id: "cancel-request",
            execution_request_id: payload.request_id});
        attach.resolve();
        await executing;
        assert(!calls.some(call => call[0] === "command"));
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "not_applied");
    }
    {
        beginCase();
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
        await waitFor(() => calls.some(call => call[0] === "command"),
            "navigate command dispatch");
        await executor.remoteState({remote_key: payload.remote_key,
            connection_id: payload.connection_id, connected: false});
        navigate.resolve();
        await executing;
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "unknown");
    }
    {
        beginCase();
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
        await waitFor(() => calls.some(call => call[0] === "command"),
            "navigate command dispatch");
        navigate.resolve();
        executor.cancel({...executor.operationIdentity(payload), request_id: "cancel-request",
            execution_request_id: payload.request_id});
        await executing;
        assert.equal(calls.filter(call => call[0] === "executor_completion").at(-1)[1].completion,
            "unknown", "cancellation after navigate resolves must suppress success");
    }
    {
        beginCase();
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
        await waitFor(() => calls.some(call => call[0] === "persist"),
            "session persistence start");
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
        assert.equal(executor.operations.size, 0,
            "terminal execution must release all operation waiters");
        assert.deepEqual(executor.operationCancelSnapshot(), [],
            "terminal cancellation must consume its durable waiter");
        assert.deepEqual(calls.filter(call => call[0] === "executor_completion")
            .map(call => call[1].completion), ["unknown"],
        "late cancellation must produce exactly one executor completion");
        assert.equal(calls.filter(call => call[0] === "cancellation_completion").at(-1)[1]
            .completion, "unknown",
        "cancellation after navigate commit must never acknowledge not_applied");
    }
    {
        beginCase();
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
        assert.equal(executor.operations.size, 0);
        assert.deepEqual(executor.operationCancelSnapshot(), []);
    }
    {
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const payload = {...executePayload(), effect_id: "cancel-before-register", command: {}};
        const cancellation = {
            ...first.executor.operationIdentity(payload),
            request_id: "cancel-before-register-request",
            execution_request_id: payload.request_id,
        };
        await first.executor.cancel(cancellation);
        assert.deepEqual(localState.abushPendingOperations, [cancellation],
            "a cancellation arriving before execute must remain durable");

        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restoreOperationCancels();
        await second.executor.execute(payload);
        assert.deepEqual(localState.abushPendingOperations, [],
            "restored cancellation must be consumed by the matching execution");
        assert.deepEqual(second.calls.filter(call => call[0] === "executor_completion")
            .map(call => call[1].completion), ["not_applied"]);
        assert.deepEqual(second.calls.filter(call => call[0] === "cancellation_completion")
            .map(call => call[1].completion), ["not_applied"]);
        assert.equal(second.executor.operations.size, 0);
        assert.equal(second.executor.pendingOperationCancels.size, 0);
    }
    {
        beginCase();
        const localState = {};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        const execution = {...executePayload(), effect_id: "cancel-ack-sequence", command: {}};
        const identity = executor.operationEffectIdentity(execution);
        const operationKey = executor.operationKey(execution);
        const effectKey = executor.operationEffectKey(execution);
        const firstCancel = {...executor.operationIdentity(execution),
            request_id: "cancel-ack-one", execution_request_id: execution.request_id};
        const secondCancel = {...executor.operationIdentity(execution),
            request_id: "cancel-ack-two", execution_request_id: execution.request_id};
        executor.executionLedger.set(effectKey, {
            version: 1, phase: "acked", identity, completion: "unknown",
        });
        executor.pendingOperationCancels.set(operationKey, [firstCancel, secondCancel]);
        await executor.persistExecutionLedger();
        await executor.persistOperationCancels();
        let failSecond = true;
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "cancellation_completion" &&
                    payload.request_id === secondCancel.request_id && failSecond) {
                failSecond = false;
                throw new Error("second cancellation ACK unavailable");
            }
            return typedResponse(operation, payload) || {};
        };
        await executor.flushExecutionLedger();
        assert.deepEqual(calls.filter(call => call[0] === "cancellation_completion")
            .map(call => call[1].request_id), [firstCancel.request_id, secondCancel.request_id]);
        assert.deepEqual(localState.abushPendingOperations, [secondCancel],
            "each successful cancellation ACK must be durably consumed before the next send");
        calls.length = 0;
        await executor.flushExecutionLedger();
        assert.deepEqual(calls.filter(call => call[0] === "cancellation_completion")
            .map(call => call[1].request_id), [secondCancel.request_id],
        "retry must not duplicate a cancellation whose ACK was already accepted");
        assert.deepEqual(localState.abushPendingOperations, []);
        assert.deepEqual(localState.abushExecutionLedger, []);
    }
    {
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        first.calls.length = 0;
        const payload = {...executePayload(), effect_id: "durable-effect-start", command: {
            command: "navigate", page: first.executor.pageRef(record),
            url: "https://durable-effect.test/",
        }};
        const originalCommand = first.executor.chrome.debugger.sendCommand;
        first.executor.chrome.debugger.sendCommand = async (...args) => {
            await originalCommand(...args);
            throw new Error("crash after physical mutation");
        };
        first.executor.nativeRequest = async (_service, operation, value) => {
            first.calls.push([operation, value]);
            if (operation === "executor_completion") throw new Error("native ACK unavailable");
            return typedResponse(operation, value) || {};
        };
        await first.executor.execute(payload);
        assert.equal(localState.abushExecutionLedger[0].phase, "terminal_pending_ack");
        assert.equal(localState.abushExecutionLedger[0].completion, "unknown");
        const second = fixture([record], [], 0, false, null, localState);
        await second.executor.restoreExecutionLedger();
        await second.executor.flushExecutionLedger();
        assert.equal(second.calls.some(call => call[0] === "command"), false,
            "restart must never re-execute a started durable effect");
        const ledgerWrites = second.calls.filter(call => call[0] === "persistLocal" &&
            Array.isArray(call[1].abushExecutionLedger));
        const ackedIndex = ledgerWrites.findIndex(call =>
            call[1].abushExecutionLedger[0]?.phase === "acked");
        const clearIndex = ledgerWrites.findIndex(call => call[1].abushExecutionLedger.length === 0);
        assert(ackedIndex >= 0 && clearIndex > ackedIndex,
            "native ACK must become durable before the execution ledger is cleared");
        assert.deepEqual(localState.abushExecutionLedger, [],
            "ledger clears only after native executor completion ACK succeeds");
    }
    {
        beginCase();
        const localState = {};
        const {executor} = fixture([], [], 0, false, null, localState);
        const firstWrite = deferred("first execute registration persistence");
        let writes = 0;
        const originalSet = executor.chrome.storage.local.set;
        executor.chrome.storage.local.set = async value => {
            if (Array.isArray(value.abushExecutionLedger) && writes++ === 0) {
                await firstWrite.promise;
            }
            await originalSet(value);
        };
        const first = {...executePayload(), effect_id: "registration-one", command: {command: "x"}};
        const second = {...executePayload({request_id: "request-two"}),
            effect_id: "registration-two", command: {command: "y"}};
        const firstRegistration = executor.registerOperation(first);
        await waitFor(() => writes === 1, "first execute registration write");
        const secondRegistration = executor.registerOperation(second);
        firstWrite.resolve();
        assert.deepEqual(await Promise.all([firstRegistration, secondRegistration]), [true, true]);
        assert.deepEqual(localState.abushExecutionLedger.map(entry => entry.identity.effect_id),
            [first.effect_id, second.effect_id],
        "serialized execute registration must preserve every durable accepted effect");
    }
    {
        beginCase();
        const localState = {};
        const {executor} = fixture([], [], 0, false, null, localState);
        const first = {...executePayload(), effect_id: "same-effect-id", command: {command: "x"}};
        await executor.registerOperation(first);
        await assert.rejects(executor.registerOperation({...first,
            request_id: "different-request", command: {command: "y"}}),
        /conflicting duplicate execute effect/);
    }
    {
        beginCase();
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
        await waitFor(() => calls.some(call => call[0] === "persist"),
            "session persistence start");
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
        beginCase();
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
        beginCase();
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
        const queued = calls.find(call => call[0] === "persistLocal" &&
            call[1].abushTopologyOutbox?.length === 1);
        assert(queued && queued[1].abushTopologyOutbox[0].page.page_id === logicalPageId,
            "invalid durable record must queue its stable page topology before removal");
        assert.deepEqual(calls.find(call => call[0] === "persist")[1].abushWorkspaces, []);
    }
    {
        beginCase();
        const ports = [];
        const makePort = () => {
            const listeners = {message: [], disconnect: []};
            const port = {
                listeners,
                sent: [],
                onMessage: {addListener: listener => listeners.message.push(listener)},
                onDisconnect: {addListener: listener => listeners.disconnect.push(listener)},
                postMessage(message) { this.sent.push(message); },
                disconnect() {},
            };
            ports.push(port);
            return port;
        };
        const reconnectTimers = [];
        const dispatcher = new NativeDispatcher(makePort, 1000, 1000);
        dispatcher.setTimeout = (callback, delay) => {
            reconnectTimers.push({callback, delay, cleared: false});
            return reconnectTimers.length - 1;
        };
        dispatcher.clearTimeout = id => {
            if (reconnectTimers[id]) reconnectTimers[id].cleared = true;
        };
        const firstBootstrap = dispatcher.bootstrap();
        assert.equal(ports.length, 1);
        assert.equal(ports[0].sent.length, 1);
        assert.equal(ports[0].sent[0].service, "native_host");
        assert.equal(ports[0].sent[0].operation, "bootstrap");
        ports[0].listeners.disconnect[0]();
        await assert.rejects(exactWait(firstBootstrap, "disconnected native bootstrap"),
            /disconnected/);
        const reconnect = reconnectTimers.find(timer => timer.delay === 100 && !timer.cleared);
        assert(reconnect, "bootstrap disconnect must schedule a reconnect attempt");
        reconnect.callback();
        assert.equal(ports.length, 2);
        assert.equal(ports[1].sent.length, 1,
            "every native generation must request a replayable bootstrap");
        const secondBootstrap = dispatcher.bootstrap();
        const payload = {
            browser_boot_id: "bootstrap-reconnected",
            browser_pid: 42,
            browser_starttime: "123",
            ordinary_launch_leases: [],
        };
        ports[1].listeners.message[0]({
            version: 1,
            reply_to: ports[1].sent[0].id,
            ok: true,
            payload,
        });
        assert.deepEqual(await exactWait(secondBootstrap,
            "reconnected native bootstrap"), payload);
        dispatcher.stop();
    }
    {
        beginCase();
        const {executor} = fixture();
        let attempts = 0;
        executor.restore = async () => {
            attempts += 1;
            if (attempts === 1) throw new Error("transient storage failure");
        };
        const first = executor.restoreWhenReady();
        assert.strictEqual(executor.whenReady(), first,
            "one restore generation must remain singleflight");
        await assert.rejects(first, /transient storage failure/);
        assert.equal(executor.restoreAttempt, null,
            "a rejected restore generation must release readiness");
        await executor.restoreWhenReady();
        assert.equal(attempts, 2, "the next bootstrap must retry restore");
        await executor.whenReady();
    }
    {
        beginCase();
        const failures = [];
        const attempts = [Promise.reject(new Error("bootstrap failed")), Promise.resolve({
            browser_boot_id: "bootstrap-retry",
            browser_pid: 43,
            browser_starttime: "124",
            ordinary_launch_leases: [],
        })];
        const native = {
            bootstrap: () => attempts.shift(),
            ensurePort() {},
        };
        let restores = 0;
        const workspaces = {
            acceptNativeBootstrap: async () => {},
            restoreWhenReady: async () => { restores += 1; },
            flushTopologyOutbox: async () => {},
            reportAdvisoryError: () => {},
            drainAdvisoryErrors: () => [],
        };
        const initialize = createWorkspaceInitializer(
            workspaces, native, message => failures.push(message));
        await assert.rejects(initialize(), /bootstrap failed/);
        assert.equal(restores, 0);
        assert.strictEqual(await initialize(), workspaces,
            "failed workspace readiness must permit a later bootstrap generation");
        assert.equal(restores, 1);
        assert.deepEqual(failures, ["bootstrap failed"]);
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
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
        await waitFor(() => calls.some(call => call[0] === "persist"),
            "session persistence start");
        assert.strictEqual(executor.workspaces.get(
            executor.key(record.remoteKey, record.principalId)), record,
        "persist-before-effect must retain in-memory authority until the write commits");
        assert.equal(calls.some(call => call[0] === "detach"), false);
        persisted.resolve();
        await converging;
        assert.equal(executor.workspaces.size, 0);
    }
    {
        beginCase();
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
        beginCase();
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
        beginCase();
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
            return typedResponse(operation, payload) || {};
        };
        const advisory = [];
        executor.onAdvisoryError = error => advisory.push(error);
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.push({id: 9999, windowId: record.windowId, index: 3, pinned: false});
        await executor.tabMoved(record.pageTabId, {windowId: 999});
        assert.equal(executor.workspaces.size, 0,
            "native report failure must not block local ownership convergence");
        const invalidation = calls.find(call => call[0] === "sendTabMessage");
        assert.equal(invalidation[1], record.pageTabId);
        assert.deepEqual(invalidation[2], {
            type: "abush_ownership_changed", owned: false,
            generation: invalidation[2].generation,
        });
        assert(Number.isSafeInteger(invalidation[2].generation));
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
        beginCase();
        const {executor, calls} = fixture();
        const bindGate = deferred();
        const originalNative = executor.nativeRequest;
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") await bindGate.promise;
            return originalNative(service, operation, payload);
        };
        const ensuring = executor.ensure(ensurePayload());
        await waitFor(() => calls.some(call => call[0] === "bind_x11_window"),
            "X11 bind dispatch");
        const unrelated = executor.enqueueLifecycle(() => false);
        bindGate.resolve();
        const record = await ensuring;
        await unrelated;
        assert(record, "unrelated lifecycle work must not reject an owned transaction");
    }
    {
        beginCase();
        const {executor, calls} = fixture();
        const bindGate = deferred();
        const originalNative = executor.nativeRequest;
        executor.nativeRequest = async (service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "bind_x11_window") await bindGate.promise;
            return originalNative(service, operation, payload);
        };
        const ensuring = executor.ensure(ensurePayload());
        await waitFor(() => calls.some(call => call[0] === "bind_x11_window"),
            "X11 bind dispatch");
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
        beginCase();
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
        beginCase();
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
        await waitFor(() => checked, "initial topology check");
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
        beginCase();
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
        beginCase();
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
        beginCase();
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
        beginCase();
        const {executor, calls} = fixture();
        const record = await executor.ensure(ensurePayload());
        const payload = {
            ...executor.ensureIdentity(ensurePayload()),
            retire_id: "retire-one",
            origin_executor_fence: fence,
            window_id: record.logicalWindowId,
            page_id: record.logicalPageId,
        };
        const tabs = await executor.chrome.tabs.query({windowId: record.windowId});
        tabs.splice(tabs.findIndex(tab => tab.id === record.pageTabId), 1);
        calls.length = 0;
        assert.equal(await executor.retireManagedPage(payload), true);
        assert.equal(executor.workspaces.size, 0);
        assert.equal(calls.filter(call =>
            call[0] === "retire_managed_page_completion").length, 1);
        assert.equal(calls.filter(call =>
            call[0] === "retire_managed_page_ack_consumed").length, 1);
        assert.deepEqual(executor.retireResponseReceived.size, 0);
        assert.equal(await executor.retireManagedPage(payload), true,
            "duplicate retire replay must converge without waiting");
        assert.equal(calls.filter(call =>
            call[0] === "retire_managed_page_completion").length, 1,
            "a locally confirmed duplicate retire must remain idempotent");
    }
    {
        beginCase();
        const localState = {};
        const {executor} = fixture([], [], 0, false, null, localState);
        const record = await executor.ensure(ensurePayload());
        const originalCleanup = executor.convergeManagedTopology.bind(executor);
        let cleanupCount = 0;
        let releaseCleanup;
        executor.convergeManagedTopology = async (...args) => {
            cleanupCount += 1;
            await new Promise(resolve => { releaseCleanup = resolve; });
            return originalCleanup(...args);
        };
        executor.nativeRequest = async (_service, operation, payload) => {
            if (operation === "retire_managed_page_completion") throw new Error("hold completion");
            return typedResponse(operation, payload) || {};
        };
        const firstPayload = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", request_id: "retire-command-old",
            retire_id: "stable-retire-singleflight", ensure_id: "ensure-retire-singleflight",
            executor_fence: fence, origin_executor_fence: fence,
            window_id: record.logicalWindowId, page_id: record.logicalPageId,
        };
        const first = executor.retireManagedPage(firstPayload);
        await waitFor(() => releaseCleanup !== undefined, "retire physical cleanup start");
        const secondPayload = {...firstPayload, request_id: "retire-command-new"};
        const second = executor.retireManagedPage(secondPayload);
        releaseCleanup();
        await Promise.all([first, second]);
        assert.equal(cleanupCount, 1,
            "same stable retire ID must singleflight physical cleanup");
        const completion = executor.retireCompleted.get(firstPayload.retire_id);
        assert.equal(completion.identity.request_id, secondPayload.request_id,
            "new retire delivery must replace command request correlation");
        assert.equal(completion.delivery.request_id, secondPayload.request_id,
            "completion must echo the received command request ID");
        assert.equal(localState.abushRetireLedger[0].identity.request_id,
            secondPayload.request_id);
    }
    {
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        const payload = {
            ...first.executor.ensureIdentity(ensurePayload()),
            request_id: "retire-completed-old-command",
            retire_id: "retire-completed-new-delivery",
            origin_executor_fence: fence,
            window_id: record.logicalWindowId,
            page_id: record.logicalPageId,
        };
        first.executor.nativeRequest = async (_service, operation, payload) => {
            if (operation === "retire_managed_page_completion") throw new Error("offline");
            return typedResponse(operation, payload) || {};
        };
        let cleanupCount = 0;
        const originalCleanup = first.executor.convergeManagedTopology.bind(first.executor);
        first.executor.convergeManagedTopology = async (...args) => {
            cleanupCount += 1;
            return originalCleanup(...args);
        };
        await first.executor.retireManagedPage(payload);
        assert.equal(cleanupCount, 1);
        const second = fixture([], [], 0, false, null, localState);
        const replay = {...payload, request_id: "retire-completed-new-command"};
        second.executor.nativeRequest = async (_service, operation, value) => {
            second.calls.push([operation, value]);
            if (operation === "retire_managed_page_completion") {
                return {
                    request_id: value.request_id, retire_id: value.retire_id,
                    executor_fence: value.executor_fence,
                    response_delivery_id: "stable-retire-response",
                    origin_executor_fence: value.origin_executor_fence,
                    completion: value.completion,
                };
            }
            if (operation === "retire_managed_page_ack_consumed") {
                return {
                    request_id: value.request_id, retire_id: value.retire_id,
                    executor_fence: value.executor_fence,
                    response_delivery_id: value.response_delivery_id,
                };
            }
            return typedResponse(operation, value) || {};
        };
        await second.executor.restoreRetireLedger();
        await second.executor.retireManagedPage(replay);
        const sent = second.calls.find(call =>
            call[0] === "retire_managed_page_completion")[1];
        assert.equal(sent.request_id, replay.request_id,
            "completed stable retire replay must echo the new command request ID");
        assert.deepEqual(localState.abushRetireLedger, []);
    }
    {
        beginCase();
        const failureCases = ["window", "x11", "marker"];
        for (const failure of failureCases) {
            const localState = {};
            const first = fixture([], [], 0, false, null, localState);
            const record = await first.executor.ensure(ensurePayload({
                request_id: `ensure-retire-${failure}`,
                ensure_id: `ensure-retire-${failure}`,
            }));
            const payload = {
                ...first.executor.ensureIdentity(ensurePayload({
                    request_id: `retire-${failure}`,
                    ensure_id: `ensure-retire-${failure}`,
                })),
                retire_id: `retire-cleanup-${failure}`,
                origin_executor_fence: fence,
                window_id: record.logicalWindowId,
                page_id: record.logicalPageId,
            };
            if (failure === "window") {
                first.executor.chrome.windows.remove = async () => {
                    throw new Error("window cleanup failed");
                };
            } else if (failure === "x11") {
                first.executor.chrome.windows.remove = async () => {};
                const originalNative = first.executor.nativeRequest;
                first.executor.nativeRequest = async (service, operation, value) => {
                    if (operation === "clear_x11_window") throw new Error("X11 cleanup failed");
                    return originalNative(service, operation, value);
                };
            } else {
                first.executor.disownMarker = async () => {
                    throw new Error("marker cleanup failed");
                };
            }
            await assert.rejects(first.executor.retireManagedPage(payload), /cleanup/);
            assert.equal(localState.abushRetireLedger.some(entry =>
                entry.phase === "effect_started" && entry.identity.retire_id ===
                    payload.retire_id), true,
            `${failure} failure must retain effect_started`);
            assert.equal(first.calls.some(call => call[0] ===
                "retire_managed_page_completion"), false,
            `${failure} failure must not emit retire completion`);

            const second = fixture([], [], 0, false, null, localState);
            await second.executor.restoreRetireLedger();
            const cleanupId = second.executor.retireEffectStarted
                .get(payload.retire_id)?.cleanup?.cleanup_id;
            second.calls.length = 0;
            await second.executor.remoteState({...payload, connected: true});
            assert.equal(second.calls.filter(call => call[0] ===
                "retire_managed_page_completion").length, 1,
            `${failure} cleanup must complete exactly once after restart retry`);
            assert.equal(localState.abushRetireLedger.some(entry =>
                entry.identity?.retire_id === payload.retire_id ||
                    entry.outcome?.cleanup_id === cleanupId), false,
            `${failure} cleanup ledger must clear after confirmed retry`);
        }
    }
    {
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        const payload = {
            ...first.executor.ensureIdentity(ensurePayload()),
            retire_id: "retire-crash",
            origin_executor_fence: fence,
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
        assert.equal(localState.abushRetireLedger[0].phase, "accepted");
        const second = fixture([record], [], 0, false, null, localState);
        await second.executor.restore();
        assert.equal(second.executor.workspaces.size, 1,
            "restart must retain accepted retire work until a peer can receive its ACK");
        second.executor.nativeRequest = async (_service, operation, value) => {
            second.calls.push([operation, value]);
            if (operation === "retire_managed_page_completion") {
                throw new Error("retire ACK lost");
            }
            return first.executor.nativeRequest(_service, operation, value);
        };
        await second.executor.remoteState({
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            executor_fence: payload.executor_fence,
            connected: true,
        });
        assert.equal(second.executor.workspaces.size, 0,
            "connected peer recovery must finish the accepted retire before ACK");
        assert.equal(localState.abushRetireLedger[0].phase, "completed");
        assert(second.calls.some(call =>
            call[0] === "retire_managed_page_completion"));
        second.executor.nativeRequest = async (_service, operation, value) => {
            second.calls.push([operation, value]);
            if (operation === "retire_managed_page_completion") {
                return {
                    request_id: value.request_id,
                    retire_id: value.retire_id,
                    executor_fence: value.executor_fence,
                    response_delivery_id: `retire-response-${value.retire_id}`,
                    origin_executor_fence: value.origin_executor_fence,
                    completion: value.completion,
                };
            }
            if (operation === "retire_managed_page_ack_consumed") {
                return {
                    request_id: value.request_id,
                    retire_id: value.retire_id,
                    executor_fence: value.executor_fence,
                    response_delivery_id: value.response_delivery_id,
                };
            }
            return typedResponse(operation, value) || {};
        };
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
            call[0] === "retire_managed_page_completion" &&
            call[1].connection_id === rotated.connection_id &&
            JSON.stringify(call[1].executor_fence) ===
                JSON.stringify(rotated.executor_fence)),
        "completed retire identity must reauthorize its ACK on a new fence");
    }
    {
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        const payload = {
            ...first.executor.ensureIdentity(ensurePayload()),
            retire_id: "retire-rotation-before-effect",
            origin_executor_fence: fence,
            window_id: record.logicalWindowId,
            page_id: record.logicalPageId,
        };
        first.executor.retirePending.set(payload.retire_id,
            first.executor.retireIdentity(payload));
        await first.executor.persistRetireLedger();
        const rotatedFence = {executor_id: "executor-rotated", epoch: 2};
        const second = fixture([record], [], 0, false, null, localState);
        await second.executor.restore();
        await second.executor.remoteState({
            remote_key: payload.remote_key,
            connection_id: "c_rotated0",
            principal_id: payload.principal_id,
            executor_fence: rotatedFence,
            connected: true,
        });
        assert.equal(second.executor.workspaces.size, 0,
            "accepted retire must physically converge after authority rotation");
        const completion = second.calls.find(call =>
            call[0] === "retire_managed_page_completion")[1];
        assert.equal(completion.connection_id, "c_rotated0");
        assert.deepEqual(completion.executor_fence, rotatedFence);
        assert.equal(completion.completion.outcome, "retired",
            "recovered effect may complete only after physical convergence");
    }
    {
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        const payload = {
            ...first.executor.ensureIdentity(ensurePayload()),
            retire_id: "retire-restart",
            origin_executor_fence: fence,
            window_id: record.logicalWindowId,
            page_id: record.logicalPageId,
        };
        await first.executor.retireManagedPage(payload);
        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restore();
        second.calls.length = 0;
        assert.equal(await second.executor.retireManagedPage(payload), true);
        assert.equal(second.calls.filter(call =>
            call[0] === "retire_managed_page_completion").length, 1,
        "service-worker restart must replay the durable terminal retire completion");
    }
    {
        beginCase();
        const localState = {};
        const {executor} = fixture([], [], 0, false, null, localState);
        const identity = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", request_id: "retire-conflict-one",
            retire_id: "stable-retire-conflict", ensure_id: "ensure-conflict",
            executor_fence: fence, origin_executor_fence: fence,
            window_id: logicalWindowId, page_id: logicalPageId,
        };
        executor.nativeRequest = async (_service, operation, payload) => {
            if (operation === "retire_managed_page_completion") {
                throw new Error("hold canonical completion for conflict test");
            }
            return typedResponse(operation, payload) || {};
        };
        await executor.retireManagedPage(identity);
        await assert.rejects(executor.retireManagedPage({
            ...identity, request_id: "retire-conflict-two", page_id: "different-page",
        }), /conflicting duplicate retire request/);
        assert.equal(executor.retireCompleted.size, 1,
            "conflicting stable retire ID must retain the canonical completion");
    }
    {
        beginCase();
        const identity = {
            remote_key: "r_12345678", connection_id: "c_old00000",
            principal_id: "principal-one", request_id: "retire-old-delivery",
            retire_id: "retire-ack-replay", ensure_id: "ensure-ack-replay",
            executor_fence: fence, origin_executor_fence: fence,
            window_id: logicalWindowId, page_id: logicalPageId,
        };
        const completion = {outcome: "already_retired"};
        const oldAck = {
            request_id: identity.request_id, retire_id: identity.retire_id,
            executor_fence: fence, response_delivery_id: "stable-retire-response",
            origin_executor_fence: fence, completion,
        };
        const localState = {abushRetireLedger: [{
            version: 8, phase: "response_received", identity,
            delivery: identity, completion, ack: oldAck,
            ack_connection_id: identity.connection_id,
        }]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreRetireLedger();
        const replayFence = {executor_id: "executor-new", epoch: 2};
        await executor.remoteState({
            remote_key: identity.remote_key, connection_id: "c_new00000",
            principal_id: identity.principal_id, executor_fence: replayFence,
            connected: true,
        });
        assert.deepEqual(localState.abushRetireLedger, [],
            "old-authority ACK must be consumed through the current authority");
        const consumed = calls.find(call => call[0] ===
            "retire_managed_page_ack_consumed")[1];
        assert.notEqual(consumed.request_id, oldAck.request_id,
            "consumption must use a distinct durable request ID");
        assert.equal(consumed.connection_id, "c_new00000");
        assert.equal(consumed.delivery_request_id, oldAck.request_id);
        assert.equal(consumed.response_delivery_id, oldAck.response_delivery_id);
        assert.deepEqual(consumed.executor_fence, replayFence);
    }
    {
        beginCase();
        const identity = {
            remote_key: "r_12345678", connection_id: "c_old00000",
            principal_id: "principal-one", request_id: "retire-delivery",
            retire_id: "retire-consumed-rotation", ensure_id: "ensure-consumed-rotation",
            executor_fence: fence, origin_executor_fence: fence,
            window_id: logicalWindowId, page_id: logicalPageId,
        };
        const completion = {outcome: "already_retired"};
        const ack = {
            request_id: identity.request_id, retire_id: identity.retire_id,
            executor_fence: fence, response_delivery_id: "stable-retire-response",
            origin_executor_fence: fence, completion,
        };
        const localState = {abushRetireLedger: [{
            version: 8, phase: "consumed_pending", identity,
            delivery: identity, completion, ack,
            ack_connection_id: identity.connection_id,
            consumed_request_id: "stable-retire-consumed-id",
        }]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreRetireLedger();
        const currentFence = {executor_id: "executor-current", epoch: 2};
        await executor.remoteState({
            remote_key: identity.remote_key, connection_id: "c_current0",
            principal_id: identity.principal_id, executor_fence: currentFence,
            connected: true,
        });
        const consumed = calls.find(call => call[0] ===
            "retire_managed_page_ack_consumed")[1];
        assert.equal(consumed.request_id, "stable-retire-consumed-id");
        assert.equal(consumed.connection_id, "c_current0");
        assert.equal(consumed.delivery_request_id, ack.request_id);
        assert.deepEqual(consumed.executor_fence, currentFence);
        assert.deepEqual(localState.abushRetireLedger, [],
            "retire consumed ACK must converge after authority rotation with the same ID");
    }
    {
        beginCase();
        const legacy = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", request_id: "legacy-retire",
            ensure_id: "legacy-ensure", executor_fence: fence,
            window_id: logicalWindowId, page_id: logicalPageId,
        };
        const localState = {abushRetireLedger: [{
            status: "completed", identity: {original: legacy, delivery: legacy},
        }]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreRetireLedger();
        assert.equal(localState.abushRetireLedger[0].phase, "legacy_cleanup");
        assert.equal(calls.some(call => call[0] === "retire_managed_page_completion"), false,
            "legacy v6 retire state must never fabricate a v7 completion or ACK");
        executor.retireRecoveryStates.set(legacy.remote_key,
            executor.authoritySnapshot({
                remote_key: legacy.remote_key, connection_id: legacy.connection_id,
                principal_id: legacy.principal_id, executor_fence: fence, connected: true,
            }));
        await executor.recoverRetireLedger();
        assert.deepEqual(localState.abushRetireLedger, [],
            "legacy retire state clears only after local physical convergence");
    }
    {
        beginCase();
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
        assert.equal(localState.abushTopologyOutbox[0].request_id, report.request_id,
            "transport failure must not persist a temporary delivery request ID");
        assert.equal(localState.abushTopologyOutbox[0].report_id, report.request_id,
            "v6 migration must preserve the stable ID as report_id");
        assert.deepEqual(localState.abushTopologyOutbox[0].page, report.page);
        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restore();
        await second.executor.flushTopologyOutbox();
        assert.deepEqual(localState.abushTopologyOutbox, []);
        assert.notEqual(second.calls.filter(call =>
            call[0] === "managed_page_moved_outside")[0][1].request_id, "ack-loss",
            "native restart must allocate a fresh temporary delivery request ID");
    }
    {
        beginCase();
        const report = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", report_id: "sync-rejected",
            request_id: "sync-delivery", executor_fence: fence,
            origin_executor_fence: fence, daemon_instance_id: "daemon-instance",
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1},
            phase: "pending", response: null, cleanup: null,
            consumed_request_id: null, consumed_migration: null,
        };
        const localState = {abushTopologyOutbox: [report]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        executor.nativeRequest = async (_service, operation, payload) => {
            calls.push([operation, payload]);
            if (operation === "managed_page_moved_outside") {
                return {
                    request_id: payload.request_id,
                    report_id: payload.report_id,
                    executor_fence: payload.executor_fence,
                    response_delivery_id: "sync-rejected-response",
                    result: {outcome: "rejected", code: "stale_page"},
                };
            }
            if (operation === "topology_response_consumed") {
                return {
                    request_id: payload.request_id,
                    report_id: payload.report_id,
                    executor_fence: payload.executor_fence,
                    response_delivery_id: payload.response_delivery_id,
                };
            }
            return typedResponse(operation, payload) || {};
        };
        await executor.restoreTopologyOutbox();
        await executor.flushTopologyOutbox();
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "synchronous rejected topology responses must be terminal and consumed");
        assert.equal(calls.filter(call => call[0] === "topology_response_consumed").length, 1);
    }
    {
        beginCase();
        const report = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", report_id: "replay-rejected",
            request_id: "old-replay-delivery", executor_fence: fence,
            origin_executor_fence: fence, daemon_instance_id: "daemon-instance",
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1},
            phase: "pending", response: null, cleanup: null,
            consumed_request_id: null, consumed_migration: null,
        };
        const localState = {abushTopologyOutbox: [report]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreTopologyOutbox();
        executor.remoteAuthorities.set(report.remote_key, {
            remote_key: report.remote_key, connection_id: report.connection_id,
            principal_id: report.principal_id, executor_fence: fence, connected: true,
        });
        const replayed = {
            remote_key: report.remote_key, connection_id: report.connection_id,
            principal_id: report.principal_id, request_id: "replayed-rejected-delivery",
            report_id: report.report_id, executor_fence: fence,
            response_delivery_id: "replayed-rejected-response",
            result: {outcome: "rejected", code: "unknown_effect"},
        };
        assert.equal(await executor.topologyReportResponse(replayed), true);
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "replayed rejected topology responses must be consumed exactly once");
        const consumed = calls.find(call => call[0] === "topology_response_consumed")[1];
        assert.notEqual(consumed.request_id, replayed.request_id,
            "topology consumption must use a distinct durable request ID");
        assert.equal(consumed.delivery_request_id, replayed.request_id);
        const invalidReport = {...report, report_id: "unknown-rejected-report"};
        executor.topologyOutbox.set(invalidReport.report_id, invalidReport);
        const invalid = {...replayed,
            request_id: "unknown-rejected-delivery",
            report_id: invalidReport.report_id,
            result: {outcome: "rejected", code: "not_a_browser_error"}};
        assert.equal(await executor.topologyReportResponse(invalid), false,
            "unknown rejected error codes must not become canonical terminal responses");
    }
    {
        beginCase();
        const report = {
            remote_key: "r_12345678", connection_id: "c_old00000",
            principal_id: "principal-one", report_id: "consumed-rotation",
            request_id: "response-delivery", executor_fence: fence,
            origin_executor_fence: fence, daemon_instance_id: "daemon-instance",
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1},
            cleanup: null, phase: "consumed_pending",
            response: {
                request_id: "response-delivery", report_id: "consumed-rotation",
                executor_fence: fence, response_delivery_id: "stable-response",
                result: {outcome: "already_converged"},
            },
            consumed_request_id: "stable-consumed-id", consumed_migration: null,
        };
        const localState = {abushTopologyOutbox: [report]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreTopologyOutbox();
        const currentFence = {executor_id: "executor-current", epoch: 2};
        await executor.remoteState({
            remote_key: report.remote_key, connection_id: "c_current0",
            principal_id: report.principal_id, executor_fence: currentFence,
            owner_display: ensurePayload().owner_display, connected: true,
        });
        const consumed = calls.find(call => call[0] === "topology_response_consumed")[1];
        assert.equal(consumed.request_id, "stable-consumed-id",
            "authority rotation must preserve the stable consumed request ID");
        assert.equal(consumed.connection_id, "c_current0");
        assert.equal(consumed.delivery_request_id, report.response.request_id);
        assert.deepEqual(consumed.executor_fence, currentFence);
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "current-fence consumed ACK must remove the durable report");
    }
    {
        beginCase();
        const report = {
            remote_key: "r_12345678", connection_id: "c_old00000",
            principal_id: "principal-one", report_id: "consumed-canonical-replay",
            request_id: "old-response-delivery", executor_fence: fence,
            origin_executor_fence: fence, daemon_instance_id: "daemon-instance",
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1},
            cleanup: null, phase: "consumed_pending",
            response: {
                request_id: "old-response-delivery", report_id: "consumed-canonical-replay",
                executor_fence: fence, response_delivery_id: "stable-canonical-response",
                result: {outcome: "applied"},
            },
            consumed_request_id: "stable-consumed-replay-id", consumed_migration: null,
        };
        const localState = {abushTopologyOutbox: [report]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreTopologyOutbox();
        const currentFence = {executor_id: "executor-reconnected", epoch: 2};
        const authority = {
            remote_key: report.remote_key, connection_id: "c_reconnect0",
            principal_id: report.principal_id, executor_fence: currentFence,
            owner_display: ensurePayload().owner_display, connected: true,
        };
        await executor.authorizeTopologyOutbox(authority);
        executor.remoteAuthorities.set(report.remote_key, authority);
        const replayed = {
            remote_key: report.remote_key, connection_id: authority.connection_id,
            principal_id: report.principal_id, request_id: "new-response-delivery",
            report_id: report.report_id, executor_fence: currentFence,
            response_delivery_id: report.response.response_delivery_id,
            result: report.response.result,
        };
        assert.equal(await executor.topologyReportResponse(replayed), true);
        const consumed = calls.find(call => call[0] === "topology_response_consumed")[1];
        assert.equal(consumed.request_id, report.consumed_request_id,
            "canonical replay must preserve a sent consumed request ID");
        assert.equal(consumed.delivery_request_id, replayed.request_id,
            "consumption must correlate the current replay delivery request ID");
        assert.equal(consumed.connection_id, authority.connection_id);
        assert.deepEqual(consumed.executor_fence, currentFence);
        assert(calls.some(call => call[0] === "persistLocal" &&
            call[1].abushTopologyOutbox?.some(entry => entry.phase === "consumed_pending" &&
                entry.consumed_request_id === report.consumed_request_id &&
                entry.connection_id === authority.connection_id &&
                entry.response.request_id === replayed.request_id &&
                JSON.stringify(entry.response.executor_fence) === JSON.stringify(currentFence))),
        "canonical replay delivery correlation must be durable before consumption");
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "current canonical delivery must reach server confirmation and deletion");

        const conflictState = {abushTopologyOutbox: [report]};
        const conflict = fixture([], [], 0, false, null, conflictState);
        await conflict.executor.restoreTopologyOutbox();
        await conflict.executor.authorizeTopologyOutbox(authority);
        conflict.executor.remoteAuthorities.set(report.remote_key, authority);
        await assert.rejects(conflict.executor.topologyReportResponse({
            ...replayed, response_delivery_id: "conflicting-response-delivery",
        }), /conflicting canonical topology response/);
        await assert.rejects(conflict.executor.topologyReportResponse({
            ...replayed, result: {outcome: "already_converged"},
        }), /conflicting canonical topology response/);
        assert.equal(conflict.calls.some(call => call[0] === "topology_response_consumed"), false);
    }
    {
        beginCase();
        const legacyResponse = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", report_id: "legacy-response",
            request_id: "legacy-delivery", executor_fence: fence,
            origin_executor_fence: fence, daemon_instance_id: "daemon-instance",
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1}, cleanup: null,
            phase: "response_received", response: {
                request_id: "legacy-delivery", report_id: "legacy-response",
                executor_fence: fence, response_delivery_id: "legacy-response-delivery",
                result: {outcome: "already_converged"},
            },
        };
        const localState = {abushTopologyOutbox: [legacyResponse]};
        const {executor} = fixture([], [], 0, false, null, localState);
        await executor.restoreTopologyOutbox();
        const restored = executor.topologyOutbox.get("legacy-response");
        assert.equal(restored.phase, "response_received",
            "legacy unsent consumption must not be declared daemon-accepted");
        assert.equal(restored.consumed_request_id, null);
        assert.equal(restored.consumed_migration, "legacy_unsent");
    }
    {
        beginCase();
        const report = {
            remote_key: "r_12345678", connection_id: "c_old00000",
            principal_id: "principal-one", report_id: "response-before-state",
            request_id: "old-delivery", executor_fence: fence,
            origin_executor_fence: fence, daemon_instance_id: "daemon-instance",
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1},
            phase: "pending", response: null, cleanup: null,
            consumed_request_id: null, consumed_migration: null,
        };
        const localState = {abushTopologyOutbox: [report]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreTopologyOutbox();
        const currentFence = {executor_id: "executor-current", epoch: 2};
        const replayed = {
            remote_key: report.remote_key, connection_id: "c_current0",
            principal_id: report.principal_id, request_id: "current-replay-delivery",
            report_id: report.report_id, executor_fence: currentFence,
            response_delivery_id: "response-before-state-delivery",
            result: {outcome: "already_converged"},
        };
        assert.equal(await executor.topologyReportResponse(replayed), true);
        assert.equal(executor.deferredTopologyResponses.size, 1,
            "valid replay before remote state must be deferred, not discarded");
        assert.equal(localState.abushDeferredTopologyResponses.length, 1,
            "response-before-state replay must survive a worker restart");
        assert.equal(calls.some(call => call[0] === "topology_response_consumed"), false);
        const second = fixture([], [], 0, false, null, localState);
        await second.executor.restoreTopologyOutbox();
        await second.executor.restoreDeferredTopologyResponses();
        await second.executor.remoteState({
            remote_key: report.remote_key, connection_id: replayed.connection_id,
            principal_id: report.principal_id, executor_fence: currentFence,
            owner_display: ensurePayload().owner_display, connected: true,
        });
        assert.equal(second.executor.deferredTopologyResponses.size, 0);
        assert.deepEqual(localState.abushDeferredTopologyResponses, []);
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "remote state must reprocess and consume the deferred replay response");
        const consumed = second.calls.find(call => call[0] === "topology_response_consumed")[1];
        assert.equal(consumed.report_id, report.report_id);
        assert.deepEqual(consumed.executor_fence, currentFence);
    }
    {
        beginCase();
        const report = {
            remote_key: "r_12345678", connection_id: "c_12345678",
            principal_id: "principal-one", report_id: "stable-replay",
            request_id: "old-delivery", executor_fence: fence,
            origin_executor_fence: fence, daemon_instance_id: "daemon-instance",
            page: {window: {workspace_id: "workspace-authoritative",
                window_id: logicalWindowId, generation: 1},
            page_id: logicalPageId, generation: 1},
            phase: "pending", response: null, cleanup: null,
            consumed_request_id: null, consumed_migration: null,
        };
        const localState = {abushTopologyOutbox: [report]};
        const {executor, calls} = fixture([], [], 0, false, null, localState);
        await executor.restoreTopologyOutbox();
        const replayFence = {executor_id: "executor-restarted", epoch: 2};
        await executor.authorizeTopologyOutbox({
            remote_key: report.remote_key, connection_id: "c_new00000",
            principal_id: report.principal_id, executor_fence: replayFence,
            owner_display: ensurePayload().owner_display, connected: true,
        });
        executor.remoteAuthorities.set(report.remote_key, {
            remote_key: report.remote_key, connection_id: "c_new00000",
            principal_id: report.principal_id, executor_fence: replayFence,
            owner_display: ensurePayload().owner_display, connected: true,
        });
        const current = executor.topologyOutbox.get(report.report_id);
        const replayed = {
            remote_key: current.remote_key, connection_id: current.connection_id,
            principal_id: current.principal_id, request_id: "daemon-replay-delivery",
            report_id: current.report_id, executor_fence: replayFence,
            response_delivery_id: "stable-response",
            result: {outcome: "already_converged"},
        };
        assert.equal(await executor.topologyReportResponse(replayed), true);
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "daemon replay must consume the canonical result after native restart");
        const consumed = calls.find(call => call[0] === "topology_response_consumed")[1];
        assert.notEqual(consumed.request_id, "daemon-replay-delivery",
            "daemon replay consumption must use its durable request ID");
        assert.equal(consumed.delivery_request_id, "daemon-replay-delivery");
        assert.deepEqual(consumed.executor_fence, replayFence);
    }
    {
        beginCase();
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
            "migrated v6 entries retain distinct stable report IDs");
        assert.deepEqual(localState.abushTopologyOutbox, [],
            "startup must drain a successfully reported durable topology outbox");
    }
    {
        beginCase();
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
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        first.executor.nativeRequest = async (_service, operation, payload) => {
            first.calls.push([operation, payload]);
            if (operation === "managed_page_moved_outside") throw new Error("offline");
            if (operation === "clear_x11_window") {
                return {cleared: true, xid: payload.xid, reclassified_group: "f"};
            }
            return typedResponse(operation, payload) || {};
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
        assert.notEqual(resent.request_id, originalRequestId,
            "retry must use a temporary delivery request ID");
        assert.equal(resent.report_id, originalReportId);
        assert.equal(resent.connection_id, "c_new00000");
        assert.deepEqual(resent.executor_fence, rotatedFence);
    }
    {
        beginCase();
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
                const tab = await executor.chrome.tabs.get(tabId);
                emitted.push(executor.observeChromeEvent("tabUpdated", {
                    tabId, changeInfo: {status: "loading", title: tab.title}, tab,
                }));
                emitted.push(executor.observeChromeEvent("tabUpdated", {
                    tabId, changeInfo: {status: "complete"}, tab,
                }));
            }
            return updated;
        };
        const record = await executor.ensure(ensurePayload({
            request_id: "real-events", ensure_id: "real-events",
        }));
        assert(record && emitted.length >= 12 && emitted.every(Boolean),
            "real listener create/attach/detach/pin/loading/title/complete events must stay owned");
    }
    {
        beginCase();
        const {executor} = fixture([], [], 1);
        const transaction = await executor.beginEnsureTransaction(
            ensurePayload({request_id: "external-conflict", ensure_id: "external-conflict"}),
            "e".repeat(32));
        transaction.windowId = 10;
        transaction.statusTabIds = [110, 111];
        transaction.plannedEffect = {kind: "tabs.pin", tab_id: 110, window_id: 10};
        assert.equal(executor.observeChromeEvent("tabUpdated", {
            tabId: 110, changeInfo: {status: "loading", title: "loading"},
            tab: {id: 110, windowId: 10, index: 0},
        }), true);
        assert.equal(transaction.invalid, false);
        assert.equal(executor.observeChromeEvent("tabUpdated", {
            tabId: 110, changeInfo: {pinned: false},
            tab: {id: 110, windowId: 10, index: 0, pinned: false},
        }), false);
        assert.equal(transaction.invalid, true,
            "external URL/pin/index conflicts must invalidate the active transaction");
    }
    {
        beginCase();
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
        beginCase();
        const localState = {};
        const {executor, calls} = fixture([], [], 1, false, null, localState);
        await executor.restore();
        calls.length = 0;
        const writes = [];
        let blockedResolve;
        let blockFirst = true;
        const originalSet = executor.chrome.storage.local.set;
        executor.chrome.storage.local.set = async value => {
            if (blockFirst) {
                blockFirst = false;
                await new Promise(resolve => { blockedResolve = resolve; });
            }
            writes.push(structuredClone(value));
            await originalSet(value);
        };
        executor.topologyOutbox.set("first", {report_id: "first"});
        const firstWrite = executor.persistTopologyOutbox();
        await waitFor(() => blockedResolve !== undefined, "first local-state write start");
        executor.topologyOutbox.set("second", {report_id: "second"});
        const secondWrite = executor.persistTopologyOutbox();
        blockedResolve();
        await Promise.all([firstWrite, secondWrite]);
        assert.deepEqual(writes.at(-1).abushTopologyOutbox.map(entry => entry.report_id),
            ["first", "second"], "local writes must snapshot at serialized execution time");
    }
    {
        beginCase();
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
        await waitFor(() => localState.abushPendingTransactions?.some(transaction =>
            transaction.status_tab_ids.length === 2), "two-marker transaction persistence");
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
        beginCase();
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
        beginCase();
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
        beginCase();
        const localState = {};
        const first = fixture([], [], 0, false, null, localState);
        const record = await first.executor.ensure(ensurePayload());
        const oldPageTabId = record.pageTabId;
        const tabs = await first.executor.chrome.tabs.query({windowId: record.windowId});
        const oldPage = tabs.find(tab => tab.id === oldPageTabId);
        tabs.splice(tabs.indexOf(oldPage), 1);
        const replacement = {...oldPage, id: 9877, index: 2, pinned: false};
        tabs.push(replacement);
        const writes = [];
        const originalLocalSet = first.executor.chrome.storage.local.set;
        first.executor.chrome.storage.local.set = async value => {
            await originalLocalSet(value);
            if (value.abushReplacementLedger?.[0]) {
                writes.push(value.abushReplacementLedger[0].phase);
            }
        };
        await first.executor.tabReplaced(replacement.id, oldPageTabId);
        assert.deepEqual(writes, ["planned", "session_candidate", "marker_candidate", "installed"]);
        assert.deepEqual(localState.abushReplacementLedger, [],
            "replacement WAL clears only after session, marker and in-memory install converge");
    }
    {
        beginCase();
        const {executor, calls} = fixture();
        await executor.status();
        assert.deepEqual(calls.map(call => call[0]), ["refresh_remotes"]);
        assert(!calls.some(call => call[0] === "createWindow"), "status must not create windows");
    }
    for (const checkpoint of ["validate", "attach", "effect", "mutation"]) {
        beginCase();
        const {executor, calls} = fixture();
        const payload = ensurePayload();
        const record = await executor.ensure(payload);
        await executor.remoteState({...payload, connected: true});
        const entered = deferred(`${checkpoint} entered`);
        const release = deferred(`${checkpoint} release`);
        const outboxEntered = deferred("disconnect outbox entered");
        const outboxRelease = deferred("disconnect outbox release");
        const pause = async () => { entered.resolve(); await release.promise; };
        if (checkpoint === "validate") {
            const validate = executor.validate.bind(executor);
            executor.validate = async (...args) => {
                const result = await validate(...args);
                await pause();
                return result;
            };
        } else if (checkpoint === "attach") {
            const attach = executor.chrome.debugger.attach;
            executor.chrome.debugger.attach = async (...args) => {
                await attach(...args);
                await pause();
            };
        } else if (checkpoint === "effect") {
            const mark = executor.markOperationEffectStarted.bind(executor);
            executor.markOperationEffectStarted = async operation => {
                await pause();
                return mark(operation);
            };
        } else {
            const send = executor.chrome.debugger.sendCommand;
            executor.chrome.debugger.sendCommand = async (...args) => {
                await send(...args);
                await pause();
            };
        }
        executor.authorizeTopologyOutbox = async () => {
            outboxEntered.resolve();
            await outboxRelease.promise;
        };
        calls.length = 0;
        const executing = executor.execute(executePayload({
            effect_id: `revoked-${checkpoint}`,
            command: {command: "navigate", page: executor.pageRef(record),
                url: "https://revoked.test/"},
        }));
        await entered.promise;
        const disconnecting = executor.remoteState({...payload, connected: false});
        await outboxEntered.promise;
        assert.throws(() => executor.requireCurrentAuthority(payload),
            error => error.browserCode === "stale_authority");
        release.resolve();
        await exactWait(executing, `${checkpoint} execution terminal before cancellation`);
        const completion = calls.find(call => call[0] === "executor_completion");
        assert(completion, `${checkpoint} must surface a terminal completion`);
        assert.equal(completion[1].completion,
            ["effect", "mutation"].includes(checkpoint) ? "unknown" : "not_applied");
        assert.equal(calls.some(call => call[0] === "command" && call[2] === "Page.navigate"),
            checkpoint === "mutation", `${checkpoint} must not start stale navigation`);
        if (checkpoint === "validate") {
            assert.equal(calls.some(call => call[0] === "attach"), false,
                "revoked validation must not attach the debugger");
        }
        outboxRelease.resolve();
        await disconnecting;
    }
    for (const checkpoint of ["admission", "effect", "bind", "rebind"]) {
        beginCase();
        const {executor, calls} = fixture();
        const payload = ensurePayload();
        if (checkpoint === "rebind") await executor.ensure(payload);
        await executor.remoteState({...payload, connected: true});
        const entered = deferred(`${checkpoint} ensure entered`);
        const release = deferred(`${checkpoint} ensure release`);
        const outboxEntered = deferred("ensure disconnect outbox entered");
        const outboxRelease = deferred("ensure disconnect outbox release");
        const pause = async () => { entered.resolve(); await release.promise; };
        if (checkpoint === "admission") {
            const get = executor.chrome.storage.local.get;
            executor.chrome.storage.local.get = async keys => {
                const result = await get(keys);
                if (keys.includes("abushEnsureCompletions")) await pause();
                return result;
            };
        } else if (checkpoint === "effect") {
            const persist = executor.persistPendingTransactions.bind(executor);
            let paused = false;
            executor.persistPendingTransactions = async () => {
                await persist();
                if (!paused && [...executor.pendingTransactions.values()].some(transaction =>
                    transaction.plannedEffect?.kind === "tabs.create.marker")) {
                    paused = true;
                    await pause();
                }
            };
        } else {
            executor.nativeMutationTail = release.promise;
            const bind = executor.bind.bind(executor);
            executor.bind = (...args) => {
                entered.resolve();
                return bind(...args);
            };
        }
        executor.authorizeTopologyOutbox = async () => {
            outboxEntered.resolve();
            await outboxRelease.promise;
        };
        calls.length = 0;
        const ensuring = executor.ensure(ensurePayload({request_id: `revoked-${checkpoint}`,
            ensure_id: `revoked-${checkpoint}`}));
        const rejected = assert.rejects(ensuring,
            error => error.browserCode === "stale_authority");
        await entered.promise;
        const disconnecting = executor.remoteState({...payload, connected: false});
        await outboxEntered.promise;
        const beforeRelease = calls.length;
        release.resolve();
        await rejected;
        assert.equal(calls.slice(beforeRelease).some(call =>
            ["createTab", "createWindow", "move", "update", "bind_x11_window"]
                .includes(call[0])), false,
        `${checkpoint} must not resume stale browser/native mutations or rollback bind`);
        if (["admission", "effect"].includes(checkpoint)) {
            assert.equal(calls.some(call => call[0] === "createTab"), false);
        }
        outboxRelease.resolve();
        await disconnecting;
    }
    {
        beginCase();
        const {executor, calls} = fixture();
        const old = ensurePayload();
        await executor.remoteState({...old, connected: true});
        const entered = deferred("invalidation cancellation entered");
        const release = deferred("invalidation cancellation release");
        const cancel = executor.cancelMatchingSerialized.bind(executor);
        executor.cancelMatchingSerialized = async matches => {
            entered.resolve();
            await release.promise;
            return cancel(matches);
        };
        const invalidating = executor.invalidateRemote(old);
        await entered.promise;
        assert.throws(() => executor.requireCurrentAuthority(old),
            error => error.browserCode === "stale_authority",
            "matching invalidation must revoke before awaiting cancellation");
        await assert.rejects(executor.ensure(old),
            error => error.browserCode === "stale_authority");
        release.resolve();
        await invalidating;
        executor.cancelMatchingSerialized = cancel;
        const current = ensurePayload({connection_id: "c_reconnected", executor_fence: {
            executor_id: fence.executor_id, epoch: fence.epoch + 1,
        }});
        await executor.remoteState({...current, connected: true});
        const candidate = await executor.ensure(current);
        calls.length = 0;
        await executor.invalidateRemote(old);
        await executor.remoteState({...old, connected: false});
        assert(executor.authorityMatches(current),
            "delayed old invalidation/disconnect must not revoke the newer fence");
        assert.strictEqual(executor.workspaces.get(executor.key(
            current.remote_key, current.principal_id)), candidate);
        for (const stale of [old, {...current, principal_id: "visible-not-owned"},
            {...current, connection_id: old.connection_id}]) {
            await assert.rejects(executor.ensure(stale),
                error => error.browserCode === "stale_authority");
        }
        assert.equal(calls.some(call => ["removeWindow", "bind_x11_window", "update"]
            .includes(call[0])), false, "old background events must not clean up a newer candidate");
        await executor.execute(executePayload({connection_id: current.connection_id,
            executor_fence: current.executor_fence, effect_id: "reconnected-navigation",
            command: {command: "navigate", page: executor.pageRef(candidate),
                url: "https://current.test/"}}));
        assert.equal(calls.find(call => call[0] === "executor_completion")[1].completion,
            "applied", "newer committed authority must supersede revocation");
    }
    console.log("workspace executor tests passed");
    } finally {
        clearTimeout(testDeadline);
    }
})().catch(error => { console.error(error); process.exitCode = 1; });
