const STATUS_PREFIX = "__ABUSH_BIND__:";
const STATUS_NONCE_RE = /^[0-9a-f]{32}$/;
const BROWSER_ERROR_CODES = new Set([
    "unknown_principal", "unknown_connection", "stale_session", "stale_authority",
    "human_controlled", "missing_capability", "capability_grant_mismatch", "backpressure",
    "stale_window", "stale_page", "page_quarantined", "operation_conflict",
    "unknown_effect", "stale_effect", "generation_exhausted", "invalid_command",
]);

function sameRef(left, right) {
    return JSON.stringify(left) === JSON.stringify(right);
}

function exactFields(value, fields) {
    return value && typeof value === "object" && !Array.isArray(value) &&
        Object.keys(value).length === fields.length && fields.every(field => field in value);
}

function validFence(value) {
    return exactFields(value, ["executor_id", "epoch"]) &&
        typeof value.executor_id === "string" && value.executor_id &&
        Number.isInteger(value.epoch) && value.epoch > 0;
}

function displayTitle(connectionDisplayName) {
    return connectionDisplayName;
}

function validPageRef(value) {
    return exactFields(value, ["window", "page_id", "generation"]) &&
        exactFields(value.window, ["workspace_id", "window_id", "generation"]) &&
        [value.window.workspace_id, value.window.window_id, value.page_id]
            .every(field => typeof field === "string" && field) &&
        Number.isInteger(value.window.generation) && value.window.generation > 0 &&
        Number.isInteger(value.generation) && value.generation > 0;
}

class WorkspaceExecutor {
    constructor(chromeApi, nativeRequest) {
        this.chrome = chromeApi;
        this.nativeRequest = nativeRequest;
        this.workspaces = new Map();
        this.windowOwners = new Map();
        this.tabOwners = new Map();
        this.operations = new Map();
        this.completedOperations = new Map();
        this.ensureFlights = new Map();
        this.ensureWaiters = new Map();
        this.persistTail = Promise.resolve();
        this.persistRevision = 0;
        this.ready = Promise.resolve();
    }

    restoreWhenReady() {
        this.ready = this.restore();
        return this.ready;
    }

    whenReady() {
        return this.ready;
    }

    key(remoteKey, principalId) {
        return `${remoteKey}\0${principalId}`;
    }

    persistedRecord(record) {
        const {debuggerAttached, ...persisted} = record;
        return {...persisted};
    }

    async persistRecords(records) {
        if (!this.chrome.storage?.session) return;
        const revision = ++this.persistRevision;
        await this.chrome.storage.session.set({abushWorkspaces: records, revision});
    }

    async persist() {
        const write = this.persistTail.catch(() => {}).then(() => {
            const records = [...this.workspaces.values()]
                .map(record => this.persistedRecord(record));
            return this.persistRecords(records);
        });
        this.persistTail = write;
        await write;
    }

    async commitRecord(record, previous = null) {
        const write = this.persistTail.catch(() => {}).then(async () => {
            const records = [...this.workspaces.values()].map(saved =>
                this.persistedRecord(saved === previous ? record : saved));
            if (previous === null) records.push(this.persistedRecord(record));
            await this.persistRecords(records);
            if (previous === null) this.install(record);
            else Object.assign(previous, record);
        });
        this.persistTail = write;
        await write;
    }

    statusUrl(record) {
        return this.chrome.runtime.getURL(`status.html#${record.nonce}`);
    }

    statusNonce(url) {
        const prefix = this.chrome.runtime.getURL("status.html#");
        if (typeof url !== "string" || !url.startsWith(prefix)) return null;
        const nonce = url.slice(prefix.length);
        return STATUS_NONCE_RE.test(nonce) ? nonce : null;
    }

    async waitForStatusTitle(tabId, nonce) {
        const expected = `${STATUS_PREFIX}${nonce}`;
        for (let attempt = 0; attempt < 100; attempt++) {
            const tab = await this.chrome.tabs.get(tabId);
            if (tab.title === expected) return;
            await new Promise(resolve => setTimeout(resolve, 10));
        }
        throw new Error("status tab title was not ready");
    }

    async closeOrphanedStatusWindows(liveWindowIds) {
        const tabs = await this.chrome.tabs.query({});
        for (const tab of tabs) {
            if (this.statusNonce(tab.url) === null || liveWindowIds.has(tab.windowId)) continue;
            try { await this.chrome.tabs.remove(tab.id); } catch (_) {}
        }
    }

    async isLive(record) {
        try {
            const tabs = await this.chrome.tabs.query({windowId: record.windowId});
            return tabs.length === 2 && tabs.filter(tab => tab.id === record.statusTabId &&
                tab.url === this.statusUrl(record)).length === 1 &&
                tabs.filter(tab => tab.id === record.pageTabId).length === 1 &&
                record.statusTabId !== record.pageTabId;
        } catch (_) {
            return false;
        }
    }

    async restore() {
        if (!this.chrome.storage?.session) return;
        const {abushWorkspaces = [], revision = 0} =
            await this.chrome.storage.session.get(["abushWorkspaces", "revision"]);
        if (Number.isInteger(revision) && revision >= 0) this.persistRevision = revision;
        let removed = false;
        const liveWindowIds = new Set();
        for (const saved of abushWorkspaces) {
            if (!saved || typeof saved.remoteKey !== "string" ||
                    typeof saved.principalId !== "string" || !saved.principalId ||
                    typeof saved.workspaceId !== "string" || !saved.workspaceId ||
                    typeof saved.logicalWindowId !== "string" || !saved.logicalWindowId ||
                    typeof saved.logicalPageId !== "string" || !saved.logicalPageId ||
                    !Number.isInteger(saved.windowGeneration ?? saved.generation) ||
                    (saved.windowGeneration ?? saved.generation) < 1 ||
                    !Number.isInteger(saved.pageGeneration ?? saved.generation) ||
                    (saved.pageGeneration ?? saved.generation) < 1 ||
                    typeof saved.windowId !== "number" || typeof saved.statusTabId !== "number" ||
                    typeof saved.pageTabId !== "number" || typeof saved.nonce !== "string") {
                removed = true;
                continue;
            }
            if (!await this.isLive(saved)) {
                removed = true;
                continue;
            }
            saved.debuggerAttached = false;
            this.install(saved);
            liveWindowIds.add(saved.windowId);
        }
        await this.closeOrphanedStatusWindows(liveWindowIds);
        if (removed) await this.persist();
    }

    install(record) {
        const key = this.key(record.remoteKey, record.principalId);
        this.workspaces.set(key, record);
        this.windowOwners.set(record.windowId, key);
        this.tabOwners.set(record.pageTabId, key);
    }

    bindingPayload(record) {
        return {
            nonce: record.nonce,
            remote_key: record.remoteKey,
            workspace_id: record.workspaceId,
            generation: 1,
            control_state: "remote",
            display_title: displayTitle(record.connectionDisplayName),
        };
    }

    async bind(record, workspaceId = record.workspaceId) {
        await this.chrome.tabs.update(record.statusTabId, {active: true});
        try {
            await this.nativeRequest("abush_browser", "bind_x11_window",
                this.bindingPayload({...record, workspaceId}));
        } finally {
            await this.chrome.tabs.update(record.pageTabId, {active: true});
        }
    }

    fail(code, message) {
        const error = new Error(message);
        error.browserCode = code;
        throw error;
    }

    validateEnsurePayload(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "workspace_id",
            "request_id", "ensure_id", "executor_fence", "owner_display",
            "connection_display_name", "expected_page"]) ||
                ![payload.remote_key, payload.connection_id, payload.principal_id,
                    payload.workspace_id, payload.request_id, payload.ensure_id].every(value =>
                    typeof value === "string" && value) ||
                (payload.expected_page !== null && !validPageRef(payload.expected_page)) ||
                (payload.expected_page !== null &&
                    payload.expected_page.window.workspace_id !== payload.workspace_id) ||
                !validFence(payload.executor_fence) ||
                !exactFields(payload.owner_display,
                    ["daemon_instance_id", "daemon_boot_id", "workbench_name"]) ||
                !Object.values(payload.owner_display).every(value =>
                    typeof value === "string" && value && value.length <= 256) ||
                typeof payload.connection_display_name !== "string" ||
                !payload.connection_display_name || payload.connection_display_name.length > 256) {
            throw new Error("invalid ensure payload");
        }
    }

    ensureIdentity(payload) {
        return {
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            request_id: payload.request_id,
            ensure_id: payload.ensure_id,
            executor_fence: payload.executor_fence,
        };
    }

    ensureRequestKey(payload) {
        return JSON.stringify(this.ensureIdentity(payload));
    }

    activeEnsureWaiters(flight) {
        return [...flight.waiters].filter(waiter => !waiter.cancelled);
    }

    async ensure(payload) {
        this.validateEnsurePayload(payload);
        const requestKey = this.ensureRequestKey(payload);
        if (this.ensureWaiters.has(requestKey)) throw new Error("duplicate ensure request");
        const key = this.key(payload.remote_key, payload.principal_id);
        let flight = this.ensureFlights.get(key);
        if (flight && !sameRef(flight.expectedPage, payload.expected_page)) {
            throw new Error("conflicting expected page");
        }
        if (!flight) {
            flight = {waiters: new Set(), cancelled: false, createdWindowId: null,
                committed: false, cleanupStarted: false, expectedPage: payload.expected_page};
            flight.promise = this.ensureWorkspace(payload, flight);
            this.ensureFlights.set(key, flight);
            flight.promise.finally(() => {
                if (this.ensureFlights.get(key) === flight) this.ensureFlights.delete(key);
            }).catch(() => {});
        }
        const waiter = {payload, flight, cancelled: false};
        flight.waiters.add(waiter);
        this.ensureWaiters.set(requestKey, waiter);
        try {
            const record = await flight.promise;
            if (waiter.cancelled) return undefined;
            await this.completeEnsure(payload, {
                outcome: "created",
                window_id: record.logicalWindowId,
                page_id: record.logicalPageId,
            });
            return record;
        } catch (error) {
            if (waiter.cancelled) return undefined;
            try {
                await this.completeEnsure(payload, {
                    outcome: "rejected",
                    code: BROWSER_ERROR_CODES.has(error.browserCode) ?
                        error.browserCode : "invalid_command",
                });
            } catch (completionError) {
                throw new AggregateError([error, completionError],
                    "workspace ensure and rejection completion failed");
            }
            throw error;
        } finally {
            flight.waiters.delete(waiter);
            this.ensureWaiters.delete(requestKey);
        }
    }

    async ensureCancellationCompleted(payload) {
        await this.nativeRequest("abush_browser", "ensure_managed_page_cancellation_completion",
            this.ensureIdentity(payload));
    }

    async retireManagedPage(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "ensure_id", "executor_fence", "window_id", "page_id"])) return false;
        const record = this.workspaces.get(this.key(payload.remote_key, payload.principal_id));
        if (record && record.connectionId === payload.connection_id &&
                sameRef(record.executorFence, payload.executor_fence) &&
                record.logicalWindowId === payload.window_id && record.logicalPageId === payload.page_id) {
            await this.detach(record.pageTabId);
            try { await this.chrome.windows.remove(record.windowId); } catch (_) {}
            this.forget(record);
            await this.persist();
        }
        await this.ensureCancellationCompleted(payload);
        return true;
    }

    async cancelEnsure(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "ensure_id", "executor_fence"]) ||
                ![payload.remote_key, payload.connection_id, payload.principal_id,
                    payload.request_id, payload.ensure_id].every(value =>
                    typeof value === "string" && value) ||
                !validFence(payload.executor_fence)) return false;
        const waiter = this.ensureWaiters.get(this.ensureRequestKey(payload));
        if (!waiter || waiter.cancelled) return false;
        waiter.cancelled = true;
        const flight = waiter.flight;
        if (this.activeEnsureWaiters(flight).length === 0) {
            flight.cancelled = true;
            if (flight.createdWindowId !== null && !flight.committed) {
                await this.cleanupFlightWindow(flight);
            }
            try { await flight.promise; } catch (_) {}
        }
        await this.ensureCancellationCompleted(payload);
        return true;
    }

    async cleanupFlightWindow(flight) {
        if (flight.cleanupStarted || flight.createdWindowId === null) return;
        flight.cleanupStarted = true;
        try { await this.chrome.windows.remove(flight.createdWindowId); } catch (_) {}
    }

    ensureActive(flight) {
        if (flight.cancelled && this.activeEnsureWaiters(flight).length === 0) {
            throw new Error("ensure cancelled");
        }
    }

    async ensureWorkspace(payload, flight) {
        const key = this.key(payload.remote_key, payload.principal_id);
        this.ensureActive(flight);
        let existing = this.workspaces.get(key);
        if (existing && !await this.isLive(existing)) {
            this.forget(existing);
            await this.persist();
            existing = null;
        }
        if (existing) {
            if (payload.expected_page !== null &&
                    !sameRef(payload.expected_page, this.pageRef(existing))) {
                this.fail("stale_page", "restored workspace does not match expected page");
            }
            this.ensureActive(flight);
            const candidate = {
                ...existing,
                workspaceId: payload.workspace_id,
                connectionId: payload.connection_id,
                executorFence: payload.executor_fence,
                ownerDisplay: payload.owner_display,
                connectionDisplayName: payload.connection_display_name,
            };
            await this.bind(candidate, payload.workspace_id);
            this.ensureActive(flight);
            if (!await this.isLive(existing)) throw new Error("workspace changed during bind");
            this.ensureActive(flight);
            await this.commitRecord(candidate, existing);
            return existing;
        }
        const nonce = crypto.randomUUID().replaceAll("-", "");
        const statusUrl = this.chrome.runtime.getURL(`status.html#${nonce}`);
        const statusTab = await this.chrome.tabs.create({url: statusUrl, active: false});
        let created;
        try {
            await this.waitForStatusTitle(statusTab.id, nonce);
            created = await this.chrome.windows.create({
                tabId: statusTab.id, focused: false, type: "normal",
            });
        } catch (error) {
            try { await this.chrome.tabs.remove(statusTab.id); } catch (_) {}
            throw error;
        }
        flight.createdWindowId = created.id;
        try {
            this.ensureActive(flight);
        } catch (error) {
            await this.cleanupFlightWindow(flight);
            throw error;
        }
        await this.chrome.tabs.update(statusTab.id, {pinned: true});
        const business = await this.chrome.tabs.create({
            windowId: created.id, url: "about:blank", active: true,
        });
        const expected = payload.expected_page;
        let logicalWindowId = crypto.randomUUID();
        let logicalPageId = crypto.randomUUID();
        let windowGeneration = 1;
        let pageGeneration = 1;
        if (expected) {
            logicalWindowId = expected.window.window_id;
            logicalPageId = expected.page_id;
            windowGeneration = expected.window.generation;
            pageGeneration = expected.generation;
        }
        const record = {
            remoteKey: payload.remote_key,
            principalId: payload.principal_id,
            workspaceId: payload.workspace_id,
            connectionId: payload.connection_id,
            executorFence: payload.executor_fence,
            ownerDisplay: payload.owner_display,
            connectionDisplayName: payload.connection_display_name,
            logicalWindowId,
            logicalPageId,
            windowGeneration,
            pageGeneration,
            windowId: created.id,
            statusTabId: statusTab.id,
            pageTabId: business.id,
            nonce,
            debuggerAttached: false,
        };
        try {
            this.ensureActive(flight);
            await this.bind(record);
            this.ensureActive(flight);
            if (!await this.isLive(record)) throw new Error("workspace changed during bind");
            this.ensureActive(flight);
            await this.commitRecord(record);
            flight.committed = true;
            return record;
        } catch (error) {
            this.forget(record);
            try { await this.persist(); } catch (_) {}
            await this.cleanupFlightWindow(flight);
            throw error;
        }
    }

    async completeEnsure(payload, completion) {
        await this.nativeRequest("abush_browser", "ensure_managed_page_completion", {
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            workspace_id: payload.workspace_id,
            request_id: payload.request_id,
            ensure_id: payload.ensure_id,
            executor_fence: payload.executor_fence,
            completion,
        });
    }

    windowRef(record) {
        return {
            workspace_id: record.workspaceId,
            window_id: record.logicalWindowId,
            generation: record.windowGeneration ?? record.generation,
        };
    }

    pageRef(record) {
        return {
            window: this.windowRef(record),
            page_id: record.logicalPageId,
            generation: record.pageGeneration ?? record.generation,
        };
    }

    async validate(payload, command) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "effect_id", "executor_fence", "command"]) ||
                ![payload.remote_key, payload.connection_id, payload.principal_id,
                    payload.request_id, payload.effect_id].every(value =>
                    typeof value === "string" && value) ||
                !validFence(payload.executor_fence)) {
            throw new Error("invalid execute payload");
        }
        const record = this.workspaces.get(this.key(payload.remote_key, payload.principal_id));
        if (!record || record.connectionId !== payload.connection_id ||
                !sameRef(payload.executor_fence, record.executorFence)) {
            throw new Error("stale executor");
        }
        if (command.command === "close_window") {
            if (!sameRef(command.window, this.windowRef(record))) throw new Error("stale window");
            return {record};
        }
        if (!sameRef(command.page, this.pageRef(record))) throw new Error("stale page");
        const tab = await this.chrome.tabs.get(record.pageTabId);
        if (tab.windowId !== record.windowId) throw new Error("page moved outside");
        return {record, tab};
    }

    async completion(payload, completion) {
        await this.nativeRequest("abush_browser", "executor_completion", {
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            request_id: payload.request_id,
            effect_id: payload.effect_id,
            executor_fence: payload.executor_fence,
            completion,
        });
    }

    operationIdentity(payload) {
        return {
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            request_id: payload.request_id,
            effect_id: payload.effect_id,
            executor_fence: payload.executor_fence,
        };
    }

    operationKey(payload) {
        return JSON.stringify(this.operationIdentity(payload));
    }

    rememberCompletedOperation(operationKey, operation) {
        this.completedOperations.delete(operationKey);
        this.completedOperations.set(operationKey, operation);
        if (this.completedOperations.size > 256) {
            this.completedOperations.delete(this.completedOperations.keys().next().value);
        }
    }

    async execute(payload) {
        let resolveDone;
        const done = new Promise(resolve => { resolveDone = resolve; });
        const operation = {
            ...this.operationIdentity(payload),
            cancelled: false,
            mutationStarted: false,
            committed: false,
            workspaceKey: null,
            tabId: null,
            done,
            resolveDone,
        };
        const operationKey = this.operationKey(payload);
        const existing = this.operations.get(operationKey);
        if (existing || this.completedOperations.has(operationKey)) return;
        this.operations.set(operationKey, operation);
        let completion = "unknown";
        try {
            const {record} = await this.validate(payload, payload.command);
            operation.workspaceKey = this.key(record.remoteKey, record.principalId);
            operation.tabId = record.pageTabId;
            if (operation.cancelled) completion = "not_applied";
            else if (payload.command.command === "navigate") {
                const url = new URL(payload.command.url);
                if (!["http:", "https:"].includes(url.protocol)) throw new Error("invalid URL");
                if (!record.debuggerAttached) {
                    await this.chrome.debugger.attach({tabId: record.pageTabId}, "1.3");
                    record.debuggerAttached = true;
                }
                if (operation.cancelled) completion = "not_applied";
                else {
                    operation.mutationStarted = true;
                    await this.chrome.debugger.sendCommand(
                        {tabId: record.pageTabId}, "Page.navigate", {url: url.href});
                    operation.committed = true;
                    completion = operation.cancelled ? "unknown" : "applied";
                }
            } else if (payload.command.command === "close_page") {
                await this.detach(record.pageTabId);
                if (operation.cancelled) completion = "not_applied";
                else {
                    operation.mutationStarted = true;
                    await this.chrome.tabs.remove(record.pageTabId);
                    operation.committed = true;
                    this.forget(record);
                    completion = operation.cancelled ? "unknown" : "applied";
                }
            } else if (payload.command.command === "close_window") {
                await this.detach(record.pageTabId);
                if (operation.cancelled) completion = "not_applied";
                else {
                    operation.mutationStarted = true;
                    await this.chrome.windows.remove(record.windowId);
                    operation.committed = true;
                    this.forget(record);
                    completion = operation.cancelled ? "unknown" : "applied";
                }
            }
        } catch (_) {
            completion = operation.mutationStarted || operation.committed || operation.uncertain ?
                "unknown" : "not_applied";
        } finally {
            try {
                await this.persist();
            } catch (_) {
                if (operation.mutationStarted || operation.committed) operation.uncertain = true;
            }
            if (operation.cancelled) {
                completion = operation.mutationStarted || operation.committed || operation.uncertain ?
                    "unknown" : "not_applied";
            }
            operation.completion = completion;
            try {
                await this.completion(payload, completion);
            } finally {
                this.rememberCompletedOperation(operationKey, operation);
                if (this.operations.get(operationKey) === operation) {
                    this.operations.delete(operationKey);
                }
                operation.resolveDone();
            }
        }
    }

    async cancellationCompletion(payload, completion) {
        await this.nativeRequest("abush_browser", "cancellation_completion", {
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            request_id: payload.request_id,
            effect_id: payload.effect_id,
            executor_fence: payload.executor_fence,
            completion,
        });
    }

    async cancel(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "execution_request_id", "effect_id", "executor_fence"]) ||
                ![payload.remote_key, payload.connection_id, payload.principal_id,
                    payload.request_id, payload.execution_request_id, payload.effect_id]
                    .every(value => typeof value === "string" && value) ||
                payload.request_id === payload.execution_request_id ||
                !validFence(payload.executor_fence)) return;
        const execution = {...payload, request_id: payload.execution_request_id};
        delete execution.execution_request_id;
        const operationKey = this.operationKey(execution);
        const operation = this.operations.get(operationKey) ||
            this.completedOperations.get(operationKey);
        if (operation && this.operations.get(operationKey) === operation) {
            operation.cancelled = true;
            if (operation.mutationStarted) operation.uncertain = true;
            try { await operation.done; } catch (_) {}
        }
        const completion = operation &&
            (operation.mutationStarted || operation.committed || operation.uncertain ||
                operation.completion === "applied" || operation.completion === "unknown") ?
            "unknown" : "not_applied";
        await this.cancellationCompletion(payload, completion);
    }

    cancelMatching(matches) {
        const cancels = [];
        for (const waiter of this.ensureWaiters.values()) {
            if (matches(waiter.payload)) cancels.push(this.cancelEnsure(this.ensureIdentity(waiter.payload)));
        }
        for (const operation of this.operations.values()) {
            if (!matches(operation)) continue;
            operation.cancelled = true;
            if (operation.mutationStarted) operation.uncertain = true;
        }
        return Promise.all(cancels);
    }

    async remoteState(state) {
        if (!state || typeof state.remote_key !== "string") return;
        const disconnected = state.connected === false;
        await this.cancelMatching(identity => identity.remote_key === state.remote_key &&
            (disconnected || (typeof state.connection_id === "string" &&
                identity.connection_id !== state.connection_id)));
    }

    async invalidateRemote(state = null) {
        await this.cancelMatching(identity => !state ||
            (identity.remote_key === state.remote_key &&
                (!state.connection_id || identity.connection_id === state.connection_id) &&
                (!state.principal_id || identity.principal_id === state.principal_id)));
    }

    async detach(tabId) {
        try { await this.chrome.debugger.detach({tabId}); } catch (_) {}
    }

    forget(record) {
        const key = this.key(record.remoteKey, record.principalId);
        this.workspaces.delete(key);
        this.windowOwners.delete(record.windowId);
        this.tabOwners.delete(record.pageTabId);
    }

    async debuggerDetached(tabId) {
        const workspaceKey = this.tabOwners.get(tabId);
        if (!workspaceKey) return;
        const record = this.workspaces.get(workspaceKey);
        if (record) record.debuggerAttached = false;
        for (const operation of this.operations.values()) {
            if (operation.workspaceKey === workspaceKey && operation.tabId === tabId) {
                operation.uncertain = true;
            }
        }
    }

    async tabMoved(tabId, moveInfo) {
        const key = this.tabOwners.get(tabId);
        if (!key) return;
        const record = this.workspaces.get(key);
        if (!record || moveInfo.windowId === record.windowId) return;
        await this.nativeRequest("abush_browser", "managed_page_moved_outside", {
            remote_key: record.remoteKey,
            connection_id: record.connectionId,
            principal_id: record.principalId,
            request_id: crypto.randomUUID(),
            executor_fence: record.executorFence,
            page: this.pageRef(record),
        });
        await this.detach(tabId);
        this.forget(record);
        await this.persist();
    }

    ownsWindow(windowId) {
        return this.windowOwners.has(windowId);
    }

    async status() {
        const {remotes} = await this.nativeRequest("abush_browser", "refresh_remotes", {});
        return remotes;
    }
}

if (typeof module !== "undefined") {
    module.exports = {WorkspaceExecutor, sameRef, displayTitle, STATUS_PREFIX,
        BROWSER_ERROR_CODES};
}
