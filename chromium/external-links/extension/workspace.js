const STATUS_PREFIX = "__ABUSH_BIND__:";
const ORDINARY_PREFIX = "__ABUSH_ORDINARY__:";
const STATUS_NONCE_RE = /^[0-9a-f]{32}$/;
const MARKER_IDS = ["a", "b"];
const OWNED_MARKERS_KEY = "abushOwnedMarkers";
const TOPOLOGY_OUTBOX_KEY = "abushTopologyOutbox";
const CLEANUP_TOMBSTONES_KEY = "abushCleanupTombstones";
const RESTORE_HOST_KEY = "abushRestoreHost";
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
        this.statusTabOwners = new Map();
        this.operations = new Map();
        this.completedOperations = new Map();
        this.ordinaryOperations = new Map();
        this.completedOrdinaryOperations = new Map();
        this.ordinaryTail = Promise.resolve();
        this.ordinaryWindowMru = new Map();
        this.ordinaryMruClock = 0;
        this.ensureFlights = new Map();
        this.ensureWaiters = new Map();
        this.persistTail = Promise.resolve();
        this.persistRevision = 0;
        this.markerTail = Promise.resolve();
        this.lifecycleTail = Promise.resolve();
        this.topologyOutbox = new Map();
        this.outboxTail = Promise.resolve();
        this.cleanupTail = Promise.resolve();
        this.retireCompletions = new Set();
        this.advisoryErrors = [];
        this.notifiedAdvisoryErrors = new WeakSet();
        this.onAdvisoryError = null;
        this.removedHostSnapshots = new Map();
        this.snapshotTails = new Map();
        this.recentSessionIds = null;
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

    async persistWithRemovedRecord(record) {
        await this.persistWithoutRecords([record]);
    }

    async persistOrForget(record) {
        try {
            await this.persistWithRemovedRecord(record);
            return null;
        } catch (error) {
            return error;
        } finally {
            if (this.workspaces.get(this.key(record.remoteKey, record.principalId)) === record) {
                this.forget(record);
            }
        }
    }

    enqueueLifecycle(task) {
        const operation = this.lifecycleTail.catch(() => {}).then(task);
        this.lifecycleTail = operation;
        return operation;
    }

    async persistWithoutRecords(removed) {
        const removedRecords = new Set(removed);
        const write = this.persistTail.catch(() => {}).then(async () => {
            const records = [...this.workspaces.values()]
                .filter(saved => !removedRecords.has(saved))
                .map(saved => this.persistedRecord(saved));
            await this.persistRecords(records);
            for (const record of removedRecords) {
                const key = this.key(record.remoteKey, record.principalId);
                if (this.workspaces.get(key) === record) {
                    this.forget(record);
                }
            }
        });
        this.persistTail = write;
        await write;
    }

    topologyReport(record, page = this.pageRef(record)) {
        return {
            remote_key: record.remoteKey,
            connection_id: record.connectionId,
            principal_id: record.principalId,
            request_id: crypto.randomUUID(),
            executor_fence: record.executorFence,
            page,
        };
    }

    topologyReportKey(report) {
        return JSON.stringify({
            remote_key: report.remote_key,
            connection_id: report.connection_id,
            principal_id: report.principal_id,
            executor_fence: report.executor_fence,
            page: report.page,
        });
    }

    async persistTopologyOutbox() {
        if (!this.chrome.storage?.local) throw new Error("local extension storage is unavailable");
        await this.chrome.storage.local.set({
            [TOPOLOGY_OUTBOX_KEY]: [...this.topologyOutbox.values()],
        });
    }

    async queueTopologyReport(report) {
        const key = this.topologyReportKey(report);
        const existing = this.topologyOutbox.get(key);
        this.topologyOutbox.set(key, existing || report);
        try {
            await this.persistTopologyOutbox();
        } catch (error) {
            if (!existing) this.topologyOutbox.delete(key);
            throw error;
        }
        return key;
    }

    async flushTopologyOutbox() {
        const flush = this.outboxTail.catch(() => {}).then(async () => {
            const failures = [];
            for (const [key, report] of [...this.topologyOutbox]) {
                try {
                    await this.nativeRequest(
                        "abush_browser", "managed_page_moved_outside", report);
                    this.topologyOutbox.delete(key);
                    await this.persistTopologyOutbox();
                } catch (error) {
                    this.topologyOutbox.set(key, report);
                    try {
                        await this.persistTopologyOutbox();
                    } catch (persistError) {
                        failures.push(new AggregateError([error, persistError],
                            "topology report and outbox persistence failed"));
                        continue;
                    }
                    failures.push(error);
                    continue;
                }
            }
            if (failures.length > 0) {
                throw new AggregateError(failures, "managed topology report is pending retry");
            }
        });
        this.outboxTail = flush;
        await flush;
    }

    async restoreTopologyOutbox() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([TOPOLOGY_OUTBOX_KEY]);
        const reports = local[TOPOLOGY_OUTBOX_KEY];
        if (!Array.isArray(reports)) return;
        let dirty = false;
        for (const report of reports) {
            if (exactFields(report, ["remote_key", "connection_id", "principal_id",
                "request_id", "executor_fence", "page"]) && validPageRef(report.page) &&
                    validFence(report.executor_fence) &&
                    [report.remote_key, report.connection_id, report.principal_id,
                        report.request_id].every(value => typeof value === "string" && value)) {
                const key = this.topologyReportKey(report);
                if (!this.topologyOutbox.has(key)) this.topologyOutbox.set(key, report);
                else dirty = true;
            } else {
                dirty = true;
            }
        }
        if (dirty) await this.persistTopologyOutbox();
    }

    reportAdvisoryError(error) {
        this.advisoryErrors.push(error);
        if (error && typeof error === "object" && !this.notifiedAdvisoryErrors.has(error) &&
                this.onAdvisoryError) {
            this.notifiedAdvisoryErrors.add(error);
            this.onAdvisoryError(error);
        }
    }

    drainAdvisoryErrors() {
        const errors = this.advisoryErrors.splice(0);
        return errors.filter(error => !this.notifiedAdvisoryErrors.has(error));
    }

    async recordCleanupTombstone(record, errors) {
        if (!this.chrome.storage?.local) return;
        const write = this.cleanupTail.catch(() => {}).then(async () => {
            const local = await this.chrome.storage.local.get([CLEANUP_TOMBSTONES_KEY]);
            const tombstones = Array.isArray(local[CLEANUP_TOMBSTONES_KEY]) ?
                local[CLEANUP_TOMBSTONES_KEY] : [];
            tombstones.push({
                nonce: record.nonce,
                window_id: record.windowId,
                status_tab_ids: [...record.statusTabIds],
                page_tab_id: record.pageTabId,
                errors: errors.map(error => error?.message || String(error)),
            });
            await this.chrome.storage.local.set({
                [CLEANUP_TOMBSTONES_KEY]: tombstones.slice(-64),
            });
        });
        this.cleanupTail = write;
        await write;
    }

    async appendCleanupError(errors, cleanup) {
        try {
            await cleanup();
        } catch (error) {
            errors.push(error);
        }
    }

    async updateOwnedMarkers(update) {
        if (!this.chrome.storage?.local) throw new Error("local extension storage is unavailable");
        const write = this.markerTail.catch(() => {}).then(async () => {
            const stored = await this.chrome.storage.local.get([OWNED_MARKERS_KEY]);
            const markers = stored[OWNED_MARKERS_KEY] || {};
            update(markers);
            await this.chrome.storage.local.set({[OWNED_MARKERS_KEY]: markers});
        });
        this.markerTail = write;
        await write;
    }

    async ownMarker(nonce) {
        await this.updateOwnedMarkers(markers => {
            markers[nonce] = {version: 1, nonce, pending: true};
        });
    }

    markerOwnership(record) {
        return {
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
            binding: this.bindingPayload(record),
        };
    }

    savedOwnershipMatches(ownership, record) {
        return Boolean(ownership && ownership.version === 2 &&
            ownership.nonce === record.nonce && ownership.window_id === record.windowId &&
            sameRef(ownership.status_tab_ids, record.statusTabIds) &&
            ownership.page_tab_id === record.pageTabId && ownership.xid === record.x11WindowId &&
            ownership.logical_window_id === record.logicalWindowId &&
            ownership.logical_page_id === record.logicalPageId &&
            ownership.window_generation === (record.windowGeneration ?? record.generation) &&
            ownership.page_generation === (record.pageGeneration ?? record.generation) &&
            sameRef(ownership.binding, this.bindingPayload(record)));
    }

    async claimMarker(record) {
        const ownership = this.markerOwnership(record);
        if (!Number.isInteger(ownership.xid) || ownership.xid <= 0) {
            throw new Error("managed marker has no verified X11 window");
        }
        await this.updateOwnedMarkers(markers => {
            markers[record.nonce] = ownership;
        });
    }

    async disownMarker(nonce) {
        await this.updateOwnedMarkers(markers => {
            delete markers[nonce];
        });
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

    statusUrl(record, marker) {
        return this.chrome.runtime.getURL(
            `status.html#nonce=${record.nonce}&marker=${marker}`
        );
    }

    parseStatusUrl(url) {
        const prefix = this.chrome.runtime.getURL("status.html#");
        if (typeof url !== "string" || !url.startsWith(prefix)) return null;
        const fragment = url.slice(prefix.length);
        if (STATUS_NONCE_RE.test(fragment)) return {nonce: fragment, marker: "legacy"};
        const match = /^nonce=([0-9a-f]{32})&marker=([ab])$/.exec(fragment);
        return match ? {nonce: match[1], marker: match[2]} : null;
    }

    ordinaryStagingUrl(nonce) {
        return this.chrome.runtime.getURL(`ordinary-staging.html#${nonce}`);
    }

    validateOrdinaryOpen(payload) {
        if (!exactFields(payload, ["version", "request_id", "operation", "url", "focus"]) ||
                payload.version !== 1 || payload.operation !== "ordinary_open" ||
                payload.focus !== true || typeof payload.request_id !== "string" ||
                !payload.request_id || typeof payload.url !== "string" || !payload.url) {
            throw new Error("invalid ordinary-open payload");
        }
        const url = new URL(payload.url);
        if (!["http:", "https:", "file:"].includes(url.protocol)) {
            throw new Error("invalid ordinary-open URL");
        }
        return url.href;
    }

    async ordinaryWindows() {
        const windows = await this.chrome.windows.getAll({windowTypes: ["normal"]});
        const ordinary = [];
        for (const window of windows) {
            if (window.type !== "normal" || this.windowOwners.has(window.id)) continue;
            const tabs = await this.chrome.tabs.query({windowId: window.id});
            if (tabs.some(tab => this.parseStatusUrl(tab.url) !== null)) continue;
            ordinary.push(window);
        }
        return ordinary;
    }

    async selectOrdinaryWindow() {
        const ordinary = await this.ordinaryWindows();
        const selected = ordinary.find(window => window.focused) ||
            ordinary.sort((left, right) =>
                (this.ordinaryWindowMru.get(right.id) || 0) -
                (this.ordinaryWindowMru.get(left.id) || 0))[0];
        if (!selected) return null;
        const tabs = await this.chrome.tabs.query({windowId: selected.id});
        const originalActiveTab = tabs.find(tab => tab.active);
        if (!Number.isInteger(originalActiveTab?.id)) {
            throw new Error("ordinary window has no active tab");
        }
        return {
            windowId: selected.id,
            stagingTabId: null,
            originalActiveTabId: originalActiveTab.id,
            createdWindow: false,
        };
    }

    rememberOrdinaryWindow(windowId) {
        this.ordinaryWindowMru.set(windowId, ++this.ordinaryMruClock);
    }

    async recordOrdinaryFocus(windowId) {
        if (!Number.isInteger(windowId) || windowId < 0) return false;
        const ordinary = await this.ordinaryWindows();
        if (!ordinary.some(window => window.id === windowId)) return false;
        this.rememberOrdinaryWindow(windowId);
        return true;
    }

    async initializeOrdinaryMru() {
        let lastFocused = null;
        try {
            if (typeof this.chrome.windows.getLastFocused === "function") {
                lastFocused = await this.chrome.windows.getLastFocused({windowTypes: ["normal"]});
            }
        } catch (_) {}
        const ordinary = await this.ordinaryWindows();
        const selected = ordinary.find(window => window.id === lastFocused?.id) ||
            ordinary.find(window => window.focused);
        if (selected) this.rememberOrdinaryWindow(selected.id);
    }

    async ordinaryCompletion(payload, completion) {
        await this.nativeRequest("ordinary_open", "ordinary_open_completion", {
            version: 1,
            request_id: payload.request_id,
            operation: "ordinary_open_completed",
            completion,
        });
    }

    rememberOrdinaryCompletion(payload, completion) {
        this.completedOrdinaryOperations.delete(payload.request_id);
        this.completedOrdinaryOperations.set(payload.request_id, {
            payload: {...payload},
            completion,
        });
        if (this.completedOrdinaryOperations.size > 256) {
            this.completedOrdinaryOperations.delete(
                this.completedOrdinaryOperations.keys().next().value);
        }
    }

    sameOrdinaryPayload(left, right) {
        return left.version === right.version && left.request_id === right.request_id &&
            left.operation === right.operation && left.url === right.url &&
            left.focus === right.focus;
    }

    async ordinaryOpen(payload) {
        this.validateOrdinaryOpen(payload);
        const completed = this.completedOrdinaryOperations.get(payload.request_id);
        if (completed) {
            if (!this.sameOrdinaryPayload(completed.payload, payload)) {
                throw new Error("conflicting duplicate ordinary-open request");
            }
            await this.ordinaryCompletion(payload, completed.completion);
            return completed.completion;
        }
        const existing = this.ordinaryOperations.get(payload.request_id);
        if (existing) {
            if (!this.sameOrdinaryPayload(existing.payload, payload)) {
                throw new Error("conflicting duplicate ordinary-open request");
            }
            return existing.operation;
        }
        const operation = this.ordinaryTail.catch(() => {})
            .then(() => this.performOrdinaryOpen(payload));
        this.ordinaryTail = operation;
        operation.finally(() => {
            if (this.ordinaryOperations.get(payload.request_id)?.operation === operation) {
                this.ordinaryOperations.delete(payload.request_id);
            }
        }).catch(() => {});
        this.ordinaryOperations.set(payload.request_id, {payload: {...payload}, operation});
        return operation;
    }

    async tabIsMissing(tabId) {
        try {
            await this.chrome.tabs.get(tabId);
            return false;
        } catch (_) {
            return true;
        }
    }

    async windowIsMissing(windowId) {
        try {
            await this.chrome.windows.get(windowId);
            return false;
        } catch (_) {
            return true;
        }
    }

    async cleanupOrdinaryPrepare(context, nonce) {
        if (!context) return true;
        if (context.createdWindow) {
            if (Number.isInteger(context.windowId)) {
                try {
                    await this.chrome.windows.remove(context.windowId);
                } catch (_) {}
                return this.windowIsMissing(context.windowId);
            }
            return this.cleanupUnknownOrdinaryStaging(
                nonce, context.baselineWindowIds || []);
        }

        let restored = false;
        if (Number.isInteger(context.originalActiveTabId)) {
            try {
                await this.chrome.tabs.update(context.originalActiveTabId, {active: true});
                const original = await this.chrome.tabs.get(context.originalActiveTabId);
                restored = original.active === true;
            } catch (_) {}
        }
        let stagingIds = [];
        if (Number.isInteger(context.stagingTabId)) stagingIds.push(context.stagingTabId);
        try {
            const tabs = await this.chrome.tabs.query({windowId: context.windowId});
            stagingIds.push(...tabs.filter(tab => tab.url === this.ordinaryStagingUrl(nonce))
                .map(tab => tab.id));
        } catch (_) {
            return false;
        }
        stagingIds = [...new Set(stagingIds.filter(Number.isInteger))];
        for (const tabId of stagingIds) {
            try {
                await this.chrome.tabs.remove(tabId);
            } catch (_) {}
        }
        const removed = (await Promise.all(stagingIds.map(tabId => this.tabIsMissing(tabId))))
            .every(Boolean);
        return restored && removed;
    }

    async cleanupUnknownOrdinaryStaging(nonce, baselineWindowIds) {
        let tabs;
        let windows;
        try {
            [tabs, windows] = await Promise.all([
                this.chrome.tabs.query({}),
                this.chrome.windows.getAll({windowTypes: ["normal"]}),
            ]);
        } catch (_) {
            return false;
        }
        const baseline = new Set(baselineWindowIds);
        const stagingUrl = this.ordinaryStagingUrl(nonce);
        const candidateIds = [...new Set(tabs
            .filter(tab => tab.url === stagingUrl && !baseline.has(tab.windowId))
            .map(tab => tab.windowId)
            .filter(Number.isInteger))];
        const observedNewIds = windows.map(window => window.id)
            .filter(windowId => !baseline.has(windowId));
        for (const windowId of candidateIds) {
            try {
                await this.chrome.windows.remove(windowId);
            } catch (_) {}
        }
        const removed = (await Promise.all(candidateIds.map(id => this.windowIsMissing(id))))
            .every(Boolean);
        if (!removed) return false;
        let remaining;
        try {
            remaining = await this.chrome.windows.getAll({windowTypes: ["normal"]});
        } catch (_) {
            return false;
        }
        const remainingNewIds = remaining.map(window => window.id)
            .filter(windowId => !baseline.has(windowId));
        return observedNewIds.every(windowId => candidateIds.includes(windowId)) &&
            remainingNewIds.length === 0;
    }

    async verifyOrdinaryStaging(context, nonce) {
        const stagingUrl = this.ordinaryStagingUrl(nonce);
        const tabs = await this.chrome.tabs.query({});
        const matches = tabs.filter(tab => tab.url === stagingUrl);
        if (matches.length !== 1 || matches[0].id !== context.stagingTabId ||
                matches[0].windowId !== context.windowId || matches[0].active !== true ||
                matches[0].title !== `${ORDINARY_PREFIX}${nonce}`) {
            throw new Error("ordinary staging tab is not unique and active in its exact window");
        }
    }

    async performOrdinaryOpen(payload) {
        let context = null;
        let navigationRequested = false;
        let completion = "not_applied";
        const nonce = crypto.randomUUID().replaceAll("-", "");
        try {
            const url = this.validateOrdinaryOpen(payload);
            context = await this.selectOrdinaryWindow();
            if (context === null) {
                const windows = await this.chrome.windows.getAll({windowTypes: ["normal"]});
                context = {
                    windowId: null,
                    stagingTabId: null,
                    originalActiveTabId: null,
                    createdWindow: true,
                    baselineWindowIds: windows.map(window => window.id),
                };
                const stagingWindow = await this.chrome.windows.create({
                    url: this.ordinaryStagingUrl(nonce), focused: false, type: "normal",
                });
                context.windowId = stagingWindow?.id;
                const returnedTabs = Array.isArray(stagingWindow?.tabs) ? stagingWindow.tabs : [];
                const returnedStaging = returnedTabs.filter(tab =>
                    tab.url === this.ordinaryStagingUrl(nonce));
                if (returnedStaging.length === 1) context.stagingTabId = returnedStaging[0].id;
                if (Number.isInteger(context.windowId) &&
                        !Number.isInteger(context.stagingTabId)) {
                    const tabs = await this.chrome.tabs.query({windowId: context.windowId});
                    const stagingTabs = tabs.filter(tab =>
                        tab.url === this.ordinaryStagingUrl(nonce));
                    if (stagingTabs.length === 1) context.stagingTabId = stagingTabs[0].id;
                }
            } else {
                const stagingTab = await this.chrome.tabs.create({
                    windowId: context.windowId,
                    url: this.ordinaryStagingUrl(nonce),
                    active: true,
                });
                context.stagingTabId = stagingTab.id;
            }
            if (!Number.isInteger(context.windowId) ||
                    !Number.isInteger(context.stagingTabId)) {
                throw new Error("ordinary staging window has no exact tab");
            }
            const staging = await this.chrome.tabs.get(context.stagingTabId);
            if (staging.windowId !== context.windowId || staging.active !== true) {
                throw new Error("ordinary staging tab is not active in its exact window");
            }
            await this.waitForOrdinaryTitle(context.stagingTabId, nonce);
            await this.verifyOrdinaryStaging(context, nonce);
            const prepared = await this.nativeRequest("ordinary_open", "prepare", {nonce});
            if (!exactFields(prepared, ["xid", "nonce"]) || prepared.nonce !== nonce ||
                    !Number.isInteger(prepared.xid) || prepared.xid <= 0) {
                throw new Error("invalid ordinary prepare response");
            }
            navigationRequested = true;
            if (new URL(url).protocol === "file:") {
                let navigated;
                try {
                    navigated = await this.nativeRequest(
                        "ordinary_open", "ordinary_file_navigate",
                        {nonce, xid: prepared.xid, url});
                } catch (error) {
                    if (error?.nativeDispatch === "not_sent") navigationRequested = false;
                    throw error;
                }
                if (!exactFields(navigated, ["nonce", "xid", "url", "completion"]) ||
                        navigated.nonce !== nonce || navigated.xid !== prepared.xid ||
                        navigated.url !== url ||
                        !["applied", "not_applied", "unknown"].includes(
                            navigated.completion)) {
                    throw new Error("invalid ordinary file navigation response");
                }
                if (navigated.completion === "not_applied") {
                    navigationRequested = false;
                    throw new Error("ordinary file navigation was not applied");
                }
                if (navigated.completion === "unknown") {
                    throw new Error("ordinary file navigation completion is unknown");
                }
            } else {
                try {
                    await this.chrome.tabs.update(context.stagingTabId, {url, active: true});
                } catch (_) {
                    throw new Error("ordinary navigation completion is unknown");
                }
            }
            try {
                await this.nativeRequest("ordinary_open", "activate", prepared);
                this.rememberOrdinaryWindow(context.windowId);
                completion = "applied";
            } catch (_) {
                completion = "unknown";
            }
        } catch (_) {
            if (navigationRequested) {
                completion = "unknown";
            } else {
                const cleaned = context ?
                    await this.cleanupOrdinaryPrepare(context, nonce) : true;
                completion = cleaned ? "not_applied" : "unknown";
            }
        }
        this.rememberOrdinaryCompletion(payload, completion);
        await this.ordinaryCompletion(payload, completion);
        return completion;
    }

    async waitForOrdinaryTitle(tabId, nonce) {
        const expected = `${ORDINARY_PREFIX}${nonce}`;
        for (let attempt = 0; attempt < 100; attempt++) {
            const tab = await this.chrome.tabs.get(tabId);
            if (tab.active === true && tab.title === expected) return;
            await new Promise(resolve => setTimeout(resolve, 10));
        }
        throw new Error("ordinary staging title was not ready");
    }

    async waitForStatusTitle(tabId, nonce, marker) {
        const expected = `${STATUS_PREFIX}${nonce}:${marker}`;
        for (let attempt = 0; attempt < 100; attempt++) {
            const tab = await this.chrome.tabs.get(tabId);
            if (tab.title === expected) return;
            await new Promise(resolve => setTimeout(resolve, 10));
        }
        throw new Error("status tab title was not ready");
    }

    sessionEntryId(entry) {
        const sessionId = entry?.window?.sessionId ?? entry?.tab?.sessionId ?? entry?.sessionId;
        return typeof sessionId === "string" && sessionId ? sessionId : null;
    }

    isOrdinarySessionEntry(entry) {
        return Boolean(entry?.window && this.sessionEntryId(entry) &&
            !entry.window.tabs?.some(tab => this.parseStatusUrl(tab.url) !== null));
    }

    sessionSnapshot(tabs) {
        const urls = (tabs || []).map(tab => tab.url).filter(url => typeof url === "string");
        return {tabCount: (tabs || []).length, urls};
    }

    sessionMatchesSnapshot(entry, snapshot) {
        if (!this.isOrdinarySessionEntry(entry) || !snapshot ||
                !Number.isInteger(snapshot.tabCount) || !Array.isArray(snapshot.urls)) return false;
        const observed = this.sessionSnapshot(entry.window.tabs);
        return observed.tabCount === snapshot.tabCount &&
            observed.urls.length === snapshot.urls.length &&
            observed.urls.every((url, index) => url === snapshot.urls[index]);
    }

    async restoreHost() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([RESTORE_HOST_KEY]);
        const intent = local[RESTORE_HOST_KEY];
        const exact = Boolean(intent && typeof intent === "object" && intent.version === 1 &&
            typeof intent.sessionId === "string" && intent.sessionId);
        if (intent !== true && !exact) return;
        await this.chrome.storage.local.set({[RESTORE_HOST_KEY]: false});
        const sessions = this.chrome.sessions;
        if (sessions?.restore && exact) {
            try {
                await sessions.restore(intent.sessionId);
                return;
            } catch (_) {}
        }
        if (sessions?.getRecentlyClosed && sessions?.restore) {
            const entries = await sessions.getRecentlyClosed({maxResults: 25});
            const window = exact ?
                entries.find(entry => this.sessionMatchesSnapshot(entry, intent.snapshot)) :
                entries.find(entry => this.isOrdinarySessionEntry(entry));
            if (window) {
                await sessions.restore(this.sessionEntryId(window));
                return;
            }
        }
        await this.chrome.windows.create({url: "about:blank", focused: false, type: "normal"});
    }

    async markRestoreHost(intent) {
        if (!this.chrome.storage?.local) return;
        await this.chrome.storage.local.set({[RESTORE_HOST_KEY]: intent});
    }

    rememberWindowSnapshot(windowId) {
        const previous = this.snapshotTails.get(windowId) || Promise.resolve();
        const update = previous.catch(() => {}).then(() => this.chrome.tabs.query({windowId}))
            .then(tabs => {
                if (tabs.length === 0 ||
                        tabs.some(tab => this.parseStatusUrl(tab.url) !== null)) return;
                this.removedHostSnapshots.set(windowId, this.sessionSnapshot(tabs));
            }).catch(() => {});
        this.snapshotTails.set(windowId, update);
        return update.finally(() => {
            if (this.snapshotTails.get(windowId) === update) this.snapshotTails.delete(windowId);
        });
    }

    async recentlyClosedEntries() {
        const sessions = this.chrome.sessions;
        if (!sessions?.getRecentlyClosed) return [];
        return sessions.getRecentlyClosed({maxResults: 25});
    }

    async primeRecentlyClosedSessions() {
        const entries = await this.recentlyClosedEntries();
        this.recentSessionIds = new Set(entries.map(entry => this.sessionEntryId(entry))
            .filter(Boolean));
    }

    async recentlyClosedHostIntent(windowId) {
        const pendingSnapshot = this.snapshotTails.get(windowId);
        if (pendingSnapshot) await pendingSnapshot;
        const snapshot = this.removedHostSnapshots.get(windowId);
        this.removedHostSnapshots.delete(windowId);
        if (!snapshot || !this.chrome.sessions?.getRecentlyClosed) return null;
        const knownSessionIds = this.recentSessionIds || new Set();
        for (let attempt = 0; attempt < 20; attempt++) {
            const entries = await this.recentlyClosedEntries();
            const window = entries.find(entry => {
                const sessionId = this.sessionEntryId(entry);
                return sessionId && !knownSessionIds.has(sessionId) &&
                    this.sessionMatchesSnapshot(entry, snapshot);
            });
            if (window) {
                this.recentSessionIds = new Set(entries.map(entry => this.sessionEntryId(entry))
                    .filter(Boolean));
                return {
                    version: 1,
                    sessionId: this.sessionEntryId(window),
                    snapshot,
                };
            }
            if (attempt + 1 < 20) {
                await new Promise(resolve => setTimeout(resolve, 25));
            }
        }
        return null;
    }

    async closeOrphanedStatusWindows(liveWindowIds) {
        const tabs = await this.chrome.tabs.query({});
        const local = this.chrome.storage?.local ?
            await this.chrome.storage.local.get([OWNED_MARKERS_KEY]) : {};
        const ownedMarkers = local[OWNED_MARKERS_KEY] || {};
        const windowTypes = new Map((await this.chrome.windows.getAll()).map(window =>
            [window.id, window.type]));
        const windows = new Map();
        const cleanupErrors = [];
        for (const tab of tabs) {
            if (!windows.has(tab.windowId)) windows.set(tab.windowId, []);
            windows.get(tab.windowId).push(tab);
        }
        for (const [windowId, windowTabs] of windows) {
            if (liveWindowIds.has(windowId)) continue;
            const parsed = windowTabs.map(tab => ({tab, marker: this.parseStatusUrl(tab.url)}));
            const markers = parsed.filter(item => item.marker !== null);
            const legacyMarkers = markers.filter(item => item.marker.marker === "legacy");
            const markerIds = new Set(markers.map(item => item.marker.marker));
            const nonces = new Set(markers.map(item => item.marker.nonce));
            const ownership = nonces.size === 1 ?
                ownedMarkers[markers[0]?.marker.nonce] : null;
            const exactOwnedFingerprint = Boolean(ownership && ownership.version === 2 &&
                Array.isArray(ownership.status_tab_ids) &&
                ownership.nonce === markers[0]?.marker.nonce &&
                ownership.window_id === windowId &&
                sameRef([...ownership.status_tab_ids].sort((left, right) => left - right),
                    markers.map(({tab}) => tab.id).sort((left, right) => left - right)) &&
                windowTabs.some(tab => tab.id === ownership.page_tab_id));
            const ownedWindow = exactOwnedFingerprint && windowTypes.get(windowId) === "normal" &&
                windowTabs.length === 3 && markers.length === 2 &&
                legacyMarkers.length === 0 && markerIds.size === 2 && nonces.size === 1 &&
                markers.every(({tab, marker}) => tab.pinned &&
                    tab.title === `${STATUS_PREFIX}${marker.nonce}:${marker.marker}`) &&
                markers.map(({tab}) => tab.index).sort().join(",") === "0,1" &&
                windowTabs.filter(tab => !tab.pinned && tab.index === 2).length === 1;
            if (ownedWindow) {
                try {
                    await this.chrome.windows.remove(windowId);
                } catch (error) {
                    if (!await this.windowIsMissing(windowId)) cleanupErrors.push(error);
                }
                delete ownedMarkers[markers[0].marker.nonce];
            } else {
                for (const {tab} of markers) await this.removeKnownTab(tab.id, cleanupErrors);
                if (ownership?.version === 2 && Number.isInteger(ownership.xid) &&
                        ownership.xid > 0) {
                    const record = {
                        remoteKey: ownership.binding?.remote_key,
                        workspaceId: ownership.binding?.workspace_id,
                        connectionDisplayName: ownership.binding?.display_title,
                        windowGeneration: ownership.binding?.generation,
                        nonce: ownership.nonce,
                        x11WindowId: ownership.xid,
                    };
                    if (record.remoteKey && record.workspaceId &&
                            Number.isInteger(record.windowGeneration)) {
                        await this.clearManagedX11(record, cleanupErrors);
                    }
                }
                if (markers.length > 0 && nonces.size === 1) {
                    delete ownedMarkers[markers[0].marker.nonce];
                }
            }
        }
        if (this.chrome.storage?.local) {
            await this.chrome.storage.local.set({[OWNED_MARKERS_KEY]: ownedMarkers});
        }
        if (cleanupErrors.length > 0) {
            this.reportAdvisoryError(new AggregateError(
                cleanupErrors, "orphan X11 cleanup was incomplete"));
        }
    }

    async normalWindowId() {
        const windows = await this.chrome.windows.getAll({windowTypes: ["normal"]});
        const normal = windows.filter(window => window.type === "normal");
        if (normal.length === 0) throw new Error("main Chrome has no normal window");
        return normal.find(window => window.focused)?.id ?? normal[0].id;
    }

    async windowType(windowId) {
        const window = await this.chrome.windows.get(windowId);
        return window.type;
    }

    async isLive(record) {
        try {
            if (await this.windowType(record.windowId) !== "normal") return false;
            const tabs = await this.chrome.tabs.query({windowId: record.windowId});
            return tabs.length === 3 && STATUS_NONCE_RE.test(record.nonce) &&
                MARKER_IDS.every((marker, index) =>
                    tabs.some(tab => tab.id === record.statusTabIds[index] &&
                        tab.url === this.statusUrl(record, marker) &&
                        tab.title === `${STATUS_PREFIX}${record.nonce}:${marker}` &&
                        tab.pinned && tab.index === index)) &&
                tabs.filter(tab => tab.id === record.pageTabId && !tab.pinned &&
                    tab.index === 2).length === 1 &&
                !record.statusTabIds.includes(record.pageTabId);
        } catch (_) {
            return false;
        }
    }

    async restore() {
        await this.restoreHost();
        await this.restoreTopologyOutbox();
        const liveWindowIds = new Set();
        let removed = false;
        const seenKeys = new Set();
        const seenPhysical = new Set();
        const ownedMarkers = this.chrome.storage?.local ?
            (await this.chrome.storage.local.get([OWNED_MARKERS_KEY]))[OWNED_MARKERS_KEY] || {} :
            {};
        if (this.chrome.storage?.session) {
            const {abushWorkspaces = [], revision = 0} =
                await this.chrome.storage.session.get(["abushWorkspaces", "revision"]);
            if (Number.isInteger(revision) && revision >= 0) this.persistRevision = revision;
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
                        typeof saved.windowId !== "number" ||
                        !Number.isInteger(saved.x11WindowId) || saved.x11WindowId <= 0 ||
                        !Array.isArray(saved.statusTabIds) || saved.statusTabIds.length !== 2 ||
                        !saved.statusTabIds.every(id => typeof id === "number") ||
                        typeof saved.pageTabId !== "number" || typeof saved.nonce !== "string") {
                    removed = true;
                    continue;
                }
                if (!await this.isLive(saved) ||
                        !this.savedOwnershipMatches(ownedMarkers[saved.nonce], saved)) {
                    removed = true;
                    continue;
                }
                const key = this.key(saved.remoteKey, saved.principalId);
                const physical = [saved.windowId, saved.pageTabId, ...saved.statusTabIds];
                if (seenKeys.has(key) || physical.some(id => seenPhysical.has(id))) {
                    removed = true;
                    continue;
                }
                seenKeys.add(key);
                physical.forEach(id => seenPhysical.add(id));
                saved.debuggerAttached = false;
                this.install(saved);
                liveWindowIds.add(saved.windowId);
            }
        }
        await this.closeOrphanedStatusWindows(liveWindowIds);
        if (removed) await this.persist();
        const normalWindows = await this.chrome.windows.getAll({windowTypes: ["normal"]});
        await Promise.all(normalWindows.map(window => this.rememberWindowSnapshot(window.id)));
        await this.primeRecentlyClosedSessions();
        await this.initializeOrdinaryMru();
    }

    install(record) {
        const key = this.key(record.remoteKey, record.principalId);
        this.workspaces.set(key, record);
        this.windowOwners.set(record.windowId, key);
        this.tabOwners.set(record.pageTabId, key);
        for (const tabId of record.statusTabIds) this.statusTabOwners.set(tabId, key);
    }

    bindingPayload(record) {
        return {
            nonce: record.nonce,
            remote_key: record.remoteKey,
            workspace_id: record.workspaceId,
            generation: record.windowGeneration ?? record.generation,
            control_state: "remote",
            display_title: displayTitle(record.connectionDisplayName),
        };
    }

    validBindResult(result, payload) {
        const observed = result?.observed;
        return exactFields(result,
            ["available", "xid", "observed", "qtile_moved", "observed_group"]) &&
            result.available === true && Number.isInteger(result.xid) && result.xid > 0 &&
            result.qtile_moved === true && result.observed_group === "c" &&
            exactFields(observed, ["WM_WINDOW_ROLE", "_ABUSH_BINDING_NONCE",
                "_ABUSH_REMOTE_ID", "_ABUSH_WORKSPACE_ID", "_ABUSH_WINDOW_GENERATION",
                "_ABUSH_CONTROL_STATE", "_ABUSH_DISPLAY_TITLE"]) &&
            observed.WM_WINDOW_ROLE === "abush-browser" &&
            observed._ABUSH_BINDING_NONCE === payload.nonce &&
            observed._ABUSH_REMOTE_ID === payload.remote_key &&
            observed._ABUSH_WORKSPACE_ID === payload.workspace_id &&
            observed._ABUSH_WINDOW_GENERATION === String(payload.generation) &&
            observed._ABUSH_CONTROL_STATE === payload.control_state &&
            observed._ABUSH_DISPLAY_TITLE === payload.display_title;
    }

    async bind(record, workspaceId = record.workspaceId) {
        const payload = this.bindingPayload({...record, workspaceId});
        await this.chrome.tabs.update(record.statusTabIds[0], {active: true});
        try {
            const result = await this.nativeRequest(
                "abush_browser", "bind_x11_window", payload);
            if (!this.validBindResult(result, payload)) {
                this.fail("invalid_command", "native X11 bind was not fully verified");
            }
            record.x11WindowId = result.xid;
            return result;
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
                committed: false, cleanupCompleted: false, expectedPage: payload.expected_page};
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
        const completionKey = this.ensureRequestKey(payload);
        if (this.retireCompletions.has(completionKey)) return true;
        const record = this.workspaces.get(this.key(payload.remote_key, payload.principal_id));
        if (record && record.connectionId === payload.connection_id &&
                sameRef(record.executorFence, payload.executor_fence) &&
                record.logicalWindowId === payload.window_id &&
                record.logicalPageId === payload.page_id) {
            await this.convergeManagedTopology(record, {closeWindow: true, report: false});
        }
        await this.ensureCancellationCompleted(payload);
        this.retireCompletions.add(completionKey);
        if (this.retireCompletions.size > 256) {
            this.retireCompletions.delete(this.retireCompletions.values().next().value);
        }
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
                try {
                    await this.cleanupFlightWindow(flight);
                } catch (error) {
                    await this.recordCleanupTombstone({
                        nonce: null,
                        windowId: flight.createdWindowId,
                        statusTabIds: [],
                        pageTabId: null,
                    }, [error]);
                    const failure = new AggregateError([error],
                        "cancelled ensure cleanup is uncertain");
                    failure.browserCode = "unknown_effect";
                    throw failure;
                }
            }
            try { await flight.promise; } catch (_) {}
        }
        await this.ensureCancellationCompleted(payload);
        return true;
    }

    async cleanupFlightWindow(flight) {
        if (flight.createdWindowId === null || flight.cleanupCompleted) return;
        await this.chrome.windows.remove(flight.createdWindowId);
        if (!await this.windowIsMissing(flight.createdWindowId)) {
            throw new Error("managed bind cleanup could not remove the new window");
        }
        flight.cleanupCompleted = true;
    }

    async cleanupPreparedWindow(windowId, statusTabs, nonce) {
        const errors = [];
        if (Number.isInteger(windowId)) {
            try {
                await this.chrome.windows.remove(windowId);
            } catch (error) {
                if (!await this.windowIsMissing(windowId)) errors.push(error);
            }
        }
        for (const tab of statusTabs) await this.removeKnownTab(tab.id, errors);
        try {
            await this.disownMarker(nonce);
        } catch (error) {
            errors.push(error);
        }
        if (errors.length > 0) {
            const record = {
                nonce,
                windowId,
                statusTabIds: statusTabs.map(tab => tab.id),
                pageTabId: null,
            };
            try {
                await this.recordCleanupTombstone(record, errors);
            } catch (error) {
                errors.push(error);
            }
            const failure = new AggregateError(errors,
                "managed window preparation cleanup is uncertain");
            failure.browserCode = "unknown_effect";
            throw failure;
        }
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
            await this.convergeManagedTopology(existing);
            existing = null;
        }
        if (existing) {
            if (payload.expected_page !== null &&
                    !sameRef(payload.expected_page, this.pageRef(existing))) {
                this.fail("stale_page", "restored workspace does not match expected page");
            }
            this.ensureActive(flight);
            const previous = {...existing, statusTabIds: [...existing.statusTabIds]};
            const candidate = {
                ...existing,
                workspaceId: payload.workspace_id,
                connectionId: payload.connection_id,
                executorFence: payload.executor_fence,
                ownerDisplay: payload.owner_display,
                connectionDisplayName: payload.connection_display_name,
            };
            let bindCompleted = false;
            let committed = false;
            try {
                await this.bind(candidate, payload.workspace_id);
                bindCompleted = true;
                this.ensureActive(flight);
                if (!await this.isLive(existing)) throw new Error("workspace changed during bind");
                this.ensureActive(flight);
                await this.commitRecord(candidate, existing);
                committed = true;
                await this.claimMarker(existing);
            } catch (error) {
                const rollbackErrors = [];
                Object.assign(existing, previous);
                if (bindCompleted) {
                    if (committed) {
                        await this.appendCleanupError(rollbackErrors, () => this.persist());
                    }
                    await this.appendCleanupError(rollbackErrors, () => this.bind(existing));
                    await this.appendCleanupError(rollbackErrors, () => this.claimMarker(existing));
                }
                if (rollbackErrors.length > 0) {
                    const failure = new AggregateError([error, ...rollbackErrors],
                        "managed failover rollback is uncertain");
                    failure.browserCode = "unknown_effect";
                    throw failure;
                }
                throw error;
            }
            return existing;
        }
        const nonce = crypto.randomUUID().replaceAll("-", "");
        await this.ownMarker(nonce);
        const hostWindowId = await this.normalWindowId();
        const statusTabs = [];
        let created;
        try {
            for (const marker of MARKER_IDS) {
                const tab = await this.chrome.tabs.create({
                    windowId: hostWindowId,
                    url: this.statusUrl({nonce}, marker),
                    active: false,
                });
                await this.waitForStatusTitle(tab.id, nonce, marker);
                statusTabs.push(tab);
            }
            created = await this.chrome.windows.create({
                tabId: statusTabs[0].id,
                focused: false,
                type: "normal",
            });
            await this.chrome.tabs.move(statusTabs[1].id, {
                windowId: created.id,
                index: -1,
            });
        } catch (error) {
            try {
                await this.cleanupPreparedWindow(created?.id, statusTabs, nonce);
            } catch (cleanupError) {
                const errors = cleanupError instanceof AggregateError ?
                    cleanupError.errors : [cleanupError];
                const failure = new AggregateError([error, ...errors],
                    "managed window preparation and cleanup failed");
                failure.browserCode = "unknown_effect";
                throw failure;
            }
            throw error;
        }
        flight.createdWindowId = created.id;
        try {
            this.ensureActive(flight);
            for (const tab of statusTabs) {
                await this.chrome.tabs.update(tab.id, {pinned: true});
            }
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
                statusTabIds: statusTabs.map(tab => tab.id),
                pageTabId: business.id,
                nonce,
                debuggerAttached: false,
            };
            try {
                this.ensureActive(flight);
                await this.bind(record);
                await this.claimMarker(record);
                this.ensureActive(flight);
                if (!await this.isLive(record)) throw new Error("workspace changed during bind");
                this.ensureActive(flight);
                await this.commitRecord(record);
                flight.committed = true;
                return record;
            } catch (error) {
                this.forget(record);
                const rollbackErrors = [];
                try {
                    await this.persist();
                } catch (persistError) {
                    rollbackErrors.push(persistError);
                }
                await this.clearManagedX11(record, rollbackErrors);
                if (rollbackErrors.length > 0) {
                    const failure = new AggregateError([error, ...rollbackErrors],
                        "managed bind rollback is uncertain");
                    failure.browserCode = "unknown_effect";
                    throw failure;
                }
                throw error;
            }
        } catch (error) {
            const cleanupErrors = [];
            try {
                await this.cleanupFlightWindow(flight);
            } catch (cleanupError) {
                cleanupErrors.push(cleanupError);
            }
            try {
                await this.disownMarker(nonce);
            } catch (cleanupError) {
                cleanupErrors.push(cleanupError);
            }
            if (cleanupErrors.length > 0) {
                const cleanupRecord = {
                    nonce,
                    windowId: flight.createdWindowId,
                    statusTabIds: statusTabs.map(tab => tab.id),
                    pageTabId: null,
                };
                try {
                    await this.recordCleanupTombstone(cleanupRecord, cleanupErrors);
                } catch (tombstoneError) {
                    cleanupErrors.push(tombstoneError);
                }
                const failure = new AggregateError([error, ...cleanupErrors],
                    "managed bind failed and cleanup is uncertain");
                failure.browserCode = "unknown_effect";
                throw failure;
            }
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
                    await this.closeOwnedWindow(record);
                    operation.committed = true;
                    completion = operation.cancelled ? "unknown" : "applied";
                }
            } else if (payload.command.command === "close_window") {
                await this.detach(record.pageTabId);
                if (operation.cancelled) completion = "not_applied";
                else {
                    operation.mutationStarted = true;
                    await this.closeOwnedWindow(record);
                    operation.committed = true;
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
                completion = operation.mutationStarted || operation.committed ||
                    operation.uncertain ? "unknown" : "not_applied";
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
            if (matches(waiter.payload)) {
                cancels.push(this.cancelEnsure(this.ensureIdentity(waiter.payload)));
            }
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
        try {
            await this.chrome.debugger.detach({tabId});
        } catch (error) {
            if (!await this.tabIsMissing(tabId)) throw error;
        }
    }

    forget(record) {
        const key = this.key(record.remoteKey, record.principalId);
        this.workspaces.delete(key);
        this.windowOwners.delete(record.windowId);
        this.tabOwners.delete(record.pageTabId);
        for (const tabId of record.statusTabIds) this.statusTabOwners.delete(tabId);
    }

    async closeOwnedWindow(record) {
        await this.convergeManagedTopology(record, {closeWindow: true, report: false});
    }

    async visibleNormalWindows(removedWindowId) {
        return (await this.chrome.windows.getAll({windowTypes: ["normal"]}))
            .filter(window => window.type === "normal" && window.id !== removedWindowId);
    }

    async replaceClosedHostWindow(removedWindowId) {
        const normalWindows = await this.visibleNormalWindows(removedWindowId);
        if (normalWindows.length === 0 || normalWindows.some(window =>
            !this.windowOwners.has(window.id))) return;
        const intent = await this.recentlyClosedHostIntent(removedWindowId);
        if (!intent) throw new Error("closed Chrome host session was not captured");
        await this.markRestoreHost(intent);
        await this.closeManagedWindowSet(normalWindows);
        await this.chrome.windows.create({url: "about:blank", focused: false, type: "normal"});
    }

    async closeManagedWindowSet(normalWindows) {
        const records = normalWindows.map(window => {
            const key = this.windowOwners.get(window.id);
            const record = key && this.workspaces.get(key);
            return record?.windowId === window.id ? record : null;
        }).filter(record => record !== null);
        if (records.length === 0) return;
        await this.persistWithoutRecords(records);
        for (const record of records) {
            await this.detach(record.pageTabId);
            try {
                await this.chrome.windows.remove(record.windowId);
            } catch (_) {
                if (await this.isLive(record)) throw new Error("managed window did not close");
            }
            await this.disownMarker(record.nonce);
        }
    }

    async windowRemoved(windowId) {
        const key = this.windowOwners.get(windowId);
        if (!key) {
            await this.replaceClosedHostWindow(windowId);
            return;
        }
        const record = this.workspaces.get(key);
        if (!record || record.windowId !== windowId) return;
        await this.convergeManagedTopology(record);
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

    async removeKnownTab(tabId, errors) {
        try {
            await this.chrome.tabs.remove(tabId);
        } catch (error) {
            if (!await this.tabIsMissing(tabId)) errors.push(error);
        }
    }

    async clearManagedX11(record, errors) {
        if (!Number.isInteger(record.x11WindowId) || record.x11WindowId <= 0) return;
        try {
            const result = await this.nativeRequest("abush_browser", "clear_x11_window", {
                xid: record.x11WindowId,
                ...this.bindingPayload(record),
            });
            if (!exactFields(result, ["cleared", "xid", "reclassified_group"]) ||
                    result.cleared !== true || result.xid !== record.x11WindowId ||
                    result.reclassified_group !== "f") {
                throw new Error("native X11 cleanup was not verified");
            }
        } catch (error) {
            errors.push(error);
        }
    }

    async convergeManagedTopology(record, {closeWindow = false, report = true} = {}) {
        const page = this.pageRef(record);
        const live = await this.isLive(record);
        let physicalGone = await this.windowIsMissing(record.windowId);
        const errors = [];
        const persistError = await this.persistOrForget(record);
        if (persistError) errors.push(persistError);
        if (this.workspaces.get(this.key(record.remoteKey, record.principalId)) === record) {
            this.forget(record);
        }
        await this.appendCleanupError(errors, () => this.detach(record.pageTabId));
        await this.appendCleanupError(errors, () => this.disownMarker(record.nonce));
        if (closeWindow && live) {
            try {
                await this.chrome.windows.remove(record.windowId);
            } catch (error) {
                if (!await this.windowIsMissing(record.windowId)) errors.push(error);
            }
            physicalGone = await this.windowIsMissing(record.windowId);
        } else {
            for (const tabId of record.statusTabIds) {
                await this.removeKnownTab(tabId, errors);
            }
            if (closeWindow) await this.removeKnownTab(record.pageTabId, errors);
        }
        if (!physicalGone) await this.clearManagedX11(record, errors);
        let reportError = null;
        if (report) {
            try {
                await this.queueTopologyReport(this.topologyReport(record, page));
                await this.flushTopologyOutbox();
            } catch (error) {
                reportError = error;
            }
        }
        if (errors.length > 0) {
            try {
                await this.recordCleanupTombstone(record, errors);
            } catch (error) {
                errors.push(error);
            }
            throw new AggregateError(errors, "managed topology cleanup was incomplete");
        }
        if (reportError) this.reportAdvisoryError(reportError);
    }

    recordForTopology(tabId, windowId = null) {
        const key = this.tabOwners.get(tabId) || this.statusTabOwners.get(tabId) ||
            this.windowOwners.get(windowId);
        return key ? this.workspaces.get(key) || null : null;
    }

    async reconcileManagedTab(tabId, info = {}, force = false) {
        const windowId = info.windowId ?? info.newWindowId ?? info.oldWindowId ?? null;
        const record = this.recordForTopology(tabId, windowId);
        if (!record || (!force && await this.isLive(record))) return false;
        await this.convergeManagedTopology(record);
        return true;
    }

    async tabCreated(tab) {
        const record = this.recordForTopology(tab.id, tab.windowId);
        if (!record || await this.isLive(record)) return false;
        await this.convergeManagedTopology(record);
        return true;
    }

    async tabUpdated(tabId, changeInfo, tab) {
        const record = this.recordForTopology(tabId, tab?.windowId);
        if (!record) return false;
        const ownerWindow = tab?.windowId === record.windowId;
        const ownedTab = record.pageTabId === tabId || record.statusTabIds.includes(tabId);
        const topologyField = "url" in changeInfo || "title" in changeInfo ||
            "pinned" in changeInfo || Number.isInteger(changeInfo.index) ||
            Number.isInteger(tab?.index);
        if ((!ownerWindow && !ownedTab) || (!ownedTab && !topologyField) ||
                await this.isLive(record)) return false;
        await this.convergeManagedTopology(record);
        return true;
    }

    async tabRemoved(tabId, info) {
        return this.reconcileManagedTab(tabId, info, true);
    }

    async tabDetached(tabId, info) {
        return this.reconcileManagedTab(tabId, info, true);
    }

    async tabAttached(tabId, info) {
        const record = this.recordForTopology(tabId, info.newWindowId);
        const movedOutside = record && info.newWindowId !== record.windowId;
        return this.reconcileManagedTab(tabId, info, movedOutside);
    }

    async tabMoved(tabId, moveInfo) {
        const record = this.recordForTopology(tabId, moveInfo.windowId);
        const movedOutside = record && moveInfo.windowId !== record.windowId;
        return this.reconcileManagedTab(tabId, moveInfo, movedOutside);
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
        ORDINARY_PREFIX, MARKER_IDS, BROWSER_ERROR_CODES};
}
