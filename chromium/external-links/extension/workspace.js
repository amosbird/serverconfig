const STATUS_PREFIX = "__ABUSH_BIND__:";
const ORDINARY_PREFIX = "__ABUSH_ORDINARY__:";
const STATUS_NONCE_RE = /^[0-9a-f]{32}$/;
const MARKER_IDS = ["a", "b"];
const OWNED_MARKERS_KEY = "abushOwnedMarkers";
const TOPOLOGY_OUTBOX_KEY = "abushTopologyOutbox";
const DEFERRED_TOPOLOGY_RESPONSES_KEY = "abushDeferredTopologyResponses";
const CLEANUP_TOMBSTONES_KEY = "abushCleanupTombstones";
const RETIRE_LEDGER_KEY = "abushRetireLedger";
const RESTORE_HOST_KEY = "abushRestoreHost";
const CLOSE_AMBIGUITIES_KEY = "abushCloseAmbiguities";
const MANAGED_CLEANUP_BATCH_KEY = "abushManagedCleanupBatch";
const CURRENT_BROWSER_BOOT_KEY = "abushCurrentBrowserBoot";
const COLD_ORDINARY_LEDGER_KEY = "abushColdOrdinaryLedger";
const PENDING_TRANSACTIONS_KEY = "abushPendingTransactions";
const PENDING_OPERATIONS_KEY = "abushPendingOperations";
const EXECUTION_LEDGER_KEY = "abushExecutionLedger";
const REPLACEMENT_LEDGER_KEY = "abushReplacementLedger";
const TRANSACTION_WAIT_MS = 5000;
const ORDINARY_CLOSE_RETRY_MS = 25;
const ORDINARY_CLOSE_RETRY_ATTEMPTS = 20;
const BROWSER_ERROR_CODES = new Set([
    "unknown_principal", "unknown_connection", "stale_session", "stale_authority",
    "human_controlled", "missing_capability", "capability_grant_mismatch", "backpressure",
    "stale_window", "stale_page", "page_quarantined", "operation_conflict",
    "unknown_effect", "stale_effect", "generation_exhausted", "executor_unavailable",
    "invalid_command",
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
    constructor(chromeApi, nativeRequest, bootstrap = null) {
        this.chrome = chromeApi;
        this.nativeRequest = nativeRequest;
        this.workspaces = new Map();
        this.windowOwners = new Map();
        this.tabOwners = new Map();
        this.statusTabOwners = new Map();
        this.operations = new Map();
        this.completedOperations = new Map();
        this.executionLedger = new Map();
        this.executionFlushTail = Promise.resolve();
        this.executionRegistrationTail = Promise.resolve();
        this.pendingOperationCancels = new Map();
        this.ordinaryOperations = new Map();
        this.completedOrdinaryOperations = new Map();
        this.ordinaryTail = Promise.resolve();
        this.ordinaryWindowMru = new Map();
        this.ordinaryMruClock = 0;
        this.ensureFlights = new Map();
        this.ensureWaiters = new Map();
        this.persistTail = Promise.resolve();
        this.persistRevision = 0;
        this.localStateTail = Promise.resolve();
        this.pendingTransactions = new Map();
        this.eventTransactions = new Map();
        this.eventTransactionClock = 0;
        this.lifecycleTail = Promise.resolve();
        this.lifecycleGeneration = 0;
        this.topologyOutbox = new Map();
        this.outboxTail = Promise.resolve();
        this.retirePending = new Map();
        this.retireEffectStarted = new Map();
        this.retireCompleted = new Map();
        this.retireResponseReceived = new Map();
        this.retireConsumedPending = new Map();
        this.retireServerConfirmed = new Map();
        this.retireLegacyCleanup = new Map();
        this.retireConfirmedIdentities = new Map();
        this.remoteAuthorities = new Map();
        this.deferredTopologyResponses = new Map();
        this.retireRecoveryReady = false;
        this.retireRecoveries = [];
        this.retireRecoveryState = null;
        this.retireFlushTail = Promise.resolve();
        this.nativeMutationTail = Promise.resolve();
        this.advisoryErrors = [];
        this.notifiedAdvisoryErrors = new WeakSet();
        this.onAdvisoryError = null;
        this.removedHostSnapshots = new Map();
        this.snapshotTails = new Map();
        this.windowSnapshotTokens = new Map();
        this.recentSessionIds = null;
        this.ordinaryCloseGeneration = 0;
        this.activeOrdinaryCloseBatch = null;
        this.ordinaryCloseWindowBatches = new Map();
        this.startupManagedRecords = [];
        this.quarantinedCleanupNonces = new Set();
        this.windowRegistry = new Map();
        this.windowRegistryRevision = 0;
        this.closingManagedWindows = new Set();
        this.browserBootId = null;
        this.bootstrapLeases = new Map();
        this.bootstrapLeaseTail = Promise.resolve();
        this.restoreAttempt = null;
        this.restoreGeneration = 0;
        this.bootstrapResolved = false;
        this.bootstrapReady = new Promise((resolve, reject) => {
            this.resolveBootstrap = resolve;
            this.rejectBootstrap = reject;
        });
        if (bootstrap) this.acceptNativeBootstrap(bootstrap).catch(error => {
            this.rejectBootstrap(error);
            this.reportAdvisoryError(error);
        });
    }

    async acceptNativeBootstrap(payload) {
        if (!exactFields(payload, ["browser_boot_id", "browser_pid", "browser_starttime",
            "ordinary_launch_leases"]) || typeof payload.browser_boot_id !== "string" ||
                !payload.browser_boot_id || !Number.isInteger(payload.browser_pid) ||
                payload.browser_pid <= 0 || typeof payload.browser_starttime !== "string" ||
                !/^\d+$/.test(payload.browser_starttime) ||
                !Array.isArray(payload.ordinary_launch_leases)) {
            throw new Error("invalid native browser bootstrap");
        }
        if (this.browserBootId && this.browserBootId !== payload.browser_boot_id) {
            throw new Error("native browser boot identity changed inside one service worker");
        }
        const leases = new Map();
        for (const lease of payload.ordinary_launch_leases) {
            this.validateOrdinaryOpen(lease);
            if (lease.deadline_ms <= Date.now()) continue;
            const previous = leases.get(lease.request_id);
            if (previous && !this.sameOrdinaryPayload(previous, lease)) {
                throw new Error("conflicting native bootstrap ordinary lease");
            }
            leases.set(lease.request_id, structuredClone(lease));
        }
        this.browserBootId = payload.browser_boot_id;
        for (const [requestId, lease] of leases) {
            const previous = this.bootstrapLeases.get(requestId);
            if (previous && !this.sameOrdinaryPayload(previous, lease)) {
                throw new Error("conflicting repeated native bootstrap ordinary lease");
            }
            this.bootstrapLeases.set(requestId, lease);
        }
        if (this.chrome.storage?.session) {
            await this.chrome.storage.session.set({
                [CURRENT_BROWSER_BOOT_KEY]: this.browserBootId,
            });
        }
        if (!this.bootstrapResolved) {
            this.bootstrapResolved = true;
            this.resolveBootstrap();
        }
        if (this.restoreAttempt) {
            await this.restoreAttempt.promise;
            await this.resumeBootstrapLeases();
        }
    }

    resumeBootstrapLeases() {
        const resume = this.bootstrapLeaseTail.catch(() => {}).then(async () => {
            if (this.bootstrapLeases.size === 0) return;
            await this.stageBootstrapLeaseIfNeeded();
            await this.executeBootstrapLeases();
        });
        this.bootstrapLeaseTail = resume;
        return resume;
    }

    async waitForNativeBootstrap() {
        await this.bootstrapReady;
        if (!this.browserBootId) throw new Error("native browser boot identity is unavailable");
    }

    restoreWhenReady() {
        if (this.restoreAttempt) return this.restoreAttempt.promise;
        const generation = ++this.restoreGeneration;
        const attempt = {generation, promise: null};
        attempt.promise = Promise.resolve().then(() => this.restore()).catch(error => {
            if (this.restoreAttempt === attempt) this.restoreAttempt = null;
            throw error;
        });
        this.restoreAttempt = attempt;
        return attempt.promise;
    }

    whenReady() {
        return this.restoreAttempt?.promise || this.restoreWhenReady();
    }

    key(remoteKey, principalId) {
        return `${remoteKey}\0${principalId}`;
    }

    persistedRecord(record) {
        const {debuggerAttached, ...persisted} = record;
        return {...persisted};
    }

    async persistRecords(records, revision = ++this.persistRevision) {
        if (!this.chrome.storage?.session) return revision;
        await this.chrome.storage.session.set({
            abushWorkspaces: records,
            revision,
            abushTransactionRevision: revision,
        });
        return revision;
    }

    recordsMatch(left, right) {
        return sameRef(left, right);
    }

    async durableRecordState(oldRecords, newRecords, revision) {
        if (!this.chrome.storage?.session) return "new";
        try {
            const stored = await this.chrome.storage.session.get([
                "abushWorkspaces", "revision", "abushTransactionRevision",
            ]);
            const records = Array.isArray(stored.abushWorkspaces) ? stored.abushWorkspaces : [];
            if (stored.abushTransactionRevision === revision &&
                    stored.revision === revision && this.recordsMatch(records, newRecords)) {
                return "new";
            }
            if (this.recordsMatch(records, oldRecords)) return "old";
        } catch (error) {
            this.reportAdvisoryError(new AggregateError([error],
                "durable workspace state could not be read after persistence failure"));
        }
        return "unknown";
    }

    enqueueLocalState(task) {
        const operation = this.localStateTail.catch(() => {}).then(task);
        this.localStateTail = operation;
        return operation;
    }

    localStateWrite(key, snapshot) {
        if (!this.chrome.storage?.local) {
            return Promise.reject(new Error("local extension storage is unavailable"));
        }
        return this.enqueueLocalState(async () => {
            const current = typeof snapshot === "function" ? await snapshot() : snapshot;
            await this.chrome.storage.local.set({[key]: structuredClone(current)});
        });
    }

    async localStateUpdate(key, update, fallback) {
        if (!this.chrome.storage?.local) {
            throw new Error("local extension storage is unavailable");
        }
        return this.enqueueLocalState(async () => {
            const stored = await this.chrome.storage.local.get([key]);
            const current = stored[key] === undefined ? fallback : stored[key];
            const next = await update(structuredClone(current));
            await this.chrome.storage.local.set({[key]: structuredClone(next)});
            return next;
        });
    }

    pendingTransactionSnapshot() {
        return [...this.pendingTransactions.values()].map(transaction => ({
            version: 2,
            transaction_id: transaction.id,
            phase: transaction.phase,
            remote_key: transaction.remoteKey,
            principal_id: transaction.principalId,
            nonce: transaction.nonce,
            host_window_id: transaction.hostWindowId,
            window_id: transaction.windowId,
            status_tab_ids: [...transaction.statusTabIds],
            page_tab_id: transaction.pageTabId,
            xid: transaction.x11WindowId,
            binding: transaction.binding,
            record: transaction.record,
            planned_effect: transaction.plannedEffect || null,
            observed_effects: structuredClone(transaction.observedEffects || []),
        }));
    }

    async persistPendingTransactions() {
        await this.localStateWrite(PENDING_TRANSACTIONS_KEY,
            () => this.pendingTransactionSnapshot());
    }

    async beginEnsureTransaction(payload, nonce) {
        const transaction = {
            id: crypto.randomUUID(),
            phase: "prepared",
            remoteKey: payload.remote_key,
            principalId: payload.principal_id,
            nonce,
            hostWindowId: null,
            windowId: null,
            statusTabIds: [],
            pageTabId: null,
            x11WindowId: null,
            binding: null,
            record: null,
            plannedEffect: null,
            observedEffects: [],
            expectedEvents: [],
            invalid: false,
            finished: false,
            clock: ++this.eventTransactionClock,
        };
        this.pendingTransactions.set(transaction.id, transaction);
        this.eventTransactions.set(transaction.id, transaction);
        try {
            await this.persistPendingTransactions();
        } catch (error) {
            this.pendingTransactions.delete(transaction.id);
            this.eventTransactions.delete(transaction.id);
            throw error;
        }
        return transaction;
    }

    async updateEnsureTransaction(transaction, update) {
        Object.assign(transaction, update);
        transaction.clock = ++this.eventTransactionClock;
        await this.persistPendingTransactions();
    }

    async finishEnsureTransaction(transaction, phase = "committed") {
        transaction.phase = phase;
        transaction.finished = true;
        this.pendingTransactions.delete(transaction.id);
        await this.persistPendingTransactions();
        this.pruneEventTransactions();
    }

    pruneEventTransactions() {
        for (const [id, transaction] of this.eventTransactions) {
            if (transaction.finished && transaction.expectedEvents.every(event => event.consumed)) {
                this.eventTransactions.delete(id);
            }
        }
        if (this.eventTransactions.size <= 256) return;
        const finished = [...this.eventTransactions.values()]
            .filter(transaction => transaction.finished)
            .sort((left, right) => left.clock - right.clock);
        for (const transaction of finished) {
            if (this.eventTransactions.size <= 256) break;
            this.eventTransactions.delete(transaction.id);
        }
    }

    expectTransactionEvent(transaction, type, match) {
        const expected = {type, match, consumed: false};
        transaction.expectedEvents.push(expected);
        transaction.clock = ++this.eventTransactionClock;
        return expected;
    }

    eventValueMatches(actual, expected) {
        if (expected && typeof expected === "object" && !Array.isArray(expected)) {
            return actual && typeof actual === "object" &&
                Object.entries(expected).every(([key, value]) =>
                    this.eventValueMatches(actual[key], value));
        }
        return actual === expected;
    }

    transactionTouchesEvent(transaction, payload) {
        const tabIds = new Set([
            ...transaction.statusTabIds,
            transaction.pageTabId,
        ].filter(Number.isInteger));
        const eventTabIds = [payload?.tabId, payload?.removedTabId, payload?.addedTabId,
            payload?.tab?.id].filter(Number.isInteger);
        if (eventTabIds.some(tabId => tabIds.has(tabId))) return true;
        const eventWindowIds = [payload?.windowId, payload?.oldWindowId, payload?.newWindowId,
            payload?.tab?.windowId, payload?.window?.id].filter(Number.isInteger);
        if (Number.isInteger(transaction.windowId) &&
                eventWindowIds.includes(transaction.windowId)) return true;
        if (Number.isInteger(transaction.hostWindowId) &&
                eventWindowIds.includes(transaction.hostWindowId) && eventTabIds.length > 0) {
            return true;
        }
        const url = payload?.tab?.url ?? payload?.changeInfo?.url;
        return typeof url === "string" && url.includes(`nonce=${transaction.nonce}`);
    }

    consumeTransactionEvent(transaction, expected, type, payload) {
        expected.consumed = true;
        transaction.clock = ++this.eventTransactionClock;
        if (type === "windowCreated" && Number.isInteger(payload?.window?.id)) {
            transaction.windowId = payload.window.id;
        }
        this.pruneEventTransactions();
    }

    transactionAllowsBenignEvent(transaction, type, payload) {
        if (type === "tabCreated") {
            const tab = payload?.tab;
            const planned = transaction.plannedEffect;
            if (!Number.isInteger(tab?.id) || !planned ||
                    !["tabs.create.marker", "tabs.create.business"].includes(planned.kind) ||
                    tab.windowId !== planned.window_id) return false;
            planned.tab_id = tab.id;
            if (typeof tab.url !== "string") return true;
            if (planned.kind === "tabs.create.marker") {
                const marker = this.parseStatusUrl(tab.url);
                return marker?.nonce === transaction.nonce && marker.marker === planned.marker;
            }
            return tab.url === "about:blank";
        }
        if (type === "windowCreated") return false;
        if (type !== "tabUpdated") return false;
        const tabId = payload?.tabId ?? payload?.tab?.id;
        if (!Number.isInteger(tabId)) return false;
        const transactionTab = transaction.statusTabIds.includes(tabId) ||
            transaction.pageTabId === tabId || transaction.plannedEffect?.tab_id === tabId;
        if (!transactionTab) return false;
        const fields = Object.keys(payload?.changeInfo || {});
        if (!fields.every(field => ["status", "title", "favIconUrl", "audible",
            "discarded", "autoDiscardable", "mutedInfo", "groupId", "url", "pinned", "index"]
            .includes(field))) return false;
        const change = payload.changeInfo || {};
        if ("url" in change) {
            const parsed = this.parseStatusUrl(change.url);
            const plannedBusiness = transaction.plannedEffect?.kind === "tabs.create.business" &&
                change.url === "about:blank";
            if (!(parsed?.nonce === transaction.nonce || plannedBusiness)) return false;
        }
        if ("pinned" in change) {
            const plannedPin = transaction.plannedEffect?.kind === "tabs.pin" &&
                transaction.plannedEffect.tab_id === tabId && change.pinned === true;
            const expectedPinned = transaction.statusTabIds.includes(tabId);
            if (!plannedPin && change.pinned !== expectedPinned) return false;
        }
        if ("index" in change) {
            const expectedIndex = transaction.statusTabIds.indexOf(tabId);
            if (expectedIndex < 0 || change.index !== expectedIndex) return false;
        }
        if (Number.isInteger(payload?.tab?.index)) {
            const expectedIndex = transaction.statusTabIds.indexOf(tabId);
            const businessPlanned = transaction.plannedEffect?.kind === "tabs.create.business" &&
                transaction.pageTabId === tabId && payload.tab.index === 2;
            if (!businessPlanned && expectedIndex >= 0 && payload.tab.index !== expectedIndex) {
                return false;
            }
        }
        return true;
    }

    async reconcileTransactionTopology(transaction) {
        const candidateWindowIds = new Set([transaction.windowId,
            transaction.plannedEffect?.window_id].filter(Number.isInteger));
        if (transaction.plannedEffect?.kind === "windows.create" ||
                transaction.windowId === null) {
            const tabs = await this.chrome.tabs.query({});
            for (const tab of tabs) {
                const marker = this.parseStatusUrl(tab.url);
                if (marker?.nonce === transaction.nonce) candidateWindowIds.add(tab.windowId);
            }
        }
        const matches = [];
        for (const windowId of candidateWindowIds) {
            let tabs;
            try {
                tabs = await this.chrome.tabs.query({windowId});
            } catch (_) {
                continue;
            }
            const markers = tabs.map(tab => ({tab, marker: this.parseStatusUrl(tab.url)}))
                .filter(item => item.marker?.nonce === transaction.nonce);
            if (markers.length > 0) matches.push({windowId, tabs, markers});
        }
        if (matches.length > 1) throw new Error("managed transaction marker is ambiguous");
        const match = matches[0];
        if (!match) return null;
        transaction.windowId = match.windowId;
        transaction.statusTabIds = MARKER_IDS.map(marker => match.markers.find(item =>
            item.marker.marker === marker)?.tab.id).filter(Number.isInteger);
        if (!Number.isInteger(transaction.pageTabId)) {
            const business = match.tabs.filter(tab =>
                !transaction.statusTabIds.includes(tab.id) && tab.pinned !== true);
            if (business.length === 1) transaction.pageTabId = business[0].id;
        }
        return match;
    }

    observeChromeEvent(type, payload) {
        const transactions = [...this.eventTransactions.values()]
            .sort((left, right) => right.clock - left.clock);
        for (const transaction of transactions) {
            const expected = transaction.expectedEvents.find(event => !event.consumed &&
                event.type === type && this.eventValueMatches(payload, event.match));
            if (expected) {
                this.consumeTransactionEvent(transaction, expected, type, payload);
                return true;
            }
            if (this.transactionAllowsBenignEvent(transaction, type, payload)) return true;
            if (!transaction.finished && this.transactionTouchesEvent(transaction, payload)) {
                transaction.invalid = true;
                transaction.clock = ++this.eventTransactionClock;
            }
        }
        return false;
    }

    async observeWindowCreated(window) {
        let tabs = Array.isArray(window?.tabs) ? window.tabs : null;
        if (tabs === null && Number.isInteger(window?.id)) {
            try {
                tabs = await this.chrome.tabs.query({windowId: window.id});
            } catch (error) {
                this.reportAdvisoryError(new AggregateError([error],
                    "created window could not be reconciled with its transaction"));
                return false;
            }
        }
        const tabIds = new Set((tabs || []).map(tab => tab.id).filter(Number.isInteger));
        const transactions = [...this.eventTransactions.values()]
            .sort((left, right) => right.clock - left.clock);
        for (const transaction of transactions) {
            const expected = transaction.expectedEvents.find(event => !event.consumed &&
                event.type === "windowCreated" &&
                this.eventValueMatches(window, event.match.window) &&
                tabIds.has(event.match.sourceTabId));
            if (!expected) continue;
            this.consumeTransactionEvent(transaction, expected, "windowCreated", {window});
            return true;
        }
        return false;
    }

    transactionCleanupRecord(transaction) {
        const record = transaction.record || {};
        return {
            nonce: transaction.nonce,
            windowId: transaction.windowId,
            x11WindowId: transaction.x11WindowId,
            statusTabIds: [...transaction.statusTabIds],
            pageTabId: transaction.pageTabId,
            remoteKey: record.remoteKey || transaction.binding?.remote_key,
            workspaceId: record.workspaceId || transaction.binding?.workspace_id,
            windowGeneration: record.windowGeneration || transaction.binding?.generation,
            connectionDisplayName: record.connectionDisplayName ||
                transaction.binding?.display_title,
        };
    }

    validPendingTransaction(transaction) {
        return transaction?.version >= 1 && transaction?.version <= 2 &&
            typeof transaction.transaction_id === "string" && transaction.transaction_id &&
            typeof transaction.nonce === "string" && STATUS_NONCE_RE.test(transaction.nonce) &&
            Array.isArray(transaction.status_tab_ids) &&
            transaction.status_tab_ids.every(Number.isInteger) &&
            (transaction.window_id === null || Number.isInteger(transaction.window_id)) &&
            (transaction.page_tab_id === null || Number.isInteger(transaction.page_tab_id)) &&
            (transaction.xid === null || Number.isInteger(transaction.xid));
    }

    transactionFromPersisted(saved) {
        return {
            id: saved.transaction_id,
            phase: saved.phase,
            remoteKey: saved.remote_key,
            principalId: saved.principal_id,
            nonce: saved.nonce,
            hostWindowId: saved.host_window_id,
            windowId: saved.window_id,
            statusTabIds: [...saved.status_tab_ids],
            pageTabId: saved.page_tab_id,
            x11WindowId: saved.xid,
            binding: saved.binding || null,
            record: saved.record || null,
            plannedEffect: saved.planned_effect || null,
            observedEffects: Array.isArray(saved.observed_effects) ?
                structuredClone(saved.observed_effects) : [],
            expectedEvents: [],
            invalid: false,
            finished: false,
            clock: ++this.eventTransactionClock,
        };
    }

    transactionMatchesRecord(transaction, record) {
        return transaction.nonce === record.nonce && transaction.windowId === record.windowId &&
            transaction.pageTabId === record.pageTabId &&
            sameRef(transaction.statusTabIds, record.statusTabIds) &&
            (!Number.isInteger(transaction.x11WindowId) ||
                transaction.x11WindowId === record.x11WindowId);
    }

    async retryPendingTransaction(transaction) {
        await this.reconcileTransactionTopology(transaction);
        const record = this.transactionCleanupRecord(transaction);
        const tombstone = this.cleanupTombstone(record, []);
        await this.retryCleanupTombstone(tombstone);
    }

    async restorePendingTransactions() {
        if (!this.chrome.storage?.local) return;
        const [local, session] = await Promise.all([
            this.chrome.storage.local.get([PENDING_TRANSACTIONS_KEY]),
            this.chrome.storage?.session ?
                this.chrome.storage.session.get(["abushWorkspaces"]) :
                Promise.resolve({abushWorkspaces: []}),
        ]);
        const saved = Array.isArray(local[PENDING_TRANSACTIONS_KEY]) ?
            local[PENDING_TRANSACTIONS_KEY] : [];
        const records = Array.isArray(session.abushWorkspaces) ? session.abushWorkspaces : [];
        const failures = [];
        for (const persisted of saved) {
            if (!this.validPendingTransaction(persisted)) continue;
            const transaction = this.transactionFromPersisted(persisted);
            this.pendingTransactions.set(transaction.id, transaction);
            this.eventTransactions.set(transaction.id, transaction);
            let match = null;
            try {
                match = await this.reconcileTransactionTopology(transaction);
            } catch (error) {
                failures.push(error);
                continue;
            }
            const committed = records.find(record =>
                this.transactionMatchesRecord(transaction, record));
            if (committed && match && await this.isLive(committed)) {
                try {
                    const ownership = await this.currentOwnedMarker(committed.nonce);
                    if (!this.savedOwnershipMatches(ownership, committed)) {
                        await this.claimMarker(committed);
                    }
                    this.pendingTransactions.delete(transaction.id);
                    this.eventTransactions.delete(transaction.id);
                    continue;
                } catch (error) {
                    failures.push(error);
                    continue;
                }
            }
            try {
                await this.retryPendingTransaction(transaction);
                this.pendingTransactions.delete(transaction.id);
            } catch (error) {
                failures.push(error);
            }
        }
        await this.persistPendingTransactions();
        if (failures.length > 0) {
            this.reportAdvisoryError(new AggregateError(
                failures, "managed transactions remain pending cleanup"));
        }
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
        await this.persistWithoutRecords([record], {forget: false});
    }

    async persistOrForget(record) {
        try {
            await this.persistWithRemovedRecord(record);
            return null;
        } catch (error) {
            return error;
        }
    }

    enqueueLifecycle(task) {
        const generation = ++this.lifecycleGeneration;
        const operation = this.lifecycleTail.catch(() => {}).then(() => task(generation));
        this.lifecycleTail = operation;
        return operation;
    }

    async persistWithoutRecords(removed, {forget = true} = {}) {
        const removedRecords = new Set(removed);
        const write = this.persistTail.catch(() => {}).then(async () => {
            const records = [...this.workspaces.values()]
                .filter(saved => !removedRecords.has(saved))
                .map(saved => this.persistedRecord(saved));
            await this.persistRecords(records);
            if (forget) {
                for (const record of removedRecords) {
                    const key = this.key(record.remoteKey, record.principalId);
                    if (this.workspaces.get(key) === record) {
                        this.forget(record);
                    }
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
            report_id: crypto.randomUUID(),
            request_id: crypto.randomUUID(),
            executor_fence: record.executorFence,
            origin_executor_fence: record.executorFence,
            daemon_instance_id: record.ownerDisplay?.daemon_instance_id || null,
            page,
            phase: "pending",
            response: null,
            consumed_request_id: null,
            consumed_migration: null,
            cleanup: {
                phase: "queued",
                nonce: record.nonce,
                window_id: record.windowId,
                xid: record.x11WindowId,
                status_tab_ids: [...record.statusTabIds],
                page_tab_id: record.pageTabId,
            },
        };
    }

    topologyReportKey(report) {
        return report.report_id || report.request_id;
    }

    topologyWireReport(report) {
        return {
            remote_key: report.remote_key,
            connection_id: report.connection_id,
            principal_id: report.principal_id,
            request_id: report.request_id,
            report_id: report.report_id,
            executor_fence: report.executor_fence,
            origin_executor_fence: report.origin_executor_fence,
            page: report.page,
        };
    }

    async persistTopologyOutbox() {
        await this.localStateWrite(TOPOLOGY_OUTBOX_KEY,
            () => [...this.topologyOutbox.values()]);
    }

    async queueTopologyReport(report) {
        const key = this.topologyReportKey(report);
        const existing = this.topologyOutbox.get(key);
        if (existing && !sameRef(existing, report)) {
            throw new Error("conflicting duplicate topology request ID");
        }
        this.topologyOutbox.set(key, existing || report);
        try {
            await this.persistTopologyOutbox();
        } catch (error) {
            this.topologyOutbox.set(key, existing || report);
            throw error;
        }
        return key;
    }

    validTopologyResult(result) {
        if (exactFields(result, ["outcome"])) {
            return ["applied", "already_converged"].includes(result.outcome);
        }
        return exactFields(result, ["outcome", "code"]) &&
            result.outcome === "rejected" && BROWSER_ERROR_CODES.has(result.code);
    }

    validTopologyResponse(response, report, requireRequest = true) {
        return exactFields(response, ["request_id", "report_id", "executor_fence",
            "response_delivery_id", "result"]) &&
            (!requireRequest || response.request_id === report.request_id) &&
            response.report_id === report.report_id &&
            sameRef(response.executor_fence, report.executor_fence) &&
            typeof response.response_delivery_id === "string" &&
            response.response_delivery_id.length > 0 &&
            this.validTopologyResult(response.result);
    }

    topologyConsumedPayload(report) {
        return {
            remote_key: report.remote_key,
            connection_id: report.connection_id,
            principal_id: report.principal_id,
            request_id: report.consumed_request_id,
            report_id: report.report_id,
            executor_fence: report.executor_fence,
            response_delivery_id: report.response.response_delivery_id,
        };
    }

    async recordTopologyResponse(key, report, response) {
        const current = this.topologyOutbox.get(key);
        if (current && ["response_received", "consumed_pending",
            "server_confirmed"].includes(current.phase)) {
            const sameCanonical = current.response.response_delivery_id ===
                response.response_delivery_id &&
                sameRef(current.response.result, response.result) &&
                (sameRef(response.executor_fence, current.response.executor_fence) ||
                    sameRef(response.executor_fence, current.executor_fence));
            if (!sameCanonical) throw new Error("conflicting canonical topology response");
            return current;
        } else if (current && !sameRef(current, report)) {
            throw new Error("stale topology response delivery");
        }
        const received = {
            ...report,
            request_id: response.request_id,
            executor_fence: response.executor_fence,
            phase: "response_received",
            response,
            consumed_request_id: null,
            consumed_migration: report.consumed_migration || null,
        };
        this.topologyOutbox.set(key, received);
        await this.persistTopologyOutbox();
        return received;
    }

    async consumeTopologyResponse(key, original) {
        if (this.topologyOutbox.get(key) !== original) return;
        let report = original;
        if (report.phase === "response_received") {
            report = {...report, phase: "consumed_pending",
                consumed_request_id: crypto.randomUUID(), consumed_migration: null};
            this.topologyOutbox.set(key, report);
            await this.persistTopologyOutbox();
        }
        if (report.phase !== "consumed_pending" ||
                typeof report.consumed_request_id !== "string" ||
                !report.consumed_request_id) {
            throw new Error("managed topology consumption has no durable request ID");
        }
        const confirmation = await this.nativeRequest(
            "abush_browser", "topology_response_consumed",
            this.topologyConsumedPayload(report));
        if (!exactFields(confirmation, ["request_id", "report_id", "executor_fence",
            "response_delivery_id"]) ||
                confirmation.request_id !== report.consumed_request_id ||
                confirmation.report_id !== report.report_id ||
                !sameRef(confirmation.executor_fence, report.executor_fence) ||
                confirmation.response_delivery_id !== report.response.response_delivery_id) {
            throw new Error("managed topology consumption was not confirmed");
        }
        if (this.topologyOutbox.get(key) !== report) return;
        const confirmed = {...report, phase: "server_confirmed"};
        this.topologyOutbox.set(key, confirmed);
        await this.persistTopologyOutbox();
        await this.removeConfirmedTopology(key, confirmed);
    }

    async removeConfirmedTopology(key, confirmed) {
        if (this.topologyOutbox.get(key) !== confirmed ||
                confirmed.phase !== "server_confirmed") return;
        await this.persistTopologyOutbox();
        if (this.topologyOutbox.get(key) !== confirmed) return;
        this.topologyOutbox.delete(key);
        try {
            await this.persistTopologyOutbox();
        } catch (error) {
            this.topologyOutbox.set(key, confirmed);
            throw error;
        }
    }

    async flushTopologyOutbox() {
        const flush = this.outboxTail.catch(() => {}).then(async () => {
            await this.convergeRestoredTopologyOutbox();
            const failures = [];
            const snapshot = [...this.topologyOutbox];
            for (const [key, original] of snapshot) {
                if (!this.topologyOutbox.has(key)) continue;
                let report = this.topologyOutbox.get(key) || original;
                try {
                    if (report.phase === "server_confirmed") {
                        await this.removeConfirmedTopology(key, report);
                        continue;
                    }
                    if (!["response_received", "consumed_pending"].includes(report.phase)) {
                        report = {...report, request_id: crypto.randomUUID(), phase: "pending"};
                        this.topologyOutbox.set(key, report);
                        await this.persistTopologyOutbox();
                        const response = await this.nativeRequest(
                            "abush_browser", "managed_page_moved_outside",
                            this.topologyWireReport(report));
                        if (!this.validTopologyResponse(response, report)) {
                            throw new Error("managed topology response was not canonical");
                        }
                        report = await this.recordTopologyResponse(key, report, response);
                    }
                    await this.consumeTopologyResponse(key, report);
                } catch (error) {
                    const current = this.topologyOutbox.get(key);
                    if (current) report = current;
                    this.topologyOutbox.set(key, report);
                    try {
                        await this.persistTopologyOutbox();
                    } catch (persistError) {
                        failures.push(new AggregateError([error, persistError],
                            "topology report and outbox persistence failed"));
                        continue;
                    }
                    failures.push(error);
                }
            }
            if (failures.length > 0) {
                throw new AggregateError(failures, "managed topology report is pending retry");
            }
        });
        this.outboxTail = flush;
        await flush;
    }

    async convergeRestoredTopologyOutbox() {
        for (const [key, report] of this.topologyOutbox) {
            if (!report.cleanup || report.cleanup.phase === "removed") continue;
            const record = [...this.workspaces.values()].find(candidate =>
                sameRef(this.pageRef(candidate), report.page));
            if (!record) {
                report.cleanup = {...report.cleanup, phase: "removed"};
                continue;
            }
            const errors = [];
            const persistError = await this.persistOrForget(record);
            if (persistError) throw persistError;
            await this.appendCleanupError(errors, () => this.detach(record.pageTabId));
            const physicalGone = await this.windowIsMissing(record.windowId);
            if (!physicalGone) {
                for (const tabId of record.statusTabIds) {
                    await this.removeKnownTab(tabId, errors);
                }
                await this.clearManagedX11(record, errors);
            }
            if (errors.length === 0) {
                await this.appendCleanupError(errors, () => this.disownMarker(record.nonce));
            }
            if (errors.length > 0) {
                await this.recordCleanupTombstone(record, errors);
                throw new AggregateError(errors, "restored topology cleanup is incomplete");
            }
            report.cleanup = {...report.cleanup, phase: "removed"};
            this.topologyOutbox.set(key, report);
            await this.persistTopologyOutbox();
        }
    }

    async restoreTopologyOutbox() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([TOPOLOGY_OUTBOX_KEY]);
        const reports = local[TOPOLOGY_OUTBOX_KEY];
        if (!Array.isArray(reports)) return;
        let dirty = false;
        for (const report of reports) {
            const legacyV6 = exactFields(report, ["remote_key", "connection_id", "principal_id",
                "request_id", "executor_fence", "page"]);
            const current = exactFields(report, ["remote_key", "connection_id", "principal_id",
                "report_id", "request_id", "executor_fence", "origin_executor_fence",
                "daemon_instance_id", "page", "cleanup", "phase", "response",
                "consumed_request_id", "consumed_migration"]);
            const previousV8 = exactFields(report, ["remote_key", "connection_id", "principal_id",
                "report_id", "request_id", "executor_fence", "origin_executor_fence",
                "daemon_instance_id", "page", "cleanup", "phase", "response",
                "consumed_request_id"]);
            const previousV7 = exactFields(report, ["remote_key", "connection_id", "principal_id",
                "report_id", "request_id", "executor_fence", "origin_executor_fence",
                "daemon_instance_id", "page", "cleanup", "phase", "response"]);
            const prePhaseV7 = exactFields(report, ["remote_key", "connection_id", "principal_id",
                "report_id", "request_id", "executor_fence", "origin_executor_fence",
                "daemon_instance_id", "page", "cleanup"]);
            if ((legacyV6 || current || previousV8 || previousV7 || prePhaseV7) &&
                    validPageRef(report.page) &&
                    validFence(report.executor_fence) &&
                    [report.remote_key, report.connection_id, report.principal_id,
                        report.request_id].every(value => typeof value === "string" && value)) {
                const restored = legacyV6 ? {
                    ...report,
                    report_id: report.request_id,
                    origin_executor_fence: report.executor_fence,
                    daemon_instance_id: null,
                    cleanup: null,
                    phase: "pending",
                    response: null,
                    consumed_request_id: null,
                    consumed_migration: null,
                } : prePhaseV7 ? {...report, phase: "pending", response: null,
                    consumed_request_id: null, consumed_migration: null} : previousV7 ? {...report,
                    consumed_request_id: null,
                    consumed_migration: report.phase === "response_received" ?
                        "legacy_unsent" : null,
                } : previousV8 ? {...report, consumed_migration: null} : report;
                const responsePhase = ["response_received", "consumed_pending",
                    "server_confirmed"].includes(restored.phase);
                const validResponse = !responsePhase ||
                    this.validTopologyResponse(restored.response, restored, false);
                const validConsumedRequest = ["pending", "response_received"].includes(
                    restored.phase) ? restored.consumed_request_id === null :
                    typeof restored.consumed_request_id === "string" &&
                    restored.consumed_request_id.length > 0;
                const validConsumedMigration = restored.consumed_migration === null ||
                    (restored.phase === "response_received" &&
                        restored.consumed_migration === "legacy_unsent");
                if (!validFence(restored.origin_executor_fence) ||
                        !["pending", "response_received", "consumed_pending",
                            "server_confirmed"].includes(restored.phase) ||
                        !validResponse || !validConsumedRequest || !validConsumedMigration) {
                    dirty = true;
                    continue;
                }
                const key = this.topologyReportKey(restored);
                const existing = this.topologyOutbox.get(key);
                if (!existing) this.topologyOutbox.set(key, restored);
                else dirty = true;
            } else {
                dirty = true;
            }
        }
        if (dirty) await this.persistTopologyOutbox();
    }

    async authorizeTopologyOutbox(state) {
        if (!state?.connected || typeof state.remote_key !== "string" ||
                typeof state.connection_id !== "string" ||
                typeof state.principal_id !== "string" || !validFence(state.executor_fence)) {
            return false;
        }
        let changed = false;
        for (const [key, report] of this.topologyOutbox) {
            if (report.remote_key !== state.remote_key ||
                    report.principal_id !== state.principal_id ||
                    (report.daemon_instance_id && state.owner_display?.daemon_instance_id !==
                        report.daemon_instance_id)) continue;
            const authorityChanged = report.connection_id !== state.connection_id ||
                !sameRef(report.executor_fence, state.executor_fence);
            const needsDeliveryRotation = authorityChanged &&
                !["response_received", "consumed_pending", "server_confirmed"]
                    .includes(report.phase);
            const next = {
                ...report,
                connection_id: state.connection_id,
                executor_fence: state.executor_fence,
                request_id: needsDeliveryRotation ? crypto.randomUUID() : report.request_id,
                phase: needsDeliveryRotation ? "pending" : report.phase,
                response: needsDeliveryRotation ? null : report.response,
                consumed_request_id: report.consumed_request_id,
            };
            this.topologyOutbox.set(key, next);
            changed = true;
        }
        if (changed) await this.persistTopologyOutbox();
        return changed;
    }

    async persistDeferredTopologyResponses() {
        await this.localStateWrite(DEFERRED_TOPOLOGY_RESPONSES_KEY,
            () => [...this.deferredTopologyResponses.values()]);
    }

    async restoreDeferredTopologyResponses() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([DEFERRED_TOPOLOGY_RESPONSES_KEY]);
        const responses = Array.isArray(local[DEFERRED_TOPOLOGY_RESPONSES_KEY]) ?
            local[DEFERRED_TOPOLOGY_RESPONSES_KEY] : [];
        let dirty = false;
        for (const payload of responses) {
            const report = this.topologyOutbox.get(payload?.report_id);
            if (!report || !exactFields(payload, ["remote_key", "connection_id", "principal_id",
                "request_id", "report_id", "executor_fence", "response_delivery_id", "result"]) ||
                    payload.remote_key !== report.remote_key ||
                    payload.principal_id !== report.principal_id ||
                    typeof payload.request_id !== "string" || !payload.request_id ||
                    typeof payload.response_delivery_id !== "string" ||
                    !payload.response_delivery_id || !validFence(payload.executor_fence) ||
                    !this.validTopologyResult(payload.result) ||
                    this.deferredTopologyResponses.has(payload.report_id)) {
                dirty = true;
                continue;
            }
            this.deferredTopologyResponses.set(payload.report_id, payload);
        }
        if (dirty) await this.persistDeferredTopologyResponses();
    }

    topologyResponseMatchesAuthority(payload, state) {
        return payload.remote_key === state.remote_key &&
            payload.principal_id === state.principal_id &&
            payload.connection_id === state.connection_id &&
            sameRef(payload.executor_fence, state.executor_fence);
    }

    async deferTopologyResponse(payload) {
        const existing = this.deferredTopologyResponses.get(payload.report_id);
        if (existing && !sameRef(existing, payload)) {
            throw new Error("conflicting deferred topology response");
        }
        if (existing) return;
        this.deferredTopologyResponses.set(payload.report_id, structuredClone(payload));
        try {
            await this.persistDeferredTopologyResponses();
        } catch (error) {
            this.deferredTopologyResponses.delete(payload.report_id);
            throw error;
        }
    }

    async processDeferredTopologyResponses(state) {
        if (!state?.connected) return;
        for (const [reportId, payload] of [...this.deferredTopologyResponses]) {
            if (!this.topologyResponseMatchesAuthority(payload, state)) continue;
            this.deferredTopologyResponses.delete(reportId);
            try {
                await this.persistDeferredTopologyResponses();
                if (!await this.topologyReportResponse(payload)) {
                    throw new Error("deferred topology response no longer matches its report");
                }
            } catch (error) {
                this.deferredTopologyResponses.set(reportId, payload);
                try {
                    await this.persistDeferredTopologyResponses();
                } catch (persistError) {
                    throw new AggregateError([error, persistError],
                        "deferred topology response and persistence both failed");
                }
                throw error;
            }
        }
    }

    async topologyReportResponse(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "report_id", "executor_fence", "response_delivery_id", "result"])) return false;
        const key = payload.report_id;
        const report = this.topologyOutbox.get(key);
        if (!report || payload.remote_key !== report.remote_key ||
                payload.principal_id !== report.principal_id) return false;
        const response = {
            request_id: payload.request_id,
            report_id: payload.report_id,
            executor_fence: payload.executor_fence,
            response_delivery_id: payload.response_delivery_id,
            result: payload.result,
        };
        if (typeof response.request_id !== "string" || !response.request_id ||
                typeof response.response_delivery_id !== "string" ||
                !response.response_delivery_id || !validFence(response.executor_fence) ||
                !this.validTopologyResult(response.result)) return false;
        const authority = this.remoteAuthorities.get(payload.remote_key);
        if (!authority || !this.topologyResponseMatchesAuthority(payload, authority)) {
            await this.deferTopologyResponse(payload);
            return true;
        }
        if (!this.validTopologyResponse(response, report, false)) return false;
        if (report.connection_id !== payload.connection_id ||
                !sameRef(report.executor_fence, payload.executor_fence)) {
            await this.authorizeTopologyOutbox(authority);
        }
        const authorized = this.topologyOutbox.get(key);
        const received = await this.recordTopologyResponse(key, authorized, response);
        await this.consumeTopologyResponse(key, received);
        return true;
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

    cleanupOwnership(record) {
        return {
            version: 2,
            nonce: record.nonce ?? null,
            window_id: record.windowId ?? null,
            status_tab_ids: Array.isArray(record.statusTabIds) ? [...record.statusTabIds] : [],
            page_tab_id: record.pageTabId ?? null,
            xid: record.x11WindowId ?? null,
            logical_window_id: record.logicalWindowId ?? null,
            logical_page_id: record.logicalPageId ?? null,
            window_generation: record.windowGeneration ?? record.generation ?? null,
            page_generation: record.pageGeneration ?? record.generation ?? null,
            binding: record.remoteKey && record.workspaceId &&
                Number.isInteger(record.windowGeneration ?? record.generation) ?
                this.bindingPayload(record) : null,
        };
    }

    cleanupTombstone(record, errors) {
        return {
            version: 1,
            nonce: record.nonce ?? null,
            window_id: record.windowId ?? null,
            xid: record.x11WindowId ?? null,
            status_tab_ids: Array.isArray(record.statusTabIds) ? [...record.statusTabIds] : [],
            page_tab_id: record.pageTabId ?? null,
            binding: record.remoteKey && record.workspaceId &&
                Number.isInteger(record.windowGeneration ?? record.generation) ?
                this.bindingPayload(record) : null,
            ownership: record.nonce && record.windowId !== undefined ?
                this.cleanupOwnership(record) : null,
            errors: errors.map(error => error?.message || String(error)),
        };
    }

    migrateCleanupTombstone(tombstone) {
        if (this.validCleanupTombstone(tombstone)) return tombstone;
        if (!tombstone || typeof tombstone !== "object" ||
                typeof tombstone.nonce !== "string" ||
                !Array.isArray(tombstone.status_tab_ids) ||
                !tombstone.status_tab_ids.every(Number.isInteger) ||
                !Number.isInteger(tombstone.window_id)) return null;
        return {
            version: 1,
            nonce: tombstone.nonce,
            window_id: tombstone.window_id,
            xid: Number.isInteger(tombstone.xid) ? tombstone.xid : null,
            status_tab_ids: [...tombstone.status_tab_ids],
            page_tab_id: Number.isInteger(tombstone.page_tab_id) ?
                tombstone.page_tab_id : null,
            binding: tombstone.binding || null,
            ownership: tombstone.ownership || null,
            legacy_proof: {
                version: 0,
                nonce: tombstone.nonce,
                window_id: tombstone.window_id,
                status_tab_ids: [...tombstone.status_tab_ids],
                page_tab_id: Number.isInteger(tombstone.page_tab_id) ?
                    tombstone.page_tab_id : null,
            },
            errors: Array.isArray(tombstone.errors) ? [...tombstone.errors] : [],
        };
    }

    tombstoneKey(tombstone) {
        return JSON.stringify([tombstone.nonce, tombstone.window_id, tombstone.xid,
            tombstone.status_tab_ids, tombstone.page_tab_id, tombstone.binding]);
    }

    async recordCleanupTombstone(record, errors) {
        await this.localStateUpdate(CLEANUP_TOMBSTONES_KEY, tombstones => {
            const current = Array.isArray(tombstones) ? tombstones : [];
            const tombstone = this.cleanupTombstone(record, errors);
            const key = this.tombstoneKey(tombstone);
            const next = current.filter(saved => this.tombstoneKey(saved) !== key);
            next.push(tombstone);
            return next.slice(-64);
        }, []);
    }

    validCleanupTombstone(tombstone) {
        const allowed = new Set(["version", "nonce", "window_id", "xid", "status_tab_ids",
            "page_tab_id", "binding", "ownership", "errors", "legacy_proof"]);
        return tombstone && tombstone.version === 1 &&
            Object.keys(tombstone).every(key => allowed.has(key)) &&
            (tombstone.nonce === null || typeof tombstone.nonce === "string") &&
            (tombstone.window_id === null || Number.isInteger(tombstone.window_id)) &&
            (tombstone.xid === null || Number.isInteger(tombstone.xid)) &&
            Array.isArray(tombstone.status_tab_ids) &&
            tombstone.status_tab_ids.every(Number.isInteger) &&
            (tombstone.page_tab_id === null || Number.isInteger(tombstone.page_tab_id)) &&
            (tombstone.binding === null || exactFields(tombstone.binding,
                ["nonce", "remote_key", "workspace_id", "generation", "control_state",
                    "display_title"])) &&
            (tombstone.ownership === undefined || tombstone.ownership === null ||
                tombstone.ownership.version === 2) &&
            (tombstone.legacy_proof === undefined || tombstone.legacy_proof === null ||
                tombstone.legacy_proof.version === 0);
    }

    async cleanupWindowFingerprint(proof) {
        if (!proof || typeof proof.nonce !== "string" ||
                !Array.isArray(proof.status_tab_ids) || proof.status_tab_ids.length !== 2 ||
                !proof.status_tab_ids.every(Number.isInteger) ||
                !Number.isInteger(proof.page_tab_id) || !Number.isInteger(proof.window_id)) {
            return null;
        }
        let window;
        let tabs;
        try {
            [window, tabs] = await Promise.all([
                this.chrome.windows.get(proof.window_id),
                this.chrome.tabs.query({windowId: proof.window_id}),
            ]);
        } catch (_) {
            return null;
        }
        if (window.type !== "normal" || tabs.length !== 3) return null;
        const markers = MARKER_IDS.map((marker, index) => tabs.find(tab =>
            tab.id === proof.status_tab_ids[index] && tab.windowId === proof.window_id &&
            tab.url === this.statusUrl({nonce: proof.nonce}, marker) &&
            tab.title === `${STATUS_PREFIX}${proof.nonce}:${marker}` &&
            tab.pinned === true && tab.index === index));
        const page = tabs.find(tab => tab.id === proof.page_tab_id &&
            tab.windowId === proof.window_id && tab.pinned !== true && tab.index === 2);
        return markers.every(Boolean) && page ? {window, tabs, markers, page} : null;
    }

    async currentOwnedMarker(nonce) {
        if (!this.chrome.storage?.local || !nonce) return null;
        const local = await this.chrome.storage.local.get([OWNED_MARKERS_KEY]);
        const ownership = local[OWNED_MARKERS_KEY]?.[nonce] || null;
        return ownership?.trusted === false || ownership?.phase === "orphan_unresolved" ?
            null : ownership;
    }

    cleanupOwnershipProof(tombstone, current) {
        if (tombstone.ownership && sameRef(tombstone.ownership, current)) {
            return tombstone.ownership;
        }
        if (tombstone.ownership?.version === 2 && current?.version === 1 &&
                current.pending === true && current.nonce === tombstone.nonce &&
                tombstone.ownership.nonce === tombstone.nonce) {
            return tombstone.ownership;
        }
        const legacy = tombstone.legacy_proof;
        if (legacy?.version === 0 && legacy.nonce === tombstone.nonce &&
                legacy.window_id === tombstone.window_id &&
                sameRef(legacy.status_tab_ids, tombstone.status_tab_ids) &&
                legacy.page_tab_id === tombstone.page_tab_id &&
                (current === true || current?.nonce === tombstone.nonce)) {
            return legacy;
        }
        return null;
    }

    async verifyCleanupX11(tombstone) {
        if (tombstone.xid === null) return true;
        if (!tombstone.binding) return false;
        const result = await this.nativeRequest("abush_browser", "verify_x11_window", {
            xid: tombstone.xid,
            ...tombstone.binding,
        });
        return exactFields(result, ["verified", "xid"]) && result.verified === true &&
            result.xid === tombstone.xid;
    }

    async exactMarkerTabs(tombstone) {
        const tabs = await this.chrome.tabs.query({});
        return tabs.filter(tab => {
            const marker = this.parseStatusUrl(tab.url);
            return marker?.nonce === tombstone.nonce && tab.pinned === true &&
                tab.title === `${STATUS_PREFIX}${tombstone.nonce}:${marker.marker}` &&
                MARKER_IDS.includes(marker.marker) && tab.index === MARKER_IDS.indexOf(marker.marker);
        });
    }

    async retryCleanupTombstone(tombstone) {
        const errors = [];
        const currentOwnership = await this.currentOwnedMarker(tombstone.nonce);
        const ownershipProof = this.cleanupOwnershipProof(tombstone, currentOwnership);
        const topology = ownershipProof ? await this.cleanupWindowFingerprint({
            nonce: tombstone.nonce,
            window_id: tombstone.window_id,
            status_tab_ids: tombstone.status_tab_ids,
            page_tab_id: tombstone.page_tab_id,
        }) : null;
        let x11Verified = tombstone.xid === null;
        if (!x11Verified && ownershipProof) {
            try {
                x11Verified = await this.verifyCleanupX11(tombstone);
            } catch (error) {
                errors.push(error);
            }
        }
        let windowGone = tombstone.window_id === null ||
            await this.windowIsMissing(tombstone.window_id);
        if (!windowGone && topology && x11Verified) {
            try {
                await this.chrome.windows.remove(tombstone.window_id);
            } catch (error) {
                if (!await this.windowIsMissing(tombstone.window_id)) errors.push(error);
            }
            windowGone = await this.windowIsMissing(tombstone.window_id);
        }
        if (!windowGone) {
            let markers = [];
            try {
                markers = await this.exactMarkerTabs(tombstone);
            } catch (error) {
                errors.push(error);
            }
            for (const tab of markers) await this.removeKnownTab(tab.id, errors);
        }
        let x11Cleared = tombstone.xid === null || windowGone;
        if (!x11Cleared && x11Verified && tombstone.binding) {
            const record = {
                nonce: tombstone.nonce,
                x11WindowId: tombstone.xid,
                remoteKey: tombstone.binding.remote_key,
                workspaceId: tombstone.binding.workspace_id,
                windowGeneration: tombstone.binding.generation,
                connectionDisplayName: tombstone.binding.display_title,
            };
            const before = errors.length;
            await this.clearManagedX11(record, errors);
            x11Cleared = errors.length === before;
        }
        let markerCleared = tombstone.nonce === null;
        if (tombstone.nonce && ownershipProof) {
            try {
                await this.disownMarker(tombstone.nonce);
                markerCleared = true;
            } catch (error) {
                errors.push(error);
            }
        } else if (tombstone.nonce && (currentOwnership === null ||
                currentOwnership?.version === 1 && currentOwnership.pending === true)) {
            await this.disownMarker(tombstone.nonce);
            markerCleared = true;
        }
        if ((!windowGone && tombstone.xid !== null && !x11Cleared) ||
                !markerCleared || errors.length > 0) {
            throw new AggregateError(errors.length > 0 ? errors :
                [new Error("cleanup identity remains present")],
            "managed cleanup tombstone remains pending");
        }
    }

    async restoreCleanupTombstones() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([CLEANUP_TOMBSTONES_KEY]);
        const saved = Array.isArray(local[CLEANUP_TOMBSTONES_KEY]) ?
            local[CLEANUP_TOMBSTONES_KEY] : [];
        const pending = [];
        const failures = [];
        for (const original of saved) {
            const tombstone = this.migrateCleanupTombstone(original);
            if (!tombstone) continue;
            try {
                await this.retryCleanupTombstone(tombstone);
            } catch (error) {
                pending.push(tombstone);
                failures.push(error);
            }
        }
        await this.localStateWrite(CLEANUP_TOMBSTONES_KEY, () => pending);
        if (failures.length > 0) {
            this.reportAdvisoryError(new AggregateError(
                failures, "managed cleanup tombstones remain pending"));
        }
    }

    async appendCleanupError(errors, cleanup) {
        try {
            await cleanup();
        } catch (error) {
            errors.push(error);
        }
    }

    async updateOwnedMarkers(update) {
        await this.localStateUpdate(OWNED_MARKERS_KEY, markers => {
            const next = markers && typeof markers === "object" && !Array.isArray(markers) ?
                markers : {};
            update(next);
            return next;
        }, {});
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
            sameRef(ownership.binding, this.bindingPayload(record)) &&
            ownership.trusted !== false && ownership.phase !== "orphan_unresolved");
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
            const oldRecords = [...this.workspaces.values()].map(saved =>
                this.persistedRecord(saved));
            const newRecords = [...this.workspaces.values()].map(saved =>
                this.persistedRecord(saved === previous ? record : saved));
            if (previous === null) newRecords.push(this.persistedRecord(record));
            const revision = ++this.persistRevision;
            try {
                await this.persistRecords(newRecords, revision);
            } catch (error) {
                error.durableCommit = await this.durableRecordState(
                    oldRecords, newRecords, revision);
                if (error.durableCommit === "new") {
                    if (previous === null) this.install(record);
                    else Object.assign(previous, record);
                }
                throw error;
            }
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
        if (!exactFields(payload, ["version", "request_id", "operation", "url", "focus",
            "deadline_ms"]) ||
                payload.version !== 1 || payload.operation !== "ordinary_open" ||
                payload.focus !== true || typeof payload.request_id !== "string" ||
                !payload.request_id || typeof payload.url !== "string" || !payload.url ||
                !Number.isSafeInteger(payload.deadline_ms) || payload.deadline_ms <= 0) {
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
            if (tabs.some(tab => this.parseStatusUrl(tab.url) !== null ||
                    tab.url?.startsWith(this.chrome.runtime.getURL("ordinary-staging.html#")))) {
                continue;
            }
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
            left.focus === right.focus && left.deadline_ms === right.deadline_ms;
    }

    async stageBootstrapLeaseIfNeeded() {
        const ledger = await this.coldOrdinaryLedger();
        const recoverable = new Set(ledger.filter(entry =>
            ["effect_started", "completed"].includes(entry.phase)).map(entry => entry.request_id));
        const valid = [...this.bootstrapLeases.values()].filter(lease =>
            lease.deadline_ms > Date.now() && !recoverable.has(lease.request_id));
        if (valid.length === 0) return;
        if (valid.length > 1) {
            this.reportAdvisoryError(new Error(
                "multiple cold ordinary launch leases are pending; bootstrap is serialized"));
        }
        const lease = valid.sort((left, right) => left.deadline_ms - right.deadline_ms)[0];
        const nonce = lease.request_id;
        const stagingUrl = this.ordinaryStagingUrl(nonce);
        const allTabs = await this.chrome.tabs.query({});
        const existing = allTabs.filter(tab => tab.url === stagingUrl);
        if (existing.length > 1) {
            throw new Error("cold ordinary launch staging identity is ambiguous");
        }
        if (existing.length === 1) {
            lease.bootstrapContext = {
                windowId: existing[0].windowId,
                stagingTabId: existing[0].id,
                originalActiveTabId: existing[0].id,
                createdWindow: true,
                baselineWindowIds: [],
                nonce,
            };
            this.rememberOrdinaryWindow(existing[0].windowId);
            this.windowRegistry.set(existing[0].windowId, {
                windowId: existing[0].windowId,
                kind: "ordinary",
                snapshot: this.sessionSnapshot([existing[0]]),
                tabIds: [existing[0].id],
                revision: ++this.windowRegistryRevision,
            });
            return;
        }
        const windows = await this.chrome.windows.getAll({windowTypes: ["normal"]});
        const ordinary = [];
        for (const window of windows) {
            if (window.type !== "normal" || this.windowOwners.has(window.id)) continue;
            const tabs = allTabs.filter(tab => tab.windowId === window.id);
            if (tabs.some(tab => this.parseStatusUrl(tab.url) !== null ||
                    tab.url?.startsWith(this.chrome.runtime.getURL("ordinary-staging.html#")))) {
                continue;
            }
            ordinary.push(window);
        }
        if (ordinary.length > 0) return;
        const stagingWindow = await this.chrome.windows.create({
            url: stagingUrl, focused: false, type: "normal",
        });
        const tabs = Array.isArray(stagingWindow?.tabs) ? stagingWindow.tabs :
            await this.chrome.tabs.query({windowId: stagingWindow?.id});
        const staging = tabs.filter(tab => tab.url === stagingUrl);
        if (!Number.isInteger(stagingWindow?.id) || staging.length !== 1 ||
                !Number.isInteger(staging[0].id)) {
            throw new Error("cold ordinary launch staging window is not exact");
        }
        lease.bootstrapContext = {
            windowId: stagingWindow.id,
            stagingTabId: staging[0].id,
            originalActiveTabId: staging[0].id,
            createdWindow: true,
            baselineWindowIds: [],
            nonce,
        };
        this.rememberOrdinaryWindow(stagingWindow.id);
        this.windowRegistry.set(stagingWindow.id, {
            windowId: stagingWindow.id,
            kind: "ordinary",
            snapshot: this.sessionSnapshot(staging),
            tabIds: [staging[0].id],
            revision: ++this.windowRegistryRevision,
        });
    }

    async coldOrdinaryLedger() {
        if (!this.chrome.storage?.local) return [];
        const local = await this.chrome.storage.local.get([COLD_ORDINARY_LEDGER_KEY]);
        return Array.isArray(local[COLD_ORDINARY_LEDGER_KEY]) ?
            local[COLD_ORDINARY_LEDGER_KEY] : [];
    }

    async persistColdOrdinaryLedger(entries) {
        await this.localStateWrite(COLD_ORDINARY_LEDGER_KEY, () => entries.slice(-256));
    }

    async cleanupStaleManagedBeforeBootstrap() {
        const stagingWindowIds = new Set([...this.bootstrapLeases.values()]
            .map(lease => lease.bootstrapContext?.windowId).filter(Number.isInteger));
        if (stagingWindowIds.size === 0) return;
        let candidates = [
            ...this.workspaces.values(),
            ...this.startupManagedRecords,
        ].filter((record, index, records) => !stagingWindowIds.has(record.windowId) &&
            records.findIndex(candidate => candidate.windowId === record.windowId) === index);
        const durable = this.chrome.storage?.session ?
            await this.chrome.storage.session.get(["abushWorkspaces"]) : {abushWorkspaces: []};
        for (const saved of durable.abushWorkspaces || []) {
            if (!Number.isInteger(saved?.windowId) || stagingWindowIds.has(saved.windowId) ||
                    candidates.some(record => record.windowId === saved.windowId)) continue;
            const ownership = await this.currentOwnedMarker(saved.nonce);
            if (ownership && this.savedOwnershipMatches(ownership, saved)) candidates.push(saved);
        }
        if (candidates.length === 0 && this.chrome.storage?.local) {
            const local = await this.chrome.storage.local.get([OWNED_MARKERS_KEY]);
            candidates = Object.values(local[OWNED_MARKERS_KEY] || {})
                .filter(ownership => ownership?.version === 2 &&
                    ownership.trusted !== false && ownership.phase !== "orphan_unresolved" &&
                    !stagingWindowIds.has(ownership.window_id))
                .map(ownership => ({
                    remoteKey: ownership.binding?.remote_key,
                    principalId: `startup-${ownership.nonce}`,
                    workspaceId: ownership.binding?.workspace_id,
                    connectionId: "startup-cleanup",
                    executorFence: {executor_id: "startup-cleanup", epoch: 1},
                    logicalWindowId: ownership.logical_window_id,
                    logicalPageId: ownership.logical_page_id,
                    windowGeneration: ownership.window_generation,
                    pageGeneration: ownership.page_generation,
                    windowId: ownership.window_id,
                    x11WindowId: ownership.xid,
                    statusTabIds: ownership.status_tab_ids,
                    pageTabId: ownership.page_tab_id,
                    nonce: ownership.nonce,
                    connectionDisplayName: ownership.binding?.display_title,
                })).filter(record => record.remoteKey && record.workspaceId &&
                    Number.isInteger(record.windowId));
        }
        if (candidates.length === 0) return;
        for (const record of candidates) {
            if (!this.workspaces.has(this.key(record.remoteKey, record.principalId))) {
                record.debuggerAttached = false;
                this.install(record);
            }
        }
        const stale = candidates.map(record => ({id: record.windowId, type: "normal"}));
        await this.closeManagedWindowSet(stale);
    }

    async coldOrdinaryIdentity(payload) {
        const digest = await crypto.subtle.digest(
            "SHA-256", new TextEncoder().encode(payload.url));
        const urlHash = [...new Uint8Array(digest)]
            .map(value => value.toString(16).padStart(2, "0")).join("");
        return {
            request_id: payload.request_id,
            url_hash: urlHash,
            deadline_ms: payload.deadline_ms,
        };
    }

    sameColdOrdinaryIdentity(entry, identity) {
        return entry.request_id === identity.request_id &&
            entry.url_hash === identity.url_hash &&
            entry.deadline_ms === identity.deadline_ms;
    }

    async executeBootstrapLeases() {
        let ledger = await this.coldOrdinaryLedger();
        for (const [requestId, payload] of [...this.bootstrapLeases]) {
            if (payload.deadline_ms <= Date.now()) {
                this.bootstrapLeases.delete(requestId);
                continue;
            }
            const identity = await this.coldOrdinaryIdentity(payload);
            const known = ledger.find(entry => entry.request_id === requestId);
            if (known && !this.sameColdOrdinaryIdentity(known, identity)) {
                this.reportAdvisoryError(new Error(
                    "conflicting durable cold ordinary launch identity"));
                continue;
            }
            const context = payload.bootstrapContext;
            delete payload.bootstrapContext;
            if (known?.phase === "completed") {
                this.bootstrapLeases.delete(requestId);
                const remembered = this.completedOrdinaryOperations.get(requestId);
                if (!remembered || remembered.completion !== known.completion) {
                    this.rememberOrdinaryCompletion(payload, known.completion);
                    await this.ordinaryCompletion(payload, known.completion);
                }
                continue;
            }
            if (known?.phase === "effect_started") {
                this.bootstrapLeases.delete(requestId);
                const completion = await this.reconcileColdOrdinaryEffect(payload, known);
                ledger = ledger.map(entry => entry === known ?
                    {...entry, phase: "completed", completion} : entry);
                await this.persistColdOrdinaryLedger(ledger);
                this.rememberOrdinaryCompletion(payload, completion);
                await this.ordinaryCompletion(payload, completion);
                continue;
            }
            this.bootstrapLeases.delete(requestId);
            const started = {
                version: 1,
                ...identity,
                phase: "effect_started",
                completion: null,
                browser_boot_id: this.browserBootId,
                target_window_id: Number.isInteger(context?.windowId) ? context.windowId : null,
                target_tab_id: Number.isInteger(context?.stagingTabId) ? context.stagingTabId : null,
            };
            ledger = [...ledger.filter(entry => entry.request_id !== requestId), started];
            await this.persistColdOrdinaryLedger(ledger);
            try {
                const completion = await this.ordinaryOpen(payload, context || null, true);
                const completed = {...started, phase: "completed", completion};
                ledger = [...ledger.filter(entry => entry.request_id !== requestId), completed];
                await this.persistColdOrdinaryLedger(ledger);
                await this.ordinaryCompletion(payload, completion);
            } catch (error) {
                this.reportAdvisoryError(new AggregateError([error],
                    "cold ordinary launch lease failed"));
            }
        }
    }

    async reconcileColdOrdinaryEffect(payload, ledger) {
        if (ledger.browser_boot_id !== this.browserBootId ||
                !Number.isInteger(ledger.target_window_id) ||
                !Number.isInteger(ledger.target_tab_id)) return "unknown";
        try {
            const tab = await this.chrome.tabs.get(ledger.target_tab_id);
            return tab.windowId === ledger.target_window_id && tab.url === payload.url ?
                "applied" : "unknown";
        } catch (_) {
            return "unknown";
        }
    }

    async ordinaryOpen(payload, bootstrapContext = null, durableBootstrap = false) {
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
            .then(() => this.performOrdinaryOpen(payload, bootstrapContext,
                !durableBootstrap));
        this.ordinaryTail = operation;
        operation.finally(() => {
            if (this.ordinaryOperations.get(payload.request_id)?.operation === operation) {
                this.ordinaryOperations.delete(payload.request_id);
            }
        }).catch(() => {});
        this.ordinaryOperations.set(payload.request_id, {
            payload: {...payload}, operation, cancelled: false,
        });
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

    async cancelOrdinaryOpen(payload) {
        if (!payload || payload.version !== 1 ||
                payload.operation !== "ordinary_open_cancel" ||
                typeof payload.request_id !== "string") return false;
        const operation = this.ordinaryOperations.get(payload.request_id);
        if (!operation) return false;
        operation.cancelled = true;
        return true;
    }

    async performOrdinaryOpen(payload, bootstrapContext = null, publishCompletion = true) {
        let context = bootstrapContext;
        let navigationRequested = false;
        let completion = "not_applied";
        const nonce = bootstrapContext?.nonce || crypto.randomUUID().replaceAll("-", "");
        const isCancelled = () => this.ordinaryOperations.get(payload.request_id)?.cancelled === true;
        try {
            const url = this.validateOrdinaryOpen(payload);
            if (isCancelled() || Date.now() >= payload.deadline_ms) {
                throw new Error("ordinary-open deadline expired or cancelled");
            }
            if (context === null) context = await this.selectOrdinaryWindow();
            if (context === null) {
                const windows = await this.chrome.windows.getAll({windowTypes: ["normal"]});
                context = {
                    windowId: null,
                    stagingTabId: null,
                    originalActiveTabId: null,
                    createdWindow: true,
                    baselineWindowIds: windows.map(window => window.id),
                };
                if (Date.now() >= payload.deadline_ms) {
                    throw new Error("ordinary-open deadline expired");
                }
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
            } else if (!Number.isInteger(context.stagingTabId)) {
                if (Date.now() >= payload.deadline_ms) {
                    throw new Error("ordinary-open deadline expired");
                }
                const stagingTab = await this.chrome.tabs.create({
                    windowId: context.windowId,
                    url: this.ordinaryStagingUrl(nonce),
                    active: true,
                });
                context.stagingTabId = stagingTab.id;
            } else {
                await this.chrome.tabs.update(context.stagingTabId, {active: true});
            }
            if (!Number.isInteger(context.windowId) ||
                    !Number.isInteger(context.stagingTabId)) {
                throw new Error("ordinary staging window has no exact tab");
            }
            if (bootstrapContext) {
                const candidate = await this.chrome.tabs.get(context.stagingTabId);
                if (candidate.url === this.ordinaryStagingUrl(nonce) &&
                        candidate.windowId === context.windowId) {
                    candidate.title = candidate.title || `${ORDINARY_PREFIX}${nonce}`;
                }
            }
            const staging = await this.chrome.tabs.get(context.stagingTabId);
            if (staging.windowId !== context.windowId || staging.active !== true) {
                throw new Error("ordinary staging tab is not active in its exact window");
            }
            await this.waitForOrdinaryTitle(context.stagingTabId, nonce, payload.deadline_ms);
            await this.verifyOrdinaryStaging(context, nonce);
            if (isCancelled() || Date.now() >= payload.deadline_ms) {
                throw new Error("ordinary-open deadline expired or cancelled");
            }
            const prepared = await this.nativeRequest("ordinary_open", "prepare",
                {nonce, deadline_ms: payload.deadline_ms});
            if (!exactFields(prepared, ["xid", "nonce"]) || prepared.nonce !== nonce ||
                    !Number.isInteger(prepared.xid) || prepared.xid <= 0) {
                throw new Error("invalid ordinary prepare response");
            }
            if (isCancelled() || Date.now() >= payload.deadline_ms) {
                throw new Error("ordinary-open deadline expired or cancelled");
            }
            navigationRequested = true;
            if (new URL(url).protocol === "file:") {
                const navigationPayload = {nonce, xid: prepared.xid, url,
                    deadline_ms: payload.deadline_ms};
                let navigated;
                try {
                    navigated = await this.nativeRequest(
                        "ordinary_open", "ordinary_file_navigate", navigationPayload);
                } catch (error) {
                    if (error?.nativeDispatch === "not_sent") navigationRequested = false;
                    throw error;
                }
                if (!exactFields(navigated, ["nonce", "xid", "url", "deadline_ms",
                    "completion"]) || navigated.nonce !== nonce || navigated.xid !== prepared.xid ||
                        navigated.url !== url || navigated.deadline_ms !== payload.deadline_ms ||
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
                    navigationRequested = true;
                } catch (_) {
                    throw new Error("ordinary navigation completion is unknown");
                }
            }
            if (isCancelled() || Date.now() >= payload.deadline_ms) {
                completion = "unknown";
                throw new Error("ordinary-open deadline expired or cancelled");
            }
            try {
                await this.nativeRequest("ordinary_open", "activate",
                    {...prepared, deadline_ms: payload.deadline_ms});
                completion = "applied";
            } catch (_) {
                completion = "unknown";
            }
            if (isCancelled() || Date.now() >= payload.deadline_ms) completion = "unknown";
            if (completion === "applied") this.rememberOrdinaryWindow(context.windowId);
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
        if (publishCompletion) await this.ordinaryCompletion(payload, completion);
        return completion;
    }

    async waitForOrdinaryTitle(tabId, nonce, deadlineMs = Date.now() + 1000) {
        const expected = `${ORDINARY_PREFIX}${nonce}`;
        while (Date.now() < deadlineMs) {
            const tab = await this.chrome.tabs.get(tabId);
            if (tab.active === true && tab.title === expected) return;
            await new Promise(resolve => setTimeout(resolve, Math.min(10,
                Math.max(0, deadlineMs - Date.now()))));
        }
        throw new Error("ordinary staging title was not ready");
    }

    async waitForStatusTitle(tabId, nonce, marker, timeoutMs = 1000) {
        const expected = `${STATUS_PREFIX}${nonce}:${marker}`;
        const deadline = Date.now() + timeoutMs;
        while (Date.now() < deadline) {
            const tab = await this.chrome.tabs.get(tabId);
            if (tab.title === expected) return;
            await new Promise(resolve => setTimeout(resolve, Math.min(10,
                Math.max(0, deadline - Date.now()))));
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
        return this.enqueueLocalState(() => this.restoreHostLocked());
    }

    async restoreHostLocked() {
        if (!this.browserBootId) throw new Error("browser boot identity is required for handoff");
        const local = await this.chrome.storage.local.get([RESTORE_HOST_KEY]);
        const intent = local[RESTORE_HOST_KEY];
        if (!intent || typeof intent !== "object" || intent.version !== 3 ||
                !["prepared", "inflight", "unknown", "verified", "consumed"]
                    .includes(intent.phase) ||
                !["exact", "ambiguous"].includes(intent.outcome) ||
                typeof intent.source_boot !== "string" ||
                typeof intent.target_boot !== "string" || !intent.target_boot ||
                (intent.sessionId !== null && typeof intent.sessionId !== "string")) return;
        if (intent.source_boot === this.browserBootId ||
                intent.target_boot === this.browserBootId) return;
        if (intent.outcome === "ambiguous") {
            this.reportAdvisoryError(new Error(
                "ordinary close handoff is ambiguous and remains pending"));
            return;
        }
        if (intent.phase === "consumed") {
            await this.localStateWriteLocked(RESTORE_HOST_KEY, false);
            return;
        }
        if (intent.phase === "verified") {
            await this.consumeRestoreHost(intent);
            return;
        }
        if (intent.phase === "unknown" || intent.phase === "inflight") {
            if (await this.restoreHostAlreadyLive(intent)) {
                await this.consumeRestoreHost(intent);
                return;
            }
            if (intent.phase === "unknown") return;
            for (let attempt = 0; attempt < 20; attempt++) {
                if (await this.restoreHostAlreadyLive(intent)) {
                    await this.consumeRestoreHost(intent);
                    return;
                }
                if (attempt + 1 < 20) await new Promise(resolve => setTimeout(resolve, 25));
            }
            const entries = this.chrome.sessions?.getRecentlyClosed ?
                await this.recentlyClosedEntries() : null;
            const stillRestorable = (entries || []).filter(entry =>
                this.sessionEntryId(entry) === intent.sessionId &&
                this.sessionMatchesSnapshot(entry, intent.snapshot));
            if (entries !== null && stillRestorable.length === 0) {
                await this.localStateWriteLocked(RESTORE_HOST_KEY,
                    {...intent, phase: "unknown"});
                this.reportAdvisoryError(new Error(
                    "ordinary host restore was accepted but is not yet visible"));
            }
            return;
        }
        if (await this.restoreHostAlreadyLive(intent)) {
            await this.consumeRestoreHost(intent);
            return;
        }
        if (!intent.sessionId || !intent.snapshot || !this.chrome.sessions?.restore) {
            this.reportAdvisoryError(new Error(
                "exact ordinary restore identity is unavailable; handoff retained"));
            return;
        }
        let entries = this.chrome.sessions?.getRecentlyClosed ?
            await this.recentlyClosedEntries() : null;
        if (entries !== null) {
            const exact = entries.filter(entry =>
                this.sessionEntryId(entry) === intent.sessionId &&
                this.sessionMatchesSnapshot(entry, intent.snapshot));
            if (exact.length !== 1) {
                this.reportAdvisoryError(new Error(
                    "exact ordinary restore session is unavailable; handoff retained"));
                return;
            }
        }
        const inflight = {...intent, phase: "inflight", target_boot: this.browserBootId};
        await this.localStateWriteLocked(RESTORE_HOST_KEY, inflight);
        let restored;
        try {
            restored = await this.chrome.sessions.restore(intent.sessionId);
        } catch (error) {
            entries = this.chrome.sessions?.getRecentlyClosed ?
                await this.recentlyClosedEntries() : null;
            const exactStillPresent = entries?.some(entry =>
                this.sessionEntryId(entry) === intent.sessionId &&
                this.sessionMatchesSnapshot(entry, intent.snapshot));
            if (exactStillPresent) {
                await this.localStateWriteLocked(RESTORE_HOST_KEY,
                    {...inflight, phase: "prepared", target_boot: "next"});
                this.reportAdvisoryError(new AggregateError([error],
                    "exact ordinary restore failed and remains pending"));
            } else {
                await this.localStateWriteLocked(RESTORE_HOST_KEY,
                    {...inflight, phase: "unknown"});
                this.reportAdvisoryError(new AggregateError([error],
                    "ordinary restore completion is unknown"));
            }
            return;
        }
        if (!this.sessionMatchesSnapshot(restored, intent.snapshot) &&
                !await this.restoreHostAlreadyLive(intent)) {
            await this.localStateWriteLocked(RESTORE_HOST_KEY,
                {...inflight, phase: "unknown"});
            this.reportAdvisoryError(new Error(
                "ordinary host restore was not verified live"));
            return;
        }
        await this.consumeRestoreHost(inflight);
    }

    async restoreHostAlreadyLive(intent) {
        const ordinary = await this.ordinaryWindows();
        if (ordinary.length === 0 || !intent.snapshot) return false;
        const matches = [];
        for (const window of ordinary) {
            const tabs = await this.chrome.tabs.query({windowId: window.id});
            if (this.sessionMatchesSnapshot({window: {sessionId: "live", tabs}},
                intent.snapshot)) matches.push(window.id);
        }
        if (matches.length > 1) throw new Error("ambiguous live ordinary restore snapshot");
        return matches.length === 1;
    }

    async consumeRestoreHost(intent) {
        const verified = {...intent, phase: "verified", target_boot: this.browserBootId};
        await this.localStateWriteLocked(RESTORE_HOST_KEY, verified);
        await this.localStateWriteLocked(RESTORE_HOST_KEY,
            {...verified, phase: "consumed"});
        await this.localStateWriteLocked(RESTORE_HOST_KEY, false);
    }

    async localStateWriteLocked(key, value) {
        await this.chrome.storage.local.set({[key]: structuredClone(value)});
    }

    async markRestoreHost(intent) {
        if (!this.chrome.storage?.local || !this.browserBootId) {
            throw new Error("browser boot identity is required before durable handoff");
        }
        const ledger = {
            version: 3,
            phase: "prepared",
            outcome: intent?.outcome || (intent?.sessionId ? "exact" : "ambiguous"),
            source_boot: this.browserBootId,
            target_boot: "next",
            sessionId: intent?.sessionId || null,
            snapshot: intent?.snapshot || null,
            registry_revision: intent?.registryRevision ?? this.windowRegistryRevision,
            ambiguity: intent?.ambiguity || null,
        };
        await this.localStateWrite(RESTORE_HOST_KEY, () => ledger);
    }

    classifyWindow(windowId, tabs, type = "normal") {
        if (type !== "normal") return "transient";
        if (this.windowOwners.has(windowId) || tabs.some(tab =>
            this.parseStatusUrl(tab.url) !== null)) return "managed";
        if (tabs.some(tab => typeof tab.url === "string" &&
                tab.url.startsWith(this.chrome.runtime.getURL("ordinary-staging.html#")))) {
            return "transient";
        }
        return "ordinary";
    }

    registryFingerprint(tabs) {
        return this.sessionSnapshot([...tabs].sort((left, right) =>
            (left.index ?? 0) - (right.index ?? 0)));
    }

    async refreshWindowRegistry() {
        const [windows, tabs] = await Promise.all([
            this.chrome.windows.getAll({windowTypes: ["normal"]}),
            this.chrome.tabs.query({}),
        ]);
        const tabsByWindow = new Map();
        for (const tab of tabs) {
            if (!tabsByWindow.has(tab.windowId)) tabsByWindow.set(tab.windowId, []);
            tabsByWindow.get(tab.windowId).push(tab);
        }
        const next = new Map();
        const revision = ++this.windowRegistryRevision;
        for (const window of windows) {
            const windowTabs = tabsByWindow.get(window.id) || [];
            next.set(window.id, {
                windowId: window.id,
                kind: this.classifyWindow(window.id, windowTabs, window.type),
                snapshot: this.registryFingerprint(windowTabs),
                tabIds: windowTabs.map(tab => tab.id).filter(Number.isInteger),
                revision,
            });
        }
        for (const [windowId, registry] of this.windowRegistry) {
            if (!next.has(windowId) && registry.kind === "managed" &&
                    this.windowOwners.has(windowId)) next.set(windowId, registry);
        }
        this.windowRegistry = next;
        return revision;
    }

    nextWindowSnapshotToken(windowId) {
        const token = (this.windowSnapshotTokens.get(windowId) || 0) + 1;
        this.windowSnapshotTokens.set(windowId, token);
        return token;
    }

    closeWindowIdentity(windowId, record = this.windowRegistry.get(windowId)) {
        const snapshot = record?.snapshot || this.removedHostSnapshots.get(windowId) || null;
        return {
            windowId,
            snapshot: snapshot ? structuredClone(snapshot) : null,
            tabIds: Array.isArray(record?.tabIds) ? [...record.tabIds] : [],
            registryRevision: record?.revision ?? this.windowRegistryRevision,
        };
    }

    captureOrdinaryCloseWindow(windowId, record = this.windowRegistry.get(windowId)) {
        const existing = this.ordinaryCloseWindowBatches.get(windowId);
        if (existing) return existing;
        let batch = this.activeOrdinaryCloseBatch;
        if (!batch || batch.settled) {
            batch = {
                generation: ++this.ordinaryCloseGeneration,
                revision: 0,
                windows: new Map(),
                expectedWindowIds: new Set([...this.windowRegistry.values()]
                    .filter(candidate => candidate.kind === "ordinary")
                    .map(candidate => candidate.windowId)),
                knownSessionIds: new Set(this.recentSessionIds || []),
                settled: false,
            };
            this.activeOrdinaryCloseBatch = batch;
        }
        batch.windows.set(windowId, this.closeWindowIdentity(windowId, record));
        batch.revision += 1;
        this.ordinaryCloseWindowBatches.set(windowId, batch);
        return batch;
    }

    freezeClosingWindow(windowId) {
        this.nextWindowSnapshotToken(windowId);
        const record = this.windowRegistry.get(windowId);
        if (!record) return false;
        if (record.kind === "ordinary") this.captureOrdinaryCloseWindow(windowId, record);
        this.removedHostSnapshots.set(windowId, structuredClone(record.snapshot));
        this.windowRegistry.set(windowId, {...record, closing: true});
        return true;
    }

    async rememberWindowSnapshot(windowId) {
        const token = this.nextWindowSnapshotToken(windowId);
        const frozen = this.windowRegistry.get(windowId);
        if (frozen?.closing) return;
        const previous = this.snapshotTails.get(windowId) || Promise.resolve();
        const update = previous.catch(() => {}).then(async () => {
            const captured = this.windowRegistry.get(windowId);
            if (captured?.closing || this.windowSnapshotTokens.get(windowId) !== token) return;
            const capturedRevision = captured?.revision ?? null;
            const tabs = await this.chrome.tabs.query({windowId});
            const current = this.windowRegistry.get(windowId);
            if (this.windowSnapshotTokens.get(windowId) !== token || current?.closing ||
                    (capturedRevision !== null && current?.revision !== capturedRevision) ||
                    (capturedRevision === null && current !== undefined)) return;
            if (tabs.length === 0 ||
                    tabs.some(tab => this.parseStatusUrl(tab.url) !== null)) return;
            this.removedHostSnapshots.set(windowId, this.sessionSnapshot(tabs));
            this.windowRegistry.set(windowId, {
                windowId,
                kind: this.classifyWindow(windowId, tabs, "normal"),
                snapshot: this.registryFingerprint(tabs),
                tabIds: tabs.map(tab => tab.id).filter(Number.isInteger),
                revision: ++this.windowRegistryRevision,
            });
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

    async settleOrdinaryCloseBatch(batch) {
        if (batch.settled) return batch;
        if (!batch.expectedWindowIds) batch.expectedWindowIds = new Set(batch.windows.keys());
        let stableRevision = null;
        for (let attempt = 0; attempt < ORDINARY_CLOSE_RETRY_ATTEMPTS; attempt++) {
            const capturedExpected = [...batch.expectedWindowIds]
                .every(windowId => batch.windows.has(windowId));
            if (capturedExpected && stableRevision === batch.revision) break;
            stableRevision = batch.revision;
            await new Promise(resolve => setTimeout(resolve, ORDINARY_CLOSE_RETRY_MS));
        }
        batch.settled = true;
        if ([...batch.expectedWindowIds].some(windowId => !batch.windows.has(windowId))) {
            batch.forcedAmbiguous = true;
        }
        if (this.activeOrdinaryCloseBatch === batch) this.activeOrdinaryCloseBatch = null;
        return batch;
    }

    async waitForOrdinaryClosePeer(batch) {
        for (let attempt = 0; attempt < ORDINARY_CLOSE_RETRY_ATTEMPTS; attempt++) {
            if (batch.windows.size > 1) return true;
            await new Promise(resolve => setTimeout(resolve, ORDINARY_CLOSE_RETRY_MS));
        }
        return batch.windows.size > 1;
    }

    settleNonFinalOrdinaryClose(windowId) {
        const batch = this.ordinaryCloseWindowBatches.get(windowId);
        if (!batch || batch.windows.size !== 1) return false;
        batch.settled = true;
        if (this.activeOrdinaryCloseBatch === batch) this.activeOrdinaryCloseBatch = null;
        this.ordinaryCloseWindowBatches.delete(windowId);
        return true;
    }

    ordinarySessionCandidate(entry) {
        const sessionId = this.sessionEntryId(entry);
        if (!sessionId || !this.isOrdinarySessionEntry(entry)) return null;
        return {
            session_id: sessionId,
            snapshot: this.sessionSnapshot(entry.window.tabs),
        };
    }

    async persistOrdinaryCloseAmbiguity(batch, entries) {
        const candidates = entries.map(entry => this.ordinarySessionCandidate(entry))
            .filter(candidate => candidate !== null &&
                !batch.knownSessionIds.has(candidate.session_id));
        const windows = [...batch.windows.values()].map(identity => ({
            window_id: identity.windowId,
            registry_revision: identity.registryRevision,
            tab_ids: [...identity.tabIds],
            snapshot: identity.snapshot ? structuredClone(identity.snapshot) : null,
            candidate_session_ids: candidates.filter(candidate => identity.snapshot &&
                sameRef(candidate.snapshot, identity.snapshot))
                .map(candidate => candidate.session_id),
        }));
        const ambiguity = {
            version: 2,
            source_boot: this.browserBootId,
            batch_generation: batch.generation,
            registry_revision: this.windowRegistryRevision,
            windows,
            session_candidates: candidates,
        };
        await this.localStateUpdate(CLOSE_AMBIGUITIES_KEY, saved => [
            ...(Array.isArray(saved) ? saved : []), ambiguity,
        ].slice(-64), []);
        return ambiguity;
    }

    async resolveOrdinaryCloseBatch(batch) {
        if (batch.resolution) return batch.resolution;
        batch.resolution = (async () => {
            await this.settleOrdinaryCloseBatch(batch);
            let latestEntries = [];
            let exact = null;
            for (let attempt = 0; attempt < ORDINARY_CLOSE_RETRY_ATTEMPTS; attempt++) {
                latestEntries = this.chrome.sessions?.getRecentlyClosed ?
                    await this.recentlyClosedEntries() : [];
                const unseen = latestEntries.filter(entry => {
                    const sessionId = this.sessionEntryId(entry);
                    return sessionId && !batch.knownSessionIds.has(sessionId);
                });
                const expectedWindowIds = batch.expectedWindowIds ||
                    new Set(batch.windows.keys());
                const exactEligible = expectedWindowIds.size === 1 &&
                    batch.windows.size === 1 && !batch.forcedAmbiguous;
                if (exactEligible) {
                    const identity = [...batch.windows.values()][0];
                    const matches = unseen.filter(entry =>
                        this.sessionMatchesSnapshot(entry, identity.snapshot));
                    if (matches.length === 1) {
                        exact = {
                            version: 3,
                            outcome: "exact",
                            sessionId: this.sessionEntryId(matches[0]),
                            snapshot: structuredClone(identity.snapshot),
                            registryRevision: identity.registryRevision,
                        };
                        break;
                    }
                    if (matches.length > 1) break;
                } else {
                    const candidates = unseen.map(entry => this.ordinarySessionCandidate(entry))
                        .filter(candidate => candidate !== null);
                    const allObserved = [...batch.windows.values()].every(identity =>
                        identity.snapshot && candidates.some(candidate =>
                            sameRef(candidate.snapshot, identity.snapshot)));
                    if (allObserved) break;
                }
                if (attempt + 1 < ORDINARY_CLOSE_RETRY_ATTEMPTS) {
                    await new Promise(resolve => setTimeout(resolve, ORDINARY_CLOSE_RETRY_MS));
                }
            }
            this.recentSessionIds = new Set(latestEntries.map(entry =>
                this.sessionEntryId(entry)).filter(Boolean));
            if (exact) return exact;
            const ambiguity = await this.persistOrdinaryCloseAmbiguity(batch, latestEntries);
            const identities = [...batch.windows.values()];
            return {
                version: 3,
                outcome: "ambiguous",
                sessionId: null,
                snapshot: identities.length === 1 && identities[0].snapshot ?
                    structuredClone(identities[0].snapshot) : null,
                registryRevision: Math.max(...identities.map(identity =>
                    identity.registryRevision), this.windowRegistryRevision),
                ambiguity,
            };
        })();
        return batch.resolution;
    }

    async recentlyClosedHostIntent(windowId) {
        const batch = this.ordinaryCloseWindowBatches.get(windowId) ||
            this.captureOrdinaryCloseWindow(windowId);
        return this.resolveOrdinaryCloseBatch(batch);
    }

    async closeOrphanedStatusWindows(liveWindowIds, ownedMarkers) {
        const tabs = await this.chrome.tabs.query({});
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
            const windowErrorStart = cleanupErrors.length;
            const parsed = windowTabs.map(tab => ({tab, marker: this.parseStatusUrl(tab.url)}));
            const markers = parsed.filter(item => item.marker !== null);
            const legacyMarkers = markers.filter(item => item.marker.marker === "legacy");
            const markerIds = new Set(markers.map(item => item.marker.marker));
            const nonces = new Set(markers.map(item => item.marker.nonce));
            const ownership = nonces.size === 1 ?
                ownedMarkers[markers[0]?.marker.nonce] : null;
            const trustedOwnership = ownership?.trusted === false ||
                ownership?.phase === "orphan_unresolved" ? null : ownership;
            const orderedMarkerTabIds = MARKER_IDS.map(marker =>
                markers.find(item => item.marker.marker === marker)?.tab.id);
            const ownedPage = windowTabs.find(tab => tab.id === trustedOwnership?.page_tab_id &&
                tab.pinned !== true && tab.index === 2);
            const exactOwnedFingerprint = Boolean(trustedOwnership &&
                trustedOwnership.version === 2 &&
                Array.isArray(trustedOwnership.status_tab_ids) &&
                trustedOwnership.nonce === markers[0]?.marker.nonce &&
                trustedOwnership.window_id === windowId &&
                sameRef(trustedOwnership.status_tab_ids, orderedMarkerTabIds) && ownedPage);
            let x11Verified = false;
            if (trustedOwnership?.version === 2 && Number.isInteger(trustedOwnership.xid) &&
                    trustedOwnership.xid > 0 && trustedOwnership.binding) {
                try {
                    x11Verified = await this.verifyCleanupX11({
                        xid: trustedOwnership.xid,
                        binding: trustedOwnership.binding,
                    });
                } catch (error) {
                    cleanupErrors.push(error);
                }
            }
            const ownedWindow = exactOwnedFingerprint && x11Verified &&
                windowTypes.get(windowId) === "normal" &&
                windowTabs.length === 3 && markers.length === 2 &&
                legacyMarkers.length === 0 && markerIds.size === 2 && nonces.size === 1 &&
                markers.every(({tab, marker}) => tab.pinned &&
                    tab.title === `${STATUS_PREFIX}${marker.nonce}:${marker.marker}`) &&
                markers.map(({tab}) => tab.index).sort().join(",") === "0,1" &&
                windowTabs.filter(tab => !tab.pinned && tab.index === 2).length === 1;
            if (ownedWindow) {
                let removed = false;
                try {
                    await this.chrome.windows.remove(windowId);
                    removed = await this.windowIsMissing(windowId);
                } catch (error) {
                    removed = await this.windowIsMissing(windowId);
                    if (!removed) cleanupErrors.push(error);
                }
                if (removed) {
                    delete ownedMarkers[markers[0].marker.nonce];
                } else if (trustedOwnership) {
                    try {
                        await this.recordCleanupTombstone({
                            nonce: ownership.nonce,
                            windowId: ownership.window_id,
                            x11WindowId: ownership.xid,
                            statusTabIds: ownership.status_tab_ids,
                            pageTabId: ownership.page_tab_id,
                            remoteKey: ownership.binding?.remote_key,
                            workspaceId: ownership.binding?.workspace_id,
                            logicalWindowId: ownership.logical_window_id,
                            logicalPageId: ownership.logical_page_id,
                            windowGeneration: ownership.window_generation,
                            pageGeneration: ownership.page_generation,
                            connectionDisplayName: ownership.binding?.display_title,
                        }, cleanupErrors);
                    } catch (error) {
                        cleanupErrors.push(error);
                    }
                }
            } else {
                for (const {tab} of markers) await this.removeKnownTab(tab.id, cleanupErrors);
                if (trustedOwnership?.version === 2 &&
                        Number.isInteger(trustedOwnership.xid) &&
                        trustedOwnership.xid > 0 && x11Verified) {
                    const record = {
                        remoteKey: trustedOwnership.binding?.remote_key,
                        workspaceId: trustedOwnership.binding?.workspace_id,
                        connectionDisplayName: trustedOwnership.binding?.display_title,
                        windowGeneration: trustedOwnership.binding?.generation,
                        nonce: trustedOwnership.nonce,
                        x11WindowId: trustedOwnership.xid,
                    };
                    if (record.remoteKey && record.workspaceId &&
                            Number.isInteger(record.windowGeneration)) {
                        await this.clearManagedX11(record, cleanupErrors);
                    }
                }
                if (markers.length > 0 && nonces.size === 1 &&
                        cleanupErrors.length === windowErrorStart) {
                    delete ownedMarkers[markers[0].marker.nonce];
                } else if (trustedOwnership && cleanupErrors.length > windowErrorStart) {
                    try {
                        await this.recordCleanupTombstone({
                            nonce: ownership.nonce,
                            windowId: ownership.window_id,
                            x11WindowId: ownership.xid,
                            statusTabIds: ownership.status_tab_ids || [],
                            pageTabId: ownership.page_tab_id,
                            remoteKey: ownership.binding?.remote_key,
                            workspaceId: ownership.binding?.workspace_id,
                            logicalWindowId: ownership.logical_window_id,
                            logicalPageId: ownership.logical_page_id,
                            windowGeneration: ownership.window_generation,
                            pageGeneration: ownership.page_generation,
                            connectionDisplayName: ownership.binding?.display_title,
                        }, cleanupErrors.slice(windowErrorStart));
                    } catch (error) {
                        cleanupErrors.push(error);
                    }
                }
            }
        }
        await this.localStateWrite(OWNED_MARKERS_KEY, () => ownedMarkers);
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

    legacyRecord(saved) {
        return saved && typeof saved.remoteKey === "string" && saved.remoteKey &&
            typeof saved.principalId === "string" && saved.principalId &&
            typeof saved.workspaceId === "string" && saved.workspaceId &&
            typeof saved.connectionId === "string" && saved.connectionId &&
            validFence(saved.executorFence) &&
            typeof saved.logicalWindowId === "string" && saved.logicalWindowId &&
            typeof saved.logicalPageId === "string" && saved.logicalPageId &&
            Number.isInteger(saved.windowGeneration ?? saved.generation) &&
            (saved.windowGeneration ?? saved.generation) > 0 &&
            Number.isInteger(saved.pageGeneration ?? saved.generation) &&
            (saved.pageGeneration ?? saved.generation) > 0 &&
            Number.isInteger(saved.windowId) && Array.isArray(saved.statusTabIds) &&
            saved.statusTabIds.length === 2 && saved.statusTabIds.every(Number.isInteger) &&
            Number.isInteger(saved.pageTabId) && typeof saved.nonce === "string" &&
            !Number.isInteger(saved.x11WindowId);
    }

    async migrateLegacyRecord(saved, ownedMarkers) {
        if (!this.legacyRecord(saved) || !await this.isLive(saved) ||
                ownedMarkers[saved.nonce] !== true) return null;
        const candidate = {...saved, statusTabIds: [...saved.statusTabIds]};
        try {
            await this.bind(candidate);
            if (!await this.isLive(candidate)) throw new Error("legacy topology changed during bind");
            await this.claimMarker(candidate);
            ownedMarkers[saved.nonce] = this.markerOwnership(candidate);
            return candidate;
        } catch (error) {
            const rollbackErrors = [];
            await this.clearManagedX11(candidate, rollbackErrors);
            delete ownedMarkers[saved.nonce];
            if (rollbackErrors.length > 0) {
                await this.appendCleanupError(rollbackErrors,
                    () => this.recordCleanupTombstone(candidate, rollbackErrors));
            }
            this.reportAdvisoryError(new AggregateError([error, ...rollbackErrors],
                "legacy managed workspace was quarantined"));
            return null;
        }
    }

    async queueInvalidRecordCleanup(saved) {
        if (!saved || typeof saved !== "object" || typeof saved.remoteKey !== "string" ||
                !saved.remoteKey || typeof saved.principalId !== "string" || !saved.principalId ||
                typeof saved.workspaceId !== "string" || !saved.workspaceId ||
                typeof saved.logicalWindowId !== "string" || !saved.logicalWindowId ||
                typeof saved.logicalPageId !== "string" || !saved.logicalPageId) return false;
        const windowGeneration = saved.windowGeneration ?? saved.generation;
        const pageGeneration = saved.pageGeneration ?? saved.generation;
        if (!Number.isInteger(windowGeneration) || windowGeneration < 1 ||
                !Number.isInteger(pageGeneration) || pageGeneration < 1) return false;
        const stable = {
            ...saved,
            windowGeneration,
            pageGeneration,
            connectionId: typeof saved.connectionId === "string" ? saved.connectionId : "unknown",
            executorFence: validFence(saved.executorFence) ? saved.executorFence :
                {executor_id: "recovery", epoch: 1},
            ownerDisplay: saved.ownerDisplay || null,
            nonce: typeof saved.nonce === "string" ? saved.nonce : null,
            statusTabIds: Array.isArray(saved.statusTabIds) ? saved.statusTabIds : [],
            pageTabId: Number.isInteger(saved.pageTabId) ? saved.pageTabId : null,
            windowId: Number.isInteger(saved.windowId) ? saved.windowId : null,
            x11WindowId: Number.isInteger(saved.x11WindowId) ? saved.x11WindowId : null,
        };
        const report = this.topologyReport(stable, this.pageRef(stable));
        report.cleanup.phase = "queued";
        await this.queueTopologyReport(report);
        await this.recordCleanupTombstone(stable,
            [new Error("invalid durable workspace record requires exact recovery cleanup")]);
        return true;
    }

    async restore() {
        await this.waitForNativeBootstrap();
        await this.refreshWindowRegistry();
        await this.restoreHost();
        await this.stageBootstrapLeaseIfNeeded();
        const bootstrapStaging = [...this.bootstrapLeases.values()]
            .map(lease => lease.bootstrapContext?.windowId)
            .filter(Number.isInteger);
        await this.restoreReplacementLedger();
        await this.restoreExecutionLedger();
        await this.restoreOperationCancels();
        await this.restorePendingTransactions();
        await this.restoreManagedCleanupBatch();
        await this.restoreCleanupTombstones();
        await this.restoreRetireLedger();
        await this.restoreTopologyOutbox();
        await this.restoreDeferredTopologyResponses();
        await this.restoreQuarantinedCleanupReports();
        const liveWindowIds = new Set(bootstrapStaging);
        let removed = false;
        const seenKeys = new Set();
        const seenPhysical = new Set();
        const ownedMarkers = this.chrome.storage?.local ?
            (await this.chrome.storage.local.get([OWNED_MARKERS_KEY]))[OWNED_MARKERS_KEY] || {} :
            {};
        this.startupManagedRecords = Object.values(ownedMarkers)
            .filter(ownership => ownership?.version === 2 && ownership.trusted !== false &&
                ownership.phase !== "orphan_unresolved")
            .map(ownership => ({
                remoteKey: ownership.binding?.remote_key,
                principalId: `startup-${ownership.nonce}`,
                workspaceId: ownership.binding?.workspace_id,
                connectionId: "startup-cleanup",
                executorFence: {executor_id: "startup-cleanup", epoch: 1},
                logicalWindowId: ownership.logical_window_id,
                logicalPageId: ownership.logical_page_id,
                windowGeneration: ownership.window_generation,
                pageGeneration: ownership.page_generation,
                windowId: ownership.window_id,
                x11WindowId: ownership.xid,
                statusTabIds: ownership.status_tab_ids,
                pageTabId: ownership.page_tab_id,
                nonce: ownership.nonce,
                connectionDisplayName: ownership.binding?.display_title,
            })).filter(record => record.remoteKey && record.workspaceId &&
                Number.isInteger(record.windowId));
        if (this.chrome.storage?.session) {
            const {abushWorkspaces = [], revision = 0} =
                await this.chrome.storage.session.get(["abushWorkspaces", "revision"]);
            if (Number.isInteger(revision) && revision >= 0) this.persistRevision = revision;
            for (const original of abushWorkspaces) {
                let saved = original;
                if (this.quarantinedCleanupNonces.has(saved?.nonce)) {
                    removed = true;
                    continue;
                }
                if (this.legacyRecord(saved)) {
                    saved = await this.migrateLegacyRecord(saved, ownedMarkers);
                    removed = true;
                    if (saved === null) continue;
                }
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
                    await this.queueInvalidRecordCleanup(saved);
                    removed = true;
                    continue;
                }
                if (!await this.isLive(saved) ||
                        !this.savedOwnershipMatches(ownedMarkers[saved.nonce], saved)) {
                    await this.queueInvalidRecordCleanup(saved);
                    removed = true;
                    continue;
                }
                const key = this.key(saved.remoteKey, saved.principalId);
                const physical = [saved.windowId, saved.pageTabId, ...saved.statusTabIds];
                if (seenKeys.has(key) || physical.some(id => seenPhysical.has(id))) {
                    await this.queueInvalidRecordCleanup(saved);
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
        await this.restoreReplacementLedger({consume: true});
        if (this.bootstrapLeases.size > 0) {
            for (const record of this.startupManagedRecords) {
                try {
                    const ownership = await this.currentOwnedMarker(record.nonce);
                    if (ownership && this.savedOwnershipMatches(ownership, record) &&
                            await this.isLive(record)) {
                        this.install(record);
                    }
                } catch (error) {
                    this.reportAdvisoryError(new AggregateError([error],
                        "startup managed window could not be verified before cold cleanup"));
                }
            }
            await this.cleanupStaleManagedBeforeBootstrap();
        }
        await this.closeOrphanedStatusWindows(liveWindowIds, ownedMarkers);
        await this.executeBootstrapLeases();
        this.startupManagedRecords = [];
        if (removed) await this.persist();
        await this.convergeRestoredTopologyOutbox();
        const normalWindows = await this.chrome.windows.getAll({windowTypes: ["normal"]});
        await Promise.all(normalWindows.map(window => this.rememberWindowSnapshot(window.id)));
        await this.refreshWindowRegistry();
        await this.primeRecentlyClosedSessions();
        await this.initializeOrdinaryMru();
        await this.flushExecutionLedger();
    }

    install(record) {
        const key = this.key(record.remoteKey, record.principalId);
        this.workspaces.set(key, record);
        this.windowOwners.set(record.windowId, key);
        const registry = this.windowRegistry.get(record.windowId);
        this.windowRegistry.set(record.windowId, registry ? {...registry, kind: "managed"} : {
            windowId: record.windowId,
            kind: "managed",
            snapshot: null,
            tabIds: [record.pageTabId, ...record.statusTabIds],
            revision: this.windowRegistryRevision,
        });
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
        const operation = this.nativeMutationTail.catch(() => {}).then(async () => {
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
        });
        this.nativeMutationTail = operation;
        return operation;
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

    async withFlightLock(flight, task) {
        const previous = flight.transactionTail || Promise.resolve();
        const operation = previous.catch(() => {}).then(task);
        flight.transactionTail = operation;
        return operation;
    }

    async boundedFlightTerminal(flight) {
        let timer;
        try {
            return await Promise.race([
                flight.promise.then(record => ({record, error: null}),
                    error => ({record: null, error})),
                new Promise((_, reject) => {
                    timer = setTimeout(() => reject(
                        new Error("cancelled ensure did not reach a terminal state")),
                    TRANSACTION_WAIT_MS);
                }),
            ]);
        } finally {
            if (timer !== undefined) clearTimeout(timer);
        }
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
                committed: false, cleanupCompleted: false, expectedPage: payload.expected_page,
                transactionTail: Promise.resolve(), transaction: null};
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

    retireIdentity(payload) {
        return {
            remote_key: payload.remote_key,
            connection_id: payload.connection_id,
            principal_id: payload.principal_id,
            request_id: payload.request_id,
            retire_id: payload.retire_id,
            ensure_id: payload.ensure_id,
            executor_fence: payload.executor_fence,
            origin_executor_fence: payload.origin_executor_fence,
            window_id: payload.window_id,
            page_id: payload.page_id,
        };
    }

    stableRetireMatches(left, right) {
        return Boolean(left && right && left.remote_key === right.remote_key &&
            left.principal_id === right.principal_id && left.retire_id === right.retire_id &&
            left.ensure_id === right.ensure_id && left.window_id === right.window_id &&
            left.page_id === right.page_id &&
            sameRef(left.origin_executor_fence, right.origin_executor_fence));
    }

    retireDelivery(identity, state) {
        return {
            ...identity,
            connection_id: state.connection_id,
            request_id: crypto.randomUUID(),
            executor_fence: state.executor_fence,
        };
    }

    retireLedgerSnapshot() {
        const snapshot = [];
        for (const identity of this.retirePending.values()) {
            snapshot.push({version: 8, phase: "accepted", identity});
        }
        for (const entry of this.retireEffectStarted.values()) {
            snapshot.push({version: 8, phase: "effect_started", ...entry});
        }
        for (const entry of this.retireCompleted.values()) {
            snapshot.push({version: 8, phase: "completed", ...entry});
        }
        for (const entry of this.retireResponseReceived.values()) {
            snapshot.push({version: 8, phase: "response_received", ...entry});
        }
        for (const entry of this.retireConsumedPending.values()) {
            snapshot.push({version: 8, phase: "consumed_pending", ...entry});
        }
        for (const entry of this.retireServerConfirmed.values()) {
            snapshot.push({version: 8, phase: "server_confirmed", ...entry});
        }
        for (const [key, identity] of this.retireLegacyCleanup) {
            snapshot.push({version: 8, phase: "legacy_cleanup", key, identity});
        }
        return snapshot;
    }

    async persistRetireLedger() {
        await this.localStateWrite(RETIRE_LEDGER_KEY, () => this.retireLedgerSnapshot());
    }

    legacyRetireKey(identity) {
        return identity.retire_id || identity.request_id || JSON.stringify([
            identity.remote_key, identity.principal_id, identity.window_id, identity.page_id,
        ]);
    }

    validLegacyRetireIdentity(identity) {
        return identity && ["remote_key", "principal_id", "window_id", "page_id"].every(field =>
            typeof identity[field] === "string" && identity[field]);
    }

    validRetireIdentity(identity) {
        return identity && ["remote_key", "connection_id", "principal_id", "request_id",
            "retire_id", "ensure_id", "window_id", "page_id"].every(field =>
            typeof identity[field] === "string" && identity[field]) &&
            validFence(identity.executor_fence) &&
            validFence(identity.origin_executor_fence);
    }

    validRetireCompletion(completion) {
        return exactFields(completion, ["outcome"]) &&
            ["retired", "already_retired"].includes(completion.outcome);
    }

    validRetireAck(ack, entry, requireDelivery = true) {
        return exactFields(ack, ["request_id", "retire_id", "executor_fence",
            "response_delivery_id", "origin_executor_fence", "completion"]) &&
            (!requireDelivery || (ack.request_id === entry.delivery.request_id &&
                sameRef(ack.executor_fence, entry.delivery.executor_fence))) &&
            ack.retire_id === entry.identity.retire_id &&
            sameRef(ack.origin_executor_fence, entry.identity.origin_executor_fence) &&
            sameRef(ack.completion, entry.completion) &&
            typeof ack.response_delivery_id === "string" && ack.response_delivery_id;
    }

    validRetireConsumedAck(ack, entry) {
        return exactFields(ack, ["request_id", "retire_id", "executor_fence",
            "response_delivery_id"]) &&
            ack.request_id === entry.consumed_request_id &&
            ack.retire_id === entry.ack.retire_id &&
            sameRef(ack.executor_fence, entry.delivery.executor_fence) &&
            ack.response_delivery_id === entry.ack.response_delivery_id;
    }

    async restoreRetireLedger() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([RETIRE_LEDGER_KEY]);
        const entries = Array.isArray(local[RETIRE_LEDGER_KEY]) ? local[RETIRE_LEDGER_KEY] : [];
        const legacyCleanup = [];
        let dirty = false;
        for (const entry of entries) {
            const legacyV7 = entry?.version === 7;
            if (legacyV7 && ["accepted", "effect_started", "completed", "legacy_cleanup"].includes(
                entry.phase)) dirty = true;
            if (entry?.version !== 8 && !legacyV7) {
                const legacy = entry?.identity?.original || entry?.identity;
                if (this.validLegacyRetireIdentity(legacy)) {
                    const key = this.legacyRetireKey(legacy);
                    this.retireLegacyCleanup.set(key, legacy);
                    legacyCleanup.push(legacy);
                }
                dirty = true;
                continue;
            }
            const identity = entry.identity;
            if (entry.phase === "legacy_cleanup" &&
                    this.validLegacyRetireIdentity(identity) &&
                    entry.key === this.legacyRetireKey(identity)) {
                this.retireLegacyCleanup.set(entry.key, identity);
                legacyCleanup.push(identity);
                continue;
            }
            if (!this.validRetireIdentity(identity)) {
                dirty = true;
                continue;
            }
            if (entry.phase === "accepted") {
                this.retirePending.set(identity.retire_id, identity);
            } else if (entry.phase === "effect_started" &&
                    this.validRetireIdentity(entry.delivery)) {
                this.retireEffectStarted.set(identity.retire_id, entry);
            } else if (entry.phase === "completed" &&
                    this.validRetireIdentity(entry.delivery) &&
                    this.validRetireCompletion(entry.completion)) {
                this.retireCompleted.set(identity.retire_id, entry);
            } else if ((entry.phase === "response_received" ||
                    legacyV7 && entry.phase === "ack_received") &&
                    this.validRetireIdentity(entry.delivery) &&
                    this.validRetireCompletion(entry.completion) &&
                    this.validRetireAck(entry.ack, entry) &&
                    typeof (entry.ack_connection_id || entry.delivery.connection_id) === "string") {
                const received = legacyV7 ? {...entry, version: 8,
                    phase: "response_received", consumed_request_id: null,
                    consumed_migration: "legacy_unsent",
                    ack_connection_id: entry.delivery.connection_id} : entry;
                this.retireResponseReceived.set(identity.retire_id, received);
                if (legacyV7) dirty = true;
            } else if (entry.phase === "consumed_pending" &&
                    this.validRetireIdentity(entry.delivery) &&
                    this.validRetireCompletion(entry.completion) &&
                    this.validRetireAck(entry.ack, entry, false) &&
                    typeof entry.ack_connection_id === "string" && entry.ack_connection_id &&
                    typeof entry.consumed_request_id === "string" &&
                    entry.consumed_request_id) {
                this.retireConsumedPending.set(identity.retire_id, entry);
            } else if (entry.phase === "server_confirmed" &&
                    this.validRetireIdentity(entry.delivery) &&
                    this.validRetireCompletion(entry.completion) &&
                    this.validRetireAck(entry.ack, entry, false) &&
                    typeof entry.ack_connection_id === "string" && entry.ack_connection_id &&
                    typeof entry.consumed_request_id === "string" &&
                    entry.consumed_request_id && this.validRetireConsumedAck(
                        entry.consumed_ack, entry)) {
                this.retireServerConfirmed.set(identity.retire_id, entry);
            } else {
                dirty = true;
            }
        }
        this.retireRecoveries = legacyCleanup;
        if (dirty) await this.persistRetireLedger();
    }

    async recoverLegacyRetire(identity) {
        const record = this.workspaces.get(this.key(identity.remote_key, identity.principal_id));
        if (record && record.logicalWindowId === identity.window_id &&
                record.logicalPageId === identity.page_id) {
            await this.convergeManagedTopology(record, {closeWindow: true, report: false});
        }
        this.retireLegacyCleanup.delete(this.legacyRetireKey(identity));
        await this.persistRetireLedger();
    }

    async recoverRetireLedger() {
        if (!this.retireRecoveryReady) return;
        const legacy = this.retireRecoveries.splice(0);
        for (const identity of legacy) {
            try {
                await this.recoverLegacyRetire(identity);
            } catch (error) {
                this.retireRecoveries.push(identity);
                this.reportAdvisoryError(new AggregateError([error],
                    "legacy retire cleanup remains pending"));
            }
        }
        for (const entry of [...this.retireEffectStarted.values()]) {
            if (entry.identity.remote_key !== this.retireRecoveryState.remote_key ||
                    entry.identity.principal_id !== this.retireRecoveryState.principal_id) continue;
            const record = this.workspaces.get(this.key(
                entry.identity.remote_key, entry.identity.principal_id));
            if (record && record.logicalWindowId === entry.identity.window_id &&
                    record.logicalPageId === entry.identity.page_id) {
                this.reportAdvisoryError(new Error(
                    "retire effect started but physical topology remains; completion is withheld"));
                continue;
            }
            this.retireEffectStarted.delete(entry.identity.retire_id);
            this.retireCompleted.set(entry.identity.retire_id, {
                identity: entry.identity,
                delivery: this.retireDelivery(entry.identity, this.retireRecoveryState),
                completion: {outcome: "retired"},
            });
            await this.persistRetireLedger();
        }
        for (const identity of [...this.retirePending.values()]) {
            if (identity.remote_key !== this.retireRecoveryState.remote_key ||
                    identity.principal_id !== this.retireRecoveryState.principal_id) continue;
            try {
                await this.finishRetire(identity);
            } catch (error) {
                this.reportAdvisoryError(new AggregateError([error],
                    "accepted retire request remains pending recovery"));
            }
        }
        await this.flushRetireLedger();
    }

    async retireManagedPage(payload) {
        return this.enqueueLifecycle(() => this.retireManagedPageSerialized(payload));
    }

    async retireManagedPageSerialized(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "retire_id", "ensure_id", "executor_fence", "origin_executor_fence", "window_id",
            "page_id"])) return false;
        const identity = this.retireIdentity(payload);
        if (!this.validRetireIdentity(identity)) return false;
        this.retireRecoveryReady = true;
        this.retireRecoveryState = {
            remote_key: identity.remote_key,
            connection_id: identity.connection_id,
            principal_id: identity.principal_id,
            executor_fence: identity.executor_fence,
            connected: true,
        };
        this.remoteAuthorities.set(identity.remote_key,
            structuredClone(this.retireRecoveryState));
        const known = this.retirePending.get(identity.retire_id) ||
            this.retireEffectStarted.get(identity.retire_id)?.identity ||
            this.retireCompleted.get(identity.retire_id)?.identity ||
            this.retireResponseReceived.get(identity.retire_id)?.identity ||
            this.retireConsumedPending.get(identity.retire_id)?.identity ||
            this.retireServerConfirmed.get(identity.retire_id)?.identity ||
            this.retireConfirmedIdentities.get(identity.retire_id);
        if (known && !this.stableRetireMatches(known, identity)) {
            throw new Error("conflicting duplicate retire request");
        }
        if (!known) {
            this.retirePending.set(identity.retire_id, identity);
            await this.persistRetireLedger();
        }
        if (this.retireConfirmedIdentities.has(identity.retire_id)) return true;
        if (this.retireServerConfirmed.has(identity.retire_id)) {
            await this.removeConfirmedRetire(identity.retire_id);
            return true;
        }
        if (this.retireResponseReceived.has(identity.retire_id) ||
                this.retireConsumedPending.has(identity.retire_id)) {
            await this.flushRetireLedger();
            return true;
        }
        if (this.retireEffectStarted.has(identity.retire_id)) {
            await this.recoverRetireLedger();
            await this.flushRetireLedger();
            return true;
        }
        if (this.retireCompleted.has(identity.retire_id)) {
            await this.flushRetireLedger();
            return true;
        }
        await this.finishRetire(identity);
        await this.flushRetireLedger();
        return true;
    }

    async finishRetire(identity) {
        const record = this.workspaces.get(this.key(identity.remote_key, identity.principal_id));
        let completion = {outcome: "already_retired"};
        if (record && record.logicalWindowId === identity.window_id &&
                record.logicalPageId === identity.page_id) {
            const state = this.retireRecoveryState;
            if (!state || state.remote_key !== identity.remote_key ||
                    state.principal_id !== identity.principal_id) return;
            const delivery = this.retireDelivery(identity, state);
            this.retirePending.delete(identity.retire_id);
            this.retireEffectStarted.set(identity.retire_id, {identity, delivery});
            await this.persistRetireLedger();
            await this.convergeManagedTopology(record, {closeWindow: true, report: false});
            this.retireEffectStarted.delete(identity.retire_id);
            completion = {outcome: "retired"};
        }
        const state = this.retireRecoveryState;
        if (!state || state.remote_key !== identity.remote_key ||
                state.principal_id !== identity.principal_id) return;
        const delivery = this.retireDelivery(identity, state);
        this.retirePending.delete(identity.retire_id);
        this.retireCompleted.set(identity.retire_id, {identity, delivery, completion});
        await this.persistRetireLedger();
    }

    async flushRetireLedger() {
        const flush = this.retireFlushTail.catch(() => {}).then(
            () => this.flushRetireLedgerOnce());
        this.retireFlushTail = flush;
        return flush;
    }

    async flushRetireLedgerOnce() {
        const state = this.retireRecoveryState;
        if (!state) return;
        const responseRetries = [...this.retireResponseReceived.values()];
        const consumedRetries = [...this.retireConsumedPending.values()];
        for (const [retireId, original] of [...this.retireCompleted]) {
            if (original.identity.remote_key !== state.remote_key ||
                    original.identity.principal_id !== state.principal_id) continue;
            let entry = original;
            if (entry.delivery.connection_id !== state.connection_id ||
                    !sameRef(entry.delivery.executor_fence, state.executor_fence)) {
                entry = {...entry, delivery: this.retireDelivery(entry.identity, state)};
                this.retireCompleted.set(retireId, entry);
                await this.persistRetireLedger();
            }
            try {
                const ack = await this.nativeRequest(
                    "abush_browser", "retire_managed_page_completion", {
                        remote_key: entry.delivery.remote_key,
                        connection_id: entry.delivery.connection_id,
                        principal_id: entry.delivery.principal_id,
                        request_id: entry.delivery.request_id,
                        retire_id: entry.delivery.retire_id,
                        executor_fence: entry.delivery.executor_fence,
                        origin_executor_fence: entry.identity.origin_executor_fence,
                        completion: entry.completion,
                    });
                if (!this.validRetireAck(ack, entry)) {
                    throw new Error("retire acknowledgement was not canonical");
                }
                const received = {...entry,
                    ack_connection_id: entry.delivery.connection_id, ack};
                this.retireCompleted.delete(retireId);
                this.retireResponseReceived.set(retireId, received);
                await this.persistRetireLedger();
                await this.consumeRetireAck(received);
            } catch (error) {
                this.reportAdvisoryError(new AggregateError([error],
                    "retire completion remains pending acknowledgement"));
            }
        }
        for (const entry of [...responseRetries, ...consumedRetries]) {
            if (entry.identity.remote_key !== state.remote_key ||
                    entry.identity.principal_id !== state.principal_id) continue;
            try {
                if (this.retireResponseReceived.get(entry.identity.retire_id) === entry ||
                        this.retireConsumedPending.get(entry.identity.retire_id) === entry) {
                    await this.consumeRetireAck(entry);
                }
            } catch (error) {
                this.reportAdvisoryError(new AggregateError([error],
                    "retire acknowledgement remains pending consumption"));
            }
        }
        for (const retireId of [...this.retireServerConfirmed.keys()]) {
            await this.removeConfirmedRetire(retireId);
        }
    }

    async removeConfirmedRetire(retireId) {
        const entry = this.retireServerConfirmed.get(retireId);
        if (!entry) return;
        await this.persistRetireLedger();
        if (this.retireServerConfirmed.get(retireId) !== entry) return;
        this.retireConfirmedIdentities.set(retireId, entry.identity);
        while (this.retireConfirmedIdentities.size > 256) {
            this.retireConfirmedIdentities.delete(this.retireConfirmedIdentities.keys().next().value);
        }
        this.retireServerConfirmed.delete(retireId);
        try {
            await this.persistRetireLedger();
        } catch (error) {
            this.retireServerConfirmed.set(retireId, entry);
            throw error;
        }
    }

    async consumeRetireAck(original) {
        const state = this.remoteAuthorities.get(original.identity.remote_key);
        if (!state || state.connected !== true ||
                state.principal_id !== original.identity.principal_id ||
                typeof state.connection_id !== "string" || !validFence(state.executor_fence)) {
            throw new Error("retire acknowledgement has no current peer authority");
        }
        let entry = original;
        const authorityChanged = entry.ack_connection_id !== state.connection_id ||
            !sameRef(entry.delivery.executor_fence, state.executor_fence);
        if (authorityChanged) {
            entry = {
                ...entry,
                ack_connection_id: state.connection_id,
                delivery: {...entry.delivery, connection_id: state.connection_id,
                    executor_fence: state.executor_fence},
            };
            if (this.retireResponseReceived.get(entry.identity.retire_id) === original) {
                this.retireResponseReceived.set(entry.identity.retire_id, entry);
            } else if (this.retireConsumedPending.get(entry.identity.retire_id) === original) {
                this.retireConsumedPending.set(entry.identity.retire_id, entry);
            }
            await this.persistRetireLedger();
        }
        if (this.retireResponseReceived.get(entry.identity.retire_id) === entry) {
            entry = {...entry, consumed_request_id: crypto.randomUUID(),
                consumed_migration: null};
            this.retireResponseReceived.delete(entry.identity.retire_id);
            this.retireConsumedPending.set(entry.identity.retire_id, entry);
            await this.persistRetireLedger();
        }
        if (this.retireConsumedPending.get(entry.identity.retire_id) !== entry) return;
        const confirmation = await this.nativeRequest(
            "abush_browser", "retire_managed_page_ack_consumed", {
                remote_key: entry.identity.remote_key,
                connection_id: entry.ack_connection_id,
                principal_id: entry.identity.principal_id,
                request_id: entry.consumed_request_id,
                retire_id: entry.ack.retire_id,
                executor_fence: entry.delivery.executor_fence,
                response_delivery_id: entry.ack.response_delivery_id,
            });
        if (!this.validRetireConsumedAck(confirmation, entry)) {
            throw new Error("retire acknowledgement consumption was not confirmed");
        }
        if (this.retireConsumedPending.get(entry.identity.retire_id) !== entry) return;
        const confirmed = {...entry, consumed_ack: confirmation};
        this.retireConsumedPending.delete(entry.identity.retire_id);
        this.retireServerConfirmed.set(entry.identity.retire_id, confirmed);
        await this.persistRetireLedger();
        await this.removeConfirmedRetire(entry.identity.retire_id);
    }

    async retireManagedPageAck(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "retire_id", "executor_fence", "response_delivery_id", "origin_executor_fence",
            "completion"])) return false;
        const ack = {
            request_id: payload.request_id,
            retire_id: payload.retire_id,
            executor_fence: payload.executor_fence,
            response_delivery_id: payload.response_delivery_id,
            origin_executor_fence: payload.origin_executor_fence,
            completion: payload.completion,
        };
        const entry = this.retireCompleted.get(ack.retire_id) ||
            this.retireResponseReceived.get(ack.retire_id) ||
            this.retireConsumedPending.get(ack.retire_id);
        if (!entry || payload.remote_key !== entry.identity.remote_key ||
                payload.principal_id !== entry.identity.principal_id ||
                !this.validRetireAck(ack, entry, false) ||
                (entry.ack && entry.ack.response_delivery_id !== ack.response_delivery_id)) {
            return false;
        }
        if (entry.consumed_request_id) {
            const state = this.remoteAuthorities.get(payload.remote_key);
            if (!state || state.connected !== true ||
                    state.principal_id !== payload.principal_id) return false;
            const pending = {
                ...entry,
                ack_connection_id: state.connection_id,
                delivery: {...entry.delivery, connection_id: state.connection_id,
                    executor_fence: state.executor_fence},
                ack,
            };
            this.retireConsumedPending.set(ack.retire_id, pending);
            await this.persistRetireLedger();
            await this.consumeRetireAck(pending);
            return true;
        }
        const state = this.remoteAuthorities.get(payload.remote_key);
        if (!state || state.connection_id !== payload.connection_id ||
                !sameRef(state.executor_fence, payload.executor_fence)) return false;
        const received = {
            ...entry,
            ack_connection_id: payload.connection_id,
            ack,
        };
        this.retireCompleted.delete(ack.retire_id);
        this.retireResponseReceived.set(ack.retire_id, received);
        await this.persistRetireLedger();
        await this.consumeRetireAck(received);
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
        const flight = waiter.flight;
        const terminal = this.boundedFlightTerminal(flight);
        let cancellationOwnsFlight = false;
        waiter.cancelled = true;
        await this.withFlightLock(flight, async () => {
            cancellationOwnsFlight = this.activeEnsureWaiters(flight).length === 0;
            if (cancellationOwnsFlight) flight.cancelled = true;
        });
        const settled = await terminal;
        if (cancellationOwnsFlight && settled.record &&
                this.workspaces.get(this.key(
                    settled.record.remoteKey, settled.record.principalId)) === settled.record) {
            await this.withFlightLock(flight, () => this.convergeManagedTopology(
                settled.record, {closeWindow: true, report: false}));
        }
        if (settled.error && !flight.cleanupCompleted && flight.createdWindowId !== null) {
            throw settled.error;
        }
        await this.ensureCancellationCompleted(payload);
        return true;
    }

    async cleanupFlightWindow(flight) {
        if (flight.createdWindowId === null || flight.cleanupCompleted) return;
        try {
            await this.chrome.windows.remove(flight.createdWindowId);
        } catch (error) {
            if (!await this.windowIsMissing(flight.createdWindowId)) throw error;
        }
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
        if (errors.length > 0) {
            try {
                await this.recordCleanupTombstone({
                    nonce, windowId, statusTabIds: statusTabs.map(tab => tab.id),
                    pageTabId: null,
                }, errors);
            } catch (error) {
                errors.push(error);
            }
        } else {
            try {
                await this.disownMarker(nonce);
            } catch (error) {
                errors.push(error);
                try {
                    await this.recordCleanupTombstone({
                        nonce, windowId, statusTabIds: statusTabs.map(tab => tab.id),
                        pageTabId: null,
                    }, errors);
                } catch (tombstoneError) {
                    errors.push(tombstoneError);
                }
            }
        }
        if (errors.length > 0) {
            const failure = new AggregateError(errors,
                "managed window preparation cleanup is uncertain");
            failure.browserCode = "unknown_effect";
            throw failure;
        }
    }

    async mutateTransaction(transaction, effects, effect) {
        const expectedEvents = Array.isArray(effects) ? effects : [effects];
        for (const {type, match} of expectedEvents) {
            this.expectTransactionEvent(transaction, type, match);
        }
        const planned = expectedEvents[0]?.planned || {kind: expectedEvents[0]?.type};
        transaction.plannedEffect = structuredClone(planned);
        transaction.phase = "effect_planned";
        await this.persistPendingTransactions();
        let result;
        try {
            result = await effect();
        } catch (error) {
            try {
                await this.reconcileTransactionTopology(transaction);
                await this.persistPendingTransactions();
            } catch (reconcileError) {
                throw new AggregateError([error, reconcileError],
                    "managed effect failed and WAL reconciliation is uncertain");
            }
            throw error;
        }
        const observation = {
            kind: planned.kind,
            window_id: result?.windowId ?? result?.id ?? transaction.windowId ?? null,
            tab_id: result?.windowId !== undefined ? result?.id : null,
        };
        transaction.observedEffects.push(observation);
        transaction.plannedEffect = null;
        transaction.phase = "effect_observed";
        await this.reconcileTransactionTopology(transaction);
        await this.persistPendingTransactions();
        if (transaction.invalid) throw new Error("managed transaction topology changed");
        return result;
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
            const previous = {...existing, statusTabIds: [...existing.statusTabIds]};
            const candidate = {
                ...existing,
                workspaceId: payload.workspace_id,
                connectionId: payload.connection_id,
                executorFence: payload.executor_fence,
                ownerDisplay: payload.owner_display,
                connectionDisplayName: payload.connection_display_name,
            };
            let committed = false;
            try {
                this.ensureActive(flight);
                await this.bind(candidate, payload.workspace_id);
                this.ensureActive(flight);
                if (!await this.isLive(existing)) throw new Error("workspace changed during bind");
                await this.withFlightLock(flight, async () => {
                    this.ensureActive(flight);
                    await this.commitRecord(candidate, existing);
                    committed = true;
                });
                await this.claimMarker(existing);
            } catch (error) {
                const rollbackErrors = [];
                const durableNew = error.durableCommit === "new";
                if (durableNew) committed = true;
                Object.assign(existing, previous);
                if (committed) {
                    await this.appendCleanupError(rollbackErrors,
                        () => this.persistRecords([...this.workspaces.values()].map(saved =>
                            this.persistedRecord(saved === existing ? previous : saved))));
                }
                await this.appendCleanupError(rollbackErrors, () => this.bind(existing));
                await this.appendCleanupError(rollbackErrors, () => this.claimMarker(existing));
                if (rollbackErrors.length > 0) {
                    await this.appendCleanupError(rollbackErrors,
                        () => this.recordCleanupTombstone(existing, rollbackErrors));
                    this.forget(existing);
                    await this.appendCleanupError(rollbackErrors, () => this.persist());
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
        const transaction = await this.beginEnsureTransaction(payload, nonce);
        flight.transaction = transaction;
        await this.ownMarker(nonce);
        const hostWindowId = await this.normalWindowId();
        await this.updateEnsureTransaction(transaction, {hostWindowId, phase: "preparing"});
        const statusTabs = [];
        let created;
        try {
            for (const marker of MARKER_IDS) {
                const url = this.statusUrl({nonce}, marker);
                const tab = await this.mutateTransaction(transaction, {
                    type: "tabCreated", match: {tab: {windowId: hostWindowId, url}},
                    planned: {kind: "tabs.create.marker", window_id: hostWindowId,
                        marker, url},
                }, () => this.chrome.tabs.create({
                        windowId: hostWindowId,
                        url,
                        active: false,
                    }));
                statusTabs.push(tab);
                transaction.statusTabIds = statusTabs.map(statusTab => statusTab.id);
                await this.updateEnsureTransaction(transaction, {
                    statusTabIds: [...transaction.statusTabIds],
                });
                await this.waitForStatusTitle(tab.id, nonce, marker);
            }
            created = await this.mutateTransaction(transaction, {
                type: "windowCreated",
                match: {window: {type: "normal"}, sourceTabId: statusTabs[0].id},
                planned: {kind: "windows.create", tab_id: statusTabs[0].id},
            }, () => this.chrome.windows.create({
                    tabId: statusTabs[0].id,
                    focused: false,
                    type: "normal",
                }));
            statusTabs[0].windowId = created.id;
            const firstStatus = await this.chrome.tabs.get(statusTabs[0].id);
            statusTabs[0].windowType = firstStatus.windowType;
            if (!created.tabs) created.tabs = [firstStatus];
            transaction.windowId = created.id;
            flight.createdWindowId = created.id;
            await this.updateEnsureTransaction(transaction, {
                windowId: created.id,
                phase: "window_created",
            });
            await this.mutateTransaction(transaction, [{
                type: "tabDetached",
                match: {tabId: statusTabs[1].id, oldWindowId: hostWindowId},
                planned: {kind: "tabs.move", tab_id: statusTabs[1].id,
                    window_id: created.id},
            }, {
                type: "tabAttached",
                match: {tabId: statusTabs[1].id, newWindowId: created.id},
            }], () => this.chrome.tabs.move(statusTabs[1].id, {
                windowId: created.id,
                index: -1,
            }));
            statusTabs[1].windowId = created.id;
        } catch (error) {
            try {
                await this.cleanupPreparedWindow(created?.id, statusTabs, nonce);
                await this.finishEnsureTransaction(transaction, "rolled_back");
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
                await this.mutateTransaction(transaction, {
                    type: "tabUpdated",
                    match: {tabId: tab.id, changeInfo: {pinned: true}},
                    planned: {kind: "tabs.pin", tab_id: tab.id,
                        window_id: created.id},
                }, () => this.chrome.tabs.update(tab.id, {pinned: true}));
            }
            const business = await this.mutateTransaction(transaction, {
                type: "tabCreated",
                match: {tab: {windowId: created.id, url: "about:blank"}},
                planned: {kind: "tabs.create.business", window_id: created.id,
                    url: "about:blank"},
            }, () => this.chrome.tabs.create({
                windowId: created.id, url: "about:blank", active: true,
            }));
            transaction.pageTabId = business.id;
            await this.updateEnsureTransaction(transaction, {
                pageTabId: business.id,
                phase: "physical_ready",
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
            transaction.record = this.persistedRecord(record);
            transaction.binding = this.bindingPayload(record);
            await this.updateEnsureTransaction(transaction, {
                record: transaction.record,
                binding: transaction.binding,
                phase: "record_ready",
            });
            try {
                this.ensureActive(flight);
                await this.bind(record);
                transaction.x11WindowId = record.x11WindowId;
                transaction.record = this.persistedRecord(record);
                await this.updateEnsureTransaction(transaction, {
                    x11WindowId: record.x11WindowId,
                    record: transaction.record,
                    phase: "bound",
                });
                await this.claimMarker(record);
                await this.updateEnsureTransaction(transaction, {phase: "claimed"});
                this.ensureActive(flight);
                if (transaction.invalid || !await this.isLive(record)) {
                    throw new Error("workspace changed during bind");
                }
                await this.withFlightLock(flight, async () => {
                    this.ensureActive(flight);
                    if (transaction.invalid) throw new Error("managed transaction changed");
                    await this.commitRecord(record);
                    flight.committed = true;
                    await this.finishEnsureTransaction(transaction);
                });
                return record;
            } catch (error) {
                const rollbackErrors = [];
                if (error.durableCommit === "new") {
                    await this.appendCleanupError(rollbackErrors,
                        () => this.persistWithoutRecords([record]));
                } else {
                    this.forget(record);
                    await this.appendCleanupError(rollbackErrors, () => this.persist());
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
            try {
                await this.finishEnsureTransaction(transaction, "rolled_back");
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

    operationEffectIdentity(payload) {
        return {
            ...this.operationIdentity(payload),
            command: structuredClone(payload.command),
        };
    }

    operationEffectKey(payload) {
        return JSON.stringify([
            payload.remote_key,
            payload.principal_id,
            payload.effect_id,
            payload.executor_fence,
        ]);
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

    operationExecutionFromCancel(payload) {
        const execution = {...payload, request_id: payload.execution_request_id};
        delete execution.execution_request_id;
        return execution;
    }

    operationLedgerSnapshot() {
        return [...this.executionLedger.values()].map(entry => structuredClone(entry));
    }

    async persistExecutionLedger() {
        await this.localStateWrite(EXECUTION_LEDGER_KEY, () => this.operationLedgerSnapshot());
    }

    validExecutionLedgerEntry(entry) {
        return entry?.version === 1 && ["accepted", "effect_started", "terminal_pending_ack",
            "acked"].includes(entry.phase) && entry.identity &&
            typeof entry.completion !== "undefined" &&
            exactFields(entry.identity, ["remote_key", "connection_id", "principal_id",
                "request_id", "effect_id", "executor_fence", "command"]) &&
            validFence(entry.identity.executor_fence);
    }

    async restoreExecutionLedger() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([EXECUTION_LEDGER_KEY]);
        const saved = Array.isArray(local[EXECUTION_LEDGER_KEY]) ? local[EXECUTION_LEDGER_KEY] : [];
        let dirty = false;
        for (const entry of saved) {
            if (!this.validExecutionLedgerEntry(entry)) {
                dirty = true;
                continue;
            }
            const key = this.operationEffectKey(entry.identity);
            const existing = this.executionLedger.get(key);
            if (existing && !sameRef(existing.identity, entry.identity)) {
                dirty = true;
                continue;
            }
            const restored = entry.phase === "accepted" ? {
                ...entry, phase: "terminal_pending_ack", completion: "unknown",
            } : entry.phase === "effect_started" ? {
                ...entry, phase: "terminal_pending_ack", completion: "unknown",
            } : entry;
            this.executionLedger.set(key, restored);
            if (!sameRef(restored, entry)) dirty = true;
        }
        if (dirty) await this.persistExecutionLedger();
    }

    async flushExecutionLedger() {
        const flush = this.executionFlushTail.catch(() => {})
            .then(() => this.flushExecutionLedgerSerialized());
        this.executionFlushTail = flush;
        return flush;
    }

    async flushExecutionLedgerSerialized() {
        for (const [effectKey, original] of [...this.executionLedger]) {
            let entry = original;
            if (entry.phase === "terminal_pending_ack") {
                try {
                    await this.completion(entry.identity, entry.completion);
                    if (this.executionLedger.get(effectKey) !== entry) continue;
                    entry = {...entry, phase: "acked"};
                    this.executionLedger.set(effectKey, entry);
                    await this.persistExecutionLedger();
                } catch (error) {
                    this.reportAdvisoryError(new AggregateError([error],
                        "executor completion remains pending acknowledgement"));
                    continue;
                }
            }
            if (entry.phase !== "acked") continue;
            const operationKey = this.operationKey(entry.identity);
            let cancellations = this.pendingOperationCancels.get(operationKey) || [];
            while (cancellations.length > 0) {
                const [payload, ...remaining] = cancellations;
                const completion = entry.completion === "not_applied" ?
                    "not_applied" : "unknown";
                try {
                    await this.cancellationCompletion(payload, completion);
                } catch (error) {
                    this.reportAdvisoryError(new AggregateError([error],
                        "cancellation completion remains pending acknowledgement"));
                    break;
                }
                if (remaining.length > 0) {
                    this.pendingOperationCancels.set(operationKey, remaining);
                } else {
                    this.pendingOperationCancels.delete(operationKey);
                }
                await this.persistOperationCancels();
                cancellations = remaining;
            }
            if (cancellations.length > 0) continue;
            if (this.executionLedger.get(effectKey) !== entry) continue;
            this.executionLedger.delete(effectKey);
            await this.persistExecutionLedger();
            this.rememberCompletedOperation(operationKey, {
                ...entry.identity,
                payload: entry.identity,
                mutationStarted: entry.completion !== "not_applied",
                committed: entry.completion === "applied",
                uncertain: entry.completion === "unknown",
                completion: entry.completion,
            });
        }
    }

    operationCancelSnapshot() {
        const pending = [];
        for (const payloads of this.pendingOperationCancels.values()) {
            pending.push(...payloads.map(payload => structuredClone(payload)));
        }
        for (const operation of this.operations.values()) {
            pending.push(...(operation.cancellationPayloads || [])
                .map(payload => structuredClone(payload)));
        }
        return pending;
    }

    async persistOperationCancels() {
        await this.localStateWrite(PENDING_OPERATIONS_KEY, () => this.operationCancelSnapshot());
    }

    async restoreOperationCancels() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([PENDING_OPERATIONS_KEY]);
        const saved = Array.isArray(local[PENDING_OPERATIONS_KEY]) ?
            local[PENDING_OPERATIONS_KEY] : [];
        for (const payload of saved) {
            if (!exactFields(payload, ["remote_key", "connection_id", "principal_id",
                "request_id", "execution_request_id", "effect_id", "executor_fence"]) ||
                    !validFence(payload.executor_fence)) continue;
            const key = this.operationKey(this.operationExecutionFromCancel(payload));
            const pending = this.pendingOperationCancels.get(key) || [];
            if (!pending.some(candidate => sameRef(candidate, payload))) pending.push(payload);
            this.pendingOperationCancels.set(key, pending);
        }
    }

    createOperation(payload) {
        let resolveDone;
        const done = new Promise(resolve => { resolveDone = resolve; });
        return {
            ...this.operationIdentity(payload),
            effectKey: this.operationEffectKey(payload),
            payload: structuredClone(payload),
            cancelled: false,
            mutationStarted: false,
            committed: false,
            workspaceKey: null,
            tabId: null,
            done,
            resolveDone,
        };
    }

    registerOperation(payload) {
        const registration = this.executionRegistrationTail.catch(() => {})
            .then(() => this.registerOperationSerialized(payload));
        this.executionRegistrationTail = registration;
        return registration;
    }

    async registerOperationSerialized(payload) {
        if (!payload || !exactFields(payload, ["remote_key", "connection_id", "principal_id",
            "request_id", "effect_id", "executor_fence", "command"]) ||
                !validFence(payload.executor_fence)) {
            throw new Error("invalid execute payload");
        }
        const operationKey = this.operationKey(payload);
        const effectKey = this.operationEffectKey(payload);
        const knownEffect = this.executionLedger.get(effectKey);
        const identity = this.operationEffectIdentity(payload);
        if (knownEffect) {
            if (!sameRef(knownEffect.identity, identity)) {
                throw new Error("conflicting duplicate execute effect");
            }
            await this.flushExecutionLedger();
            return false;
        }
        if (this.operations.has(operationKey) || this.completedOperations.has(operationKey)) {
            const known = this.operations.get(operationKey)?.payload ||
                this.completedOperations.get(operationKey)?.payload;
            if (known && !sameRef(known, payload)) {
                throw new Error("conflicting duplicate execute request");
            }
            return false;
        }
        const operation = this.createOperation(payload);
        this.executionLedger.set(effectKey, {
            version: 1,
            phase: "accepted",
            identity,
            completion: null,
        });
        await this.persistExecutionLedger();
        const pending = this.pendingOperationCancels.get(operationKey);
        if (pending) {
            operation.cancelled = true;
            operation.cancellationPayloads = pending;
            this.pendingOperationCancels.delete(operationKey);
        }
        this.operations.set(operationKey, operation);
        return true;
    }

    async execute(payload) {
        if (!await this.registerOperation(payload)) return;
        return this.executeRegistered(payload);
    }

    async executeRegistered(payload) {
        await this.persistOperationCancels();
        return this.enqueueLifecycle(() => this.executeSerialized(payload));
    }

    async markOperationEffectStarted(operation) {
        const entry = this.executionLedger.get(operation.effectKey);
        if (!entry || !sameRef(entry.identity, this.operationEffectIdentity(operation.payload))) {
            throw new Error("execute ledger identity was lost");
        }
        const started = {...entry, phase: "effect_started", completion: null};
        this.executionLedger.set(operation.effectKey, started);
        await this.persistExecutionLedger();
        operation.mutationStarted = true;
    }

    async executeSerialized(payload) {
        const operationKey = this.operationKey(payload);
        const operation = this.operations.get(operationKey);
        if (!operation || operation.started || this.completedOperations.has(operationKey)) return;
        operation.started = true;
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
                    await this.markOperationEffectStarted(operation);
                    await this.chrome.debugger.sendCommand(
                        {tabId: record.pageTabId}, "Page.navigate", {url: url.href});
                    operation.committed = true;
                    completion = operation.cancelled ? "unknown" : "applied";
                }
            } else if (payload.command.command === "close_page") {
                await this.detach(record.pageTabId);
                if (operation.cancelled) completion = "not_applied";
                else {
                    await this.markOperationEffectStarted(operation);
                    await this.closeOwnedWindow(record);
                    operation.committed = true;
                    completion = operation.cancelled ? "unknown" : "applied";
                }
            } else if (payload.command.command === "close_window") {
                await this.detach(record.pageTabId);
                if (operation.cancelled) completion = "not_applied";
                else {
                    await this.markOperationEffectStarted(operation);
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
            const cancellations = operation.cancellationPayloads || [];
            if (cancellations.length > 0) {
                this.pendingOperationCancels.set(operationKey,
                    cancellations.map(payload => structuredClone(payload)));
                await this.persistOperationCancels();
            }
            const terminal = {
                version: 1,
                phase: "terminal_pending_ack",
                identity: this.operationEffectIdentity(payload),
                completion,
            };
            this.executionLedger.set(operation.effectKey, terminal);
            await this.persistExecutionLedger();
            if (this.operations.get(operationKey) === operation) {
                this.operations.delete(operationKey);
            }
            operation.resolveDone();
            await this.flushExecutionLedger();
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

    async waitForOperationDone(operation) {
        let timer;
        try {
            await Promise.race([
                operation.done,
                new Promise((_, reject) => {
                    timer = setTimeout(() => reject(
                        new Error("browser operation did not terminate after cancellation")),
                    TRANSACTION_WAIT_MS);
                }),
            ]);
        } finally {
            if (timer !== undefined) clearTimeout(timer);
        }
    }

    async cancel(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "execution_request_id", "effect_id", "executor_fence"]) ||
                ![payload.remote_key, payload.connection_id, payload.principal_id,
                    payload.request_id, payload.execution_request_id, payload.effect_id]
                    .every(value => typeof value === "string" && value) ||
                payload.request_id === payload.execution_request_id ||
                !validFence(payload.executor_fence)) return;
        const execution = this.operationExecutionFromCancel(payload);
        const operationKey = this.operationKey(execution);
        const operation = this.operations.get(operationKey) ||
            this.completedOperations.get(operationKey);
        const ledger = this.executionLedger.get(this.operationEffectKey(execution));
        if (!operation && ledger) {
            const identity = {...this.operationIdentity(execution),
                command: ledger.identity.command};
            if (!sameRef(ledger.identity, identity)) return;
            const pending = this.pendingOperationCancels.get(operationKey) || [];
            if (!pending.some(saved => sameRef(saved, payload))) pending.push(structuredClone(payload));
            this.pendingOperationCancels.set(operationKey, pending);
            await this.persistOperationCancels();
            await this.flushExecutionLedger();
            return;
        }
        if (!operation) {
            const pending = this.pendingOperationCancels.get(operationKey) || [];
            if (!pending.some(saved => sameRef(saved, payload))) pending.push(structuredClone(payload));
            this.pendingOperationCancels.set(operationKey, pending);
            await this.persistOperationCancels();
            return;
        }
        if (this.operations.get(operationKey) === operation) {
            operation.cancelled = true;
            operation.cancellationPayloads ||= [];
            if (!operation.cancellationPayloads.some(saved => sameRef(saved, payload))) {
                operation.cancellationPayloads.push(structuredClone(payload));
            }
            const pending = this.pendingOperationCancels.get(operationKey) || [];
            if (!pending.some(saved => sameRef(saved, payload))) pending.push(structuredClone(payload));
            this.pendingOperationCancels.set(operationKey, pending);
            if (operation.mutationStarted) operation.uncertain = true;
            await this.persistOperationCancels();
            await this.waitForOperationDone(operation);
            await this.flushExecutionLedger();
            return;
        }
        const completion = operation &&
            (operation.mutationStarted || operation.committed || operation.uncertain ||
                operation.completion === "applied" || operation.completion === "unknown") ?
            "unknown" : "not_applied";
        await this.cancellationCompletion(payload, completion);
        const pending = this.pendingOperationCancels.get(operationKey);
        if (pending) {
            const remaining = pending.filter(saved => !sameRef(saved, payload));
            if (remaining.length > 0) this.pendingOperationCancels.set(operationKey, remaining);
            else this.pendingOperationCancels.delete(operationKey);
        }
        if (operation?.cancellationPayloads) {
            operation.cancellationPayloads = operation.cancellationPayloads
                .filter(saved => !sameRef(saved, payload));
        }
        await this.persistOperationCancels();
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
        await this.authorizeTopologyOutbox(state);
        if (state?.connected === true && typeof state.connection_id === "string" &&
                typeof state.principal_id === "string" && validFence(state.executor_fence)) {
            this.remoteAuthorities.set(state.remote_key, structuredClone(state));
        } else {
            this.remoteAuthorities.delete(state.remote_key);
        }
        this.retireRecoveryReady = state?.connected === true;
        this.retireRecoveryState = state?.connected === true ? state : null;
        if (state?.connected === true) {
            await this.restoreQuarantinedCleanupReports();
            await this.processDeferredTopologyResponses(state);
        }
        await this.recoverRetireLedger();
        if (state?.connected === true) await this.flushTopologyOutbox();
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
        if (!this.browserBootId) throw new Error("browser boot identity is unavailable");
        const batch = this.ordinaryCloseWindowBatches.get(removedWindowId) ||
            this.captureOrdinaryCloseWindow(removedWindowId);
        const normalWindows = await this.visibleNormalWindows(removedWindowId);
        const registryOrdinary = [...this.windowRegistry.values()].filter(record =>
            record.windowId !== removedWindowId && record.kind === "ordinary" && !record.closing);
        const liveOrdinary = normalWindows.filter(window => !this.windowOwners.has(window.id));
        if (batch.windows.size === 1 &&
                (registryOrdinary.length > 0 || liveOrdinary.length > 0)) {
            if (await this.waitForOrdinaryClosePeer(batch)) {
                await this.resolveOrdinaryCloseBatch(batch);
                this.reportAdvisoryError(new Error(
                    "ordinary close identity is ambiguous; managed cleanup remains pending"));
                return;
            }
            this.settleNonFinalOrdinaryClose(removedWindowId);
            await this.primeRecentlyClosedSessions();
            return;
        }
        const intent = await this.resolveOrdinaryCloseBatch(batch);
        if (intent.outcome !== "exact") {
            this.reportAdvisoryError(new Error(
                "ordinary close identity is ambiguous; managed cleanup remains pending"));
            return;
        }
        await this.markRestoreHost(intent);
        if (normalWindows.length > 0) await this.closeManagedWindowSet(normalWindows);
    }

    async persistManagedCleanupBatch(batch) {
        await this.localStateWrite(MANAGED_CLEANUP_BATCH_KEY, () => batch);
    }

    cleanupBatchRecord(record) {
        return {
            key: this.key(record.remoteKey, record.principalId),
            record: this.persistedRecord(record),
            phase: "prepared",
        };
    }

    async closeManagedWindowSet(normalWindows) {
        const records = normalWindows.map(window => {
            const key = this.windowOwners.get(window.id);
            const record = key && this.workspaces.get(key);
            return record?.windowId === window.id ? record : null;
        }).filter(record => record !== null);
        if (records.length === 0) return;
        const batch = {
            version: 1,
            batch_id: crypto.randomUUID(),
            source_boot: this.browserBootId,
            phase: "prepared",
            records: records.map(record => this.cleanupBatchRecord(record)),
        };
        await this.persistManagedCleanupBatch(batch);
        await this.persistWithoutRecords(records);
        batch.phase = "records_removed";
        await this.persistManagedCleanupBatch(batch);
        const failures = [];
        for (const [index, record] of records.entries()) {
            const entry = batch.records[index];
            this.closingManagedWindows.add(record.windowId);
            entry.phase = "converging";
            await this.persistManagedCleanupBatch(batch);
            try {
                await this.convergeManagedTopology(record, {closeWindow: true, report: false,
                    persisted: true});
                entry.phase = "removed";
            } catch (error) {
                entry.phase = "pending";
                entry.error = error?.message || String(error);
                failures.push(error);
            }
            await this.persistManagedCleanupBatch(batch);
        }
        if (failures.length === 0) {
            await this.persistManagedCleanupBatch(false);
        } else {
            this.reportAdvisoryError(new AggregateError(failures,
                "managed shutdown cleanup remains pending"));
        }
    }

    stableCleanupOwnershipMatches(ownership, record) {
        return Boolean(ownership && ownership.version === 2 &&
            ownership.nonce === record.nonce &&
            ownership.logical_window_id === record.logicalWindowId &&
            ownership.logical_page_id === record.logicalPageId &&
            ownership.window_generation === (record.windowGeneration ?? record.generation) &&
            ownership.page_generation === (record.pageGeneration ?? record.generation) &&
            ownership.binding?.nonce === record.nonce &&
            ownership.binding?.remote_key === record.remoteKey &&
            ownership.binding?.workspace_id === record.workspaceId &&
            ownership.binding?.generation ===
                (record.windowGeneration ?? record.generation) && ownership.trusted !== false &&
            ownership.phase !== "orphan_unresolved");
    }

    async relocateCleanupBatchRecord(record) {
        if (!record || typeof record.nonce !== "string" ||
                !STATUS_NONCE_RE.test(record.nonce)) return null;
        const ownership = await this.currentOwnedMarker(record.nonce);
        if (ownership?.version !== 2 || !this.stableCleanupOwnershipMatches(ownership, record)) {
            return null;
        }
        const matches = [];
        const tabs = await this.chrome.tabs.query({});
        const windowIds = new Set(tabs.filter(tab => {
            const marker = this.parseStatusUrl(tab.url);
            return marker?.nonce === record.nonce && MARKER_IDS.includes(marker.marker);
        }).map(tab => tab.windowId).filter(Number.isInteger));
        for (const windowId of windowIds) {
            const windowTabs = tabs.filter(tab => tab.windowId === windowId);
            if (windowTabs.length !== 3) continue;
            const markers = MARKER_IDS.map((markerId, index) => windowTabs.find(tab => {
                const marker = this.parseStatusUrl(tab.url);
                return marker?.nonce === record.nonce && marker.marker === markerId &&
                    tab.title === `${STATUS_PREFIX}${record.nonce}:${markerId}` &&
                    tab.pinned === true && tab.index === index;
            }));
            const pages = windowTabs.filter(tab => tab.pinned !== true && tab.index === 2);
            if (markers.every(Boolean) && pages.length === 1) {
                matches.push({windowId, statusTabIds: markers.map(tab => tab.id),
                    pageTabId: pages[0].id});
            }
        }
        if (matches.length !== 1) return null;
        const match = matches[0];
        const candidate = {
            ...record,
            windowId: match.windowId,
            statusTabIds: match.statusTabIds,
            pageTabId: match.pageTabId,
            x11WindowId: null,
            remoteKey: ownership.binding.remote_key,
            workspaceId: ownership.binding.workspace_id,
            windowGeneration: ownership.window_generation,
            pageGeneration: ownership.page_generation,
            connectionDisplayName: ownership.binding.display_title,
        };
        if (!await this.isLive(candidate)) return null;
        try {
            await this.bind(candidate);
            if (!await this.isLive(candidate)) {
                throw new Error("relocated managed topology changed during X11 binding");
            }
            const currentOwnership = this.markerOwnership(candidate);
            if (!this.stableCleanupOwnershipMatches(currentOwnership, record)) {
                throw new Error("relocated managed ownership changed stable identity");
            }
            await this.updateOwnedMarkers(markers => {
                markers[candidate.nonce] = currentOwnership;
            });
            return candidate;
        } catch (error) {
            const cleanupErrors = [];
            await this.clearManagedX11(candidate, cleanupErrors);
            if (cleanupErrors.length > 0) {
                throw new AggregateError([error, ...cleanupErrors],
                    "relocated X11 binding could not be rolled back");
            }
            throw error;
        }
    }

    async quarantineCrossBootCleanup(entry, record, error) {
        entry.phase = "orphan_unresolved";
        entry.error = error.message;
        entry.record = {
            remoteKey: record.remoteKey,
            principalId: record.principalId,
            workspaceId: record.workspaceId,
            connectionId: record.connectionId,
            executorFence: record.executorFence,
            ownerDisplay: record.ownerDisplay || null,
            logicalWindowId: record.logicalWindowId,
            logicalPageId: record.logicalPageId,
            windowGeneration: record.windowGeneration ?? record.generation,
            pageGeneration: record.pageGeneration ?? record.generation,
            nonce: record.nonce,
            connectionDisplayName: record.connectionDisplayName || null,
        };
        entry.logical_tombstone ||= {report_id: crypto.randomUUID()};
        if (typeof record.nonce === "string" && record.nonce) {
            this.quarantinedCleanupNonces.add(record.nonce);
            await this.updateOwnedMarkers(markers => {
                const ownership = markers[record.nonce];
                markers[record.nonce] = {
                    version: 2,
                    nonce: record.nonce,
                    trusted: false,
                    phase: "orphan_unresolved",
                    logical_window_id: record.logicalWindowId ?? null,
                    logical_page_id: record.logicalPageId ?? null,
                    window_generation: record.windowGeneration ?? record.generation ?? null,
                    page_generation: record.pageGeneration ?? record.generation ?? null,
                    binding: ownership?.binding || (record.remoteKey && record.workspaceId ?
                        this.bindingPayload(record) : null),
                };
            });
        }
        if (this.chrome.storage?.session) {
            const stored = await this.chrome.storage.session.get(["abushWorkspaces"]);
            const records = Array.isArray(stored.abushWorkspaces) ? stored.abushWorkspaces : [];
            const filtered = records.filter(saved => saved.nonce !== record.nonce &&
                !(saved.remoteKey === record.remoteKey &&
                    saved.principalId === record.principalId));
            if (filtered.length !== records.length) await this.persistRecords(filtered);
        }
        const active = this.workspaces.get(this.key(record.remoteKey, record.principalId));
        if (active && active.nonce === record.nonce) this.forget(active);
    }

    async restoreQuarantinedCleanupReports() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([MANAGED_CLEANUP_BATCH_KEY]);
        const batch = local[MANAGED_CLEANUP_BATCH_KEY];
        if (!batch || batch.version !== 1 || !Array.isArray(batch.records)) return;
        for (const entry of batch.records) {
            const record = entry.record;
            const reportId = entry.logical_tombstone?.report_id;
            if (entry.phase !== "orphan_unresolved" || typeof reportId !== "string" ||
                    !reportId || !record || !validFence(record.executorFence) ||
                    !record.remoteKey || !record.connectionId || !record.principalId ||
                    !record.workspaceId || !record.logicalWindowId || !record.logicalPageId ||
                    !Number.isInteger(record.windowGeneration) ||
                    !Number.isInteger(record.pageGeneration)) continue;
            const report = {
                remote_key: record.remoteKey,
                connection_id: record.connectionId,
                principal_id: record.principalId,
                report_id: reportId,
                request_id: crypto.randomUUID(),
                executor_fence: record.executorFence,
                origin_executor_fence: record.executorFence,
                daemon_instance_id: record.ownerDisplay?.daemon_instance_id || null,
                page: {
                    window: {workspace_id: record.workspaceId,
                        window_id: record.logicalWindowId,
                        generation: record.windowGeneration},
                    page_id: record.logicalPageId,
                    generation: record.pageGeneration,
                },
                cleanup: null,
                phase: "pending",
                response: null,
                consumed_request_id: null,
                consumed_migration: null,
            };
            const existing = this.topologyOutbox.get(reportId);
            if (!existing) await this.queueTopologyReport(report);
        }
    }

    async restoreManagedCleanupBatch() {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([MANAGED_CLEANUP_BATCH_KEY]);
        const batch = local[MANAGED_CLEANUP_BATCH_KEY];
        if (!batch || batch.version !== 1 || !Array.isArray(batch.records)) return;
        const crossBoot = typeof batch.source_boot !== "string" ||
            batch.source_boot !== this.browserBootId;
        const failures = [];
        for (const entry of batch.records) {
            if (entry.phase === "orphan_unresolved") {
                if (typeof entry.record?.nonce === "string") {
                    this.quarantinedCleanupNonces.add(entry.record.nonce);
                }
                continue;
            }
            if (entry.phase === "removed" || !entry.record) continue;
            let record = {...entry.record, debuggerAttached: false};
            if (crossBoot) {
                let relocationError = null;
                try {
                    record = await this.relocateCleanupBatchRecord(record);
                } catch (error) {
                    relocationError = error;
                    record = null;
                }
                if (!record) {
                    const error = relocationError || new Error(
                        "managed cleanup identity could not be uniquely relocated after restart");
                    await this.quarantineCrossBootCleanup(entry,
                        {...entry.record, debuggerAttached: false}, error);
                    failures.push(error);
                    await this.persistManagedCleanupBatch(batch);
                    continue;
                }
                entry.record = this.persistedRecord(record);
            }
            try {
                this.closingManagedWindows.add(record.windowId);
                await this.convergeManagedTopology(record, {closeWindow: true, report: false,
                    persisted: true});
                entry.phase = "removed";
                delete entry.error;
            } catch (error) {
                entry.phase = "pending";
                entry.error = error?.message || String(error);
                failures.push(error);
            }
            await this.persistManagedCleanupBatch(batch);
        }
        if (failures.length === 0) await this.persistManagedCleanupBatch(false);
        else this.reportAdvisoryError(new AggregateError(failures,
            "managed shutdown cleanup remains pending"));
    }

    async windowRemoved(windowId) {
        const registry = this.windowRegistry.get(windowId);
        if (registry?.kind === "ordinary") this.captureOrdinaryCloseWindow(windowId, registry);
        this.windowRegistry.delete(windowId);
        this.nextWindowSnapshotToken(windowId);
        if (this.closingManagedWindows.delete(windowId)) return;
        const key = this.windowOwners.get(windowId);
        if (!key) {
            if (registry?.kind === "ordinary" || this.removedHostSnapshots.has(windowId)) {
                await this.replaceClosedHostWindow(windowId);
            }
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

    async convergeManagedTopology(record, {closeWindow = false, report = true,
        persisted = false} = {}) {
        const page = this.pageRef(record);
        const live = await this.isLive(record);
        let physicalGone = await this.windowIsMissing(record.windowId);
        const errors = [];
        let reportError = null;
        if (report) {
            const reportPayload = this.topologyReport(record, page);
            await this.queueTopologyReport(reportPayload);
        }
        const persistError = persisted ? null : await this.persistOrForget(record);
        if (persistError) throw persistError;
        if (this.workspaces.get(this.key(record.remoteKey, record.principalId)) === record) {
            this.forget(record);
        }
        await this.appendCleanupError(errors, () => this.detach(record.pageTabId));
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
        if (errors.length === 0) {
            await this.appendCleanupError(errors, () => this.disownMarker(record.nonce));
        }
        if (report) {
            try {
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
        if (report) {
            const queued = [...this.topologyOutbox.values()].find(saved =>
                sameRef(saved.page, page));
            if (queued?.cleanup) {
                queued.cleanup = {...queued.cleanup, phase: "removed"};
                await this.persistTopologyOutbox();
            }
        }
        if (reportError) this.reportAdvisoryError(reportError);
    }

    recordForTopology(tabId, windowId = null) {
        const key = this.tabOwners.get(tabId) || this.statusTabOwners.get(tabId) ||
            this.windowOwners.get(windowId);
        return key ? this.workspaces.get(key) || null : null;
    }

    async reconcileManagedTab(tabId, info = {}) {
        const windowId = info.windowId ?? info.newWindowId ?? info.oldWindowId ?? null;
        const record = this.recordForTopology(tabId, windowId);
        if (!record || await this.isLive(record)) return false;
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
        if (info?.isWindowClosing) {
            this.freezeClosingWindow(info.windowId);
            return this.reconcileManagedTab(tabId, info);
        }
        return this.reconcileManagedTab(tabId, info);
    }

    async tabDetached(tabId, info) {
        return this.reconcileManagedTab(tabId, info);
    }

    async tabAttached(tabId, info) {
        return this.reconcileManagedTab(tabId, info);
    }

    async tabMoved(tabId, moveInfo) {
        const reconciled = await this.reconcileManagedTab(tabId, moveInfo);
        if (!reconciled && Number.isInteger(moveInfo?.windowId)) {
            await this.rememberWindowSnapshot(moveInfo.windowId);
        }
        return reconciled;
    }

    async persistReplacementLedger(entries) {
        const snapshot = structuredClone(entries);
        await this.localStateWrite(REPLACEMENT_LEDGER_KEY, () => snapshot);
    }

    validReplacementEntry(entry) {
        return entry?.version === 1 && ["planned", "session_candidate", "marker_candidate",
            "installed"].includes(entry.phase) && typeof entry.remote_key === "string" &&
            typeof entry.principal_id === "string" && Number.isInteger(entry.old_tab_id) &&
            Number.isInteger(entry.new_tab_id) && entry.record;
    }

    async restoreReplacementLedger({consume = false} = {}) {
        if (!this.chrome.storage?.local) return;
        const local = await this.chrome.storage.local.get([REPLACEMENT_LEDGER_KEY]);
        const entries = Array.isArray(local[REPLACEMENT_LEDGER_KEY]) ?
            local[REPLACEMENT_LEDGER_KEY] : [];
        const pending = [];
        for (const entry of entries) {
            if (!this.validReplacementEntry(entry)) continue;
            const key = this.key(entry.remote_key, entry.principal_id);
            let records = [];
            if (this.chrome.storage?.session) {
                const stored = await this.chrome.storage.session.get(["abushWorkspaces"]);
                records = Array.isArray(stored.abushWorkspaces) ? stored.abushWorkspaces : [];
            }
            const candidate = records.find(record => this.key(record.remoteKey,
                record.principalId) === key && record.pageTabId === entry.new_tab_id);
            let replacement;
            try {
                replacement = await this.chrome.tabs.get(entry.new_tab_id);
            } catch (_) {
                replacement = null;
            }
            const marker = await this.currentOwnedMarker(entry.record.nonce);
            if (candidate && replacement && replacement.windowId === candidate.windowId &&
                    replacement.pinned !== true && replacement.index === 2 &&
                    await this.isLive(candidate)) {
                if (!this.savedOwnershipMatches(marker, candidate)) await this.claimMarker(candidate);
                const installed = this.workspaces.get(key);
                if (consume && installed && installed.pageTabId === entry.new_tab_id &&
                        await this.isLive(installed)) continue;
                pending.push({...entry, phase: "marker_candidate"});
                continue;
            }
            if (entry.phase === "planned" && consume) continue;
            pending.push(entry);
        }
        await this.persistReplacementLedger(pending);
    }

    async tabReplaced(addedTabId, removedTabId) {
        const key = this.tabOwners.get(removedTabId) ||
            this.statusTabOwners.get(removedTabId);
        if (!key) return false;
        const record = this.workspaces.get(key);
        if (!record) return false;
        let replacement;
        try {
            replacement = await this.chrome.tabs.get(addedTabId);
        } catch (error) {
            this.reportAdvisoryError(new AggregateError([error],
                "replacement tab lookup failed during topology reconciliation"));
            await this.convergeManagedTopology(record);
            return true;
        }
        if (removedTabId === record.pageTabId && replacement.windowId === record.windowId &&
                replacement.pinned !== true && replacement.index === 2) {
            const previousTabId = record.pageTabId;
            const candidate = {...record, statusTabIds: [...record.statusTabIds],
                pageTabId: addedTabId};
            if (await this.isLive(candidate)) {
                let replacementEntry = {
                    version: 1,
                    phase: "planned",
                    remote_key: record.remoteKey,
                    principal_id: record.principalId,
                    old_tab_id: previousTabId,
                    new_tab_id: addedTabId,
                    record: this.persistedRecord(candidate),
                };
                await this.persistReplacementLedger([replacementEntry]);
                const write = this.persistTail.catch(() => {}).then(async () => {
                    const records = [...this.workspaces.values()].map(saved =>
                        this.persistedRecord(saved === record ? candidate : saved));
                    await this.persistRecords(records);
                    replacementEntry = {...replacementEntry, phase: "session_candidate"};
                    await this.persistReplacementLedger([replacementEntry]);
                    const ownership = this.markerOwnership(candidate);
                    await this.updateOwnedMarkers(markers => {
                        markers[candidate.nonce] = ownership;
                    });
                    replacementEntry = {...replacementEntry, phase: "marker_candidate"};
                    await this.persistReplacementLedger([replacementEntry]);
                    this.tabOwners.delete(previousTabId);
                    record.pageTabId = addedTabId;
                    this.tabOwners.set(addedTabId, key);
                    replacementEntry = {...replacementEntry, phase: "installed"};
                    await this.persistReplacementLedger([replacementEntry]);
                });
                this.persistTail = write;
                await write;
                await this.persistReplacementLedger([]);
                return true;
            }
        }
        await this.convergeManagedTopology(record);
        return true;
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
