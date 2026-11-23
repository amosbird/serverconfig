if (typeof importScripts === "function") importScripts("playwright-transport.js");
const ORDINARY_PREFIX = "__ABUSH_ORDINARY__:";
const AUTOMATION_KEY = "abushBrowserAutomationV9";
const CONSENT_KEY = "abushBrowserConsentV9";
const WAL_KEY = "abushBrowserEffectsV10";
const RECEIPTS_KEY = "abushBrowserReceiptsV10";
const INHERITANCE_KEY = "abushBrowserInheritanceV9";
const API_TIMEOUT_MS = 2000;
// tabs.create clamps this signed API index to the current end, even after concurrent inserts.
const RIGHT_EDGE_TAB_INDEX = 0x7fffffff;
const BROWSER_ERROR_CODES = new Set([
    "browser_disabled", "unknown_principal", "unknown_connection", "stale_session",
    "stale_authority", "human_controlled", "missing_capability", "stale_page",
    "operation_conflict", "unknown_effect", "executor_unavailable", "invalid_command",
    "capability_grant_mismatch", "backpressure", "stale_window", "page_quarantined",
    "stale_effect", "generation_exhausted",
]);
const clone = value => structuredClone(value);
function sameRef(a, b) {
    if (a === b) return true;
    if (!a || !b || typeof a !== "object" || typeof b !== "object") return false;
    const keys = Object.keys(a);
    return keys.length === Object.keys(b).length && keys.every(key => sameRef(a[key], b[key]));
}
function exactFields(value, fields) {
    return value && typeof value === "object" && !Array.isArray(value) &&
        Object.keys(value).length === fields.length && fields.every(key => key in value);
}
const text = value => typeof value === "string" && value.length > 0;
function validFence(value) {
    return exactFields(value, ["executor_id", "epoch"]) && text(value.executor_id) &&
        Number.isSafeInteger(value.epoch) && value.epoch > 0;
}
function validPageRef(value) {
    return exactFields(value, ["workspace_id", "page_id", "generation"]) &&
        text(value.workspace_id) && text(value.page_id) &&
        Number.isSafeInteger(value.generation) && value.generation > 0;
}
function displayTitle(display) {
    return display ? `Abush · ${display}` : "Abush";
}
function expectedUngroupMove(tab, members) {
    const indices = members.map(member => member.index);
    if (!Number.isSafeInteger(tab.index) || indices.some(index => !Number.isSafeInteger(index)) ||
            tab.index <= Math.min(...indices) || tab.index >= Math.max(...indices)) return null;
    return {windowId: tab.windowId, fromIndex: tab.index, toIndex: Math.max(...indices)};
}
function fail(code, message) {
    throw Object.assign(new Error(message), {browserCode: code});
}
async function bounded(task, label, timeoutMs = API_TIMEOUT_MS) {
    let timer;
    try {
        return await Promise.race([Promise.resolve().then(task), new Promise((_, reject) => {
            timer = setTimeout(() => reject(new Error(`${label} timed out`)), timeoutMs);
        })]);
    } finally {
        clearTimeout(timer);
    }
}

function validEngineAction(action) {
    const fields = {snapshot: [], text: [], click: ["snapshot_id", "ref"],
        fill: ["snapshot_id", "ref", "value"], press: ["snapshot_id", "ref", "key"],
        select: ["snapshot_id", "ref", "values"], check: ["snapshot_id", "ref", "checked"],
        hover: ["snapshot_id", "ref"], scroll: ["x", "y"], evaluate: ["expression"],
        wait: ["text", "timeout_ms"], network_list: [], network_body: ["request_id"],
        console: [], screenshot: ["full_page"]};
    const keys = fields[action?.type];
    if (!keys || !exactFields(action, ["type", ...keys])) return false;
    return keys.every(key => {
        const value = action[key];
        if (["checked", "full_page"].includes(key)) return typeof value === "boolean";
        if (["x", "y", "timeout_ms"].includes(key)) return Number.isSafeInteger(value) &&
            (key !== "timeout_ms" || value >= 1 && value <= 30000);
        if (key === "values") return Array.isArray(value) && value.every(v => typeof v === "string");
        return typeof value === "string" && (key === "value" || value.length > 0);
    });
}
function engineBinding(payload) {
    return {remote_key: payload.remote_key, connection_id: payload.connection_id,
        principal_id: payload.principal_id, executor_fence: payload.executor_fence,
        authorization: payload.authorization, page: payload.command?.page};
}
const IDLE_ENGINE_COMMANDS = new Set([
    "Browser.getVersion", "Target.getTargetInfo", "Page.getFrameTree", "Page.getLayoutMetrics",
    "Page.getNavigationHistory", "Runtime.getProperties", "Runtime.releaseObject",
    "Runtime.releaseObjectGroup", "DOM.getDocument", "DOM.describeNode", "DOM.resolveNode",
    "Network.getResponseBody", "Network.getRequestPostData", "Log.enable", "Log.disable",
]);
function validEngineResult(result) {
    return exactFields(result, ["text", "data", "images"]) && typeof result.text === "string" &&
        Array.isArray(result.images) && result.images.every(image =>
            exactFields(image, ["mime_type", "data_base64"]) &&
            ["image/png", "image/jpeg"].includes(image.mime_type) && typeof image.data_base64 === "string");
}

class WorkspaceExecutor {
    constructor(chromeApi, nativeRequest) {
        this.chrome = chromeApi;
        this.nativeRequest = nativeRequest;
        this.engineSessions = new Map();
        this.cancellations = {};
        this.controlTokens = new Map();
        this.restoreGeneration = 0;
        this.restoreRevoked = false;
        this.transportGeneration = 0;
        this.ownershipGeneration = 1;
        this.pendingCreations = new Map();
        this.attachments = new Map();
        this.ordinaryTail = Promise.resolve();
        this.browserBootId = null;
        this.browserExecutorStatus = {phase: "unavailable", error: null};
        this.remotes = new Map();
        this.records = new Map();
        this.revisions = new Map();
        this.effects = {};
        this.creations = {};
        this.ordinary = {};
        this.receipts = {};
        this.operations = new Map();
        this.executionTails = new Map();
        this.ordinaryOperations = new Map();
        this.ordinaryCancellations = {};
        this.controls = new Set();
        this.revokedTabs = new Set();
        this.localTail = Promise.resolve();
        this.sessionTail = Promise.resolve();
        this.restoreAttempt = null;
        this.restored = false;
        this.lastError = null;
        this.bootstrapLeases = [];
        this.automationOn = false;
        this.automationInhibited = false;
        this.preferenceReady = false;
        this.preferenceGeneration = 0;
        this.preferencePending = false;
        this.preferenceTail = Promise.resolve();
        this.reconciliations = new Map();
        this.reconcileVersions = new Map();
        this.remoteErrors = new Map();
        this.aliases = new Map();
        this.currentExecutors = new Map();
        this.personalCandidates = new Map();
        this.personalPlacements = new Map();
        this.workCreations = new Map();
        this.sourceGroupEvents = new Map();
        this.sourceGroupEventsOverflow = false;
        this.sourceGroupsHydrated = false;
        this.inheritanceTail = Promise.resolve();
        this.workCreatesInFlight = 0;
        this.presentationTail = Promise.resolve();
        this.presentationRevision = 0;
        this.presentationDefaults = new Map();
        this.activeTabs = new Map();
        this.activationGenerations = new Map();
        this.removedActiveTabs = new Set();
        this.activeBoot = null;
        this.manualControls = false;
    }

    api(task, label) { return bounded(task, label); }
    native(service, operation, payload) {
        return bounded(() => this.nativeRequest(service, operation, clone(payload)),
            `${service}.${operation}`, 8000);
    }
    reportAdvisoryError(error) {
        this.lastError = error.message || String(error);
        this.onAdvisoryError?.(error);
    }
    async acceptNativeBootstrap(payload) {
        if (!exactFields(payload, ["browser_boot_id", "browser_pid", "browser_starttime",
            "ordinary_launch_leases"]) || !text(payload.browser_boot_id) ||
                !Number.isSafeInteger(payload.browser_pid) || payload.browser_pid <= 0 ||
                !/^\d+$/.test(payload.browser_starttime) ||
                !Array.isArray(payload.ordinary_launch_leases)) {
            fail("invalid_command", "Invalid native browser bootstrap");
        }
        if (this.browserBootId && this.browserBootId !== payload.browser_boot_id) {
            this.restoreGeneration++;
            for (const key of this.controls) this.bumpControl(key);
            for (const record of this.records.values()) this.closeAuthorization(record, "disabled");
            this.records.clear();
            this.presentationDefaults.clear();
            this.revisions.clear();
            this.remotes.clear();
            this.aliases.clear();
            this.currentExecutors.clear();
            for (const candidate of this.personalCandidates.values()) clearTimeout(candidate.timer);
            this.personalCandidates.clear();
            this.personalPlacements.clear();
            this.workCreations.clear();
            this.sourceGroupEvents.clear();
            this.sourceGroupEventsOverflow = false;
            this.sourceGroupsHydrated = false;
            this.activeTabs.clear();
            this.activationGenerations.clear();
            this.removedActiveTabs.clear();
            this.activeBoot = null;
            this.restoreAttempt = null;
            this.restored = false;
        }
        this.browserBootId = payload.browser_boot_id;
        for (const candidate of this.personalCandidates.values()) {
            if (candidate.browser_boot_id === null) candidate.browser_boot_id = this.browserBootId;
        }
        this.bootstrapLeases = clone(payload.ordinary_launch_leases);
        if (this.restored) {
            for (const lease of this.bootstrapLeases) await this.ordinaryOpen(lease);
            this.bootstrapLeases = [];
        }
    }
    restoreWhenReady() {
        if (this.restored && this.activeBoot !== this.browserBootId) {
            return this.seedActiveTabs().then(() => this.restoreAttempt);
        }
        const boot = this.browserBootId;
        const generation = this.restoreGeneration;
        const preferenceGeneration = this.preferenceGeneration;
        this.restoreAttempt ||= this.restore().catch(error => {
            if (boot === this.browserBootId && generation === this.restoreGeneration &&
                    preferenceGeneration === this.preferenceGeneration) this.reportAdvisoryError(error);
            throw error;
        });
        return this.restoreAttempt;
    }
    async restore() {
        if (!this.browserBootId) fail("executor_unavailable", "Native browser boot is unavailable");
        const boot = this.browserBootId;
        const generation = this.restoreGeneration;
        const preferenceGeneration = this.preferenceGeneration;
        const local = await this.api(() => this.chrome.storage.local.get([WAL_KEY, RECEIPTS_KEY,
            AUTOMATION_KEY]), "read effect journal and automation preference");
        if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
        if (preferenceGeneration === this.preferenceGeneration && !this.automationInhibited) {
            if (local[AUTOMATION_KEY] !== undefined && typeof local[AUTOMATION_KEY] !== "boolean") {
                fail("executor_unavailable", "Invalid stored automation preference");
            }
            this.automationOn = local[AUTOMATION_KEY] !== false;
            this.preferenceReady = true;
        }
        const wal = local[WAL_KEY];
        if (wal?.version === 10) {
            this.cancellations = {...(wal.cancellations || {}), ...this.cancellations};
            this.effects = wal.effects || {};
            this.creations = wal.creations || {};
            this.ordinary = wal.ordinary || {};
            this.ordinaryCancellations = {...(wal.ordinary_cancellations || {}), ...this.ordinaryCancellations};
            for (const entry of Object.values(this.creations)) {
                if (!["completed", "abandoned", "not_applied"].includes(entry.phase)) entry.phase = "unknown";
            }
            for (const entry of [...Object.values(this.effects), ...Object.values(this.ordinary)]) {
                if (!entry.completion) entry.completion =
                    entry.phase === "accepted" ? "not_applied" : "unknown";
            }
            for (const entry of Object.values(this.effects)) {
                if (entry.completion !== "applied" && !text(entry.error)) {
                    entry.result = null;
                    entry.error = "Engine interrupted before durable output";
                }
            }
            for (const entry of Object.values(this.cancellations)) {
                if (!entry.completion) {
                    const effect = this.effects[this.effectKey(entry.payload)];
                    entry.completion = effect?.completion === "not_applied" ? "not_applied" : "unknown";
                }
            }
        }
        this.receipts = local[RECEIPTS_KEY]?.version === 10 ? local[RECEIPTS_KEY].receipts : {};
        const session = await this.api(() => this.chrome.storage.session.get(CONSENT_KEY),
            "read browser consent");
        if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
        const saved = session[CONSENT_KEY];
        if (saved?.version === 9 && saved.browser_boot_id === boot) {
            this.ownershipGeneration = Math.max(this.ownershipGeneration,
                Number.isSafeInteger(saved.ownership_generation) ? saved.ownership_generation + 1 : 1);
            for (const [key, revision] of Object.entries(saved.revisions || {})) {
                if (!Number.isSafeInteger(revision) || revision <= 0) continue;
                const changed = this.manualControls && (this.restoreRevoked ||
                    (this.controlTokens.get(key) || 0) > 0);
                this.revisions.set(key, Math.max(this.revisions.get(key) || 0, revision + Number(changed)));
            }
            for (const candidate of saved.records || []) {
                if (!text(candidate.remote_key) || !validPageRef(candidate.page) ||
                        !text(candidate.enable_id) || !Number.isSafeInteger(candidate.revision) || candidate.revision <= 0 ||
                        !(Number.isSafeInteger(candidate.tab_id) || candidate.tab_id === null &&
                            candidate.preparation === "reserved") || !Number.isSafeInteger(candidate.window_id) ||
                        !validFence(candidate.executor_fence) || !text(candidate.principal_id) ||
                        !text(candidate.owner_display?.daemon_instance_id) ||
                        !["enabled", "paused"].includes(candidate.phase)) continue;
                if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
                if (this.manualControls && (this.restoreRevoked ||
                        (this.controlTokens.get(candidate.remote_key) || 0) > 0 ||
                        (this.revisions.get(candidate.remote_key) || 0) > candidate.revision)) continue;
                if (this.records.has(candidate.remote_key) ||
                        Number.isSafeInteger(candidate.tab_id) && [...this.records.values()].some(record =>
                            record.tab_id === candidate.tab_id)) continue;
                this.revisions.set(candidate.remote_key, Math.max(candidate.revision,
                    this.revisions.get(candidate.remote_key) || 1));
                const live = [...this.remotes.values()].find(remote =>
                    remote.principal_id === candidate.principal_id &&
                    remote.workspace_id === candidate.page.workspace_id &&
                    remote.owner_display.daemon_instance_id === candidate.owner_display.daemon_instance_id);
                const record = {...candidate, remote_key: live?.remote_key || candidate.remote_key,
                    agreed: false, publication: null, attached: false};
                if (this.sourceGroupEvents.has(record.tab_id) || this.sourceGroupEventsOverflow) {
                    record.group_id = this.sourceGroupEvents.get(record.tab_id) ?? record.group_id;
                    record.group_reused = true;
                    record.group_generation = (record.group_generation || 0) + 1;
                }
                if (!this.manualControls) {
                    record.phase = "paused";
                    record.revision = this.nextRevision(record.remote_key);
                }
                this.records.set(record.remote_key, record);
                if (!this.manualControls) {
                    for (const remote of this.remotes.values()) {
                        if (remote.remote_key !== record.remote_key && remote.principal_id === record.principal_id &&
                                remote.workspace_id === record.page.workspace_id &&
                                remote.owner_display?.daemon_instance_id === record.owner_display.daemon_instance_id) {
                            this.aliases.set(remote.remote_key, record.remote_key);
                        }
                    }
                }
                try {
                    await this.validateReservation(record);
                    if (record.phase === "disabled") this.records.delete(record.remote_key);
                } catch (error) {
                    if (this.records.get(record.remote_key) === record) {
                        this.closeAuthorization(record, "disabled");
                        this.records.delete(record.remote_key);
                    }
                    this.reportAdvisoryError(error);
                }
            }
        }
        if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
        this.sourceGroupsHydrated = true;
        this.sourceGroupEvents.clear();
        this.sourceGroupEventsOverflow = false;
        await this.restoreInheritance();
        if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
        await this.seedActiveTabs();
        if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
        await this.persistWal();
        await this.persistConsent();
        for (const lease of this.bootstrapLeases) await this.ordinaryOpen(lease);
        this.bootstrapLeases = [];
        this.restored = true;
        if (!this.controls.size) this.revokedTabs.clear();
        if (!this.manualControls) await Promise.all([...this.remotes.keys()].map(key => this.reconcile(key)));
    }
    persistWal() {
        const write = this.localTail.catch(() => {}).then(() =>
            this.chrome.storage.local.set({[WAL_KEY]: clone({version: 10,
                effects: this.effects, creations: this.creations, ordinary: this.ordinary,
                cancellations: this.cancellations, ordinary_cancellations: this.ordinaryCancellations})}));
        this.localTail = write;
        return this.api(() => write, "persist effect journal");
    }
    persistConsent() {
        const write = this.sessionTail.catch(() => {}).then(() => {
            const records = [...this.records.values()].filter(record => record.phase !== "disabled")
                .map(({agreed, publication, attached, attachment, pendingDetach, ...record}) => record);
            return this.chrome.storage.session.set({[CONSENT_KEY]: {
                version: 9, browser_boot_id: this.browserBootId,
                ownership_generation: this.ownershipGeneration,
                revisions: Object.fromEntries(this.revisions), records: clone(records),
            }});
        });
        this.sessionTail = write;
        return this.api(() => write, "persist browser consent");
    }
    bumpControl(key) {
        this.controlTokens.set(key, (this.controlTokens.get(key) || 0) + 1);
        for (const op of this.operations.values()) {
            if (op.payload.remote_key === key) op.cancelled = true;
        }
    }
    ownershipChanged(tabId) {
        this.ownershipGeneration++;
        if (Number.isSafeInteger(tabId) && this.chrome.tabs.sendMessage) {
            this.chrome.tabs.sendMessage(tabId, {type: "abush_ownership_changed",
                owned: this.ownsTab(tabId), generation: this.ownershipGeneration})
                .catch(error => this.reportAdvisoryError(error));
        }
    }
    nextRevision(key) {
        const revision = (this.revisions.get(key) || 0) + 1;
        if (!Number.isSafeInteger(revision)) fail("executor_unavailable", "Revision exhausted");
        this.revisions.set(key, revision);
        return revision;
    }
    recordFor(key) {
        const record = this.records.get(this.aliases.get(key) || key);
        const remote = this.remotes.get(key);
        if (this.aliases.has(key) && remote && record &&
                (record.principal_id !== remote.principal_id || record.page.workspace_id !== remote.workspace_id ||
                record.owner_display.daemon_instance_id !== remote.owner_display?.daemon_instance_id)) return undefined;
        return record;
    }
    state(key) {
        const record = this.recordFor(key);
        return {browser_boot_id: this.browserBootId, revision: record?.revision || this.revisions.get(key) || 1,
            phase: record?.phase || "disabled", binding: record?.phase !== "disabled" && record ?
                {enable_id: record.enable_id, page: clone(record.page)} : null};
    }
    async validateReservation(record) {
        if (record.tab_id !== null) {
            if (record.preparation !== undefined && (record.preparation !== "materialized" ||
                    !Object.values(this.creations).some(entry => entry.enable_id === record.enable_id &&
                        entry.phase === "completed") ||
                    Object.values(this.creations).some(entry => entry.enable_id === record.enable_id &&
                        ["unknown", "abandoned"].includes(entry.phase)))) {
                fail("unknown_effect", "Work page preparation is not durably completed");
            }
            return this.validateTab(record);
        }
        if (record.preparation !== "reserved" || record.browser_boot_id !== this.browserBootId) {
            fail("stale_page", "Work page reservation is unavailable");
        }
    }
    async validateTab(record) {
        if (!Number.isSafeInteger(record.tab_id) || record.browser_boot_id !== this.browserBootId || this.revokedTabs.has(record.tab_id)) {
            fail("stale_page", "Original work tab was closed, moved, or replaced");
        }
        const tab = await this.api(() => this.chrome.tabs.get(record.tab_id), "validate work tab");
        const window = await this.api(() => this.chrome.windows.get(record.window_id),
            "validate existing window");
        if (tab.id !== record.tab_id || tab.windowId !== record.window_id || window.type !== "normal") {
            fail("stale_page", "Original work tab was closed, moved, or replaced; enable again");
        }
        return tab;
    }
    ownsTab(tabId) {
        return Number.isSafeInteger(tabId) && [...this.records.values()].some(record =>
            Number.isSafeInteger(record.tab_id) && record.phase !== "disabled" && record.tab_id === tabId);
    }
    closeAuthorization(record, phase) {
        this.closeEngine(record);
        this.bumpControl(record.remote_key);
        const token = this.attachments.get(record.tab_id);
        if (token) token.cleanup = true;
        record.pendingDetach = record.attachment;
        record.agreed = false;
        record.publication = null;
        record.phase = phase;
        record.revision = this.nextRevision(record.remote_key);
        this.ownershipChanged(record.tab_id);
        for (const op of this.operations.values()) {
            if (this.recordFor(op.payload.remote_key) === record) op.cancelled = true;
        }
    }
    releaseAttachment(token) {
        if (this.attachments.get(token.record.tab_id) !== token) return;
        this.attachments.delete(token.record.tab_id);
        token.record.attached = false;
        token.record.attachment = null;
        token.record.pendingDetach = null;
    }
    async detach(record, attachment = record?.pendingDetach || record?.attachment) {
        this.closeEngine(record);
        const token = record && this.attachments.get(record.tab_id);
        if (!token || token.record !== record || attachment && attachment !== token) return;
        token.cleanup = true;
        record.attached = false;
        if (token.pending) {
            this.reportAdvisoryError(new Error("Debugger cleanup pending: waiting for attachment completion"));
            return;
        }
        if (!token.detaching) {
            const attempt = Promise.resolve().then(() =>
                this.chrome.debugger.detach({tabId: record.tab_id}));
            token.detaching = attempt.then(() => this.releaseAttachment(token), error => {
                token.detaching = null;
                if (token.detached) {
                    this.releaseAttachment(token);
                    return;
                }
                this.reportAdvisoryError(error);
                throw error;
            });
        }
        try {
            await this.api(() => token.detaching, "detach work tab debugger");
        } catch (error) {
            this.reportAdvisoryError(error);
            throw error;
        }
    }
    async drainAttachments(key) {
        const tokens = [...this.attachments.values()].filter(token =>
            token.record.remote_key === key && token.cleanup);
        await Promise.all(tokens.map(token => this.detach(token.record, token)));
        if ([...this.attachments.values()].some(token =>
            token.record.remote_key === key && token.cleanup)) {
            fail("operation_conflict", "Debugger cleanup pending; retry after attachment cleanup completes");
        }
    }
    async publish(key) {
        const remote = this.remotes.get(key);
        if (!remote?.connected) fail("executor_unavailable", "Reconnect the selected workbench first");
        const record = this.records.get(key);
        const state = this.state(key);
        const payload = {remote_key: key, connection_id: remote.connection_id,
            principal_id: remote.principal_id, request_id: crypto.randomUUID(),
            executor_fence: clone(remote.executor_fence), state};
        const receipt = await bounded(() => {
            if (this.remotes.get(key) !== remote || this.records.get(key) !== record ||
                    !sameRef(this.state(key), state)) {
                fail("stale_authority", "Browser authorization changed before publication");
            }
            if (record?.phase === "enabled") record.publication = {remote, state: clone(state)};
            return this.nativeRequest("abush_browser", "workspace_state", clone(payload));
        }, "abush_browser.workspace_state", 8000).catch(error => {
            if (record?.publication?.remote === remote &&
                    sameRef(record.publication.state, state)) record.publication = null;
            throw error;
        });
        const expected = {type: "workspace_state_accepted", request_id: payload.request_id,
            executor_fence: payload.executor_fence, state};
        if (!sameRef(receipt, expected)) {
            if (record?.publication?.remote === remote &&
                    sameRef(record.publication.state, state)) record.publication = null;
            fail("stale_authority", "Workbench did not accept browser state");
        }
        if (this.remotes.get(key) !== remote || this.records.get(key) !== record ||
                !sameRef(this.state(key), state)) fail("stale_authority", "Browser authorization changed");
        if (record && record.phase === "enabled") record.agreed = true;
        if (!this.manualControls) {
            for (const [alias, canonical] of this.aliases) {
                const peer = this.remotes.get(alias);
                if (canonical !== key || !peer?.connected || !this.recordFor(alias) ||
                        !sameRef(this.executorIdentity(peer), this.executorIdentity(remote))) continue;
                const aliasPayload = {...payload, remote_key: alias, connection_id: peer.connection_id,
                    request_id: crypto.randomUUID()};
                const accepted = await this.native("abush_browser", "workspace_state", aliasPayload);
                if (this.remotes.get(alias) !== peer || this.remotes.get(key) !== remote ||
                        this.records.get(key) !== record || !sameRef(this.state(key), state)) {
                    fail("stale_authority", "Alias changed during browser state publication");
                }
                if (!sameRef(accepted, {...expected, request_id: aliasPayload.request_id})) {
                    fail("stale_authority", "Alias did not accept current browser state");
                }
            }
        }
        return state;
    }
    validRemote(remote) {
        return remote && text(remote.remote_key) && typeof remote.connected === "boolean" &&
            (!remote.connected || [remote.connection_id, remote.principal_id, remote.workspace_id]
                .every(text) && validFence(remote.executor_fence) &&
                text(remote.owner_display?.daemon_instance_id) &&
                text(remote.owner_display?.daemon_boot_id));
    }
    executorKey(remote) {
        return JSON.stringify([remote.principal_id, remote.owner_display?.daemon_instance_id]);
    }
    executorIdentity(remote) {
        return {daemon_boot_id: remote.owner_display?.daemon_boot_id,
            executor_fence: remote.executor_fence};
    }
    currentExecutor(remote) {
        const current = this.currentExecutors.get(this.executorKey(remote));
        return !current || sameRef(current.identity, this.executorIdentity(remote));
    }
    async remoteState(remote) {
        if (!this.validRemote(remote)) fail("invalid_command", "Invalid authenticated remote state");
        if (remote.connected && !this.manualControls) {
            const current = this.currentExecutors.get(this.executorKey(remote));
            const identity = this.executorIdentity(remote);
            if (current && (current.superseded.has(identity.daemon_boot_id) ||
                    current.identity.daemon_boot_id === identity.daemon_boot_id &&
                    !sameRef(current.identity, identity) &&
                    identity.executor_fence.epoch <= current.identity.executor_fence.epoch)) {
                fail("stale_authority", "Old executor cannot replace current executor");
            }
        }
        const key = remote.remote_key;
        const previous = this.remotes.get(key);
        if (sameRef(previous, remote)) {
            const record = this.recordFor(key);
            if (record && this.recordEnabledAgreed(key)) await this.validateReservation(record);
            else if (!this.manualControls) await this.reconcile(key);
            return true;
        }
        if (!remote.connected && previous && text(remote.connection_id) &&
                (remote.connection_id !== previous.connection_id ||
                remote.executor_fence && !sameRef(remote.executor_fence, previous.executor_fence) ||
                remote.owner_display && !sameRef(this.executorIdentity(remote), this.executorIdentity(previous)))) return true;
        if (!remote.connected && this.aliases.has(key)) {
            this.remotes.set(key, clone(remote));
            return true;
        }
        if (remote.connected && !this.manualControls) {
            const executorKey = this.executorKey(remote);
            const current = this.currentExecutors.get(executorKey);
            const identity = clone(this.executorIdentity(remote));
            const superseded = current?.superseded || new Set();
            if (current && current.identity.daemon_boot_id !== identity.daemon_boot_id) {
                superseded.add(current.identity.daemon_boot_id);
            }
            this.currentExecutors.set(executorKey, {identity, superseded});
            for (const other of this.records.values()) {
                if (this.executorKey(other) === executorKey &&
                        !sameRef(this.executorIdentity(other), identity) && other.phase === "enabled") {
                    this.closeAuthorization(other, "paused");
                    await this.persistConsent();
                    await this.detach(other);
                }
            }
            const record = [...this.records.values()].find(record =>
                this.stableIdentity({principal_id: record.principal_id,
                    workspace_id: record.page.workspace_id, owner_display: record.owner_display}) ===
                    this.stableIdentity(remote));
            if (!record) {
                const peer = [...this.remotes.values()].find(other => other.remote_key !== key &&
                    other.connected && sameRef(this.executorIdentity(other), this.executorIdentity(remote)) &&
                    this.stableIdentity(other) === this.stableIdentity(remote));
                if (peer) {
                    const canonical = this.aliases.get(peer.remote_key) || peer.remote_key;
                    this.aliases.set(key, canonical);
                    this.remotes.set(key, clone(remote));
                    if (this.restored) await this.reconcile(canonical);
                    return true;
                }
            }
            if (record && record.remote_key !== key) {
                const oldKey = record.remote_key;
                this.records.delete(oldKey);
                for (const [alias, canonical] of this.aliases) {
                    if (canonical === oldKey) this.aliases.set(alias, key);
                }
                this.aliases.set(oldKey, key);
                this.aliases.delete(key);
                record.remote_key = key;
                this.revisions.set(key, record.revision);
                this.records.set(key, record);
            }
        }
        const record = this.records.get(key);
        if (!remote.connected || previous && !sameRef(previous, remote)) this.bumpControl(key);
        const acceptedRemote = clone(remote);
        this.remotes.set(key, acceptedRemote);
        if (!this.revisions.has(key)) this.nextRevision(key);
        if (!this.restored && !this.manualControls) return true;
        if (record && record.phase !== "disabled") {
            const unchanged = remote.connected && (!this.manualControls ||
                remote.connection_id === record.connection_id) && remote.principal_id === record.principal_id &&
                remote.workspace_id === record.page.workspace_id &&
                sameRef(remote.executor_fence, record.executor_fence) &&
                sameRef(remote.owner_display, record.owner_display);
            if (!unchanged) {
                const attachment = record.attachment;
                this.closeAuthorization(record, "paused");
                await this.persistConsent();
                await this.detach(record, attachment);
            } else {
                if (record.phase === "enabled") await this.validateReservation(record);
                record.connection_id = remote.connection_id;
                if (record.phase === "enabled" && record.agreed) {
                    await this.publish(key);
                    await this.flushEffects(key);
                    return true;
                }
            }
        }
        if (this.remotes.get(key) !== acceptedRemote) return false;
        await this.persistConsent();
        if (this.remotes.get(key) !== acceptedRemote) return false;
        if (remote.connected) {
            if (!this.preferenceReady && !this.manualControls) return true;
            if (this.automationOn && !this.manualControls) await this.reconcile(key);
            else {
                await this.publish(key);
                await this.flushEffects(key);
            }
        }
        return true;
    }
    async invalidateRemote() {
        this.transportGeneration++;
        if (!this.restored) this.restoreRevoked = true;
        for (const key of new Set([...this.remotes.keys(), ...this.controls])) this.bumpControl(key);
        for (const remote of this.remotes.values()) remote.connected = false;
        for (const record of this.records.values()) {
            if (record.phase !== "disabled") this.closeAuthorization(record, "paused");
        }
        await Promise.all([...this.records.values()].map(record => this.detach(record)));
        await this.persistConsent();
    }
    async refreshRemotes() {
        const response = await this.native("abush_browser", "refresh_remotes", {});
        const remotes = Array.isArray(response) ? response : response?.remotes;
        if (!Array.isArray(remotes)) fail("invalid_command", "Unable to list authenticated workbenches");
        for (const remote of remotes) await this.remoteState(remote);
        return this.popupState();
    }
    setBrowserExecutorStatus(status) {
        this.browserExecutorStatus = clone(status);
    }
    popupState() {
        return {ok: true, automation_on: this.automationOn, preference_pending: this.preferencePending,
            browser_executor: clone(this.browserExecutorStatus),
            stop_pending: this.automationInhibited && this.preferencePending,
            error: this.lastError || this.browserExecutorStatus.error,
            cleanup_pending: [...this.attachments.values()].some(token => token.cleanup),
            remotes: [...this.remotes.values()].map(remote => ({
            remote_key: remote.remote_key, principal_id: remote.principal_id,
            display: remote.display_name || remote.connection_display_name || remote.owner_display?.workbench_name || "Abush",
            connected: remote.connected, state: this.state(remote.remote_key),
            error: this.remoteErrors.get(remote.remote_key) || null,
            creation_unknown: Object.entries(this.creations).some(([key, entry]) =>
                entry.phase === "unknown" && (key.endsWith(`\0${remote.remote_key}`) ||
                    entry.identity === this.stableIdentity(remote))),
            agreed: this.recordEnabledAgreed(remote.remote_key),
            cleanup_pending: [...this.attachments.values()].some(token =>
                token.record.remote_key === remote.remote_key && token.cleanup),
        }))};
    }
    async setAutomation(on, ready = null) {
        if (typeof on !== "boolean") fail("invalid_command", "Expected global automation preference");
        const generation = ++this.preferenceGeneration;
        const previousChoice = this.automationOn;
        const previousReady = this.preferenceReady;
        const previousPending = this.preferencePending;
        if (!on) this.automationInhibited = true;
        this.preferencePending = true;
        this.automationOn = false;
        this.preferenceReady = false;
        for (const key of new Set([...this.remotes.keys(), ...this.controls])) this.bumpControl(key);
        for (const record of this.records.values()) {
            if (record.phase !== "disabled") this.closeAuthorization(record, "paused");
        }
        const cleanup = Promise.all([...this.records.values()].map(record => this.detach(record)));
        cleanup.catch(error => this.reportAdvisoryError(error));
        const write = this.preferenceTail.catch(() => {}).then(() =>
            this.chrome.storage.local.set({[AUTOMATION_KEY]: on})).then(() => {
                if (generation === this.preferenceGeneration) {
                    this.preferencePending = false;
                    this.preferenceReady = true;
                    this.automationOn = on;
                    if (on) this.automationInhibited = false;
                    this.lastError = null;
                }
            }, error => {
                if (generation === this.preferenceGeneration) {
                    this.preferencePending = !on || previousPending;
                    this.automationOn = this.automationInhibited ? false : previousChoice;
                    this.preferenceReady = this.automationInhibited ? false : previousReady;
                    if (!on) error = new Error(`Automation off locally, but Stop is not saved. Retry Stop; ` +
                        `a browser/worker restart may restore automation while storage is unavailable. ${error.message}`);
                    this.reportAdvisoryError(error);
                }
                throw error;
            });
        this.preferenceTail = write;
        try {
            await this.api(() => write, "persist global automation preference");
            if (generation !== this.preferenceGeneration) return this.popupState();
            this.preferenceReady = true;
            this.automationOn = on;
            if (on && ready) await this.api(ready, "prepare browser integration");
            if (generation !== this.preferenceGeneration) return this.popupState();
            await this.persistConsent();
            await cleanup;
            if (on) await Promise.all([...this.remotes.keys()].map(key => this.reconcile(key)));
            else {
                await Promise.all([...this.remotes.values()].filter(remote => remote.connected &&
                    this.restored && !this.aliases.has(remote.remote_key))
                    .map(remote => this.publish(remote.remote_key)));
                await bounded(() => Promise.all([...this.operations.values()].map(op =>
                    op.done.catch(() => {}))), "drain stopped browser work", 5000);
            }
            return this.popupState();
        } catch (error) {
            if (generation === this.preferenceGeneration) {
                if (!on && this.preferencePending && !error.message.startsWith("Automation off locally")) {
                    error = new Error(`Automation off locally, but Stop is not saved. Retry Stop; ` +
                        `a browser/worker restart may restore automation while storage is unavailable. ${error.message}`);
                }
                this.reportAdvisoryError(error);
            }
            return {...this.popupState(), ok: false};
        }
    }
    stableIdentity(remote) {
        return JSON.stringify([remote.principal_id, remote.workspace_id,
            remote.owner_display?.daemon_instance_id]);
    }
    reconcile(key) {
        this.reconcileVersions.set(key, (this.reconcileVersions.get(key) || 0) + 1);
        if (this.reconciliations.has(key)) return this.reconciliations.get(key);
        const task = Promise.resolve().then(async () => {
            let version;
            do {
                version = this.reconcileVersions.get(key);
                if (!this.restored || !this.preferenceReady || !this.automationOn || this.manualControls) return;
                const remote = this.remotes.get(key);
                if (!remote?.connected || this.aliases.has(key)) return;
                try {
                    if (this.popupState().remotes.find(item => item.remote_key === key)?.creation_unknown) {
                        fail("unknown_effect", "Tab creation unknown: inspect partial tab; no automatic retry");
                    }
                    const record = this.records.get(key);
                    if (this.recordEnabledAgreed(key)) {
                        await this.validateReservation(record);
                    } else if (record && record.phase !== "disabled") {
                        await this.resume(key);
                    } else {
                        await this.control("enable", key);
                    }
                    await this.flushEffects(key);
                    this.remoteErrors.delete(key);
                } catch (error) {
                    this.remoteErrors.set(key, error.message || String(error));
                    this.reportAdvisoryError(error);
                }
            } while (version !== this.reconcileVersions.get(key));
        }).finally(() => this.reconciliations.delete(key));
        this.reconciliations.set(key, task);
        return task;
    }
    recordEnabledAgreed(key) {
        const record = this.recordFor(key);
        const remote = this.remotes.get(key);
        return !!(record?.phase === "enabled" && record.agreed && remote?.connected &&
            (!this.aliases.has(key) ? record.connection_id === remote.connection_id : true) &&
            record.principal_id === remote.principal_id && record.page.workspace_id === remote.workspace_id &&
            sameRef(record.owner_display, remote.owner_display) &&
            sameRef(record.executor_fence, remote.executor_fence) &&
            this.currentExecutor(remote));
    }
    async reconcileAll() {
        await Promise.all([...this.remotes.keys()].map(key => this.reconcile(key)));
    }
    async recoverPreparation(key, confirmed) {
        if (confirmed !== true || !text(key)) fail("invalid_command", "Confirm abandonment of unknown preparation");
        const remote = this.selectedRemote(key);
        const entries = Object.entries(this.creations).filter(([, entry]) =>
            entry.phase === "unknown" && entry.identity === this.stableIdentity(remote));
        if (!entries.length) fail("invalid_command", "Only unknown preparation can be recovered");
        if (this.controls.has(key)) fail("operation_conflict", "Preparation still running; wait for its bounded result");
        this.bumpControl(key);
        const record = this.recordFor(key);
        if (record) this.closeAuthorization(record, "disabled");
        for (const [creationKey, entry] of entries) {
            this.creations[creationKey] = {...entry, phase: "abandoned", outcome: "unknown",
                abandoned_at: Date.now()};
        }
        try {
            await this.persistWal();
        } catch (error) {
            for (const [creationKey, entry] of entries) this.creations[creationKey] = entry;
            this.reportAdvisoryError(error);
            return {...this.popupState(), ok: false};
        }
        let consentError;
        try {
            await this.persistConsent();
        } catch (error) {
            consentError = error;
        }
        await this.detach(record);
        this.lastError = null;
        this.remoteErrors.delete(key);
        if (this.automationOn) await this.reconcile(key);
        if (consentError) this.reportAdvisoryError(consentError);
        return this.popupState();
    }
    async control(action, key, ready = null) {
        if (action === "refresh") return this.refreshRemotes();
        if (!["enable", "pause", "takeover", "resume", "disconnect"].includes(action) || !text(key)) {
            fail("invalid_command", "Unknown popup action");
        }
        const record = this.records.get(key);
        if (["pause", "takeover", "disconnect"].includes(action)) {
            this.bumpControl(key);
            const attachment = record?.attachment;
            if (action === "disconnect") {
                const creationKey = `${this.browserBootId}\0${key}`;
                if (this.creations[creationKey]?.phase === "unknown") delete this.creations[creationKey];
                this.lastError = null;
            }
            if (record) this.closeAuthorization(record, action === "disconnect" ? "disabled" : "paused");
            else this.nextRevision(key);
            if (ready) await ready();
            if (action === "disconnect") {
                const creationKey = `${this.browserBootId}\0${key}`;
                if (this.creations[creationKey]?.phase === "unknown") delete this.creations[creationKey];
            }
            await this.persistConsent();
            if (action === "disconnect") await this.persistWal();
            await this.detach(record, attachment);
            if (this.remotes.get(key)?.connected) await this.publish(key);
            return this.popupState();
        }
        if (this.controls.has(key)) fail("operation_conflict", "Enable is already in progress");
        this.controls.add(key);
        try {
            if (action === "resume") await this.resume(key);
            else await this.enable(key);
            this.lastError = null;
            return this.popupState();
        } catch (error) {
            this.reportAdvisoryError(error);
            throw error;
        } finally {
            this.controls.delete(key);
            if (!this.controls.size && this.restored) this.revokedTabs.clear();
        }
    }
    selectedRemote(key) {
        const remote = this.remotes.get(key);
        if (!remote?.connected) fail("executor_unavailable", "Select a connected authenticated workbench");
        if (!this.manualControls && !this.currentExecutor(remote)) {
            fail("stale_authority", "Selected alias has an old executor");
        }
        return remote;
    }
    async enable(key) {
        const remote = this.selectedRemote(key);
        const boot = this.browserBootId;
        const transport = this.transportGeneration;
        const token = this.controlTokens.get(key) || 0;
        let abandoned = false;
        const check = () => {
            if ((!this.manualControls && (!this.automationOn || !this.preferenceReady)) ||
                    abandoned || transport !== this.transportGeneration || boot !== this.browserBootId ||
                    this.remotes.get(key) !== remote ||
                    token !== (this.controlTokens.get(key) || 0)) {
                fail("stale_authority", "Remote or consent changed during enable");
            }
        };
        const existing = this.records.get(key);
        await this.drainAttachments(key);
        check();
        if (existing?.phase !== "disabled" && existing) {
            fail("operation_conflict", "Use Resume for the original work tab, or Disconnect first");
        }
        const creationKey = this.manualControls ? `${this.browserBootId}\0${key}` :
            `${this.browserBootId}\0${key}\0${crypto.randomUUID()}`;
        if (Object.entries(this.creations).some(([creationKey, entry]) => entry.phase === "unknown" &&
                (creationKey.endsWith(`\0${key}`) || entry.identity === this.stableIdentity(remote)))) {
            fail("unknown_effect", "Interrupted tab creation is unknown. Inspect the partial tab; automatic retry is blocked");
        }
        const available = this.manualControls ?
            [await this.api(() => this.chrome.windows.getCurrent(), "select current window")] :
            await this.api(() => this.chrome.windows.getAll({windowTypes: ["normal"]}),
                "find existing normal window");
        check();
        const window = available.find(candidate => candidate.type === "normal");
        if (!window) fail("executor_unavailable", "Automation unavailable: no existing normal Chrome window");
        const record = {remote_key: key, principal_id: remote.principal_id,
            connection_id: remote.connection_id,
            executor_fence: clone(remote.executor_fence), owner_display: clone(remote.owner_display),
            browser_boot_id: this.browserBootId, enable_id: crypto.randomUUID(),
            revision: this.nextRevision(key), page: {workspace_id: remote.workspace_id,
                page_id: crypto.randomUUID(), generation: 1}, window_id: window.id,
            tab_id: null, group_id: null, phase: "paused", agreed: false, attached: false};
        if (!this.manualControls) {
            record.preparation = "reserved";
            this.records.set(key, record);
            await this.persistConsent();
            check();
            await this.resume(key);
            return;
        }
        const creationRevision = record.revision;
        const creation = {phase: "unknown", enable_id: record.enable_id,
            identity: this.stableIdentity(remote)};
        this.creations[creationKey] = creation;
        try {
            await this.persistWal();
            check();
            const staging = {url: `about:blank#abush-work-${record.enable_id}`,
                browser_boot_id: boot, window_id: window.id, tab_id: null,
                group_id: null, stage: "creating", generation: creationRevision};
            this.workCreations.set(staging.url, staging);
            await this.persistInheritance();
            check();
            this.workCreatesInFlight++;
            const rawCreation = Promise.resolve().then(() => {
                check();
                return this.chrome.tabs.create({windowId: window.id, url: staging.url,
                    active: false, index: RIGHT_EDGE_TAB_INDEX});
            }).then(async created => {
                staging.tab_id = created.id;
                staging.stage = "created";
                this.forgetPersonalCandidate(created.id);
                await this.persistInheritance();
                check();
                record.tab_id = created.id;
                this.pendingCreations.set(created.id, record);
                return created;
            }).catch(error => {
                this.reportAdvisoryError(error);
                throw error;
            }).finally(() => {
                this.workCreatesInFlight--;
                this.drainPersonalCandidates();
            });
            const tab = await this.api(() => rawCreation, "create inactive work tab");
            const checkTab = () => {
                check();
                if (this.revokedTabs.has(tab.id) || this.revisions.get(key) !== creationRevision) {
                    fail("stale_page", "Created work tab was moved, replaced, or revoked");
                }
            };
            checkTab();
            await this.groupWorkTab(record, staging,
                displayTitle(remote.display_name || remote.connection_display_name), checkTab);
            checkTab();
            await this.validateTab(record);
            checkTab();
            this.records.set(key, record);
            this.drainPersonalCandidates();
            this.ownershipChanged(record.tab_id);
            await this.persistConsent();
            if (this.revisions.get(key) !== creationRevision || record.phase !== "paused") {
                fail("stale_authority", "Consent changed during enable persistence");
            }
            const revision = record.revision;
            creation.phase = "completed";
            await this.persistWal();
            if (record.revision !== revision || this.remotes.get(key) !== remote) {
                fail("stale_authority", "Consent changed before initial resume");
            }
            await this.resume(key);
            this.workCreations.delete(staging.url);
            await this.persistInheritance();
        } catch (error) {
            abandoned = true;
            if (record.revision === this.revisions.get(key) && record.phase !== "disabled") {
                record.agreed = false;
                record.phase = "paused";
            }
            throw new Error(`Preparation failed; no tabs were removed. ${error.message}. Inspect any partial tab; unknown creation is never retried automatically`);
        } finally {
            this.pendingCreations.delete(record.tab_id);
        }
    }
    async materialize(record, op) {
        if (record.tab_id !== null) return this.validateReservation(record);
        const boot = this.browserBootId;
        const transport = this.transportGeneration;
        const remote = this.remotes.get(op.payload.remote_key);
        const revision = record.revision;
        const recordKey = record.remote_key;
        const control = this.controlTokens.get(recordKey) || 0;
        let abandoned = false;
        const check = () => {
            if (abandoned || boot !== this.browserBootId || transport !== this.transportGeneration ||
                    this.remotes.get(op.payload.remote_key) !== remote ||
                    this.records.get(record.remote_key) !== record ||
                    record.revision !== revision || control !== (this.controlTokens.get(recordKey) || 0) ||
                    this.authorization(op.payload, op) !== record) {
                fail("stale_authority", "Work page creation authorization changed");
            }
        };
        check();
        await this.validateReservation(record);
        check();
        if (Object.values(this.creations).some(entry => entry.phase === "unknown" &&
                entry.identity === this.stableIdentity(remote))) {
            fail("unknown_effect", "Tab creation unknown; inspect partial tab before retrying");
        }
        const available = await this.api(() => this.chrome.windows.getAll({windowTypes: ["normal"]}),
            "select existing work window");
        check();
        const window = available.find(candidate => candidate.type === "normal" &&
            candidate.id === record.window_id) || available.find(candidate => candidate.type === "normal");
        if (!window) fail("executor_unavailable", "Automation unavailable: no existing normal Chrome window");
        record.window_id = window.id;
        const creationKey = `${boot}\0${record.enable_id}\0${crypto.randomUUID()}`;
        const creation = {phase: "unknown", enable_id: record.enable_id,
            identity: this.stableIdentity(remote)};
        this.creations[creationKey] = creation;
        let createIssued = false;
        let completedDurably = false;
        const staging = {url: `about:blank#abush-work-${record.enable_id}`,
            browser_boot_id: boot, window_id: record.window_id, tab_id: null,
            group_id: null, stage: "creating", generation: revision};
        try {
            await this.persistWal();
            check();
            this.workCreations.set(staging.url, staging);
            await this.persistInheritance();
            check();
            this.workCreatesInFlight++;
            const rawCreation = Promise.resolve().then(() => {
                check();
                createIssued = true;
                return this.chrome.tabs.create({windowId: record.window_id,
                    url: staging.url, active: false, index: RIGHT_EDGE_TAB_INDEX});
            }).then(async created => {
                check();
                staging.tab_id = created.id;
                staging.stage = "created";
                this.forgetPersonalCandidate(created.id);
                await this.persistInheritance();
                check();
                record.tab_id = created.id;
                record.preparation = "materializing";
                this.pendingCreations.set(created.id, record);
                return created;
            }).finally(() => {
                this.workCreatesInFlight--;
                this.drainPersonalCandidates();
            });
            const tab = await this.api(() => rawCreation, "create inactive work tab");
            const checkTab = () => {
                check();
                if (this.revokedTabs.has(tab.id)) fail("stale_page", "Created work tab was revoked");
            };
            checkTab();
            await this.groupWorkTab(record, staging,
                displayTitle(remote.display_name || remote.connection_display_name), checkTab);
            checkTab();
            await this.validateTab(record);
            checkTab();
            record.preparation = "materialized";
            this.ownershipChanged(record.tab_id);
            await this.persistConsent();
            checkTab();
            creation.phase = "completed";
            await this.persistWal();
            completedDurably = true;
            checkTab();
            this.workCreations.delete(staging.url);
            await this.persistInheritance();
            checkTab();
        } catch (error) {
            abandoned = true;
            if (createIssued && !completedDurably) creation.phase = "unknown";
            if (!createIssued) {
                creation.phase = "not_applied";
                try {
                    await this.persistWal();
                } catch (failure) {
                    creation.phase = "unknown";
                    throw failure;
                }
                this.workCreations.delete(staging.url);
                await this.persistInheritance();
            }
            if (createIssued && this.records.get(record.remote_key) === record &&
                    record.revision === revision) {
                this.closeAuthorization(record, "paused");
                await this.persistConsent();
            }
            throw error;
        } finally {
            abandoned = true;
            this.pendingCreations.delete(record.tab_id);
        }
    }
    groupWorkTab(record, staging, title, check) {
        const task = this.presentationTail.catch(() => {}).then(async () => {
            check();
            const groups = await this.chrome.tabGroups.query({windowId: record.window_id, title});
            check();
            const candidate = groups.filter(group => Number.isSafeInteger(group.id) && group.id >= 0 &&
                group.windowId === record.window_id && group.title === title).sort((a, b) => a.id - b.id)[0];
            await this.validateTab(record);
            check();
            if (candidate) {
                const current = await this.chrome.tabGroups.get(candidate.id);
                check();
                if (current.id !== candidate.id || current.windowId !== record.window_id || current.title !== title) {
                    fail("stale_page", "Presentation group changed during work tab preparation");
                }
            }
            const defaults = {changed: false, groups: new Map()};
            if (!candidate) this.presentationDefaults.set(record, defaults);
            try {
                record.group_reused = !!candidate;
                staging.group_reused = record.group_reused;
                record.grouping = true;
                try {
                    // The title selects a container only; the sole membership target is the new tab.
                    record.group_id = await this.chrome.tabs.group({tabIds: [record.tab_id],
                        ...(candidate ? {groupId: candidate.id} : {})});
                } finally {
                    delete record.grouping;
                }
                check();
                const groupedId = record.group_id;
                const groupGeneration = record.group_generation || 0;
                staging.group_id = groupedId;
                staging.stage = "grouped";
                await this.persistInheritance();
                this.drainPersonalCandidates();
                check();
                if (!candidate) {
                    const tab = await this.validateTab(record);
                    check();
                    if (record.group_id !== groupedId || record.group_reused ||
                            (record.group_generation || 0) !== groupGeneration || tab.groupId !== groupedId) {
                        fail("stale_page", "Created presentation group changed before labeling");
                    }
                    const group = await this.chrome.tabGroups.get(groupedId);
                    check();
                    const members = await this.chrome.tabs.query({windowId: record.window_id, groupId: groupedId});
                    check();
                    const witness = defaults.groups.get(groupedId);
                    if (!defaults.changed && witness?.created && !witness.changed &&
                            sameRef(this.presentationVisuals(group), witness.created) &&
                            record.group_id === groupedId && !record.group_reused &&
                            (record.group_generation || 0) === groupGeneration &&
                            group.id === groupedId && group.windowId === record.window_id &&
                            !group.title && !group.collapsed && members.length === 1 &&
                            members[0].id === record.tab_id && members[0].active === false &&
                            this.activeTabs.get(record.window_id)?.tab_id !== record.tab_id) {
                        await this.chrome.tabGroups.update(groupedId, {title, color: "purple", collapsed: true});
                    }
                }
                check();
            } finally {
                this.presentationDefaults.delete(record);
            }
        });
        // A caller timeout cannot release the queue while Chrome's grouping effect is unresolved.
        this.presentationTail = task;
        return this.api(() => task, "prepare work tab presentation group");
    }
    presentationVisuals(group) {
        return {windowId: group.windowId, title: group.title || "", color: group.color,
            collapsed: group.collapsed};
    }
    presentationGroupChanged(group, event = "updated", memberId = null) {
        if (!Number.isSafeInteger(group.id) || group.id < 0) return;
        this.presentationRevision++;
        for (const [record, defaults] of this.presentationDefaults) {
            if (event === "membership" && memberId === record.tab_id ||
                    group.windowId !== undefined && group.windowId !== record.window_id) continue;
            if (!defaults.groups.has(group.id)) {
                if (defaults.groups.size >= 128) { defaults.changed = true; continue; }
                defaults.groups.set(group.id, {created: null, visual: null, changed: false});
            }
            const witness = defaults.groups.get(group.id);
            if (event === "created" || event === "updated") {
                const visual = this.presentationVisuals(group);
                if (witness.created && !sameRef(witness.created, visual) ||
                        witness.visual && !sameRef(witness.visual, visual)) witness.changed = true;
                if (event === "created") witness.created = visual;
                else witness.visual = visual;
            } else witness.changed = true;
        }
    }
    async seedActiveTabs() {
        const boot = this.browserBootId;
        const generations = new Map(this.activationGenerations);
        try {
            const active = await this.api(() => this.chrome.tabs.query({active: true}), "read active tab identities");
            if (boot !== this.browserBootId) return;
            for (const tab of active) {
                if (Number.isSafeInteger(tab.id) && Number.isSafeInteger(tab.windowId) &&
                        !this.removedActiveTabs.has(tab.id) &&
                        generations.get(tab.windowId) === this.activationGenerations.get(tab.windowId)) {
                    this.activeTabs.set(tab.windowId, {tab_id: tab.id});
                }
            }
            this.activeBoot = boot;
            await this.persistInheritance();
        } catch (error) { this.reportAdvisoryError(error); }
    }
    tabActivated(info) {
        if (!Number.isSafeInteger(info.tabId) || !Number.isSafeInteger(info.windowId)) return;
        for (const [record, defaults] of this.presentationDefaults) {
            if (info.tabId === record.tab_id) defaults.changed = true;
        }
        const previous = this.activeTabs.get(info.windowId);
        if (previous?.tab_id === info.tabId) return;
        this.activationGenerations.set(info.windowId, (this.activationGenerations.get(info.windowId) || 0) + 1);
        for (const [tabId, placement] of this.personalPlacements) {
            if (placement.window_id === info.windowId && tabId !== info.tabId) placement.retired = true;
        }
        this.activeTabs.set(info.windowId, {tab_id: info.tabId, previous_tab_id: previous?.tab_id,
            previous_unseeded: !previous && (!this.activeBoot || this.activeBoot !== this.browserBootId)});
        return this.persistInheritance();
    }
    inheritanceSourceForId(tabId, windowId) {
        const record = [...this.records.values(), ...this.pendingCreations.values()].find(record =>
            tabId === record.tab_id && record.browser_boot_id === this.browserBootId &&
            record.phase !== "disabled" && !this.revokedTabs.has(record.tab_id) &&
            record.window_id === windowId && record.group_id >= 0 && record.group_id !== null);
        return record ? {tab_id: record.tab_id, enable_id: record.enable_id,
            group_id: record.group_id, group_generation: record.group_generation || 0,
            window_id: record.window_id, browser_boot_id: this.browserBootId} : null;
    }
    personalCreationContext(tab) {
        if (Number.isSafeInteger(tab.openerTabId)) return {source_tab_id: tab.openerTabId};
        if (tab.active !== true) return {};
        const active = this.activeTabs.get(tab.windowId);
        if (!active) return {prior_active: true};
        if (active.tab_id !== tab.id) return {source_tab_id: active.tab_id};
        return active.previous_unseeded ? {prior_active: true} : {source_tab_id: active.previous_tab_id};
    }
    personalInheritanceSource(tab) {
        if (!this.restored && this.inheritanceBoot !== this.browserBootId) return null;
        return this.inheritanceSourceForId(this.personalCreationContext(tab).source_tab_id, tab.windowId);
    }
    inheritedWorkGroups(candidate) {
        const groups = this.workGroups();
        const source = candidate?.inheritance_source;
        if (source && source.browser_boot_id === this.browserBootId &&
                [...this.records.values(), ...this.pendingCreations.values()].some(record =>
                    record.browser_boot_id === this.browserBootId && record.phase !== "disabled" &&
                    !this.revokedTabs.has(record.tab_id) && record.tab_id === source.tab_id &&
                    record.enable_id === source.enable_id && record.window_id === source.window_id &&
                    record.group_id === source.group_id &&
                    (record.group_generation || 0) === (source.group_generation || 0))) groups.add(source.group_id);
        return groups;
    }
    persistInheritance() {
        // Events can arrive before bootstrap and before the previous worker's journal is read.
        if (!this.browserBootId || this.inheritanceBoot !== this.browserBootId) return Promise.resolve();
        const snapshot = {browser_boot_id: this.browserBootId,
            active_tabs: [...this.activeTabs].map(([window_id, active]) => ({window_id,
                tab_id: active.tab_id, inheritance_source: this.inheritanceSourceForId(active.tab_id, window_id)})),
            candidates: [...this.personalCandidates.values()].map(({timer, ...value}) => value),
            creations: [...this.workCreations.values()].map(clone)};
        const write = this.inheritanceTail.catch(() => {}).then(() =>
            this.chrome.storage.session.set({[INHERITANCE_KEY]: snapshot}));
        this.inheritanceTail = write;
        return this.api(() => write, "persist personal inheritance obligations")
            .catch(error => this.reportAdvisoryError(error));
    }
    async restoreInheritance() {
        try {
            const boot = this.browserBootId;
            const saved = (await this.api(() => this.chrome.storage.session.get(INHERITANCE_KEY),
                "read personal inheritance obligations"))[INHERITANCE_KEY];
            if (boot !== this.browserBootId) return;
            for (const value of saved?.browser_boot_id === boot ? saved.creations || [] : []) {
                if (value.browser_boot_id !== boot || !text(value.url) ||
                        !value.url.startsWith("about:blank#abush-work-") ||
                        !Number.isSafeInteger(value.window_id) || !Number.isSafeInteger(value.generation) ||
                        !["creating", "created", "grouped"].includes(value.stage)) continue;
                if (Number.isSafeInteger(value.tab_id)) {
                    try {
                        const tab = await this.api(() => this.chrome.tabs.get(value.tab_id),
                            "verify partial work creation identity");
                        if (tab.windowId !== value.window_id ||
                                value.group_id !== null && tab.groupId !== value.group_id) continue;
                    } catch (error) { this.reportAdvisoryError(error); continue; }
                } else if (value.stage !== "creating" || value.group_id !== null) continue;
                if (boot !== this.browserBootId) return;
                const record = [...this.records.values()].find(record => record.tab_id === value.tab_id);
                if (record?.group_reused === true) {
                    value.group_id = record.group_id;
                    value.group_reused = true;
                }
                if (!this.workCreations.has(value.url)) this.workCreations.set(value.url, value);
            }
            for (const value of saved?.browser_boot_id === boot ? saved.candidates || [] : []) {
                if (!Number.isSafeInteger(value.tab_id) || !Number.isSafeInteger(value.window_id) ||
                        !Number.isSafeInteger(value.group_id) || !Number.isSafeInteger(value.generation) ||
                        value.browser_boot_id !== boot || this.personalCandidates.has(value.tab_id)) continue;
                this.personalCandidates.set(value.tab_id, value);
                if (value.stage !== "expired") this.schedulePersonalExpiry(value);
                const generation = value.generation;
                try {
                    const tab = await this.api(() => this.chrome.tabs.get(value.tab_id),
                        "verify pending personal inheritance");
                    if (this.personalCandidates.get(value.tab_id) !== value ||
                            generation !== value.generation) continue;
                    if (boot !== this.browserBootId || tab.windowId !== value.window_id ||
                            tab.groupId !== value.group_id) this.forgetPersonalCandidate(value.tab_id);
                } catch (error) {
                    this.forgetPersonalCandidate(value.tab_id);
                    this.reportAdvisoryError(error);
                }
            }
            if (boot !== this.browserBootId) return;
            for (const candidate of this.personalCandidates.values()) {
                const context = candidate.creation_context;
                if (!context || candidate.inheritance_source) continue;
                if (saved?.browser_boot_id === boot) {
                    const source = context.prior_active ?
                        saved.active_tabs?.find(active => active.window_id === candidate.window_id)?.inheritance_source :
                        this.inheritanceSourceForId(context.source_tab_id, candidate.window_id);
                    if (source && !context.invalidated) candidate.inheritance_source = source;
                }
                delete candidate.creation_context;
            }
            this.inheritanceBoot = boot;
            await this.persistInheritance();
            this.drainPersonalCandidates();
        } catch (error) { this.reportAdvisoryError(error); }
    }
    workGroups() {
        return new Set([...this.records.values(), ...this.pendingCreations.values(),
            ...this.workCreations.values()].filter(value => value.group_reused !== true)
            .map(value => value.group_id).filter(id => id >= 0 && id !== null));
    }
    forgetPersonalCandidate(tabId, preservePlacement = false) {
        this.presentationRevision++;
        if (!preservePlacement) {
            this.personalPlacements.get(tabId)?.wake?.();
            this.personalPlacements.delete(tabId);
        }
        clearTimeout(this.personalCandidates.get(tabId)?.timer);
        if (this.personalCandidates.delete(tabId)) return this.persistInheritance();
    }
    drainPersonalCandidates() {
        for (const id of this.personalCandidates.keys()) {
            this.isolateNewPersonalTab(id).catch(error => this.reportAdvisoryError(error));
        }
    }
    newPersonalCandidate(tab) {
        if (!Number.isSafeInteger(tab.id)) return;
        this.presentationRevision++;
        this.presentationGroupChanged({id: tab.groupId, windowId: tab.windowId}, "membership", tab.id);
        const context = this.personalCreationContext(tab);
        const source = this.personalInheritanceSource(tab);
        const active = this.activeTabs.get(tab.windowId);
        if (active?.tab_id === tab.id) {
            delete active.previous_tab_id;
            delete active.previous_unseeded;
        }
        const candidate = {tab_id: tab.id, window_id: tab.windowId, group_id: tab.groupId ?? -1,
            initial_group: tab.groupId ?? -1, browser_boot_id: this.browserBootId,
            inheritance_source: source,
            ...(!this.restored ? {creation_context: context} : {}),
            stage: "created", generation: 1};
        this.personalCandidates.set(tab.id, candidate);
        this.schedulePersonalExpiry(candidate);
        this.persistInheritance();
        const isolation = this.isolateNewPersonalTab(tab.id);
        const url = tab.pendingUrl || tab.url;
        if (this.restored && this.inheritanceBoot === this.browserBootId && tab.active === true &&
                !tab.pinned && text(url) && !url.startsWith("about:blank#abush-work-") &&
                !url.startsWith(this.chrome.runtime.getURL("ordinary-staging.html")) &&
                (tab.groupId === -1 || source?.group_id === tab.groupId)) {
            this.positionNewPersonalTab(tab, isolation).catch(error => this.reportAdvisoryError(error));
        }
        return isolation;
    }
    positionNewPersonalTab(created, isolation) {
        const placement = {browser_boot_id: this.browserBootId, window_id: created.windowId,
            url: created.pendingUrl || created.url, deadline: Date.now() + API_TIMEOUT_MS,
            isolation, retired: false, expectedMove: null};
        this.personalPlacements.set(created.id, placement);
        const current = () => this.personalPlacements.get(created.id) === placement &&
            this.browserBootId === placement.browser_boot_id && !placement.retired &&
            Date.now() < placement.deadline && !this.ownsTab(created.id) &&
            !this.pendingCreations.has(created.id);
        const task = this.presentationTail.catch(() => {}).then(async () => {
            while (current()) {
                const changed = new Promise(resolve => { placement.wake = resolve; });
                const isolation = placement.isolation;
                try {
                    await isolation;
                } finally {
                    await placement.isolationEffect;
                }
                if (!current()) return;
                const {index, tab, revision} = await this.ordinaryPlacement(created.windowId,
                    (task, label) => this.api(task, label), () => {}, created.id);
                if (!current() || index === RIGHT_EDGE_TAB_INDEX) return;
                if (isolation !== placement.isolation || revision !== this.presentationRevision) continue;
                if (tab?.groupId >= 0) {
                    const candidate = this.personalCandidates.get(created.id);
                    if (candidate?.initial_group !== -1 || candidate.generation !== 1 ||
                            candidate.inheritance_source?.group_id !== tab.groupId) return;
                    await bounded(() => changed, "wait for initial personal inheritance",
                        Math.max(1, placement.deadline - Date.now()));
                    continue;
                }
                if (!tab || tab.windowId !== placement.window_id || tab.groupId !== -1 ||
                        tab.pinned || !tab.active || tab.index <= index ||
                        (tab.pendingUrl || tab.url) !== placement.url) return;
                placement.expectedMove = {windowId: tab.windowId, fromIndex: tab.index, toIndex: index};
                await this.chrome.tabs.move(created.id, {index});
                return;
            }
        }).finally(() => {
            if (this.personalPlacements.get(created.id) === placement) this.personalPlacements.delete(created.id);
        });
        this.presentationTail = task;
        return this.api(() => task, "place new personal tab before work groups").finally(() => {
            placement.retired = true;
        });
    }
    personalTabMoved(tabId, info) {
        this.presentationRevision++;
        const placement = this.personalPlacements.get(tabId);
        if (!placement) return;
        if (placement.expectedMove && sameRef(placement.expectedMove, info)) placement.expectedMove = null;
        else placement.retired = true;
        placement.wake?.();
    }
    schedulePersonalExpiry(candidate) {
        // The first creation generation allows Chrome's delayed initial group update.
        candidate.timer = setTimeout(() => {
            if (this.personalCandidates.get(candidate.tab_id) !== candidate) return;
            candidate.stage = "expired";
            return this.persistInheritance().then(() => this.isolateNewPersonalTab(candidate.tab_id))
                .catch(error => this.reportAdvisoryError(error));
        }, 30000);
        candidate.timer.unref?.();
    }
    personalTabChanged(tabId, change, tab) {
        const placement = this.personalPlacements.get(tabId);
        const initial = this.personalCandidates.get(tabId);
        const initialMembership = initial?.initial_group === -1 && initial.generation === 1 &&
            change.groupId === initial.inheritance_source?.group_id;
        if (change.groupId !== undefined || change.pinned !== undefined || change.windowId !== undefined) {
            this.presentationRevision++;
        }
        if (placement && (change.groupId !== undefined && change.groupId !== -1 && !initialMembership ||
                change.pinned === true || change.windowId !== undefined ||
                change.url && change.url !== placement.url ||
                change.pendingUrl && change.pendingUrl !== placement.url)) placement.retired = true;
        if (change.groupId !== undefined) {
            for (const candidate of this.personalCandidates.values()) {
                const context = candidate.creation_context;
                if (context && (context.source_tab_id === tabId ||
                        context.prior_active && candidate.window_id === tab?.windowId)) context.invalidated = true;
            }
        }
        if (!this.sourceGroupsHydrated && change.groupId !== undefined) {
            // Before consent loads, delivered group events can only revoke saved provenance.
            if (this.sourceGroupEvents.has(tabId) || this.sourceGroupEvents.size < 128) {
                this.sourceGroupEvents.set(tabId, change.groupId);
            } else this.sourceGroupEventsOverflow = true;
        }
        const pending = this.pendingCreations.get(tabId);
        if (Number.isSafeInteger(change.groupId)) {
            this.presentationGroupChanged({id: change.groupId, windowId: tab?.windowId}, "membership", tabId);
        }
        if (pending?.grouping && change.groupId >= 0 && pending.group_id === null) {
            pending.group_id = change.groupId;
            const staging = [...this.workCreations.values()].find(value => value.tab_id === tabId);
            if (staging) { staging.group_id = change.groupId; staging.stage = "grouped"; }
            this.persistInheritance();
            this.drainPersonalCandidates();
        }
        const records = new Set([...this.records.values(), ...this.pendingCreations.values()]);
        for (const record of records) {
            if (record.tab_id !== tabId || change.groupId === undefined || record.grouping) continue;
            if (record.group_id !== change.groupId) {
                record.group_id = change.groupId;
                record.group_reused = true;
                record.group_generation = (record.group_generation || 0) + 1;
                this.persistConsent().catch(error => this.reportAdvisoryError(error));
                this.persistInheritance();
            }
        }
        if (!pending?.grouping && change.groupId !== undefined) {
            for (const staging of this.workCreations.values()) {
                if (staging.tab_id !== tabId || staging.group_id === change.groupId) continue;
                staging.group_id = change.groupId;
                staging.group_reused = true;
                this.persistInheritance();
            }
        }
        const candidate = this.personalCandidates.get(tabId);
        if (!candidate) return;
        if (change.groupId !== undefined || change.windowId !== undefined) {
            candidate.generation++;
            candidate.group_id = change.groupId ?? candidate.group_id;
            candidate.window_id = change.windowId ?? candidate.window_id;
            if ((candidate.window_id !== tab?.windowId && tab?.windowId !== undefined) ||
                    (change.windowId !== undefined) || change.groupId === -1 ||
                    (candidate.group_id >= 0 && !this.inheritedWorkGroups(candidate).has(candidate.group_id))) {
                return this.forgetPersonalCandidate(tabId, change.groupId === -1 && placement?.isolating === true);
            }
            this.persistInheritance();
        }
        const isolation = this.isolateNewPersonalTab(tabId);
        if (placement) {
            placement.isolation = isolation;
            placement.wake?.();
        }
        return isolation;
    }
    async isolateNewPersonalTab(tabId) {
        const candidate = this.personalCandidates.get(tabId);
        let generation = candidate?.generation;
        const exempt = () => this.pendingCreations.has(tabId) ||
            [...this.records.values(), ...this.workCreations.values()].some(value => value.tab_id === tabId);
        if (!candidate || candidate.browser_boot_id !== this.browserBootId ||
                this.inheritanceBoot !== this.browserBootId || exempt()) return;
        const tab = await this.api(() => this.chrome.tabs.get(tabId), "inspect new personal tab");
        if (candidate.stage === "expired" && this.personalCandidates.get(tabId) === candidate &&
                generation === candidate.generation && candidate.browser_boot_id === this.browserBootId && !exempt()) {
            if (candidate.window_id !== tab.windowId) return this.forgetPersonalCandidate(tabId);
            if (candidate.group_id !== tab.groupId) {
                if (candidate.inheritance_source) return this.forgetPersonalCandidate(tabId);
                candidate.group_id = tab.groupId;
                generation = ++candidate.generation;
                await this.persistInheritance();
            }
        }
        const unresolved = () => {
            if (candidate.stage === "expired") this.reportAdvisoryError(new Error(
                this.personalCandidates.size > 128 ?
                    "Personal inheritance backlog exceeds 128 tabs; unresolved obligations retained" :
                    "Personal inheritance remains unresolved after 30 seconds"));
        };
        const current = () => this.personalCandidates.get(tabId) === candidate &&
            candidate.generation === generation && candidate.browser_boot_id === this.browserBootId &&
            candidate.window_id === tab.windowId && candidate.group_id === tab.groupId && !exempt();
        if (!current()) return;
        const staging = this.workCreations.get(tab.pendingUrl || tab.url);
        if (staging && staging.browser_boot_id === this.browserBootId &&
                staging.stage === "creating" && staging.window_id === tab.windowId) {
            // A matching pending request exempts this tab, but never grants control from a URL.
            unresolved();
            return;
        }
        const pendingUnknown = [...this.workCreations.values()].some(value =>
            value.browser_boot_id === this.browserBootId && value.stage === "creating");
        const initialPlaceholder = !tab.url || tab.url === "about:blank";
        if (initialPlaceholder && !tab.pendingUrl && (this.workCreatesInFlight || pendingUnknown)) {
            unresolved();
            return;
        }
        const source = candidate.inheritance_source;
        if (source && this.inheritedWorkGroups(candidate).has(source.group_id)) {
            const sourceTab = await this.api(() => this.chrome.tabs.get(source.tab_id),
                "verify exact personal inheritance source");
            if (!current()) return;
            if (sourceTab.windowId !== source.window_id || sourceTab.groupId !== source.group_id) {
                return this.forgetPersonalCandidate(tabId);
            }
        }
        if (this.inheritedWorkGroups(candidate).has(tab.groupId)) {
            const placement = this.personalPlacements.get(tabId);
            const members = placement ? await this.api(() => this.chrome.tabs.query({windowId: tab.windowId,
                groupId: tab.groupId}), "inspect personal inheritance movement") : [];
            const isolationEffect = Promise.resolve().then(async () => {
                // Chrome has no conditional ungroup API: event identity fences the last synchronous step.
                if (!current() || !this.inheritedWorkGroups(candidate).has(tab.groupId)) return;
                if (placement) {
                    placement.isolating = true;
                    placement.expectedMove = expectedUngroupMove(tab, members);
                }
                try {
                    await this.chrome.tabs.ungroup([tabId]);
                } finally {
                    if (placement) placement.isolating = false;
                }
            });
            if (placement) placement.isolationEffect = isolationEffect;
            await this.api(() => isolationEffect, "remove new personal tab inheritance");
            if (this.personalCandidates.get(tabId) === candidate) await this.forgetPersonalCandidate(tabId, true);
        } else if (tab.groupId >= 0 || candidate.stage === "expired") {
            await this.forgetPersonalCandidate(tabId);
        }
    }
    async resume(key) {
        const remote = this.selectedRemote(key);
        const record = this.records.get(key);
        if (!record || record.phase === "disabled") fail("browser_disabled", "Enable a new work tab first");
        if (record.principal_id !== remote.principal_id ||
                record.page.workspace_id !== remote.workspace_id ||
                record.owner_display.daemon_instance_id !== remote.owner_display.daemon_instance_id) {
            fail("stale_authority", "Original workbench identity changed; disconnect and enable again");
        }
        const revision = record.revision;
        const draining = [...this.operations.values()].filter(op => this.recordFor(op.payload.remote_key) === record);
        await bounded(() => Promise.all(draining.map(op => op.done.catch(() => {}))),
            "drain previous browser work", 5000);
        await this.drainAttachments(key);
        await this.detach(record, record.pendingDetach);
        await this.validateReservation(record);
        if (this.remotes.get(key) !== remote || this.records.get(key) !== record ||
                record.revision !== revision || !this.manualControls && !this.automationOn) {
            fail("stale_authority", "Remote changed during resume");
        }
        record.connection_id = remote.connection_id;
        record.executor_fence = clone(remote.executor_fence);
        record.owner_display = clone(remote.owner_display);
        record.revision = this.nextRevision(key);
        record.phase = "enabled";
        record.agreed = false;
        record.publication = null;
        const enableRevision = record.revision;
        try {
            await this.persistConsent();
            if (record.revision !== enableRevision || this.remotes.get(key) !== remote) {
                fail("stale_authority", "Authorization changed before publication");
            }
            await this.publish(key);
        } catch (error) {
            if (this.records.get(key) === record && !record.agreed &&
                    record.revision === enableRevision) {
                this.closeAuthorization(record, "paused");
                await this.persistConsent();
            }
            throw error;
        }
    }
    revokeTab(tabId, info = {}) {
        if (!Number.isSafeInteger(tabId)) return Promise.resolve();
        this.removedActiveTabs.add(tabId);
        const affected = new Set([info.windowId, info.oldWindowId].filter(Number.isSafeInteger));
        for (const [windowId, active] of this.activeTabs) {
            if (active.tab_id === tabId) { this.activeTabs.delete(windowId); affected.add(windowId); }
            else if (active.previous_tab_id === tabId) { delete active.previous_tab_id; affected.add(windowId); }
        }
        for (const windowId of affected) this.activationGenerations.set(windowId,
            (this.activationGenerations.get(windowId) || 0) + 1);
        this.persistInheritance();
        for (const [url, value] of this.workCreations) {
            if (value.tab_id === tabId) {
                this.workCreations.delete(url);
                this.persistInheritance();
            }
        }
        const pending = this.pendingCreations.get(tabId);
        const record = [...this.records.values()].find(record =>
            record.tab_id === tabId && record.phase !== "disabled");
        if (this.controls.size || !this.restored) this.revokedTabs.add(tabId);
        if (pending) this.bumpControl(pending.remote_key);
        if (!record) return Promise.resolve();
        const attachment = record.attachment;
        this.closeAuthorization(record, "disabled");
        return (async () => {
            await this.persistConsent();
            await this.detach(record, attachment);
            if (this.remotes.get(record.remote_key)?.connected) await this.publish(record.remote_key);
            if (!this.manualControls) await this.reconcile(record.remote_key);
        })();
    }
    debuggerDetached(tabId) {
        const token = this.attachments.get(tabId);
        if (token && !token.pending) {
            token.detached = true;
            if (!token.detaching) this.releaseAttachment(token);
        }
        const record = [...this.records.values()].find(record => record.tab_id === tabId);
        this.closeEngine(record);
        if (record) record.attached = false;
        for (const op of this.operations.values()) if (op.tab_id === tabId) op.cancelled = true;
    }
    effectKey(payload) { return `${payload.remote_key}\0${payload.principal_id}\0${payload.effect_id}`; }
    validateExecute(payload) {
        if (!exactFields(payload, ["remote_key", "connection_id", "principal_id", "request_id",
            "effect_id", "executor_fence", "authorization", "command"]) ||
                ![payload.remote_key, payload.connection_id, payload.principal_id,
                    payload.request_id, payload.effect_id].every(text) || !validFence(payload.executor_fence) ||
                !exactFields(payload.authorization, ["browser_boot_id", "enable_id", "revision"]) ||
                !text(payload.authorization.browser_boot_id) || !text(payload.authorization.enable_id) ||
                !Number.isSafeInteger(payload.authorization.revision) || payload.authorization.revision <= 0 ||
                !validPageRef(payload.command?.page)) fail("invalid_command", "Invalid v10 execute event");
        if (payload.command.command === "action") {
            if (!exactFields(payload.command, ["command", "page", "action"]) ||
                    !validEngineAction(payload.command.action)) fail("invalid_command", "Invalid engine action");
            return;
        }
        if (!exactFields(payload.command, ["command", "page", "url"]) ||
                payload.command.command !== "navigate" || !text(payload.command.url)) {
            fail("invalid_command", "Invalid navigation command");
        }
        let url;
        try { url = new URL(payload.command.url); }
        catch (_) { fail("invalid_command", "Invalid navigation URL"); }
        if (!["http:", "https:"].includes(url.protocol) || url.username || url.password ||
                !payload.command.url.startsWith(`${url.protocol}//`) ||
                /\s|[\x00-\x1f]/.test(payload.command.url) ||
                new TextEncoder().encode(payload.command.url).length > 32768) {
            fail("invalid_command", "Navigate requires an exact HTTP(S) URL without credentials");
        }
    }
    authorization(payload, op) {
        const record = this.recordFor(payload.remote_key);
        const remote = this.remotes.get(payload.remote_key);
        // Exact Execute is daemon acceptance proof after persisted consent starts publication.
        const published = record?.publication && record.publication.remote === remote &&
            sameRef(record.publication.state, this.state(payload.remote_key));
        if ((!this.manualControls && (!this.automationOn || !this.preferenceReady)) || op.cancelled || !record || record.phase !== "enabled" || (!record.agreed && !published)) {
            fail("browser_disabled", "Browser is disabled or paused; use the Chrome popup");
        }
        if (!remote?.connected || remote.connection_id !== payload.connection_id ||
                !this.currentExecutor(remote) ||
                remote.principal_id !== payload.principal_id || record.principal_id !== payload.principal_id ||
                !sameRef(remote.executor_fence, payload.executor_fence) ||
                !sameRef(record.executor_fence, payload.executor_fence) ||
                !sameRef(payload.authorization, {browser_boot_id: this.browserBootId,
                    enable_id: record.enable_id, revision: record.revision}) ||
                !sameRef(record.page, payload.command.page)) fail("stale_authority", "Stale browser authorization");
        return record;
    }
    async execute(payload) {
        this.validateExecute(payload);
        const key = this.effectKey(payload);
        const known = this.effects[key];
        if (known) {
            const {connection_id, request_id, ...identity} = payload;
            const {connection_id: oldConnection, request_id: oldRequest, ...oldIdentity} = known.payload;
            if (!sameRef(identity, oldIdentity)) fail("operation_conflict", "Conflicting duplicate effect");
            const running = this.operations.get(key);
            if (running) return running.done;
            return this.deliverEffect(key);
        }
        const queued = [...this.operations.values()].filter(operation =>
            operation.payload.remote_key === payload.remote_key);
        const bytes = value => new TextEncoder().encode(JSON.stringify(value)).length;
        if (queued.length >= 32 || bytes(payload) > 1024 * 1024 ||
                queued.reduce((total, operation) => total + bytes(operation.payload), 0) +
                    bytes(payload) > 4 * 1024 * 1024) {
            fail("backpressure", "Execution admission exhausted (32 / 4 MiB per remote)");
        }
        const op = {payload: clone(payload), cancelled: false, dispatched: false, tab_id: null};
        this.operations.set(key, op);
        const entry = {version: 10, payload: clone(payload), phase: "accepted", completion: null,
            result: null, error: null};
        this.effects[key] = entry;
        const reservationKey = JSON.stringify([payload.authorization.browser_boot_id,
            payload.authorization.enable_id]);
        const tail = this.executionTails.get(reservationKey) || Promise.resolve();
        op.done = tail.catch(() => {}).then(() => this.performExecute(key, op, entry));
        this.executionTails.set(reservationKey, op.done);
        op.done.catch(() => {});
        return op.done;
    }
    closeEngine(record) {
        const session = this.engineSessions.get(record);
        if (!session) return;
        this.engineSessions.delete(record);
        session.closed = true;
        session.bridge.close();
        session.reject(new Error("Engine lease revoked"));
        if (session.lease_id) this.nativeRequest("abush_browser", "engine_close",
            {...clone(session.op.payload), lease_id: session.lease_id})
            .catch(error => this.reportAdvisoryError(error));
    }
    engineLease(payload) {
        const origin = Object.fromEntries(Object.entries(payload).filter(([key]) => key !== "lease_id"));
        const record = this.recordFor(payload.remote_key);
        const session = this.engineSessions.get(record);
        if (!session || session.closed || !sameRef(session.binding, engineBinding(origin))) {
            if (text(payload.lease_id)) this.nativeRequest("abush_browser", "engine_close", clone(payload))
                .catch(error => this.reportAdvisoryError(error));
            return false;
        }
        this.authorization(session.op.payload, session.op);
        if (!text(payload.lease_id) || session.lease_id && session.lease_id !== payload.lease_id) {
            throw new Error("Conflicting engine lease reservation");
        }
        session.lease_id = payload.lease_id;
        return true;
    }
    engineCdp(payload) {
        const session = [...this.engineSessions.values()].find(candidate =>
            sameRef(candidate.binding, engineBinding(payload)));
        if (!session || session.closed || !text(payload.lease_id) ||
                session.lease_id && session.lease_id !== payload.lease_id) return false;
        this.authorization(session.op.payload, session.op);
        session.lease_id = payload.lease_id;
        session.bridge.handle(payload.message).catch(error => {
            this.reportAdvisoryError(error);
            this.closeEngine(this.recordFor(session.op.payload.remote_key));
        });
        return true;
    }
    async executeEngine(record, op) {
        let session = this.engineSessions.get(record);
        if (session && !sameRef(session.binding, engineBinding(op.payload))) {
            this.closeEngine(record);
            session = null;
        }
        op.engineActive = true;
        if (!session) {
            let reject;
            const stopped = new Promise((_, rejectPromise) => { reject = rejectPromise; });
            stopped.catch(() => {});
            session = {op, binding: clone(engineBinding(op.payload)),
                closed: false, lease_id: null, stopped, reject};
            const check = () => {
                if (session.closed || this.engineSessions.get(record) !== session) throw new Error("Stale engine lease");
                this.authorization(session.op.payload, session.op);
            };
            const chromeApi = Object.create(this.chrome);
            chromeApi.debugger = Object.create(this.chrome.debugger);
            chromeApi.debugger.sendCommand = (source, method, params) => {
                check();
                if (source.tabId !== record.tab_id) throw new Error("Foreign CDP tab");
                const current = session.op;
                if (!IDLE_ENGINE_COMMANDS.has(method) && (!current.engineActive ||
                        this.operations.get(this.effectKey(current.payload)) !== current ||
                        this.effects[this.effectKey(current.payload)]?.phase !== "dispatched")) {
                    throw new Error("CDP page command requires a current accepted operation");
                }
                if (/^(Page\.(navigate|reload|navigateToHistoryEntry|handleJavaScriptDialog)|Input\.|DOM\.(focus|scrollIntoViewIfNeeded)|Runtime\.(evaluate|callFunctionOn))/.test(method)) {
                    current.dispatched = true;
                }
                return this.chrome.debugger.sendCommand(source, method, params);
            };
            const Bridge = globalThis.AuthorizedCdpBridge ||
                (typeof require === "function" && require("./playwright-transport.js").AuthorizedCdpBridge);
            session.bridge = new Bridge({chrome: chromeApi, tabId: record.tab_id,
                checkAuthorization: check,
                send: message => {
                    check();
                    if (!session.lease_id) throw new Error("Engine CDP has no lease");
                    return this.nativeRequest("abush_browser", "engine_cdp",
                        {...clone(session.op.payload), lease_id: session.lease_id, message});
                }, onError: error => { this.reportAdvisoryError(error); this.closeEngine(record); }});
            this.engineSessions.set(record, session);
            const opening = this.nativeRequest("abush_browser", "engine_open", clone(op.payload))
                .then(reply => {
                    if (!text(reply?.lease_id)) throw new Error("Invalid engine lease");
                    if (session.closed) {
                        this.nativeRequest("abush_browser", "engine_close",
                            {...clone(op.payload), lease_id: reply.lease_id}).catch(error => this.reportAdvisoryError(error));
                        throw new Error("Engine opening revoked");
                    }
                    if (session.lease_id && session.lease_id !== reply.lease_id) throw new Error("Conflicting engine lease");
                    session.lease_id = reply.lease_id;
                    check();
                });
            await bounded(() => Promise.race([opening, session.stopped]), "initialize official engine", 35000);
        }
        session.op = op;
        this.authorization(op.payload, op);
        try {
            return await bounded(() => Promise.race([session.stopped,
                this.nativeRequest("abush_browser", "engine_execute", {...clone(op.payload), lease_id: session.lease_id})]),
            "execute official engine", 35000);
        } finally {
            op.engineActive = false;
        }
    }
    async performExecute(key, op, entry) {
        let error;
        try {
            await this.persistWal();
            const record = this.authorization(op.payload, op);
            await this.materialize(record, op);
            this.authorization(op.payload, op);
            op.tab_id = record.tab_id;
            await this.validateTab(record);
            this.authorization(op.payload, op);
            if (!record.attached) {
                if (this.attachments.has(record.tab_id)) {
                    fail("operation_conflict", "Debugger attachment or cleanup is still pending");
                }
                const token = {record, pending: true, cleanup: false, detaching: null};
                this.attachments.set(record.tab_id, token);
                op.attachment = token;
                const completion = Promise.resolve().then(() => {
                    this.authorization(op.payload, op);
                    return this.chrome.debugger.attach({tabId: record.tab_id}, "1.3");
                }).then(() => {
                    token.pending = false;
                    let authorized = false;
                    try { authorized = this.authorization(op.payload, op) === record; }
                    catch (_) {}
                    if (token.cleanup || !authorized || this.recordFor(op.payload.remote_key) !== record) {
                        token.cleanup = true;
                        // The map owns this continuation even after the bounded worker exits.
                        this.detach(record, token).catch(error => this.reportAdvisoryError(error));
                        fail("browser_disabled", "Attachment authorization was revoked");
                    }
                    record.attachment = token;
                    record.attached = true;
                }, error => {
                    token.pending = false;
                    this.releaseAttachment(token);
                    throw error;
                });
                try {
                    await this.api(() => completion, "attach exact work tab debugger");
                } catch (error) {
                    if (this.attachments.get(record.tab_id) === token) token.cleanup = true;
                    throw error;
                }
            } else {
                op.attachment = record.attachment;
            }
            this.authorization(op.payload, op);
            entry.phase = "dispatched";
            await this.persistWal();
            this.authorization(op.payload, op);
            const result = await this.executeEngine(record, op);
            this.authorization(op.payload, op);
            if (result?.ok !== true || !validEngineResult(result.result) || result.error !== null) {
                throw new Error(result?.error || "Invalid engine output");
            }
            entry.result = clone(result.result);
            entry.error = null;
            entry.completion = "applied";
        } catch (failure) {
            error = failure;
            op.cancelled = true;
            entry.completion = op.dispatched ? "unknown" : "not_applied";
            entry.result = null;
            entry.error = failure.message || "Engine operation failed";
            this.closeEngine(this.recordFor(op.payload.remote_key));
            this.reportAdvisoryError(failure);
        }
        entry.phase = "terminal";
        let cleanupError;
        try {
            if (op.cancelled && op.attachment) {
                try {
                    await this.detach(op.attachment.record, op.attachment);
                } catch (failure) {
                    cleanupError = failure;
                }
            }
            await this.persistWal();
            await this.deliverEffect(key);
        } finally {
            this.operations.delete(key);
        }
        if (cleanupError) throw cleanupError;
        if (error) throw error;
        return entry.completion;
    }
    async deliverEffect(key) {
        const entry = this.effects[key];
        if (!entry?.completion) fail("unknown_effect", "Effect has no durable terminal result");
        await this.persistWal();
        const origin = entry.payload;
        const remote = this.remotes.get(origin.remote_key);
        const payload = {remote_key: origin.remote_key,
            connection_id: remote?.connected && remote.principal_id === origin.principal_id ?
                remote.connection_id : origin.connection_id,
            principal_id: origin.principal_id, request_id: origin.request_id,
            effect_id: origin.effect_id, executor_fence: origin.executor_fence,
            completion: entry.completion, result: entry.result, error: entry.error};
        if (this.receipts[key]) return entry.completion;
        const receipt = await this.native("abush_browser", "executor_completion", payload);
        const expected = {type: "completion_accepted", request_id: payload.request_id,
            effect_id: payload.effect_id, executor_fence: payload.executor_fence,
            completion: payload.completion, result: payload.result, error: payload.error};
        if (!sameRef(receipt, expected)) fail("stale_authority", "Invalid completion receipt; result remains pending");
        this.receipts[key] = receipt;
        try {
            await this.api(() => this.chrome.storage.local.set({[RECEIPTS_KEY]: {
                version: 10, receipts: clone(this.receipts),
            }}), "persist completion receipt");
        } catch (error) { delete this.receipts[key]; throw error; }
        return entry.completion;
    }
    async flushEffects(remoteKey) {
        for (const [key, entry] of Object.entries(this.effects)) {
            if (entry.payload.remote_key === remoteKey && entry.completion && !this.operations.has(key)) {
                await this.deliverEffect(key);
            }
        }
        for (const [key, entry] of Object.entries(this.cancellations)) {
            if (entry.payload.remote_key === remoteKey && entry.completion) await this.deliverCancellation(key);
        }
    }
    async deliverCancellation(key) {
        const entry = this.cancellations[key];
        if (!entry.completion) fail("unknown_effect", "Cancellation is still draining");
        if (entry.receipt) return entry.completion;
        await this.persistWal();
        const origin = entry.payload;
        const remote = this.remotes.get(origin.remote_key);
        const wire = {remote_key: origin.remote_key,
            connection_id: remote?.connected && remote.principal_id === origin.principal_id ?
                remote.connection_id : origin.connection_id,
            principal_id: origin.principal_id, request_id: origin.request_id,
            effect_id: origin.effect_id, executor_fence: origin.executor_fence,
            completion: entry.completion};
        const receipt = await this.native("abush_browser", "cancellation_completion", wire);
        if (!sameRef(receipt, {type: "cancellation_completion_accepted", request_id: wire.request_id,
            effect_id: wire.effect_id, executor_fence: wire.executor_fence, completion: wire.completion})) {
            fail("stale_authority", "Invalid cancellation receipt");
        }
        entry.receipt = receipt;
        try { await this.persistWal(); }
        catch (error) { delete entry.receipt; throw error; }
        return entry.completion;
    }
    async cancel(payload) {
        const fields = ["remote_key", "connection_id", "principal_id", "request_id", "effect_id",
            "executor_fence"];
        if (payload?.execution_request_id !== undefined) fields.push("execution_request_id");
        if (!exactFields(payload, fields) || !validFence(payload.executor_fence) ||
                !fields.filter(key => key !== "executor_fence").every(key => text(payload[key]))) {
            fail("invalid_command", "Invalid cancellation event");
        }
        const key = this.effectKey(payload);
        const remote = this.remotes.get(payload.remote_key);
        if (!remote?.connected || remote.principal_id !== payload.principal_id ||
                remote.connection_id !== payload.connection_id ||
                !sameRef(remote.executor_fence, payload.executor_fence) ||
                !this.currentExecutor(remote)) {
            fail("stale_authority", "Cancellation authority is stale");
        }
        const cancellationKey = `${key}\0${payload.request_id}`;
        const previous = this.cancellations[cancellationKey];
        if (previous) {
            const {connection_id, ...identity} = payload;
            const {connection_id: oldConnection, ...oldIdentity} = previous.payload;
            if (!sameRef(identity, oldIdentity)) fail("operation_conflict", "Conflicting cancellation retry");
            return this.deliverCancellation(cancellationKey);
        }
        const entry = this.effects[key];
        if (!entry || !sameRef(entry.payload.executor_fence, payload.executor_fence) ||
                (payload.execution_request_id && entry.payload.request_id !== payload.execution_request_id)) {
            fail("unknown_effect", "Cancellation does not match an execution");
        }
        const op = this.operations.get(key);
        const cancellation = {payload: clone(payload), completion: null};
        this.cancellations[cancellationKey] = cancellation;
        if (op) { op.cancelled = true; this.closeEngine(this.recordFor(payload.remote_key)); }
        await this.persistWal();
        if (op) {
            const record = this.recordFor(payload.remote_key);
            if (record) await this.detach(record);
            await bounded(() => op.done.catch(() => {}), "cancel worker exit", 5000);
        }
        const completion = entry.completion === "not_applied" ? "not_applied" : "unknown";
        cancellation.completion = completion;
        await this.persistWal();
        return this.deliverCancellation(cancellationKey);
    }

    validateOrdinaryOpen(payload) {
        if (!exactFields(payload, ["version", "operation", "request_id", "url", "focus", "deadline_ms"]) ||
                payload.version !== 1 || payload.operation !== "ordinary_open" || payload.focus !== true ||
                !text(payload.request_id) || !text(payload.url) || !Number.isSafeInteger(payload.deadline_ms)) {
            fail("invalid_command", "Invalid ordinary-open event");
        }
        const url = new URL(payload.url);
        if (payload.request_id.length > 128 || new TextEncoder().encode(payload.url).length > 32768 ||
                payload.deadline_ms <= 0) fail("invalid_command", "Invalid ordinary request size or deadline");
        if (!["http:", "https:", "file:"].includes(url.protocol) ||
                (url.protocol === "file:" && (!payload.url.startsWith("file:///") || url.host))) {
            fail("invalid_command", "Invalid ordinary URL");
        }
        return payload.url;
    }
    cancelOrdinaryOpen(payload) {
        if (payload?.version !== 1 || payload.operation !== "ordinary_open_cancel" || !text(payload.request_id)) {
            fail("invalid_command", "Invalid ordinary cancellation");
        }
        this.ordinaryCancellations[payload.request_id] = true;
        if (this.restored) this.persistWal().catch(error => this.reportAdvisoryError(error));
        const op = this.ordinaryOperations.get(payload.request_id);
        if (op) {
            op.cancelled = true;
            op.wake?.();
        }
        return true;
    }
    observeOrdinaryTab(url, initialNavigation = false) {
        let target = null;
        let initialGroup = null;
        let isolated = false;
        let ungrouped = false;
        let navigating = false;
        let destinationObserved = false;
        let expectedMove = null;
        let invalid = false;
        const early = [];
        const sameUrl = value => {
            try { return new URL(value).href === new URL(url).href; }
            catch (_) { return false; }
        };
        const placeholder = value => !value || value === "about:blank" || sameUrl(value);
        const matchesDocument = tab => placeholder(tab?.url) && (!tab?.pendingUrl || sameUrl(tab.pendingUrl));
        const observeDocument = tab => {
            const destination = sameUrl(tab?.pendingUrl || tab?.url);
            if (!matchesDocument(tab) || destinationObserved && !destination) invalid = true;
            destinationObserved ||= destination;
        };
        const observe = (kind, args) => {
            if (!target) {
                if (early.length >= 128) invalid = true;
                else early.push([kind, args]);
                return;
            }
            const [id, change, tab] = args;
            if (kind === "activated") {
                if (id.windowId === target.windowId && id.tabId !== target.id) invalid = true;
            } else if (kind === "created") {
                if (id.id === target.id) {
                    initialGroup = id.groupId;
                    if (initialNavigation) observeDocument(id);
                }
            } else if (kind === "replaced") {
                if (change === target.id) invalid = true;
            } else if (id === target.id && kind === "moved") {
                if (expectedMove && sameRef(expectedMove, change)) expectedMove = null;
                else invalid = true;
            } else if (id === target.id) {
                if (kind === "updated" && initialNavigation) observeDocument(tab);
                if (kind !== "updated" || tab?.windowId !== target.windowId ||
                        change.pinned !== undefined ||
                        change.groupId !== undefined && change.groupId !== -1 &&
                            (isolated || ungrouped || change.groupId !== initialGroup) ||
                        !navigating && (change.url !== undefined && !placeholder(change.url) ||
                            change.pendingUrl && !sameUrl(change.pendingUrl))) invalid = true;
                if (kind === "updated" && change.groupId === -1) ungrouped = true;
            }
        };
        const listeners = ["onCreated", "onUpdated", "onRemoved", "onMoved", "onDetached", "onAttached", "onActivated", "onReplaced"]
            .map(event => [this.chrome.tabs[event], (...args) => observe(event.slice(2).toLowerCase(), args)]);
        for (const [event, listener] of listeners) event.addListener(listener);
        return {
            bind: tab => {
                target = {id: tab.id, windowId: tab.windowId};
                initialGroup = tab.groupId;
                for (const [kind, args] of early) observe(kind, args);
                if (initialNavigation && sameUrl(tab.pendingUrl || tab.url)) destinationObserved = true;
                early.length = 0;
            },
            check: () => { if (invalid) throw new Error("Ordinary tab changed during preparation"); },
            validate: tab => {
                if (initialNavigation) observeDocument(tab);
                if (invalid || tab.id !== target.id || tab.windowId !== target.windowId || tab.pinned ||
                        !matchesDocument(tab) || tab.groupId !== -1 && tab.groupId !== initialGroup) {
                    throw new Error("Created ordinary tab no longer matches its staging request");
                }
            },
            move: async (task, expected) => {
                expectedMove = expected;
                return task();
            },
            isolate: () => { isolated = true; },
            navigate: () => { navigating = true; },
            close: () => { for (const [event, listener] of listeners) event.removeListener(listener); },
        };
    }
    async waitOrdinaryStaging(payload, op, tab, url, nonce, api, check) {
        let revision = 0;
        let stopped = false;
        let invalid = false;
        let signal = null;
        const wake = () => { revision++; signal?.(); };
        const updated = id => { if (id === tab.id) wake(); };
        const removed = id => { if (id === tab.id) { invalid = true; wake(); } };
        const activated = info => {
            if (info.windowId === tab.windowId && info.tabId !== tab.id) {
                invalid = true;
                wake();
            }
        };
        const listeners = [[this.chrome.tabs.onUpdated, updated], [this.chrome.tabs.onRemoved, removed],
            [this.chrome.tabs.onDetached, removed], [this.chrome.tabs.onActivated, activated]];
        for (const [event, listener] of listeners) event.addListener(listener);
        op.wake = wake;
        const current = () => {
            check();
            if (stopped || invalid) throw new Error("Ordinary staging target changed while loading");
        };
        try {
            return await bounded(async () => {
                while (true) {
                    current();
                    const observed = revision;
                    const changed = new Promise(resolve => { signal = resolve; });
                    const staging = await api(() => this.chrome.tabs.get(tab.id), "observe ordinary staging document");
                    current();
                    if (observed !== revision) continue;
                    if (staging.id !== tab.id || staging.windowId !== tab.windowId ||
                            this.ownsTab(tab.id) || staging.groupId !== -1 || !staging.active ||
                            staging.pendingUrl && staging.pendingUrl !== url ||
                            staging.url && !["about:blank", url].includes(staging.url)) {
                        throw new Error("Ordinary staging target is not exact, active, and ungrouped");
                    }
                    if (staging.url === url && staging.title === `${ORDINARY_PREFIX}${nonce}`) return staging;
                    await changed;
                }
            }, "ordinary staging document readiness", Math.max(1, payload.deadline_ms - Date.now()));
        } finally {
            stopped = true;
            wake();
            if (op.wake === wake) delete op.wake;
            for (const [event, listener] of listeners) event.removeListener(listener);
        }
    }
    async ordinaryCompletion(payload, completion) {
        await this.native("ordinary_open", "ordinary_open_completion", {version: 1,
            operation: "ordinary_open_completed", request_id: payload.request_id, completion});
        return completion;
    }
    async ordinaryOpen(payload) {
        this.validateOrdinaryOpen(payload);
        const old = this.ordinary[payload.request_id];
        if (old) {
            if (!sameRef(old.payload, payload)) fail("operation_conflict", "Conflicting ordinary request");
            const running = this.ordinaryOperations.get(payload.request_id);
            if (running) return running.done;
            return this.ordinaryCompletion(payload, old.completion || "unknown");
        }
        if (this.ordinaryOperations.size >= 32) {
            fail("backpressure", "Ordinary execution admission exhausted (32)");
        }
        const op = {cancelled: this.ordinaryCancellations[payload.request_id] === true, dispatched: false,
            browser_boot_id: this.browserBootId, transport_generation: this.transportGeneration};
        const entry = {version: 9, payload: clone(payload), phase: "accepted", completion: null};
        this.ordinary[payload.request_id] = entry;
        this.ordinaryOperations.set(payload.request_id, op);
        op.done = this.ordinaryTail.catch(() => {}).then(() => this.performOrdinary(payload, entry, op))
            .finally(() => this.ordinaryOperations.delete(payload.request_id));
        this.ordinaryTail = op.done;
        return op.done;
    }
    async ordinaryPlacement(windowId, api, check, excludedId = null) {
        for (let attempt = 0; attempt < 3; attempt++) {
            check();
            const revision = this.presentationRevision;
            const titles = new Set([...this.remotes.values()].filter(remote => remote.connected)
                .map(remote => displayTitle(remote.display_name || remote.connection_display_name)));
            const groups = await api(() => this.chrome.tabGroups.query({windowId}), "read ordinary presentation boundaries");
            check();
            const displayGroups = new Set(groups.filter(group => group.windowId === windowId && titles.has(group.title))
                .map(group => group.id));
            const tabs = await api(() => this.chrome.tabs.query({windowId}), "read ordinary insertion indices");
            check();
            if (revision !== this.presentationRevision) continue;
            const members = tabs.filter(tab => tab.id !== excludedId && displayGroups.has(tab.groupId));
            const tab = tabs.find(tab => tab.id === excludedId);
            if (!members.length) return {index: RIGHT_EDGE_TAB_INDEX, tab, revision};
            if (tabs.some(tab => !Number.isSafeInteger(tab.index) || tab.index < 0)) {
                throw new Error("Ordinary placement requires current tab-strip indices");
            }
            const pinnedEnd = Math.max(0, ...tabs.filter(tab => tab.pinned).map(tab => tab.index + 1));
            return {index: Math.max(pinnedEnd, Math.min(...members.map(tab => tab.index))), tab, revision};
        }
        throw new Error("Ordinary presentation changed during placement observation");
    }
    async performOrdinary(payload, entry, op) {
        let observation;
        const boot = op.browser_boot_id;
        const transport = op.transport_generation;
        const check = () => {
            observation?.check();
            if (boot !== this.browserBootId || transport !== this.transportGeneration ||
                    op.cancelled || Date.now() >= payload.deadline_ms) {
                fail("executor_unavailable", "Ordinary open cancelled or deadline expired");
            }
        };
        const api = (task, label) => {
            check();
            return bounded(() => { check(); return task(); }, label,
                Math.max(1, Math.min(API_TIMEOUT_MS, payload.deadline_ms - Date.now())));
        };
        const native = (operation, wire) => {
            check();
            return bounded(() => { check(); return this.nativeRequest("ordinary_open", operation, clone(wire)); },
                `ordinary_open.${operation}`, Math.max(1, Math.min(8000, payload.deadline_ms - Date.now())));
        };
        try {
            await this.persistWal();
            check();
            const windows = await api(() => this.chrome.windows.getAll({windowTypes: ["normal"]}),
                "select ordinary window");
            check();
            const window = windows.filter(item => item.type === "normal")
                .sort((a, b) => Number(b.focused) - Number(a.focused))[0];
            const file = new URL(payload.url).protocol === "file:";
            const nonce = file ? crypto.randomUUID().replaceAll("-", "") : null;
            const url = file ? this.chrome.runtime.getURL(`ordinary-staging.html#${nonce}`) : payload.url;
            entry.phase = "dispatched";
            await this.persistWal();
            check();
            const preparation = this.presentationTail.catch(() => {}).then(async () => {
                check();
                const placement = window ? await this.ordinaryPlacement(window.id, api, check) : null;
                check();
                if (placement && placement.revision !== this.presentationRevision) {
                    throw new Error("Ordinary presentation changed before creation");
                }
                const index = placement?.index;
                observation = this.observeOrdinaryTab(url, !file);
                let tab;
                if (window) {
                    op.dispatched = true;
                    tab = await this.chrome.tabs.create({windowId: window.id, url, active: false, index});
                } else {
                    op.dispatched = true;
                    const created = await this.chrome.windows.create({url, type: "normal", focused: true});
                    tab = created.tabs?.[0];
                }
                if (!Number.isSafeInteger(tab?.id) || !Number.isSafeInteger(tab.windowId) ||
                        window && tab.windowId !== window.id || this.ownsTab(tab.id)) {
                    throw new Error("No exact ordinary tab");
                }
                observation.bind(tab);
                check();
                const created = await this.chrome.tabs.get(tab.id);
                observation.validate(created);
                check();
                const members = created.groupId >= 0 ? await this.chrome.tabs.query({windowId: tab.windowId,
                    groupId: created.groupId}) : [];
                check();
                await observation.move(() => this.chrome.tabs.ungroup([tab.id]), expectedUngroupMove(created, members));
                observation.isolate();
                check();
                const {index: indexAfterIsolation, tab: isolated, revision} =
                    await this.ordinaryPlacement(tab.windowId, api, check, tab.id);
                check();
                if (revision !== this.presentationRevision) throw new Error("Ordinary isolation placement changed");
                if (!isolated || isolated.id !== tab.id || isolated.windowId !== tab.windowId ||
                        isolated.groupId !== -1 || this.ownsTab(tab.id)) {
                    throw new Error("Ordinary tab must be isolated before activation");
                }
                if (indexAfterIsolation !== RIGHT_EDGE_TAB_INDEX && isolated.index > indexAfterIsolation) {
                    await observation.move(() => this.chrome.tabs.move(tab.id, {index: indexAfterIsolation}),
                        {windowId: tab.windowId, fromIndex: isolated.index, toIndex: indexAfterIsolation});
                    check();
                }
                return isolated;
            });
            this.presentationTail = preparation;
            let preparedTab;
            try {
                preparedTab = await api(() => preparation, "prepare ordinary tab presentation");
            } catch (error) {
                op.cancelled = true;
                throw error;
            }
            check();
            const tab = preparedTab;
            let prepared;
            const requested = new URL(payload.url).href;
            if (file) {
                if (!tab.active) await api(() => this.chrome.tabs.update(tab.id, {active: true}),
                    "activate exact isolated ordinary staging tab");
                await this.waitOrdinaryStaging(payload, op, tab, url, nonce, api, check);
                prepared = await native("prepare", {nonce, deadline_ms: payload.deadline_ms});
                if (!exactFields(prepared, ["nonce", "xid"]) || prepared.nonce !== nonce ||
                        !Number.isSafeInteger(prepared.xid) || prepared.xid <= 0) {
                    throw new Error("Invalid ordinary prepare receipt");
                }
                check();
                await this.waitOrdinaryStaging(payload, op, tab, url, nonce, api, check);
            } else {
                const current = await api(() => this.chrome.tabs.get(tab.id), "verify exact ordinary tab before activation");
                check();
                observation.validate(current);
                if (current.groupId !== -1 || this.ownsTab(tab.id) ||
                        !current.pendingUrl && !current.url ||
                        new URL(current.pendingUrl || current.url).href !== requested) {
                    throw new Error("Ordinary loading tab no longer has its pending or committed destination");
                }
            }
            check();
            if (file) {
                observation.navigate();
                const wire = {...prepared, url: payload.url, deadline_ms: payload.deadline_ms};
                const receipt = await native("ordinary_file_navigate", wire);
                if (!sameRef(receipt, {...wire, completion: "applied"})) throw new Error("File navigation is not confirmed");
                check();
                const activated = await native("activate", {...prepared, deadline_ms: payload.deadline_ms});
                if (!sameRef(activated, prepared)) throw new Error("Invalid ordinary activation receipt");
            } else if (!tab.active) {
                await api(() => this.chrome.tabs.update(tab.id, {active: true}), "activate exact ordinary loading tab");
            }
            const result = await api(() => this.chrome.tabs.get(tab.id), "verify ordinary result");
            const destination = new URL(result.pendingUrl || result.url);
            if (destination.href !== requested || result.id !== tab.id ||
                    result.windowId !== tab.windowId || this.ownsTab(tab.id) ||
                    result.groupId !== -1 || result.pinned || !result.active) {
                throw new Error("Ordinary tab result is not exact, active, and ungrouped");
            }
            check();
            entry.completion = "applied";
        } catch (error) {
            op.cancelled = true;
            entry.completion = op.dispatched ? "unknown" : "not_applied";
            entry.error = error.message || String(error);
            this.reportAdvisoryError(new Error(`Ordinary open ${payload.request_id}: ${entry.error}`));
        } finally {
            observation?.close();
        }
        entry.phase = "terminal";
        await this.persistWal();
        return this.ordinaryCompletion(payload, entry.completion);
    }
}
if (typeof module !== "undefined") module.exports = {WorkspaceExecutor, BROWSER_ERROR_CODES,
    ORDINARY_PREFIX, AUTOMATION_KEY, CONSENT_KEY, WAL_KEY, RECEIPTS_KEY, displayTitle, bounded};
