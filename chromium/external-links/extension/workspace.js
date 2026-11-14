const ORDINARY_PREFIX = "__ABUSH_ORDINARY__:";
const CONSENT_KEY = "abushBrowserConsentV9";
const WAL_KEY = "abushBrowserEffectsV9";
const RECEIPTS_KEY = "abushBrowserReceiptsV9";
const API_TIMEOUT_MS = 2000;
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

class WorkspaceExecutor {
    constructor(chromeApi, nativeRequest) {
        this.chrome = chromeApi;
        this.nativeRequest = nativeRequest;
        this.cancellations = {};
        this.controlTokens = new Map();
        this.restoreGeneration = 0;
        this.restoreRevoked = false;
        this.transportGeneration = 0;
        this.ownershipGeneration = 1;
        this.pendingCreations = new Map();
        this.ordinaryTail = Promise.resolve();
        this.browserBootId = null;
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
        this.controls = new Set();
        this.revokedTabs = new Set();
        this.localTail = Promise.resolve();
        this.sessionTail = Promise.resolve();
        this.restoreAttempt = null;
        this.restored = false;
        this.lastError = null;
        this.bootstrapLeases = [];
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
            this.revisions.clear();
            this.remotes.clear();
            this.restoreAttempt = null;
            this.restored = false;
        }
        this.browserBootId = payload.browser_boot_id;
        this.bootstrapLeases = clone(payload.ordinary_launch_leases);
        if (this.restored) {
            for (const lease of this.bootstrapLeases) await this.ordinaryOpen(lease);
            this.bootstrapLeases = [];
        }
    }
    restoreWhenReady() {
        this.restoreAttempt ||= this.restore();
        return this.restoreAttempt;
    }
    async restore() {
        if (!this.browserBootId) fail("executor_unavailable", "Native browser boot is unavailable");
        const boot = this.browserBootId;
        const generation = this.restoreGeneration;
        const local = await this.api(() => this.chrome.storage.local.get([WAL_KEY, RECEIPTS_KEY]),
            "read effect journal");
        if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
        const wal = local[WAL_KEY];
        if (wal?.version === 9) {
            this.cancellations = {...(wal.cancellations || {}), ...this.cancellations};
            this.effects = wal.effects || {};
            this.creations = wal.creations || {};
            this.ordinary = wal.ordinary || {};
            for (const entry of Object.values(this.creations)) {
                if (entry.phase !== "completed") entry.phase = "unknown";
            }
            for (const entry of [...Object.values(this.effects), ...Object.values(this.ordinary)]) {
                if (!entry.completion) entry.completion =
                    entry.phase === "accepted" ? "not_applied" : "unknown";
            }
            for (const entry of Object.values(this.cancellations)) {
                if (!entry.completion) {
                    const effect = this.effects[this.effectKey(entry.payload)];
                    entry.completion = effect?.completion === "not_applied" ? "not_applied" : "unknown";
                }
            }
        }
        this.receipts = local[RECEIPTS_KEY]?.version === 9 ? local[RECEIPTS_KEY].receipts : {};
        const session = await this.api(() => this.chrome.storage.session.get(CONSENT_KEY),
            "read browser consent");
        if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
        const saved = session[CONSENT_KEY];
        if (saved?.version === 9 && saved.browser_boot_id === boot) {
            this.ownershipGeneration = Math.max(this.ownershipGeneration,
                Number.isSafeInteger(saved.ownership_generation) ? saved.ownership_generation + 1 : 1);
            for (const [key, revision] of Object.entries(saved.revisions || {})) {
                if (!Number.isSafeInteger(revision) || revision <= 0) continue;
                const changed = this.restoreRevoked || (this.controlTokens.get(key) || 0) > 0;
                this.revisions.set(key, Math.max(this.revisions.get(key) || 0, revision + Number(changed)));
            }
            for (const candidate of saved.records || []) {
                if (!text(candidate.remote_key) || !validPageRef(candidate.page) ||
                        !text(candidate.enable_id) || !Number.isSafeInteger(candidate.revision) || candidate.revision <= 0 ||
                        !Number.isSafeInteger(candidate.tab_id) || !Number.isSafeInteger(candidate.window_id) ||
                        !validFence(candidate.executor_fence) || !text(candidate.principal_id) ||
                        !text(candidate.owner_display?.daemon_instance_id) ||
                        !["enabled", "paused"].includes(candidate.phase)) continue;
                if (boot !== this.browserBootId || generation !== this.restoreGeneration) return;
                if (this.restoreRevoked || (this.controlTokens.get(candidate.remote_key) || 0) > 0 ||
                        (this.revisions.get(candidate.remote_key) || 0) > candidate.revision) continue;
                if (this.records.has(candidate.remote_key) ||
                        [...this.records.values()].some(record => record.tab_id === candidate.tab_id)) continue;
                this.revisions.set(candidate.remote_key, Math.max(candidate.revision,
                    this.revisions.get(candidate.remote_key) || 1));
                const record = {...candidate, agreed: false, attached: false};
                this.records.set(record.remote_key, record);
                try {
                    await this.validateTab(record);
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
        await this.persistWal();
        await this.persistConsent();
        for (const lease of this.bootstrapLeases) await this.ordinaryOpen(lease);
        this.bootstrapLeases = [];
        this.restored = true;
    }
    persistWal() {
        const write = this.localTail.catch(() => {}).then(() => this.api(() =>
            this.chrome.storage.local.set({[WAL_KEY]: clone({version: 9,
                effects: this.effects, creations: this.creations, ordinary: this.ordinary,
                cancellations: this.cancellations})}),
        "persist effect journal"));
        this.localTail = write;
        return write;
    }
    persistConsent() {
        const write = this.sessionTail.catch(() => {}).then(() => {
            const records = [...this.records.values()].filter(record => record.phase !== "disabled")
                .map(({agreed, attached, attachment, pendingDetach, ...record}) => record);
            return this.api(() => this.chrome.storage.session.set({[CONSENT_KEY]: {
                version: 9, browser_boot_id: this.browserBootId,
                ownership_generation: this.ownershipGeneration,
                revisions: Object.fromEntries(this.revisions), records: clone(records),
            }}), "persist browser consent");
        });
        this.sessionTail = write;
        return write;
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
    state(key) {
        const record = this.records.get(key);
        return {browser_boot_id: this.browserBootId, revision: this.revisions.get(key) || 1,
            phase: record?.phase || "disabled", binding: record?.phase !== "disabled" && record ?
                {enable_id: record.enable_id, page: clone(record.page)} : null};
    }
    async validateTab(record) {
        if (record.browser_boot_id !== this.browserBootId || this.revokedTabs.has(record.tab_id)) {
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
        return [...this.records.values()].some(record =>
            record.phase !== "disabled" && record.tab_id === tabId);
    }
    closeAuthorization(record, phase) {
        this.bumpControl(record.remote_key);
        record.pendingDetach = record.attachment;
        record.agreed = false;
        record.phase = phase;
        record.revision = this.nextRevision(record.remote_key);
        this.ownershipChanged(record.tab_id);
        for (const op of this.operations.values()) {
            if (op.payload.remote_key === record.remote_key) op.cancelled = true;
        }
    }
    async detach(record, attachment = record?.pendingDetach || record?.attachment) {
        if (!record || !attachment || record.attachment !== attachment) return;
        record.attached = false;
        record.attachment = null;
        record.pendingDetach = null;
        await this.api(() => {
            if (record.attachment) return;
            return this.chrome.debugger.detach({tabId: record.tab_id});
        }, "detach work tab debugger");
    }
    async publish(key) {
        const remote = this.remotes.get(key);
        if (!remote?.connected) fail("executor_unavailable", "Reconnect the selected workbench first");
        const record = this.records.get(key);
        const state = this.state(key);
        const payload = {remote_key: key, connection_id: remote.connection_id,
            principal_id: remote.principal_id, request_id: crypto.randomUUID(),
            executor_fence: clone(remote.executor_fence), state};
        const receipt = await this.native("abush_browser", "workspace_state", payload);
        const expected = {type: "workspace_state_accepted", request_id: payload.request_id,
            executor_fence: payload.executor_fence, state};
        if (!sameRef(receipt, expected)) fail("stale_authority", "Workbench did not accept browser state");
        if (this.remotes.get(key) !== remote || this.records.get(key) !== record ||
                !sameRef(this.state(key), state)) fail("stale_authority", "Browser authorization changed");
        if (record && record.phase === "enabled") record.agreed = true;
        return state;
    }
    validRemote(remote) {
        return remote && text(remote.remote_key) && typeof remote.connected === "boolean" &&
            (!remote.connected || [remote.connection_id, remote.principal_id, remote.workspace_id]
                .every(text) && validFence(remote.executor_fence) &&
                text(remote.owner_display?.daemon_instance_id) &&
                text(remote.owner_display?.daemon_boot_id));
    }
    async remoteState(remote) {
        if (!this.validRemote(remote)) fail("invalid_command", "Invalid authenticated remote state");
        const key = remote.remote_key;
        const record = this.records.get(key);
        if (!remote.connected || this.remotes.get(key) && !sameRef(this.remotes.get(key), remote)) this.bumpControl(key);
        const acceptedRemote = clone(remote);
        this.remotes.set(key, acceptedRemote);
        if (!this.revisions.has(key)) this.nextRevision(key);
        if (record && record.phase !== "disabled") {
            const revision = record.revision;
            const unchanged = remote.connected && remote.principal_id === record.principal_id &&
                remote.workspace_id === record.page.workspace_id &&
                sameRef(remote.executor_fence, record.executor_fence) &&
                sameRef(remote.owner_display, record.owner_display);
            if (!unchanged) {
                const attachment = record.attachment;
                this.closeAuthorization(record, "paused");
                await this.persistConsent();
                await this.detach(record, attachment);
            } else {
                record.agreed = false;
                try { await this.validateTab(record); }
                catch (error) {
                    if (this.remotes.get(key) !== acceptedRemote || record.revision !== revision) return false;
                    this.closeAuthorization(record, "disabled");
                    this.reportAdvisoryError(error);
                }
            }
        }
        if (this.remotes.get(key) !== acceptedRemote) return false;
        await this.persistConsent();
        if (this.remotes.get(key) !== acceptedRemote) return false;
        if (remote.connected) {
            await this.publish(key);
            if (this.remotes.get(key) !== acceptedRemote) return false;
            await this.flushEffects(key);
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
    popupState() {
        return {ok: true, error: this.lastError, remotes: [...this.remotes.values()].map(remote => ({
            remote_key: remote.remote_key, principal_id: remote.principal_id,
            display: remote.display_name || remote.connection_display_name || remote.owner_display?.workbench_name || "Abush",
            connected: remote.connected, state: this.state(remote.remote_key),
            creation_unknown: this.creations[`${this.browserBootId}\0${remote.remote_key}`]?.phase === "unknown",
            agreed: this.records.get(remote.remote_key)?.agreed || false,
        }))};
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
        } finally { this.controls.delete(key); }
    }
    selectedRemote(key) {
        const remote = this.remotes.get(key);
        if (!remote?.connected) fail("executor_unavailable", "Select a connected authenticated workbench");
        return remote;
    }
    async enable(key) {
        const remote = this.selectedRemote(key);
        const boot = this.browserBootId;
        const transport = this.transportGeneration;
        const token = this.controlTokens.get(key) || 0;
        let abandoned = false;
        const check = () => {
            if (abandoned || transport !== this.transportGeneration || boot !== this.browserBootId || this.remotes.get(key) !== remote ||
                    token !== (this.controlTokens.get(key) || 0)) {
                fail("stale_authority", "Remote or consent changed during enable");
            }
        };
        const existing = this.records.get(key);
        if (existing?.phase !== "disabled" && existing) {
            fail("operation_conflict", "Use Resume for the original work tab, or Disconnect first");
        }
        const creationKey = `${this.browserBootId}\0${key}`;
        if (this.creations[creationKey]?.phase === "unknown") {
            fail("unknown_effect", "Interrupted tab creation is unknown. Inspect the partial tab; Disconnect leaves it ordinary, then Enable creates a fresh tab");
        }
        const window = await this.api(() => this.chrome.windows.getCurrent(), "select current window");
        check();
        if (window.type !== "normal") fail("invalid_command", "Open the popup in an existing normal Chrome window");
        const record = {remote_key: key, principal_id: remote.principal_id,
            executor_fence: clone(remote.executor_fence), owner_display: clone(remote.owner_display),
            browser_boot_id: this.browserBootId, enable_id: crypto.randomUUID(),
            revision: this.nextRevision(key), page: {workspace_id: remote.workspace_id,
                page_id: crypto.randomUUID(), generation: 1}, window_id: window.id,
            tab_id: null, group_id: null, phase: "paused", agreed: false, attached: false};
        const creationRevision = record.revision;
        const creation = {phase: "unknown", enable_id: record.enable_id};
        this.creations[creationKey] = creation;
        try {
            await this.persistWal();
            check();
            const tab = await this.api(async () => {
                check();
                const created = await this.chrome.tabs.create({windowId: window.id,
                    url: "about:blank", active: false});
                check();
                record.tab_id = created.id;
                this.pendingCreations.set(created.id, record);
                return created;
            }, "create inactive work tab");
            const checkTab = () => {
                check();
                if (this.revokedTabs.has(tab.id) || this.revisions.get(key) !== creationRevision) {
                    fail("stale_page", "Created work tab was moved, replaced, or revoked");
                }
            };
            checkTab();
            await this.validateTab(record);
            record.group_id = await this.api(() => {
                checkTab();
                return this.chrome.tabs.group({tabIds: [tab.id]});
            }, "group exact work tab");
            await this.api(() => {
                checkTab();
                return this.chrome.tabGroups.update(record.group_id, {
                    title: displayTitle(remote.display_name || remote.connection_display_name), color: "purple",
                });
            }, "label work tab group");
            checkTab();
            await this.validateTab(record);
            checkTab();
            this.records.set(key, record);
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
        } catch (error) {
            abandoned = true;
            if (record.revision === this.revisions.get(key) && record.phase !== "disabled") {
                record.agreed = false;
                record.phase = "paused";
            }
            throw new Error(`Enable failed; no tabs were removed. ${error.message}. Inspect the partial tab; Disconnect leaves it ordinary, then Enable creates a fresh tab`);
        } finally {
            this.pendingCreations.delete(record.tab_id);
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
        const draining = [...this.operations.values()].filter(op => op.payload.remote_key === key);
        await bounded(() => Promise.all(draining.map(op => op.done.catch(() => {}))),
            "drain previous browser work", 5000);
        await this.detach(record, record.pendingDetach);
        await this.validateTab(record);
        if (this.remotes.get(key) !== remote || this.records.get(key) !== record ||
                record.revision !== revision) {
            fail("stale_authority", "Remote changed during resume");
        }
        record.executor_fence = clone(remote.executor_fence);
        record.owner_display = clone(remote.owner_display);
        record.revision = this.nextRevision(key);
        record.phase = "enabled";
        record.agreed = false;
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
    revokeTab(tabId) {
        this.revokedTabs.add(tabId);
        const pending = this.pendingCreations.get(tabId);
        if (pending) this.bumpControl(pending.remote_key);
        const record = [...this.records.values()].find(record =>
            record.tab_id === tabId && record.phase !== "disabled");
        if (!record) return Promise.resolve();
        const attachment = record.attachment;
        this.closeAuthorization(record, "disabled");
        return (async () => {
            await this.persistConsent();
            await this.detach(record, attachment);
            if (this.remotes.get(record.remote_key)?.connected) await this.publish(record.remote_key);
        })();
    }
    debuggerDetached(tabId) {
        const record = [...this.records.values()].find(record => record.tab_id === tabId);
        if (record) {
            record.attached = false;
            record.attachment = null;
        }
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
                !exactFields(payload.command, ["command", "page", "url"]) ||
                payload.command.command !== "navigate" || !validPageRef(payload.command.page) ||
                !text(payload.command.url)) fail("invalid_command", "Invalid v9 navigate event");
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
        const record = this.records.get(payload.remote_key);
        const remote = this.remotes.get(payload.remote_key);
        if (op.cancelled || !record || record.phase !== "enabled" || !record.agreed) {
            fail("browser_disabled", "Browser is disabled or paused; use the Chrome popup");
        }
        if (!remote?.connected || remote.connection_id !== payload.connection_id ||
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
        const op = {payload: clone(payload), cancelled: false, dispatched: false, tab_id: null};
        this.operations.set(key, op);
        const entry = {version: 9, payload: clone(payload), phase: "accepted", completion: null};
        this.effects[key] = entry;
        const tail = this.executionTails.get(payload.remote_key) || Promise.resolve();
        op.done = tail.catch(() => {}).then(() => this.performExecute(key, op, entry));
        this.executionTails.set(payload.remote_key, op.done);
        op.done.catch(() => {});
        return op.done;
    }
    async performExecute(key, op, entry) {
        let error;
        try {
            await this.persistWal();
            const record = this.authorization(op.payload, op);
            op.tab_id = record.tab_id;
            await this.validateTab(record);
            this.authorization(op.payload, op);
            if (!record.attached) {
                await this.api(async () => {
                    this.authorization(op.payload, op);
                    const token = {};
                    record.attachment = token;
                    const attachment = this.chrome.debugger.attach({tabId: record.tab_id}, "1.3");
                    await attachment;
                    if (op.cancelled || record.attachment !== token || this.records.get(op.payload.remote_key) !== record ||
                            !record.agreed || record.revision !== op.payload.authorization.revision) {
                        if (!record.attachment || record.attachment === token) {
                            record.attachment = null;
                            await this.api(() => {
                                if (!record.attachment) return this.chrome.debugger.detach({tabId: record.tab_id});
                            }, "detach superseded attachment");
                        }
                        fail("browser_disabled", "Attachment authorization was revoked");
                    }
                    record.attached = true;
                }, "attach exact work tab debugger");
            }
            this.authorization(op.payload, op);
            entry.phase = "dispatched";
            await this.persistWal();
            this.authorization(op.payload, op);
            const result = await this.api(() => {
                this.authorization(op.payload, op);
                op.dispatched = true;
                return this.chrome.debugger.sendCommand({tabId: record.tab_id},
                    "Page.navigate", {url: op.payload.command.url});
            }, "navigate exact work tab");
            if (!result || typeof result !== "object" || !text(result.frameId) ||
                    result.errorText !== undefined && result.errorText !== "" ||
                    result.loaderId !== undefined && !text(result.loaderId) ||
                    result.isDownload !== undefined && typeof result.isDownload !== "boolean") {
                throw new Error(`Page.navigate response is not confirmed: ${result?.errorText || "invalid response"}`);
            }
            entry.completion = "applied";
        } catch (failure) {
            error = failure;
            op.cancelled = true;
            entry.completion = op.dispatched ? "unknown" : "not_applied";
            this.reportAdvisoryError(failure);
        }
        entry.phase = "terminal";
        try {
            await this.persistWal();
            await this.deliverEffect(key);
        } finally {
            this.operations.delete(key);
            if (op.cancelled) {
                const record = this.records.get(op.payload.remote_key);
                if (record) await this.detach(record);
            }
        }
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
            completion: entry.completion};
        if (this.receipts[key]) return entry.completion;
        const receipt = await this.native("abush_browser", "executor_completion", payload);
        const expected = {type: "completion_accepted", request_id: payload.request_id,
            effect_id: payload.effect_id, executor_fence: payload.executor_fence,
            completion: payload.completion};
        if (!sameRef(receipt, expected)) fail("stale_authority", "Invalid completion receipt; result remains pending");
        this.receipts[key] = receipt;
        try {
            await this.api(() => this.chrome.storage.local.set({[RECEIPTS_KEY]: {
                version: 9, receipts: clone(this.receipts),
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
                !sameRef(remote.executor_fence, payload.executor_fence)) {
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
        if (op) op.cancelled = true;
        await this.persistWal();
        if (op) {
            const record = this.records.get(payload.remote_key);
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
        const op = this.ordinaryOperations.get(payload.request_id);
        if (op) op.cancelled = true;
        return Boolean(op);
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
        const op = {cancelled: false, dispatched: false};
        const entry = {version: 9, payload: clone(payload), phase: "accepted", completion: null};
        this.ordinary[payload.request_id] = entry;
        this.ordinaryOperations.set(payload.request_id, op);
        op.done = this.ordinaryTail.catch(() => {}).then(() => this.performOrdinary(payload, entry, op))
            .finally(() => this.ordinaryOperations.delete(payload.request_id));
        this.ordinaryTail = op.done;
        return op.done;
    }
    async performOrdinary(payload, entry, op) {
        const check = () => {
            if (op.cancelled || Date.now() >= payload.deadline_ms) {
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
            const nonce = crypto.randomUUID().replaceAll("-", "");
            const url = this.chrome.runtime.getURL(`ordinary-staging.html#${nonce}`);
            entry.phase = "dispatched";
            await this.persistWal();
            check();
            let tab;
            if (window) tab = await api(() => {
                check();
                op.dispatched = true;
                return this.chrome.tabs.create({windowId: window.id, url, active: true});
            }, "create ordinary staging tab");
            else {
                const created = await api(() => {
                    check();
                    op.dispatched = true;
                    return this.chrome.windows.create({url, type: "normal", focused: true});
                }, "explicit ordinary cold window");
                tab = created.tabs?.[0];
            }
            if (!Number.isSafeInteger(tab?.id) || this.ownsTab(tab.id)) throw new Error("No exact ordinary tab");
            await api(() => this.chrome.tabs.ungroup([tab.id]), "ungroup exact ordinary tab");
            const staging = await api(() => this.chrome.tabs.get(tab.id), "verify ordinary staging");
            if (staging.groupId !== -1 || staging.url !== url || !staging.active) {
                throw new Error("Ordinary staging tab must be active and ungrouped");
            }
            check();
            await bounded(async () => {
                while (true) {
                    check();
                    const current = await api(() => this.chrome.tabs.get(tab.id), "ordinary title");
                    if (current.title === `${ORDINARY_PREFIX}${nonce}`) return;
                    check();
                    await new Promise(resolve => setTimeout(resolve,
                        Math.min(10, Math.max(1, payload.deadline_ms - Date.now()))));
                }
            }, "ordinary staging title", Math.max(1, payload.deadline_ms - Date.now()));
            const prepared = await native("prepare", {nonce,
                deadline_ms: payload.deadline_ms});
            if (!exactFields(prepared, ["nonce", "xid"]) || prepared.nonce !== nonce ||
                    !Number.isSafeInteger(prepared.xid) || prepared.xid <= 0) throw new Error("Invalid ordinary prepare receipt");
            check();
            let navigationConfirmed = false;
            const requested = new URL(payload.url).href;
            if (new URL(payload.url).protocol === "file:") {
                const wire = {...prepared, url: payload.url, deadline_ms: payload.deadline_ms};
                const receipt = await native("ordinary_file_navigate", wire);
                if (!sameRef(receipt, {...wire, completion: "applied"})) throw new Error("File navigation is not confirmed");
                navigationConfirmed = true;
            } else {
                const navigated = await api(() => {
                    check();
                    return this.chrome.tabs.update(tab.id, {url: payload.url, active: true});
                }, "navigate exact ordinary tab");
                navigationConfirmed = navigated?.id === tab.id &&
                    new URL(navigated.pendingUrl || navigated.url).href === requested;
            }
            check();
            const activated = await native("activate", {...prepared, deadline_ms: payload.deadline_ms});
            if (!sameRef(activated, prepared)) throw new Error("Invalid ordinary activation receipt");
            const result = await api(() => this.chrome.tabs.get(tab.id), "verify ordinary result");
            const destination = result.pendingUrl || result.url;
            const exactDestination = new URL(destination).href === requested;
            if ((!exactDestination && (!navigationConfirmed || result.pendingUrl)) ||
                    result.groupId !== -1 || !result.active) {
                throw new Error("Ordinary tab result is not exact, active, and ungrouped");
            }
            check();
            entry.completion = "applied";
        } catch (error) {
            entry.completion = op.dispatched ? "unknown" : "not_applied";
            this.reportAdvisoryError(error);
        }
        entry.phase = "terminal";
        await this.persistWal();
        return this.ordinaryCompletion(payload, entry.completion);
    }
}
if (typeof module !== "undefined") module.exports = {WorkspaceExecutor, BROWSER_ERROR_CODES,
    ORDINARY_PREFIX, CONSENT_KEY, WAL_KEY, RECEIPTS_KEY, displayTitle, bounded};
