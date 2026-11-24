if (typeof importScripts === "function") importScripts("workspace.js");

const NATIVE_HOST = "io.github.amosbird.browser_router";
const ENVELOPE_VERSION = 1;
const MAX_NATIVE_MESSAGE_BYTES = 1024 * 1024;
const NATIVE_TEXT_ENCODER = new TextEncoder();

const NATIVE_TIMEOUTS_MS = Object.freeze({
    native_bootstrap: 15000,
    browser_executor_prepare: 8000,
    browser_executor_ready: 8000,
    external_links_open: 30000,
    ordinary_open_prepare: 8000,
    ordinary_open_activate: 8000,
    ordinary_file_navigate: 8000,
    workspace_state: 8000,
    executor_completion: 8000,
    cancellation_completion: 8000,
    engine_open: 35000,
    engine_execute: 35000,
    engine_close: 5000,
    engine_cdp: 5000,
    refresh_remotes: 8000,
});
const SERIAL_NATIVE_OPERATIONS = new Set();
const SERIAL_NATIVE_KEY = "native-host";

class NativeDispatcher {
    constructor(connect, timeoutMs = 3000,
                bootstrapTimeoutMs = NATIVE_TIMEOUTS_MS.native_bootstrap) {
        this.connect = connect;
        this.timeoutMs = timeoutMs;
        this.bootstrapTimeoutMs = bootstrapTimeoutMs;
        this.port = null;
        this.requestTails = new Map();
        this.pending = new Map();
        this.eventListeners = new Set();
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 8;
        this.reconnectExhausted = false;
        this.reconnectTimer = null;
        this.stopped = false;
        this.bootstrapEnabled = false;
        this.browserReadyEnabled = false;
        this.browserExecutorStatus = {phase: "unavailable", error: null};
        this.bootstrapGeneration = 0;
        this.bootstrapAttempt = null;
        this.bootstrapWaiters = [];
        this.setTimeout = (callback, delay) => globalThis.setTimeout(callback, delay);
        this.clearTimeout = timer => globalThis.clearTimeout(timer);
    }

    bootstrapFailure(message, dispatch = "not_sent") {
        const error = new Error(message);
        error.nativeDispatch = dispatch;
        return error;
    }

    createBootstrapAttempt(port) {
        const previous = this.bootstrapAttempt;
        if (previous && !previous.settled) {
            this.finishBootstrapAttempt(previous, false,
                this.bootstrapFailure("native bootstrap attempt was superseded"));
        }
        let resolve;
        let reject;
        const promise = new Promise((resolvePromise, rejectPromise) => {
            resolve = resolvePromise;
            reject = rejectPromise;
        });
        promise.catch(() => {});
        const attempt = {
            generation: ++this.bootstrapGeneration,
            port,
            promise,
            resolve,
            reject,
            timer: null,
            settled: false,
            outcome: null,
        };
        this.bootstrapAttempt = attempt;
        if (this.browserReadyEnabled && port !== null) this.startBrowserReady(attempt);
        for (const waiter of this.bootstrapWaiters.splice(0)) {
            attempt.promise.then(waiter.resolve, waiter.reject);
        }
        if (port !== null) {
            attempt.timer = this.setTimeout(() => {
                if (this.bootstrapAttempt !== attempt || attempt.settled) return;
                const error = this.bootstrapFailure("native bootstrap timed out");
                this.finishBootstrapAttempt(attempt, false, error);
                this.emitLifecycle("bootstrap_timeout", {generation: attempt.generation});
                if (this.port === port) {
                    this.port = null;
                    const dispatchError = this.bootstrapFailure(
                        "native host bootstrap timed out", "unknown");
                    this.rejectAll(dispatchError);
                    try {
                        port.disconnect?.();
                    } catch (_) {}
                    this.scheduleReconnect();
                }
            }, this.bootstrapTimeoutMs);
            attempt.timer?.unref?.();
        }
        return attempt;
    }

    finishBootstrapAttempt(attempt, ok, value) {
        if (!attempt || attempt.settled) return;
        attempt.settled = true;
        attempt.outcome = ok ? "ready" : "failed";
        if (attempt.timer !== null) {
            this.clearTimeout(attempt.timer);
            attempt.timer = null;
        }
        if (ok) attempt.resolve(value);
        else attempt.reject(value instanceof Error ? value : new Error(String(value)));
    }

    rejectBootstrapAttempt(error, port = null) {
        if (!this.bootstrapEnabled) return null;
        let attempt = this.bootstrapAttempt;
        if (!attempt || attempt.settled || (port !== null && attempt.port !== port)) {
            attempt = this.createBootstrapAttempt(null);
        }
        this.finishBootstrapAttempt(attempt, false, error);
        return attempt;
    }

    resolveBootstrapAttempt(payload, port) {
        this.bootstrapEnabled = true;
        let attempt = this.bootstrapAttempt;
        if (!attempt || attempt.port !== port ||
                (attempt.settled && (attempt.outcome !== "ready" ||
                    attempt.bootstrapPayload?.browser_boot_id !== payload?.browser_boot_id))) {
            attempt = this.createBootstrapAttempt(port);
        }
        if (!attempt.settled) attempt.bootstrapPayload = payload;
        this.finishBootstrapAttempt(attempt, true, payload);
    }

    bootstrap() {
        this.bootstrapEnabled = true;
        if (this.bootstrapAttempt?.settled && this.bootstrapAttempt.outcome === "failed") {
            if (this.port) return this.createBootstrapAttempt(this.port).promise;
            if (this.reconnectExhausted) {
                return Promise.reject(this.bootstrapFailure(
                    "native host reconnect attempts exhausted"));
            }
            if (this.reconnectTimer !== null) {
                return new Promise((resolve, reject) => {
                    this.bootstrapWaiters.push({resolve, reject});
                });
            }
        }
        try {
            const port = this.ensurePort();
            if (!this.bootstrapAttempt || this.bootstrapAttempt.port !== port ||
                    (this.bootstrapAttempt.settled &&
                        this.bootstrapAttempt.outcome === "failed")) {
                this.createBootstrapAttempt(port);
            }
        } catch (error) {
            this.rejectBootstrapAttempt(error);
        }
        return this.bootstrapAttempt.promise;
    }

    browserReady() {
        this.browserReadyEnabled = true;
        const bootstrap = this.bootstrap();
        bootstrap.catch(() => {});
        const attempt = this.bootstrapAttempt;
        if (!attempt || attempt.port === null) {
            return bootstrap.then(() => this.browserReady());
        }
        return this.startBrowserReady(attempt);
    }

    assertBrowserReadyAttempt(attempt) {
        if (this.bootstrapAttempt !== attempt || this.port !== attempt.port || this.stopped) {
            throw this.bootstrapFailure("native browser readiness attempt was superseded");
        }
    }

    startBrowserReady(attempt) {
        if (attempt.browserReadyPromise) return attempt.browserReadyPromise;
        const status = (phase, error = null) => {
            if (this.bootstrapAttempt === attempt && this.port === attempt.port) {
                this.emitLifecycle("browser_executor_status", {phase, error});
            }
        };
        const exact = (value, fields) => value && typeof value === "object" &&
            !Array.isArray(value) && Object.keys(value).length === fields.length &&
            fields.every(key => Object.hasOwn(value, key));
        attempt.browserReadyPromise = Promise.resolve().then(async () => {
            status("pending");
            const bootstrap = await attempt.promise;
            this.assertBrowserReadyAttempt(attempt);
            const boot = bootstrap?.browser_boot_id;
            if (typeof boot !== "string" || !boot.length) {
                throw new Error("Native browser boot is unavailable for executor readiness");
            }
            const prepared = await this.dispatchRequest("native_host", "browser_executor_prepare",
                {browser_boot_id: boot, protocol_version: 10}, attempt.port);
            this.assertBrowserReadyAttempt(attempt);
            if (!exact(prepared, ["browser_boot_id", "native_connection_id", "protocol_version",
                "primary_profile"]) || prepared.browser_boot_id !== boot ||
                    prepared.protocol_version !== 10 ||
                    typeof prepared.native_connection_id !== "string" ||
                    !prepared.native_connection_id.length ||
                    typeof prepared.primary_profile !== "boolean") {
                throw new Error("Invalid native browser executor prepare receipt");
            }
            if (!prepared.primary_profile) {
                const error = "Browser automation belongs to the configured primary browser profile";
                status("secondary", error);
                return {ready: false, primary_profile: false};
            }
            const receipt = await this.dispatchRequest("native_host", "browser_executor_ready", {
                browser_boot_id: boot, native_connection_id: prepared.native_connection_id,
                protocol_version: 10, capabilities: {cancellation: true, owned_tab_group: true,
                    official_engine: true, typed_output: true},
            }, attempt.port);
            this.assertBrowserReadyAttempt(attempt);
            if (!exact(receipt, ["browser_boot_id", "native_connection_id", "protocol_version",
                "ready"]) || receipt.browser_boot_id !== boot || receipt.protocol_version !== 10 ||
                    receipt.native_connection_id !== prepared.native_connection_id ||
                    receipt.ready !== true) {
                throw new Error("Invalid native browser executor ready receipt");
            }
            status("ready");
            return receipt;
        }).catch(error => {
            status("unavailable", error.message || String(error));
            throw error;
        });
        attempt.browserReadyPromise.catch(() => {});
        return attempt.browserReadyPromise;
    }

    requestBootstrap(port) {
        const attempt = this.bootstrapAttempt;
        this.dispatchRequest("native_host", "bootstrap", {}, port).then(payload => {
            if (this.bootstrapAttempt !== attempt || attempt.settled || this.port !== port) return;
            this.resolveBootstrapAttempt(payload, port);
        }).catch(error => {
            if (this.bootstrapAttempt !== attempt || attempt.settled || this.port !== port) return;
            this.finishBootstrapAttempt(attempt, false, error);
            this.emitLifecycle("bootstrap_request_failed", {generation: attempt.generation});
            this.port = null;
            this.rejectAll(error);
            try {
                port.disconnect?.();
            } catch (_) {}
            this.scheduleReconnect();
        });
    }

    ensurePort() {
        if (this.port) return this.port;
        this.stopped = false;
        if (this.reconnectTimer !== null) {
            this.clearTimeout(this.reconnectTimer);
            this.reconnectTimer = null;
        }
        const port = (() => {
            try {
                return this.connect();
            } catch (error) {
                this.rejectBootstrapAttempt(error);
                this.scheduleReconnect();
                throw error;
            }
        })();
        this.port = port;
        port.onMessage.addListener(message => this.dispatch(message, port));
        port.onDisconnect.addListener(() => {
            if (this.port !== port) return;
            this.port = null;
            const error = new Error("native host disconnected");
            error.nativeDispatch = "unknown";
            this.rejectBootstrapAttempt(error, port);
            this.emitLifecycle("disconnect");
            this.rejectAll(error);
            this.scheduleReconnect();
        });
        if (this.bootstrapEnabled) {
            this.createBootstrapAttempt(port);
            this.requestBootstrap(port);
        }
        return port;
    }

    request(service, operation, payload = {}) {
        if (!SERIAL_NATIVE_OPERATIONS.has(operation)) {
            return this.dispatchRequest(service, operation, payload);
        }
        const key = SERIAL_NATIVE_KEY;
        const tail = this.requestTails.get(key) || Promise.resolve();
        const request = tail.catch(() => {})
            .then(() => this.dispatchRequest(service, operation, payload));
        this.requestTails.set(key, request);
        request.finally(() => {
            if (this.requestTails.get(key) === request) this.requestTails.delete(key);
        }).catch(() => {});
        return request;
    }

    dispatchRequest(service, operation, payload = {}, expectedPort = null) {
        const id = crypto.randomUUID();
        let timeoutMs = Math.max(this.timeoutMs, NATIVE_TIMEOUTS_MS[operation] || 0);
        if (service === "external_links" && operation === "open") {
            timeoutMs = NATIVE_TIMEOUTS_MS.external_links_open;
        }
        if (service === "ordinary_open") {
            const configured = NATIVE_TIMEOUTS_MS[`ordinary_open_${operation}`];
            if (configured !== undefined) timeoutMs = configured;
            if (Number.isSafeInteger(payload.deadline_ms)) {
                timeoutMs = Math.max(1, Math.min(timeoutMs,
                    payload.deadline_ms - Date.now()));
            }
        }
        return new Promise((resolve, reject) => {
            const timer = this.setTimeout(() => {
                if (!this.pending.delete(id)) return;
                this.emitLifecycle("timeout", {service, operation});
                const error = new Error("native request timed out");
                error.nativeDispatch = "unknown";
                reject(error);
            }, timeoutMs);
            this.pending.set(id, {resolve, reject, timer});
            let dispatchStarted = false;
            try {
                const port = expectedPort || this.ensurePort();
                if (expectedPort && this.port !== expectedPort) {
                    throw this.bootstrapFailure("native request port was superseded");
                }
                const envelope = {
                    version: ENVELOPE_VERSION,
                    id,
                    service,
                    operation,
                    payload,
                };
                const encodedBytes = NATIVE_TEXT_ENCODER.encode(JSON.stringify(envelope)).length;
                if (encodedBytes > MAX_NATIVE_MESSAGE_BYTES) {
                    throw new Error(`native request exceeds 1 MiB limit (${operation}: ${encodedBytes} bytes)`);
                }
                dispatchStarted = true;
                port.postMessage(envelope);
            } catch (error) {
                const failure = new Error(error?.message || String(error));
                failure.nativeDispatch = dispatchStarted ? "unknown" : "not_sent";
                this.finish(id, false, failure);
            }
        });
    }

    reconnectDelay() {
        return Math.min(5000, 100 * (2 ** Math.min(this.reconnectAttempts, 6)));
    }

    scheduleReconnect() {
        if (this.stopped || this.reconnectTimer !== null) return;
        if (this.reconnectAttempts >= this.maxReconnectAttempts) {
            if (!this.reconnectExhausted) {
                this.reconnectExhausted = true;
                const error = this.bootstrapFailure(
                    "native host reconnect attempts exhausted");
                this.rejectBootstrapAttempt(error);
                for (const waiter of this.bootstrapWaiters.splice(0)) waiter.reject(error);
                this.emitLifecycle("reconnect_exhausted", {
                    attempts: this.reconnectAttempts,
                });
            }
            return;
        }
        const delay = this.reconnectDelay();
        this.reconnectAttempts += 1;
        this.reconnectTimer = this.setTimeout(() => {
            this.reconnectTimer = null;
            if (this.stopped || this.port) return;
            try {
                this.ensurePort();
                this.emitLifecycle("reconnect");
            } catch (_) {
                this.scheduleReconnect();
            }
        }, delay);
    }

    stop() {
        this.stopped = true;
        if (this.reconnectTimer !== null) {
            this.clearTimeout(this.reconnectTimer);
            this.reconnectTimer = null;
        }
        const error = this.bootstrapFailure("native dispatcher stopped");
        this.rejectBootstrapAttempt(error);
        for (const waiter of this.bootstrapWaiters.splice(0)) waiter.reject(error);
        this.port?.disconnect?.();
        this.port = null;
        this.rejectAll(error);
    }

    dispatch(message, sourcePort = null) {
        if (sourcePort !== null && this.port !== sourcePort) return;
        if (!message || typeof message !== "object") return;
        if (typeof message.reply_to === "string" && this.pending.has(message.reply_to) &&
                message.version !== ENVELOPE_VERSION) {
            this.finish(message.reply_to, false, new Error("mismatched native response version"));
            return;
        }
        if (message.version !== ENVELOPE_VERSION) return;
        const validEvent = typeof message.event === "string" &&
            typeof message.service === "string" && message.payload &&
            typeof message.payload === "object";
        const validResponse = typeof message.reply_to === "string" &&
            this.pending.has(message.reply_to);
        if (!validEvent && !validResponse) return;
        this.reconnectAttempts = 0;
        this.reconnectExhausted = false;
        if (validEvent) {
            if (message.service === "native_host" && message.event === "bootstrap") {
                this.resolveBootstrapAttempt(message.payload, sourcePort || this.port);
            }
            this.eventListeners.forEach(listener => listener(message));
            return;
        }
        if (typeof message.reply_to !== "string") return;
        const pending = this.pending.get(message.reply_to);
        if (!pending) return;
        if (typeof message.ok !== "boolean" ||
                (message.ok && message.error !== undefined) ||
                (!message.ok && typeof message.error !== "string")) {
            this.finish(message.reply_to, false, new Error("malformed native response"));
            return;
        }
        if (message.ok) this.finish(message.reply_to, true, message.payload);
        else this.finish(message.reply_to, false, new Error(message.error));
    }

    finish(id, ok, value) {
        const pending = this.pending.get(id);
        if (!pending) return;
        this.pending.delete(id);
        this.clearTimeout(pending.timer);
        if (ok) pending.resolve(value);
        else pending.reject(value instanceof Error ? value : new Error(String(value)));
    }

    rejectAll(error) {
        for (const id of [...this.pending.keys()]) this.finish(id, false, error);
    }

    emitLifecycle(event, payload = {}) {
        if (event === "browser_executor_status") this.browserExecutorStatus = {...payload};
        else if (["disconnect", "bootstrap_timeout", "reconnect_exhausted"].includes(event)) {
            this.browserExecutorStatus = {phase: "unavailable",
                error: "Native browser executor is disconnected"};
        }
        this.eventListeners.forEach(listener => listener({
            version: ENVELOPE_VERSION,
            service: "native_host",
            event,
            payload,
        }));
    }

    onEvent(listener) {
        this.eventListeners.add(listener);
        return () => this.eventListeners.delete(listener);
    }
}

let nativeDispatcher;
let workspaceExecutor;
let scrollQueues = Promise.resolve();

function dispatcher() {
    nativeDispatcher ||= new NativeDispatcher(
        () => chrome.runtime.connectNative(NATIVE_HOST),
    );
    return nativeDispatcher;
}

function workspace() {
    const Executor = typeof WorkspaceExecutor !== "undefined" ? WorkspaceExecutor :
        require("./workspace.js").WorkspaceExecutor;
    workspaceExecutor ||= new Executor(chrome, sendNativeRequest);
    return workspaceExecutor;
}

function notifyFailure(message) {
    chrome.notifications.create({
        type: "basic",
        iconUrl: "icon.png",
        title: "Browser integration failed",
        message,
    });
}

function sendNativeRequest(service, operation, payload) {
    return dispatcher().request(service, operation, payload);
}

function handleScrollCommand(command, tab) {
    const current = scrollQueues
        .catch(() => {})
        .then(() => sendNativeRequest("external_links", "scroll", {
            direction: command,
            url: tab?.url,
        }))
        .catch(error => notifyFailure(error.message || command));
    scrollQueues = current.catch(() => {});
}

function openInTargetBrowser(url, sourceUrl, callback = () => {}) {
    sendNativeRequest("external_links", "open", {url, sourceUrl})
        .then(payload => callback({ok: true, payload}))
        .catch(error => {
            notifyFailure(error.message || url);
            callback({ok: false, error: error.message});
        });
}

async function configureSiteLanguages() {
    const response = await fetch(chrome.runtime.getURL("site-languages.json"));
    const config = await response.json();
    const oldRules = await chrome.declarativeNetRequest.getDynamicRules();
    const addRules = Object.entries(config).map(([language, domains], index) => ({
        id: index + 1,
        priority: 1,
        action: {
            type: "modifyHeaders",
            requestHeaders: [{
                header: "accept-language",
                operation: "set",
                value: language,
            }],
        },
        condition: {
            requestDomains: domains,
            resourceTypes: ["main_frame", "sub_frame", "xmlhttprequest"],
        },
    }));
    await chrome.declarativeNetRequest.updateDynamicRules({
        removeRuleIds: oldRules.map(rule => rule.id),
        addRules,
    });
}

function connectWorkspaceAfterRestore(workspaces, connect, bootstrap = null) {
    const accept = bootstrap === null ? Promise.resolve() : Promise.resolve(bootstrap)
        .then(payload => payload ? workspaces.acceptNativeBootstrap(payload) : undefined);
    return accept.then(() => workspaces.restoreWhenReady()).then(connect);
}

function createWorkspaceInitializer(workspaces, native, onFailure = notifyFailure) {
    let initialization = null;
    workspaces.setBrowserExecutorStatus(native.browserExecutorStatus);
    native.onEvent(message => {
        if (message.service !== "native_host") return;
        if (message.event === "browser_executor_status") {
            workspaces.setBrowserExecutorStatus(message.payload);
            if (message.payload.error) onFailure(message.payload.error);
        } else if (["disconnect", "bootstrap_timeout", "reconnect_exhausted"].includes(message.event)) {
            workspaces.setBrowserExecutorStatus({phase: "unavailable",
                error: "Native browser executor is disconnected"});
        }
    });
    return function initialize() {
        if (initialization) return initialization;
        const readiness = native.browserReady().catch(() => {});
        const attempt = connectWorkspaceAfterRestore(
            workspaces,
            () => native.ensurePort(),
            native.bootstrap(),
        ).then(() => readiness).then(() => workspaces);
        const current = attempt.catch(error => {
            onFailure(error.message || "Unable to restore Abush browser workspaces");
            if (initialization === current) initialization = null;
            throw error;
        });
        initialization = current;
        return current;
    };
}

function registerWorkspaceWindowListeners(chromeApi, workspaces, ready, onFailure) {
    const run = (tabId, info) => workspaces.revokeTab(tabId, info).catch(onFailure);
    for (const [event, kind] of [["onCreated", "created"], ["onUpdated", "updated"],
        ["onMoved", "moved"], ["onRemoved", "removed"]]) {
        chromeApi.tabGroups?.[event]?.addListener(group => workspaces.presentationGroupChanged(group, kind));
    }
    chromeApi.tabs.onActivated.addListener(info =>
        Promise.resolve(workspaces.tabActivated(info)).catch(onFailure));
    chromeApi.tabs.onCreated.addListener(tab => Promise.resolve(workspaces.newPersonalCandidate(tab)).catch(onFailure));
    chromeApi.tabs.onMoved.addListener((tabId, info) => workspaces.personalTabMoved(tabId, info));
    chromeApi.tabs.onUpdated.addListener((tabId, change, tab) =>
        Promise.resolve(workspaces.personalTabChanged(tabId, change, tab)).catch(onFailure));
    chromeApi.tabs.onRemoved.addListener(tabId => workspaces.forgetPersonalCandidate(tabId));
    chromeApi.tabs.onDetached.addListener(tabId => workspaces.forgetPersonalCandidate(tabId));
    chromeApi.tabs.onAttached.addListener(tabId => workspaces.forgetPersonalCandidate(tabId));
    chromeApi.tabs.onRemoved.addListener(run);
    chromeApi.tabs.onDetached.addListener(run);
    chromeApi.tabs.onAttached.addListener(run);
    chromeApi.windows.onCreated?.addListener(() => ready().then(() => workspaces.reconcileAll())
        .catch(onFailure));
    chromeApi.tabs.onReplaced.addListener((addedTabId, removedTabId) => run(removedTabId));
}

function registerRuntimeMessageRouter(chromeApi, workspaces, ready, open = openInTargetBrowser,
        legacyControl = null) {
    chromeApi.runtime.onMessage.addListener((message, sender, sendResponse) => {
        const ownedFailure = () => ({owned: null, ready: false, ok: false, route: false});
        const respondAfterReady = async () => {
            try {
                if (message.control !== undefined) {
                    if (sender.id !== chromeApi.runtime.id || sender.tab !== undefined ||
                            sender.url !== chromeApi.runtime.getURL("popup.html")) {
                        return {ok: false, error: "Only the extension popup can authorize browser work"};
                    }
                    if (legacyControl) return await legacyControl(message, ready);
                    if (message.control === "stop") return await workspaces.setAutomation(false, ready);
                    if (message.control === "start") {
                        await ready();
                        return await workspaces.setAutomation(true);
                    }
                    if (message.control === "recover") {
                        await ready();
                        return await workspaces.recoverPreparation(message.remote_key, message.confirmed);
                    }
                    if (message.control === "status") {
                        await ready();
                        return workspaces.popupState();
                    }
                    throw new Error("Only global Start/Stop automation controls are supported");
                }
                await ready();
                const owned = workspaces.ownsTab(sender.tab?.id);
                const generation = workspaces.ownershipGeneration;
                if (message.query === "abush_owned_tab") {
                    return {owned, generation, ready: true, ok: true, route: false};
                }
                if (!message.url) return {owned, generation, ready: true, ok: false, route: false};
                if (owned) return {owned: true, generation, ready: true, ok: true, route: false};
                return await new Promise(resolve => {
                    open(message.url, message.sourceUrl || sender.tab?.url || "", response => {
                        resolve({...response, owned: false, generation, ready: true,
                            route: response.ok});
                    });
                });
            } catch (error) {
                return {...ownedFailure(), error: error.message, code: error.browserCode};
            }
        };
        respondAfterReady().then(sendResponse, () => sendResponse(ownedFailure()));
        return true;
    });
}

function registerNativeEventRouter(native, workspaces, ready, onFailure = notifyFailure) {
    const run = (task, fallback) => ready().then(task).then(result => {
        if (result === false) throw new Error(fallback);
        return result;
    }).catch(error => onFailure(error.message || fallback));
    return native.onEvent(message => {
        if (message.service === "native_host") {
            if (message.event === "browser_executor_status") return;
            if (message.event === "bootstrap") {
                workspaces.acceptNativeBootstrap(message.payload)
                    .then(() => workspaces.restoreWhenReady())
                    .then(() => ready())
                    .catch(error => onFailure(error.message ||
                        "Unable to accept native browser bootstrap"));
                return;
            }
            // Only transport-loss lifecycle events invalidate remote connections.
            // A single request timeout rejects its own caller and must not wedge connected remotes.
            if (!["disconnect", "bootstrap_timeout", "bootstrap_request_failed",
                "reconnect_exhausted"].includes(message.event)) {
                return;
            }
            workspaces.invalidateRemote().catch(error => onFailure(error.message));
            if (message.event === "reconnect_exhausted") {
                onFailure("Native browser integration could not reconnect");
            }
            return;
        }
        if (message.service === "ordinary_open" &&
                ["ordinary_open", "ordinary_open_bootstrap"].includes(message.event)) {
            run(() => workspaces.ordinaryOpen(message.payload), "Unable to open ordinary URL");
            return;
        }
        if (message.service === "ordinary_open" && message.event === "ordinary_open_cancel") {
            try { workspaces.cancelOrdinaryOpen(message.payload); }
            catch (error) { onFailure(error.message); }
            return;
        }
        if (message.service !== "abush_browser") return;
        if (message.event === "abush_browser.engine_lease") {
            try { workspaces.engineLease(message.payload); }
            catch (error) { onFailure(error.message); }
            return;
        }
        if (message.event === "abush_browser.engine_cdp") {
            try { workspaces.engineCdp(message.payload); }
            catch (error) { onFailure(error.message); }
            return;
        }
        const methods = {"abush_browser.remote_state": "remoteState",
            "abush_browser.execute": "execute", "abush_browser.cancel": "cancel"};
        const method = methods[message.event];
        if (!method) {
            onFailure("Unsupported browser event; protocol v10 is required");
            return;
        }
        if (method === "cancel" || method === "remoteState") {
            return Promise.resolve(workspaces[method](message.payload)).catch(error => onFailure(error.message));
        } else return run(() => workspaces[method](message.payload), `Browser ${method} failed`);
    });
}

if (typeof chrome !== "undefined") {
    configureSiteLanguages().catch(error => notifyFailure(error.message));
    const workspaces = workspace();
    const native = dispatcher();
    workspaces.onAdvisoryError = error => notifyFailure(
        error.message || "Browser state is pending retry");
    const workspaceReady = createWorkspaceInitializer(workspaces, native);
    workspaceReady().catch(() => {});
    registerNativeEventRouter(dispatcher(), workspaces, workspaceReady);
    registerWorkspaceWindowListeners(chrome, workspaces, workspaceReady, error =>
        notifyFailure(error.message || "Unable to revoke changed work tab"));
    chrome.debugger.onDetach.addListener(source => {
        if (source.tabId) {
            workspaces.debuggerDetached(source.tabId);
        }
    });
    chrome.commands.onCommand.addListener((command, tab) => {
        if (command === "scrollToTop" || command === "scrollToBottom") {
            handleScrollCommand(command, tab);
        }
    });

    registerRuntimeMessageRouter(chrome, workspaces, workspaceReady);
}

if (typeof module !== "undefined") {
    module.exports = {NativeDispatcher, ENVELOPE_VERSION, workspace,
        connectWorkspaceAfterRestore, createWorkspaceInitializer, registerNativeEventRouter,
        registerWorkspaceWindowListeners, registerRuntimeMessageRouter};
}
