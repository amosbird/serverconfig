if (typeof importScripts === "function") importScripts("workspace.js");

const NATIVE_HOST = "io.github.amosbird.browser_router";
const ENVELOPE_VERSION = 1;

const NATIVE_TIMEOUTS_MS = Object.freeze({
    native_bootstrap: 15000,
    external_links_open: 30000,
    bind_x11_window: 15000,
    verify_x11_window: 8000,
    clear_x11_window: 8000,
    managed_page_moved_outside: 20000,
    topology_response_consumed: 20000,
    retire_managed_page_completion: 20000,
    retire_managed_page_ack_consumed: 20000,
});
const SERIAL_NATIVE_OPERATIONS = new Set([
    "bind_x11_window", "verify_x11_window", "clear_x11_window",
    "managed_page_moved_outside",
    "topology_response_consumed", "retire_managed_page_completion",
    "retire_managed_page_ack_consumed",
]);
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
        this.bootstrapGeneration = 0;
        this.bootstrapAttempt = null;
        this.bootstrapWaiters = [];
        this.setTimeout = setTimeout;
        this.clearTimeout = clearTimeout;
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
                (attempt.settled && attempt.outcome !== "ready")) {
            attempt = this.createBootstrapAttempt(port);
        }
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
        if (this.bootstrapEnabled) this.createBootstrapAttempt(port);
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

    dispatchRequest(service, operation, payload = {}) {
        const id = crypto.randomUUID();
        let timeoutMs = Math.max(this.timeoutMs, NATIVE_TIMEOUTS_MS[operation] || 0);
        if (service === "external_links" && operation === "open") {
            timeoutMs = NATIVE_TIMEOUTS_MS.external_links_open;
        }
        if (service === "ordinary_open" && Number.isSafeInteger(payload.deadline_ms)) {
            timeoutMs = Math.max(1, payload.deadline_ms - Date.now());
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
                const port = this.ensurePort();
                dispatchStarted = true;
                port.postMessage({
                    version: ENVELOPE_VERSION,
                    id,
                    service,
                    operation,
                    payload,
                });
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
    return accept.then(() => workspaces.restoreWhenReady()).then(async () => {
        await connect();
        return workspaces.drainAdvisoryErrors?.() || [];
    });
}

function createWorkspaceInitializer(workspaces, native, onFailure = notifyFailure) {
    let initialization = null;
    return function initialize() {
        if (initialization) return initialization;
        const attempt = connectWorkspaceAfterRestore(
            workspaces,
            async () => {
                native.ensurePort();
                try {
                    await workspaces.flushTopologyOutbox();
                } catch (error) {
                    workspaces.reportAdvisoryError(error);
                }
            },
            native.bootstrap(),
        ).then(errors => {
            if (errors.length > 0) {
                onFailure(errors[0].message ||
                    "Managed browser topology reports are pending retry");
            }
            return workspaces;
        });
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
    const getReady = typeof ready === "function" ? ready : () => ready;
    const run = task => getReady().then(() => workspaces.enqueueLifecycle(task)).catch(onFailure);
    const observe = (type, payload) => workspaces.observeChromeEvent?.(type, payload) === true;
    chromeApi.windows.onCreated.addListener(window => {
        if (window.type !== "normal") return;
        Promise.resolve(workspaces.observeWindowCreated?.(window)).then(owned => {
            if (owned || observe("windowCreated", {window})) return;
            run(async () => {
                await workspaces.tabCreated({id: null, windowId: window.id});
                await workspaces.rememberWindowSnapshot(window.id);
            });
        }).catch(onFailure);
    });
    chromeApi.windows.onFocusChanged.addListener(windowId => {
        run(() => workspaces.recordOrdinaryFocus(windowId));
    });
    chromeApi.windows.onRemoved.addListener(windowId => {
        workspaces.freezeClosingWindow?.(windowId);
        run(() => workspaces.windowRemoved(windowId));
    });
    chromeApi.tabs.onCreated.addListener(tab => {
        if (observe("tabCreated", {tab})) return;
        run(async () => {
            await workspaces.tabCreated(tab);
            await workspaces.rememberWindowSnapshot(tab.windowId);
        });
    });
    chromeApi.tabs.onUpdated.addListener((tabId, changeInfo, tab) => {
        if (observe("tabUpdated", {tabId, changeInfo, tab})) return;
        run(async () => {
            await workspaces.tabUpdated(tabId, changeInfo, tab);
            await workspaces.rememberWindowSnapshot(tab.windowId);
        });
    });
    chromeApi.tabs.onMoved.addListener((tabId, info) => {
        if (observe("tabMoved", {tabId, ...info})) return;
        run(async () => {
            await workspaces.tabMoved(tabId, info);
            await workspaces.rememberWindowSnapshot(info.windowId);
        });
    });
    chromeApi.tabs.onRemoved.addListener((tabId, info) => {
        if (info?.isWindowClosing) workspaces.freezeClosingWindow?.(info.windowId);
        if (observe("tabRemoved", {tabId, ...info})) return;
        run(async () => {
            await workspaces.tabRemoved(tabId, info);
            if (!info?.isWindowClosing) await workspaces.rememberWindowSnapshot(info.windowId);
        });
    });
    chromeApi.tabs.onAttached.addListener((tabId, info) => {
        if (observe("tabAttached", {tabId, ...info})) return;
        run(async () => {
            await workspaces.tabAttached(tabId, info);
            await workspaces.rememberWindowSnapshot(info.newWindowId);
        });
    });
    chromeApi.tabs.onDetached.addListener((tabId, info) => {
        if (observe("tabDetached", {tabId, ...info})) return;
        run(async () => {
            await workspaces.tabDetached(tabId, info);
            await workspaces.rememberWindowSnapshot(info.oldWindowId);
        });
    });
    if (chromeApi.tabs.onReplaced?.addListener) {
        chromeApi.tabs.onReplaced.addListener((addedTabId, removedTabId) => {
            if (observe("tabReplaced", {addedTabId, removedTabId})) return;
            run(() => workspaces.tabReplaced(addedTabId, removedTabId));
        });
    }
}

function registerRuntimeMessageRouter(chromeApi, workspaces, ready, open = openInTargetBrowser) {
    chromeApi.runtime.onMessage.addListener((message, sender, sendResponse) => {
        const ownedFailure = () => ({owned: null, ready: false, ok: false, route: false});
        const respondAfterReady = async () => {
            try {
                await ready();
                const owned = workspaces.ownsWindow(sender.tab?.windowId);
                if (message.query === "abush_owned_window") {
                    return {owned, ready: true, ok: true, route: false};
                }
                if (!message.url) return {owned, ready: true, ok: false, route: false};
                if (owned) return {owned: true, ready: true, ok: true, route: false};
                return await new Promise(resolve => {
                    open(message.url, message.sourceUrl || sender.tab?.url || "", response => {
                        resolve({...response, owned: false, ready: true, route: response.ok});
                    });
                });
            } catch (_) {
                return ownedFailure();
            }
        };
        respondAfterReady().then(sendResponse, () => sendResponse(ownedFailure()));
        return true;
    });
}

if (typeof chrome !== "undefined") {
    configureSiteLanguages();
    const workspaces = workspace();
    const native = dispatcher();
    workspaces.onAdvisoryError = error => notifyFailure(
        error.message || "Managed browser topology report is pending retry");
    const workspaceReady = createWorkspaceInitializer(workspaces, native);
    workspaceReady().catch(() => {});
    dispatcher().onEvent(message => {
        if (message.service === "native_host") {
            if (message.event === "bootstrap") {
                workspaces.acceptNativeBootstrap(message.payload)
                    .then(() => workspaceReady())
                    .catch(error => notifyFailure(error.message ||
                        "Unable to accept native browser bootstrap"));
                return;
            }
            workspaceReady().then(async () => {
                await workspaces.invalidateRemote();
                await workspaces.flushTopologyOutbox();
            }).catch(error => notifyFailure(error.message ||
                "Unable to retry managed browser topology reports"));
            if (message.event === "reconnect_exhausted") {
                notifyFailure("Native browser integration could not reconnect");
            }
            return;
        }
        if (message.service === "ordinary_open" &&
                ["ordinary_open", "ordinary_open_bootstrap"].includes(message.event)) {
            workspaceReady().then(() => workspaces.ordinaryOpen(message.payload))
                .catch(error => notifyFailure(error.message || "Unable to open ordinary URL"));
            return;
        }
        if (message.service === "ordinary_open" && message.event === "ordinary_open_cancel") {
            workspaceReady().then(() => workspaces.cancelOrdinaryOpen(message.payload))
                .catch(() => {});
            return;
        }
        if (message.service !== "abush_browser") return;
        if (message.event === "abush_browser.remote_state") {
            workspaceReady().then(() => workspaces.remoteState(message.payload)).catch(() => {});
            return;
        }
        if (message.event === "abush_browser.ensure_managed_page") {
            workspaceReady().then(() => workspaces.ensure(message.payload))
                .catch(error => notifyFailure(
                    error.message || "Unable to ensure Abush browser workspace"));
            return;
        }
        if (message.event === "abush_browser.retire_managed_page") {
            workspaceReady().then(() => workspaces.retireManagedPage(message.payload))
                .catch(error => notifyFailure(
                    error.message || "Unable to retire Abush browser workspace"));
            return;
        }
        if (message.event === "abush_browser.topology_report_response") {
            workspaceReady().then(() => workspaces.topologyReportResponse(message.payload))
                .catch(error => notifyFailure(
                    error.message || "Unable to consume managed topology response"));
            return;
        }
        if (message.event === "abush_browser.retire_managed_page_ack") {
            workspaceReady().then(() => workspaces.retireManagedPageAck(message.payload))
                .catch(error => notifyFailure(
                    error.message || "Unable to consume managed retire acknowledgement"));
            return;
        }
        if (message.event === "abush_browser.cancel_ensure_managed_page") {
            workspaceReady().then(() => workspaces.cancelEnsure(message.payload)).catch(() => {});
            return;
        }
        if (message.event === "abush_browser.execute") {
            workspaceReady().then(async () => {
                if (!await workspaces.registerOperation(message.payload)) return;
                await workspaces.executeRegistered(message.payload);
            }).catch(error => notifyFailure(
                error.message || "Unable to complete Abush browser command"));
            return;
        }
        if (message.event === "abush_browser.cancel") {
            workspaceReady().then(() => workspaces.cancel(message.payload)).catch(() => {});
        }
    });
    registerWorkspaceWindowListeners(chrome, workspaces, workspaceReady, error =>
        notifyFailure(error.message || "Unable to retire closed Abush browser workspace"));
    chrome.debugger.onDetach.addListener(source => {
        if (source.tabId) {
            workspaceReady().then(() => workspaces.debuggerDetached(source.tabId)).catch(() => {});
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
        connectWorkspaceAfterRestore, createWorkspaceInitializer,
        registerWorkspaceWindowListeners, registerRuntimeMessageRouter};
}
