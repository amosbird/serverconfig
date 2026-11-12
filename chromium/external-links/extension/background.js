if (typeof importScripts === "function") importScripts("workspace.js");

const NATIVE_HOST = "io.github.amosbird.browser_router";
const ENVELOPE_VERSION = 1;

const NATIVE_TIMEOUTS_MS = Object.freeze({
    bind_x11_window: 15000,
    clear_x11_window: 8000,
    managed_page_moved_outside: 20000,
});
const SERIAL_NATIVE_OPERATIONS = new Set([
    "bind_x11_window", "clear_x11_window", "managed_page_moved_outside",
]);
const SERIAL_NATIVE_KEY = "native-host";

class NativeDispatcher {
    constructor(connect, timeoutMs = 3000) {
        this.connect = connect;
        this.timeoutMs = timeoutMs;
        this.port = null;
        this.requestTails = new Map();
        this.pending = new Map();
        this.eventListeners = new Set();
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 8;
        this.reconnectExhausted = false;
        this.reconnectTimer = null;
        this.stopped = false;
        this.setTimeout = setTimeout;
        this.clearTimeout = clearTimeout;
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
                this.scheduleReconnect();
                throw error;
            }
        })();
        this.port = port;
        port.onMessage.addListener(message => this.dispatch(message));
        port.onDisconnect.addListener(() => {
            if (this.port !== port) return;
            this.port = null;
            this.emitLifecycle("disconnect");
            const error = new Error("native host disconnected");
            error.nativeDispatch = "unknown";
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
        const timeoutMs = Math.max(this.timeoutMs, NATIVE_TIMEOUTS_MS[operation] || 0);
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
        this.port?.disconnect?.();
        this.port = null;
        this.rejectAll(new Error("native dispatcher stopped"));
    }

    dispatch(message) {
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

function connectWorkspaceAfterRestore(workspaces, connect) {
    return workspaces.restoreWhenReady().then(async () => {
        await connect();
        return workspaces.drainAdvisoryErrors?.() || [];
    });
}

function registerWorkspaceWindowListeners(chromeApi, workspaces, ready, onFailure) {
    const run = task => ready.then(() => workspaces.enqueueLifecycle(task)).catch(onFailure);
    chromeApi.windows.onCreated.addListener(window => {
        if (window.type !== "normal") return;
        run(async () => {
            await workspaces.tabCreated({id: null, windowId: window.id});
            await workspaces.rememberWindowSnapshot(window.id);
        });
    });
    chromeApi.windows.onFocusChanged.addListener(windowId => {
        run(() => workspaces.recordOrdinaryFocus(windowId));
    });
    chromeApi.windows.onRemoved.addListener(windowId => {
        run(() => workspaces.windowRemoved(windowId));
    });
    chromeApi.tabs.onCreated.addListener(tab => {
        run(async () => {
            await workspaces.tabCreated(tab);
            await workspaces.rememberWindowSnapshot(tab.windowId);
        });
    });
    chromeApi.tabs.onUpdated.addListener((tabId, changeInfo, tab) => {
        run(async () => {
            await workspaces.tabUpdated(tabId, changeInfo, tab);
            await workspaces.rememberWindowSnapshot(tab.windowId);
        });
    });
    chromeApi.tabs.onMoved.addListener((tabId, info) => {
        run(async () => {
            await workspaces.tabMoved(tabId, info);
            await workspaces.rememberWindowSnapshot(info.windowId);
        });
    });
    chromeApi.tabs.onRemoved.addListener((tabId, info) => {
        run(async () => {
            await workspaces.tabRemoved(tabId, info);
            await workspaces.rememberWindowSnapshot(info.windowId);
        });
    });
    chromeApi.tabs.onAttached.addListener((tabId, info) => {
        run(async () => {
            await workspaces.tabAttached(tabId, info);
            await workspaces.rememberWindowSnapshot(info.newWindowId);
        });
    });
    chromeApi.tabs.onDetached.addListener((tabId, info) => {
        run(async () => {
            await workspaces.tabDetached(tabId, info);
            await workspaces.rememberWindowSnapshot(info.oldWindowId);
        });
    });
}

if (typeof chrome !== "undefined") {
    configureSiteLanguages();
    const workspaces = workspace();
    workspaces.onAdvisoryError = error => notifyFailure(
        error.message || "Managed browser topology report is pending retry");
    const workspaceReady = connectWorkspaceAfterRestore(
        workspaces,
        async () => {
            dispatcher().ensurePort();
            try {
                await workspaces.flushTopologyOutbox();
            } catch (error) {
                workspaces.reportAdvisoryError(error);
            }
        },
    ).then(errors => {
        if (errors.length > 0) {
            notifyFailure(errors[0].message ||
                "Managed browser topology reports are pending retry");
        }
        return workspaces;
    }).catch(error => {
        notifyFailure(error.message || "Unable to restore Abush browser workspaces");
        throw error;
    });
    dispatcher().onEvent(message => {
        if (message.service === "native_host") {
            workspaceReady.then(async () => {
                await workspaces.invalidateRemote();
                await workspaces.flushTopologyOutbox();
            }).catch(error => notifyFailure(error.message ||
                "Unable to retry managed browser topology reports"));
            if (message.event === "reconnect_exhausted") {
                notifyFailure("Native browser integration could not reconnect");
            }
            return;
        }
        if (message.service === "ordinary_open" && message.event === "ordinary_open") {
            workspaceReady.then(() => workspaces.ordinaryOpen(message.payload))
                .catch(error => notifyFailure(error.message || "Unable to open ordinary URL"));
            return;
        }
        if (message.service !== "abush_browser") return;
        if (message.event === "abush_browser.remote_state") {
            workspaceReady.then(() => workspaces.remoteState(message.payload)).catch(() => {});
            return;
        }
        if (message.event === "abush_browser.ensure_managed_page") {
            workspaceReady.then(() => workspaces.ensure(message.payload))
                .catch(error => notifyFailure(
                    error.message || "Unable to ensure Abush browser workspace"));
        }
        if (message.event === "abush_browser.retire_managed_page") {
            workspaceReady.then(() => workspaces.retireManagedPage(message.payload))
                .catch(error => notifyFailure(
                    error.message || "Unable to retire Abush browser workspace"));
        }
        if (message.event === "abush_browser.cancel_ensure_managed_page") {
            workspaceReady.then(() => workspaces.cancelEnsure(message.payload)).catch(() => {});
        }
        if (message.event === "abush_browser.execute") {
            workspaceReady.then(() => workspaces.execute(message.payload))
                .catch(error => notifyFailure(
                    error.message || "Unable to complete Abush browser command"));
        }
        if (message.event === "abush_browser.cancel") {
            workspaceReady.then(() => workspaces.cancel(message.payload)).catch(() => {});
        }
    });
    registerWorkspaceWindowListeners(chrome, workspaces, workspaceReady, error =>
        notifyFailure(error.message || "Unable to retire closed Abush browser workspace"));
    chrome.debugger.onDetach.addListener(source => {
        if (source.tabId) {
            workspaceReady.then(() => workspaces.debuggerDetached(source.tabId)).catch(() => {});
        }
    });
    chrome.commands.onCommand.addListener((command, tab) => {
        if (command === "scrollToTop" || command === "scrollToBottom") {
            handleScrollCommand(command, tab);
        }
    });

    chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
        if (message.query === "abush_owned_window") {
            sendResponse({owned: workspace().ownsWindow(sender.tab?.windowId)});
            return;
        }
        if (!message.url) return;
        if (workspace().ownsWindow(sender.tab?.windowId)) {
            sendResponse({ok: true, route: false});
            return;
        }
        openInTargetBrowser(message.url, message.sourceUrl || sender.tab?.url || "", response => {
            sendResponse({...response, route: response.ok});
        });
        return true;
    });
}

if (typeof module !== "undefined") {
    module.exports = {NativeDispatcher, ENVELOPE_VERSION, workspace,
        connectWorkspaceAfterRestore, registerWorkspaceWindowListeners};
}
