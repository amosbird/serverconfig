if (typeof importScripts === "function") importScripts("workspace.js");

const NATIVE_HOST = "io.github.amosbird.browser_router";
const ENVELOPE_VERSION = 1;

class NativeDispatcher {
    constructor(connect, timeoutMs = 3000) {
        this.connect = connect;
        this.timeoutMs = timeoutMs;
        this.port = null;
        this.pending = new Map();
        this.eventListeners = new Set();
    }

    ensurePort() {
        if (this.port) return this.port;
        const port = this.connect();
        this.port = port;
        port.onMessage.addListener(message => this.dispatch(message));
        port.onDisconnect.addListener(() => {
            if (this.port !== port) return;
            this.port = null;
            this.rejectAll(new Error("native host disconnected"));
        });
        return port;
    }

    request(service, operation, payload = {}) {
        const id = crypto.randomUUID();
        return new Promise((resolve, reject) => {
            const timer = setTimeout(() => {
                if (!this.pending.delete(id)) return;
                reject(new Error("native request timed out"));
            }, this.timeoutMs);
            this.pending.set(id, {resolve, reject, timer});
            try {
                this.ensurePort().postMessage({
                    version: ENVELOPE_VERSION,
                    id,
                    service,
                    operation,
                    payload,
                });
            } catch (error) {
                this.finish(id, false, error);
            }
        });
    }

    dispatch(message) {
        if (!message || typeof message !== "object") return;
        if (typeof message.reply_to === "string" && this.pending.has(message.reply_to) &&
                message.version !== ENVELOPE_VERSION) {
            this.finish(message.reply_to, false, new Error("mismatched native response version"));
            return;
        }
        if (message.version !== ENVELOPE_VERSION) return;
        if (typeof message.event === "string" && typeof message.service === "string" &&
                message.payload && typeof message.payload === "object") {
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
        clearTimeout(pending.timer);
        if (ok) pending.resolve(value);
        else pending.reject(value instanceof Error ? value : new Error(String(value)));
    }

    rejectAll(error) {
        for (const id of [...this.pending.keys()]) this.finish(id, false, error);
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

if (typeof chrome !== "undefined") {
    configureSiteLanguages();
    dispatcher().ensurePort();
    workspace().restore();
    dispatcher().onEvent(message => {
        if (message.service !== "abush_browser") return;
        if (message.event === "abush_browser.execute") workspace().execute(message.payload);
        if (message.event === "abush_browser.cancel") workspace().cancel(message.payload);
    });
    chrome.tabs.onAttached.addListener((tabId, info) => workspace().tabMoved(tabId, info));
    chrome.debugger.onDetach.addListener(source => {
        if (source.tabId) workspace().debuggerDetached(source.tabId).catch(() => {});
    });
    chrome.action.onClicked.addListener(async () => {
        try {
            const {remotes} = await sendNativeRequest("abush_browser", "refresh_remotes", {});
            const remote = remotes.find(item => item.connected);
            if (!remote) return notifyFailure("No active Abush browser remote");
            await workspace().open(remote.remote_key, remote.connection_id);
        } catch (error) {
            notifyFailure(error.message);
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
    module.exports = {NativeDispatcher, ENVELOPE_VERSION, workspace};
}
