"use strict";
importScripts("background.js");

// Native messaging plumbing only: all Chrome APIs and production reducers stay real.
const output = [], calls = [], errors = [], frames = [], native = [], expectedStagingErrors = [];
const remoteKey = "r_12345678";
const stagingAdvisories = [];
let wake, executor, chainDispatcher, ready, workTab = null, personal = [], frameTail = Promise.resolve();
let sequence = 0;
const registrations = [];
const localStateChanges = event();
function enabledReady() {
    return Boolean(executor.remotes.get(remoteKey)?.connected &&
        executor.records.get(remoteKey)?.agreed && executor.state(remoteKey).phase === "enabled");
}
function localCommit(phase) {
    frame(phase);
    localStateChanges.emit();
}
const addRuntimeListener = chrome.runtime.onMessage.addListener.bind(chrome.runtime.onMessage);
chrome.runtime.onMessage.addListener = listener => {
    registrations.push([chrome.runtime.onMessage, listener]);
    addRuntimeListener(listener);
};
function publish(message) {
    output.push(message);
    if (wake) { const resolve = wake; wake = null; resolve(); }
}
function event() {
    const listeners = new Set();
    return {addListener: fn => listeners.add(fn), removeListener: fn => listeners.delete(fn),
        emit: (...args) => { for (const fn of [...listeners]) fn(...args); }};
}
function tabView(tab) {
    return {id: tab.id, windowId: tab.windowId, index: tab.index, active: tab.active,
        pinned: tab.pinned, groupId: tab.groupId, url: tab.url};
}
const actualTabMessage = chrome.tabs.sendMessage.bind(chrome.tabs);
chrome.tabs.sendMessage = async (tabId, message) => {
    const tab = await chrome.tabs.get(tabId);
    try { return await actualTabMessage(tabId, message); }
    catch (error) {
        if (tab.url?.startsWith("about:blank#abush-work-") &&
                message.type === "abush_ownership_changed" &&
                error.message === "Could not establish connection. Receiving end does not exist.") {
            expectedStagingErrors.push(error.message);
            stagingAdvisories.push({tabId, url: tab.url, error: error.message});
        }
        throw error;
    }
};
function failure(error) {
    const message = error.message || String(error);
    const index = expectedStagingErrors.indexOf(message);
    if (index >= 0) expectedStagingErrors.splice(index, 1);
    else errors.push(message);
    publish({kind: "diagnostic", phase: "error", error: error.message || String(error), stack: error.stack});
}
function frame(phase) {
    const state = executor ? structuredClone(executor.state(remoteKey)) : null;
    const leases = executor ? [...executor.engineSessions.values()].map(value => value.lease_id) : [];
    const number = ++sequence;
    frameTail = frameTail.then(async () => {
        const tabs = (await chrome.tabs.query({})).map(tabView);
        frames.push({phase, sequence: number, state, tabs, leases});
    }).catch(failure);
}
for (const [areaName, names] of Object.entries({
    tabs: ["create", "update", "move", "remove", "group", "ungroup"],
    tabGroups: ["update"], windows: ["create", "update", "remove"],
    debugger: ["attach", "detach", "sendCommand"],
})) {
    const area = chrome[areaName];
    for (const name of names) {
        const actual = area[name].bind(area);
        area[name] = async (...args) => {
            const call = {name: `${areaName}.${name}`, method: name === "sendCommand" ? args[1] : null,
                source: name === "sendCommand" ? args[0] : null};
            calls.push(call);
            publish({kind: "diagnostic", phase: "api_enter", ...call});
            if (name === "sendCommand" && args[1] === "Runtime.callFunctionOn" &&
                    JSON.stringify(args[2]).includes("Never appears")) publish({kind: "wait_entered"});
            try {
                const result = await actual(...args);
                if (areaName === "tabs" && name === "create" &&
                        args[0].url?.startsWith("about:blank#abush-work-")) {
                    if (workTab !== null) throw new Error("More than one creation-time work tab");
                    if (args[0].active !== false) throw new Error("Work tab must be inactive");
                    workTab = result.id;
                }
                frame(`api_result:${areaName}.${name}:${call.method || ""}`);
                return result;
            } catch (error) {
                call.error = error.message;
                publish({kind: "diagnostic", phase: "api_error", ...call});
                throw error;
            }
        };
    }
    for (const [name, value] of Object.entries(area)) {
        if (!name.startsWith("on") || !value?.addListener) continue;
        const actual = value.addListener.bind(value);
        value.addListener = listener => { registrations.push([value, listener]); actual(listener); };
    }
}
const port = {onMessage: event(), onDisconnect: event(), disconnect() {},
    postMessage(message) {
        native.push({direction: "request", operation: message.operation, payload: message.payload});
        publish({kind: "native", message});
        frame(`native_request:${message.operation}`);
    }};
function routeWorker() {
    for (const [source, listener] of registrations.splice(0)) source.removeListener(listener);
    executor = new WorkspaceExecutor(chrome, (...args) => chainDispatcher.request(...args));
    executor.onAdvisoryError = failure;
    for (const name of ["remoteState", "publish", "reconcile"]) {
        const actual = executor[name].bind(executor);
        executor[name] = async (...args) => {
            try { return await actual(...args); }
            finally { localCommit(`local_commit:${name}`); }
        };
    }
    ready = createWorkspaceInitializer(executor, chainDispatcher, failure);
    registerWorkspaceWindowListeners(chrome, executor, ready, failure);
    registerRuntimeMessageRouter(chrome, executor, ready);
    chrome.debugger.onDetach.addListener(source => {
        if (source.tabId) executor.debuggerDetached(source.tabId);
    });
}
async function snapshot() {
    await frameTail;
    const tabs = (await chrome.tabs.query({})).map(tabView);
    for (const original of personal) {
        const current = tabs.find(tab => tab.id === original.id);
        if (JSON.stringify(current) !== JSON.stringify(original)) {
            throw new Error(`Personal tab changed: ${JSON.stringify({original, current})}`);
        }
    }
    if (workTab !== null && tabs.find(tab => tab.id === workTab)?.active !== false) {
        throw new Error("Work tab became active");
    }
    const local = await chrome.storage.local.get([WAL_KEY, RECEIPTS_KEY]);
    const session = await chrome.storage.session.get(CONSENT_KEY);
    const receiptLost = native.some(item => item.event === "abush_browser.remote_state" &&
        item.payload.connected === false);
    const expectedReceiptErrors = receiptLost ? errors.filter(message =>
        /receipt timed out|remote is unavailable/.test(message)) : [];
    return {state: executor.state(remoteKey), tabs, consent: session[CONSENT_KEY],
        wal: local[WAL_KEY], receipts: local[RECEIPTS_KEY], frames, calls,
        stagingAdvisories,
        errors: errors.filter(message => !expectedReceiptErrors.includes(message)), expectedReceiptErrors,
        native: native.filter(item => item.operation || item.event?.includes("completion") ||
            item.event?.includes("cancel")).map(item => ({...item, payload: undefined})),
        engineOpenCount: native.filter(item => item.operation === "engine_open").length,
        engineExecuteCount: native.filter(item => item.operation === "engine_execute").length};
}
globalThis.fixture = {
    async setup() {
        await chrome.tabs.create({url: "about:blank#personal", active: false});
        personal = (await chrome.tabs.query({})).map(tabView);
        if (personal.length !== 2) throw new Error(`Expected two isolated personal tabs, got ${personal.length}`);
        chainDispatcher = new NativeDispatcher(() => port);
        routeWorker();
        registerNativeEventRouter(chainDispatcher, new Proxy({}, {
            get: (_, name) => (...args) => executor[name](...args),
        }), () => ready(), failure);
    },
    async next() {
        if (!output.length) await new Promise(resolve => { wake = resolve; });
        return output.splice(0);
    },
    receive(message) {
        native.push({direction: "response", event: message.event, payload: message.payload});
        port.onMessage.emit(message);
        frame(`native_response:${message.event || message.reply_to}`);
    },
    async bootstrap() {
        await ready();
        return snapshot();
    },
    async ready() {
        await ready();
        try {
            await bounded(async () => {
                while (!enabledReady()) {
                    let listener;
                    try {
                        const changed = new Promise(resolve => {
                            listener = resolve;
                            localStateChanges.addListener(listener);
                            if (enabledReady()) resolve();
                        });
                        await bounded(() => changed, "next post-local-commit state signal", 14000);
                    } finally { localStateChanges.removeListener(listener); }
                }
            }, "post-local-commit connected/agreed/enabled reservation", 15000);
        } catch (error) {
            throw new Error(`${error.message}: ${JSON.stringify({
                connected: executor.remotes.get(remoteKey)?.connected,
                agreed: executor.records.get(remoteKey)?.agreed,
                state: executor.state(remoteKey), frames: frames.slice(-8), native: native.slice(-8),
            })}`, {cause: error});
        }
        frame("ready");
        return snapshot();
    },
    async control(op) {
        if (op === "snapshot") {
            const drained = await bounded(() => Promise.allSettled([...executor.executionTails.values()]),
                "execution drain", 15000);
            for (const result of drained) if (result.status === "rejected" &&
                    !/receipt timed out|Engine lease revoked|cancel/i.test(result.reason.message)) throw result.reason;
            await executor.localTail;
        } else if (op === "stop" || op === "start") {
            const response = await executor.setAutomation(op === "start", ready);
            if (response.ok !== true) throw new Error(JSON.stringify(response));
        } else if (op === "restart_worker") {
            if (executor.operations.size) throw new Error("Worker reconstruction requires drained operations");
            for (const candidate of executor.personalCandidates.values()) clearTimeout(candidate.timer);
            for (const record of executor.records.values()) await executor.detach(record);
            routeWorker();
            await ready();
            const refreshed = await chainDispatcher.request("abush_browser", "refresh_remotes", {});
            for (const remote of refreshed.remotes) await executor.remoteState(remote);
        } else if (op !== "quit") throw new Error(`Unknown worker control ${op}`);
        frame(op);
        return snapshot();
    },
    disconnected() { port.onDisconnect.emit(); },
    async stop() {
        for (const candidate of executor.personalCandidates.values()) clearTimeout(candidate.timer);
        await bounded(() => executor.inheritanceTail, "inheritance teardown", 5000);
        chainDispatcher.stop();
        publish({kind: "stop"});
    },
};
console.log("fixture-worker-ready");
