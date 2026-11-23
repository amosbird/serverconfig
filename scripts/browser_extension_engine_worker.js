"use strict";
importScripts("playwright-transport.js");

// Only transport plumbing and telemetry live here; every debugger call uses Chrome's real API.
const output = [], calls = [], errors = [];
let wake, bridge, lateRawResult, authorized = true, attached = false, tabId, otherId;
function publish(message) {
    output.push(message);
    if (wake) { const resolve = wake; wake = null; resolve(); }
}
const actualSendCommand = chrome.debugger.sendCommand.bind(chrome.debugger);
chrome.debugger.sendCommand = async (source, method, params) => {
    const call = {source, method, params, phase: "dispatched"};
    calls.push(call);
    let result;
    try {
        result = await actualSendCommand(source, method, params);
        call.phase = "completed";
    } catch (error) {
        call.phase = "failed";
        call.error = error.message;
        throw error;
    }
    if (method === "Runtime.evaluate" && params.expression?.includes("fixture-late-entered")) {
        lateRawResult = result;
    }
    return result;
};
chrome.debugger.onEvent.addListener((source, method, params) => {
    if (source.tabId === tabId && method === "Runtime.consoleAPICalled" &&
        params.args?.some(arg => arg.value === "fixture-late-entered")) {
        publish({type: "late_entered"});
    }
});
globalThis.fixture = {
    async setup() {
        tabId = (await chrome.tabs.create({url: "about:blank#work", active: false})).id;
        otherId = (await chrome.tabs.create({url: "about:blank#other", active: false})).id;
        await new Promise((resolve, reject) => {
            const timer = setTimeout(() => {
                chrome.tabs.onUpdated.removeListener(updated);
                reject(new Error("Blank control document did not commit"));
            }, 2000);
            function updated(id, change) {
                if (id === otherId && change.status === "complete") {
                    clearTimeout(timer); chrome.tabs.onUpdated.removeListener(updated); resolve();
                }
            }
            chrome.tabs.onUpdated.addListener(updated);
            chrome.tabs.get(otherId).then(tab => {
                if (tab.status === "complete") updated(otherId, {status: "complete"});
            }, error => { clearTimeout(timer); chrome.tabs.onUpdated.removeListener(updated); reject(error); });
        });
        await chrome.debugger.attach({tabId}, "1.3");
        attached = true;

        let rawVersionError;
        try { await actualSendCommand({tabId}, "Browser.getVersion", {}); }
        catch (error) { rawVersionError = error.message; }
        const target = await actualSendCommand({tabId}, "Target.getTargetInfo", {});
        bridge = new AuthorizedCdpBridge({chrome, tabId,
            checkAuthorization: () => authorized,
            send: message => publish({type: "cdp", message}),
            onError: error => errors.push(error.message)});
        const otherTarget = (await chrome.debugger.getTargets()).find(target => target.tabId === otherId);
        return {tabId, otherId, otherTargetId: otherTarget.id, target, rawVersionError,
            userAgent: navigator.userAgent};
    },
    async next() {
        if (!output.length) await new Promise(resolve => { wake = resolve; });
        return output.splice(0);
    },
    handle(message) { return bridge.handle(message); },
    async pageState() {
        const reply = await actualSendCommand({tabId}, "Runtime.evaluate", {
            expression: "({visibility:document.visibilityState,focused:document.hasFocus()})",
            returnByValue: true,
        });
        if (reply.exceptionDetails) throw new Error(reply.exceptionDetails.text);
        return reply.result.value;
    },
    async releasedPageState() {
        const results = await chrome.scripting.executeScript({target: {tabId}, func: () => ({
            visibility: document.visibilityState, focused: document.hasFocus(),
        })});
        return results[0].result;
    },
    state() {
        return {calls, errors, lateRawResult, closed: bridge.closed,
            rootSessionId: bridge.rootSessionId};
    },
    async tabs() { return Promise.all([chrome.tabs.get(tabId), chrome.tabs.get(otherId)]); },
    async personal() {
        return (await chrome.tabs.query({})).filter(tab => tab.id !== tabId).map(tab => ({
            id: tab.id, windowId: tab.windowId, index: tab.index, active: tab.active,
            pinned: tab.pinned, groupId: tab.groupId, url: tab.url,
        }));
    },
    async release() {
        bridge?.close();
        if (attached) { await chrome.debugger.detach({tabId}); attached = false; }
    },
    async late(message) { await bridge.handle(message); publish({type: "late_completed"}); },
    async revokeAndResolve() {
        authorized = false;
        await actualSendCommand({tabId}, "Runtime.evaluate", {
            expression: "globalThis.fixtureLateResolve('fixture-secret')"});
    },
    stop() { publish({type: "stop"}); },
};
console.log("fixture-worker-ready");
