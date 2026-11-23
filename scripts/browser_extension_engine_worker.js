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
    calls.push({source, method, params});
    const result = await actualSendCommand(source, method, params);
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
    async setup(origin) {
        tabId = (await chrome.tabs.create({url: `${origin}/owned`, active: false})).id;
        otherId = (await chrome.tabs.create({url: `${origin}/other`, active: false})).id;
        await chrome.debugger.attach({tabId}, "1.3");
        attached = true;
        await Promise.all([tabId, otherId].map(id => new Promise((resolve, reject) => {
            const timer = setTimeout(() => {
                chrome.tabs.onUpdated.removeListener(updated);
                reject(new Error("Fixture tab load deadline"));
            }, 10000);
            function updated(updatedId, change) {
                if (updatedId === id && change.status === "complete") {
                    clearTimeout(timer); chrome.tabs.onUpdated.removeListener(updated); resolve();
                }
            }
            chrome.tabs.onUpdated.addListener(updated);
            function fail(error) {
                clearTimeout(timer); chrome.tabs.onUpdated.removeListener(updated); reject(error);
            }
            chrome.tabs.get(id).then(tab => {
                if (tab.status === "complete") updated(id, {status: "complete"});
            }, fail);
        })));
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
    state() {
        return {calls, errors, lateRawResult, closed: bridge.closed,
            rootSessionId: bridge.rootSessionId};
    },
    async tabs() { return Promise.all([chrome.tabs.get(tabId), chrome.tabs.get(otherId)]); },
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
