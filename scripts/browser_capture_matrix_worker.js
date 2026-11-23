"use strict";
importScripts("base-worker.js");

// Diagnostic-only direct calls deliberately bypass the production method allowlist.
const matrixCalls = [];
const matrixSend = chrome.debugger.sendCommand.bind(chrome.debugger);
chrome.debugger.sendCommand = async (source, method, params) => {
    const call = {source, logicalSessionId: bridge?.rootSessionId || null,
        method, params, dispatchMs: performance.now(), phase: "pending"};
    matrixCalls.push(call);
    try {
        const result = await matrixSend(source, method, params);
        call.phase = "completed";
        return result;
    } catch (error) {
        call.phase = "failed";
        call.error = error.message;
        throw error;
    } finally { call.completionMs = performance.now(); }
};
fixture.raw = (method, params) => chrome.debugger.sendCommand({tabId}, method, params);
fixture.matrixState = () => ({tabId, rootSessionId: bridge.rootSessionId,
    calls: matrixCalls.filter(call => /captureScreenshot|Emulation\./.test(call.method)),
    forbidden: matrixCalls.filter(call => /bringToFront|activateTarget|setWindow|setViewport/.test(call.method))});
