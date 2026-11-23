"use strict";
importScripts("base-worker.js");

// Diagnostic baseline bypasses only capture preparation, never target authorization.
fixture.rawCapture = params => {
    if (!authorized || !attached || bridge.closed) throw new Error("Fixture authorization revoked");
    return chrome.debugger.sendCommand({tabId}, "Page.captureScreenshot", params);
};
fixture.matrixState = () => ({tabId, rootSessionId: bridge.rootSessionId,
    calls: calls.filter(call => /captureScreenshot|Screencast|Emulation\./.test(call.method)),
    forbidden: calls.filter(call => /bringToFront|activateTarget|setWindow|setViewport|setDeviceMetrics/.test(call.method))});
