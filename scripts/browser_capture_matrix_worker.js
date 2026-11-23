"use strict";
importScripts("base-worker.js");

// Diagnostic-only direct calls deliberately bypass the production method allowlist.
const matrixCalls = [];
let capturePreparation = null;
const matrixSend = chrome.debugger.sendCommand.bind(chrome.debugger);
let screencastFrame;
chrome.debugger.onEvent.addListener((source, method, params) => {
    if (source.tabId === tabId && method === "Page.screencastFrame") {
        matrixSend(source, "Page.screencastFrameAck", {sessionId: params.sessionId}).catch(error => errors.push(error.message));
        screencastFrame?.();
    }
});
chrome.debugger.sendCommand = async (source, method, params) => {
    const call = {source, logicalSessionId: bridge?.rootSessionId || null,
        method, params, dispatchMs: performance.now(), phase: "pending"};
    matrixCalls.push(call);
    try {
        if (method === "Page.captureScreenshot" && capturePreparation) {
            if (capturePreparation.includes("screencast")) {
                let ready;
                const frame = new Promise(resolve => { ready = resolve; });
                screencastFrame = ready;
                await matrixSend(source, "Page.startScreencast", {format: "jpeg", quality: 10, maxWidth: 16, maxHeight: 16, everyNthFrame: 1});
                await frame;
                call.screencastReadyMs = performance.now();
            }
            if (capturePreparation.includes("metrics_viewport")) {
                const observed = await matrixSend(source, "Runtime.evaluate", {expression:
                    "({width:innerWidth,height:innerHeight,dpr:devicePixelRatio})", returnByValue: true});
                await matrixSend(source, "Emulation.setDeviceMetricsOverride", {
                    width: observed.result.value.width, height: observed.result.value.height,
                    deviceScaleFactor: 0, mobile: false, dontSetVisibleSize: true,
                    viewport: {x: 0, y: 0, width: params.clip.width, height: params.clip.height, scale: 1},
                });
            }
            if (!capturePreparation.includes("screencast")) {
                const prepared = await matrixSend(source, "Runtime.evaluate", {
                    expression: "new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(() => resolve(true))))",
                    awaitPromise: true, returnByValue: true,
                });
                if (prepared.exceptionDetails || prepared.result.value !== true) throw new Error("Paint preparation failed");
            }
            call.preparedMs = performance.now();
        }
        const captureParams = method === "Page.captureScreenshot" && capturePreparation?.includes("no_beyond") ?
            {...params, captureBeyondViewport: false} : params;
        const result = await matrixSend(source, method, captureParams);
        if (method === "Page.captureScreenshot" && capturePreparation?.includes("screencast")) {
            await matrixSend(source, "Page.stopScreencast", {});
            screencastFrame = null;
        }
        call.phase = "completed";
        return result;
    } catch (error) {
        call.phase = "failed";
        call.error = error.message;
        throw error;
    } finally { call.completionMs = performance.now(); }
};
fixture.prepareCapture = mode => { capturePreparation = mode; };
fixture.raw = (method, params) => chrome.debugger.sendCommand({tabId}, method, params);
fixture.matrixState = () => ({tabId, rootSessionId: bridge.rootSessionId,
    calls: matrixCalls.filter(call => /captureScreenshot|Emulation\./.test(call.method)),
    forbidden: matrixCalls.filter(call => /bringToFront|activateTarget|setWindow|setViewport/.test(call.method))});
