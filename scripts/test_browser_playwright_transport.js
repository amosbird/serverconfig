#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const {AuthorizedCdpBridge} =
    require("../chromium/external-links/extension/playwright-transport.js");
const AUTO = {autoAttach: true, waitForDebuggerOnStart: true, flatten: true};
const FILTER = [{type: "iframe", exclude: false}, {exclude: true}];
const clone = value => JSON.parse(JSON.stringify(value));
const deferred = () => {
    let resolve;
    const promise = new Promise(done => { resolve = done; });
    return {promise, resolve};
};
function eventBus() {
    const listeners = new Set();
    return {listeners, addListener: fn => listeners.add(fn),
        removeListener: fn => listeners.delete(fn),
        emit: (...args) => { for (const fn of [...listeners]) fn(...args); }};
}
function fixture(send) {
    const f = {calls: [], sent: [], errors: [], on: true, hooks: {}, nextId: 0};
    f.chrome = {debugger: {onEvent: eventBus(), onDetach: eventBus(),
        sendCommand: async (source, method, params) => {
            f.calls.push(clone({source, method, params}));
            assert.equal(source.tabId, 17);
            if (f.hooks[method]) return f.hooks[method](source, params);
            if (method.startsWith("Browser.")) throw new Error(`'${method}' wasn't found`);
            if (method === "Target.getTargetInfo") return {targetInfo: {targetId: "page-exact",
                type: "page", title: "fixture", url: "https://fixture.test/", attached: true,
                browserContextId: "context-exact"}};
            return {};
        }}};
    f.bridge = new AuthorizedCdpBridge({chrome: f.chrome, tabId: 17,
        checkAuthorization: () => f.on, onError: error => f.errors.push(error),
        send: message => { f.sent.push(clone(message)); return send?.(message); }});
    f.request = async (method, params = {}, sessionId) => {
        const id = ++f.nextId;
        await f.bridge.handle({id, method, params, ...(sessionId ? {sessionId} : {})});
        return f.sent.find(message => message.id === id);
    };
    f.boot = async () => {
        await f.request("Target.setAutoAttach", AUTO);
        return f.sent.find(message => message.method === "Target.attachedToTarget").params.sessionId;
    };
    f.event = (method, params, sessionId, tabId = 17) =>
        f.chrome.debugger.onEvent.emit({tabId, ...(sessionId ? {sessionId} : {})}, method, params);
    f.attach = (id, type = "iframe", parent, tabId = 17) => f.event("Target.attachedToTarget",
        {sessionId: id, targetInfo: {type, targetId: `target-${id}`, url: "https://frame.test/"},
            waitingForDebugger: true}, parent, tabId);
    return f;
}
const cases = [
    ["CommonJS and importScripts-style global export", async () => {
        const context = {crypto: require("node:crypto").webcrypto};
        vm.runInNewContext(fs.readFileSync(path.join(__dirname,
            "../chromium/external-links/extension/playwright-transport.js"), "utf8"), context);
        assert.equal(typeof context.AuthorizedCdpBridge, "function");
    }],
    ["version metadata is local and never enters the unsupported debugger Browser domain", async () => {
        const f = fixture();
        const reply = await f.request("Browser.getVersion");
        assert.equal(reply.error, undefined);
        assert.equal(reply.result.protocolVersion, "1.3");
        assert.equal(reply.result.product, "Chrome/Extension-Bridge");
        assert.equal(typeof reply.result.userAgent, "string");
        assert.deepEqual(f.calls, []);
        assert.ok((await f.request("Browser.getVersion", {targetId: "foreign"})).error);
        const session = await f.boot();
        const before = f.calls.length;
        assert.ok((await f.request("Browser.getVersion", {}, session)).error);
        assert.equal(f.calls.length, before);
        f.on = false;
        const sent = f.sent.length;
        await f.request("Browser.getVersion");
        assert.equal(f.sent.length, sent);
        assert.equal(f.bridge.closed, true);
    }],
    ["official root bootstrap maps opaque page session onto exact debugger tab", async () => {
        const f = fixture();
        assert.equal(f.chrome.debugger.onEvent.listeners.size, 1);
        assert.equal(f.chrome.debugger.onDetach.listeners.size, 1);
        assert.ok((await f.request("Browser.getVersion")).result.product.startsWith("Chrome/"));
        const session = await f.boot();
        assert.match(session, /^[\da-f-]{36}$/);
        assert.notEqual(session, "page-exact");
        const attached = f.sent.find(message => message.method === "Target.attachedToTarget");
        assert.equal(attached.sessionId, undefined);
        assert.equal(attached.params.targetInfo.targetId, "page-exact");
        assert.equal(attached.params.waitingForDebugger, false);
        await f.request("Page.enable", {}, session);
        assert.deepEqual(f.calls.at(-1).source, {tabId: 17});
        const targets = await f.request("Target.getTargets");
        assert.deepEqual(targets.result.targetInfos.map(info => info.targetId), ["page-exact"]);
        assert.ok((await f.request("Target.getTargetInfo")).result.targetInfo);
        assert.ok((await f.request("Target.getTargetInfo", {targetId: "foreign"})).error);
        assert.equal(f.calls.some(call => call.method === "Target.getTargets"), false);
        f.bridge.close();
    }],
    ["root attachment prepares only the exact tab before publishing page readiness", async () => {
        const f = fixture();
        let prepared = false;
        f.hooks["Emulation.setFocusEmulationEnabled"] = async (source, params) => {
            assert.deepEqual(source, {tabId: 17});
            assert.deepEqual(params, {enabled: true});
            prepared = true;
            return {};
        };
        const session = await f.boot();
        assert.equal(prepared, true, "noDefaults requires exact-tab focus preparation");
        assert.deepEqual(f.calls.map(call => call.method),
            ["Target.getTargetInfo", "Emulation.setFocusEmulationEnabled"]);
        assert.equal(f.sent.filter(message => message.method === "Target.attachedToTarget").length, 1);
        await f.request("Target.setAutoAttach", AUTO);
        assert.equal(f.calls.length, 2, "duplicate bootstrap must not reprepare or adopt another tab");
        await f.request("Input.dispatchKeyEvent", {type: "keyDown", key: "Enter"}, session);
        assert.equal(f.calls.at(-1).source.tabId, 17);
        f.bridge.close();
    }],
    ["blocked focus preparation shares initialization and exposes no usable page", async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Emulation.setFocusEmulationEnabled"] = async () => {
            entered.resolve(); await release.promise; return {};
        };
        const first = f.request("Target.setAutoAttach", AUTO);
        await entered.promise;
        const second = f.request("Target.setAutoAttach", AUTO);
        assert.equal(f.bridge.sessions.size, 0);
        assert.equal(f.sent.some(message => message.method === "Target.attachedToTarget"), false);
        assert.ok((await f.request("Input.dispatchKeyEvent", {}, f.bridge.rootSessionId)).error);
        assert.equal(f.calls.some(call => call.method === "Input.dispatchKeyEvent"), false);
        release.resolve(); await Promise.all([first, second]);
        assert.equal(f.calls.filter(call => call.method === "Emulation.setFocusEmulationEnabled").length, 1);
        assert.equal(f.sent.filter(message => message.method === "Target.attachedToTarget").length, 1);
        f.bridge.close();
    }],
    ["failed focus preparation reports initialization failure without a page", async () => {
        const f = fixture();
        f.hooks["Emulation.setFocusEmulationEnabled"] = async () => { throw new Error("focus unavailable"); };
        const reply = await f.request("Target.setAutoAttach", AUTO);
        assert.match(reply.error.message, /focus unavailable/);
        assert.equal(f.bridge.sessions.size, 0);
        assert.equal(f.sent.some(message => message.method === "Target.attachedToTarget"), false);
        delete f.hooks["Emulation.setFocusEmulationEnabled"];
        await f.boot();
        assert.equal(f.calls.filter(call => call.method === "Emulation.setFocusEmulationEnabled").length, 2);
        f.bridge.close();
    }],
    ["focus preparation deadline rejects initialization and ignores the late completion", async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Emulation.setFocusEmulationEnabled"] = async () => {
            entered.resolve(); await release.promise; return {};
        };
        const preparing = f.request("Target.setAutoAttach", AUTO);
        await entered.promise;
        const reply = await preparing;
        assert.match(reply.error.message, /focus preparation timed out/);
        assert.equal(f.bridge.closed, true);
        assert.equal(f.bridge.sessions.size, 0);
        const count = f.sent.length;
        const calls = f.calls.length;
        await f.request("Target.setAutoAttach", AUTO);
        assert.equal(f.calls.length, calls, "uncertain preparation cannot retry within the same bridge");
        f.bridge.close(); release.resolve();
        await Promise.resolve();
        assert.equal(f.sent.length, count);
        assert.equal(f.bridge.sessions.size, 0);
    }],
    ...["resolve", "reject"].map(outcome => [`Stop during focus preparation ${outcome} never publishes a page`, async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Emulation.setFocusEmulationEnabled"] = async () => {
            entered.resolve(); await release.promise;
            if (outcome === "reject") throw new Error("late focus failure");
            return {};
        };
        const preparing = f.request("Target.setAutoAttach", AUTO);
        await entered.promise; f.attach("early"); f.on = false;
        release.resolve(); await preparing;
        assert.equal(f.bridge.closed, true);
        assert.equal(f.bridge.sessions.size, 0);
        assert.equal(f.sent.some(message => message.method === "Target.attachedToTarget"), false);
        const before = f.calls.length;
        f.on = true;
        await f.request("Target.setAutoAttach", AUTO);
        await f.request("Input.dispatchKeyEvent", {}, f.bridge.rootSessionId);
        assert.equal(f.calls.length, before);
    }]),
    ["old focus completion cannot publish into a replacement bridge", async () => {
        const old = fixture(); const entered = deferred(); const release = deferred();
        old.hooks["Emulation.setFocusEmulationEnabled"] = async () => {
            entered.resolve(); await release.promise; return {};
        };
        const preparing = old.request("Target.setAutoAttach", AUTO);
        await entered.promise; old.bridge.close();
        const current = fixture(); await current.boot();
        release.resolve(); await preparing;
        assert.deepEqual(old.sent, []);
        assert.equal(old.bridge.sessions.size, 0);
        assert.equal(current.sent.filter(message => message.method === "Target.attachedToTarget").length, 1);
        current.bridge.close();
    }],
    ["concurrent duplicate root bootstrap emits one attachment and shares roundtrip", async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Target.getTargetInfo"] = async () => {
            entered.resolve(); await release.promise;
            return {targetInfo: {type: "page", targetId: "page-exact", url: "https://exact.test/"}};
        };
        const a = f.request("Target.setAutoAttach", AUTO); await entered.promise;
        const b = f.request("Target.setAutoAttach", AUTO);
        release.resolve(); await Promise.all([a, b]);
        await f.request("Target.setAutoAttach", AUTO);
        assert.equal(f.sent.filter(message => message.method === "Target.attachedToTarget").length, 1);
        assert.equal(f.calls.length, 2);
        assert.equal(f.chrome.debugger.onEvent.listeners.size, 1);
        f.bridge.close();
    }],
    ["frame-only autoattach rewrites default filter and rejects broad filters", async () => {
        const f = fixture(); const session = await f.boot();
        await f.request("Target.setAutoAttach", AUTO, session);
        assert.deepEqual(f.calls.at(-1), {source: {tabId: 17}, method: "Target.setAutoAttach",
            params: {...AUTO, filter: FILTER}});
        for (const params of [{...AUTO, filter: [{type: "page"}]}, {...AUTO, flatten: false},
            {...AUTO, autoAttach: false}, {...AUTO, filter: [{exclude: false}]},
            {...AUTO, extra: true}]) {
            const before = f.calls.length;
            assert.ok((await f.request("Target.setAutoAttach", params, session)).error);
            assert.equal(f.calls.length, before);
        }
        f.bridge.close();
    }],
    ["known frame children and nested children route real sessions, never pages or workers", async () => {
        const f = fixture(); const root = await f.boot();
        const before = f.sent.length;
        for (const type of ["page", "worker", "service_worker", "shared_worker", "other"]) {
            f.attach(type, type);
            assert.ok((await f.request("Runtime.evaluate", {}, type)).error);
        }
        assert.equal(f.sent.slice(before).some(message => message.method === "Target.attachedToTarget"), false);
        f.attach("frame"); f.attach("nested", "iframe", "frame");
        f.attach("foreign-parent", "iframe", "unknown");
        await f.request("Runtime.evaluate", {expression: "1 + 1"}, "nested");
        assert.deepEqual(f.calls.at(-1).source, {tabId: 17, sessionId: "nested"});
        const events = f.sent.filter(message => message.method === "Target.attachedToTarget");
        assert.equal(events[1].sessionId, root);
        assert.equal(events[2].sessionId, "frame");
        assert.ok((await f.request("Runtime.enable", {}, "foreign-parent")).error);
        const count = f.sent.length; f.attach("frame"); assert.equal(f.sent.length, count);
        f.bridge.close();
    }],
    ["foreign source events and unknown sessions expose nothing", async () => {
        const f = fixture(); await f.boot(); const count = f.sent.length;
        f.attach("foreign", "iframe", undefined, 18);
        f.event("Runtime.consoleAPICalled", {secret: 1}, undefined, 18);
        f.event("Runtime.consoleAPICalled", {secret: 1}, "unknown");
        f.event("Browser.downloadWillBegin", {secret: 1});
        f.event("Page.windowOpen", {secret: 1});
        assert.equal(f.sent.length, count);
        f.bridge.close();
    }],
    ["recursive detaches require exact parent and reject stale commands", async () => {
        const f = fixture(); await f.boot(); f.attach("frame"); f.attach("nested", "iframe", "frame");
        f.event("Target.detachedFromTarget", {sessionId: "nested"});
        assert.ok((await f.request("Runtime.enable", {}, "nested")).result);
        f.event("Target.detachedFromTarget", {sessionId: "frame", targetId: "foreign"});
        assert.ok((await f.request("Runtime.enable", {}, "frame")).result);
        f.event("Target.detachedFromTarget", {sessionId: "frame", targetId: "target-frame"});
        const count = f.calls.length;
        for (const id of ["frame", "nested"]) {
            assert.ok((await f.request("Runtime.enable", {}, id)).error);
        }
        assert.equal(f.calls.length, count);
        f.attach("frame"); f.attach("nested", "iframe", "frame");
        assert.ok((await f.request("Runtime.enable", {}, "frame")).error);
        f.bridge.close();
    }],
    ["known frame events before root resolution are buffered without broad enumeration", async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Target.getTargetInfo"] = async () => {
            entered.resolve(); await release.promise;
            return {targetInfo: {type: "page", targetId: "page-exact"}};
        };
        const running = f.boot(); await entered.promise;
        f.attach("early"); f.attach("popup", "page"); f.attach("foreign", "iframe", undefined, 99);
        assert.equal(f.sent.length, 0);
        release.resolve(); await running;
        assert.deepEqual(f.sent.filter(message => message.method === "Target.attachedToTarget")
            .map(message => message.params.targetInfo.type), ["page", "iframe"]);
        assert.equal(f.calls.length, 2);
        f.bridge.close();
    }],
    ...["resolve", "reject"].map(outcome => [`Stop during command ${outcome} publishes no data or error`, async () => {
        const f = fixture(); const session = await f.boot();
        const entered = deferred(); const release = deferred();
        f.hooks["Runtime.evaluate"] = async () => {
            entered.resolve(); await release.promise;
            if (outcome === "reject") throw new Error("secret failure");
            return {result: {value: "secret result"}};
        };
        const before = f.sent.length;
        const running = f.request("Runtime.evaluate", {expression: "secret"}, session);
        await entered.promise; f.on = false; release.resolve(); await running;
        await f.request("Page.navigate", {url: "https://never.test/"}, session);
        f.event("Runtime.consoleAPICalled", {secret: 1});
        assert.equal(f.sent.length, before);
        assert.equal(f.calls.filter(call => call.method === "Runtime.evaluate").length, 1);
        assert.equal(f.calls.some(call => call.method === "Page.navigate"), false);
        assert.equal(f.errors.length, 1);
        assert.equal(f.chrome.debugger.onEvent.listeners.size, 0);
    }]),
    ["Stop during bootstrap never publishes or revives a session", async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Target.getTargetInfo"] = async () => {
            entered.resolve(); await release.promise;
            return {targetInfo: {type: "page", targetId: "page-exact"}};
        };
        const running = f.request("Target.setAutoAttach", AUTO); await entered.promise;
        f.attach("early"); f.on = false; release.resolve(); await running;
        f.on = true; await f.request("Target.setAutoAttach", AUTO);
        assert.deepEqual(f.sent, []); assert.equal(f.bridge.sessions.size, 0);
    }],
    ["every outgoing event and response checks authority", async () => {
        let f; f = fixture(message => {
            if (message.method === "Target.attachedToTarget") f.on = false;
        });
        await f.request("Target.setAutoAttach", AUTO);
        assert.equal(f.sent.length, 1); assert.equal(f.bridge.closed, true);
        const g = fixture(); await g.boot(); const before = g.sent.length; g.on = false;
        g.event("Runtime.consoleAPICalled", {secret: 1});
        assert.equal(g.sent.length, before); assert.equal(g.bridge.closed, true);
    }],
    ["errors are correlated to command and session without swallowing transport failures", async () => {
        const f = fixture(); const session = await f.boot();
        f.hooks["DOM.getDocument"] = async () => { throw new Error("command failed"); };
        const response = await f.request("DOM.getDocument", {}, session);
        assert.deepEqual(response, {id: f.nextId, sessionId: session,
            error: {code: -32000, message: "command failed"}});
        const g = fixture(() => { throw new Error("send failed"); });
        await g.request("Browser.getVersion");
        assert.equal(g.bridge.closed, true); assert.equal(g.errors[0].message, "send failed");
        f.bridge.close();
    }],
    ["decoded chunked and compressed sizes gate body dispatch before allocation", async () => {
        const f = fixture(); const session = await f.boot();
        const limit = 256 * 1024;
        for (const [id, lengths] of [["chunked", [limit, 1]], ["compressed", [limit + 1]],
            ["unknown", []], ["small", [2, 3]], ["zero", [0]]]) {
            f.event("Network.requestWillBeSent", {requestId: id, request: {} });
            f.event("Network.responseReceived", {requestId: id,
                response: {headers: {"Content-Length": "1", "Content-Encoding": "gzip"}}});
            for (const dataLength of lengths) f.event("Network.dataReceived", {requestId: id, dataLength});
            const before = f.calls.length;
            assert.ok((await f.request("Network.getResponseBody", {requestId: id}, session)).error);
            assert.equal(f.calls.length, before);
            f.event("Network.loadingFinished", {requestId: id, encodedDataLength: 1});
            const reply = await f.request("Network.getResponseBody", {requestId: id}, session);
            assert.equal(Boolean(reply.result), id === "small" || id === "zero");
            assert.equal(f.calls.length - before, id === "small" || id === "zero" ? 1 : 0);
        }
        f.attach("child");
        assert.ok((await f.request("Network.getResponseBody", {requestId: "small"}, "child")).error);
        f.bridge.close();
    }],
    ["allowlist forwards page primitives, denies global and profile effects", async () => {
        const f = fixture(); const session = await f.boot();
        for (const method of ["Page.getFrameTree", "Page.getLayoutMetrics", "Runtime.callFunctionOn",
            "DOM.getContentQuads", "Accessibility.getFullAXTree", "Input.dispatchMouseEvent",
            "Input.insertText", "Page.captureScreenshot"]) {
            assert.ok((await f.request(method, {}, session)).result);
            assert.equal(f.calls.at(-1).method, method);
        }
        for (const method of ["Browser.close", "Browser.getWindowForTarget", "Browser.setWindowBounds",
            "Browser.unknown", "Target.createTarget", "Target.closeTarget", "Target.activateTarget",
            "Target.attachToTarget", "Target.detachFromTarget", "Target.setDiscoverTargets",
            "Storage.getCookies", "Storage.setCookies", "Page.bringToFront", "Page.close",
            "Page.printToPDF", "Network.getCookies", "Network.getAllCookies", "Network.setCookie",
            "Network.clearBrowserCookies", "Network.clearBrowserCache", "Network.setBlockedURLs",
            "Network.setRequestInterception", "Network.setCacheDisabled", "Network.setExtraHTTPHeaders",
            "Fetch.enable", "Emulation.setDeviceMetricsOverride", "Security.setIgnoreCertificateErrors",
            "Runtime.unknown", "DOM.setFileInputFiles"]) {
            for (const target of [undefined, session]) {
                const before = f.calls.length;
                const reply = await f.request(method, {}, target);
                assert.equal(reply.error.code, -32000, method);
                assert.equal(f.calls.length, before, method);
            }
        }
        f.bridge.close();
    }],
    ["close is idempotent listener removal only and invalidates pending generation", async () => {
        const f = fixture(); const session = await f.boot();
        const entered = deferred(); const release = deferred();
        f.hooks["Page.captureScreenshot"] = async () => {
            entered.resolve(); await release.promise; return {data: "private"};
        };
        const running = f.request("Page.captureScreenshot", {}, session); await entered.promise;
        const count = f.sent.length; f.bridge.close(); f.bridge.close(); release.resolve(); await running;
        f.event("Runtime.consoleAPICalled", {secret: 1});
        assert.equal(f.sent.length, count);
        assert.equal(f.chrome.debugger.onEvent.listeners.size, 0);
        assert.equal(f.chrome.debugger.onDetach.listeners.size, 0);
        assert.equal(f.calls.some(call => /detach|attachToTarget/.test(call.method)), false);
    }],
    ["detach while child command awaits suppresses result and session cannot revive", async () => {
        const f = fixture(); await f.boot(); f.attach("frame");
        const entered = deferred(); const release = deferred();
        f.hooks["Runtime.evaluate"] = async () => {
            entered.resolve(); await release.promise; return {result: {value: "private"}};
        };
        const running = f.request("Runtime.evaluate", {}, "frame"); await entered.promise;
        f.event("Target.detachedFromTarget", {sessionId: "frame"});
        release.resolve();
        const reply = await running;
        assert.equal(reply.error.code, -32000); assert.equal(reply.result, undefined);
        f.attach("frame"); assert.ok((await f.request("Runtime.enable", {}, "frame")).error);
        f.bridge.close();
    }],
    ["failed bootstrap discards buffered frames before retry", async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Target.getTargetInfo"] = async () => {
            entered.resolve(); await release.promise; throw new Error("lookup failed");
        };
        const running = f.request("Target.setAutoAttach", AUTO); await entered.promise;
        f.attach("orphan"); release.resolve(); assert.ok((await running).error);
        delete f.hooks["Target.getTargetInfo"]; await f.boot();
        assert.ok((await f.request("Runtime.enable", {}, "orphan")).error);
        f.bridge.close();
    }],
    ["attach then detach during bootstrap leaves no child", async () => {
        const f = fixture(); const entered = deferred(); const release = deferred();
        f.hooks["Target.getTargetInfo"] = async () => {
            entered.resolve(); await release.promise;
            return {targetInfo: {type: "page", targetId: "page-exact"}};
        };
        const running = f.boot(); await entered.promise;
        f.attach("transient");
        f.event("Target.detachedFromTarget", {sessionId: "transient"});
        release.resolve(); await running;
        assert.ok((await f.request("Runtime.enable", {}, "transient")).error);
        f.bridge.close();
    }],
    ["async authorization fails closed rather than allowing a check-to-send race", async () => {
        const f = fixture(); f.bridge.checkAuthorization = async () => true;
        await f.request("Browser.getVersion");
        assert.equal(f.bridge.closed, true); assert.deepEqual(f.calls, []);
        assert.deepEqual(f.sent, []); assert.match(f.errors[0].message, /synchronous/);
    }],
    ["exact debugger detach reports failure, foreign detach does not stop bridge", async () => {
        const f = fixture(); await f.boot();
        f.chrome.debugger.onDetach.emit({tabId: 99}, "foreign");
        assert.equal(f.bridge.closed, false);
        f.chrome.debugger.onDetach.emit({tabId: 17}, "target_closed");
        assert.equal(f.bridge.closed, true); assert.match(f.errors[0].message, /target_closed/);
    }],
];

// Optional OFFLINE interoperability: the official engine uses our object transport, not a browser.
// Point ABUSH_PLAYWRIGHT_CORE at the pinned playwright-core package directory; no install/GUI/socket.
async function officialSmoke() {
    const packagePath = process.env.ABUSH_PLAYWRIGHT_CORE;
    if (!packagePath) { console.log("SKIP official engine (set ABUSH_PLAYWRIGHT_CORE)"); return; }
    const version = require(path.join(packagePath, "package.json")).version;
    assert.equal(version, "1.65.1", "Re-review initialization before changing the engine pin");
    const {chromium} = require(packagePath);
    let transport;
    const f = fixture(message => queueMicrotask(() => {
        transport.onmessage?.(message);
        if (message.result?.frameTree) {
            f.event("Runtime.executionContextCreated", {context: {id: 1,
                origin: "https://fixture.test", name: "",
                auxData: {frameId: "page-exact", isDefault: true}}});
        }
    }));
    transport = {send: message => { void f.bridge.handle(message); },
        close: () => { f.bridge.close(); queueMicrotask(() => transport.onclose?.()); }};
    f.hooks["Page.getFrameTree"] = async () => ({frameTree: {frame: {id: "page-exact",
        loaderId: "loader", url: "https://fixture.test/", securityOrigin: "https://fixture.test",
        mimeType: "text/html"}}});
    f.hooks["Page.addScriptToEvaluateOnNewDocument"] = async () => ({identifier: "init"});
    f.hooks["Runtime.evaluate"] = async () => ({result: {type: "number", value: 42}});
    f.hooks["Runtime.callFunctionOn"] = async () => ({result: {type: "number", value: 42}});
    const browser = await chromium.connectOverCDP(transport, {noDefaults: true, timeout: 5000});
    try {
        assert.equal(browser.contexts().length, 1);
        const pages = browser.contexts()[0].pages();
        assert.deepEqual(f.sent.filter(message => message.error), []);
        assert.equal(pages.length, 1); assert.equal(pages[0].url(), "https://fixture.test/");
        assert.equal(await pages[0].evaluate(() => 42), 42);
        assert.ok(f.calls.some(call => ["Runtime.evaluate", "Runtime.callFunctionOn"]
            .includes(call.method)));
        assert.deepEqual(f.sent.filter(message => message.error), []);
        assert.equal(f.calls.some(call => call.method.startsWith("Browser.") &&
            call.method !== "Browser.getVersion"), false);
    } finally { await browser.close(); }
    assert.equal(f.chrome.debugger.onEvent.listeners.size, 0);
    console.log(`PASS official Playwright ${version} noDefaults object-transport initialization/evaluate`);
}
async function bounded(name, run) {
    let timer;
    try {
        return await Promise.race([run(), new Promise((_, reject) => {
            timer = setTimeout(() => reject(new Error(`Timed out: ${name}`)), 8000);
        })]);
    } finally { clearTimeout(timer); }
}
(async () => {
    for (const [name, run] of cases) {
        await bounded(name, run); console.log(`PASS ${name}`);
    }
    await bounded("official engine interoperability", officialSmoke);
    console.log(`${cases.length} deterministic CDP bridge cases passed`);
})().catch(error => { console.error(error); process.exitCode = 1; });
