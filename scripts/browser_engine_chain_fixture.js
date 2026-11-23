#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const readline = require("node:readline");
const {spawn} = require("node:child_process");
const {once} = require("node:events");
if (process.env.ABUSH_ENGINE_REAL_EXTENSION === "1") {
    require("./browser_engine_real_extension_chain.js");
} else {
const {fixture} = require("./browser_tab_groups_fixture.js");
const extension = process.argv[2], host = process.argv[3], remoteRoot = process.argv[4];
const artifacts = process.argv[5];
const engineRoot = process.env.ABUSH_BROWSER_ENGINE_ROOT;
assert.equal(process.env.ABUSH_ENGINE_FIXTURE, "1");
assert.ok(engineRoot && process.env.ABUSH_ENGINE_CHROME && artifacts);
const {loadOfficialPlaywright} = require(path.join(engineRoot, "engine.cjs"));
const {WorkspaceExecutor, WAL_KEY, RECEIPTS_KEY, CONSENT_KEY, bounded} =
    require(path.join(extension, "workspace.js"));
const {NativeDispatcher, createWorkspaceInitializer, registerNativeEventRouter,
    registerWorkspaceWindowListeners, registerRuntimeMessageRouter} =
    require(path.join(extension, "background.js"));
function barrier() {
    let resolve;
    const promise = new Promise(yes => { resolve = yes; });
    return {promise, resolve};
}
function event() {
    const listeners = new Set();
    return {addListener: fn => listeners.add(fn), removeListener: fn => listeners.delete(fn),
        emit: (...args) => { for (const fn of [...listeners]) fn(...args); }};
}
let browser, fixturePid, session, python, dispatcher, executor, ready;
let wake = barrier(), blocked = barrier();
const native = [], errors = [], frames = [];
const remoteKey = "r_12345678";
const f = fixture({globalAutomation: true, officialEngine: true});
const personal = structuredClone([...f.tabs.values()]);
let workTab = null, posts = 0, finished = 0;
function signal() { const previous = wake; wake = barrier(); previous.resolve(); }
function frame(phase) {
    if (!executor) return;
    for (const original of personal) assert.deepEqual(f.tabs.get(original.id), original);
    assert.ok(executor.engineSessions.size <= 1, "one live engine lease");
    if (workTab !== null) assert.equal(f.tabs.get(workTab).active, false);
    frames.push({phase, state: structuredClone(executor.state(remoteKey)),
        tabs: structuredClone([...f.tabs.values()]),
        leases: [...executor.engineSessions.values()].map(value => value.lease_id),
        posts, finished});
    signal();
}
async function until(predicate, label) {
    await bounded(async () => {
        while (!predicate()) {
            const changed = wake.promise;
            if (predicate()) return;
            await changed;
        }
    }, label, 15000);
}
function snapshot() {
    for (const original of personal) assert.deepEqual(f.tabs.get(original.id), original);
    const responseError = message => /receipt timed out|remote is unavailable/.test(message);
    const receiptLost = native.some(item => item.event === "abush_browser.remote_state" &&
        item.payload.connected === false);
    const observedErrors = receiptLost ? errors.filter(responseError) : [];
    return {state: executor.state(remoteKey), tabs: [...f.tabs.values()],
        consent: f.session[CONSENT_KEY], wal: f.local[WAL_KEY],
        receipts: f.local[RECEIPTS_KEY], frames, posts, finished,
        errors: errors.filter(message => !observedErrors.includes(message)),
        expectedReceiptErrors: observedErrors,
        calls: f.calls.filter(call => !call.name.includes("storage") &&
            !call.name.startsWith("local.") && !call.name.startsWith("session."))
            .map(call => ({name: call.name, method: call.args[1]})),
        native: native.filter(item => item.operation || item.event?.includes("completion") ||
            item.event?.includes("cancel")).map(item => ({...item, payload: undefined})),
        engineOpenCount: native.filter(item => item.operation === "engine_open").length,
        engineExecuteCount: native.filter(item => item.operation === "engine_execute").length};
}
function routeWorker() {
    f.api.runtime.onMessage.listeners.length = 0;
    for (const area of [f.api.tabs, f.api.tabGroups, f.api.windows]) {
        for (const [key, value] of Object.entries(area)) {
            if (key.startsWith("on") && value.listeners) value.listeners.length = 0;
        }
    }
    executor = new WorkspaceExecutor(f.api, (...args) => dispatcher.request(...args));
    executor.onAdvisoryError = error => { errors.push(error.message); signal(); };
    const remoteState = executor.remoteState.bind(executor);
    executor.remoteState = async payload => {
        try { return await remoteState(payload); } finally { frame("remote_state"); }
    };
    ready = createWorkspaceInitializer(executor, dispatcher, error => errors.push(String(error)));
    registerWorkspaceWindowListeners(f.api, executor, ready, error => errors.push(error.message));
    registerRuntimeMessageRouter(f.api, executor, ready);
    return ready;
}
async function setup() {
    await fs.mkdir(artifacts, {recursive: true});
    process.env.TMPDIR = artifacts;
    const playwright = await loadOfficialPlaywright(artifacts);
    browser = await bounded(() => playwright.chromium.launch({
        executablePath: process.env.ABUSH_ENGINE_CHROME, headless: true,
        timeout: 10000, artifactsDir: artifacts,
    }), "fresh pipe Chromium launch", 15000);
    const browserSession = await browser.newBrowserCDPSession();
    const processes = await browserSession.send("SystemInfo.getProcessInfo");
    fixturePid = processes.processInfo.find(info => info.type === "browser").id;
    await browserSession.detach();
    const page = await browser.newPage({viewport: {width: 800, height: 600}});
    await page.context().route("**/*", async route => {
        const request = route.request(), url = new URL(request.url());
        assert.equal(url.origin, "http://fixture.test", "no external website requests");
        if (url.pathname === "/api") {
            assert.equal(request.method(), "POST");
            assert.equal(request.postData(), "Ada");
            posts++;
            await route.fulfill({contentType: "application/json", body: JSON.stringify({answer: 42})});
        } else {
            await route.fulfill({contentType: "text/html", body: `<!doctype html><title>Chain fixture</title>
                <label>Name<input aria-label="Name"></label>
                <button onclick="fetch('/api',{method:'POST',body:document.querySelector('input').value})
                .then(r=>r.json()).then(v=>{document.querySelector('output').textContent='Ready '+v.answer})">Load</button>
                <button onclick="this.textContent='Receipt once'; window.receiptClicks=(window.receiptClicks||0)+1">Receipt</button>
                <output></output>`});
        }
        frame("website_fulfilled");
    });
    page.on("requestfinished", request => {
        if (request.url().endsWith("/api")) { finished++; frame("api_finished"); }
    });
    session = await page.context().newCDPSession(page);
    const emit = session.emit;
    session.emit = function(method, params) {
        if (typeof method === "string" && method.includes(".")) {
            assert.ok(workTab !== null, "no debugger traffic before exact tab creation");
            f.api.debugger.onEvent.emit({tabId: workTab}, method, params);
            frame(`cdp_event:${method}`);
        }
        return emit.call(this, method, params);
    };
    f.hooks["tabs.create.after"] = (tab, args) => {
        assert.equal(workTab, null, "one exact new work tab only");
        assert.equal(page.url(), "about:blank");
        assert.equal(args.active, false);
        assert.match(args.url, /^about:blank#abush-work-/);
        workTab = tab.id;
        frame("materialized");
    };
    f.hooks["debugger.sendCommand"] = async (source, method, params) => {
        assert.equal(source.tabId, workTab, "no personal/restored tab adoption");
        assert.equal(source.sessionId, undefined, "root-page fixture; OOPIF not claimed");
        assert.equal(method.startsWith("Browser."), false,
            "chrome.debugger does not expose the Browser domain");
        assert.ok(!/bringToFront|activateTarget|closeTarget|setDeviceMetrics|setWindow/.test(method),
            "automation must not mutate window presentation");
        const result = await session.send(method, params);
        if (method === "Page.navigate") f.tabs.get(workTab).url = params.url;
        if (method === "Runtime.callFunctionOn" && JSON.stringify(params).includes("Never appears")) {
            blocked.resolve();
        }
        frame(`cdp_result:${method}`);
        return result;
    };
    python = spawn("python3", ["-B", path.join(__dirname, "browser_engine_chain_host.py"),
        host, remoteRoot], {stdio: ["pipe", "pipe", "inherit"]});
    const port = {onMessage: event(), onDisconnect: event(), disconnect: () => python.stdin.end(),
        postMessage: message => {
            native.push({direction: "request", operation: message.operation, payload: message.payload});
            const body = Buffer.from(JSON.stringify(message)), header = Buffer.alloc(4);
            header.writeUInt32LE(body.length);
            python.stdin.write(Buffer.concat([header, body]));
            frame(`native_request:${message.operation}`);
        }};
    python.stdin.on("error", error => { errors.push(error.message); signal(); });
    python.once("exit", () => port.onDisconnect.emit());
    let buffer = Buffer.alloc(0);
    python.stdout.on("data", chunk => {
        buffer = Buffer.concat([buffer, chunk]);
        while (buffer.length >= 4) {
            const length = buffer.readUInt32LE();
            assert.ok(length > 0 && length <= 1024 * 1024);
            if (buffer.length < length + 4) break;
            const message = JSON.parse(buffer.subarray(4, length + 4));
            buffer = buffer.subarray(length + 4);
            native.push({direction: "response", event: message.event, payload: message.payload});
            port.onMessage.emit(message);
            frame(`native_response:${message.event || message.reply_to}`);
        }
    });
    dispatcher = new NativeDispatcher(() => port);
    routeWorker();
    registerNativeEventRouter(dispatcher, new Proxy({}, {
        get: (_, name) => (...args) => executor[name](...args),
    }), () => ready(), error => errors.push(String(error)));
    await ready();
    await until(() => executor.remotes.get(remoteKey)?.connected &&
        executor.records.get(remoteKey)?.agreed && executor.state(remoteKey).phase === "enabled",
    "real remote hello and enabled reservation");
    frame("ready");
    return page;
}
async function run(command, page) {
    switch (command.op) {
    case "ready": return snapshot();
    case "snapshot":
        const drained = await bounded(() => Promise.allSettled([...executor.executionTails.values()]),
            "execution drain", 15000);
        for (const result of drained) {
            if (result.status === "rejected") assert.match(result.reason.message,
                /receipt timed out|Engine lease revoked|cancel/i);
        }
        await executor.localTail;
        frame("snapshot"); return snapshot();
    case "lose_receipt":
        await fs.writeFile(path.join(process.env.HOME, "drop-receipt"), "armed");
        return snapshot();
    case "receipt_reconnected":
        await until(() => native.filter(item => item.event === "abush_browser.remote_state" &&
            item.payload.connected).length >= 2 && Object.keys(executor.receipts).length >= 9,
        "real RemotePeer reconnect and exact completion receipt retry");
        assert.ok(await fs.readFile(path.join(process.env.HOME, "receipt-lost"), "utf8"));
        assert.equal(await page.evaluate(() => window.receiptClicks), 1);
        frame("receipt_reconnected"); return snapshot();
    case "api_finished":
        await until(() => finished === 1, "preregistered API finished event");
        assert.equal(await page.getByRole("textbox").inputValue(), "Ada");
        return snapshot();
    case "arm_wait": blocked = barrier(); return snapshot();
    case "wait_entered":
        await bounded(() => blocked.promise, "real waiting Runtime call", 15000); return snapshot();
    case "stop": {
        const response = await bounded(() => new Promise(resolve => f.api.runtime.onMessage.emit(
            {control: "stop"}, f.trusted, resolve)), "Stop exits real waiting engine", 15000);
        assert.equal(response.ok, true, JSON.stringify(response));
        frame("stopped"); return snapshot();
    }
    case "start": {
        const response = await bounded(() => new Promise(resolve => f.api.runtime.onMessage.emit(
            {control: "start"}, f.trusted, resolve)), "Start", 15000);
        assert.equal(response.ok, true, JSON.stringify(response)); return snapshot();
    }
    case "restart_worker":
        assert.equal(executor.operations.size, 0);
        for (const candidate of executor.personalCandidates.values()) clearTimeout(candidate.timer);
        // Public worker shutdown releases its exact engine attachment, not the fixture page.
        for (const record of executor.records.values()) await executor.detach(record);
        routeWorker();
        await ready();
        const refreshed = await dispatcher.request("abush_browser", "refresh_remotes", {});
        for (const remote of refreshed.remotes) await executor.remoteState(remote);
        await until(() => executor.records.get(remoteKey)?.agreed, "same-boot worker reconciliation");
        frame("worker_restarted"); return snapshot();
    case "quit": return snapshot();
    default: throw new Error(`Unknown fixture control ${command.op}`);
    }
}
async function teardown() {
    const failures = [];
    const attempt = async (label, task, ms = 5000) => {
        try { await bounded(task, label, ms); } catch (error) { failures.push(error); }
    };
    if (executor) {
        for (const candidate of executor.personalCandidates.values()) clearTimeout(candidate.timer);
        await attempt("inheritance teardown", () => executor.inheritanceTail);
    }
    dispatcher?.stop();
    if (python) {
        const exited = python.exitCode !== null || python.signalCode !== null ?
            Promise.resolve() : once(python, "exit");
        python.stdin.end();
        try { await bounded(() => exited, "owned Python teardown", 5000); }
        catch (error) {
            failures.push(error); python.kill("SIGKILL");
            await attempt("owned Python killed join", () => exited, 2000);
        }
        if (python.exitCode !== 0) failures.push(new Error(`Python exited ${python.exitCode}`));
    }
    if (browser) {
        try { await bounded(() => browser.close(), "owned Chromium teardown", 5000); }
        catch (error) {
            failures.push(error);
            if (fixturePid) process.kill(fixturePid, "SIGKILL");
        }
    }
    if (failures.length) throw new AggregateError(failures, "chain fixture teardown");
}
(async () => {
    const controls = readline.createInterface({input: process.stdin});
    const commands = controls[Symbol.asyncIterator]();
    try {
        const page = await setup();
        for await (const line of {[Symbol.asyncIterator]: () => commands}) {
            const command = JSON.parse(line);
            const result = await bounded(() => run(command, page), `control ${command.op}`, 20000);
            process.stdout.write(JSON.stringify(result) + "\n");
            if (command.op === "quit") break;
        }
    } finally { controls.close(); await teardown(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
}
