#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const readline = require("node:readline");
const {spawn} = require("node:child_process");
const {once} = require("node:events");
const {launchUnmanagedPipeFixture, waitForWorkerFixture} = require("./browser_unmanaged_pipe_fixture.js");
const extension = process.argv[2], host = process.argv[3], remoteRoot = process.argv[4];
const artifacts = process.argv[5];
const engineRoot = process.env.ABUSH_BROWSER_ENGINE_ROOT;
const executablePath = process.env.ABUSH_ENGINE_EXTENSION_CHROME;
assert.equal(process.env.ABUSH_ENGINE_FIXTURE, "1");
assert.ok(engineRoot && executablePath && artifacts);
const {loadOfficialPlaywright} = require(path.join(engineRoot, "engine.cjs"));
const {bounded} = require(path.join(extension, "workspace.js"));
function barrier() {
    let resolve;
    const promise = new Promise(yes => { resolve = yes; });
    return {promise, resolve};
}
let context, browser, worker, python, ownedChrome, pump, primaryError, fixtureReady = false;
let wake = barrier(), blocked = barrier(), posts = 0, finished = 0, stopping = false;
let lastDiagnostic = null;
const failures = [], native = [], tasks = new Set();
function signal() { const previous = wake; wake = barrier(); previous.resolve(); }
function evaluate(fn, argument) {
    return bounded(() => worker.evaluate(fn, argument), "extension worker evaluation", 20000);
}
function dispatch(message) {
    const task = evaluate(message => fixture.receive(message), message);
    tasks.add(task);
    task.catch(error => { failures.push(error); signal(); }).finally(() => tasks.delete(task));
}
async function until(predicate, label) {
    await bounded(async () => {
        while (!predicate()) {
            if (failures.length) throw failures[0];
            const changed = wake.promise;
            if (predicate()) return;
            await changed;
        }
    }, label, 15000);
}
async function setup() {
    await fs.mkdir(artifacts, {recursive: true});
    process.env.TMPDIR = artifacts;
    const unpacked = path.join(artifacts, "extension");
    await fs.mkdir(unpacked);
    for (const name of ["workspace.js", "playwright-transport.js", "routes.js", "content.js"]) {
        await fs.copyFile(path.join(extension, name), path.join(unpacked, name));
    }
    const background = await fs.readFile(path.join(extension, "background.js"), "utf8");
    const marker = '\nif (typeof chrome !== "undefined") {';
    assert.equal(background.split(marker).length, 2, "production startup boundary changed");
    // Replace only automatic native plumbing startup, not dispatcher/router implementations.
    await fs.writeFile(path.join(unpacked, "background.js"), background.split(marker)[0]);
    await fs.copyFile(path.join(__dirname, "browser_engine_chain_worker.js"), path.join(unpacked, "worker.js"));
    await fs.writeFile(path.join(unpacked, "manifest.json"), JSON.stringify({
        manifest_version: 3, name: "Owned full native chain regression", version: "1.0",
        permissions: ["debugger", "tabs", "tabGroups", "storage"],
        background: {service_worker: "worker.js"},
        content_scripts: [{matches: ["http://fixture.test/*"], js: ["routes.js", "content.js"],
            run_at: "document_start"}],
    }));
    const playwright = await loadOfficialPlaywright(artifacts);
    ownedChrome = await launchUnmanagedPipeFixture({playwright, executablePath, artifacts});
    ({context, browser} = ownedChrome);
    const root = await browser.newBrowserCDPSession();
    await context.route("**/*", async route => {
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
        signal();
    });
    context.on("requestfinished", request => {
        if (request.url().endsWith("/api")) { finished++; signal(); }
    });
    const workerReady = context.waitForEvent("serviceworker", {
        predicate: candidate => candidate.url().endsWith("/worker.js"), timeout: 10000,
    });
    workerReady.catch(() => {});
    await root.send("Extensions.loadUnpacked", {path: unpacked});
    worker = await workerReady;
    await waitForWorkerFixture(worker);
    fixtureReady = true;
    await root.detach();
    worker.on("console", message => {
        if (message.type() === "error") process.stderr.write(`WORKER ${message.text()}\n`);
    });
    await evaluate(() => fixture.setup());
    pump = (async () => {
        while (!stopping) {
            const messages = await worker.evaluate(() => fixture.next());
            for (const item of messages) {
                if (item.kind === "stop") return;
                if (item.kind === "wait_entered") { blocked.resolve(); continue; }
                if (item.kind === "diagnostic") {
                    lastDiagnostic = item;
                    if (item.phase === "api_error" || item.phase === "error") {
                        process.stderr.write(`CHAIN ${JSON.stringify(item)}\n`);
                    }
                    continue;
                }
                assert.equal(item.kind, "native");
                const message = item.message;
                native.push(message);
                const body = Buffer.from(JSON.stringify(message)), header = Buffer.alloc(4);
                header.writeUInt32LE(body.length);
                python.stdin.write(Buffer.concat([header, body]));
                signal();
            }
        }
    })();
    pump.catch(error => { failures.push(error); signal(); });
    python = spawn("python3", ["-B", path.join(__dirname, "browser_engine_chain_host.py"),
        host, remoteRoot], {stdio: ["pipe", "pipe", "inherit"]});
    python.stdin.on("error", error => { failures.push(error); signal(); });
    python.once("exit", () => {
        if (!stopping) dispatchDisconnect();
    });
    let buffer = Buffer.alloc(0);
    python.stdout.on("data", chunk => {
        try {
            buffer = Buffer.concat([buffer, chunk]);
            while (buffer.length >= 4) {
                const length = buffer.readUInt32LE();
                assert.ok(length > 0 && length <= 1024 * 1024);
                if (buffer.length < length + 4) break;
                const message = JSON.parse(buffer.subarray(4, length + 4));
                buffer = buffer.subarray(length + 4);
                native.push(message);
                dispatch(message);
                signal();
            }
        } catch (error) { failures.push(error); signal(); }
    });
}
function dispatchDisconnect() {
    const task = evaluate(() => fixture.disconnected());
    tasks.add(task);
    task.catch(error => failures.push(error)).finally(() => tasks.delete(task));
}
async function snapshot(op = "snapshot") {
    const result = await evaluate(op => fixture.control(op), op);
    if (failures.length) throw new AggregateError(failures, "native chain transport");
    ownedChrome.assertObserverDefaults();
    return {...result, posts, finished, lastDiagnostic};
}
function page() {
    const match = context.pages().find(page => page.url().startsWith("http://fixture.test"));
    assert.ok(match, "production navigation must materialize an exact fixture page");
    return match;
}
async function run(command) {
    switch (command.op) {
    case "bootstrap":
        await until(() => native.some(message => message.service === "native_host" &&
            message.event === "bootstrap"), "real Python native bootstrap");
        return {...await evaluate(() => fixture.bootstrap()), posts, finished, lastDiagnostic};
    case "ready": {
        const result = await evaluate(() => fixture.ready());
        ownedChrome.assertObserverDefaults();
        return {...result, posts, finished, lastDiagnostic};
    }
    case "lose_receipt":
        await fs.writeFile(path.join(process.env.HOME, "drop-receipt"), "armed");
        return snapshot();
    case "receipt_reconnected":
        await until(() => native.filter(message => message.event === "abush_browser.remote_state" &&
            message.payload.connected).length >= 2 && native.filter(message =>
            message.operation === "executor_completion").length >= 10, "real receipt retry reconnect");
        assert.ok(await fs.readFile(path.join(process.env.HOME, "receipt-lost"), "utf8"));
        assert.equal(await page().evaluate(() => window.receiptClicks), 1);
        return snapshot();
    case "api_finished":
        await until(() => finished === 1, "API finished event");
        assert.equal(await page().getByRole("textbox").inputValue(), "Ada");
        return snapshot();
    case "arm_wait": blocked = barrier(); return snapshot();
    case "wait_entered":
        await bounded(() => blocked.promise, "actual chrome.debugger waiting Runtime call", 15000);
        return snapshot("quit");
    default: return snapshot(command.op);
    }
}
async function teardown() {
    const teardownErrors = [];
    const attempt = async (label, task, ms = 5000) => {
        try { await bounded(task, label, ms); } catch (error) { teardownErrors.push(error); }
    };
    if (worker && fixtureReady) await attempt("worker stop", () => evaluate(() => fixture.stop()));
    if (pump) await attempt("native pump join", () => pump);
    stopping = true;
    await attempt("native delivery joins", () => Promise.allSettled([...tasks]));
    if (python) {
        const exited = python.exitCode !== null || python.signalCode !== null ?
            Promise.resolve() : once(python, "exit");
        python.stdin.end();
        try { await bounded(() => exited, "owned Python teardown", 5000); }
        catch (error) {
            teardownErrors.push(error); python.kill("SIGKILL");
            await attempt("owned Python killed join", () => exited, 2000);
        }
        if (python.exitCode !== 0) teardownErrors.push(new Error(`Python exited ${python.exitCode}`));
    }
    if (ownedChrome) await attempt("owned Chrome shutdown", () => ownedChrome.shutdown(), 15000);
    if (teardownErrors.length) throw new AggregateError(
        [...(primaryError ? [primaryError] : []), ...teardownErrors], "real extension chain teardown");
}
(async () => {
    const controls = readline.createInterface({input: process.stdin});
    const commands = controls[Symbol.asyncIterator]();
    try {
        await setup();
        for await (const line of {[Symbol.asyncIterator]: () => commands}) {
            const command = JSON.parse(line);
            try {
                const result = await bounded(() => run(command), `control ${command.op}`, 20000);
                process.stdout.write(JSON.stringify(result) + "\n");
            } catch (error) {
                process.stderr.write(`CHAIN FAILURE ${JSON.stringify({control: command.op, lastDiagnostic,
                    nativeTail: native.slice(-4).map(message => ({operation: message.operation,
                        event: message.event, id: message.id, reply_to: message.reply_to,
                        error: message.error})), error: error.message})}\n`);
                throw error;
            }
            if (command.op === "quit") break;
        }
    } catch (error) {
        primaryError = error;
        console.error("CHAIN PRIMARY FAILURE", error);
        throw error;
    } finally { controls.close(); await teardown(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
