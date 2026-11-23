#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const readline = require("node:readline");
const http = require("node:http");
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
let context, browser, worker, python, ownedChrome, website, pump, primaryError, fixtureReady = false;
let wake = barrier(), blocked = barrier(), posts = 0, finished = 0, stopping = false;
let lastDiagnostic = null;
const failures = [], native = [], tasks = new Set(), finishedRequests = new Set();
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
async function startWebsite() {
    website = http.createServer((request, response) => {
        const task = (async () => {
            assert.equal(request.headers.host, "fixture.test", "only the fixture hostname");
            if (request.url === "/api") {
                assert.equal(request.method, "POST");
                const body = await bounded(async () => {
                    const chunks = [];
                    let size = 0;
                    for await (const chunk of request) {
                        size += chunk.length;
                        assert.ok(size <= 3, "bounded Ada request body");
                        chunks.push(chunk);
                    }
                    return Buffer.concat(chunks).toString("utf8");
                }, "website API body", 5000);
                assert.equal(body, "Ada");
                assert.equal(++posts, 1, "API POST exactly once");
                response.writeHead(200, {"Content-Type": "application/json"});
                response.end(JSON.stringify({answer: 42}));
            } else if (request.url === "/" && request.method === "GET") {
                response.writeHead(200, {"Content-Type": "text/html",
                    "Content-Security-Policy": "default-src 'none'; script-src 'unsafe-inline'; connect-src 'self'"});
                response.end(`<!doctype html><title>Chain fixture</title>
                    <label>Name<input aria-label="Name"></label>
                    <button onclick="fetch('/api',{method:'POST',body:document.querySelector('input').value})
                    .then(r=>r.json()).then(v=>{document.querySelector('output').textContent='Ready '+v.answer})">Load</button>
                    <button id="receipt" data-clicks="0" onclick="this.textContent='Receipt once';
                    this.dataset.clicks=Number(this.dataset.clicks)+1">Receipt</button>
                    <output></output>`);
            } else {
                assert.equal(request.url, "/favicon.ico", "known website paths only");
                assert.equal(request.method, "GET");
                response.writeHead(204);
                response.end();
            }
            signal();
        })();
        tasks.add(task);
        task.catch(error => {
            failures.push(error); signal(); request.destroy(); response.destroy();
        }).finally(() => tasks.delete(task));
    });
    website.headersTimeout = 5000;
    website.requestTimeout = 5000;
    website.setTimeout(5000, socket => socket.destroy());
    website.on("error", error => { failures.push(error); signal(); });
    const listening = once(website, "listening");
    listening.catch(() => {});
    website.listen(0, "127.0.0.1");
    await bounded(() => listening, "website startup", 5000);
    return website.address().port;
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
        permissions: ["debugger", "tabs", "tabGroups", "storage", "scripting"],
        host_permissions: ["http://fixture.test/*"],
        background: {service_worker: "worker.js"},
        content_scripts: [{matches: ["http://fixture.test/*"], js: ["routes.js", "content.js"],
            run_at: "document_start"}],
    }));
    const playwright = await loadOfficialPlaywright(artifacts);
    const fixtureHostPort = await startWebsite();
    ownedChrome = await launchUnmanagedPipeFixture({playwright, executablePath, artifacts,
        observePages: false, fixtureHostPort});
    ({context, browser} = ownedChrome);
    const root = await browser.newBrowserCDPSession();
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
                if (item.kind === "api_finished") {
                    assert.ok(Number.isInteger(item.tabId) && typeof item.requestId === "string");
                    const identity = JSON.stringify([item.tabId, item.requestId]);
                    assert.ok(!finishedRequests.has(identity), "client finish exactly once");
                    finishedRequests.add(identity);
                    finished++; signal(); continue;
                }
                if (item.kind === "diagnostic") {
                    lastDiagnostic = item;
                    if (item.phase === "api_error" || item.phase === "error" || item.phase === "capture_enter") {
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
async function pageDiagnostics() {
    const result = await evaluate(() => fixture.pageDiagnostics());
    assert.equal(result.url, "http://fixture.test/");
    ownedChrome.assertObserverDefaults();
    return result;
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
    case "verify_full_capture": {
        const evidence = await evaluate(() => fixture.captureEvidence());
        const {decodePng} = require("./browser_capture_png_fixture.js");
        const image = decodePng(Buffer.from(evidence.data, "base64"));
        assert.equal(image.width, evidence.width);
        assert.equal(image.height, evidence.height);
        assert.deepEqual(image.pixel(40, 40), [20, 40, 220]);
        assert.deepEqual(image.pixel(40, 1540), [20, 200, 40]);
        return {width: image.width, height: image.height, top: true, bottom: true};
    }
    case "diagnostics_rejections":
        return evaluate(() => fixture.diagnosticsRejections());
    case "page_diagnostics":
        return pageDiagnostics();
    case "lose_receipt":
        await fs.writeFile(path.join(process.env.HOME, "drop-receipt"), "armed");
        return snapshot();
    case "receipt_reconnected":
        await until(() => native.filter(message => message.event === "abush_browser.remote_state" &&
            message.payload.connected).length >= 2 && native.filter(message =>
            message.operation === "executor_completion").length >= 10, "real receipt retry reconnect");
        assert.ok(await fs.readFile(path.join(process.env.HOME, "receipt-lost"), "utf8"));
        // Native connected is published before the local authority/record commit.
        // Use the fixture's production-reducer barrier, not a delay or a wire-only signal.
        const rebound = await evaluate(() => fixture.ready());
        assert.equal(rebound.state.phase, "enabled");
        assert.ok(rebound.state.binding?.page);
        assert.equal((await pageDiagnostics()).receiptClicks, 1);
        return snapshot();
    case "api_finished":
        await until(() => finished === 1, "API finished event");
        assert.equal(posts, 1);
        assert.equal((await pageDiagnostics()).inputValue, "Ada");
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
    if (website) {
        await attempt("website shutdown", () => new Promise((resolve, reject) => {
            website.close(error => error ? reject(error) : resolve());
            website.closeAllConnections();
        }));
    }
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
