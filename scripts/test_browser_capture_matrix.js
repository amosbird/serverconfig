#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const http = require("node:http");
const {once} = require("node:events");
const {decodePng} = require("./browser_capture_png_fixture.js");
const {launchUnmanagedPipeFixture, waitForWorkerFixture} = require("./browser_unmanaged_pipe_fixture.js");
const engineRoot = process.env.ABUSH_BROWSER_ENGINE_ROOT;
assert.equal(process.env.ABUSH_ENGINE_FIXTURE, "1");
assert.ok(engineRoot && process.env.ABUSH_ENGINE_EXTENSION_CHROME);
const {Engine, loadOfficialPlaywright} = require(path.join(engineRoot, "engine.cjs"));
const {execFileSync} = require("node:child_process");
const common = execFileSync("git", ["rev-parse", "--path-format=absolute", "--git-common-dir"],
    {cwd: path.join(__dirname, ".."), encoding: "utf8", timeout: 5000}).trim();
const tmp = path.join(path.dirname(common), ".worktrees/.tmp");
const rows = process.env.ABUSH_CAPTURE_ROWS?.split(",") ||
    ["baseline_viewport", "production_viewport", "baseline_full", "production_full"];
assert.ok(rows.every(row => /^(baseline|production)_(viewport|full)$/.test(row)),
    "Only baseline/production viewport/full diagnostic rows are supported");
async function bounded(label, task, ms = 3000) {
    let timer;
    try {
        return await Promise.race([Promise.resolve().then(task), new Promise((_, reject) => {
            timer = setTimeout(() => reject(new Error(`Deadline: ${label}`)), ms);
        })]);
    } finally { clearTimeout(timer); }
}
function decode(png) {
    const image = decodePng(png);
    let topPixels = 0, bottomPixels = 0;
    for (let y = 0; y < image.height; y++) {
        const pixel = image.pixel(Math.min(350, image.width - 1), y);
        if (String(pixel) === "20,40,220") topPixels++;
        if (String(pixel) === "20,200,40") bottomPixels++;
    }
    return {width: image.width, height: image.height, topPixels, bottomPixels,
        bottomPresent: bottomPixels > 50};
}
async function runRow(row, origin, artifacts, playwright) {
    let owned, worker, engine, pump, stopping = false;
    const tasks = new Set(), errors = [], replies = new Map();
    const record = {row, startMs: performance.now(), artifacts, mode: "headless=new"};
    const evaluate = (fn, arg) => bounded("worker", () => worker.evaluate(fn, arg));
    try {
        const extension = path.join(artifacts, "extension");
        await fs.mkdir(extension);
        for (const [source, target] of [
            ["../chromium/external-links/extension/playwright-transport.js", "playwright-transport.js"],
            ["browser_extension_engine_worker.js", "base-worker.js"],
            ["browser_capture_matrix_worker.js", "worker.js"],
        ]) await fs.copyFile(path.join(__dirname, source), path.join(extension, target));
        await fs.writeFile(path.join(extension, "manifest.json"), JSON.stringify({
            manifest_version: 3, name: "Private capture matrix", version: "1.0",
            permissions: ["debugger", "tabs", "scripting"], host_permissions: ["http://127.0.0.1/*"],
            background: {service_worker: "worker.js"},
        }));
        owned = await launchUnmanagedPipeFixture({playwright, artifacts, observePages: false,
            executablePath: process.env.ABUSH_ENGINE_EXTENSION_CHROME});
        record.pid = owned.pid;
        const root = await owned.browser.newBrowserCDPSession();
        const ready = owned.context.waitForEvent("serviceworker", {predicate: worker => worker.url().endsWith("/worker.js"), timeout: 5000});
        ready.catch(() => {});
        const loaded = await bounded("extension load", () => root.send("Extensions.loadUnpacked", {path: extension}));
        worker = owned.context.serviceWorkers().find(w => w.url().startsWith(`chrome-extension://${loaded.id}/`)) || await ready;
        await bounded("worker committed", () => waitForWorkerFixture(worker), 11000);
        await root.detach();
        const setup = await evaluate(() => fixture.setup());
        record.tabId = setup.tabId;
        record.targetId = setup.target.targetInfo.targetId;
        record.personalBefore = await evaluate(() => fixture.personal());
        record.activeBefore = record.personalBefore.filter(tab => tab.active).map(tab => tab.id);
        record.initialDOM = await evaluate(() => fixture.pageState());
        function dispatch(message) {
            const task = evaluate(message => fixture.handle(message), message);
            tasks.add(task);
            task.catch(error => { if (!stopping) errors.push(error.message); })
                .finally(() => tasks.delete(task));
        }
        engine = new Engine({playwright, artifactsDir: artifacts,
            onFatal: error => { if (!stopping) errors.push(error.message); },
            send: message => {
                if (message.type === "cdp") dispatch(message.message);
                else replies.get(message.request_id)?.(message);
            }});
        pump = (async () => {
            while (!stopping) {
                const batch = await worker.evaluate(() => fixture.next());
                for (const item of batch) {
                    if (item.type === "stop") return;
                    if (item.type === "cdp") await engine.receive({type: "cdp", lease_id: "matrix", message: item.message});
                }
            }
        })();
        pump.catch(error => { if (!stopping) errors.push(error.message); });
        let sequence = 0;
        async function receive(type, action) {
            const request_id = `matrix-${++sequence}`;
            const response = new Promise(resolve => replies.set(request_id, resolve));
            try {
                const sending = engine.receive({type, lease_id: "matrix", request_id, ...(action ? {action} : {})});
                await bounded(type, () => sending);
                const result = await bounded(`${type} reply`, () => response);
                assert.equal(result.ok, true, result.error);
                return result;
            } finally { replies.delete(request_id); }
        }
        await receive("open");
        const lease = engine.leases.get("matrix");
        await receive("execute", {type: "navigate", url: `${origin}/${row}`});
        // Explicit DOM-commit barrier; no timing sleeps or animation pacing assumptions.
        await bounded("static marker committed", () => lease.page.evaluate(() => new Promise(resolve => {
            const done = () => {
                if (document.querySelector("#bottom") && document.readyState === "complete") {
                    document.removeEventListener("readystatechange", done);
                    resolve(true);
                }
            };
            document.addEventListener("readystatechange", done); done();
        })));
        const dom = () => ({width: innerWidth, height: innerHeight, dpr: devicePixelRatio,
            scrollWidth: document.documentElement.scrollWidth, scrollHeight: document.documentElement.scrollHeight,
            visibility: document.visibilityState, focused: document.hasFocus(), ready: document.readyState,
            bottom: document.querySelector("#bottom").getBoundingClientRect().toJSON()});
        record.before = await bounded("DOM metrics", () => lease.page.evaluate(dom));
        record.captureDispatchMs = performance.now();
        try {
            let png;
            const fullPage = row.endsWith("_full");
            if (row.startsWith("production")) {
                record.request = {api: "Page.screenshot", fullPage, timeout: 10000};
                const {api, ...options} = record.request;
                png = await bounded("official production capture", () => lease.page.screenshot(options), 11000);
            } else {
                const params = {format: "png", captureBeyondViewport: fullPage, clip: {
                    x: 0, y: 0, width: fullPage ? record.before.scrollWidth : record.before.width,
                    height: fullPage ? record.before.scrollHeight : record.before.height, scale: 1,
                }};
                record.request = {method: "Page.captureScreenshot", params};
                const result = await evaluate(params => fixture.rawCapture(params), params);
                png = Buffer.from(result.data, "base64");
            }
            record.captureCompletionMs = performance.now();
            record.image = decode(png);
            await fs.writeFile(path.join(artifacts, "capture.png"), png);
            const expectFull = row.endsWith("_full");
            record.captureValid = record.image.topPixels > 50 && (expectFull ?
                record.image.height >= 1600 && record.image.bottomPresent :
                record.image.height === record.before.height && !record.image.bottomPresent);
        } catch (error) {
            record.captureCompletionMs = performance.now();
            record.error = error.message.split("\n")[0];
            record.captureValid = false;
        }
        record.after = await bounded("post DOM", () => lease.page.evaluate(dom));
        record.workActive = (await evaluate(() => fixture.tabs()))[0].active;
        record.personalAfter = await evaluate(() => fixture.personal());
        record.activeAfter = record.personalAfter.filter(tab => tab.active).map(tab => tab.id);
        record.personalUnchanged = JSON.stringify(record.personalBefore) === JSON.stringify(record.personalAfter);
        record.telemetry = await evaluate(() => fixture.matrixState());
        assert.equal(record.personalUnchanged, true);
        assert.equal(record.workActive, false);
        assert.deepEqual(record.telemetry.forbidden, []);
        assert.equal(record.telemetry.calls.filter(call => call.method === "Page.captureScreenshot").length, 1);
        owned.assertObserverDefaults();
    } catch (error) { record.infrastructureError = error.stack; }
    finally {
        stopping = true;
        // Detach first: pending native captures must not hold the Engine teardown queue.
        if (worker) {
            try { await evaluate(() => fixture.release()); } catch (error) { errors.push(`detach: ${error.message}`); }
            try { await evaluate(() => fixture.stop()); } catch (error) { errors.push(`stop: ${error.message}`); }
        }
        if (engine) {
            try { engine.leases.get("matrix")?.close("Diagnostic finished"); }
            catch (error) { errors.push(`lease: ${error.message}`); }
        }
        if (pump) {
            try { await bounded("pump join", () => pump); } catch (error) { errors.push(`pump: ${error.message}`); }
        }
        try { await bounded("transport joins", () => Promise.allSettled([...tasks])); }
        catch (error) { errors.push(error.message); }
        if (owned) {
            try { await bounded("private Chrome shutdown", () => owned.shutdown(), 15000); }
            catch (error) { errors.push(`shutdown: ${error.message}`); }
        }
        record.cleanupErrors = errors;
        record.endMs = performance.now();
    }
    return record;
}
(async () => {
    await fs.mkdir(tmp, {recursive: true});
    const suite = await fs.mkdtemp(path.join(tmp, "m-"));
    process.env.TMPDIR = suite;
    const playwright = await loadOfficialPlaywright(suite);
    const website = http.createServer((request, response) => {
        response.writeHead(200, {"content-type": "text/html", "cache-control": "no-store"});
        response.end(`<!doctype html><meta charset="utf-8"><title>Static capture matrix</title>
            <style>html,body{margin:0;padding:0;background:#ddd}body{width:700px;height:1600px}
            html{scrollbar-gutter:stable}#top,#bottom{position:absolute;left:0;width:700px;height:100px}
            #top{top:0;background:rgb(20,40,220)}#bottom{top:1500px;background:rgb(20,200,40)}</style>
            <div id="top"></div><div id="bottom"></div>`);
    });
    const listening = once(website, "listening");
    website.listen(0, "127.0.0.1"); await listening;
    const results = [];
    try {
        for (const row of rows) {
            const artifacts = await fs.mkdtemp(path.join(suite, "r-"));
            const result = await runRow(row, `http://127.0.0.1:${website.address().port}`, artifacts, playwright);
            results.push(result);
            await fs.writeFile(path.join(suite, "matrix.json"), JSON.stringify({engineRoot,
                immutableEngine: await fs.realpath(engineRoot), chrome: process.env.ABUSH_ENGINE_EXTENSION_CHROME,
                results}, null, 2));
            console.log(JSON.stringify({row:result.row, valid:result.captureValid, image:result.image,
                durationMs:result.captureCompletionMs-result.captureDispatchMs, error:result.error,
                before:result.before, after:result.after, personalUnchanged:result.personalUnchanged,
                cleanupErrors:result.cleanupErrors, infrastructureError:result.infrastructureError}));
        }
        console.log(JSON.stringify({diagnosticOnly: true, report: path.join(suite, "matrix.json"),
            valid: results.filter(r => r.captureValid).length, total: results.length,
            infrastructureFailures: results.filter(r => r.infrastructureError || r.cleanupErrors.length).length}));
        if (results.some(r => r.infrastructureError || r.cleanupErrors.length || !r.personalUnchanged ||
            (r.row.startsWith("production") && !r.captureValid))) process.exitCode = 1;
    } finally {
        website.closeAllConnections();
        await bounded("HTTP fixture close", () => new Promise(resolve => website.close(resolve)));
    }
})().catch(error => { console.error(error); process.exitCode = 1; });
