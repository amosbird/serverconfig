#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const http = require("node:http");
const {once} = require("node:events");
const {inflateSync} = require("node:zlib");
const {launchUnmanagedPipeFixture, waitForWorkerFixture} = require("./browser_unmanaged_pipe_fixture.js");
const engineRoot = process.env.ABUSH_BROWSER_ENGINE_ROOT;
assert.equal(process.env.ABUSH_ENGINE_FIXTURE, "1");
assert.ok(engineRoot && process.env.ABUSH_ENGINE_EXTENSION_CHROME);
const {Engine, loadOfficialPlaywright} = require(path.join(engineRoot, "engine.cjs"));
const {execFileSync} = require("node:child_process");
const common = execFileSync("git", ["rev-parse", "--path-format=absolute", "--git-common-dir"],
    {cwd: path.join(__dirname, ".."), encoding: "utf8", timeout: 5000}).trim();
const tmp = path.join(path.dirname(common), ".worktrees/.tmp");
const rows = process.env.ABUSH_CAPTURE_ROWS?.split(",") || ["official_full", "raw_viewport_no_clip", "raw_viewport_clip", "raw_beyond_clip",
    "official_metrics_dontSetVisibleSize_true", "official_metrics_dontSetVisibleSize_false"];
async function bounded(label, task, ms = 3000) {
    let timer;
    try {
        return await Promise.race([Promise.resolve().then(task), new Promise((_, reject) => {
            timer = setTimeout(() => reject(new Error(`Deadline: ${label}`)), ms);
        })]);
    } finally { clearTimeout(timer); }
}
function decode(png) {
    assert.deepEqual(png.subarray(0, 8), Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]));
    const width = png.readUInt32BE(16), height = png.readUInt32BE(20);
    assert.equal(png[24], 8); assert.equal(png[28], 0);
    const channels = {2: 3, 6: 4}[png[25]];
    assert.ok(channels, `Unsupported PNG color type ${png[25]}`);
    assert.ok(width * height < 8000000);
    const chunks = [];
    for (let offset = 8; offset < png.length;) {
        const length = png.readUInt32BE(offset);
        if (png.toString("ascii", offset + 4, offset + 8) === "IDAT") {
            chunks.push(png.subarray(offset + 8, offset + 8 + length));
        }
        offset += length + 12;
    }
    const stride = width * channels, raw = inflateSync(Buffer.concat(chunks), {maxOutputLength: 40000000});
    assert.equal(raw.length, (stride + 1) * height);
    let previous = Buffer.alloc(stride), topPixels = 0, bottomPixels = 0;
    function paeth(a, b, c) {
        const p = a + b - c, pa = Math.abs(p - a), pb = Math.abs(p - b), pc = Math.abs(p - c);
        return pa <= pb && pa <= pc ? a : pb <= pc ? b : c;
    }
    for (let y = 0; y < height; y++) {
        const filter = raw[y * (stride + 1)];
        assert.ok(filter <= 4);
        const scan = Buffer.from(raw.subarray(y * (stride + 1) + 1, (y + 1) * (stride + 1)));
        for (let x = 0; x < stride; x++) {
            const a = x >= channels ? scan[x - channels] : 0, b = previous[x];
            const c = x >= channels ? previous[x - channels] : 0;
            scan[x] = (scan[x] + [0, a, b, Math.floor((a + b) / 2), paeth(a, b, c)][filter]) & 255;
        }
        const x = Math.min(350, width - 1) * channels;
        if (scan[x] === 20 && scan[x + 1] === 40 && scan[x + 2] === 220) topPixels++;
        if (scan[x] === 20 && scan[x + 1] === 200 && scan[x + 2] === 40) bottomPixels++;
        previous = scan;
    }
    return {width, height, topPixels, bottomPixels, bottomPresent: bottomPixels > 50};
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
        const ready = owned.context.waitForEvent("serviceworker", {timeout: 3000});
        ready.catch(() => {});
        const loaded = await bounded("extension load", () => root.send("Extensions.loadUnpacked", {path: extension}));
        worker = owned.context.serviceWorkers().find(w => w.url().startsWith(`chrome-extension://${loaded.id}/`)) || await ready;
        await bounded("worker committed", () => waitForWorkerFixture(worker));
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
        const raw = (method, params) => evaluate(({method, params}) => fixture.raw(method, params), {method, params});
        if (row.includes("metrics")) {
            record.override = {width: row.includes("_content") ? record.before.scrollWidth : record.before.width,
                height: row.includes("_document") ? record.before.scrollHeight : record.before.height,
                deviceScaleFactor: 0, mobile: false, dontSetVisibleSize: row.endsWith("_true")};
            if (row.includes("_viewport")) record.override.viewport = {x: 0, y: 0,
                width: record.before.width, height: record.before.scrollHeight, scale: 1};
            await raw("Emulation.setDeviceMetricsOverride", record.override);
            // Response-ordered test barrier, not a second capture or window-resize path.
            record.afterOverride = await bounded("override DOM barrier", () => lease.page.evaluate(dom));
        }
        if (row.includes("_paint_barrier")) {
            await bounded("two committed animation frames", () => lease.page.evaluate(() =>
                new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(() => resolve(true))))));
        }
        record.captureDispatchMs = performance.now();
        try {
            let png;
            if (row.startsWith("official")) {
                record.request = {api: "Page.screenshot", fullPage: true, timeout: 2800,
                    ...(row === "official_caret_initial" ? {caret: "initial"} : {}),
                    ...(row.startsWith("official_style_scrollbar") ? {style: ':root { scrollbar-width: none !important; }'} : {})};
                const {api, ...options} = record.request;
                png = await bounded("official capture", () => lease.page.screenshot(options));
            } else {
                const beyond = row.startsWith("raw_beyond");
                const params = {format: "png", ...(row.endsWith("_implicit_surface") ? {} : {fromSurface: true}), captureBeyondViewport: beyond};
                if (row !== "raw_viewport_no_clip") params.clip = {x: 0, y: 0,
                    width: row.includes("_content") ? record.before.scrollWidth : row.includes("_wide") ? record.before.width + 20 : record.before.width,
                    height: beyond ? 1600 : record.before.height, scale: 1};
                record.request = {method: "Page.captureScreenshot", params};
                const result = await raw("Page.captureScreenshot", params);
                png = Buffer.from(result.data, "base64");
            }
            record.captureCompletionMs = performance.now();
            record.image = decode(png);
            await fs.writeFile(path.join(artifacts, "capture.png"), png);
            const expectFull = row.startsWith("official") || row.startsWith("raw_beyond");
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
            #top,#bottom{position:absolute;left:0;width:700px;height:100px}
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
        console.log(JSON.stringify({report: path.join(suite, "matrix.json"),
            valid: results.filter(r => r.captureValid).length, total: results.length,
            infrastructureFailures: results.filter(r => r.infrastructureError || r.cleanupErrors.length).length}));
        if (results.some(r => r.infrastructureError || r.cleanupErrors.length || !r.personalUnchanged)) process.exitCode = 1;
    } finally {
        website.closeAllConnections();
        await bounded("HTTP fixture close", () => new Promise(resolve => website.close(resolve)));
    }
})().catch(error => { console.error(error); process.exitCode = 1; });
