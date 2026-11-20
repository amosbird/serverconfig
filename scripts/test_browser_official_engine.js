#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const net = require("node:net");
const {spawn} = require("node:child_process");
const {once} = require("node:events");
const {gzipSync} = require("node:zlib");
const http = require("node:http");
const {AuthorizedCdpBridge} = require("../chromium/external-links/extension/playwright-transport.js");
const engineDir = process.env.ABUSH_BROWSER_ENGINE;
const chromePath = process.env.ABUSH_ENGINE_CHROME;
if (process.env.ABUSH_ENGINE_FIXTURE !== "1" || !engineDir || !chromePath) {
    throw new Error("Requires ABUSH_ENGINE_FIXTURE=1, ABUSH_BROWSER_ENGINE and ABUSH_ENGINE_CHROME; fresh pipe fixture only");
}
const {Engine, loadOfficialPlaywright} = require(path.join(engineDir, "engine.cjs"));
const {FramedSocket, encodeFrame} = require(path.join(engineDir, "protocol.cjs"));
function bus() {
    const listeners = new Set();
    return {addListener: fn => listeners.add(fn), removeListener: fn => listeners.delete(fn),
        emit: (...args) => { for (const fn of listeners) fn(...args); }};
}
async function bounded(name, run, ms = 15000) {
    let timer;
    try {
        return await Promise.race([Promise.resolve().then(run), new Promise((_, reject) => {
            timer = setTimeout(() => reject(new Error(`Deadline: ${name}`)), ms);
        })]);
    } finally { clearTimeout(timer); }
}
(async () => {
    const artifacts = await fs.mkdtemp(path.join(engineDir, "test/.bridge-fixture-"));
    const oldTmp = process.env.TMPDIR;
    process.env.TMPDIR = artifacts;
    let browser, engine, bridge, session, fixturePid, child, framed, peer, client, server, website;
    let sequence = 0;
    const waiters = new Map();
    const calls = [], replies = [], errors = [], protocolErrors = [];
    try {
        await bounded("whole fresh-browser journey", async () => {
        const playwright = await loadOfficialPlaywright(artifacts);
        browser = await bounded("fresh Chromium pipe launch", () => playwright.chromium.launch({
            executablePath: chromePath, headless: true, timeout: 10000, artifactsDir: artifacts,
        }));
        const browserSession = await browser.newBrowserCDPSession();
        const processes = await browserSession.send("SystemInfo.getProcessInfo");
        fixturePid = processes.processInfo.find(info => info.type === "browser").id;
        await browserSession.detach();
        website = http.createServer((request, response) => {
            if (request.url === "/compressed") {
                const body = gzipSync(Buffer.from("x".repeat(300000)));
                response.writeHead(200, {"content-type": "text/plain", "content-encoding": "gzip",
                    "content-length": String(body.length), "access-control-allow-origin": "*"});
                response.end(body);
            } else {
                response.writeHead(200, {"content-type": "text/plain", "access-control-allow-origin": "*"});
                response.write("x".repeat(150000)); response.end("x".repeat(150000));
            }
        });
        website.listen(0, "127.0.0.1");
        await bounded("website fixture listen (not automation transport)", () => once(website, "listening"));
        const origin = `http://127.0.0.1:${website.address().port}`;
        const page = await browser.newPage({viewport: {width: 800, height: 600}});
        await page.context().route("**/*", async route => {
            const url = new URL(route.request().url());
            if (url.origin === origin) return route.continue();
            if (url.pathname === "/api") return route.fulfill({contentType: "application/json",
                headers: {"set-cookie": "private=secret"}, body: JSON.stringify({answer: 42, message: "hello"})});
            if (url.pathname === "/large") return route.fulfill({contentType: "text/plain", body: "x".repeat(300000)});
            if (url.pathname === "/binary") return route.fulfill({contentType: "application/octet-stream",
                body: Buffer.from([0, 1, 255, 2])});
            if (url.pathname === "/failed") return route.abort("failed");
            return route.fulfill({contentType: "text/html", body: `<!doctype html><title>Fixture</title>
                <button onclick="fetch('/api',{method:'POST',headers:{Authorization:'secret'},body:'question'}).then(r=>r.json()).then(v=>{console.log('answer',v.answer);document.querySelector('output').textContent='Ready 42'})">Load</button>
                <input aria-label="Name"><select aria-label="Choice"><option value="one">One</option><option value="two">Two</option></select>
                <input type="checkbox" aria-label="Agree"><output></output><div style="height:1300px">Tall content</div>`});
        });
        session = await page.context().newCDPSession(page);
        const onEvent = bus(), onDetach = bus();
        // Test-only event tap: CDPSession has no wildcard event API. Commands and event payloads
        // are the real public CDPSession protocol, not fabricated Playwright/DOM responses.
        const emit = session.emit;
        session.emit = function(method, params) {
            if (typeof method === "string" && method.includes(".")) onEvent.emit({tabId: 17}, method, params);
            return emit.call(this, method, params);
        };
        const chrome = {debugger: {onEvent, onDetach, sendCommand: async (source, method, params) => {
            assert.equal(source.tabId, 17);
            assert.equal(source.sessionId, undefined, "This fixture exercises the exact root page");
            calls.push({method, params});
            return session.send(method, params);
        }}};
        bridge = new AuthorizedCdpBridge({chrome, tabId: 17, checkAuthorization: () => true,
            onError: error => errors.push(error), send: message => {
                if (message.error) protocolErrors.push(message);
                queueMicrotask(() => { void engine.receive({type: "cdp", lease_id: "fresh", message}); });
            }});
        const output = message => {
            encodeFrame(message);
            if (message.type === "cdp") void bridge.handle(message.message);
            else { replies.push(message); waiters.get(message.request_id)?.(message); }
        };
        if (process.env.ABUSH_ENGINE_CLI === "1") {
            server = net.createServer();
            server.listen(path.join(artifacts, "engine.sock"));
            await bounded("Unix listener", () => once(server, "listening"));
            const accepted = once(server, "connection");
            client = net.connect(path.join(artifacts, "engine.sock"));
            await bounded("Unix connect", () => once(client, "connect"));
            [peer] = await bounded("Unix accept", () => accepted);
            client.pause();
            // Test-only FD extraction for the inherited AF_UNIX production CLI contract.
            child = spawn(process.execPath, [path.join(engineDir, "main.cjs"), "--fd", "3",
                "--artifacts-dir", artifacts], {stdio: ["ignore", "ignore", "pipe", client._handle.fd]});
            client.destroy();
            child.stderr.on("data", bytes => errors.push(new Error(bytes.toString())));
            framed = new FramedSocket(peer, output, error => {
                if (error.message !== "EOF" && waiters.size) errors.push(error);
            });
            engine = {receive: message => framed.send(message), dispose: () => peer.end()};
        } else engine = new Engine({playwright, artifactsDir: artifacts,
            onFatal: error => errors.push(error), send: output});
        const receive = async (type, action) => {
            const request_id = `fixture-${++sequence}`;
            const reply = new Promise(resolve => waiters.set(request_id, resolve));
            try {
                await engine.receive({type, lease_id: "fresh", request_id, ...(action ? {action} : {})});
                return await bounded(`${type} ${action?.type || ""}`, () => reply);
            } finally { waiters.delete(request_id); }
        };
        const run = async action => {
            const reply = await receive("execute", action);
            assert.equal(reply.ok, true, reply.error);
            assert.ok(Buffer.byteLength(JSON.stringify(reply.result)) <= 700000);
            return reply.result;
        };
        assert.equal((await receive("open")).ok, true, JSON.stringify(replies));
        await run({type: "navigate", url: "http://fixture.test/"});
        const snapshot = await run({type: "snapshot"});
        const ref = /button "Load" \[ref=([^\]]+)\]/.exec(snapshot.text)[1];
        const namedRef = name => snapshot.text.split("\n").find(line => line.includes(`"${name}"`)).match(/\[ref=([^\]]+)\]/)[1];
        const withRef = (type, name, extra = {}) => run({type, snapshot_id: snapshot.data.snapshot_id,
            ref: namedRef(name), ...extra});
        await withRef("fill", "Name", {value: "Ada"});
        await withRef("press", "Name", {key: "End"});
        await withRef("select", "Choice", {values: ["two"]});
        await withRef("check", "Agree", {checked: true});
        await withRef("hover", "Load");
        assert.equal(await page.getByRole("textbox").inputValue(), "Ada");
        assert.equal(await page.getByRole("combobox").inputValue(), "two");
        assert.equal(await page.getByRole("checkbox").isChecked(), true);
        assert.match((await run({type: "text"})).text, /Tall content/);
        await run({type: "scroll", x: 0, y: 10});
        const apiFinished = bounded("API loadingFinished barrier", () => page.waitForEvent("requestfinished",
            request => request.url() === "http://fixture.test/api"));
        apiFinished.catch(error => errors.push(error));
        await run({type: "click", snapshot_id: snapshot.data.snapshot_id, ref});
        await apiFinished;
        await run({type: "wait", text: "Ready 42", timeout_ms: 5000});
        const list = await run({type: "network_list"});
        assert.ok(list.text.includes("http://fixture.test/api"));
        const request = list.data.requests.find(r => r.url.endsWith("/api"));
        assert.ok(request); assert.equal(request.method, "POST");
        assert.equal(request.resource_type, "fetch"); assert.equal(request.post_data, "question");
        assert.equal(request.headers.authorization, "[REDACTED]");
        assert.equal(JSON.stringify(request.response_headers).includes("private=secret"), false);
        const body = await run({type: "network_body", request_id: request.request_id});
        assert.equal(body.data.status, "available", JSON.stringify(body));
        assert.deepEqual(body.data.json, {answer: 42, message: "hello"});
        assert.match(body.text, /hello/); assert.equal(body.data.data_base64, undefined);
        assert.deepEqual(await run({type: "network_body", request_id: request.request_id}), body);
        assert.equal((await run({type: "network_list"})).data.requests.find(r => r.url.endsWith("/api")).request_id, request.request_id);
        assert.ok((await run({type: "console"})).data.entries.some(e => e.text.includes("answer 42")));
        const value = await run({type: "evaluate", expression: "typeof process + ':' + document.title"});
        assert.equal(value.data.value, "undefined:Fixture");
        for (const full_page of [false, true]) {
            const shot = await run({type: "screenshot", full_page});
            const png = Buffer.from(shot.images[0].data_base64, "base64");
            assert.ok(png.length <= 500000); assert.equal(png.subarray(1, 4).toString(), "PNG");
            assert.equal(png.readUInt32BE(16), 800);
            assert.ok(full_page ? png.readUInt32BE(20) > 600 : png.readUInt32BE(20) === 600);
        }
        await run({type: "evaluate", expression: "document.querySelector('button').outerHTML='<button>Replacement</button>'"});
        const stale = await receive("execute", {type: "click", snapshot_id: snapshot.data.snapshot_id, ref});
        assert.equal(stale.ok, false); assert.match(stale.error, /ref|snapshot|resolve/i);
        await run({type: "navigate", url: "http://fixture.test/next"});
        assert.equal((await receive("execute", {type: "click", snapshot_id: snapshot.data.snapshot_id, ref})).ok, false);
        await run({type: "navigate", url: `${origin}/fixture`});
        for (const endpoint of ["large", "compressed"]) {
            const finished = bounded(`${endpoint} loadingFinished barrier`, () => page.waitForEvent("requestfinished",
                request => request.url() === `${origin}/${endpoint}`));
            finished.catch(error => errors.push(error));
            await run({type: "evaluate", expression: `fetch('${origin}/${endpoint}').then(r=>r.text()).then(()=>true)`});
            await finished;
            const large = (await run({type: "network_list"})).data.requests.find(r => r.url.endsWith(`/${endpoint}`));
            const before = calls.filter(c => c.method === "Network.getResponseBody").length;
            const unavailable = await run({type: "network_body", request_id: large.request_id});
            assert.equal(unavailable.data.status, "oversized", JSON.stringify(unavailable));
            assert.equal(calls.filter(c => c.method === "Network.getResponseBody").length, before);
        }
        await run({type: "navigate", url: "http://fixture.test/next"});
        const binaryFinished = bounded("binary loadingFinished barrier", () => page.waitForEvent("requestfinished",
            request => request.url() === "http://fixture.test/binary"));
        binaryFinished.catch(error => errors.push(error));
        await run({type: "evaluate", expression: "fetch('/binary').then(r=>r.arrayBuffer()).then(()=>true)"});
        await binaryFinished;
        const binaryRequest = (await run({type: "network_list"})).data.requests.find(r => r.url.endsWith("/binary"));
        const binary = await run({type: "network_body", request_id: binaryRequest.request_id});
        assert.equal(binary.text, "Binary body (base64)");
        assert.equal(binary.data.encoding, "base64");
        assert.deepEqual(Buffer.from(binary.data.data_base64, "base64"), Buffer.from([0, 1, 255, 2]));
        await run({type: "evaluate", expression: "fetch('/failed').catch(()=>false)"});
        const failed = (await run({type: "network_list"})).data.requests.find(r => r.url.endsWith("/failed"));
        assert.equal(failed.status, "failed");
        await receive("close"); bridge.close();
        assert.equal(browser.isConnected(), true); assert.equal(page.isClosed(), false);
        assert.equal(calls.some(c => /bringToFront|activateTarget|closeTarget|setDeviceMetrics|setWindow/.test(c.method)), false);
        assert.deepEqual(errors, []);
        assert.deepEqual(protocolErrors, []);
        console.log(`PASS fresh official ${process.env.ABUSH_ENGINE_CLI === "1" ? "CLI inherited AF_UNIX FD" : "Engine.receive"} + AuthorizedCdpBridge: ${[...new Set(calls.map(c => c.method))].sort().join(', ')}`);
        }, 60000);
    } finally {
        const teardownErrors = [];
        engine?.dispose(); bridge?.close();
        if (child) {
            const exited = child.exitCode !== null || child.signalCode !== null ? Promise.resolve() : once(child, "exit");
            try { await bounded("CLI EOF exit", () => exited, 3000); }
            catch (error) {
                child.kill("SIGKILL"); teardownErrors.push(error);
                await bounded("CLI abort exit", () => exited, 3000).catch(e => teardownErrors.push(e));
            }
            if (child.exitCode !== 0) teardownErrors.push(new Error(`CLI exited ${child.exitCode}`));
        }
        framed?.dispose(new Error("Fixture complete")); peer?.destroy(); client?.destroy();
        if (server) await bounded("Unix listener teardown", () => new Promise(resolve => server.close(resolve)), 3000)
            .catch(error => teardownErrors.push(error));
        if (browser) {
            try { await bounded("fresh fixture teardown", () => browser.close(), 5000); }
            catch (error) {
                // PID obtained only from the exact browser we just launched, never user Chrome.
                teardownErrors.push(error);
                if (fixturePid) {
                    try { process.kill(fixturePid, "SIGKILL"); }
                    catch (killError) { teardownErrors.push(killError); }
                }
            }
        }
        if (website) {
            website.closeAllConnections();
            await bounded("website fixture teardown", () => new Promise(resolve => website.close(resolve)), 3000)
                .catch(error => teardownErrors.push(error));
        }
        if (oldTmp === undefined) delete process.env.TMPDIR; else process.env.TMPDIR = oldTmp;
        await fs.rm(artifacts, {recursive: true, force: true});
        if (teardownErrors.length) throw new AggregateError(teardownErrors, "Fixture teardown failed");
    }
})().catch(error => { console.error(error); process.exitCode = 1; });
