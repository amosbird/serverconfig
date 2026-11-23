"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const {spawn, execFileSync} = require("node:child_process");

async function bounded(label, task, ms = 10000) {
    let timer;
    try {
        return await Promise.race([Promise.resolve().then(task), new Promise((_, reject) => {
            timer = setTimeout(() => reject(new Error(`Deadline: ${label}`)), ms);
        })]);
    } finally { clearTimeout(timer); }
}

async function launchUnmanagedPipeFixture({playwright, executablePath, artifacts, observePages = true,
    fixtureHostPort = null}) {
    assert.equal(process.env.ABUSH_ENGINE_FIXTURE, "1", "private fixture launch only");
    assert.ok(path.isAbsolute(executablePath) && path.isAbsolute(artifacts));
    const profile = path.join(artifacts, "profile");
    await fs.mkdir(profile, {recursive: false, mode: 0o700});
    assert.ok(fixtureHostPort === null || (Number.isInteger(fixtureHostPort) &&
        fixtureHostPort > 0 && fixtureHostPort <= 65535), "valid private website port");
    const websiteArgs = fixtureHostPort === null ? [] : [
        `--host-resolver-rules=MAP fixture.test 127.0.0.1:${fixtureHostPort}, MAP * ~NOTFOUND`,
        "--no-proxy-server",
    ];
    const args = ["--headless=new", "--remote-debugging-pipe", ...websiteArgs,
        // Installed component extensions are not part of this private fixture.
        "--disable-component-extensions-with-background-pages",
        "--enable-unsafe-extension-debugging", "--no-first-run", "--no-default-browser-check",
        "--no-sandbox", "--window-size=800,743", `--user-data-dir=${profile}`, "about:blank"];
    const child = spawn(executablePath, args, {
        stdio: ["ignore", "ignore", "pipe", "pipe", "pipe"],
        env: {...process.env, TMPDIR: artifacts},
    });
    const pid = child.pid;
    const failures = [], commands = [], observedTargets = [];
    let stderr = "", buffer = Buffer.alloc(0), closed = false, stopping = false;
    let pendingBytes = 0, pendingWrites = 0, frameTimer;
    const input = child.stdio[3], output = child.stdio[4];
    const maxBytes = 8 * 1024 * 1024;
    const exited = new Promise(resolve => child.once("exit", (code, signal) => resolve({code, signal})));
    const transport = {
        send(message) {
            if (closed) throw new Error("Owned Chrome pipe closed");
            if (message.method) commands.push(message.method);
            assert.ok(commands.length <= 100000, "observer command trace bounded");
            // Service-worker-only observation avoids a second debugger client on Engine's exact page.
            if (!observePages && message.method === "Target.setAutoAttach" && !message.sessionId) {
                message = {...message, params: {...message.params,
                    filter: [{type: "service_worker", exclude: false}, {exclude: true}]}};
            }
            const frame = Buffer.from(JSON.stringify(message) + "\0");
            if (frame.length > maxBytes || pendingBytes + frame.length > maxBytes ||
                    pendingWrites >= 1024) throw new Error("Owned Chrome pipe backpressure");
            pendingBytes += frame.length;
            pendingWrites++;
            const timer = setTimeout(() => fail(new Error("Owned Chrome pipe write deadline")), 10000);
            input.write(frame, error => {
                clearTimeout(timer); pendingBytes -= frame.length; pendingWrites--;
                if (error) fail(error);
            });
        },
        close() {
            if (closed) return;
            closed = true;
            clearTimeout(frameTimer);
            input.destroy(); output.destroy();
            transport.onclose?.("Owned Chrome pipe closed");
        },
    };
    function fail(error) {
        if (!stopping) failures.push(error);
        transport.close();
    }
    child.stderr.on("data", chunk => { stderr = (stderr + chunk.toString()).slice(-16384); });
    child.on("error", fail);
    input.on("error", fail); output.on("error", fail);
    output.on("end", () => {
        if (!stopping) fail(new Error(`Owned Chrome pipe ended: ${stderr}`));
    });
    output.on("data", chunk => {
        try {
            buffer = Buffer.concat([buffer, chunk]);
            let end;
            while ((end = buffer.indexOf(0)) !== -1) {
                if (end > maxBytes) throw new Error("Owned Chrome CDP frame oversized");
                const message = JSON.parse(buffer.subarray(0, end).toString("utf8"));
                buffer = buffer.subarray(end + 1);
                if (message.method === "Target.attachedToTarget") {
                    observedTargets.push(message.params.targetInfo.type);
                }
                if (!transport.onmessage) throw new Error("Owned Chrome observer not registered");
                transport.onmessage(message);
            }
            if (buffer.length > maxBytes) throw new Error("Owned Chrome CDP frame oversized");
            if (!buffer.length) { clearTimeout(frameTimer); frameTimer = undefined; }
            else if (!frameTimer) frameTimer = setTimeout(() =>
                fail(new Error("Owned Chrome incomplete CDP frame deadline")), 10000);
        } catch (error) { fail(error); }
    });
    child.once("exit", (code, signal) => {
        if (!stopping) fail(new Error(`Owned Chrome ${pid} exited ${code}/${signal}: ${stderr}`));
    });
    let browser;
    function assertObserverDefaults() {
        assert.deepEqual(commands.filter(method =>
            /setFocusEmulationEnabled|setWebLifecycleState|setDeviceMetricsOverride/.test(method)), [],
        "noDefaults observer must not prepare focus, background lifecycle or viewport");
        if (!observePages) assert.deepEqual(observedTargets.filter(type =>
            type !== "service_worker" && type !== "browser"), [],
        "service-worker observer must never attach to page targets");
        if (failures.length) throw new AggregateError(failures, `Owned Chrome pipe ${pid}`);
    }
    async function shutdown() {
        stopping = true;
        const errors = [...failures];
        if (browser?.isConnected()) {
            try {
                const root = await bounded("owned shutdown browser session", () => browser.newBrowserCDPSession(), 3000);
                await bounded("owned browser graceful close", () => root.send("Browser.close"), 5000);
                await bounded(`owned Chrome ${pid} graceful join`, () => exited, 5000);
            } catch (error) { errors.push(error); }
            try { await bounded("observer disconnect", () => browser.close(), 5000); }
            catch (error) { errors.push(error); }
        }
        transport.close();
        if (child.exitCode === null && child.signalCode === null && pid) {
            child.kill("SIGTERM");
            try { await bounded(`owned Chrome ${pid} TERM join`, () => exited, 5000); }
            catch (error) {
                errors.push(error);
                if (child.exitCode === null && child.signalCode === null) child.kill("SIGKILL");
                try { await bounded(`owned Chrome ${pid} KILL join`, () => exited, 2000); }
                catch (joinError) { errors.push(joinError); }
            }
        }
        if (errors.length) throw new AggregateError(errors, `Owned Chrome ${pid} teardown`);
    }
    try {
        await fs.writeFile(path.join(artifacts, "chrome-process.json"),
            JSON.stringify({pid, executablePath, args}));
        browser = await playwright.chromium.connectOverCDP(transport,
            {noDefaults: true, artifactsDir: artifacts, timeout: 10000});
        assertObserverDefaults();
        const context = browser.contexts()[0];
        assert.ok(context, "owned Chrome default context");
        return {browser, context, pid, child, commands, assertObserverDefaults, shutdown};
    } catch (error) {
        try { await shutdown(); } catch (teardown) {
            throw new AggregateError([error, teardown], "Owned Chrome launch and teardown");
        }
        throw error;
    }
}
async function waitForWorkerFixture(worker) {
    let resolve;
    const committed = new Promise(yes => { resolve = yes; });
    const listener = message => {
        if (message.text() === "fixture-worker-ready") resolve();
    };
    worker.on("console", listener);
    try {
        if (await worker.evaluate(() => typeof globalThis.fixture !== "undefined")) resolve();
        await bounded("extension fixture script committed", () => committed);
    } finally { worker.off("console", listener); }
}
module.exports = {launchUnmanagedPipeFixture, waitForWorkerFixture};

if (require.main === module) {
    (async () => {
        const common = execFileSync("git", ["rev-parse", "--path-format=absolute", "--git-common-dir"],
            {cwd: path.join(__dirname, ".."), encoding: "utf8", timeout: 5000}).trim();
        const tmp = path.join(path.dirname(common), ".worktrees/.tmp");
        await fs.mkdir(tmp, {recursive: true});
        const artifacts = await fs.mkdtemp(path.join(tmp, "p-"));
        let fixture;
        try {
            const {loadOfficialPlaywright} = require(path.join(process.env.ABUSH_BROWSER_ENGINE_ROOT,
                "engine.cjs"));
            fixture = await launchUnmanagedPipeFixture({artifacts,
                executablePath: process.env.ABUSH_ENGINE_EXTENSION_CHROME,
                playwright: await loadOfficialPlaywright(artifacts)});
            const root = await fixture.browser.newBrowserCDPSession();
            const info = await root.send("SystemInfo.getProcessInfo");
            assert.equal(info.processInfo.find(process => process.type === "browser").id, fixture.pid);
            assert.equal(fixture.context.pages().length, 1);
            assert.equal(await bounded("owned visible page observation", () =>
                fixture.context.pages()[0].evaluate(() => document.visibilityState)), "visible");
            await root.detach();
            fixture.assertObserverDefaults();
            console.log("PASS unmanaged owned Chrome PIPE identity and official noDefaults observer");
        } finally {
            try { await fixture?.shutdown(); }
            finally { await fs.rm(artifacts, {recursive: true, force: true}); }
        }
    })().catch(error => { console.error(error); process.exitCode = 1; });
}
