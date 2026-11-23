#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const path = require("node:path");
const http = require("node:http");
const {once} = require("node:events");
const {execFileSync} = require("node:child_process");
const {decodePng} = require("./browser_capture_png_fixture.js");
const {launchUnmanagedPipeFixture, waitForWorkerFixture} = require("./browser_unmanaged_pipe_fixture.js");
const engineRoot = process.env.ABUSH_BROWSER_ENGINE_ROOT;
const executablePath = process.env.ABUSH_ENGINE_EXTENSION_CHROME;
if (process.env.ABUSH_ENGINE_FIXTURE !== "1" || !engineRoot || !executablePath) {
    throw new Error("Requires ABUSH_ENGINE_FIXTURE=1, ABUSH_BROWSER_ENGINE_ROOT and " +
        "ABUSH_ENGINE_EXTENSION_CHROME (extension-capable Chrome; isolated pipe fixture only)");
}
const {Engine, loadOfficialPlaywright} = require(path.join(engineRoot, "engine.cjs"));
async function bounded(label, run, ms = 15000) {
    let timer;
    try {
        return await Promise.race([Promise.resolve().then(run), new Promise((_, reject) => {
            timer = setTimeout(() => reject(new Error(`Deadline: ${label}`)), ms);
        })]);
    } finally { clearTimeout(timer); }
}
function barrier() {
    let resolve;
    const promise = new Promise(yes => { resolve = yes; });
    return {promise, resolve};
}
(async () => {
    const common = execFileSync("git", ["rev-parse", "--path-format=absolute", "--git-common-dir"],
        {cwd: path.join(__dirname, ".."), encoding: "utf8", timeout: 5000}).trim();
    const tmp = path.join(path.dirname(common), ".worktrees/.tmp");
    await fs.mkdir(tmp, {recursive: true});
    // Short prefix keeps Chrome's SingletonSocket pathname below the Unix socket limit.
    const artifacts = await fs.mkdtemp(path.join(tmp, "e-"));
    const oldTmp = process.env.TMPDIR;
    process.env.TMPDIR = artifacts;
    let context, browser, worker, engine, website, ownedChrome, pump;
    let stopping = false, engineReceiving = false, sequence = 0, fixtureReady = false, primaryError;
    const replies = new Map(), cdpReplies = new Map(), protocolErrors = [], failures = [];
    const emitted = [];
    const entered = barrier(), completed = barrier();
    const transportTasks = new Set();
    function evaluate(fn, argument) {
        return bounded("extension worker evaluation", () => worker.evaluate(fn, argument));
    }
    function dispatch(message) {
        const task = evaluate(message => fixture.handle(message), message);
        transportTasks.add(task);
        task.catch(error => failures.push(error)).finally(() => transportTasks.delete(task));
    }
    try {
        const extension = path.join(artifacts, "extension");
        await fs.mkdir(extension);
        await fs.copyFile(path.join(__dirname, "../chromium/external-links/extension/playwright-transport.js"),
            path.join(extension, "playwright-transport.js"));
        await fs.copyFile(path.join(__dirname, "browser_extension_engine_worker.js"),
            path.join(extension, "worker.js"));
        await fs.writeFile(path.join(extension, "manifest.json"), JSON.stringify({
            manifest_version: 3, name: "Owned Abush bridge regression", version: "1.0",
            permissions: ["debugger", "tabs", "tabGroups", "scripting"], host_permissions: ["http://127.0.0.1/*"],
            background: {service_worker: "worker.js"},
        }));
        website = http.createServer((request, response) => {
            response.writeHead(200, {"content-type": "text/html", "cache-control": "no-store"});
            response.end(`<!doctype html><title>Extension fixture ${request.url}</title>
                <style>html,body{margin:0;padding:0;background:#ddd}
                body{height:3200px}
                #top,#bottom{position:absolute;left:0;width:100%;height:100px}
                #top{top:0;background:rgb(20,40,220)}
                #bottom{top:3100px;background:rgb(20,200,40)}
                form{padding-top:120px}</style>
                <div id="top"></div><div id="bottom">Initial bottom</div>
                <form><input aria-label="Name"></form>
                <button onclick="document.querySelector('output').textContent='Hello '+document.querySelector('input').value">Apply</button>
                <output></output><p id="submitted"></p>
                <script>
                window.keyEvents = []; window.submits = [];
                for (const type of ['keydown', 'keypress', 'keyup']) {
                    document.querySelector('input').addEventListener(type, event => {
                        window.keyEvents.push({type:event.type, key:event.key, trusted:event.isTrusted});
                    });
                }
                document.querySelector('form').addEventListener('submit', event => {
                    event.preventDefault();
                    const value = document.querySelector('input').value;
                    window.submits.push({value, trusted:event.isTrusted});
                    document.querySelector('#submitted').textContent='Submitted '+value;
                });
                </script>`);
        });
        const listening = once(website, "listening");
        website.listen(0, "127.0.0.1");
        await bounded("page-only HTTP fixture listen", () => listening);
        const origin = `http://127.0.0.1:${website.address().port}`;
        const playwright = await loadOfficialPlaywright(artifacts);
        const deviceScaleFactor = process.env.ABUSH_ENGINE_TEST_DPR === undefined ? null : Number(process.env.ABUSH_ENGINE_TEST_DPR);
        ownedChrome = await launchUnmanagedPipeFixture({playwright, executablePath, artifacts,
            observePages: false, deviceScaleFactor});
        ({context, browser} = ownedChrome);
        const root = await browser.newBrowserCDPSession();
        const workerReady = context.waitForEvent("serviceworker", {
            predicate: candidate => candidate.url().endsWith("/worker.js"), timeout: 10000,
        });
        workerReady.catch(() => {});
        const loaded = await bounded("load actual unpacked MV3 extension", () =>
            root.send("Extensions.loadUnpacked", {path: extension}));
        worker = context.serviceWorkers().find(w => w.url().startsWith(`chrome-extension://${loaded.id}/`)) ||
            await bounded("actual MV3 service worker", () => workerReady);

        await waitForWorkerFixture(worker);
        fixtureReady = true;
        await bounded("setup root CDP detach", () => root.detach());
        const setup = await bounded("actual chrome.debugger attach and domain probe", () =>
            evaluate(() => fixture.setup()));
        assert.match(setup.rawVersionError, /Browser.getVersion.*wasn't found/);
        assert.equal(setup.target.targetInfo.type, "page");
        assert.equal(typeof setup.target.targetInfo.browserContextId, "string");
        assert.ok(setup.target.targetInfo.browserContextId.length > 0);
        assert.ok(worker.url().startsWith(`chrome-extension://${loaded.id}/`));
        console.log(`PASS actual chrome.debugger Browser domain restriction; Target.getTargetInfo: ${JSON.stringify(setup.target)}`);
        const before = await evaluate(() => fixture.tabs());
        const otherBefore = before[1];
        const personalBefore = await evaluate(() => fixture.personal());
        const presentationBefore = await evaluate(() => fixture.presentation());
        assert.equal(before[0].active, false);
        assert.equal(personalBefore.filter(tab => tab.active).length, 1);
        assert.deepEqual(await evaluate(() => fixture.pageState()), {visibility: "hidden", focused: false},
            "unmanaged observer must not emulate owned-page focus");
        console.log("PASS unmanaged work document initially hidden and unfocused");
        ownedChrome.assertObserverDefaults();
        engine = new Engine({playwright, artifactsDir: artifacts, onFatal: error => failures.push(error),
            send: message => {
                if (message.type === "cdp") dispatch(message.message);
                else replies.get(message.request_id)?.resolve(message);
            }});
        pump = (async () => {
            while (!stopping) {
                const messages = await worker.evaluate(() => fixture.next());
                for (const item of messages) {
                    if (item.type === "stop") return;
                    if (item.type === "late_entered") { entered.resolve(); continue; }
                    if (item.type === "late_completed") { completed.resolve(); continue; }
                    emitted.push(item.message);
                    if (item.message.error) protocolErrors.push(item.message);
                    if (cdpReplies.has(item.message.id)) cdpReplies.get(item.message.id).resolve(item.message);
                    else if (engineReceiving) await engine.receive({type: "cdp", lease_id: "extension", message: item.message});
                }
            }
        })();
        pump.catch(error => failures.push(error));
        async function cdp(message) {
            const waiting = barrier();
            cdpReplies.set(message.id, waiting);
            try { dispatch(message); return await bounded(message.method, () => waiting.promise); }
            finally { cdpReplies.delete(message.id); }
        }
        const version = await cdp({id: 100000, method: "Browser.getVersion"});
        assert.deepEqual(version.result, {protocolVersion: "1.3", product: "Chrome/Extension-Bridge",
            userAgent: setup.userAgent});
        assert.equal((await evaluate(() => fixture.state())).calls.some(c =>
            c.method.startsWith("Browser.")), false);
        console.log("PASS production bridge local root version; no Browser.* dispatched");
        async function receive(type, action) {
            const request_id = `extension-${++sequence}`, waiting = barrier();
            replies.set(request_id, waiting);
            try {
                return await bounded(`${type} ${action?.type || ""}`, async () => {
                    await engine.receive({type, lease_id: "extension", request_id, ...(action ? {action} : {})});
                    return waiting.promise;
                });
            } finally { replies.delete(request_id); }
        }
        async function run(action) {
            const reply = await receive("execute", action);
            assert.equal(reply.ok, true, reply.error);
            assert.ok(Buffer.byteLength(JSON.stringify(reply.result)) <= 700000);
            return reply.result;
        }
        const exactTargets = await cdp({id: 100005, method: "Target.getTargets"});
        assert.deepEqual(exactTargets.result.targetInfos.map(info => info.targetId),
            [setup.target.targetInfo.targetId]);
        assert.equal(exactTargets.result.targetInfos[0].browserContextId,
            setup.target.targetInfo.browserContextId);
        engineReceiving = true;
        const open = await receive("open");
        if (!open.ok) console.error("REAL EXTENSION INITIALIZATION FAILURE", JSON.stringify({open,
            protocolErrors, state: await evaluate(() => fixture.state())}));
        assert.equal(open.ok, true, open.error);
        const lease = engine.leases.get("extension");
        assert.equal(lease.browser.contexts().flatMap(c => c.pages()).length, 1);
        await run({type: "navigate", url: `${origin}/owned`});
        const preparedVisibility = (await run({type: "evaluate", expression:
            "({visibility:document.visibilityState,focused:document.hasFocus()})"})).data.value;
        assert.deepEqual(preparedVisibility, {visibility: "visible", focused: true},
            "only production bridge focus preparation may make the owned document ready for input");
        assert.equal((await evaluate(() => fixture.tabs()))[0].active, false);
        console.log(`PREPARED WORK DOCUMENT ${JSON.stringify(preparedVisibility)}`);
        const snapshot = await run({type: "snapshot"});
        const ref = name => snapshot.text.split("\n").find(line => line.includes(`"${name}"`))
            .match(/\[ref=([^\]]+)\]/)[1];
        assert.ok(snapshot.data.refs.includes(ref("Name")));
        await run({type: "fill", snapshot_id: snapshot.data.snapshot_id, ref: ref("Name"), value: "Ada"});
        await run({type: "press", snapshot_id: snapshot.data.snapshot_id, ref: ref("Name"), key: "Enter"});
        const keyboard = (await run({type: "evaluate", expression:
            "({events:window.keyEvents,submits:window.submits,text:document.querySelector('#submitted').textContent})"})).data.value;
        assert.deepEqual(keyboard.events, ["keydown", "keypress", "keyup"].map(type =>
            ({type, key: "Enter", trusted: true})), `Trusted ordered Enter events: ${JSON.stringify(keyboard)}`);
        assert.deepEqual(keyboard.submits, [{value: "Ada", trusted: true}], "Exactly one trusted form submit");
        assert.equal(keyboard.text, "Submitted Ada", "Enter must commit the form consequence");
        console.log(`KEYBOARD TRACE ${JSON.stringify({keyboard, focusCalls:
            (await evaluate(() => fixture.state())).calls.filter(call => /Focus|dispatchKey/.test(call.method))})}`);
        assert.equal((await evaluate(() => fixture.tabs()))[0].active, false);
        assert.deepEqual(await evaluate(() => fixture.personal()), personalBefore);
        ownedChrome.assertObserverDefaults();
        await run({type: "click", snapshot_id: snapshot.data.snapshot_id, ref: ref("Apply")});
        assert.match((await run({type: "text"})).text, /Hello Ada/);
        if (process.env.ABUSH_ENGINE_TEST_CHILD_GUTTER) {
            const gutter = process.env.ABUSH_ENGINE_TEST_CHILD_GUTTER;
            assert.ok(["auto", "stable both-edges"].includes(gutter));
            await run({type: "evaluate", expression: `new Promise(resolve => {
                const frame=document.createElement('iframe'); frame.id='capture-child';
                frame.style.cssText='width:400px;height:200px;border:0';
                frame.src='/child'; frame.onload=()=>resolve(true); document.body.appendChild(frame);
            })`});
            await run({type: "evaluate", expression: `(() => {
                const root=document.querySelector('#capture-child').contentDocument.documentElement;
                root.style.cssText='scrollbar-gutter:${gutter};overflow-y:auto';
                root.querySelector('body').style.cssText='margin:0;height:100px';
                root.querySelectorAll('#top,#bottom').forEach(element=>element.remove());
            })()`});
            const child = async () => (await run({type: "evaluate", expression: `(() => {
                const root=document.querySelector('#capture-child').contentDocument.documentElement;
                return {width:root.clientWidth,gutter:getComputedStyle(root).scrollbarGutter};
            })()`})).data.value;
            const beforeChild = await child();
            await run({type: "screenshot", full_page: false});
            assert.deepEqual(await child(), beforeChild);
            const beforeNative = (await evaluate(() => fixture.captureState())).outputs.length;
            const rejected = await receive("execute", {type: "screenshot", full_page: true});
            assert.equal(rejected.ok, false);
            assert.match(rejected.error, /cannot preserve automatic scrollbar layout with child frames/);
            assert.equal((await evaluate(() => fixture.captureState())).outputs.length, beforeNative);
            assert.equal(engine.leases.get("extension").closed, true);
            assert.equal((await evaluate(() => fixture.tabs()))[0].active, false);
            assert.deepEqual(await evaluate(() => fixture.personal()), personalBefore);
            console.log(`PASS viewport preserves child ${gutter}; unsafe full-page style rejected before native capture`);
            return;
        }
        const geometry = async () => (await run({type: "evaluate", expression: `({
            width:innerWidth,height:innerHeight,dpr:devicePixelRatio,x:scrollX,y:scrollY,
            scrollWidth:document.documentElement.scrollWidth,
            scrollHeight:document.documentElement.scrollHeight,
            htmlStyle:document.documentElement.getAttribute('style'),
            bodyStyle:document.body.getAttribute('style'),
            top:document.querySelector('#top').getBoundingClientRect().toJSON(),
            bottom:document.querySelector('#bottom').getBoundingClientRect().toJSON(),
            styles:[document.documentElement,document.body].map(element => {
                const style=getComputedStyle(element);
                return [style.width,style.height,style.overflow,style.position,style.scrollbarWidth];
            })})`})).data.value;
        const dimensions = [], imageOutputs = [];
        let captures = 0;
        async function capture(full_page, topColor, bottomColor) {
            const before = await geometry();
            assert.ok(before.scrollHeight > before.height * 2, "fixture must exceed two viewports");
            const prior = await evaluate(() => fixture.captureState());
            const shot = await run({type: "screenshot", full_page});
            assert.equal(shot.images.length, 1);
            assert.equal(shot.images[0].mime_type, "image/png");
            const base64 = shot.images[0].data_base64;
            const image = decodePng(Buffer.from(base64, "base64"));
            const scale = before.dpr;
            assert.equal(image.width, Math.round((full_page ? before.scrollWidth : before.width) * scale));
            assert.equal(image.height, Math.round((full_page ? before.scrollHeight : before.height) * scale));
            assert.deepEqual(image.pixel(Math.floor(40 * scale), Math.floor(40 * scale)), topColor);
            if (full_page) assert.deepEqual(image.pixel(Math.floor(40 * scale),
                Math.floor((before.bottom.y + 40) * scale)), bottomColor);
            const after = await evaluate(() => fixture.captureState());
            assert.equal(after.outputs.length, ++captures);
            assert.equal(base64, after.outputs.at(-1), "Engine must return original native PNG bytes");
            imageOutputs.push(shot);
            dimensions.push({full_page, width: image.width, height: image.height, dpr: scale});
            const trace = after.trace.slice(prior.trace.length);
            const dispatched = trace.filter(event => event.phase === "dispatched");
            for (const method of ["Page.startScreencast", "Page.captureScreenshot", "Page.stopScreencast"]) {
                assert.equal(dispatched.filter(event => event.method === method).length, 1, method);
                assert.equal(trace.filter(event => event.method === method &&
                    event.phase === "completed").length, 1, `${method} completion`);
            }
            const index = (method, phase) => trace.findIndex(event =>
                event.method === method && event.phase === phase);
            assert.ok(index("Page.screencastFrameAck", "completed") >= 0);
            assert.ok(index("Page.screencastFrameAck", "completed") <
                index("Page.captureScreenshot", "dispatched"));
            assert.ok(index("Page.startScreencast", "completed") <
                index("Page.captureScreenshot", "dispatched"));
            assert.ok(index("Page.captureScreenshot", "completed") <
                index("Page.stopScreencast", "dispatched"));
            assert.ok(index("Page.stopScreencast", "completed") <
                index("Page.captureScreenshot", "published"), "cleanup must precede output publication");
            assert.ok(after.privateFrames.length > prior.privateFrames.length);
            assert.equal(emitted.some(message => /screencast/i.test(message.method || "")), false);
            for (const frame of after.privateFrames) {
                assert.equal(JSON.stringify(emitted).includes(frame), false, "private JPEG leaked to CDP");
                assert.equal(JSON.stringify(imageOutputs).includes(frame), false, "private JPEG leaked to Engine output");
            }
            assert.deepEqual(await geometry(), before, "capture changed viewport, scroll, styles or geometry");
            assert.equal(engine.leases.get("extension"), lease, "capture must preserve the live lease");
            assert.equal((await evaluate(() => fixture.tabs()))[0].active, false);
            assert.deepEqual(await evaluate(() => fixture.personal()), personalBefore);
            assert.deepEqual(await evaluate(() => fixture.presentation()), presentationBefore);
        }
        for (const [round, colors] of [
            [[20, 40, 220], [20, 200, 40]],
            [[220, 40, 20], [180, 20, 200]],
        ].entries()) {
            await run({type: "evaluate", expression: `(() => {
                document.querySelector('#top').style.background='rgb(${colors[0]})';
                document.querySelector('#bottom').style.background='rgb(${colors[1]})';
                document.querySelector('#bottom').textContent='Fresh bottom ${round}';
            })()`});
            assert.match((await run({type: "text"})).text, new RegExp(`Fresh bottom ${round}`));
            await capture(false, ...colors);
            await capture(true, ...colors);
        }
        await run({type: "navigate", url: `${origin}/second`});
        const secondSnapshot = await run({type: "snapshot"});
        assert.notEqual(secondSnapshot.data.snapshot_id, snapshot.data.snapshot_id);
        assert.match((await run({type: "text"})).text, /Initial bottom/);
        await capture(false, [20, 40, 220], [20, 200, 40]);
        await capture(true, [20, 40, 220], [20, 200, 40]);
        assert.deepEqual(protocolErrors, []);
        console.log(`PASS repeated fresh viewport/full-page decoded PNGs across two documents on one lease ${JSON.stringify(dimensions)}`);
        const state = await evaluate(() => fixture.state());
        assert.equal(state.calls.every(c => c.source.tabId === setup.tabId), true);
        assert.equal(state.calls.some(c => /bringToFront|activateTarget|closeTarget|setDeviceMetrics|setWindow/.test(c.method)), false);
        assert.deepEqual(state.errors, []);
        for (const message of [
            {id: 100001, method: "Target.getTargetInfo", params: {targetId: setup.otherTargetId}},
            {id: 100002, method: "Runtime.evaluate", sessionId: "foreign", params: {expression: "1"}},
            {id: 100003, method: "Browser.close"},
            {id: 100006, sessionId: state.rootSessionId, method: "Page.startScreencast"},
            {id: 100007, sessionId: state.rootSessionId, method: "Page.stopScreencast"},
            {id: 100008, sessionId: state.rootSessionId, method: "Page.screencastFrameAck", params: {sessionId: 1}},
        ]) assert.ok((await cdp(message)).error);
        assert.equal((await evaluate(() => fixture.state())).calls.length, state.calls.length);
        engineReceiving = false;
        await receive("close");
        assert.equal(browser.isConnected(), true);
        const lateId = 100004;
        const late = evaluate(message => fixture.late(message), {id: lateId,
            sessionId: state.rootSessionId, method: "Runtime.evaluate", params: {
                expression: "new Promise(resolve => { globalThis.fixtureLateResolve = resolve; console.log('fixture-late-entered'); })",
                awaitPromise: true, returnByValue: true,
            }});
        late.catch(error => failures.push(error));
        await bounded("registered real Runtime entered barrier", () => entered.promise);
        await evaluate(() => fixture.revokeAndResolve());
        await bounded("late actual debugger completion", () => completed.promise);
        await bounded("late worker return", () => late);
        assert.equal((await evaluate(() => fixture.state())).lateRawResult.result.value, "fixture-secret");
        assert.equal(emitted.some(message => message.id === lateId), false);
        assert.equal(JSON.stringify(emitted).includes("fixture-secret"), false);
        await evaluate(() => fixture.release());
        assert.equal((await evaluate(() => fixture.state())).closed, true);
        const releasedDocument = await evaluate(() => fixture.releasedPageState());
        assert.equal(releasedDocument.visibility, "hidden",
            "detaching the owned debugger must remove visibility emulation without activating the tab");
        const after = await evaluate(() => fixture.tabs());
        assert.equal(after[0].id, setup.tabId);
        assert.equal(after[0].url, `${origin}/second`);
        assert.deepEqual(after[1], otherBefore);
        assert.equal(after[0].active, false);
        assert.deepEqual(await evaluate(() => fixture.personal()), personalBefore);
        assert.deepEqual(await evaluate(() => fixture.presentation()), presentationBefore);
        ownedChrome.assertObserverDefaults();
        assert.equal(browser.isConnected(), true);
        assert.equal(context.pages().length, 0, "fixture observer must not attach Engine's page");
        assert.deepEqual(failures, []);
        console.log("PASS foreign target/session and Browser.close denied; revoked late response suppressed; release preserves both tabs/browser");
        console.log("PASS inactive work-tab trusted ordered Enter events and exactly-once submit");
    } catch (error) {
        primaryError = error;
        if (worker && fixtureReady) {
            try {
                const state = await evaluate(() => fixture.state());
                console.error("REAL EXTENSION FAILURE TRACE", JSON.stringify({error: error.message,
                    calls: state.calls.slice(-12).map(call => ({...call,
                        params: call.method === "Runtime.callFunctionOn" ? {function: call.params.functionDeclaration?.slice(0, 80)} :
                            call.method === "Runtime.evaluate" ? {expression: call.params.expression?.slice(0, 80)} : call.params})),
                    protocolErrors: protocolErrors.slice(-4), failures: failures.map(failure => failure.message)}));
            } catch (diagnosticError) { console.error("TRACE FAILED", diagnosticError); }
        }
        throw error;
    } finally {
        const teardownErrors = [];
        const attempt = async (label, run, ms = 5000) => {
            try { await bounded(label, run, ms); } catch (error) { teardownErrors.push(error); }
        };
        engine?.dispose();
        if (worker && fixtureReady) {
            await attempt("worker release", () => evaluate(() => fixture.release()));
            await attempt("worker pump stop", () => evaluate(() => fixture.stop()));
        }
        if (pump) await attempt("pump join", () => pump);
        stopping = true;
        await attempt("transport tasks join", () => Promise.allSettled([...transportTasks]));
        if (ownedChrome) await attempt("owned Chrome shutdown", () => ownedChrome.shutdown(), 15000);
        if (website) {
            website.closeAllConnections();
            await attempt("HTTP fixture close", () => new Promise(resolve => website.close(resolve)));
        }
        if (oldTmp === undefined) delete process.env.TMPDIR; else process.env.TMPDIR = oldTmp;
        await attempt("fixture artifacts removal", () => fs.rm(artifacts, {recursive: true, force: true}));
        if (teardownErrors.length) throw new AggregateError(
            [...(primaryError ? [primaryError] : []), ...teardownErrors], "Extension fixture teardown");
    }
})().catch(error => { console.error(error); process.exitCode = 1; });
