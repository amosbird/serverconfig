#!/usr/bin/env node
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const source = fs.readFileSync(path.join(__dirname,
    "../chromium/external-links/extension/popup.js"), "utf8");
function element() {
    return {textContent: "", disabled: false, children: [], listeners: new Map(),
        replaceChildren(...children) { this.children = children; },
        append(child) { this.children.push(child); },
        addEventListener(name, handler) { this.listeners.set(name, handler); }};
}
function popup() {
    const nodes = Object.fromEntries(["toggle", "state", "error", "workbenches"]
        .map(name => [name, element()]));
    const requests = [];
    const context = vm.createContext({document: {
        getElementById: id => nodes[id], createElement: () => element(),
    }, chrome: {runtime: {sendMessage(message) {
        return new Promise(resolve => requests.push({message, resolve}));
    }}}, confirm: () => false});
    let render = vm.runInContext(source, context);
    const frames = [];
    async function respond(response) {
        const request = requests.shift();
        assert.ok(request, "popup must register the request before its response");
        request.resolve(structuredClone(response));
        await render;
        const frame = {action: request.message.control, headline: nodes.state.textContent,
            error: nodes.error.textContent, toggle: nodes.toggle.textContent,
            disabled: nodes.toggle.disabled,
            items: nodes.workbenches.children.map(item => item.textContent)};
        frames.push(frame);
        return frame;
    }
    function control(action) { render = vm.runInContext(`control(${JSON.stringify(action)})`, context); }
    return {nodes, frames, respond, control};
}
function status(phase, on = true, fields = {}) {
    return {ok: true, automation_on: on, preference_pending: false, stop_pending: false,
        browser_executor: {phase, error: phase === "secondary" ?
            "Browser automation belongs to the configured primary browser profile" :
            phase === "unavailable" ? "Native browser executor is disconnected" : null},
        error: null, remotes: [], ...fields};
}
(async () => {
    const f = popup();
    for (const phase of ["pending", "secondary", "unavailable", "ready", "unavailable", "pending", "ready"]) {
        if (f.frames.length) f.control("status");
        const state = status(phase);
        state.error = state.browser_executor.error;
        const frame = await f.respond(state);
        assert.equal(frame.toggle, "Stop automation", "readiness must not change the saved preference");
        assert.equal(frame.disabled, false);
        if (phase === "ready") assert.match(frame.headline, /work tabs open on first navigation/);
        else {
            assert.doesNotMatch(frame.headline, /work tabs open on first navigation/,
                `unadmitted ${phase} frame must not promise browser execution`);
            assert.match(frame.headline, phase === "secondary" ? /primary browser/ :
                phase === "pending" ? /preparing/ : /unavailable/);
        }
        assert.equal(frame.error, state.error || "");
    }
    for (const phase of ["pending", "secondary", "unavailable", "ready"]) {
        f.control("status");
        const frame = await f.respond(status(phase, false));
        assert.match(frame.headline, /Automation off/);
        assert.equal(frame.toggle, "Start automation");
        assert.doesNotMatch(frame.headline, /work tabs open on first navigation/);
    }
    f.control("status");
    const unsaved = await f.respond(status("ready", false, {stop_pending: true,
        preference_pending: true, error: "Stop not saved"}));
    assert.match(unsaved.headline, /Stop not saved/);
    assert.equal(unsaved.toggle, "Retry Stop");
    f.control("status");
    const pendingPreference = await f.respond(status("ready", true, {preference_pending: true}));
    assert.match(pendingPreference.headline, /preference pending/);
    assert.doesNotMatch(pendingPreference.headline, /work tabs open on first navigation/);
    f.control("status");
    const legacy = status("ready"); delete legacy.browser_executor;
    assert.match((await f.respond(legacy)).headline, /unavailable/,
        "missing readiness status must fail closed in presentation");
    f.control("status");
    const ready = await f.respond(status("ready", true, {remotes: [{display: "fixture",
        connected: true, agreed: true, error: null}]}));
    assert.deepEqual(ready.items, ["fixture: Ready"]);
    console.log(`PASS production popup readiness/preference trace: ${f.frames.length} frames`);
})().catch(error => { console.error(error); process.exitCode = 1; });
