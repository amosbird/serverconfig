#!/usr/bin/env node
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const {assert, clone, deferred, fixture, execute, runCases} = require("./browser_tab_groups_fixture.js");
const rofiPath = process.env.ABUSH_ROFI_EXTENSION_PATH;
const settle = () => new Promise(resolve => setImmediate(resolve));
function dom() {
    const nodes = new Map();
    const node = () => ({textContent: "", value: "", children: [], dataset: {}, style: {},
        classList: {add() {}, toggle() {}}, listeners: {},
        addEventListener(event, listener) { this.listeners[event] = listener; },
        replaceChildren(...children) { this.children = children; }, append(...children) { this.children.push(...children); }});
    const get = id => { if (!nodes.has(id)) nodes.set(id, node()); return nodes.get(id); };
    return {nodes, getElementById: get, querySelector: get, querySelectorAll: () => [],
        createElement: node, addEventListener() {}};
}
function loadRofi(f, file) {
    const noopEvent = {addListener() {}};
    const creates = [];
    const chrome = {...f.api, tabs: {...f.api.tabs, onActivated: noopEvent,
        create(args) { const task = f.api.tabs.create({windowId: 1, active: true, ...args}); creates.push(task); return task; }},
        runtime: {...f.api.runtime, onMessage: noopEvent, sendMessage(message, cb) { cb?.({connected: true}); },
            connectNative: () => ({onMessage: noopEvent, onDisconnect: noopEvent, postMessage() {}})},
        commands: {onCommand: noopEvent}, downloads: {onChanged: noopEvent},
        permissions: {contains: async () => false, request: async () => true},
        bookmarks: {onCreated: noopEvent, onChanged: noopEvent, onMoved: noopEvent, onRemoved: noopEvent}};
    const context = vm.createContext({chrome, document: dom(), window: {close() {}}, console, URL, confirm: () => true});
    vm.runInContext(fs.readFileSync(path.join(rofiPath, file), "utf8"), context, {filename: file});
    return {context, creates};
}
const cases = [
    ["new plain tabs same URL and late group updates never own work", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        const work = f.executor.recordFor("remote"); const authority = clone(f.executor.state("remote"));
        await f.api.tabs.update(work.tab_id, {active: true});
        for (let i = 0; i < 3; i++) {
            const personal = await f.api.tabs.create({windowId: 1, url: "about:blank", active: true});
            await settle();
            assert.equal(f.tabs.get(personal.id).groupId, -1);
            assert.equal(f.executor.ownsTab(personal.id), false);
            f.tabs.get(personal.id).groupId = work.group_id;
            await Promise.all(f.api.tabs.onUpdated.emit(personal.id, {groupId: work.group_id}, clone(f.tabs.get(personal.id))));
            assert.equal(f.tabs.get(personal.id).groupId, -1);
            await f.api.tabs.update(work.tab_id, {active: true});
        }
        f.tabs.get(1).groupId = work.group_id;
        await Promise.all(f.api.tabs.onUpdated.emit(1, {groupId: work.group_id}, clone(f.tabs.get(1))));
        assert.equal(f.tabs.get(1).groupId, work.group_id);
        assert.equal(f.executor.ownsTab(1), false);
        assert.deepEqual(f.executor.state("remote"), authority);
        await f.executor.execute(execute(f));
        assert.ok(f.calls.filter(c => c.name === "debugger.sendCommand").every(c => c.args[0].tabId === work.tab_id));
        assert.equal(f.tabs.get(work.tab_id).active, true);
    }],
    ["onCreated before work callback never strips exact explicit group", async () => {
        const f = fixture({globalAutomation: true});
        const entered = deferred(); const gate = deferred();
        f.hooks["tabs.create.after"] = async () => { entered.resolve(); await gate.promise; };
        const ready = f.ready(); await entered.promise;
        const pending = [...f.tabs.values()].find(tab => tab.id >= 10);
        await Promise.all(f.api.tabs.onUpdated.emit(pending.id, {groupId: 100}, clone(pending)));
        gate.resolve(); await ready; await settle();
        const record = f.executor.recordFor("remote");
        assert.equal(f.tabs.get(record.tab_id).groupId, record.group_id);
        assert.equal(record.agreed, true);
        assert.equal(f.calls.filter(c => c.name === "tabs.ungroup").length, 0);
    }],
    ["popup unknown row confirms one recovery and leaves OFF", async () => {
        const f = fixture({globalAutomation: true});
        f.hooks["tabs.create"] = async () => { throw new Error("lost result"); };
        await f.ready(); delete f.hooks["tabs.create"]; await f.send({control: "stop"});
        const document = dom(); let confirmed = false;
        vm.runInNewContext(fs.readFileSync(path.join(__dirname, "../chromium/external-links/extension/popup.js"), "utf8"),
            {document, chrome: {runtime: {sendMessage: message => f.send(message)}}, confirm: () => confirmed});
        await settle();
        const button = document.getElementById("workbenches").children[0].children[0];
        assert.match(button.textContent, /Abandon/);
        button.listeners.click(); await settle();
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
        confirmed = true; button.listeners.click(); await settle();
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, false);
        assert.equal(f.executor.automationOn, false);
        assert.equal(document.getElementById("toggle").textContent, "Start automation");
        assert.equal(f.calls.filter(c => c.name === "tabs.create").length, 1);
    }],
    ["popup renders committed ON with readiness error", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        f.hooks["session.set"] = async () => { throw new Error("consent failed"); };
        await f.send({control: "start"});
        const document = dom();
        vm.runInNewContext(fs.readFileSync(path.join(__dirname, "../chromium/external-links/extension/popup.js"), "utf8"),
            {document, chrome: {runtime: {sendMessage: message => f.send(message)}}, confirm: () => true});
        await settle();
        assert.equal(document.getElementById("toggle").textContent, "Stop automation");
        assert.match(document.getElementById("error").textContent, /consent failed/);
        assert.match(document.getElementById("workbenches").children[0].textContent, /preparing/);
    }],
];
if (rofiPath) {
    for (const [file, entrance] of [
        ["bg.js", 'onNativeMessage({info: "switchTab", result: "g personal search"})'],
        ["bg.js", 'onNativeMessage({info: "openHistory", result: "History ::: https://history.test/"})'],
        ["bg.js", 'onNativeMessage({info: "switchTab", result: "https://plain.test/"})'],
        ["bookmarks.js", 'activateBookmark({ctrlKey: true}, {id: "one", url: "https://bookmark.test/"})'],
        ["popup.js", 'openExtensionPage("bookmarks.html")'],
    ]) cases.push([`actual Rofi ${file}: ${entrance}`, async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        const work = f.executor.recordFor("remote"); const authority = clone(f.executor.state("remote"));
        await f.api.tabs.update(work.tab_id, {active: true});
        const {context, creates} = loadRofi(f, file);
        await vm.runInContext(entrance, context);
        await Promise.all(creates); await settle();
        assert.equal(creates.length, 1);
        const personal = await creates[0];
        assert.equal(f.tabs.get(personal.id).groupId, -1);
        assert.equal(f.tabs.get(personal.id).active, true);
        assert.equal(f.executor.ownsTab(personal.id), false);
        assert.deepEqual(f.executor.state("remote"), authority);
        assert.equal(f.tabs.get(work.tab_id).groupId, work.group_id);
        await f.executor.execute(execute(f));
        assert.ok(f.calls.filter(c => c.name === "debugger.sendCommand").every(c => c.args[0].tabId === work.tab_id));
    }]);
} else console.log("Rofi source entrances skipped: set ABUSH_ROFI_EXTENSION_PATH to require them");
runCases(cases).catch(error => { console.error(error); process.exitCode = 1; });
