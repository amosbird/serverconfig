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
    ["new plain tabs same URL isolate once and later manual regroup stays human", async () => {
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
            assert.equal(f.tabs.get(personal.id).groupId, work.group_id);
            assert.equal(f.executor.personalCandidates.has(personal.id), false);
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
    ["human creation during group label sees pending exact group", async () => {
        const f = fixture({globalAutomation: true});
        const entered = deferred(); const gate = deferred();
        f.hooks["tabGroups.update"] = async () => { entered.resolve(); await gate.promise; };
        const ready = f.ready(); await entered.promise;
        const pending = [...f.tabs.values()].find(tab => tab.id >= 10);
        await f.api.tabs.update(pending.id, {active: true});
        const personal = await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        await settle();
        assert.equal(f.tabs.get(personal.id).groupId, -1);
        gate.resolve(); await ready;
        const authority = clone(f.executor.state("remote"));
        assert.equal(f.executor.ownsTab(personal.id), false);
        assert.equal(f.tabs.get(personal.id).active, true);
        assert.equal(f.tabs.get(personal.id).url, "https://human.test/");
        assert.deepEqual(f.executor.state("remote"), authority);
        assert.equal(f.calls.filter(call => call.name === "tabs.ungroup").length, 1);
    }],
    ...["group", "window"].map(move => [`stale read cannot strip human ${move} move`, async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        await f.api.tabs.update(f.executor.recordFor("remote").tab_id, {active: true});
        const entered = deferred(); const gate = deferred();
        f.hooks["tabs.get.after"] = async tab => {
            if (tab.id <= 10) return;
            entered.resolve(); await gate.promise;
        };
        const personal = await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        await entered.promise;
        delete f.hooks["tabs.get.after"];
        const tab = f.tabs.get(personal.id);
        if (move === "group") {
            tab.groupId = 700;
            await Promise.all(f.api.tabs.onUpdated.emit(tab.id, {groupId: 700}, clone(tab)));
        } else {
            tab.windowId = 2;
            await Promise.all(f.api.tabs.onDetached.emit(tab.id, {oldWindowId: 1}));
            await Promise.all(f.api.tabs.onAttached.emit(tab.id, {newWindowId: 2}));
        }
        gate.resolve(); await settle();
        assert.equal(f.calls.filter(call => call.name === "tabs.ungroup").length, 0);
        assert.equal(tab.groupId, move === "group" ? 700 : 100);
        tab.groupId = 100;
        await Promise.all(f.api.tabs.onUpdated.emit(tab.id, {groupId: 100}, clone(tab)));
        assert.equal(tab.groupId, 100);
    }]),
    ["fresh worker restores actual onCreated obligation but not manual group members", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        const personal = await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        await settle(); await f.executor.inheritanceTail;
        const old = f.executor;
        for (const candidate of old.personalCandidates.values()) clearTimeout(candidate.timer);
        old.personalCandidates.clear();
        f.tabs.get(1).groupId = 100;
        await f.restart();
        const tab = f.tabs.get(personal.id); tab.groupId = 100;
        await Promise.all(f.api.tabs.onUpdated.emit(tab.id, {groupId: 100}, clone(tab)));
        assert.equal(tab.groupId, -1);
        assert.equal(f.tabs.get(1).groupId, 100);
        assert.equal(f.executor.ownsTab(tab.id), false);
    }],
    ["raw late work creation does not block human inheritance or authorize after Stop", async () => {
        const f = fixture({globalAutomation: true});
        const entered = deferred(); const gate = deferred();
        const originalApi = f.executor.api.bind(f.executor);
        f.executor.api = (task, label) => label === "create inactive work tab" ?
            require("../chromium/external-links/extension/workspace.js").bounded(task, label, 20) : originalApi(task, label);
        f.hooks["tabs.create.after"] = async () => { entered.resolve(); await gate.promise; };
        const ready = f.ready(); await entered.promise; await ready;
        assert.equal(f.executor.workCreatesInFlight, 1);
        delete f.hooks["tabs.create.after"];
        const partial = [...f.tabs.values()].find(tab => tab.id >= 10);
        f.tabs.get(1).groupId = 100;
        // Another exact workbench group remains guarded while a raw request is unresolved.
        f.executor.workCreations.set("other", {tab_id: 900, group_id: 100});
        const personal = await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        await settle(); assert.equal(f.tabs.get(personal.id).groupId, -1);
        await f.send({control: "stop"});
        gate.resolve(); await settle(); await f.executor.inheritanceTail;
        assert.equal(f.executor.workCreatesInFlight, 0);
        assert.equal(f.executor.ownsTab(partial.id), false);
        assert.equal(f.calls.filter(call => call.name === "tabs.group").length, 0);
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
    }],
    ["inheritance storage failure is advisory and leaves human usage available", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        f.hooks["session.set"] = async value => {
            if (value.abushBrowserInheritanceV9) throw new Error("inheritance storage unavailable");
        };
        await f.api.tabs.update(f.executor.recordFor("remote").tab_id, {active: true});
        const personal = await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        await settle();
        assert.equal(f.tabs.get(personal.id).groupId, -1);
        assert.match(f.executor.lastError, /inheritance storage unavailable/);
    }],
    ["pending backlog above cap retains obligations instead of reclassifying", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        for (let index = 0; index < 130; index++) {
            await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        }
        await settle(); await f.executor.inheritanceTail;
        assert.equal(f.executor.personalCandidates.size, 130);
        assert.match(f.executor.lastError, /backlog/);
        assert.equal(f.session.abushBrowserInheritanceV9.candidates.length, 130);
    }],
    ["advisory expiry retains unresolved delayed inheritance", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        const personal = await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        await settle();
        const candidate = f.executor.personalCandidates.get(personal.id);
        candidate.timer._onTimeout();
        assert.equal(f.executor.personalCandidates.get(personal.id), candidate);
        assert.match(f.executor.lastError, /unresolved/);
        f.tabs.get(personal.id).groupId = 100;
        await Promise.all(f.api.tabs.onUpdated.emit(personal.id, {groupId: 100}, clone(f.tabs.get(personal.id))));
        assert.equal(f.tabs.get(personal.id).groupId, -1);
    }],
    ["unknown initial onCreated URL protects exact pending own tab until URL arrives", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        await f.api.tabs.update(f.executor.recordFor("remote").tab_id, {active: true});
        const entered = deferred(); const gate = deferred();
        const second = {...require("./browser_tab_groups_fixture.js").REMOTE, remote_key: "second",
            principal_id: "second-principal", workspace_id: "second-workspace"};
        f.hooks["tabs.create"] = async args => {
            const tab = {id: 99, windowId: 1, active: false, groupId: 100};
            f.tabs.set(99, tab);
            await Promise.all(f.api.tabs.onCreated.emit(clone(tab)));
            assert.equal(tab.groupId, 100);
            tab.pendingUrl = args.url;
            await Promise.all(f.api.tabs.onUpdated.emit(99, {pendingUrl: args.url}, clone(tab)));
            assert.equal(tab.groupId, 100);
            entered.resolve(); await gate.promise;
            return clone(tab);
        };
        const enabling = f.emitRemote(second); await entered.promise;
        assert.equal(f.calls.filter(call => call.name === "tabs.ungroup").length, 0);
        gate.resolve(); await enabling;
        assert.equal(f.executor.recordFor("second").tab_id, 99);
        assert.equal(f.executor.recordFor("second").agreed, true);
    }],
    ["pending creation identity survives worker termination without control adoption", async () => {
        const f = fixture({globalAutomation: true});
        const originalApi = f.executor.api.bind(f.executor);
        f.executor.api = (task, label) => label === "create inactive work tab" ?
            require("../chromium/external-links/extension/workspace.js").bounded(task, label, 20) : originalApi(task, label);
        // The raw callback is lost with the old worker, not released after restart.
        f.hooks["tabs.create"] = () => new Promise(() => {});
        await f.ready();
        const old = f.executor;
        for (const candidate of old.personalCandidates.values()) clearTimeout(candidate.timer);
        const url = [...old.workCreations.keys()][0];
        delete f.hooks["tabs.create"];
        await f.restart();
        await f.emitRemote(require("./browser_tab_groups_fixture.js").REMOTE);
        const late = {id: 99, windowId: 1, groupId: 100, active: false};
        f.tabs.set(99, late);
        await Promise.all(f.api.tabs.onCreated.emit(clone(late)));
        late.pendingUrl = url;
        await Promise.all(f.api.tabs.onUpdated.emit(99, {pendingUrl: url}, clone(late)));
        assert.equal(late.groupId, 100);
        assert.equal(f.executor.ownsTab(99), false);
        assert.equal(f.calls.filter(call => call.name === "tabs.group").length, 0);
        assert.equal(f.executor.popupState().remotes[0].creation_unknown, true);
    }],
    ["different browser boot does not restore personal obligations", async () => {
        const f = fixture({globalAutomation: true}); await f.ready();
        const personal = await f.api.tabs.create({windowId: 1, url: "https://human.test/", active: true});
        await settle(); await f.executor.inheritanceTail;
        const old = f.executor;
        for (const candidate of old.personalCandidates.values()) clearTimeout(candidate.timer);
        old.personalCandidates.clear();
        await f.restart({...require("./browser_tab_groups_fixture.js").BOOT, browser_boot_id: "different"});
        assert.equal(f.executor.personalCandidates.has(personal.id), false);
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
        ["bg.js", 'onNativeMessage({info: "switchTab", result: "g personal.example"})'],
        ["bg.js", 'onNativeMessage({info: "switchTab", result: "g https://direct.test/"})'],
        ["bg.js", 'onNativeMessage({info: "openHistory", result: "History ::: personal.example"})'],
        ["popup.js", 'openExtensionPage("history.html")'],
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
