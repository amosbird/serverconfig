#!/usr/bin/env node
// Offline release gate. Requires Node and the installed libqtile Python package, not a display.
// timeout -s TERM -k 5s 45s node scripts/test_browser_lifecycle_release.js
// LIFECYCLE_CASE selects diagnostic cases; the release gate runs every case by default.
const assert = require("node:assert/strict");
const {spawn} = require("node:child_process");
const readline = require("node:readline");
const path = require("node:path");
global.crypto ||= require("node:crypto").webcrypto;
const {WorkspaceExecutor} = require("../chromium/external-links/extension/workspace.js");
const {registerWorkspaceWindowListeners} =
    require("../chromium/external-links/extension/background.js");

const clone = value => structuredClone(value);
const OFFLINE_BOOTSTRAP = Object.freeze({browser_boot_id: "offline-boot", browser_pid: 4242,
    browser_starttime: "1234", ordinary_launch_leases: []});

async function bootstrapExecutor(executor) {
    await executor.acceptNativeBootstrap(clone(OFFLINE_BOOTSTRAP));
}

async function boundedCase(name, run, deadlineMs = 8000) {
    let timer;
    try {
        return await Promise.race([Promise.resolve().then(run), new Promise((_, reject) => {
            timer = setTimeout(() => {
                const error = new Error(`lifecycle case ${name} exceeded ${deadlineMs}ms`);
                error.lifecycleDeadline = true;
                reject(error);
            }, deadlineMs);
        })]);
    } finally {
        clearTimeout(timer);
    }
}

function event() {
    const listeners = [];
    return {addListener: listener => listeners.push(listener),
        emit: (...args) => listeners.forEach(listener => listener(...clone(args)))};
}

class Bridge {
    constructor() {
        this.pending = new Map();
        this.sequence = 0;
        this.child = spawn("python3", ["-B", path.join(__dirname,
            "browser_lifecycle_native_fixture.py")], {
            env: {...process.env, DISPLAY: "", WAYLAND_DISPLAY: "", PYTHONDONTWRITEBYTECODE: "1"},
            stdio: ["pipe", "pipe", "inherit"],
        });
        this.exited = new Promise(resolve => this.child.once("exit", resolve));
        this.child.once("error", error => this.fail(error));
        this.child.once("exit", code => this.fail(new Error(`native fixture exited: ${code}`)));
        this.lines = readline.createInterface({input: this.child.stdout});
        this.lines.on("line", line => {
            const reply = JSON.parse(line);
            const pending = this.pending.get(reply.id);
            if (!pending) return;
            this.pending.delete(reply.id);
            clearTimeout(pending.timer);
            if (reply.ok) pending.resolve(reply.payload);
            else pending.reject(new Error(reply.error));
        });
        this.deadline = setTimeout(() => {
            this.fail(new Error("native child exceeded exact 25s lifetime"));
            this.child.kill("SIGTERM");
        }, 25000);
    }

    fail(error) {
        for (const pending of this.pending.values()) {
            clearTimeout(pending.timer);
            pending.reject(error);
        }
        this.pending.clear();
    }

    call(kind, fields = {}) {
        const id = ++this.sequence;
        return new Promise((resolve, reject) => {
            const timer = setTimeout(() => {
                this.pending.delete(id);
                reject(new Error(`bridge ${kind} ${fields.request?.operation || ""} exceeded 2s`));
            }, 2000);
            this.pending.set(id, {resolve, reject, timer});
            this.child.stdin.write(JSON.stringify({id, kind, ...fields}) + "\n");
        });
    }

    async close() {
        clearTimeout(this.deadline);
        this.child.stdin.end();
        let timer;
        try {
            await Promise.race([this.exited, new Promise((_, reject) => {
                timer = setTimeout(() => reject(new Error("native teardown exceeded 2s")), 2000);
            })]);
        } catch (error) {
            this.child.kill("SIGKILL");
            throw error;
        } finally {
            clearTimeout(timer);
            this.lines.close();
            this.fail(new Error("bridge closed"));
        }
    }
}

class ChromeModel {
    constructor(bridge, fault = null, pendingCreated = false) {
        this.bridge = bridge;
        this.fault = fault;
        this.pendingCreated = pendingCreated;
        this.createdAttempts = 0;
        this.nextTab = 1000;
        this.nextWindow = 10;
        this.windows = new Map([[1, {id: 1, type: "normal", focused: false, tabs: [
            {id: 1, windowId: 1, index: 0, url: "https://ordinary-one.test/",
                title: "Ordinary one", active: true, pinned: false, groupId: 73},
            {id: 2, windowId: 1, index: 1, url: "https://ordinary-two.test/",
                title: "Ordinary two", active: false, pinned: false, groupId: -1},
        ]}]]);
        this.original = clone(this.windows.get(1).tabs);
        this.errors = [];
        this.events = [];
        const storage = () => {
            const state = {};
            return {get: async () => clone(state), set: async value => {
                Object.assign(state, clone(value));
            }};
        };
        this.api = {
            runtime: {getURL: file => `chrome-extension://id/${file}`},
            storage: {session: storage(), local: storage()},
            debugger: {detach: async () => {}},
            windows: {
                onCreated: event(), onRemoved: event(), onFocusChanged: event(),
                get: async id => clone(this.window(id)),
                getAll: async () => clone([...this.windows.values()]),
                create: properties => this.createWindow(properties),
                remove: id => this.removeWindow(id),
            },
            tabs: {
                onCreated: event(), onUpdated: event(), onRemoved: event(), onMoved: event(),
                onAttached: event(), onDetached: event(), onReplaced: event(),
                get: async id => clone(this.tab(id)),
                query: async query => clone([...this.windows.values()].flatMap(win => win.tabs)
                    .filter(tab => query.windowId === undefined || query.windowId === tab.windowId)),
                create: properties => this.createTab(properties),
                update: (id, properties) => this.updateTab(id, properties),
                move: (id, properties) => this.moveTab(id, properties),
                remove: id => this.removeTab(id),
                sendMessage: async () => {},
            },
        };
    }

    window(id) {
        const window = this.windows.get(id);
        if (!window) throw new Error(`missing window ${id}`);
        return window;
    }

    tab(id) {
        const tab = [...this.windows.values()].flatMap(win => win.tabs).find(tab => tab.id === id);
        if (!tab) throw new Error(`missing tab ${id}`);
        return tab;
    }

    emit(target, name, ...args) {
        this.events.push(clone({target, name, args}));
        this.api[target][name].emit(...args);
    }

    async sync(action) {
        await this.bridge.call("sync", {action, windows: clone([...this.windows.values()])});
        for (const original of this.original) {
            const tab = this.tab(original.id);
            assert.equal(tab.url, original.url, `ordinary URL changed during ${action}`);
            assert.equal(tab.groupId, original.groupId, `ordinary tab group changed during ${action}`);
        }
    }

    indices(windowId) {
        this.window(windowId).tabs.forEach((tab, index) => { tab.index = index; });
    }

    activate(tab) {
        for (const item of this.window(tab.windowId).tabs) item.active = item.id === tab.id;
    }

    title(url) {
        if (url.includes("status.html#")) {
            const params = new URLSearchParams(url.split("#")[1]);
            return `__ABUSH_BIND__:${params.get("nonce")}:${params.get("marker")}`;
        }
        if (url.includes("ordinary-staging.html#")) return `__ABUSH_ORDINARY__:${url.split("#")[1]}`;
        return url === "about:blank" ? "New Tab" : "Business title after navigation";
    }

    async createTab(properties) {
        const window = this.window(properties.windowId ?? 1);
        const url = properties.url;
        const tab = {id: this.nextTab++, windowId: window.id, index: window.tabs.length,
            pinned: false, active: properties.active ?? true, groupId: -1,
            url: this.pendingCreated ? "" : url, title: "", status: "loading"};
        if (this.pendingCreated) tab.pendingUrl = url;
        window.tabs.push(tab); // Chrome appends unpinned markers after real ordinary tabs.
        if (tab.active) this.activate(tab);
        await this.sync("tabs.create:loading");
        this.emit("tabs", "onCreated", tab);
        const returned = clone(tab); // Promise results must not mutate behind the executor's back.
        tab.url = url;
        delete tab.pendingUrl;
        tab.title = this.title(url);
        tab.status = "complete";
        await this.sync("tabs.create:title");
        this.emit("tabs", "onUpdated", tab.id,
            {url, title: tab.title, status: "complete"}, tab);
        return returned;
    }

    async updateTab(id, properties) {
        const tab = this.tab(id);
        const oldIndex = tab.index;
        Object.assign(tab, properties);
        if (properties.active) this.activate(tab);
        if ("pinned" in properties) {
            const tabs = this.window(tab.windowId).tabs;
            tabs.splice(tabs.indexOf(tab), 1);
            const pinnedEnd = tabs.filter(item => item.pinned).length;
            tabs.splice(pinnedEnd, 0, tab);
            this.indices(tab.windowId);
        }
        if (properties.url) tab.title = this.title(properties.url);
        await this.sync("tabs.update");
        if (oldIndex !== tab.index) this.emit("tabs", "onMoved", id,
            {windowId: tab.windowId, fromIndex: oldIndex, toIndex: tab.index});
        const change = {...properties};
        delete change.active; // Chrome onUpdated does not report active; onActivated is separate.
        if (properties.url) Object.assign(change, {title: tab.title, status: "complete"});
        if (Object.keys(change).length) this.emit("tabs", "onUpdated", id, change, tab);
        return clone(tab);
    }

    async createWindow(properties) {
        this.createdAttempts++;
        if (this.fault === "unknown-create") return new Promise(() => {});
        if (this.fault === "create") throw new Error("injected create");
        if (this.fault === "partial-reject") {
            const id = this.nextWindow++;
            this.windows.set(id, {id, type: "normal", focused: false, tabs: []});
            await this.sync("windows.create:allocated-before-rejection");
            throw new Error("MoveTabToWindow rejected after allocation");
        }
        assert.equal(properties.focused, false);
        assert.equal(properties.type, "normal");
        const tab = this.tab(properties.tabId);
        assert.match(tab.title, /:a$/);
        const oldWindowId = tab.windowId;
        const oldPosition = tab.index;
        this.window(oldWindowId).tabs.splice(oldPosition, 1);
        this.indices(oldWindowId);
        const id = this.nextWindow++;
        Object.assign(tab, {windowId: id, index: 0, active: true});
        this.windows.set(id, {id, type: "normal", focused: false, tabs: [tab]});
        await this.sync("windows.create:marker-a");
        this.emit("tabs", "onDetached", tab.id, {oldWindowId, oldPosition});
        this.emit("windows", "onCreated", this.window(id));
        this.emit("tabs", "onAttached", tab.id, {newWindowId: id, newPosition: 0});
        return clone(this.window(id));
    }

    async moveTab(id, properties) {
        const tab = this.tab(id);
        const oldWindowId = tab.windowId;
        const oldPosition = tab.index;
        this.window(oldWindowId).tabs.splice(oldPosition, 1);
        this.indices(oldWindowId);
        tab.windowId = properties.windowId;
        this.window(tab.windowId).tabs.push(tab);
        this.indices(tab.windowId);
        await this.sync("tabs.move:marker-b");
        this.emit("tabs", "onDetached", id, {oldWindowId, oldPosition});
        this.emit("tabs", "onAttached", id,
            {newWindowId: tab.windowId, newPosition: tab.index});
        return clone(tab);
    }

    async removeTab(id) {
        assert(![1, 2].includes(id), "ordinary tab deletion");
        const tab = this.tab(id);
        this.window(tab.windowId).tabs.splice(tab.index, 1);
        this.indices(tab.windowId);
        await this.sync("tabs.remove");
        this.emit("tabs", "onRemoved", id, {windowId: tab.windowId, isWindowClosing: false});
    }

    async removeWindow(id) {
        assert.notEqual(id, 1, "ordinary host deletion");
        this.windows.delete(id);
        await this.sync("windows.remove");
        this.emit("windows", "onRemoved", id);
    }
}

function ensurePayload() {
    return {remote_key: "r_12345678", connection_id: "c_12345678",
        principal_id: "principal-one", workspace_id: "workspace-authoritative",
        request_id: "request-one", ensure_id: "ensure-one",
        executor_fence: {executor_id: "executor", epoch: 1},
        owner_display: {daemon_instance_id: "daemon", daemon_boot_id: "boot",
            workbench_name: "workbench"}, connection_display_name: "local", expected_page: null};
}

async function fixture(bridge, fault, pendingCreated) {
    await bridge.call("reset");
    const model = new ChromeModel(bridge, fault, pendingCreated);
    const nativeCalls = [];
    let terminalReplyLost = false;
    const executor = new WorkspaceExecutor(model.api, async (service, operation, payload) => {
        nativeCalls.push(clone({service, operation, payload}));
        const result = await bridge.call("native", {request: {version: 1,
            id: `native-${nativeCalls.length}`, service, operation, payload}});
        if (["cancel", "post-arm-cancel"].includes(fault) && operation ===
                (fault === "cancel" ? "register_candidate_lease" : "arm_candidate_lease")) {
            model.cancellation = executor.cancelEnsure(executor.ensureIdentity(ensurePayload()));
        }
        if (fault === "terminal-loss" && !terminalReplyLost &&
                operation === "ensure_managed_page_completion") {
            terminalReplyLost = true;
            const error = new Error("terminal native reply lost after broker acceptance");
            error.nativeDispatch = "unknown";
            throw error;
        }
        return result;
    });
    await bootstrapExecutor(executor);
    registerWorkspaceWindowListeners(model.api, executor, () => Promise.resolve(),
        error => model.errors.push(error));
    await model.sync("initial");
    if (fault === "register") await bridge.call("fault", {operation: "register_candidate_lease"});
    if (fault === "bind") await bridge.call("fault", {operation: "bind_x11_window"});
    if (fault === "release") await bridge.call("fault", {operation: "release_candidate_lease"});
    return {model, executor, nativeCalls};
}

async function drain(executor) {
    // Event callbacks enqueue microtasks; a fixed point, not a sleep-based race.
    for (let turn = 0; turn < 20; turn++) {
        const tail = executor.lifecycleTail;
        await Promise.resolve();
        await tail;
        if (tail === executor.lifecycleTail) return;
    }
    throw new Error("Chrome lifecycle event queue did not converge");
}

async function managedCase(bridge, fault = null, pendingCreated = false) {
    const {model, executor, nativeCalls} = await fixture(bridge, fault, pendingCreated);
    const payload = ensurePayload();
    let failure;
    try { await executor.ensure(payload); } catch (error) { failure = error; }
    await drain(executor);
    if (model.cancellation) await model.cancellation;
    assert.deepEqual(model.windows.get(1).tabs, model.original, "ordinary host changed");
    assert.equal(model.errors.length, 0, model.errors.map(error => error.stack).join("\n"));
    if (fault === "terminal-loss") {
        assert(failure, "accepted terminal reply loss must reject this delivery");
        const effects = model.createdAttempts;
        await executor.ensure(payload); // One retry may only replay the immutable terminal result.
        await drain(executor);
        assert.equal(model.createdAttempts, effects);
        const completions = nativeCalls.filter(call =>
            call.operation === "ensure_managed_page_completion");
        assert.equal(completions.length, 2);
        assert.deepEqual(completions[0].payload, completions[1].payload);
        assert.equal(completions[0].payload.completion.outcome, "created");
    }
    const state = await bridge.call("state");
    for (const frame of state.trace) {
        assert.equal(frame.desktop, "e", `desktop changed at ${frame.action}`);
        assert.equal(frame.focus, 900, `focus changed at ${frame.action}`);
        if (!frame.windows[101]) continue; // client_new precedes windows_map insertion.
        assert.equal(frame.windows[101].group, "f", `ordinary host moved at ${frame.action}`);
        assert.equal(frame.windows[101]?.title, "Ordinary one - Google Chrome",
            `ordinary host title changed at ${frame.action}`);
        assert(!Object.keys(frame.windows[101]?.props || {}).some(name =>
            name.startsWith("_ABUSH_") && name !== "_ABUSH_PREMAP_BASELINE"),
        `ordinary host metadata contaminated at ${frame.action}`);
    }
    assert.equal(nativeCalls.filter(call => call.operation === "release_candidate_lease").length,
        fault === "release" ? 2 : fault === "create" ? 0 : 1,
        "failed release is retried only after physical cleanup");
    if (["cancel", "post-arm-cancel"].includes(fault)) {
        assert.equal(executor.workspaces.size, 0);
        assert.equal(executor.pendingTransactions.size, 0);
        assert.equal(model.windows.size, 1);
        if (fault === "post-arm-cancel") {
            const terminal = nativeCalls.find(call => call.operation === "finish_candidate_lease");
            assert.equal(terminal.payload.outcome, "not_invoked");
            assert.equal(model.createdAttempts, 0);
        }
        return;
    }
    if (fault === "release") {
        assert(failure, "release failure must remain observable");
        assert.equal(executor.workspaces.size, 0);
        assert.equal(executor.pendingTransactions.size, 1, "retain durable pending cleanup");
        await executor.restorePendingTransactions();
        assert.equal(executor.pendingTransactions.size, 0, "idempotent recovery releases claim");
        assert.equal(model.windows.size, 1);
        return;
    }
    if (["register", "create", "bind"].includes(fault)) {
        const reached = fault === "create" ? model.createdAttempts === 1 : nativeCalls.some(call =>
            call.operation === (fault === "register" ? "register_candidate_lease" :
                "bind_x11_window"));
        assert(reached, `production failed before injected ${fault} boundary: ${failure?.stack}`);
        assert(failure, `${fault} must reject ensure`);
        assert.equal(executor.workspaces.size, 0);
        assert.equal(model.windows.size, 1);
        if (fault === "create") {
            assert.equal(executor.pendingTransactions.size, 1);
            assert(!nativeCalls.some(call => call.operation === "finish_candidate_lease"));
        }
        return;
    }
    if (fault !== "terminal-loss") assert.ifError(failure);
    assert.equal(executor.workspaces.size, 1,
        `ensure must deliver a real managed page: ${failure?.stack || "no rejection"}`);
    const record = [...executor.workspaces.values()][0];
    assert.equal(await executor.isLive(record), true);
    const created = state.trace.find(frame => frame.action === "map:claimed");
    assert(created, "marker a must claim a registered launch lease on initial map");
    assert.equal(created.windows[created.xid].group, "__abush_quarantine");
    const bind = nativeCalls.find(call => call.operation === "bind_x11_window");
    assert.deepEqual(bind.payload, {nonce: record.nonce, remote_key: payload.remote_key,
        workspace_id: payload.workspace_id, generation: 1, control_state: "remote",
        display_title: "local"});
    const releaseIndex = nativeCalls.findIndex(call => call.operation === "release_candidate_lease");
    const bindIndex = nativeCalls.findIndex(call => call.operation === "bind_x11_window");
    assert(releaseIndex > bindIndex,
        "launch lease release before bind invalidates the claimed Qtile candidate");
    const registered = state.trace.findIndex(frame =>
        frame.action === "native:validated:register_candidate_lease");
    const quarantined = state.trace.findIndex(frame => frame.action === "map:claimed");
    const metadataValidated = state.trace.findIndex(frame => frame.action === "roundtrip" &&
        frame.windows[record.x11WindowId]?.props._ABUSH_BINDING_NONCE === record.nonce &&
        frame.windows[record.x11WindowId]?.group === "__abush_quarantine");
    const promoted = state.trace.findIndex(frame =>
        frame.windows[record.x11WindowId]?.group === "c");
    const bindValidated = state.trace.findIndex(frame =>
        frame.action === "native:validated:bind_x11_window");
    const released = state.trace.findIndex(frame =>
        frame.action === "native:release_candidate_lease");
    assert.deepEqual([registered, quarantined, metadataValidated, promoted, bindValidated, released]
        .map((index, i, indices) => index >= 0 && (i === 0 || index > indices[i - 1])),
    [true, true, true, true, true, true],
    "register -> initial quarantine -> metadata roundtrip -> c -> validated bind -> release");
    assert(state.trace.slice(quarantined, promoted).every(frame =>
        frame.windows[record.x11WindowId]?.group === "__abush_quarantine"),
    "title and tab changes must not release an admitted candidate");
    const final = state.trace.at(-1).windows[record.x11WindowId];
    assert.equal(final.group, "c");
    assert.equal(final.props._ABUSH_BINDING_NONCE, record.nonce);
    assert.equal(final.props._ABUSH_WORKSPACE_ID, payload.workspace_id);
    assert.equal(final.props._ABUSH_WINDOW_GENERATION, "1");
    assert.equal(final.props.WM_WINDOW_ROLE, "abush-browser");
    await model.updateTab(record.pageTabId, {url: "https://business.test/", active: true});
    await drain(executor);
    assert.equal(await executor.isLive(record), true, "business title must not unbind workspace");
    const afterTitle = await bridge.call("state");
    assert.equal(afterTitle.trace.at(-1).windows[record.x11WindowId].group, "c");
    assert.equal(afterTitle.completions.length, 1, "one ensure completion accepted by broker");
    if (fault === "terminal-loss") {
        assert(failure || executor.advisoryErrors.length > 0,
            "terminal reply loss must remain observable without publishing rejection");
    }
}

async function binderCase(bridge, releaseBeforeBind = false) {
    const {model, executor} = await fixture(bridge);
    const nonce = "a".repeat(32);
    const a = await model.createTab({windowId: 1, url: executor.statusUrl({nonce}, "a"),
        active: false});
    const b = await model.createTab({windowId: 1, url: executor.statusUrl({nonce}, "b"),
        active: false});
    await executor.updateCandidateLease("register", nonce);
    await executor.updateCandidateLease("arm", nonce);
    const window = await model.createWindow({tabId: a.id, type: "normal", focused: false});
    const admission = await executor.nativeRequest("abush_browser", "admit_candidate", {nonce});
    assert.equal(admission.first_group, "__abush_quarantine");
    await model.moveTab(b.id, {windowId: window.id, index: -1});
    await model.updateTab(a.id, {pinned: true});
    await model.updateTab(b.id, {pinned: true});
    const page = await model.createTab({windowId: window.id, url: "about:blank", active: true});
    const record = {nonce, remoteKey: "r_12345678", workspaceId: "workspace-authoritative",
        windowGeneration: 1, connectionDisplayName: "local", statusTabIds: [a.id, b.id],
        pageTabId: page.id, windowId: window.id};
    // Admitted claims outlive admission TTL and survive a config reload from X11 properties.
    await bridge.call("recover_claims");
    await model.updateTab(page.id, {url: "https://delayed-candidate.test/", active: true});
    let state = await bridge.call("state");
    assert.equal(state.trace.at(-1).windows[window.id + 100].group, "__abush_quarantine");
    await model.updateTab(a.id, {active: true});
    if (releaseBeforeBind) {
        await executor.updateCandidateLease("release", nonce);
        await assert.rejects(executor.bind(record),
            /candidate|claim|quarantine/i,
            "released Qtile claims must not remain bindable");
        return;
    }
    await executor.bind(record);
    await executor.updateCandidateLease("release", nonce);
    assert.equal(await executor.isLive(record), true);
    await model.updateTab(page.id, {url: "https://business.test/", active: true});
    await drain(executor);
    const {trace} = await bridge.call("state");
    assert(trace.some(frame => frame.action === "map:claimed"));
    assert(trace.some(frame => frame.action === "ipc:quarantine_abush_browser_window"));
    assert(trace.some(frame => frame.action === "ipc:place_abush_browser_window"));
    const final = trace.at(-1).windows[record.x11WindowId];
    assert.equal(final.group, "c");
    assert.equal(final.props._ABUSH_BINDING_NONCE, nonce);
    assert.equal(final.props._ABUSH_WORKSPACE_ID, record.workspaceId);
    assert.equal(final.props._ABUSH_WINDOW_GENERATION, "1");
    assert(trace.every(frame => frame.desktop === "e" && frame.focus === 900));
    await model.updateTab(b.id, {pinned: false});
    assert.equal(await executor.isLive(record), false, "unpinned marker is not a live workspace");
    await model.updateTab(b.id, {pinned: true});
    assert.equal(await executor.isLive(record), true);
    const tabs = model.window(window.id).tabs;
    [tabs[0], tabs[1]] = [tabs[1], tabs[0]];
    model.indices(window.id);
    await model.sync("user-reordered-markers");
    assert.equal(await executor.isLive(record), false,
        "marker titles alone do not prove the exact physical index fingerprint");
}

async function expiredAdmissionCase(bridge) {
    const {model, executor} = await fixture(bridge);
    const nonce = "b".repeat(32);
    const a = await model.createTab({windowId: 1, url: executor.statusUrl({nonce}, "a"),
        active: false});
    await executor.updateCandidateLease("register", nonce);
    await bridge.call("expire_admission");
    await assert.rejects(executor.updateCandidateLease("arm", nonce), /expired before dispatch/);
    const {trace} = await bridge.call("state");
    assert(!trace.some(frame => frame.action === "native:validated:arm_candidate_lease"),
        "expired reservations must reject before windows.create dispatch");
    await executor.updateCandidateLease("release", nonce);
}

async function unknownTerminalCase(bridge) {
    const {model, executor, nativeCalls} = await fixture(bridge, "unknown-create");
    await assert.rejects(executor.ensure(ensurePayload()));
    assert.equal(model.createdAttempts, 1);
    assert.equal(executor.pendingTransactions.size, 1);
    assert(!nativeCalls.some(call => call.operation === "finish_candidate_lease"));
    assert(!nativeCalls.some(call => call.operation === "release_candidate_lease"));
    const transaction = [...executor.pendingTransactions.values()][0];
    assert.equal(transaction.admissionSettlement, undefined);
    assert.equal(transaction.admission.chrome, "unknown");
    const restarted = new WorkspaceExecutor(model.api, executor.nativeRequest);
    await bootstrapExecutor(restarted);
    await restarted.restorePendingTransactions();
    assert.equal(restarted.pendingTransactions.size, 1);
    assert(restarted.advisoryErrors.length > 0);
}

async function partialRejectionCase(bridge) {
    const {model, executor, nativeCalls} = await fixture(bridge, "partial-reject");
    await assert.rejects(executor.ensure(ensurePayload()));
    assert.equal(model.createdAttempts, 1);
    assert.equal(model.windows.size, 2, "unproven empty stranger must not be deleted");
    assert.equal(model.window(1).tabs.length, 2, "markers cleaned from original host");
    assert.equal(model.window(10).tabs.length, 0);
    assert.equal(executor.pendingTransactions.size, 1);
    assert(!nativeCalls.some(call => call.operation === "finish_candidate_lease"));
    assert(!nativeCalls.some(call => call.operation === "release_candidate_lease"));
    const restarted = new WorkspaceExecutor(model.api, executor.nativeRequest);
    await bootstrapExecutor(restarted);
    await restarted.restorePendingTransactions();
    assert.equal(restarted.pendingTransactions.size, 1);
    const state = await bridge.call("state");
    assert(state.trace.every(frame => !frame.windows[110]), "partial window never reaches f");
    assert(state.trace.every(frame => frame.desktop === "e" && frame.focus === 900));
}

async function terminalRecoveryCase(bridge, fault) {
    const {model, executor} = await fixture(bridge);
    const originalNative = executor.nativeRequest;
    let injected = false;
    if (fault === "terminal-wal") {
        const set = model.api.storage.local.set;
        model.api.storage.local.set = async value => {
            if (!injected && value.abushPendingTransactions?.some(transaction =>
                transaction.admission_terminal)) {
                injected = true;
                throw new Error("terminal WAL failed after persisted settlement");
            }
            return set(value);
        };
    }
    if (["fulfilled-cleanup", "finish-loss", "terminal-wal"].includes(fault)) {
        await bridge.call("fault", {operation: "admit_candidate"});
    }
    executor.nativeRequest = async (service, operation, payload) => {
        const result = await originalNative(service, operation, payload);
        if (fault === "wal-before-call" && operation === "arm_candidate_lease") {
            const set = model.api.storage.local.set;
            model.api.storage.local.set = async value => {
                if (!injected && value.abushPendingTransactions?.some(transaction =>
                    transaction.admission?.chrome === "unknown")) {
                    injected = true;
                    throw new Error("injected pre-call WAL failure");
                }
                return set(value);
            };
        }
        if (fault === "finish-loss" && operation === "finish_candidate_lease" && !injected) {
            injected = true;
            throw new Error("finish ACK lost after durable terminal");
        }
        return result;
    };
    await assert.rejects(executor.ensure(ensurePayload()));
    assert(injected || fault === "fulfilled-cleanup");
    if (fault === "fulfilled-cleanup") {
        assert.equal(model.createdAttempts, 1);
        assert.equal(executor.pendingTransactions.size, 0);
        assert.equal(model.windows.size, 1);
    } else if (fault === "wal-before-call") {
        assert.equal(model.createdAttempts, 0);
        assert.equal(executor.pendingTransactions.size, 0);
    } else {
        assert.equal(executor.pendingTransactions.size, 1);
        const restarted = new WorkspaceExecutor(model.api, originalNative);
        await bootstrapExecutor(restarted);
        await restarted.restorePendingTransactions();
        assert.equal(restarted.pendingTransactions.size, 0);
        assert.equal(model.createdAttempts, 1);
    }
    const state = await bridge.call("state");
    assert(state.trace.every(frame => frame.desktop === "e" && frame.focus === 900));
    await originalNative("abush_browser", "register_candidate_lease",
        {nonce: "e".repeat(32), operation_token: "f".repeat(32)});
}

async function unknownOrdinaryCase(bridge) {
    const {model, executor} = await fixture(bridge);
    const nonce = "a".repeat(32);
    await executor.updateCandidateLease("register", nonce);
    await executor.updateCandidateLease("arm", nonce);
    const candidateId = 40;
    model.windows.set(candidateId, {id: candidateId, type: "normal", focused: false,
        tabs: [{id: 4000, windowId: candidateId, index: 0, url: "about:blank",
            title: "New Tab", active: true, pinned: false, groupId: -1}]});
    await model.sync("unknown-candidate");
    const ordinaryNonce = "b".repeat(32);
    const ordinaryId = 41;
    const url = executor.chrome.runtime.getURL("ordinary-staging.html") + "#" + ordinaryNonce;
    model.windows.set(ordinaryId, {id: ordinaryId, type: "normal", focused: false,
        tabs: [{id: 4100, windowId: ordinaryId, index: 0, url,
            title: `__ABUSH_ORDINARY__:${ordinaryNonce}`, active: true, pinned: false, groupId: -1}]});
    await model.sync("explicit-ordinary");
    const prepared = await executor.nativeRequest("ordinary_open", "prepare",
        {nonce: ordinaryNonce, deadline_ms: Date.now() + 10000});
    assert.deepEqual(prepared, {nonce: ordinaryNonce, xid: 141});
    await executor.nativeRequest("ordinary_open", "prepare",
        {nonce: ordinaryNonce, deadline_ms: Date.now() + 10000});
    let state = await bridge.call("state");
    assert(state.trace.every(frame => !frame.windows[140]), "unknown candidate stays unmapped");
    assert(state.trace.every(frame => frame.desktop === "e" && frame.focus === 900));
    assert.equal(state.trace.filter(frame => frame.action === "constructor:set_group" &&
        frame.xid === 141).length, 1);
    assert(state.trace.filter(frame => frame.windows[141]).every(frame =>
        frame.windows[141].group === "f"));
    await assert.rejects(executor.updateCandidateLease("release", nonce), /unknown|not fully verified/);
    await executor.nativeRequest("ordinary_open", "activate", {...prepared,
        deadline_ms: Date.now() + 10000});
    state = await bridge.call("state");
    assert.equal(state.trace.at(-1).focus, 141);
    assert.equal(state.trace.at(-1).desktop, "f");
    assert(state.trace.every(frame => !frame.windows[140]));
}

async function ordinaryCase(bridge) {
    const {model, executor} = await fixture(bridge);
    const payload = {version: 1, request_id: "ordinary-request", operation: "ordinary_open",
        url: "https://ordinary-request.test/", focus: true, deadline_ms: Date.now() + 10000};
    const completion = await executor.ordinaryOpen(payload);
    await drain(executor);
    assert.equal(completion, "applied");
    assert.deepEqual(model.windows.get(1).tabs.slice(0, 2).map(tab => ({...tab,
        active: tab.id === 1})), model.original);
    assert.equal(model.windows.get(1).tabs.filter(tab => tab.url === payload.url).length, 1);
    const state = await bridge.call("state");
    assert.equal(state.completions.length, 1);
    assert.equal(state.trace.at(-1).desktop, "f");
    assert.equal(state.trace.at(-1).focus, 101);
    assert(state.trace.filter(frame => frame.windows[101])
        .every(frame => frame.windows[101].group === "f"));
}

let gateCompleted = false;
process.once("beforeExit", () => {
    if (!gateCompleted) {
        console.error("lifecycle release gate exited before completing its selected cases");
        process.exitCode = 1;
    }
});

(async () => {
    await assert.rejects(boundedCase("unsettled fixture", () => new Promise(() => {}), 20),
        /unsettled fixture exceeded/);
    const bridge = new Bridge();
    const failures = [];
    let selected = 0;
    let completed = 0;
    try {
        for (const [name, run] of [
            ["ordinary open", () => ordinaryCase(bridge)],
            ["unknown candidate exact ordinary", () => unknownOrdinaryCase(bridge)],
            ["unknown API bounded durable denial", () => unknownTerminalCase(bridge)],
            ["partial allocation rejected keeps durable denial", () => partialRejectionCase(bridge)],
            ["terminal ACK loss worker restart", () => terminalRecoveryCase(bridge, "finish-loss")],
            ["pre-call WAL failure", () => terminalRecoveryCase(bridge, "wal-before-call")],
            ["terminal WAL failure restart", () => terminalRecoveryCase(bridge, "terminal-wal")],
            ["fulfilled exact candidate cleanup", () => terminalRecoveryCase(bridge, "fulfilled-cleanup")],
            ["managed post-arm cancel", () => managedCase(bridge, "post-arm-cancel")],
            ["exact binder and Qtile argument casting", () => binderCase(bridge)],
            ["unadmitted admission expires", () => expiredAdmissionCase(bridge)],
            ["released candidate cannot bind", () => binderCase(bridge, true)],
            ["managed URL event before promise", () => managedCase(bridge)],
            ["managed pendingUrl event before promise", () => managedCase(bridge, null, true)],
            ...["register", "create", "bind", "release", "cancel", "terminal-loss"].map(fault =>
                [`managed ${fault}`, () => managedCase(bridge, fault,
                    ["register", "create"].includes(fault))]),
        ]) {
            if (process.env.LIFECYCLE_CASE && !new RegExp(process.env.LIFECYCLE_CASE).test(name)) {
                continue;
            }
            selected++;
            try {
                await boundedCase(name, run);
                completed++;
            } catch (error) {
                failures.push(`${name}: ${error.stack}`);
                if (error.lifecycleDeadline) throw error;
            }
        }
        assert(selected > 0, "lifecycle release gate selected no cases");
        if (failures.length) throw new Error(failures.join("\n\n"));
        assert.equal(completed, selected, "every selected lifecycle case must finish");
    } finally { await bridge.close(); }
    gateCompleted = true;
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
