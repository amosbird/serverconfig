const assert = require("node:assert/strict");
global.crypto ||= require("node:crypto").webcrypto;
const {WorkspaceExecutor, bounded, ORDINARY_PREFIX} =
    require("../chromium/external-links/extension/workspace.js");
const {registerRuntimeMessageRouter, registerWorkspaceWindowListeners} =
    require("../chromium/external-links/extension/background.js");
const clone = value => structuredClone(value);
function event() {
    const listeners = [];
    return {listeners, addListener: fn => listeners.push(fn),
        emit: (...args) => listeners.map(fn => fn(...args))};
}
function deferred() {
    let resolve;
    let reject;
    const promise = new Promise((yes, no) => { resolve = yes; reject = no; });
    return {promise, resolve, reject};
}
const BOOT = {browser_boot_id: "boot-one", browser_pid: 123,
    browser_starttime: "456", ordinary_launch_leases: []};
const REMOTE = {remote_key: "remote", connection_id: "connection", principal_id: "principal",
    workspace_id: "workspace", connected: true, executor_fence: {executor_id: "executor", epoch: 1},
    owner_display: {daemon_instance_id: "instance", daemon_boot_id: "daemon-boot",
        workbench_name: "Workbench"}, connection_display_name: "local"};
function fixture() {
    const calls = [];
    const local = {abushOwnedMarkers: {legacy: true}};
    const session = {};
    const windows = new Map([[1, {id: 1, type: "normal", focused: true}],
        [2, {id: 2, type: "popup", focused: false}]]);
    const tabs = new Map([[1, {id: 1, windowId: 1, url: "https://ordinary.test/a",
        groupId: -1, active: true}], [2, {id: 2, windowId: 2,
        url: "https://popup.test/", groupId: -1, active: true}]]);
    let nextTab = 10;
    let nextGroup = 100;
    const hooks = {};
    const invoke = async (name, args, run) => {
        calls.push({name, args: clone(args)});
        if (hooks[name]) await hooks[name](...args);
        return run();
    };
    const storage = (state, name) => ({get: async () => clone(state),
        set: value => invoke(`${name}.set`, [value], () => Object.assign(state, clone(value)))});
    const tab = id => {
        if (!tabs.has(id)) throw new Error("No tab");
        return tabs.get(id);
    };
    const api = {
        runtime: {id: "extension", getURL: file => `chrome-extension://extension/${file}`,
            onMessage: event()}, storage: {local: storage(local, "local"),
            session: storage(session, "session")},
        windows: {getCurrent: () => invoke("windows.getCurrent", [], () => clone(windows.get(1))),
            get: id => invoke("windows.get", [id], () => clone(windows.get(id))),
            getAll: () => invoke("windows.getAll", [], () => clone([...windows.values()])),
            create: args => invoke("windows.create", [args], () => {
                const window = {id: 3, type: "normal", focused: args.focused};
                windows.set(3, window);
                const created = createTab({...args, windowId: 3, active: true});
                return {...window, tabs: [created]};
            })},
        tabs: {get: id => invoke("tabs.get", [id], () => clone(tab(id))),
            create: args => invoke("tabs.create", [args], () => createTab(args)),
            group: args => invoke("tabs.group", [args], () => {
                const group = nextGroup++;
                args.tabIds.forEach(id => { tab(id).groupId = group; });
                return group;
            }), ungroup: ids => invoke("tabs.ungroup", [ids], () => {
                ids.forEach(id => { tab(id).groupId = -1; });
            }), update: (id, args) => invoke("tabs.update", [id, args], () => {
                if (args.active) activate(tab(id));
                Object.assign(tab(id), args);
                return clone(tab(id));
            }), onRemoved: event(), onMoved: event(), onDetached: event(), onAttached: event(),
            onReplaced: event(), onCreated: event(), onUpdated: event()},
        tabGroups: {update: (id, args) => invoke("tabGroups.update", [id, args], () => ({id, ...args}))},
        debugger: {attach: (target, version) => invoke("debugger.attach", [target, version], () => {}),
            detach: target => invoke("debugger.detach", [target], () => {}),
            sendCommand: (target, method, args) => invoke("debugger.sendCommand", [target, method, args],
                () => { tab(target.tabId).url = args.url; return {}; })},
    };
    function activate(current) {
        for (const item of tabs.values()) {
            if (item.windowId === current.windowId) item.active = item.id === current.id;
        }
    }
    function createTab(args) {
        const active = [...tabs.values()].find(item => item.windowId === args.windowId && item.active);
        const current = {id: nextTab++, windowId: args.windowId, url: args.url,
            active: args.active, groupId: active?.groupId ?? -1};
        if (args.url.includes("ordinary-staging.html#")) {
            current.title = ORDINARY_PREFIX + args.url.split("#")[1];
        }
        tabs.set(current.id, current);
        if (current.active) activate(current);
        return clone(current);
    }
    const native = (service, operation, payload) => invoke(`native.${operation}`, [payload], () => {
        if (operation === "refresh_remotes") return {remotes: [clone(REMOTE)]};
        if (operation === "workspace_state") {
            assert.deepEqual(Object.keys(payload).sort(), ["remote_key", "connection_id", "principal_id",
                "request_id", "executor_fence", "state"].sort());
            assert.deepEqual(Object.keys(payload.state).sort(),
                ["browser_boot_id", "revision", "phase", "binding"].sort());
            assert.ok(Number.isSafeInteger(payload.state.revision) && payload.state.revision > 0);
            if (payload.state.binding) {
                assert.deepEqual(Object.keys(payload.state.binding).sort(), ["enable_id", "page"]);
                assert.deepEqual(Object.keys(payload.state.binding.page).sort(),
                    ["workspace_id", "page_id", "generation"].sort());
            }
            return {type: "workspace_state_accepted", request_id: payload.request_id,
                executor_fence: payload.executor_fence, state: payload.state};
        }
        if (operation === "ordinary_open_completion") {
            assert.deepEqual(Object.keys(payload).sort(),
                ["version", "operation", "request_id", "completion"].sort());
            assert.equal(payload.operation, "ordinary_open_completed");
            return {};
        }
        if (["executor_completion", "cancellation_completion"].includes(operation)) {
            return {type: operation === "executor_completion" ? "completion_accepted" :
                "cancellation_completion_accepted", request_id: payload.request_id,
            effect_id: payload.effect_id, executor_fence: payload.executor_fence,
            completion: payload.completion};
        }
        if (operation === "prepare") return {nonce: payload.nonce, xid: 42};
        if (operation === "ordinary_file_navigate") {
            const current = [...tabs.values()].find(item => item.url.endsWith(`#${payload.nonce}`));
            current.url = payload.url;
            return {...payload, completion: "applied"};
        }
        return {};
    });
    let executor = new WorkspaceExecutor(api, native);
    const trusted = {id: api.runtime.id, url: api.runtime.getURL("popup.html")};
    function route() {
        api.runtime.onMessage.listeners.length = 0;
        registerRuntimeMessageRouter(api, executor, async () => {},
            (url, source, cb) => cb({ok: true}));
    }
    route();
    const send = (message, sender = trusted) => bounded(() => new Promise(resolve =>
        api.runtime.onMessage.emit(message, sender, resolve)), "runtime response", 4000);
    return {api, calls, tabs, windows, local, session, hooks, trusted, send,
        get executor() { return executor; }, async ready() {
            await executor.acceptNativeBootstrap(BOOT);
            await executor.restoreWhenReady();
            await executor.remoteState(REMOTE);
        }, async enable() {
            const response = await send({control: "enable", remote_key: "remote"});
            assert.equal(response.ok, true, response.error);
            return executor.records.get("remote");
        }, async restart(boot = BOOT) {
            executor = new WorkspaceExecutor(api, native);
            route();
            await executor.acceptNativeBootstrap(boot);
            await executor.restoreWhenReady();
            return executor;
        }, listeners() {
            registerWorkspaceWindowListeners(api, executor, async () => {}, error => { throw error; });
        }};
}
function execute(f, overrides = {}) {
    const record = f.executor.records.get("remote");
    return {remote_key: "remote", connection_id: "connection", principal_id: "principal",
        request_id: crypto.randomUUID(), effect_id: crypto.randomUUID(),
        executor_fence: clone(REMOTE.executor_fence), authorization: {
            browser_boot_id: BOOT.browser_boot_id, enable_id: record?.enable_id || "disabled",
            revision: record?.revision || 1}, command: {command: "navigate",
            page: clone(record?.page || {workspace_id: "workspace", page_id: "none", generation: 1}),
            url: "https://work.test/exact?q=one#fragment"}, ...overrides};
}
async function runCases(cases) {
    for (const [name, run] of cases) {
        await bounded(run, `case ${name}`, 10000);
        console.log(`ok ${name}`);
    }
    console.log(`${cases.length} cases passed`);
}
module.exports = {assert, clone, deferred, fixture, execute, runCases, BOOT, REMOTE};
