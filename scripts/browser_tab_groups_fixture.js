const assert = require("node:assert/strict");
global.crypto ||= require("node:crypto").webcrypto;
const {WorkspaceExecutor, bounded, ORDINARY_PREFIX} =
    require("../chromium/external-links/extension/workspace.js");
const {registerRuntimeMessageRouter, registerWorkspaceWindowListeners, registerNativeEventRouter} =
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
    let timer;
    const promise = new Promise((yes, no) => {
        resolve = value => { clearTimeout(timer); yes(value); };
        reject = error => { clearTimeout(timer); no(error); };
        timer = setTimeout(() => no(new Error("Fixture barrier timed out")), 4000);
        timer.unref?.();
    });
    promise.catch(() => {});
    return {promise, resolve, reject};
}
const BOOT = {browser_boot_id: "boot-one", browser_pid: 123,
    browser_starttime: "456", ordinary_launch_leases: []};
const REMOTE = {remote_key: "remote", connection_id: "connection", principal_id: "principal",
    workspace_id: "workspace", connected: true, executor_fence: {executor_id: "executor", epoch: 1},
    owner_display: {daemon_instance_id: "instance", daemon_boot_id: "daemon-boot",
        workbench_name: "Workbench"}, connection_display_name: "local"};
function fixture({globalAutomation = false} = {}) {
    const calls = [];
    const local = {abushOwnedMarkers: {legacy: true}};
    if (!globalAutomation) local.abushBrowserAutomationV9 = false;
    const session = {};
    const windows = new Map([[1, {id: 1, type: "normal", focused: true}],
        [2, {id: 2, type: "popup", focused: false}]]);
    const tabs = new Map([[1, {id: 1, windowId: 1, url: "https://ordinary.test/a",
        groupId: -1, active: true}], [2, {id: 2, windowId: 2,
        url: "https://popup.test/", groupId: -1, active: true}]]);
    const groups = new Map();
    let nextTab = 10;
    let nextGroup = 100;
    let browserRestart = 0;
    function syncGroups() {
        for (const current of tabs.values()) {
            if (current.groupId < 0) continue;
            if (!groups.has(current.groupId)) groups.set(current.groupId, {
                id: current.groupId, windowId: current.windowId,
                title: "", color: "grey", collapsed: false,
            });
            nextGroup = Math.max(nextGroup, current.groupId + 1);
        }
        for (const [id, group] of groups) {
            if (![...tabs.values()].some(current => current.groupId === id)) {
                groups.delete(id);
                api.tabGroups.onRemoved.emit(clone(group));
            }
        }
    }
    function group(id) {
        syncGroups();
        if (!groups.has(id)) throw new Error("No group");
        return groups.get(id);
    }
    const hooks = {};
    const attachedTabs = new Set();
    const invoke = async (name, args, run) => {
        calls.push({name, args: clone(args)});
        if (hooks[name]) {
            const override = await hooks[name](...args);
            if (override !== undefined) return override;
        }
        const result = run();
        if (hooks[`${name}.after`]) await hooks[`${name}.after`](result, ...args);
        return result;
    };
    const storage = (state, name) => ({get: () => invoke(`${name}.get`, [], () => clone(state)),
        set: value => invoke(`${name}.set`, [value], () => Object.assign(state, clone(value)))});
    const tab = id => {
        if (!tabs.has(id)) throw new Error("No tab");
        return tabs.get(id);
    };
    const api = {
        runtime: {id: "extension", getURL: file => `chrome-extension://extension/${file}`,
            onMessage: event()}, storage: {local: storage(local, "local"),
            session: storage(session, "session")},
        windows: {getCurrent: () => invoke("windows.getCurrent", [], () =>
            clone([...windows.values()].find(window => window.focused) || windows.values().next().value)),
            get: id => invoke("windows.get", [id], () => clone(windows.get(id))),
            getAll: () => invoke("windows.getAll", [], () => clone([...windows.values()])),
            onCreated: event(), create: args => invoke("windows.create", [args], () => {
                const window = {id: 3, type: "normal", focused: args.focused};
                windows.set(3, window);
                const created = createTab({...args, windowId: 3, active: true});
                return {...window, tabs: [created]};
            })},
        tabs: {sendMessage: (id, message) => invoke("tabs.sendMessage", [id, message], () => {}),
            get: id => invoke("tabs.get", [id], () => clone(tab(id))),
            query: (query = {}) => invoke("tabs.query", [query], () => clone([...tabs.values()].filter(
                current => Object.entries(query).every(([key, value]) => current[key] === value)))),
            create: args => invoke("tabs.create", [args], () => createTab(args)),
            group: args => invoke("tabs.group", [args], () => {
                syncGroups();
                const ids = Array.isArray(args.tabIds) ? args.tabIds : [args.tabIds];
                const windowId = tab(ids[0]).windowId;
                if (ids.some(id => tab(id).windowId !== windowId)) throw new Error("Different windows");
                if (args.groupId !== undefined && group(args.groupId).windowId !== windowId) {
                    throw new Error("Group belongs to another window");
                }
                const groupId = args.groupId ?? nextGroup++;
                if (!groups.has(groupId)) {
                    const created = {id: groupId, windowId, title: "", color: "grey", collapsed: false};
                    groups.set(groupId, created);
                    api.tabGroups.onCreated.emit(clone(created));
                }
                ids.forEach(id => {
                    tab(id).groupId = groupId;
                    api.tabs.onMoved.emit(id, {windowId: tab(id).windowId, fromIndex: 0, toIndex: 1});
                    api.tabs.onUpdated.emit(id, {groupId}, clone(tab(id)));
                });
                syncGroups();
                return groupId;
            }), ungroup: ids => invoke("tabs.ungroup", [ids], () => {
                ids.forEach(id => {
                    tab(id).groupId = -1;
                    api.tabs.onUpdated.emit(id, {groupId: -1}, clone(tab(id)));
                    api.tabs.onMoved.emit(id, {windowId: tab(id).windowId, fromIndex: 1, toIndex: 0});
                });
                syncGroups();
            }), update: (id, args) => invoke("tabs.update", [id, args], () => {
                if (args.active) activate(tab(id));
                Object.assign(tab(id), args);
                return clone(tab(id));
            }), onRemoved: event(), onMoved: event(), onDetached: event(), onAttached: event(),
            onActivated: event(), onReplaced: event(), onCreated: event(), onUpdated: event()},
        tabGroups: {onUpdated: event(), onCreated: event(), onRemoved: event(), onMoved: event(),
            get: id => invoke("tabGroups.get", [id], () => clone(group(id))),
            query: (query = {}) => invoke("tabGroups.query", [query], () => {
                syncGroups();
                return clone([...groups.values()].filter(item => Object.entries(query).every(
                    ([key, value]) => item[key] === value)));
            }),
            update: (id, args) => invoke("tabGroups.update", [id, args], () => {
                const updated = Object.assign(group(id), clone(args));
                api.tabGroups.onUpdated.emit(clone(updated));
                return clone(updated);
            })},
        debugger: {attach: (target, version) => invoke("debugger.attach", [target, version], () => {
            if (attachedTabs.has(target.tabId)) throw new Error("Already attached");
            attachedTabs.add(target.tabId);
        }), detach: target => invoke("debugger.detach", [target], () => {
            if (!attachedTabs.delete(target.tabId)) throw new Error("Not attached");
        }),
            sendCommand: (target, method, args) => invoke("debugger.sendCommand", [target, method, args],
                () => { tab(target.tabId).url = args.url; return {frameId: `frame-${target.tabId}`, loaderId: "loader"}; })},
    };
    function activate(current) {
        for (const item of tabs.values()) {
            if (item.windowId === current.windowId) item.active = item.id === current.id;
        }
        api.tabs.onActivated.emit({tabId: current.id, windowId: current.windowId});
    }
    function createTab(args) {
        const active = [...tabs.values()].find(item => item.windowId === args.windowId && item.active);
        const current = {id: nextTab++, windowId: args.windowId, url: args.url,
            active: args.active, groupId: active?.groupId ?? -1,
            ...(args.openerTabId !== undefined ? {openerTabId: args.openerTabId} : {})};
        if (args.url.includes("ordinary-staging.html#")) {
            current.title = ORDINARY_PREFIX + args.url.split("#")[1];
        }
        tabs.set(current.id, current);
        if (current.active) activate(current);
        api.tabs.onCreated.emit(clone(current));
        for (const item of tabs.values()) {
            if (item.id !== current.id && item.windowId === current.windowId) {
                api.tabs.onMoved.emit(item.id, {windowId: item.windowId, fromIndex: 0, toIndex: 1});
            }
        }
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
        if (["prepare", "activate"].includes(operation)) return {nonce: payload.nonce, xid: 42};
        if (operation === "ordinary_file_navigate") {
            const current = [...tabs.values()].find(item => item.url.endsWith(`#${payload.nonce}`));
            current.url = payload.url;
            return {...payload, completion: "applied"};
        }
        return {};
    });
    let executor = new WorkspaceExecutor(api, native);
    executor.manualControls = !globalAutomation;
    const trusted = {id: api.runtime.id, url: api.runtime.getURL("popup.html")};
    const nativeEvents = event();
    const errors = [];
    function route() {
        api.runtime.onMessage.listeners.length = 0;
        nativeEvents.listeners.length = 0;
        for (const name of ["onRemoved", "onMoved", "onDetached", "onAttached", "onReplaced", "onCreated", "onUpdated", "onActivated"]) {
            api.tabs[name].listeners.length = 0;
        }
        api.windows.onCreated.listeners.length = 0;
        for (const name of ["onUpdated", "onCreated", "onRemoved", "onMoved"]) {
            api.tabGroups[name].listeners.length = 0;
        }
        registerWorkspaceWindowListeners(api, executor, async () => {}, error => errors.push(error.message));
        registerNativeEventRouter({onEvent: nativeEvents.addListener}, executor,
            async () => {}, message => errors.push(message));
        registerRuntimeMessageRouter(api, executor, async () => {},
            (url, source, cb) => cb({ok: true}), globalAutomation ? null :
                async (message, ready) => {
                    const revoke = ["pause", "takeover", "disconnect"].includes(message.control);
                    if (!revoke) await ready();
                    return executor.control(message.control, message.remote_key, revoke ? ready : null);
                });
    }
    route();
    const send = (message, sender = trusted) => bounded(() => new Promise(resolve =>
        api.runtime.onMessage.emit(message, sender, resolve)), "runtime response", 4000);
    const materializations = new Map();
    const result = {api, calls, tabs, windows, groups, syncGroups, local, session, hooks, attachedTabs, trusted, send, errors,
        emitRemote: payload => Promise.all(nativeEvents.emit({service: "abush_browser",
            event: "abush_browser.remote_state", payload})),
        emitNative: payload => Promise.all(nativeEvents.emit({service: "abush_browser",
            event: "abush_browser.execute", payload})),
        get executor() { return executor; }, async ready() {
            await executor.acceptNativeBootstrap(BOOT);
            await executor.restoreWhenReady();
            await executor.remoteState(REMOTE);
        }, async materialize() {
            const record = executor.recordFor("remote");
            assert.ok(record, "ready must reserve a work page first");
            const key = `${record.browser_boot_id}\0${record.enable_id}`;
            if (!materializations.has(key)) {
                materializations.set(key, executor.execute(execute(result)));
            }
            await materializations.get(key);
            return record;
        }, async enable() {
            const response = await send({control: "enable", remote_key: "remote"});
            assert.equal(response.ok, true, response.error);
            return executor.records.get("remote");
        }, resetWorker() {
            executor = new WorkspaceExecutor(api, native);
            executor.manualControls = !globalAutomation;
            route();
            return executor;
        }, async restartBrowser({beforeBootstrap = async () => {}} = {}) {
            for (const candidate of executor.personalCandidates.values()) clearTimeout(candidate.timer);
            syncGroups();
            const offset = ++browserRestart * 1000;
            const windowIds = new Map([...windows.keys()].map(id => [id, id + offset]));
            const tabIds = new Map([...tabs.keys()].map(id => [id, id + offset]));
            const groupIds = new Map([...groups.keys()].map(id => [id, id + offset]));
            const restoredWindows = [...windows.values()].map(item => ({...item,
                id: windowIds.get(item.id)}));
            const restoredTabs = [...tabs.values()].map(item => ({...item,
                id: tabIds.get(item.id), windowId: windowIds.get(item.windowId),
                groupId: item.groupId < 0 ? -1 : groupIds.get(item.groupId)}));
            const restoredGroups = [...groups.values()].map(item => ({...item,
                id: groupIds.get(item.id), windowId: windowIds.get(item.windowId)}));
            windows.clear(); tabs.clear(); groups.clear(); attachedTabs.clear();
            restoredWindows.forEach(item => windows.set(item.id, item));
            restoredTabs.forEach(item => tabs.set(item.id, item));
            restoredGroups.forEach(item => groups.set(item.id, item));
            nextTab = Math.max(nextTab, ...tabs.keys()) + 1;
            nextGroup = Math.max(nextGroup, ...groups.keys()) + 1;
            for (const key of Object.keys(session)) delete session[key];
            executor = new WorkspaceExecutor(api, native);
            executor.manualControls = !globalAutomation;
            route();
            const boot = {...BOOT, browser_boot_id: `restored-boot-${browserRestart}`,
                browser_pid: BOOT.browser_pid + browserRestart};
            const restored = {windowIds, tabIds, groupIds, boot};
            await beforeBootstrap(restored);
            await executor.acceptNativeBootstrap(boot);
            await executor.restoreWhenReady();
            return restored;
        }, async restart(boot = BOOT) {
            executor = new WorkspaceExecutor(api, native);
            executor.manualControls = !globalAutomation;
            route();
            await executor.acceptNativeBootstrap(boot);
            await executor.restoreWhenReady();
            return executor;
        }};
    return result;
}
function execute(f, overrides = {}) {
    const record = f.executor.recordFor("remote");
    return {remote_key: "remote", connection_id: "connection", principal_id: "principal",
        request_id: crypto.randomUUID(), effect_id: crypto.randomUUID(),
        executor_fence: clone(REMOTE.executor_fence), authorization: {
            browser_boot_id: f.executor.browserBootId || BOOT.browser_boot_id,
            enable_id: record?.enable_id || "disabled",
            revision: record?.revision || 1}, command: {command: "navigate",
            page: clone(record?.page || {workspace_id: "workspace", page_id: "none", generation: 1}),
            url: "https://work.test/exact?q=one#fragment"}, ...overrides};
}
async function runCases(cases) {
    const failures = [];
    for (const [name, run] of cases) {
        try {
            await bounded(run, `case ${name}`, 10000);
            if (process.env.ABUSH_BROWSER_TEST_VERBOSE === "1") console.log(`ok ${name}`);
        } catch (error) {
            failures.push(name);
            console.error(`FAIL ${name}: ${error.stack}`);
        }
    }
    if (failures.length) throw new Error(`${failures.length}/${cases.length} cases failed`);
    console.log(`${cases.length} cases passed`);
}
module.exports = {assert, clone, deferred, fixture, execute, runCases, BOOT, REMOTE};
