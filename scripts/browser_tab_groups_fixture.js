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
        removeListener: fn => { const index = listeners.indexOf(fn); if (index >= 0) listeners.splice(index, 1); },
        emit: (...args) => [...listeners].map(fn => fn(...args))};
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
function fixture({globalAutomation = false, inheritActiveGroup = true} = {}) {
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
    const tabOrders = new Map();
    const presentationFrames = [];
    function tabSnapshot(current) {
        return {...clone(current), index: tabStrip(current.windowId).indexOf(current.id)};
    }
    function tabStrip(windowId) {
        const order = (tabOrders.get(windowId) || []).filter(id => tabs.get(id)?.windowId === windowId);
        for (const current of tabs.values()) {
            if (current.windowId === windowId && !order.includes(current.id)) order.push(current.id);
        }
        tabOrders.set(windowId, order);
        return order;
    }
    function recordPresentation(name, args) {
        presentationFrames.push({name, args: clone(args),
            strips: [...windows.keys()].map(windowId => ({windowId, ids: [...tabStrip(windowId)]})),
            tabs: [...tabs.values()].map(tabSnapshot), groups: clone([...groups.values()]),
            windows: clone([...windows.values()])});
    }
    function insertionIndex(order, index, pinned = false) {
        const prefix = order.filter(id => tab(id).pinned).length;
        const clamped = index < 0 ? order.length : Math.max(0, Math.min(index, order.length));
        return pinned ? Math.min(clamped, prefix) : Math.max(clamped, prefix);
    }
    function outsideGroup(order, index, groupId = -1) {
        const left = tabs.get(order[index - 1]);
        const right = tabs.get(order[index]);
        if (left?.groupId >= 0 && left.groupId === right?.groupId &&
            left.groupId !== groupId) {
            while (index < order.length && tab(order[index]).groupId === left.groupId) index++;
        }
        return index;
    }
    function moveTab(id, index, groupId = tab(id).groupId, changed = null) {
        const current = tab(id);
        const order = tabStrip(current.windowId);
        const fromIndex = order.indexOf(id);
        order.splice(fromIndex, 1);
        const toIndex = outsideGroup(order, insertionIndex(order, index, current.pinned), groupId);
        order.splice(toIndex, 0, id);
        current.groupId = groupId;
        tabStrip(current.windowId);
        changed?.();
        if (fromIndex !== toIndex) {
            api.tabs.onMoved.emit(id, {windowId: current.windowId, fromIndex, toIndex});
        }
    }
    function joinGroup(ids, groupId) {
        for (const id of ids) {
            const order = tabStrip(tab(id).windowId);
            if (tab(id).groupId === groupId) continue;
            const remaining = order.filter(other => other !== id);
            const lastMember = remaining.findLastIndex(other => tab(other).groupId === groupId);
            moveTab(id, lastMember < 0 ? order.indexOf(id) : lastMember + 1, groupId);
            api.tabs.onUpdated.emit(id, {groupId}, tabSnapshot(tab(id)));
        }
    }
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
        recordPresentation(name, args);
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
            get: id => invoke("tabs.get", [id], () => tabSnapshot(tab(id))),
            query: (query = {}) => invoke("tabs.query", [query], () => [...tabs.values()].map(tabSnapshot).filter(
                current => Object.entries(query).every(([key, value]) => current[key] === value))
                .sort((a, b) => a.windowId - b.windowId || a.index - b.index)),
            create: args => invoke("tabs.create", [args], () => createTab(args)),
            group: args => invoke("tabs.group", [args], () => {
                syncGroups();
                const ids = Array.isArray(args.tabIds) ? args.tabIds : [args.tabIds];
                const windowId = tab(ids[0]).windowId;
                if (ids.some(id => tab(id).windowId !== windowId)) throw new Error("Different windows");
                if (args.groupId !== undefined && group(args.groupId).windowId !== windowId) {
                    throw new Error("Group belongs to another window");
                }
                if (ids.some(id => tab(id).pinned)) throw new Error("Cannot group pinned tabs");
                const groupId = args.groupId ?? nextGroup++;
                if (!groups.has(groupId)) {
                    const created = {id: groupId, windowId, title: "", color: "grey", collapsed: false};
                    groups.set(groupId, created);
                    api.tabGroups.onCreated.emit(clone(created));
                    api.tabGroups.onUpdated.emit(clone(created));
                }
                joinGroup(ids, groupId);
                syncGroups();
                return groupId;
            }), ungroup: ids => invoke("tabs.ungroup", [ids], () => {
                ids = Array.isArray(ids) ? ids : [ids];
                ids.forEach(id => {
                    const current = tab(id);
                    if (current.groupId < 0) return;
                    const order = tabStrip(current.windowId);
                    const index = order.indexOf(id);
                    moveTab(id, index, -1, () =>
                        api.tabs.onUpdated.emit(id, {groupId: -1}, tabSnapshot(current)));
                });
                syncGroups();
            }), update: (id, args) => invoke("tabs.update", [id, args], () => {
                const current = tab(id);
                const wasPinned = !!current.pinned;
                if (args.active) activate(current);
                Object.assign(current, args);
                if (args.pinned !== undefined && !!args.pinned !== wasPinned) {
                    const previousGroup = current.groupId;
                    moveTab(id, current.pinned ? -1 : 0, current.pinned ? -1 : current.groupId);
                    api.tabs.onUpdated.emit(id, {pinned: current.pinned,
                        ...(previousGroup !== current.groupId ? {groupId: current.groupId} : {})},
                    tabSnapshot(current));
                    syncGroups();
                }
                return tabSnapshot(current);
            }), move: (id, args) => invoke("tabs.move", [id, args], () => {
                const current = tab(id);
                if (args.windowId !== undefined && args.windowId !== current.windowId) {
                    throw new Error("Cross-window moves are not supported by this fixture");
                }
                const order = tabStrip(current.windowId).filter(other => other !== id);
                const index = insertionIndex(order, args.index, current.pinned);
                const adjacent = [tabs.get(order[index - 1]), tabs.get(order[index])];
                const groupId = adjacent.some(item => item?.groupId === current.groupId) ?
                    current.groupId : -1;
                const previousGroup = current.groupId;
                moveTab(id, args.index, groupId);
                if (previousGroup !== groupId) {
                    api.tabs.onUpdated.emit(id, {groupId}, tabSnapshot(current));
                    syncGroups();
                }
                return tabSnapshot(current);
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
                const current = group(id);
                if (args.collapsed && [...tabs.values()].some(item => item.groupId === id && item.active)) {
                    throw new Error("Cannot collapse the active tab's group without changing selection");
                }
                const updated = Object.assign(current, clone(args));
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
        args = {windowId: 1, url: "chrome://newtab/", active: true, ...args};
        const active = [...tabs.values()].find(item => item.windowId === args.windowId && item.active);
        const order = tabStrip(args.windowId);
        const index = insertionIndex(order, args.index ?? -1, args.pinned);
        const left = tabs.get(order[index - 1]);
        const right = tabs.get(order[index]);
        // Append-only tests deliberately simulate hostile Chrome inheritance.
        const append = args.index === undefined || args.index < 0 || args.index >= order.length;
        // Ordinary preparation also exercises hostile inheritance at an explicit insertion boundary.
        const hostile = append || args.url === "about:blank" || args.url.includes("ordinary-staging.html#") ||
            args.active === false && /^https?:/.test(args.url);
        const inherited = inheritActiveGroup && hostile ? active?.groupId :
            left?.groupId >= 0 && left.groupId === right?.groupId ? left.groupId : -1;
        const current = {id: nextTab++, windowId: args.windowId, url: args.url,
            active: args.active, pinned: !!args.pinned, groupId: args.pinned ? -1 : inherited ?? -1,
            ...(args.openerTabId !== undefined ? {openerTabId: args.openerTabId} : {})};
        if (args.url.includes("ordinary-staging.html#")) {
            current.title = ORDINARY_PREFIX + args.url.split("#")[1];
        }
        let target = outsideGroup(order, index, current.groupId);
        if (current.groupId >= 0 && ![left, right].some(item =>
            item?.groupId === current.groupId)) {
            target = order.findLastIndex(id => tab(id).groupId === current.groupId) + 1;
        }
        order.splice(target, 0, current.id);
        tabs.set(current.id, current);
        tabStrip(current.windowId);
        if (current.active) activate(current);
        api.tabs.onCreated.emit(tabSnapshot(current));
        return tabSnapshot(current);
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
    const result = {api, calls, tabs, windows, groups, syncGroups, tabStrip, presentationFrames,
        local, session, hooks, attachedTabs, trusted, send, errors,
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
            const restoredOrders = [...windows.keys()].map(id => [windowIds.get(id),
                tabStrip(id).map(tabId => tabIds.get(tabId))]);
            windows.clear(); tabs.clear(); groups.clear(); attachedTabs.clear(); tabOrders.clear();
            restoredOrders.forEach(([id, order]) => tabOrders.set(id, order));
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
