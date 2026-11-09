const STATUS_PREFIX = "__ABUSH_BIND__:";

function sameRef(left, right) {
    return JSON.stringify(left) === JSON.stringify(right);
}

class WorkspaceExecutor {
    constructor(chromeApi, nativeRequest) {
        this.chrome = chromeApi;
        this.nativeRequest = nativeRequest;
        this.workspaces = new Map();
        this.windowOwners = new Map();
        this.tabOwners = new Map();
        this.operations = new Map();
    }

    key(remoteKey, connectionId) {
        return `${remoteKey}\0${connectionId}`;
    }

    async persist() {
        if (!this.chrome.storage?.session) return;
        const records = [...this.workspaces.values()].map(record => ({
            ...record,
            pages: [...record.pages.entries()],
        }));
        await this.chrome.storage.session.set({abushWorkspaces: records});
    }

    async restore() {
        if (!this.chrome.storage?.session) return;
        const {abushWorkspaces = []} = await this.chrome.storage.session.get("abushWorkspaces");
        for (const saved of abushWorkspaces) {
            try {
                const tabs = await this.chrome.tabs.query({windowId: saved.windowId});
                const status = tabs.find(tab => tab.url === this.chrome.runtime.getURL(
                    `status.html#${saved.nonce}`));
                if (!status || saved.statusTabId !== status.id) continue;
                const record = {...saved, pages: new Map(saved.pages)};
                this.install(record);
            } catch (_) {
                // A missing nonce-marked status tab means the record is not safe to restore.
            }
        }
    }

    install(record) {
        const key = this.key(record.remoteKey, record.connectionId);
        this.workspaces.set(key, record);
        this.windowOwners.set(record.windowId, key);
        for (const [, value] of record.pages) this.tabOwners.set(value.tabId, key);
    }

    async open(remoteKey, connectionId) {
        const key = this.key(remoteKey, connectionId);
        const existing = this.workspaces.get(key);
        if (existing) return existing;
        const bootstrap = await this.nativeRequest("abush_browser", "open_workspace", {
            remote_key: remoteKey, connection_id: connectionId,
        });
        const nonce = crypto.randomUUID().replaceAll("-", "");
        const statusUrl = this.chrome.runtime.getURL(`status.html#${nonce}`);
        const created = await this.chrome.windows.create({url: statusUrl, focused: false, type: "normal"});
        const statusTab = created.tabs[0];
        await this.chrome.tabs.update(statusTab.id, {pinned: true});
        const business = await this.chrome.tabs.create({windowId: created.id, url: "about:blank", active: true});
        const pageId = crypto.randomUUID();
        const registered = await this.nativeRequest("abush_browser", "register_managed_page", {
            remote_key: remoteKey, connection_id: connectionId,
            window: bootstrap.default_window, page_id: pageId,
        });
        const record = {
            remoteKey, connectionId, defaultWindow: bootstrap.default_window,
            executorFence: bootstrap.executor_fence, windowId: created.id,
            statusTabId: statusTab.id, nonce, controlState: "remote",
            pages: new Map([[registered.page.page_id, {page: registered.page, tabId: business.id}]]),
        };
        this.install(record);
        await this.persist();
        await this.nativeRequest("abush_browser", "bind_x11_window", {
            nonce, remote_key: remoteKey,
            workspace_id: bootstrap.default_window.workspace_id,
            generation: bootstrap.default_window.generation,
            control_state: "remote",
        });
        return record;
    }

    async validate(payload, command) {
        const record = this.workspaces.get(this.key(payload.remote_key, payload.connection_id));
        if (!record || !sameRef(payload.executor_fence, record.executorFence)) throw new Error("stale executor");
        if (command.command === "close_window") {
            if (!sameRef(command.window, record.defaultWindow)) throw new Error("stale window");
            return {record};
        }
        const owned = record.pages.get(command.page?.page_id);
        if (!owned || !sameRef(command.page, owned.page)) throw new Error("stale page");
        const tab = await this.chrome.tabs.get(owned.tabId);
        if (tab.windowId !== record.windowId) throw new Error("page moved outside");
        return {record, owned, tab};
    }

    async completion(payload, completion) {
        await this.nativeRequest("abush_browser", "executor_completion", {
            remote_key: payload.remote_key, connection_id: payload.connection_id,
            request_id: payload.request_id, effect_id: payload.effect_id,
            executor_fence: payload.executor_fence, completion,
        });
    }

    async execute(payload) {
        const operation = {cancelled: false, committed: false};
        this.operations.set(payload.effect_id, operation);
        let completion = "unknown";
        try {
            const {record, owned} = await this.validate(payload, payload.command);
            if (operation.cancelled) completion = "not_applied";
            else if (payload.command.command === "navigate") {
                const url = new URL(payload.command.url);
                if (!["http:", "https:"].includes(url.protocol)) throw new Error("invalid URL");
                await this.chrome.debugger.attach({tabId: owned.tabId}, "1.3");
                if (operation.cancelled) completion = "not_applied";
                else {
                    await this.chrome.debugger.sendCommand({tabId: owned.tabId}, "Page.navigate", {url: url.href});
                    operation.committed = true;
                    completion = "applied";
                }
            } else if (payload.command.command === "close_page") {
                await this.detach(owned.tabId);
                if (operation.cancelled) completion = "not_applied";
                else {
                    await this.chrome.tabs.remove(owned.tabId);
                    operation.committed = true;
                    record.pages.delete(owned.page.page_id);
                    this.tabOwners.delete(owned.tabId);
                    completion = "applied";
                }
            } else if (payload.command.command === "close_window") {
                for (const value of record.pages.values()) await this.detach(value.tabId);
                if (operation.cancelled) completion = "not_applied";
                else {
                    await this.chrome.windows.remove(record.windowId);
                    operation.committed = true;
                    this.forget(record);
                    completion = "applied";
                }
            }
        } catch (_) {
            completion = operation.committed || operation.uncertain ? "unknown" : "not_applied";
        } finally {
            this.operations.delete(payload.effect_id);
            await this.completion(payload, completion);
            await this.persist();
        }
    }

    cancel(payload) {
        const operation = this.operations.get(payload.effect_id);
        if (operation) operation.cancelled = true;
    }

    async detach(tabId) {
        try { await this.chrome.debugger.detach({tabId}); } catch (_) {}
    }

    forget(record) {
        const key = this.key(record.remoteKey, record.connectionId);
        this.workspaces.delete(key);
        this.windowOwners.delete(record.windowId);
        for (const value of record.pages.values()) this.tabOwners.delete(value.tabId);
    }

    async debuggerDetached(tabId) {
        const key = this.tabOwners.get(tabId);
        if (!key) return;
        const record = this.workspaces.get(key);
        const entry = record && [...record.pages.values()].find(value => value.tabId === tabId);
        if (!entry) return;
        entry.quarantined = true;
        for (const operation of this.operations.values()) operation.uncertain = true;
        await this.persist();
    }

    async tabMoved(tabId, moveInfo) {
        const key = this.tabOwners.get(tabId);
        if (!key) return;
        const record = this.workspaces.get(key);
        if (!record || moveInfo.windowId === record.windowId) return;
        const entry = [...record.pages.values()].find(value => value.tabId === tabId);
        await this.detach(tabId);
        record.pages.delete(entry.page.page_id);
        this.tabOwners.delete(tabId);
        await this.nativeRequest("abush_browser", "moved_outside", {
            remote_key: record.remoteKey, connection_id: record.connectionId, page: entry.page,
        });
        await this.persist();
    }

    ownsWindow(windowId) {
        return this.windowOwners.has(windowId);
    }
}

if (typeof module !== "undefined") module.exports = {WorkspaceExecutor, sameRef, STATUS_PREFIX};
