#!/usr/bin/env node

const assert = require("assert");
const {WorkspaceExecutor} = require("../chromium/external-links/extension/workspace.js");

const windowRef = {workspace_id: "workspace", window_id: "window", generation: 1};
const pageRef = {window: windowRef, page_id: "page", generation: 1};
const fence = {executor_id: "executor", epoch: 1};

function fixture(tabWindow = 10) {
    const calls = [];
    const chrome = {
        tabs: {
            get: async id => ({id, windowId: tabWindow}),
            update: async (id, properties) => calls.push(["update", id, properties]),
            remove: async id => calls.push(["remove", id]),
        },
        windows: {remove: async id => calls.push(["removeWindow", id])},
        debugger: {
            attach: async target => calls.push(["attach", target]),
            detach: async target => calls.push(["detach", target]),
            sendCommand: async (...args) => calls.push(["command", ...args]),
        },
    };
    const native = async (service, operation, payload) => calls.push([operation, payload]);
    const executor = new WorkspaceExecutor(chrome, native);
    const record = {
        remoteKey: "r_12345678", connectionId: "c_12345678",
        defaultWindow: windowRef, executorFence: fence, windowId: 10,
        statusTabId: 19, nonce: "0123456789abcdef0123456789abcdef", controlState: "remote",
        pages: new Map([["page", {page: pageRef, tabId: 20}]]),
    };
    executor.install(record);
    return {executor, calls, record};
}

function payload(command, overrides = {}) {
    return {
        remote_key: "r_12345678", connection_id: "c_12345678",
        request_id: "request", effect_id: "effect", executor_fence: fence,
        command, ...overrides,
    };
}

(async () => {
    {
        const {executor, calls} = fixture();
        await executor.execute(payload({command: "navigate", page: pageRef,
            url: "https://example.test/"}));
        assert(calls.some(call => call[0] === "command"));
        assert.equal(calls.find(call => call[0] === "executor_completion")[1].completion, "applied");
        assert(!calls.some(call => call[0].toLowerCase().includes("focus")));
    }
    {
        const {executor, calls} = fixture();
        await executor.execute(payload({command: "navigate", page: {...pageRef, generation: 2},
            url: "https://example.test/"}));
        assert.equal(calls.find(call => call[0] === "executor_completion")[1].completion,
            "not_applied");
        assert(!calls.some(call => call[0] === "attach"));
    }
    {
        const {executor, calls, record} = fixture();
        assert.equal(executor.tabOwners.has(999), false, "ordinary tab must not be adopted");
        await executor.tabMoved(20, {windowId: 99});
        assert(calls.some(call => call[0] === "detach"));
        assert(calls.some(call => call[0] === "moved_outside"));
        assert.equal(record.pages.size, 0);
    }
    {
        const {executor, calls} = fixture(99);
        await executor.execute(payload({command: "close_page", page: pageRef}));
        assert(!calls.some(call => call[0] === "remove"));
        assert.equal(calls.find(call => call[0] === "executor_completion")[1].completion,
            "not_applied");
    }
    {
        const {executor, calls, record} = fixture();
        const reopened = await executor.open(record.remoteKey, record.connectionId);
        assert.strictEqual(reopened, record);
        assert.deepEqual(calls.map(call => call[0]), ["update", "bind_x11_window", "update"]);
        assert.deepEqual(calls[0], ["update", 19, {active: true}]);
        assert.deepEqual(calls[1][1], {
            nonce: "0123456789abcdef0123456789abcdef",
            remote_key: "r_12345678",
            workspace_id: "workspace",
            generation: 1,
            control_state: "remote",
        });
        assert.deepEqual(calls[2], ["update", 20, {active: true}]);
    }
    console.log("workspace executor tests passed");
})().catch(error => { console.error(error); process.exitCode = 1; });
