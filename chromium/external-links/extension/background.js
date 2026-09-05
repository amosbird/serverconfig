async function configureSiteLanguages() {
    const response = await fetch(chrome.runtime.getURL("site-languages.json"));
    const config = await response.json();
    const oldRules = await chrome.declarativeNetRequest.getDynamicRules();
    const addRules = Object.entries(config).map(([language, domains], index) => ({
        id: index + 1,
        priority: 1,
        action: {
            type: "modifyHeaders",
            requestHeaders: [{
                header: "accept-language",
                operation: "set",
                value: language,
            }],
        },
        condition: {
            requestDomains: domains,
            resourceTypes: ["main_frame", "sub_frame", "xmlhttprequest"],
        },
    }));
    await chrome.declarativeNetRequest.updateDynamicRules({
        removeRuleIds: oldRules.map(rule => rule.id),
        addRules,
    });
}

configureSiteLanguages();

let nativePort;
let rejectNativeRequests = [];
let scrollQueues = Promise.resolve();

function connectNativePort() {
    if (nativePort) return nativePort;
    nativePort = chrome.runtime.connectNative("io.github.amosbird.browser_router");
    nativePort.onDisconnect.addListener(() => {
        nativePort = null;
        rejectNativeRequests.splice(0).forEach(resolve => resolve({ok: false}));
    });
    return nativePort;
}

function sendNativeRequest(message) {
    return new Promise(resolve => {
        const port = connectNativePort();
        const finish = response => {
            clearTimeout(timeout);
            port.onMessage.removeListener(finish);
            rejectNativeRequests = rejectNativeRequests.filter(resolve => resolve !== finish);
            resolve(response);
        };
        const timeout = setTimeout(() => finish({ok: false}), 3000);
        rejectNativeRequests.push(finish);
        port.onMessage.addListener(finish);
        port.postMessage(message);
    });
}

function handleScrollCommand(command, tab) {
    const current = scrollQueues
        .catch(() => {})
        .then(async () => {
            const response = await sendNativeRequest({
                command: "scroll",
                direction: command,
                url: tab?.url,
            });
            if (!response?.ok) {
                chrome.notifications.create({
                    type: "basic",
                    iconUrl: "icon.png",
                    title: "Browser integration failed",
                    message: command,
                });
            }
        });
    scrollQueues = current.catch(() => {});
}

chrome.commands.onCommand.addListener((command, tab) => {
    if (command === "scrollToTop" || command === "scrollToBottom") {
        handleScrollCommand(command, tab);
    }
});

function sendNativeMessage(message, callback = () => {}) {
    chrome.runtime.sendNativeMessage(
        "io.github.amosbird.browser_router",
        message,
        response => {
            const error = chrome.runtime.lastError;
            if (error || !response?.ok) {
                chrome.notifications.create({
                    type: "basic",
                    iconUrl: "icon.png",
                    title: "Browser integration failed",
                    message: error?.message || message.path || message.url || message.direction,
                });
            }
            callback(response);
        },
    );
}

function openInTargetBrowser(url, sourceUrl, callback = () => {}) {
    sendNativeMessage({url, sourceUrl}, callback);
}

chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
    if (!message.url) return;
    openInTargetBrowser(message.url, message.sourceUrl || sender.tab?.url || "", sendResponse);
    return true;
});
