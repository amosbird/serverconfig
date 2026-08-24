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
                    message: error?.message || message.path || message.url,
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
