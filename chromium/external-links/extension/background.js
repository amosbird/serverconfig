function openInTargetBrowser(url, sourceUrl, callback = () => {}) {
    chrome.runtime.sendNativeMessage(
        "io.github.amosbird.browser_router",
        {url, sourceUrl},
        response => {
            const error = chrome.runtime.lastError;
            if (error || !response?.ok) {
                chrome.notifications.create({
                    type: "basic",
                    iconUrl: "icon.png",
                    title: "Cannot route link",
                    message: error?.message || url,
                });
            }
            callback(response);
        },
    );
}

chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
    if (!message.url) return;
    openInTargetBrowser(message.url, message.sourceUrl || sender.tab?.url || "", sendResponse);
    return true;
});
