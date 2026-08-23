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

async function copyDownloadPath(id) {
    const [download] = await chrome.downloads.search({id});
    if (!download?.filename) return;
    if (!(await chrome.offscreen.hasDocument())) {
        await chrome.offscreen.createDocument({
            url: "offscreen.html",
            reasons: [chrome.offscreen.Reason.CLIPBOARD],
            justification: "Copy completed download paths",
        });
    }
    await chrome.runtime.sendMessage({
        action: "copyDownloadPath",
        text: download.filename,
    });
}

chrome.downloads.onChanged.addListener(delta => {
    if (delta.state?.current === "complete") copyDownloadPath(delta.id);
});
