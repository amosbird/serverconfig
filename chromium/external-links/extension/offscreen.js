chrome.runtime.onMessage.addListener(message => {
    if (message.action === "copyDownloadPath") {
        return navigator.clipboard.writeText(message.text);
    }
});
