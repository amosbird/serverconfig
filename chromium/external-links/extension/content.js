function groupForHostname(hostname) {
    return Object.entries(ROUTES.groups).find(([, group]) =>
        group.sites.includes(hostname) || group.suffixes.some(suffix => hostname.endsWith(suffix))
    )?.[0];
}

function isAuthHostname(hostname) {
    return ROUTES.authSites.includes(hostname);
}

function shouldRouteLink(pageUrl, linkUrl) {
    try {
        const page = new URL(pageUrl);
        const link = new URL(linkUrl, page);
        if (!["http:", "https:"].includes(link.protocol)) return false;
        const sourceGroup = groupForHostname(page.hostname);
        const targetGroup = groupForHostname(link.hostname);
        if (isAuthHostname(page.hostname) || isAuthHostname(link.hostname)) return false;
        if (sourceGroup) return targetGroup !== sourceGroup;
        return Boolean(targetGroup);
    } catch (_) {
        return false;
    }
}

const OWNED_RECHECK_MS = 1000;
let ownedWorkspace = null;
let ownershipGeneration = -1;
let ownedCheckedAt = 0;
let ownershipRequest = null;

function refreshOwnership({force = false} = {}) {
    if (ownershipRequest) return ownershipRequest;
    if (!force && ownedWorkspace !== null && Date.now() - ownedCheckedAt < OWNED_RECHECK_MS) {
        return Promise.resolve(ownedWorkspace);
    }
    ownershipRequest = new Promise(resolve => {
        chrome.runtime.sendMessage({query: "abush_owned_tab"}, response => {
            if (response?.ready === true && typeof response.owned === "boolean" &&
                    Number.isSafeInteger(response.generation) &&
                    response.generation >= ownershipGeneration) {
                ownedWorkspace = response.owned;
                ownershipGeneration = response.generation;
                ownedCheckedAt = Date.now();
            }
            ownershipRequest = null;
            resolve(ownedWorkspace);
        });
    });
    return ownershipRequest;
}

function applyOwnershipChange(message) {
    if (message?.type !== "abush_ownership_changed" || typeof message.owned !== "boolean" ||
            !Number.isSafeInteger(message.generation) ||
            message.generation < ownershipGeneration) return false;
    ownedWorkspace = message.owned;
    ownershipGeneration = message.generation;
    ownedCheckedAt = Date.now();
    return true;
}

if (typeof document !== "undefined") {
    refreshOwnership();
    chrome.runtime.onMessage?.addListener?.(message => applyOwnershipChange(message));
    document.addEventListener("click", event => {
        if (ownedWorkspace === null || Date.now() - ownedCheckedAt >= OWNED_RECHECK_MS) {
            refreshOwnership({force: true});
        }
        if (ownedWorkspace !== false || event.defaultPrevented || event.button !== 0) return;
        const link = event.target.closest("a[href]");
        if (!link || !shouldRouteLink(location.href, link.href)) return;
        event.preventDefault();
        event.stopImmediatePropagation();
        chrome.runtime.sendMessage({url: link.href, sourceUrl: location.href});
    }, true);
}

if (typeof module !== "undefined") {
    const routes = require("./routes.js");
    global.ROUTES = routes.ROUTES;
    module.exports = {groupForHostname, isAuthHostname, shouldRouteLink, refreshOwnership,
        applyOwnershipChange};
}
