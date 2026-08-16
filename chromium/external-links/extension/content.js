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

if (typeof document !== "undefined") {
    document.addEventListener("click", event => {
        if (event.defaultPrevented || event.button !== 0) return;
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
    module.exports = {groupForHostname, isAuthHostname, shouldRouteLink};
}
