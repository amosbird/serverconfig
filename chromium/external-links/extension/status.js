const params = new URLSearchParams(location.hash.slice(1));
const nonce = params.get("nonce");
const marker = params.get("marker");
const canonical = `nonce=${nonce}&marker=${marker}`;
if (location.hash.slice(1) === canonical && /^[0-9a-f]{32}$/.test(nonce) &&
        /^[ab]$/.test(marker)) {
    document.title = `__ABUSH_BIND__:${nonce}:${marker}`;
}
