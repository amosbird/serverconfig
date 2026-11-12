const nonce = location.hash.slice(1);
if (/^[0-9a-f]{32}$/.test(nonce)) {
    document.title = `__ABUSH_ORDINARY__:${nonce}`;
} else {
    document.title = "Invalid ordinary staging nonce";
}
