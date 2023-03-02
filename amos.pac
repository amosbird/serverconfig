var direct = "DIRECT";
var http_proxy = "PROXY 127.0.0.1:12639";

var gfwed_list = ["oa.com", "woa.com", "oa.tencent.com"];

var gfwed = {};
for (var i = 0; i < gfwed_list.length; i += 1) {
    gfwed[gfwed_list[i]] = true;
}

function host2domain(host) {
    var dotpos = host.lastIndexOf(".");
    if (dotpos === -1) return host;
    // Find the second last dot
    dotpos = host.lastIndexOf(".", dotpos - 1);
    if (dotpos === -1) return host;
    return host.substring(dotpos + 1);
}

function FindProxyForURL(url, host) {
    // return http_proxy;
    return gfwed[host2domain(host)] ? http_proxy : direct;
}
