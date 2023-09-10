var direct = "DIRECT";
var http_proxy = "PROXY 127.0.0.1:12639";

var gfwed_list = ["oa.com", "woa.com"];
var cn_list = ["qq.com", "taobao.com", "jd.com"];

var gfwed = {};
var cned = {};
for (var i = 0; i < gfwed_list.length; i += 1) {
    gfwed[gfwed_list[i]] = true;
}

for (var i = 0; i < cn_list.length; i += 1) {
    cned[cn_list[i]] = true;
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
    if (cned[host2domain(host)])
        return "PROXY 127.0.0.1:8888";

    if (isInNet(host, "11.0.0.0", "255.0.0.0") || isInNet(host, "9.0.0.0", "255.0.0.0") || isInNet(host, "30.0.0.0", "255.0.0.0")) {
        return http_proxy;
    }
    // return http_proxy;
    return gfwed[host2domain(host)] ? http_proxy : direct;
}
