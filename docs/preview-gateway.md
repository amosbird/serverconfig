# 远程预览网关

`preview` 将远端仅监听 loopback 的 Web 开发服务器通过现有 `gremote2` SSH 会话暴露到
本机固定地址。实现只依赖 Python 3 标准库和 OpenSSH。

## 架构

```text
Browser http://NAME.localhost:17890
  -> 本机 127.0.0.1:17890
  -> gremote2 最终 SSH master 的 -L
  -> 远端 $XDG_RUNTIME_DIR/preview-gateway/http.sock
  -> preview gateway
  -> 远端 127.0.0.1:PORT
```

远端 Gateway 的数据面 `http.sock` 和控制面 `control.sock` 都是 Unix domain socket，远端
没有 TCP listener。`gremote2` 不创建额外 ControlMaster；预览转发属于该次登录最终建立的
SSH master。当前占有本机 `17890` 的那个 `gremote2` 主 session 关闭后转发随之消失，后续
登录可重新建立。

## 使用

远端启动开发服务器和前台租约：

```bash
python3 -m http.server 4173 --bind 127.0.0.1
preview expose 4173 data-ingress data-ingress-alt
```

本机通过 `gremote2` 登录后访问：

```bash
curl http://data-ingress.localhost:17890/
```

名称必须是小写 DNS label，只允许字母、数字和中划线，最长 63 字节。一个租约可原子注册
最多 32 个名称。同名注册会失败；`Ctrl-C`、进程退出或控制 socket EOF 会立即删除全部
路由并关闭该租约的活跃 HTTP/WebSocket 连接。

管理命令：

```bash
preview gateway ensure
preview gateway ensure --quiet --print-http-socket
preview gateway status
preview gateway stop
preview list
```

`ensure` 使用 `flock` 和 readiness pipe 并发安全地 double-fork daemon。默认 runtime base
是 `$XDG_RUNTIME_DIR`；未设置时只尝试 `/run/user/$(id -u)`。base 必须已存在、归当前 uid
所有且不可被 group/other 写入。`preview-gateway` 子目录为 `0700`，socket 为 `0600`，连接
还通过 Linux `SO_PEERCRED` 验证同 uid。安全校验失败不会回退到 `/tmp`。日志写入
`$XDG_STATE_HOME/preview-gateway/gateway.log`，默认位置为
`~/.local/state/preview-gateway/gateway.log`。

## HTTP 行为

Gateway 接受 HTTP/1.0 和 HTTP/1.1 origin-form 请求，并按精确
`Host: <name>.localhost[:17890]` 路由。它支持固定 `Content-Length`、chunked 流式 body、
EOF response 和 WebSocket Upgrade 后的 raw 双向 relay。未知路由返回 `404`，上游连接失败
返回 `502`。它拒绝冲突 framing、obs-fold、absolute-form、`CONNECT` 和 h2c。

代理保留 `Host` 和 `Origin`，删除逐跳 header，并重建 `X-Forwarded-For`、
`X-Forwarded-Proto` 和 `X-Forwarded-Host`。普通 HTTP 当前每个客户端请求使用一个上游连接
并返回 `Connection: close`；WebSocket/HMR 保持长连接和 backpressure。

## `gremote2` 集成与失败策略

`gremote2` 在 prelogin 后执行远端：

```bash
$HOME/scripts/preview gateway ensure --quiet --print-http-socket
```

输出必须是无换行外的安全绝对路径。若本机 `127.0.0.1:17890` 空闲，最终现有 SSH 命令会
增加：

```text
-o ExitOnForwardFailure=yes -L 127.0.0.1:17890:<remote-http.sock>
```

如果远端 ensure 失败、socket 路径非法或本机端口已监听，默认打印明确警告并继续普通登录，
不抢占或关闭已有 listener。设置 `GREMOTE_REQUIRE_PREVIEW=1` 后，这些情况会在启动 kitty
前非零退出。由于不维护独立转发进程，同时只能有一个 SSH session 拥有本机固定端口；其他
登录仍可正常进行但不会提供预览。

## 安全与限制

安全边界是本机 loopback、SSH、远端 Unix socket 权限和同 uid 信任。上游固定为
`127.0.0.1:PORT`，控制协议不能指定任意地址。该功能不提供认证、TLS、公网分享、多用户
隔离、任意 TCP、HTTP/2/3 或恶意上游沙箱。`*.localhost` 必须由本机 resolver/browser
解析为 loopback；实现不会修改 `/etc/hosts`。
