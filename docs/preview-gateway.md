# 远程预览网关设计

> 状态：设计方案，尚未实现。

## 1. 目标与范围

开发者从本机通过 `gremote2` 登录远程开发机，在远端启动 Web 开发服务器后执行：

```bash
preview expose 4173 data-ingress
```

本机随后访问：

```text
http://data-ingress.localhost:17890
```

系统需要满足：

- 远端网关仅监听 `127.0.0.1:17890`，不暴露到远端局域网或公网。
- 本机仅监听 `127.0.0.1:17890`，通过固定 SSH 本地转发连接远端网关。
- 使用 `Host` 路由，一个网关可同时暴露多个名字和端口。
- 支持普通 HTTP、流式 HTTP、WebSocket 和开发服务器 HMR。
- `preview expose` 是前台租约进程；其 Unix socket EOF 表示租约结束。
- 租约结束后立即删除路由，并主动关闭该路由的活跃 WebSocket。
- 同名路由不得覆盖，注册时直接拒绝。
- 只依赖 Python 3 标准库和 OpenSSH。

### 1.1 非目标

本方案不提供：

- VPS、公网入口、Cloudflare、TLS 或跨互联网分享。
- 用户认证、访问授权或多用户隔离代理。
- Caddy、nginx、systemd 服务或需要 `sudo` 的安装步骤。
- 任意 TCP 转发、TLS passthrough、HTTP/2 或 HTTP/3。
- 远端代码同步。`gremote2` 不执行 `git pull`。
- 对恶意上游应用的沙箱隔离。

## 2. 总体架构

```text
Browser
  │ http://data-ingress.localhost:17890
  ▼
127.0.0.1:17890（本机 OpenSSH 专用 ControlMaster）
  │ -L 127.0.0.1:17890:127.0.0.1:17890
  ▼
127.0.0.1:17890（远端 preview gateway）
  │ Host: data-ingress.localhost:17890
  ▼
127.0.0.1:4173（远端开发服务器）

preview expose 4173 data-ingress
  │ Unix domain socket：注册 + 租约
  ▼
preview gateway
```

远端 `$HOME/scripts` 是 `$HOME/git/serverconfig/scripts` 的符号链接。实现文件提交到
`serverconfig` 后会通过该链接生效，但 `gremote2` 只调用现有远端文件，不负责更新仓库。

## 3. 端口与命名

### 3.1 固定端口

网关两端都使用 `17890`：

- 远端：gateway 绑定 `127.0.0.1:17890`。
- 本机：SSH 绑定 `127.0.0.1:17890`。
- 转发：`-L 127.0.0.1:17890:127.0.0.1:17890`。

固定端口使 URL 稳定，也决定了本机同一时刻只能将预览入口关联到一台开发机。端口不做
自动递增或随机回退；无法证明现有转发属于同一 endpoint 时，预览功能必须失败关闭。

选择 `17890` 是为了避开现有 `gremote2` 中的固定转发端口，同时保留可辨识的非特权端口。

### 3.2 路由名

`name` 必须匹配：

```text
[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?
```

禁止点、下划线、空字符串和大写字母。访问域名固定为 `<name>.localhost`。注册请求中的
上游地址不开放配置，只允许 `127.0.0.1:<port>`，其中端口范围为 `1..65535`。

`preview expose PORT NAME [NAME ...]` 可让一个租约注册多个名字。注册为原子操作：任一名字
非法或已占用时全部拒绝。不同 `preview expose` 进程可并发注册不同名字。

## 4. 进程与文件结构

建议新增：

```text
scripts/preview                 # 用户入口与 gateway ensure/daemon 子命令
scripts/preview_gateway.py      # asyncio HTTP/WebSocket 网关和控制协议
scripts/gremote2.sh             # 接入远端 ensure 与本地专用 ControlMaster
scripts/gremote-preview         # 可选：本地转发编排，降低 gremote2 复杂度
docs/preview-gateway.md         # 本设计
```

远端运行时目录为 `$XDG_RUNTIME_DIR/preview-gateway`。启动前必须验证：

- `XDG_RUNTIME_DIR` 存在、是当前用户所有的目录，且不可被 group/other 写入。
- 子目录由当前用户创建，权限为 `0700`，且不是符号链接。
- socket、lock、pid 等文件均在该目录内，进程使用 `umask 077`。

运行时文件：

```text
$XDG_RUNTIME_DIR/preview-gateway/control.sock
$XDG_RUNTIME_DIR/preview-gateway/gateway.lock
$XDG_RUNTIME_DIR/preview-gateway/gateway.pid
```

安全检查失败时不回退到 `/tmp` 或其他共享目录。

## 5. gateway 启动与单实例

每次 `preview` 命令先执行 ensure；`gremote2` 每次登录也在远端执行 ensure。ensure 必须幂等：

1. 对 `gateway.lock` 使用 Python `fcntl.flock(LOCK_EX)` 串行化检查和启动。
2. 连接 `control.sock` 并执行 `PING`。成功则立即返回。
3. 无健康实例时清理经过类型、所有者检查的陈旧 socket 和 pid 文件。
4. 通过 double-fork 启动用户级 daemon：第一次 fork 后 `setsid`，再 fork 一次。
5. daemon 设置 `umask 077`、工作目录 `/`，并将标准输入输出错误重定向到
   `/dev/null`；诊断写入受保护的运行时日志或按需状态文件。
6. daemon 完成 control socket 和 TCP listener 绑定后，通过启动 pipe 向 ensure 父进程
   返回 `READY`；绑定失败则返回明确错误。
7. daemon 继承并在整个生命周期持有 flock，确保只有一个实例。

`gateway.pid` 只用于诊断，不作为互斥或存活判断依据。ensure 应有短超时，不能无限等待。
daemon 不因当前没有路由而退出。

## 6. 控制协议与租约

### 6.1 传输与编码

控制面使用 `AF_UNIX/SOCK_STREAM`。每条消息是一行 UTF-8 JSON，单行上限建议为 8 KiB；
超限、未知字段类型、未知版本或非法 JSON 都关闭连接。协议版本首期固定为 `1`。

请求示例：

```json
{"version":1,"op":"register","names":["data-ingress"],"port":4173}
```

成功响应：

```json
{"ok":true,"lease_id":"<random-128-bit-hex>"}
```

失败响应：

```json
{"ok":false,"code":"name_in_use","message":"data-ingress is already exposed"}
```

健康检查使用独立短连接：

```json
{"version":1,"op":"ping"}
{"ok":true,"version":1}
```

### 6.2 注册规则

gateway 在发出成功响应前完成所有校验和原子占位：

- 所有名字合法且请求内无重复。
- 所有名字当前均未注册。
- port 合法；gateway 始终连接远端 loopback，不接受任意 host。
- 每个控制连接最多持有一个租约，名字数量设置合理上限，例如 32。

不提前探测上游端口。开发服务器暂未监听时，注册仍可成功，请求到达时返回 `502`；这样可
避免检查与实际连接之间的竞态，也允许先建立稳定 URL 再重启开发服务器。

### 6.3 EOF 租约

成功后，`preview expose` 不退出，也不轮询；它保持控制 socket 打开并等待：

- 用户按 `Ctrl-C`、进程收到终止信号或异常退出时，socket 关闭。
- SSH 会话断开导致进程和 socket 关闭时，gateway 观察到 EOF。
- gateway 收到 EOF 或协议错误后，先从路由表原子删除全部名字，再取消该租约的连接任务。
- 已升级的 WebSocket 立即关闭；可直接关闭 TCP，不要求完成 WebSocket close handshake。
- 尚未升级或正在处理的 HTTP 连接也可中止，不能在路由删除后创建新的上游连接。

gateway 异常退出时，租约连接得到 EOF，`preview expose` 输出错误并非零退出，不自动切换
到其他方案。用户重新运行命令即可重新注册。

## 7. HTTP/1.1 与 WebSocket 语义

### 7.1 受限 HTTP/1.1 解析

使用 `asyncio.start_server` 实现受限 HTTP/1.1 反向代理。只解析路由和消息边界所需内容：

- 接受 HTTP/1.0 和 HTTP/1.1 的 origin-form request-target。
- 请求行和单个 header 行建议限制为 8 KiB，全部 header 建议限制为 64 KiB。
- 拒绝 obs-fold、非法 header 名、NUL、重复或冲突的 `Content-Length`。
- 拒绝同时出现 `Transfer-Encoding` 与 `Content-Length` 的请求。
- `Transfer-Encoding` 仅接受最终编码为 `chunked` 的合法形式。
- 设置 header 读取、上游连接和空闲连接超时，限制并发连接数。

网关不是通用容错代理。解析不明确时返回 `400` 并关闭连接，资源超限返回 `431` 或
`503`，上游连接失败返回 `502`。

### 7.2 Host 路由

HTTP/1.1 必须恰有一个有效 `Host`。路由时：

1. 去除可选端口，并按 ASCII 小写处理 host。
2. 只接受精确的 `<name>.localhost`，不接受后缀近似匹配。
3. 查询当前路由表，未注册返回 `404`。
4. 连接 `127.0.0.1:<registered-port>`。

代理保留原始 request-target、`Host` 和端到端 header，不注入认证信息，不改写响应正文、
重定向或 Cookie。这样 HMR 客户端可继续看到浏览器使用的稳定 host 和端口。

每个客户端 keep-alive 连接固定到首个请求解析出的路由。后续请求若更换 `Host`，返回
`421 Misdirected Request` 并关闭连接，防止跨路由复用已建立的上游连接。

### 7.3 请求与响应转发

首期不缓存完整 body。网关在验证首部后，将首部和 body 按 HTTP 消息边界双向流式传输，
提供 backpressure。支持固定长度 body、chunked body、无 body 响应及 EOF 终止的 HTTP/1.0
响应。`HEAD`、`1xx`、`204` 和 `304` 按 RFC 的无响应 body 语义处理。

代理不自动重试请求。上游在部分响应后断开时，直接关闭客户端连接，避免重复有副作用的
请求。

### 7.4 WebSocket 与 HMR

WebSocket 仍由 HTTP Upgrade 建立。网关验证普通 HTTP 首部并选择路由，将握手原样发送给
上游。上游返回 `101 Switching Protocols` 后：

- 不解析或重写 WebSocket frame。
- 使用两个 asyncio copy task 双向透明转发字节并传播 backpressure。
- 任一方向 EOF、错误、超时或租约结束时取消另一方向并关闭两端 TCP。
- 将连接 task 记录到对应 lease，确保 EOF 租约可立即关闭活跃 WebSocket。

HMR 使用浏览器当前 hostname 和 port 建立 WebSocket 时无需额外配置；若框架硬编码其他
端口，需在项目侧将 HMR client port 配为 `17890`，这不由 gateway 猜测或改写 JavaScript。

## 8. 安全边界

安全性来自 loopback、SSH 和同用户 Unix socket，而非应用层认证：

- 远端和本机都只绑定 `127.0.0.1`，明确禁用 wildcard bind。
- SSH 提供本机到远端的加密和主机认证。
- control socket 所在目录为 `0700`；gateway 还应通过 `SO_PEERCRED` 验证注册者 UID 与
  gateway UID 一致。
- 上游固定为远端 `127.0.0.1`，不能借控制协议访问内网地址或 Unix socket。
- 严格校验 Host，不能用 Host 请求任意端口。
- 名字冲突拒绝，避免后启动进程劫持已有预览。
- daemon 不提权、不调用 shell 解释控制消息、不读取项目文件。

本机其他同 UID 进程仍可访问 `*.localhost:17890`；远端同 UID 进程也能连接 gateway 或
control socket。这是明确的信任边界。需要跨用户或不可信本地进程隔离时，本方案不适用。

## 9. `gremote2` 集成

### 9.1 为什么不用每个交互 SSH 的 `-L`

`gremote2` 可同时打开多个 session。本机固定端口只能被一个 listener 绑定；若把同一个
`-L` 加到每次交互 SSH，第二次登录会遇到端口冲突，而且首个窗口关闭会意外移除预览。

因此预览转发使用独立、稳定、endpoint-specific 的专用 ControlMaster，生命周期不依附
任何 kitty 窗口。现有交互 SSH 和其随机 `SSH_MASTER_CTRL` 保持不变。

### 9.2 endpoint 标识和本地状态

endpoint 至少由 SSH 最终用户、主机和端口组成，必须包含 `ssh -G` 展开后的目标，避免
别名不同但目标相同或同名别名指向不同主机。对规范 endpoint 做 SHA-256，得到稳定路径：

```text
$XDG_RUNTIME_DIR/gremote-preview/<endpoint-hash>.ctl
$XDG_RUNTIME_DIR/gremote-preview/active.json
$XDG_RUNTIME_DIR/gremote-preview/setup.lock
```

本地目录同样要求当前用户所有、`0700` 且非符号链接。`active.json` 记录当前固定端口对应的
规范 endpoint 和 control socket；它只是可验证状态，不单独作为事实来源。

### 9.3 登录时序

每次 `gremote2 ENDPOINT` 在启动 kitty 交互会话前执行：

1. 在本地 `setup.lock` 上加锁，串行化多 session 并发登录。
2. 计算规范 endpoint，并检查其专用 ControlMaster：
   `ssh -S <ctl> -O check ENDPOINT`。
3. 不存在时启动无转发的后台 master，例如
   `ssh -MNf -o ControlMaster=yes -o ControlPersist=yes -S <ctl> ENDPOINT`。
4. 通过该 master 在远端执行 `$HOME/scripts/preview gateway ensure`，并检查退出状态。
5. 验证本地 `17890` 当前状态：
   - 已由同 endpoint 的健康 master 转发：直接复用。
   - 未监听且没有有效 active 记录：继续创建。
   - 属于不同 endpoint、状态不一致或无法证明所有权：预览失败关闭。
6. 仅在 ensure 成功后执行：

   ```bash
   ssh -S <ctl> -O forward \
       -L 127.0.0.1:17890:127.0.0.1:17890 ENDPOINT
   ```

7. 再次验证 master 和本地 listener，原子写入 `active.json`，释放锁。
8. 按原有逻辑启动交互登录。

必须为 forward 启用等价于 `ExitOnForwardFailure=yes` 的确认语义，不能在端口绑定失败时标记
成功。相同 endpoint 的后续登录复用 master 和已有 forward，不重复申请。

### 9.4 失败策略

预览初始化失败时输出一条包含 endpoint 和原因的明确警告：

- 默认：跳过预览，继续正常 `gremote2` 登录。
- `GREMOTE_REQUIRE_PREVIEW=1`：在启动交互会话前非零退出。

不同 endpoint 已占用本机固定端口时，不关闭旧 master、不取消旧 forward、不抢占端口。
这是 fail-close preview。用户应显式结束旧 endpoint 的专用 master 后再切换。

远端 ensure、SSH 连接、状态校验或本地写状态失败均不得猜测成功。预览失败不能改变现有
GPG、kitty、DBus、SSH agent 或其他转发逻辑。

## 10. 错误模型

| 场景 | 行为 |
|---|---|
| gateway 已健康 | ensure 成功且不重启 |
| runtime dir 不安全 | ensure 失败，不回退共享目录 |
| `17890` 已被远端其他进程占用 | ensure 失败并报告 bind 错误 |
| 名字非法或重复 | 注册整体失败，`preview expose` 非零退出 |
| 名字已存在 | 返回 `name_in_use`，不影响原租约 |
| 上游未监听或拒绝连接 | 对该 HTTP 请求返回 `502` |
| 未知 Host 或租约已结束 | 返回 `404` |
| 请求语法不明确 | 返回 `400` 并关闭连接 |
| header 超限 | 返回 `431` 并关闭连接 |
| 网关过载 | 返回 `503` 或拒绝新连接，不杀死 daemon |
| expose socket EOF | 立即删路由并关闭该租约的 WebSocket |
| gateway 退出 | expose 报错退出；现有 HTTP/WS 连接关闭 |
| 本机端口属于同 endpoint | 复用已有专用 master |
| 本机端口属于不同 endpoint | 预览失败；默认登录继续 |
| 严格模式下预览失败 | `gremote2` 非零退出，不打开交互会话 |

## 11. 测试矩阵

仓库没有自动测试框架；实现阶段应提供可重复的手工或 Python 临时测试脚本，至少覆盖：

| 类别 | 用例 | 期望 |
|---|---|---|
| ensure | 并发运行 20 次 | 仅一个 daemon，全部得到一致结果 |
| ensure | 陈旧 socket/pid | 安全清理并启动，不误删非 socket 文件 |
| 安全 | runtime dir owner/mode 错误或 symlink | 拒绝启动 |
| bind | 远端 `17890` 被占用 | ensure 明确失败 |
| 注册 | 单名字、多名字 | 全部可按 Host 访问 |
| 注册 | 请求内重名、非法名、已占用名 | 原子拒绝，无部分路由 |
| 生命周期 | `Ctrl-C` expose | 路由立即 `404` |
| 生命周期 | `kill -9` expose | Unix EOF 后路由立即删除 |
| HTTP | GET、POST、HEAD、chunked、流式响应 | body 和状态正确、无整包缓存 |
| HTTP | keep-alive 同 Host | 多请求正常复用 |
| HTTP | keep-alive 更换 Host | `421` 并关闭 |
| HTTP | 双 Content-Length、TE+CL、obs-fold | `400` 并关闭 |
| HTTP | header 超限 | `431` |
| 上游 | 未监听、响应中途断开 | `502` 或关闭，不重试 |
| WebSocket | echo、双向大消息、ping/pong | 字节透明、backpressure 正常 |
| HMR | Vite 等开发服务器热更新 | 页面收到更新，无手工重连 |
| 租约 | 活跃 WebSocket 时结束 expose | WebSocket 立即断开 |
| SSH | 同 endpoint 两个 gremote2 session | 共享一个 forward，互不影响 |
| SSH | 关闭任一交互 session | preview forward 仍存在 |
| SSH | 不同 endpoint 请求同端口 | 不抢占；默认仍可登录 |
| SSH | `GREMOTE_REQUIRE_PREVIEW=1` 且冲突 | 登录前失败 |
| SSH | 两次并发首次登录 | 本地锁避免创建重复 master/forward |
| 恢复 | master 异常退出后再次登录 | 清理可验证陈旧状态并重建 |

测试期间使用临时名字和端口，不依赖公网 DNS；`localhost` 子域应由现代浏览器和 resolver
解析到 loopback。若目标环境不支持该解析，应将环境不兼容明确报出，不修改 `/etc/hosts`。

## 12. 分阶段实施

### 阶段 A：远端控制面与生命周期

- 实现安全 runtime dir、flock、double-fork 和 ensure 握手。
- 实现 JSON line 控制协议、同名拒绝、多名字原子注册和 EOF 租约。
- 先用状态查询或最小 HTTP handler 验证并发注册与清理。

### 阶段 B：受限 HTTP/1.1 代理

- 实现严格请求解析、Host 路由、上游连接和流式 body。
- 加入大小、并发、连接和超时限制。
- 覆盖 keep-alive、chunked、错误响应和不重试语义。

### 阶段 C：WebSocket 与 HMR

- 实现 Upgrade 握手后透明双向 copy。
- 将连接 task 归属 lease，并验证租约结束立即断开。
- 用实际开发服务器验证 HMR URL 和重连行为。

### 阶段 D：`gremote2` 专用 ControlMaster

- 实现 endpoint 规范化、安全本地状态目录和 setup flock。
- 严格按“远端 ensure，再 `ssh -O forward`”接入。
- 实现同 endpoint 复用、不同 endpoint fail-close 和严格模式。
- 确认现有交互 SSH 转发、GPG、kitty 和 tmux 流程无回归。

### 阶段 E：稳健性收尾

- 补齐协议畸形输入、资源上限、daemon/master 异常恢复测试。
- 文档化诊断命令、显式切换 endpoint 的停止流程和已知限制。
- 只在完整矩阵通过后将功能视为可用；本文本身不表示功能已经实现。

## 13. 验收命令

以下命令描述实现后的验收流程，不代表当前仓库已经提供这些能力。

远端启动一个仅监听 loopback 的测试服务：

```bash
python3 -m http.server 4173 --bind 127.0.0.1
```

另一个远端终端建立租约：

```bash
preview expose 4173 data-ingress
```

本机登录并建立专用预览转发：

```bash
gremote2 user@dev-host
```

本机验证 Host 路由：

```bash
curl --fail --show-error \
    http://data-ingress.localhost:17890/
```

验证未知路由：

```bash
curl --silent --output /dev/null --write-out '%{http_code}\n' \
    http://not-exposed.localhost:17890/
# 期望：404
```

验证同名拒绝：

```bash
preview expose 4174 data-ingress
# 期望：非零退出，并报告 name_in_use
```

结束第一个 `preview expose` 后再次访问：

```bash
curl --silent --output /dev/null --write-out '%{http_code}\n' \
    http://data-ingress.localhost:17890/
# 期望：404
```

多名字验收：

```bash
preview expose 4173 data-ingress data-ingress-alt
curl --fail http://data-ingress.localhost:17890/
curl --fail http://data-ingress-alt.localhost:17890/
```

严格模式验收：先让另一个 endpoint 占用预览端口，再运行：

```bash
GREMOTE_REQUIRE_PREVIEW=1 gremote2 user@another-dev-host
# 期望：非零退出，不打开交互会话，也不影响原 endpoint
```

WebSocket/HMR 验收应使用实际项目开发服务器：浏览器打开稳定 URL，修改源文件，确认页面
热更新；随后终止 `preview expose`，确认浏览器中的 WebSocket 立即断开。
