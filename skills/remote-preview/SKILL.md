---
name: remote-preview
description: >-
  当用户要求“预览”“打开”“展示”“给链接”“启动网页”“看看页面”，且可能位于 SSH/gremote
  远程开发环境时，使用 preview 暴露远端 Web 页面、原型或开发服务器。Agent 主动需要展示刚实现的网页时也适用，
  但必须先判断当前是否确实处于 SSH remote 环境；本机环境禁止运行 preview expose。
---

# 远程 Web 预览

使用 `preview` 将 SSH/gremote 远程开发机上仅监听 loopback 的 HTTP 开发服务器映射到用户本机固定入口。`preview` 只注册路由；它不是 Web server，也不是需要常驻的转发进程。

## 首先判断执行环境

执行任何 `preview expose` 前，按以下顺序判断：

1. 检查 `SSH_CONNECTION` 或 `SSH_CLIENT`。任一变量非空，都是当前 shell 位于 SSH remote 的强信号。
2. 仓库路径、工作目录位置和 `hostname` 都不是可靠信号，不得据此认定为远端。
3. 如果两个 SSH 变量都为空，默认视为本机：不要运行 `preview expose`，直接提供本机 URL。
4. 如果环境变量被 `env -i` 等方式清理，但用户明确说明当前是 gremote 远端，可以按这一明确上下文作为远端处理。
5. 如果仍不确定，先询问用户，不要自行 expose。

可使用：

```bash
if [ -n "${SSH_CONNECTION:-}" ] || [ -n "${SSH_CLIENT:-}" ]; then
    printf '%s\n' remote
else
    printf '%s\n' local
fi
```

## 决策树

```text
SSH_CONNECTION 或 SSH_CLIENT 非空？
├─ 是：按远端流程处理
└─ 否：用户是否明确说明当前是变量被清理的 gremote 远端？
   ├─ 是：按远端流程处理
   ├─ 否且确定为本机：按本机流程处理
   └─ 不确定：询问用户，不 expose
```

## 远端流程

1. 为 Web server 选择空闲端口，并且必须绑定 `127.0.0.1`。不要为了预览绑定 `0.0.0.0`。
2. 启动真正的 Web server。它需要持续运行；记录本次任务创建进程的 PID，并将日志写入已知文件，以便安全检查和清理。不要杀死未知进程。
3. 确认该端口正在监听，并从远端本机访问 HTTP 成功，例如 `curl http://127.0.0.1:PORT/`。确认成功前不要 expose。
4. 先运行 `preview list` 检查已有路由。选择稳定、描述性的名称，避免覆盖其他 Agent 或其他任务的路由。
5. 名称必须是最长 63 字节的 lowercase DNS label，匹配：

   ```text
   ^[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?$
   ```

6. 注册路由：

   ```bash
   preview expose PORT NAME [NAME...]
   ```

   一次可以原子注册最多 32 个名称。命令会立即退出，不要后台启动它，也不要尝试保持一个 `preview expose` 进程。同名 `expose` 会原子覆盖并续期 24 小时；只有用户明确要求覆盖，或确认该名称属于当前任务时才这样做。发现名称冲突时默认换一个新名称。
7. 返回给用户：

   ```text
   http://NAME.localhost:17890
   ```

8. 明确说明本机需要由新版 `gremote2` 登录会话建立 SSH `-L` 转发。如果 URL 无法打开，让用户用新版 `gremote2` 重新登录；不要改用公网隧道。

示例模板：

```bash
# Web server 示例；实际命令按项目调整。
python3 -m http.server PORT --bind 127.0.0.1 >preview-server.log 2>&1 &
server_pid=$!
printf '%s\n' "$server_pid" >preview-server.pid

curl --fail --show-error http://127.0.0.1:PORT/
preview list
preview expose PORT NAME
```

这里需要常驻的是 Web server，而不是 `preview expose`。若 Agent 启动 server，必须保留 PID 和日志位置并告知用户；清理时只操作记录的 PID，并应先核对该 PID 仍是本任务的进程。

## 本机流程

本机环境不要运行任何 `preview` 命令。正常启动项目 server 后，直接给出：

```text
http://127.0.0.1:PORT
```

也可以给出项目自身生成的本地 URL。如果 Agent 所在环境与用户浏览器处于不同网络命名空间，不要猜测地址或声称 URL 一定可访问；说明该限制，并让用户在可访问浏览器的环境中启动或转发服务。

## 路由管理与清理

- `preview list`：列出路由；不探测上游健康。
- `preview remove NAME...`：删除指定路由，并关闭这些路由当前 generation 的活跃连接。
- `preview clear`：删除全部路由。仅当用户明确要求清除全部路由时使用；不要影响其他 Agent 或任务。
- 再次执行 `preview expose PORT NAME`：覆盖同名路由并续期 24 小时。
- Web server 停止后，路由仍保留到期，访问会返回 `502`。
- 不要提供、建议或尝试任何 GC 命令。

任务结束时，根据用户意图运行 `preview remove NAME`。不要因为当前任务结束就运行 `preview clear`。如果用户仍需访问页面，则保留路由和 Web server，并告知其 PID、日志及 24 小时 TTL。

## 安全边界

- 只 expose HTTP/WebSocket 开发服务，不要 expose 数据库、SSH、任意 TCP 或其他非 HTTP 服务。
- 上游只绑定 `127.0.0.1`；禁止为了 preview 使用 `0.0.0.0`。
- 禁止改用或建议公网隧道，包括 Cloudflare Tunnel、ngrok、frp 等。
- expose 前先 `preview list`。不得覆盖不属于当前任务的同名路由，除非用户明确要求。
- 不要抢占端口、停止未知 listener 或杀死未知进程。
- 进程清理只使用本任务记录并核验过的 PID。
