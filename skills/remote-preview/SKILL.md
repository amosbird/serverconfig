---
name: remote-preview
description: 当用户要求预览、打开、展示或提供 Web 页面链接时使用。仅在 SSH/gremote 远程开发环境中调用 `preview expose`；本机直接提供 localhost URL。
---

# 远程 Web 预览

## 判断环境

仅在以下条件之一成立时使用 `preview expose`：

- `SSH_CONNECTION` 或 `SSH_CLIENT` 非空；
- 用户明确说明当前是环境变量被清理的 gremote 远端。

两者都不成立时视为本机，不运行 `preview`。不要根据路径或 hostname 猜测；不确定就询问用户。

## 远端流程

1. 启动 HTTP 服务，仅绑定 `127.0.0.1`，记录 PID 和日志。
2. 用 `curl http://127.0.0.1:PORT/` 验证服务。
3. 用 `preview list` 检查名称冲突。
4. 选择描述性的 lowercase DNS label，执行：

   ```bash
   preview expose PORT NAME
   ```

5. 返回：

   ```text
   http://NAME.localhost:17890
   ```

`expose` 会立即退出；路由保留 24 小时。同名操作会覆盖并续期。真正需要常驻的是 HTTP 服务。如果链接打不开，让用户通过新版 `gremote2` 重新登录以建立 SSH 转发。

## 管理

```bash
preview list
preview remove NAME [NAME...]
preview clear  # 仅在用户明确要求清空全部路由时
```

不要覆盖其他任务的同名路由；默认换名称。任务结束时只删除本任务的路由，并只用已核验的 PID 停止本任务启动的服务。

## 本机场景

不要运行 `preview`。启动服务后直接返回 `http://127.0.0.1:PORT`。

## 约束

- 只预览 HTTP/WebSocket 服务，不暴露数据库、SSH 或任意 TCP。
- 不为预览绑定 `0.0.0.0`。
- 不改用 Cloudflare Tunnel、ngrok 或 frp。
- 不提供 GC 命令；过期由 Gateway 自动处理。
