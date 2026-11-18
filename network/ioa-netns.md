# 方案 A：把 iOA 放进独立的网络命名空间

状态：设计稿，尚未部署。回滚点：tag `pre-ioa-netns`，系统快照 `.tmp/pre-a-20261118-000813/`。

## 目标

iOA（`iOA.bin`、`SmartGateAgent`、`iOALinux`）运行在命名空间 `ioa` 里，那里只有一条固定的上行链路、
一条默认路由、地址永不变化。宿主机的 Tailscale 出口节点、CN 直连、策略路由与 iOA 互不可见。

要删掉的东西（见 `.tmp/ioa-patch-inventory.md` 中 C 类）：`scripts/overrides/ip`、包装脚本里的等待和
PATH 改写、`network-reconfigure` 里的 iOA 共存规则（400/401/1150/1151、1100/1200 审计、owner mark、
tinyproxy 标记、源地址保护、tun0/underlay 的 MASQUERADE）、SmartDNS 的 `ipset ioa` 标记。

要保留的东西（V 类）：SmartGateAgent 的 argv[0] 和硬链接、GUI 的 root 入口保护、BPF 心跳超时、
`ioa-health`、`gui-bounce`。N 类（漫游修复、captive portal、有线 802.1X）与 iOA 无关，不动。

## 拓扑

```
 宿主机 (init netns)                              命名空间 ioa
 ───────────────────                              ─────────────
 wlan0/eth  ── 物理上行                            lo
 tailscale0 ── 出口节点                            veth-ioa-ns  169.254.170.2/30
 veth-ioa   169.254.170.1/30  ◄──────────────────►   default via 169.254.170.1
                                                     tun0 (SmartGateAgent 创建)
                                                     192.168.255.10
```

- 地址取 `169.254.170.0/30`：链路本地，不会和任何 DHCP 网段、Tailscale（100.64/10）、iOA（192.168.255/24、
  100.12/16）、公司内网（10/8）冲突。
- 命名空间内只有一条 `default via 169.254.170.1`，iOA 自己加的 tun0 路由、table 20/230、规则 1100/1200
  全部留在命名空间里，不需要 ip 拦截脚本去改写。
- 宿主机对 veth 出来的流量做 MASQUERADE 到当前物理网卡，并强制走物理路径（不进 Tailscale 出口节点）。
  宿主机漫游只改变 MASQUERADE 的出口地址，命名空间内的地址和路由不变，iOA 收不到地址事件。

## 流量

**iOA 自己的控制流量和隧道**（心跳、票据、到 sgw 网关的 TLS）：命名空间内 → veth → 宿主机 → 物理网卡。
宿主机只需一条规则：来自 veth 的包在 Tailscale 规则之前查 `main`，即策略路由 `from 169.254.170.0/30
lookup main`，再加 prohibit 兜底，防止物理路由暂时缺失时泄漏进 tailscale0。

**宿主机进程访问内网**（浏览器、ssh、curl）：iOA 的 tun0 不在宿主机，宿主机需要把内网流量送进命名空间。
只用一条机制：宿主机在命名空间里运行一个转发进程，宿主机侧只连本地端口。

- 首选：命名空间内跑 tinyproxy（或现有的 tinyproxy 改为在命名空间内运行），宿主机应用和 corkscrew 继续
  走 HTTP 代理。SmartGateAgent 看到的发起进程就是 tinyproxy，按进程授权保持现状。
- 已核实：`UnknownProcess` 发起的请求票据照样成功（当前日志 47 次全部 `success(0)`，且都拿到网关响应），
  所以即使改用路由转发，按进程授权也不会拒绝。
- 透明转发（宿主机把 100.12/16、10/8 路由到 veth）作为第二阶段，便于不支持代理的程序。

**DNS**：SmartGateAgent 在命名空间内监听 `192.168.255.10:53` 并解析 fakeip（100.12/16）。宿主机 SmartDNS 的
`ioa` 组改为发往这个地址，经 veth 路由进去（宿主机加一条 `192.168.255.10 via 169.254.170.2`）。

## GUI 和 daemon

- `iOA.bin`、`SmartGateAgent` 由 `ngnclient.service` 启动，加 `NetworkNamespacePath=/run/netns/ioa`。
- `iOALinux` 留在宿主机，不进命名空间。它和 daemon 之间只靠 `127.0.0.1:56100/56101`，这两个端口由
  `ioa-netns-forward` 从宿主机回环转进命名空间，GUI 不需要任何改动，也就不需要 sudo 入口、X11 也不受影响。
- 同一个转发器还转发 SmartGateAgent 的本地端口（12639/12649/12679/12759/12769/12101），所以 corkscrew、
  `amos.pac`、qutebrowser 和 GUI 里写死的 `PROXY 127.0.0.1:12639` 都不用改。
- 转发器先在宿主机绑定这些地址，再把自己切进 `ioa`：监听 socket 留在宿主机，之后建立的上游连接都在 `ioa`
  里。代价是 SmartGateAgent 看到的发起进程变成转发器（或 `UnknownProcess`），票据照样成功（已核实）。

## BPF 心跳超时

`ngnclient.service` 的 cgroup 不变，`BPFProgram=sock_ops` 照样生效。顺带解决之前的越界：命名空间里
SmartGateAgent 的隧道和 iOA.bin 的控制连接仍在同一 cgroup，按进程名例外 SmartGateAgent 的设计保留不变
（30s 给 iOA.bin，120s 给 SmartGateAgent）。

## 不会变的风险

- iOA 安装脚本会改 `/etc/environment`、装 `ioa-resume.service`（重启 ngnclient，必须保持不装）。
- iOA 自己的 bug（崩溃、重启即要求重新登录）仍在；隔离只减少由宿主机网络事件触发的那部分。

## 上线步骤（每一步可单独回滚）

1. **离线验证**（不碰运行中的 iOA）：在临时命名空间里跑一份独立的 SmartGateAgent？不可行——同一账号同时
   两个实例会互相踢。所以离线只验证网络骨架：建 `ioa` 命名空间、veth、宿主机规则，在里面用 curl 验证
   物理出网、不进 Tailscale、宿主机漫游后仍通。
2. **切换 daemon**（一次重新登录）：停 ngnclient → 装 netns 单元和 drop-in → 起 ngnclient。验证 tun0 在命名
   空间里、心跳和票据成功。
3. **GUI 和本地代理**：`ioa-netns-forward.service` 随 daemon 一起上线；验证 GUI 登录状态、`ioa-health`、
   corkscrew SSH、浏览器 PAC。
4. **宿主机的 TUN 流量**（DNS fakeip、直接访问 100.12/16 和 10/8）：SmartDNS 的 ioa 组改指向命名空间内的
   `192.168.255.10`，宿主机把这些网段路由到 veth；验证 git/iwiki 直连。
5. **删除 C 类补丁**：逐项删并跑测试；每删一项都在系统上验证一次。

回滚：任一步失败，跑快照里的 `rollback.sh --apply --restart-ioa`。

## 待实测的问题

2. SmartGateAgent 在命名空间里能否创建 tun0 并正常工作（`/dev/net/tun` 不受 netns 影响，应可以）。
3. 宿主机漫游时，命名空间内 iOA 是否真的收不到任何地址事件，隧道是否仍需 BPF 才能恢复。
4. `exec -a firefox` 的来历（命名空间后是否还需要）。
