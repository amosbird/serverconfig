# 方案 A：把 iOA 放进独立的网络命名空间

状态：实现完成，待切换（`.tmp/cutover-ioa-netns.sh`，需扫码一次）。
回滚点：tag `pre-ioa-netns`，系统快照 `.tmp/pre-a-20261118-000813/`。

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

**iOA 自己的流量**（心跳、票据、到 sgw 网关的 TLS）：命名空间内 → veth → 宿主机 → 物理网卡，宿主机做
MASQUERADE。宿主机规则（`network/ioa-netns`）：600 `iif veth-ioa lookup 19`（办公室 802.1X 有线授权时
`network-reconfigure` 往 table 19 放默认路由，否则清空，等于跳过），601 `iif veth-ioa lookup main` 和
`to 169.254.170.0/30 lookup main`（回程），602 `iif veth-ioa prohibit`（物理路由缺失时立即失败，绝不掉进
Tailscale）。优先级避开 `network-reconfigure` 每次都清理的退役优先级（其中有 900）。

命名空间里 SmartGateAgent 按厂商原样工作，不再需要 `ip` 拦截脚本：它把 `default via 192.168.255.1 dev
tun0` 加到 main，用自己的 0xa38 mark（table 20）和源地址规则（table 230）逃逸到上行。上行默认路由的
metric 是 600，所以两者共存。iOA 自己产生的包（`iif lo`）先查 main 去掉默认路由的部分，再查只有上行
默认路由的 table 170，因此路径和以前在宿主机上时一样：不会被自己的隧道默认路由吸进 tun0。

**宿主机进程访问内网**：宿主机策略照旧决定什么是 iOA 流量（1150：mark 0x1，以及不带 mark 的 10/8 和
192.168.255.0/24；1151 prohibit 兜底）。table `ioa` 现在是 `default via 169.254.170.2 dev veth-ioa`，
进入命名空间后走 SmartGateAgent 的隧道默认路由进 tun0，再 MASQUERADE 成 tun0 地址。命名空间只允许从
veth 进来的包从 tun0 出去：没有 tun0 时（iOA 停了或没登录）TCP 立即收到 RST，其他协议收到 ICMP
admin-prohibited（命名空间内关掉了 ICMP 限速，否则拒绝会被吞成超时），绝不会从物理上行漏出去。
宿主机进入命名空间的流量统一 MASQUERADE 成 veth 地址：带 mark 的 socket 保留第一次查路由时选的源地址
（出口节点或上行的地址），命名空间里回这些地址只能走隧道。tinyproxy 的 uid mark 保留，tailnet 客户端
经代理访问内网走同一条路。已核实 `UnknownProcess` 的票据请求照样成功（47 次全部 `success(0)`）。

**DNS**：
- iOA 自己的解析发往 `127.0.0.1:53`（`resolv.conf` 固定为宿主机 SmartDNS，见 `network/resolv-pin`）。
  命名空间里 DNAT 到 veth 的宿主机端，宿主机再 DNAT 到 `127.0.0.1:53`（两端都开 `route_localnet`）。
- 宿主机 SmartDNS 的 `ioa` 组上游 `192.168.255.10`（SmartGateAgent 在命名空间里监听）经 1150 的
  192.168.255.0/24 规则进命名空间；命名空间把来源改写成目的地址本身，解析器看到的对端和以前一样。
- 从办公室租约学到的内网解析器（如 10.76.9.15）不再带 `-interface tun0`，经 1150 的 10/8 规则进隧道。

## GUI 和 daemon

- `iOA.bin`、`SmartGateAgent` 由 `ngnclient.service` 启动，加 `NetworkNamespacePath=/run/netns/ioa`。
- `iOALinux` 留在宿主机，不进命名空间。它和 daemon 之间只靠 `127.0.0.1:56100/56101`，这两个端口从宿主机
  回环转进命名空间，GUI 不需要任何改动，也就不需要 sudo 入口、X11 也不受影响。
- SmartGateAgent 的本地端口（12639/12649/12679/12759/12769/12101）同样转发，所以 corkscrew、`amos.pac`、
  qutebrowser 和 GUI 里写死的 `PROXY 127.0.0.1:12639` 都不用改。
- 转发用 systemd 自带的组件，不写代码：`ioa-port@.socket` 在宿主机绑定 `127.0.0.1:%i`，被连接时激活
  `ioa-port@.service`，后者带 `NetworkNamespacePath=/run/netns/ioa` 运行 `systemd-socket-proxyd 127.0.0.1:%i`。
  每个端口一个实例，互不影响；代理退出后下一个连接会重新激活。代价是 SmartGateAgent 看到的发起进程变成
  代理（或 `UnknownProcess`），票据照样成功（已核实）。

## BPF 心跳超时

`ngnclient.service` 的 cgroup 不变，`BPFProgram=sock_ops` 照样生效。顺带解决之前的越界：命名空间里
SmartGateAgent 的隧道和 iOA.bin 的控制连接仍在同一 cgroup，按进程名例外 SmartGateAgent 的设计保留不变
（30s 给 iOA.bin，120s 给 SmartGateAgent）。

## 不会变的风险

- iOA 安装脚本会改 `/etc/environment`、装 `ioa-resume.service`（重启 ngnclient，必须保持不装）。
- iOA 自己的 bug（崩溃、重启即要求重新登录）仍在；隔离只减少由宿主机网络事件触发的那部分。

## 上线步骤

离线部分已完成：`network/test-ioa-netns.sh`（沙箱，全部路径加变异测试）、`network/test-ioa-netns-wiring.sh`
（真实 systemd 跑单元模板）、`network/test-reconfigure.sh` 和 `network/test-static-policy.sh`（宿主机侧
删掉共存补丁后的策略）。

切换只有一次，需要扫码一次：`.tmp/cutover-ioa-netns.sh`（默认只打印，`--apply` 执行）。顺序是固定的：
正在运行的 SmartGateAgent 调用 `scripts/overrides/ip`，正在运行的策略依赖 400/401，而合入后这两样都不在
了，所以先停 ngnclient，再合入 master，再安装单元，起命名空间和端口，跑一次 `network-reconfigure`
（tun0 已不在宿主机，它顺带清掉 1100/1200、table 20/230 和 tun0 的 MASQUERADE），最后起 ngnclient。

验证：命名空间里有 tun0、宿主机没有；心跳和票据成功；GUI 登录状态和 `ioa-health`；git/iwiki；
corkscrew SSH；浏览器 PAC；之后再做一次真实漫游。

回滚：快照里的 `rollback.sh --apply --restart-ioa`（daemon 已在命名空间里时它会拒绝不带
`--restart-ioa` 运行）。

## 待实测的问题

1. SmartGateAgent 在命名空间里能否创建 tun0 并正常工作（`/dev/net/tun` 不受 netns 影响，应可以）。
2. 宿主机漫游时，命名空间内 iOA 是否真的收不到任何地址事件，隧道是否仍需 BPF 才能恢复。
3. `exec -a firefox` 的来历（命名空间后是否还需要）。

## 风险核实记录（2026-11-18，只读）

### SmartGateAgent 会不会改宿主机的 `/etc/resolv.conf`

**会尝试，被挡住了。** 网络命名空间不隔离文件系统，所以它在命名空间里的行为和现在一样。

- 宿主机的 `/etc/resolv.conf` 带不可变属性（`lsattr` 显示 `i`），内容 `nameserver 127.0.0.1`。
- 日志里共 109 次 `Setting system DNS servers: [192.168.255.10 114.114.114.114]`，每次都以
  `rename /etc/resolv.conf /etc/resolv_ngn_backup.conf: operation not permitted` 失败。
- `/etc/resolv_ngn.conf`（它想写进去的内容）停在 2026-07-27，之后没有成功写过。

结论：只要不可变属性保留，A 不改变这一点。但这个属性是 A 的前提，必须写进部署检查。

### 漫游后 iOA 是否依赖地址事件来恢复

**不依赖。** 当前 iOA 收到 `RTM_NEWADDR` 后只做一次 `http agent inconn`（2s 超时）和 `BuildPacket`，
不重建心跳连接；心跳仍按自己的 60 秒节奏走，连接由 BPF 超时（`close:…,2,110`）或对端 RST
（`close:…,2,104`）结束后再重连。最近 30 次地址事件里，下一次心跳成功的时间就是心跳相位决定的
1–64 秒（个别 200 秒以上的出在连续换 IP 期间），和有没有收到地址事件无关。

结论：命名空间里收不到地址事件，恢复速度不会因此变慢。
