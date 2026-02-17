---
name: aidoc
description: 记录 AI 对话产出的知识到 aidoc 仓库。当用户说"记录到 aidoc"、"存到 aidoc"、"保存这个知识点"时使用此 skill。
---

# AI Doc 知识库

将对话中产出的知识记录到 `~/git/aidoc` 仓库。

## 操作步骤

1. **整理内容**: 提取对话中的关键知识点，去除冗余
2. **创建文件**: 在 `~/git/aidoc/` 根目录创建 `topic-name.md`（小写短横线命名）
3. **文件格式**:
   ```markdown
   # 标题

   > 关键词: keyword1, keyword2

   正文内容...
   ```
4. **提交**: `cd ~/git/aidoc && git add <file> && git commit -m "docs: 简短描述"`

## 示例

用户说: "把刚才的 docker 网络配置记录到 aidoc"

操作:
1. 创建 `~/git/aidoc/docker-network.md`
2. 执行 `cd ~/git/aidoc && git add docker-network.md && git commit -m "docs: docker network configuration"`

## 注意事项

- 文件名用小写英文和短横线
- 内容要精炼，去除对话中的冗余部分
- 提交信息用 `docs:` 前缀
