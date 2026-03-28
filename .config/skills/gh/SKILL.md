---
name: gh
description: "使用 gh CLI 操作 GitHub。涉及 GitHub 的操作（PR、issue、repo、release、gist、workflow 等）一律使用 gh 命令，不要用 curl 调 GitHub API。"
---

# GitHub CLI (gh)

所有 GitHub 操作使用 `gh` 命令完成，不要直接调用 GitHub API。

## 常用操作

### Pull Request
- 创建: `gh pr create --title "title" --body "body"`
- 查看: `gh pr view [number]`
- 列表: `gh pr list`
- 合并: `gh pr merge [number]`
- checkout: `gh pr checkout [number]`
- diff: `gh pr diff [number]`

### Issue
- 创建: `gh issue create --title "title" --body "body"`
- 查看: `gh issue view [number]`
- 列表: `gh issue list`
- 关闭: `gh issue close [number]`

### Repo
- 克隆: `gh repo clone owner/repo`
- 创建: `gh repo create name --public/--private`
- 查看: `gh repo view [owner/repo]`
- fork: `gh repo fork [owner/repo]`

### Release
- 创建: `gh release create tag --title "title" --notes "notes"`
- 列表: `gh release list`
- 下载: `gh release download tag`

### Workflow
- 列表: `gh run list`
- 查看: `gh run view [run-id]`
- 触发: `gh workflow run [workflow]`

### 其他
- gist: `gh gist create/list/view/edit/delete`
- API 调用: `gh api [endpoint]`（需要直接调 API 时用这个，而不是 curl）

## 注意事项

- 优先使用 gh 子命令，只有子命令不支持时才退回 `gh api`
- 创建 PR 时用 HEREDOC 写 body 避免转义问题
- 不要用 `curl` 调 GitHub API
