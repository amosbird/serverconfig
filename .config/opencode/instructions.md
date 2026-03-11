# Git operations

- Never use git commands that open an interactive editor (e.g. `git commit` without `-m`, `git rebase -i`, `git merge` without `--no-edit`).
- Always pass messages inline: use `git commit -m "..."`, `GIT_EDITOR=true git rebase --continue`, `git merge --no-edit`, etc.
- This prevents the session from hanging on an editor prompt.
