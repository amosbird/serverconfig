# A Practical Linux Environment Built from Experience

This repository documents how I build and use my Linux environment. It's not just a list of tools, but a reflection of how I think about interaction, configuration, and control. Every tool is the result of over ten years of iteration, experimentation, and refinement. Nothing is random. Each choice reflects a real need, a hard-earned preference, or a lesson learned from daily use. Most tools are customized and integrated into a setup that feels consistent and efficient across all my machines.

I often contribute back to the tools I use. Whether it is fixing bugs, adding features, or understanding the internals, this process helps me grasp how each tool works and how to make it fit better into my workflow.

The environment is organized into a few categories: 

---

## Interaction layer

These are the tools I interact with all day, so they need to feel consistent. I prefer software that supports live or dynamic configuration, ideally scriptable in Lua, Python, Elisp, or similar. Keyboard-first is preferred, but good gesture or mouse support is also important.

**Examples:**

- Editor: Emacs with Doom. Keyboard-first, modal editing, programmable
- Window Manager: Qtile. Tiling, dynamic, Python-configurable
- Terminal: Kitty. Feature-rich, extendable, remote-friendly
- Browser: Vivaldi. Rich, extendable, mail support

---

## Supporting tools

These tools work behind the scenes but are essential for a smooth workflow. I choose ones that focus on a single task and can be scripted or combined easily.

**Examples:**

- Clipboard: CopyQ (scriptable)
- Screenshot: Flameshot (feature rich)
- Input method: fcitx5 + librime + librimelua (scriptable)
- Keyboard & Gesture control: xkeysnail + libinput-gestures (extendable)
- Popup menu: rofi (scriptable)

---

## Terminal ecosystem

There is no one-size-fits-all solution here. I regularly rotate tools, experiment, and tweak settings. The goal is to make the terminal interactive in multiple ways, including history, suggestions, consistent control across tools, speed, and reliability.

**Some suggestions:**

- Editor: emacs (again)
- Shell: fish (consistent)
- Multiplexer: tmux (remote workspace) or just kitty (local workspace)
- History: Atuin (a good new option)

---

## Dev environments

I do most development on remote machines with hundreds of cores because hacking ClickHouse requires significant resources. This is natural since the remote environment is always on, and the workspace is ready within seconds after connecting. The philosophy is to make the environment portable and consistent.

I build my own dev environment using Gentoo Prefix inside a standalone directory, `/tmp/gentoo`. I also wrote a namespace mount script to mount this fixed prefix directory to any target directory. This setup provides:

- A fully self-contained Linux distribution with development tools installed by default
- Compatibility across any Linux distro or server
- Portability and reproducibility — just rsync the folder and you are done

This approach has saved me countless times when working across bare-bones cloud VMs or locked-down company servers.

---

## Final note

This is just an initial overview. The content is extensive and will continue to evolve as I refine ideas and add details. This repository is mainly for reference and inspiration rather than an out-of-the-box solution. Feel free to explore and adapt what fits your workflow.
