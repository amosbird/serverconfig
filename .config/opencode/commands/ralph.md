---
description: "Start Craftsman Ralph Loop to autonomously implement tasks from a Craftsman plan"
agent: craftsman-ralph
---

Start the Ralph Loop for the PRD folder at: $ARGUMENTS

Read PROGRESS.md first (create it if missing), then begin the implementation loop.
Delegate each task to the craftsman-coder subagent, verify with craftsman-inspector,
and continue until all tasks are complete.

If the user said "HITL" or "human in the loop", enable Human-in-the-Loop mode
(pause at phase boundaries for human approval).
