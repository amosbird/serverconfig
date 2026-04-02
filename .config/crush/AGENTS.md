# Global Agent Instructions

## Language

When the user writes in Chinese or asks you to respond in Chinese, you MUST
respond entirely in Chinese (简体中文). This takes priority over all other
language instructions.

## Worker Tool Usage

Only use the Worker tool when you can launch multiple workers in parallel on
independent files/modules. For serial/sequential tasks, do the work directly
instead of delegating to a worker — the extra indirection adds latency and
context overhead with no benefit.
