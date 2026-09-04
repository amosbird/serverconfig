#!/usr/bin/env python3

import math
import statistics
import struct
import sys

GOOD_MAX_ABS = 12
GOOD_MAX_STDDEV = 4.0
GOOD_CONSECUTIVE_FRAMES = 3


def frame_is_good(data: bytes) -> tuple[bool, str]:
    if not data or len(data) % 2:
        return False, "invalid-image"

    values = struct.unpack(f"<{len(data) // 2}h", data)
    maximum = max(abs(value) for value in values)
    standard_deviation = statistics.pstdev(values)
    rms = math.sqrt(sum(value * value for value in values) / len(values))
    summary = (
        f"samples={len(values)} max_abs={maximum} "
        f"stddev={standard_deviation:.2f} rms={rms:.2f}"
    )
    good = maximum <= GOOD_MAX_ABS and standard_deviation <= GOOD_MAX_STDDEV
    return good, summary


def main() -> int:
    paths = sys.argv[1:]
    if not paths:
        paths = ["-"]

    good_frames = 0
    longest_run = 0
    current_run = 0
    for path in paths:
        if path == "-":
            data = sys.stdin.buffer.read()
        else:
            with open(path, "rb") as image:
                data = image.read()
        good, summary = frame_is_good(data)
        print(f"{path}: {summary}")
        good_frames += good
        current_run = current_run + 1 if good else 0
        longest_run = max(longest_run, current_run)

    required = min(GOOD_CONSECUTIVE_FRAMES, len(paths))
    print(
        f"good_frames={good_frames}/{len(paths)} "
        f"longest_run={longest_run} required_run={required}"
    )
    return 0 if longest_run >= required else 1


if __name__ == "__main__":
    raise SystemExit(main())
