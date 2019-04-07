#!/usr/bin/env python3
import os
import subprocess
import sys
import time
try:
    from time import monotonic as monotonic_clock
except ImportError:
    # Python 3.2 and older
    from time import time as monotonic_clock


# Wait N seconds before stoping a process if the load is higher
# than the maximum
NEXT_STOP = 5.0


def get_system_load():
    return os.getloadavg()[0]


def burn_cpu():
    try:
        while 1: pass
    except KeyboardInterrupt:
        pass
    os._exit(0)


def stop_child(proc):
    print("Stop child %s" % proc.pid)
    proc.terminate()
    returncode = proc.wait(1.0)
    if returncode is None:
        print("Kill child %s" % proc.pid)
        proc.kill()
        proc.wait()


def load_controller(min_load, max_load):
    if max_load is not None:
        diff = max_load - min_load
    else:
        diff = 0.0
    if diff < 1.0:
        max_load = min_load + 1.0
        print("Adjust max load: %.2f" % max_load)

    processes = []
    next_stop = monotonic_clock() + NEXT_STOP
    try:
        while True:
            load = get_system_load()
            print("System load: %.2f (min=%.2f, max=%.2f)"
                  " -- %s child processes"
                  % (load, min_load, max_load, len(processes)))
            if load < min_load:
                args = [sys.executable, sys.argv[0], 'burn']
                proc = subprocess.Popen(args)
                processes.append(proc)
                print("Spawn a new child: %s" % proc.pid)

                next_stop = monotonic_clock() + NEXT_STOP
            else:
                now = monotonic_clock()
                if now >= next_stop and processes and load > max_load:
                    next_stop = now + NEXT_STOP
                    proc = processes.pop()
                    stop_child(proc)
            time.sleep(1.0)
    except KeyboardInterrupt:
        print()
        print("CTRL+c: stop")

    for proc in processes:
        stop_child(proc)

    print()
    print("System load at exit: %.2f" % get_system_load())


def main():
    if len(sys.argv) == 2 and sys.argv[1] == 'burn':
        burn_cpu()

    if len(sys.argv) == 3:
        min_load = float(sys.argv[1])
        max_load = float(sys.argv[1])
    elif len(sys.argv) == 2:
        min_load = float(sys.argv[1])
        max_load = None
    else:
        print("usage: %s min_load [max_load]")
        sys.exit(1)
    load_controller(min_load, max_load)


if __name__ == "__main__":
    main()
