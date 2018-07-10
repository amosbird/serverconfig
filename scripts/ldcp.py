#!/usr/bin/env python

import argparse
import os
import shutil
import subprocess

LDLINUX = 'ld-linux-x86-64.so.2'


def collect(roots):
    paths = {}

    def recur(path):
        filename = os.path.basename(path)
        if filename in paths:
            return
        paths[filename] = path

        try:
            out = subprocess.check_output(['ldd', path])
        except subprocess.CalledProcessError:
            return

        for line in out.splitlines():
            args = line.split()
            if 'linux-vdso.so.1' in args or 'statically' in args:
                continue

            if '=>' in args:
                pos = args.index('=>')
                arg = args[pos + 1]
                print(arg)
                recur(arg)

            recur(args[0])

    for root in roots:
        recur(root)

    return paths


def save(paths, dst):
    if not os.path.exists(dst):
        os.makedirs(dst)

    for filename, path in paths.iteritems():
        dstpath = os.path.join(dst, filename)
        shutil.copy(path, dst)
        os.chmod(dstpath, 0755)

        if filename == LDLINUX:
            continue

        try:
            headers = subprocess.check_output(['objdump', '-h', dstpath])
        except subprocess.CalledProcessError:
            continue

        args = ['patchelf', '--set-rpath', '$ORIGIN']
        if '.interp' in headers.split():
            args += ['--set-interpreter', './ld-linux-x86-64.so.2']
            subprocess.check_call(args + [dstpath])

        if '.so' not in filename:
            os.rename(dstpath, dstpath + '.bin')
            with open(dstpath, 'w') as f:
                f.write('#!/bin/sh\n')
                f.write('d="$(dirname "$(readlink -f "$0")")"\n')
                f.write('exec "$d/{}" "$d/{}" "$@"\n'.format(LDLINUX, filename + '.bin'))
                os.chmod(dstpath, 0755)


def main():
    parser = argparse.ArgumentParser()
    arg = parser.add_argument
    arg('dst')
    arg('path', nargs='+')
    args = parser.parse_args()
    paths = collect(args.path)
    save(paths, args.dst)


if __name__ == '__main__':
    main()
