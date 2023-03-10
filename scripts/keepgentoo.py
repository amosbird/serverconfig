#!/usr/bin/env python

import os
import shutil
import tempfile
import time
import sys
import ctypes
import logging
import errno
import renameat2

# logger = logging.getLogger(os.path.basename(__file__))
# libc = ctypes.CDLL("libc.so.6", use_errno=True)
# AT_FDCWD = -100
# RENAME_EXCHANGE = 1 << 1


# def rename_exchange(a, b):
#     if hasattr(libc, "renameat2"):
#         print("prepare to renameat2")
#         ret = libc.renameat2(AT_FDCWD, a, AT_FDCWD, b, RENAME_EXCHANGE)
#         print("ret = " + str(ret))
#         if ret == 0:
#             print("renameat2 with RENAME_EXCHANGE succeeded")
#             return

#         err = ctypes.get_errno()
#         if err == errno.EINVAL:
#             print("RENAME_EXCHANGE flag not supported by kernel")
#         elif err == errno.ENOSYS:
#             print("renameat2 syscall not available in kernel")
#         else:
#             raise OSError(err, os.strerror(err), a, None, b)
#     else:
#         print("renameat2 not available in libc")

#     # Fallback using a temporary name in the same directory as a. b is
#     # renamed to the temporary name first so that if they're not on the
#     # same filesystem no recovery is needed.
#     print("Falling back to renames with temporary files")
#     a_tmp = tempfile.mktemp(prefix=a, dir=os.path.dirname(a))
#     os.rename(b, a_tmp)
#     os.rename(a, b)
#     os.rename(a_tmp, a)


if len(sys.argv) < 2:
    print("请提供一个参数")
    sys.exit(1)

src = sys.argv[1]

if not os.path.isdir(src):
    print("提供的参数不是一个目录")
    sys.exit(1)

dest = "/tmp/gentoo"
while True:
    if os.path.islink(dest):
        time.sleep(3)
    else:
        try:
            os.symlink(src, dest)
        except FileExistsError:
            temp_dest = os.path.join("/tmp/", next(tempfile._get_candidate_names()))
            os.symlink(src, temp_dest)
            renameat2.exchange(temp_dest, dest)
            shutil.rmtree(temp_dest)
