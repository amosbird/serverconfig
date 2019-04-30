#!/usr/bin/env bash

usage() {
    echo 'Usage: kernelmodule.sh directory-name'
    exit 0
}

module="${1}"

if [ "$(type -fP dpkg)" = "/usr/bin/dpkg" ]; then
    should_have="linux-headers-$(uname -r) build-essential"
    install=
    for package in ${should_have}; do
        if ! dpkg -s "${package}" >/dev/null 2>&1; then
            [ -n "${install}" ] && install="${install} "
            install="${install}${package}"
        fi
    done
    [ -n "${install}" ] && sudo apt-get install ${install}
fi

if [ -d "${module}" ]; then
    echo "The directory \`${module}' already exists. Aborting."
    exit 1
fi

mkdir "${module}"

cat >"${module}/Makefile" <<EOF
CC := /usr/bin/cc
obj-m := ${module}.o
KDIR := /lib/modules/\$(shell uname -r)/build
PWD := \$(shell pwd)

.PHONY: build clean

build:
    make -C /lib/modules/\$(shell uname -r)/build M=\$(PWD) modules

clean:
    make -C /lib/modules/\$(shell uname -r)/build M=\$(PWD) clean
EOF

cat >"${module}/${module}.c" <<EOF
#include <linux/module.h>
#include <linux/kernel.h>

static int
init_${module} ( void )
{
    printk ( KERN_INFO "init_module() called\n" );
    return 0;
}

static void
exit_${module} ( void )
{
    printk ( KERN_INFO "cleanup_module() called\n" );
}

module_init ( init_${module} );
module_exit ( exit_${module} );

MODULE_LICENSE("Dual BSD/GPL");
EOF
