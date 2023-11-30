# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

inherit git-r3
EGIT_REPO_URI="https://github.com/amosbird/cgdb.git"
EGIT_CLONE_TYPE="shallow"
KEYWORDS="amd64"

inherit autotools multilib-minimal

DESCRIPTION="A curses front-end for GDB, the GNU debugger"
HOMEPAGE="https://cgdb.github.io/"

LICENSE="GPL-2"
SLOT="0"
IUSE="test"
# Tests are broken, need additional research to figure out the cause
# Bug: https://bugs.gentoo.org/831899
RESTRICT="test"

DEPEND="
	sys-libs/ncurses:0=
	sys-libs/readline:0="

BDEPEND="
	test? (
		dev-util/dejagnu
		app-misc/dtach
	)"

RDEPEND="
	${DEPEND}
	sys-devel/gdb"

DOCS=( AUTHORS ChangeLog FAQ INSTALL NEWS README.md )

src_prepare() {
	default
	AT_M4DIR="config" eautoreconf
}

multilib_src_configure() {
	ECONF_SOURCE="${S}" econf
}
