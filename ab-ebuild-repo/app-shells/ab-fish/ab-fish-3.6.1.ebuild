# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

PYTHON_COMPAT=( python3_{9..11} )

inherit cmake python-any-r1 readme.gentoo-r1 xdg

DESCRIPTION="Friendly Interactive SHell"
HOMEPAGE="https://fishshell.com/"

MY_PV="${PV/_beta/b}"
MY_P="${PN}-${MY_PV}"

inherit git-r3
EGIT_REPO_URI="https://github.com/amosbird/fish-shell.git"
EGIT_CLONE_TYPE="shallow"
KEYWORDS="amd64"

LICENSE="GPL-2"
SLOT="0"
IUSE="+doc nls split-usr test"

RESTRICT="!test? ( test )"

RDEPEND="
	>=dev-libs/libpcre2-10.32:=[pcre32]
	sys-apps/coreutils
	sys-libs/ncurses:=[unicode(+)]
"

DEPEND="${RDEPEND}"
BDEPEND="
	nls? ( sys-devel/gettext )
	test? (
		${PYTHON_DEPS}
		dev-tcltk/expect
		$(python_gen_any_dep '
			dev-python/pexpect[${PYTHON_USEDEP}]
		')
	)
"
DEPEND+=" doc? ( dev-python/sphinx )"

S="${WORKDIR}/${MY_P}"

python_check_deps() {
	use test || return 0
	python_has_version "dev-python/pexpect[${PYTHON_USEDEP}]"
}

src_prepare() {
	# workaround for https://github.com/fish-shell/fish-shell/issues/4883
	if use split-usr; then
		sed -i 's#${TEST_INSTALL_DIR}/${CMAKE_INSTALL_PREFIX}#${TEST_INSTALL_DIR}#' \
			cmake/Tests.cmake || die
	fi
	cmake_src_prepare
}

src_configure() {
	local mycmakeargs=(
		# installing into /bin breaks tests on merged usr systems.
		# sbin -> bin symlink confuses tests.
		# so on split-usr we install to /bin.
		# on merge-usr we set sbindir to bin.
		$(usex split-usr "-DCMAKE_INSTALL_BINDIR=${EPREFIX}/bin" \
			"-DCMAKE_INSTALL_SBINDIR=${EPREFIX}/usr/bin")
		-DCMAKE_INSTALL_SYSCONFDIR="${EPREFIX}/etc"
		-DCURSES_NEED_NCURSES=ON
		-DINSTALL_DOCS="$(usex doc)"
		-DWITH_GETTEXT="$(usex nls)"
	)
	# release tarballs ship pre-built docs // -DHAVE_PREBUILT_DOCS=TRUE
	mycmakeargs+=( -DBUILD_DOCS="$(usex doc)" )
	cmake_src_configure
}

src_install() {
	cmake_src_install
	keepdir /usr/share/fish/vendor_{completions,conf,functions}.d
}

pkg_postinst() {
	xdg_pkg_postinst
}
