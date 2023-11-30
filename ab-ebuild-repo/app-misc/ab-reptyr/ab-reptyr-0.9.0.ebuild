# Copyright 1999-2022 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

inherit git-r3
EGIT_REPO_URI="https://github.com/amosbird/reptyr.git"
EGIT_CLONE_TYPE="shallow"
KEYWORDS="amd64"

LICENSE="MIT"
SLOT="0"
KEYWORDS="amd64"

RESTRICT="test"

src_install() {
    emake PREFIX="${EPREFIX}/usr" install
}
