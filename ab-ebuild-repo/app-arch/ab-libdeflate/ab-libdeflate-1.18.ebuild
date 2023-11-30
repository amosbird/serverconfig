
EAPI=8

DESCRIPTION="Heavily optimized DEFLATE/zlib/gzip (de)compression"
HOMEPAGE="https://github.com/ebiggers/libdeflate"

inherit cmake
inherit git-r3
EGIT_REPO_URI="https://github.com/ebiggers/libdeflate"
KEYWORDS="amd64"

LICENSE="MIT"
SLOT="0"
IUSE="static-libs test"
RESTRICT="test"

src_prepare() {
	cmake_src_prepare
}

src_configure() {
	cmake_src_configure
}

src_install() {
	cmake_src_install
	dodoc NEWS.md README.md
}
