# Maintainer: Amos Bird <amosbird@gmail.com>

_pkgname=feh
pkgname=feh-git
pkgver=3.1.3.r42.ge73eba5
pkgrel=1
pkgdesc='Fast and light imlib2-based image viewer'
arch=('x86_64')
url="https://github.com/amosbird/feh"
license=('custom:MIT')
depends=('imlib2' 'curl' 'libxinerama' 'libexif')
optdepends=('imagemagick: support more file formats')
makedepends=('libxt')
categories=('graphics')
source=("${_pkgname}::git+https://github.com/amosbird/${_pkgname}.git")
md5sums=('SKIP')
provides=("${_pkgname}=${pkgver%%.r*}-${pkgrel}")
conflicts=("${_pkgname}")

pkgver() {
  cd "$_pkgname"
  git describe --long | sed 's/\([^-]*-g\)/r\1/;s/-/./g'
}

build() {
	cd "$_pkgname"
	make PREFIX=/usr exif=1 help=1 stat64=1
}

package() {
	cd "$_pkgname"
	make PREFIX=/usr DESTDIR="$pkgdir" install
	install -Dm644 COPYING "${pkgdir}/usr/share/licenses/${_pkgname}/LICENSE"
}
