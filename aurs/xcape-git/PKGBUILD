# Maintainer: joetw <call dot imera42 at googlemail dot com>
# Contributor: Greg Fitzgerald <greg at gregf dot org>

pkgname=xcape-git
pkgver=1.1.r31.g6262889
pkgrel=1
epoch=1
pkgdesc="Use modifier pressed/released solo as another key/chord, e.g. CapsLock > Escape"
arch=('i686' 'x86_64')
url="https://github.com/amosbird/xcape"
license=('GPL3')
depends=('libxtst')
makedepends=('git')
provides=('xcape')
conflicts=('xcape')
source=("git+${url}")
sha256sums=('SKIP')

pkgver() {
  cd xcape
  git describe --long --tags | sed 's/^v//;s/\([^-]*-g\)/r\1/;s/-/./g'
}

build() {
  cd xcape
  make
}

package() {
  cd xcape
  make DESTDIR="$pkgdir/" PREFIX="/usr/" MANDIR="/share/man/man1" install
  install -Dm 644 README.md "$pkgdir/usr/share/doc/$pkgname/README.md"

  find "$pkgdir" -type d -name .git -exec rm -r '{}' +
}

# vim:set ft=PKGBUILD ts=2 sw=2 et:
