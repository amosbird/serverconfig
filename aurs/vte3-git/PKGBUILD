# Maintainer: Igor <f2404@yandex.ru>
# Maintainer: Lubosz Sarnecki <lubosz@gmail.com>
# Original package: Ionut Biru <ibiru@archlinux.org>

_realname=vte3
pkgname=$_realname-git

pkgver=0.59.0.4583.f62faf93
pkgrel=1
pkgdesc="Virtual Terminal Emulator widget for use with GTK3"
arch=('i686' 'x86_64')
license=('LGPL')
options=('!emptydirs')
makedepends=('intltool' 'gobject-introspection' 'vala' 'gtk-doc' 'gperf' 'meson>=0.49.0')
url="https://github.com/amosbird/libvte"
depends=('gtk3' 'vte-common' 'glibc' 'pcre2')

provides=(vte3=$pkgver vte-common)
conflicts=($_realname vte-common)

source=("git+https://github.com/amosbird/libvte")
md5sums=("SKIP")

prepare() {
  cd libvte
  meson setup --prefix=/usr --sysconfdir=/etc \
      --libexecdir=/usr/lib/vte --localstatedir=/var \
      builddir
}

pkgver() {
  cd libvte
  version=$(grep "\#define VERSION " builddir/config.h | sed 's/\#define VERSION //' | sed 's/\"//g')
  hash=$(git log --pretty=format:'%h' -n 1)
  revision=$(git rev-list --count HEAD)
  echo $version.$revision.$hash
}

build() {
  cd libvte
  ninja -C builddir
}

package() {
  cd libvte
  DESTDIR="$pkgdir" meson install -C builddir
}
