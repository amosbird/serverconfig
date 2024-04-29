# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

inherit autotools elisp-common flag-o-matic readme.gentoo-r1 toolchain-funcs

inherit git-r3
EGIT_REPO_URI="https://github.com/amosbird/emacs.git"
EGIT_BRANCH="master"
EGIT_CHECKOUT_DIR="${WORKDIR}/emacs"
EGIT_CLONE_TYPE="shallow"
S="${EGIT_CHECKOUT_DIR}"
SLOT="${PV%%.*-vcs}"
KEYWORDS="amd64"

DESCRIPTION="The extensible, customizable, self-documenting real-time display editor"
HOMEPAGE="https://www.gnu.org/software/emacs/"

LICENSE="GPL-3+ FDL-1.3+ BSD HPND MIT W3C unicode PSF-2"
IUSE="acl alsa aqua athena cairo dbus dynamic-loading games gfile gif +gmp gpm gsettings gtk gui gzip-el harfbuzz imagemagick +inotify jit jpeg json kerberos lcms libxml2 livecd m17n-lib mailutils motif png selinux sound source sqlite ssl svg systemd +threads tiff toolkit-scroll-bars tree-sitter valgrind webp wide-int +X Xaw3d xft +xpm xwidgets zlib"

RDEPEND="sys-libs/ncurses:0=
	acl? ( virtual/acl )
	alsa? ( media-libs/alsa-lib )
	dbus? ( sys-apps/dbus )
	games? ( acct-group/gamestat )
	gmp? ( dev-libs/gmp:0= )
	gpm? ( sys-libs/gpm )
	!inotify? ( gfile? ( >=dev-libs/glib-2.28.6 ) )
	jit? (
		sys-devel/gcc:=[jit(-)]
		sys-libs/zlib
	)
	json? ( dev-libs/jansson:= )
	kerberos? ( virtual/krb5 )
	lcms? ( media-libs/lcms:2 )
	libxml2? ( >=dev-libs/libxml2-2.2.0 )
	mailutils? ( net-mail/mailutils[clients] )
	!mailutils? ( acct-group/mail net-libs/liblockfile )
	selinux? ( sys-libs/libselinux )
	sqlite? ( dev-db/sqlite:3 )
	ssl? ( net-libs/gnutls:0= )
	systemd? ( sys-apps/systemd )
	tree-sitter? ( dev-libs/tree-sitter )
	valgrind? ( dev-util/valgrind )
	zlib? ( sys-libs/zlib )
	"

DEPEND="${RDEPEND}"

BDEPEND="sys-apps/texinfo
	virtual/pkgconfig
	gzip-el? ( app-arch/gzip )"

IDEPEND="app-eselect/eselect-emacs"

RDEPEND+=" ${IDEPEND}"

EMACS_SUFFIX="emacs-${SLOT}"
SITEFILE="20${EMACS_SUFFIX}-gentoo.el"

src_prepare() {
	FULL_VERSION=$(sed -n 's/^AC_INIT([^,]*,[^0-9.]*\([0-9.]*\).*/\1/p' \
		configure.ac)
	[[ ${FULL_VERSION} ]] || die "Cannot determine current Emacs version"
	einfo "Emacs branch: ${EGIT_BRANCH}"
	einfo "Commit: ${EGIT_VERSION}"
	einfo "Emacs version number: ${FULL_VERSION}"
	[[ ${FULL_VERSION} =~ ^${PV%.*}(\..*)?$ ]] \
		|| die "Upstream version number changed to ${FULL_VERSION}"

	if use jit; then
		find lisp -type f -name "*.elc" -delete || die

		# These files ignore LDFLAGS. We assign the variable here, because
		# for live ebuilds FULL_VERSION doesn't exist in global scope
		QA_FLAGS_IGNORED="usr/$(get_libdir)/emacs/${FULL_VERSION}/native-lisp/.*"

		# gccjit doesn't play well with ccache or distcc #801580
		# For now, work around the problem with an explicit LIBRARY_PATH
		has ccache ${FEATURES} || has distcc ${FEATURES} && tc-is-gcc \
			&& export LIBRARY_PATH=$("$(tc-getCC)" -print-search-dirs \
				| sed -n '/^libraries:/{s:^[^/]*::;p}')
	fi

	default

	# Fix filename reference in redirected man page
	sed -i -e "/^\\.so/s/etags/&-${EMACS_SUFFIX}/" doc/man/ctags.1 || die

	# libseccomp is detected by configure but doesn't appear to have any
	# effect on the installed image. Suppress it by supplying pkg-config
	# with a wrong library name.
	sed -i -e "/CHECK_MODULES/s/libseccomp/DiSaBlE&/" configure.ac || die

	AT_M4DIR=m4 eautoreconf
}

src_configure() {
	local myconf

	# Prevents e.g. tests interfering with running Emacs.
	unset EMACS_SOCKET_NAME

	if use alsa; then
		use sound || ewarn \
			"USE flag \"alsa\" overrides \"-sound\"; enabling sound support."
		myconf+=" --with-sound=alsa"
	else
		myconf+=" --with-sound=$(usex sound oss)"
	fi

	if use jit; then
		use zlib || ewarn \
			"USE flag \"jit\" overrides \"-zlib\"; enabling zlib support."
		myconf+=" --with-zlib"
	else
		myconf+=" $(use_with zlib)"
	fi

	einfo "Configuring to build without window system support"
	myconf+=" --without-x --without-pgtk --without-ns"
	myconf+=" --with-dumping=pdumper"

	econf \
		--program-suffix="-${EMACS_SUFFIX}" \
		--includedir="${EPREFIX}"/usr/include/${EMACS_SUFFIX} \
		--infodir="${EPREFIX}"/usr/share/info/${EMACS_SUFFIX} \
		--localstatedir="${EPREFIX}"/var \
		--enable-locallisppath="${EPREFIX}/etc/emacs:${EPREFIX}${SITELISP}" \
		--without-compress-install \
		--without-hesiod \
		--without-pop \
		--with-file-notification=$(usev inotify || usev gfile || echo no) \
		--with-pdumper \
		$(use_enable acl) \
		$(use_with dbus) \
		$(use_with dynamic-loading modules) \
		$(use_with games gameuser ":gamestat") \
		$(use_with gmp libgmp) \
		$(use_with gpm) \
		$(use_with jit native-compilation aot) \
		$(use_with json) \
		$(use_with kerberos) $(use_with kerberos kerberos5) \
		$(use_with lcms lcms2) \
		$(use_with libxml2 xml2) \
		$(use_with mailutils) \
		$(use_with selinux) \
		$(use_with sqlite sqlite3) \
		$(use_with ssl gnutls) \
		$(use_with systemd libsystemd) \
		$(use_with threads) \
		$(use_with tree-sitter) \
		$(use_with wide-int) \
		${myconf}
}

src_compile() {
	export ac_cv_header_valgrind_valgrind_h=$(usex valgrind)
	append-cppflags -DUSE_VALGRIND=$(usex valgrind)
	emake
}

src_install() {
	emake DESTDIR="${D}" NO_BIN_LINK=t BLESSMAIL_TARGET= install

	mv "${ED}"/usr/bin/{emacs-${FULL_VERSION}-,}${EMACS_SUFFIX} || die
	mv "${ED}"/usr/share/man/man1/{emacs-,}${EMACS_SUFFIX}.1 || die
	mv "${ED}"/usr/share/metainfo/{emacs-,}${EMACS_SUFFIX}.metainfo.xml || die

	# dissuade Portage from removing our dir file #257260
	touch "${ED}"/usr/share/info/${EMACS_SUFFIX}/.keepinfodir
	docompress -x /usr/share/info/${EMACS_SUFFIX}/dir

	# movemail must be setgid mail
	if ! use mailutils; then
		fowners root:mail /usr/libexec/emacs/${FULL_VERSION}/${CHOST}/movemail
		fperms 2751 /usr/libexec/emacs/${FULL_VERSION}/${CHOST}/movemail
	fi

	# avoid collision between slots, see bug #169033 e.g.
	rm "${ED}"/usr/share/emacs/site-lisp/subdirs.el || die
	rm -rf "${ED}"/usr/share/{applications,icons} || die
	rm -rf "${ED}"/usr/share/glib-2.0 || die #911117
	rm -rf "${ED}/usr/$(get_libdir)/systemd" || die
	rm -rf "${ED}"/var || die

	# remove unused <version>/site-lisp dir
	rm -rf "${ED}"/usr/share/emacs/${FULL_VERSION}/site-lisp || die

	# remove COPYING file (except for etc/COPYING used by describe-copying)
	rm "${ED}"/usr/share/emacs/${FULL_VERSION}/lisp/COPYING || die

	if use gzip-el; then
		# compress .el files when a corresponding .elc exists
		find "${ED}"/usr/share/emacs/${FULL_VERSION}/lisp -type f \
			-name "*.elc" -print | sed 's/\.elc$/.el/' | xargs gzip -9n
		assert "gzip .el failed"
	fi

	local cdir

	if use source; then
		cdir="/usr/share/emacs/${FULL_VERSION}/src"
		insinto "${cdir}"
		# This is not meant to install all the source -- just the
		# C source you might find via find-function
		doins src/*.{c,h,m}
	elif has installsources ${FEATURES}; then
		cdir="/usr/src/debug/${CATEGORY}/${PF}/${S#"${WORKDIR}/"}/src"
	fi

	sed -e "${cdir:+#}/^Y/d" -e "s/^[XY]//" >"${T}/${SITEFILE}" <<-EOF || die
	X
	;;; ${EMACS_SUFFIX} site-lisp configuration
	X
	(when (string-match "\\\\\`${FULL_VERSION//./\\\\.}\\\\>" emacs-version)
	Y  (setq find-function-C-source-directory
	Y	"${EPREFIX}${cdir}")
	X  (let ((path (getenv "INFOPATH"))
	X	(dir "${EPREFIX}/usr/share/info/${EMACS_SUFFIX}")
	X	(re "\\\\\`${EPREFIX}/usr/share\\\\>"))
	X    (and path
	X	 ;; move Emacs Info dir before anything else in /usr/share
	X	 (let* ((p (cons nil (split-string path ":" t))) (q p))
	X	   (while (and (cdr q) (not (string-match re (cadr q))))
	X	     (setq q (cdr q)))
	X	   (setcdr q (cons dir (delete dir (cdr q))))
	X	   (setenv "INFOPATH" (mapconcat 'identity (cdr p) ":"))))))
	EOF
	elisp-site-file-install "${T}/${SITEFILE}" || die

	dodoc README BUGS CONTRIBUTE

	local DOC_CONTENTS="You can set the version to be started by
		/usr/bin/emacs through the Emacs eselect module, which also
		redirects man and info pages. Therefore, several Emacs versions can
		be installed at the same time. \"man emacs.eselect\" for details.
		\\n\\nIf you upgrade from a previous major version of Emacs, then
		it is strongly recommended that you use app-admin/emacs-updater
		to rebuild all byte-compiled elisp files of the installed Emacs
		packages."

	readme.gentoo_create_doc
}

pkg_preinst() {
	# verify that the PM hasn't removed our Info directory index #257260
	local infodir="${ED}/usr/share/info/${EMACS_SUFFIX}"
	[[ -f ${infodir}/dir || ! -d ${infodir} ]] || die
}

pkg_postinst() {
	elisp-site-regen
	readme.gentoo_print_elog

	if use livecd; then
		# force an update of the emacs symlink for the livecd/dvd,
		# because some microemacs packages set it with USE=livecd
		eselect emacs update
	else
		eselect emacs update ifunset
	fi
}

pkg_postrm() {
	elisp-site-regen
	eselect emacs update ifunset
}
