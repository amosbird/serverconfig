;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode)
(package! cuda-mode)
(package! demangle-mode)
(package! disaster)
(package! clang-format)
(package! modern-cpp-font-lock)
(package! opencl-mode)
(package! ccls :recipe (:fetcher github :repo "MaskRay/emacs-ccls"))
