;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm" . objc-mode) ("\\.h\\'" . c++-mode)
  :preface
  (defun +cc-c++-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (or (file-exists-p (expand-file-name
                             (concat (file-name-sans-extension buffer-file-name)
                                     ".cpp")))
             (when-let (file (car-safe (projectile-get-other-files
                                        buffer-file-name
                                        (projectile-current-project-files))))
               (equal (file-name-extension file) "cpp")))))

  (defun +cc-objc-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))

  (push (cons #'+cc-c++-header-file-p  'c++-mode)  magic-mode-alist)
  (push (cons #'+cc-objc-header-file-p 'objc-mode) magic-mode-alist)

  :init
  (setq-default c-basic-offset tab-width)

  :config
  (set! :electric '(c-mode c++-mode objc-mode java-mode)
    :chars '(?{ ?\n ?}))

  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (map! (:map (c-mode-map c++-mode-map)
          "<" nil
          :i ">"        #'+cc/autoclose->-maybe
          :n "C-e"      #'+amos/maybe-add-end-of-statement
          "C-c i"       #'+amos/ivy-add-include)

        (:after lsp-ui-peek
          :map lsp-ui-peek-mode-map
          "M-j" #'lsp-ui-peek--select-next-file
          "M-k" #'lsp-ui-peek--select-prev-file
          "C-j" #'lsp-ui-peek--select-next
          "C-k" #'lsp-ui-peek--select-prev))

  ;;; Style/formatting
  ;; C/C++ style settings
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  (c-set-offset 'substatement-open '0) ; don't indent brackets
  (c-set-offset 'innamespace       '0)
  (c-set-offset 'inline-open       '0)
  (c-set-offset 'block-open        '+)
  (c-set-offset 'brace-list-open   '+)
  (c-set-offset 'case-label        '+)
  (c-set-offset 'access-label      '-)
  (c-set-offset 'arglist-intro     '+)
  (c-set-offset 'arglist-close     '0)
  ;; Indent privacy keywords at same level as class properties
  ;; (c-set-offset 'inclass #'+cc-c-lineup-inclass)

  (add-hook 'c-mode-common-hook #'(lambda () (modify-syntax-entry ?_ "w")))

  ;; Improve indentation of inline lambdas in C++11
  (advice-add #'c-lineup-arglist :around #'+cc*align-lambda-arglist)

  ;;; Keybindings
  ;; Completely disable electric keys because it interferes with smartparens and
  ;; custom bindings. We'll do this ourselves.
  (setq c-tab-always-indent t
        c-electric-flag nil)
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")"))
    (define-key c-mode-base-map key nil))

  ;; ...and leave it to smartparens
  (after! smartparen
    (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
      ;; (sp-local-pair "<" ">" :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p))
      (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
      ;; Doxygen blocks
      (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
      (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))))

(def-package! cmake-mode
  :mode
  (("/CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

(set!
  :rotate 'c++-mode
  :symbols '(("public" "protected" "private")
             ("class" "struct")))

(def-package! disaster :commands disaster)

(def-package! cuda-mode :mode "\\.cuh?$")

(def-package! opencl-mode :mode "\\.cl$")

(def-package! demangle-mode
  :commands demangle-mode
  :init (add-hook 'llvm-mode-hook #'demangle-mode))

(def-package! clang-format
  :commands clang-format-buffer clang-format)

(defvar +amos/default-include-headers
  '("algorithm" "any" "array" "atomic" "bitset" "cassert" "ccomplex" "cctype" "cerrno"
    "cfenv" "cfloat" "chrono" "cinttypes" "ciso646" "climits" "clocale" "cmath" "codecvt"
    "complex" "complex.h" "condition_variable" "csetjmp" "csignal" "cstdalign" "cstdarg"
    "cstdbool" "cstddef" "cstdint" "cstdio" "cstdlib" "cstring" "ctgmath" "ctime" "cuchar"
    "cwchar" "cwctype" "cxxabi.h" "deque" "exception" "fenv.h" "forward_list" "fstream"
    "functional" "future" "initializer_list" "iomanip" "ios" "iosfwd" "iostream" "istream"
    "iterator" "limits" "list" "locale" "map" "math.h" "memory" "mutex" "new" "numeric"
    "optional" "ostream" "queue" "random" "ratio" "regex" "scoped_allocator" "set"
    "shared_mutex" "sstream" "stack" "stdexcept" "stdlib.h" "streambuf" "string" "string_view"
    "system_error" "tgmath.h" "thread" "tuple" "type_traits" "typeindex" "typeinfo" "unordered_map"
    "unordered_set" "utility" "valarray" "variant" "vector" "auto_ptr.h" "backward_warning.h"
    "binders.h" "hash_fun.h" "hash_map" "hash_set" "hashtable.h" "strstream" "adxintrin.h"
    "altivec.h" "ammintrin.h" "arm_acle.h" "arm_neon.h" "armintr.h" "avx2intrin.h" "avx512bwintrin.h"
    "avx512cdintrin.h" "avx512dqintrin.h" "avx512erintrin.h" "avx512fintrin.h" "avx512ifmaintrin.h"
    "avx512ifmavlintrin.h" "avx512pfintrin.h" "avx512vbmiintrin.h" "avx512vbmivlintrin.h"
    "avx512vlbwintrin.h" "avx512vlcdintrin.h" "avx512vldqintrin.h" "avx512vlintrin.h" "avx512vpopcntdqintrin.h"
    "avxintrin.h" "bmi2intrin.h" "bmiintrin.h" "clflushoptintrin.h" "clzerointrin.h" "cpuid.h"
    "cuda_wrappers" "emmintrin.h" "f16cintrin.h" "fcntl.h" "float.h" "fma4intrin.h" "fmaintrin.h" "fxsrintrin.h"
    "htmintrin.h" "htmxlintrin.h" "ia32intrin.h" "immintrin.h" "intrin.h" "inttypes.h" "iso646.h"
    "limits.h" "lwpintrin.h" "lzcntintrin.h" "mm3dnow.h" "mm_malloc.h" "mmintrin.h" "module.modulemap"
    "msa.h" "mwaitxintrin.h" "nmmintrin.h" "opencl-c.h" "pkuintrin.h" "pmmintrin.h" "popcntintrin.h"
    "prfchwintrin.h" "rdseedintrin.h" "rtmintrin.h" "s390intrin.h" "sanitizer" "shaintrin.h" "smmintrin.h"
    "stdalign.h" "stdarg.h" "stdatomic.h" "stdbool.h" "stddef.h" "stdint.h" "stdnoreturn.h" "tbmintrin.h"
    "tgmath.h" "tmmintrin.h" "unwind.h" "vadefs.h" "varargs.h" "vecintrin.h" "wmmintrin.h" "x86intrin.h"
    "xmmintrin.h" "xopintrin.h" "xray" "xsavecintrin.h" "xsaveintrin.h" "xsaveoptintrin.h" "xsavesintrin.h"
    "xtestintrin.h" "unistd.h" "libaio.h" "numa.h"))

(defun +amos/add-include (h &rest others)
  "Add an #include line for `h' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (dolist (header (cons h others))
    (let ((incl (format "#include <%s>" header)))
      (save-excursion
        (if (search-backward incl nil t)
            nil
          (when (search-backward "#include" nil 'stop-at-top)
            (forward-line)
            (beginning-of-line))
          (insert incl)
          (newline))))))

(defun +amos/ivy-add-include ()
  (interactive)
  (ivy-read "Include: "
            (append
             +amos/default-include-headers
             (split-string
              (shell-command-to-string "(cd /usr/local/include ; find . -type f ; cd /usr/include ; find ./sys -type f) | sed 's=^./=='")))
            :action #'+amos/add-include))
