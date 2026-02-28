;;; cc-playground.el --- Local C/C++ playground for short snippets.

;; Copyright (C) 2017-2018 Amos Bird
;;   ___                       ______ _         _
;;  / _ \                      | ___ (_)       | |
;; / /_\ \_ __ ___   ___  ___  | |_/ /_ _ __ __| |
;; |  _  | '_ ` _ \ / _ \/ __| | ___ \ | '__/ _` |
;; | | | | | | | | | (_) \__ \ | |_/ / | | | (_| |
;; \_| |_/_| |_| |_|\___/|___/ \____/|_|_|  \__,_|

;; Author: Amos Bird <amosbird@gmail.com>
;; URL: https://github.com/amosbird/cc-playground
;; Keywords: tools, c/c++
;; Version: 1.1
;; Package-Requires: ((consult) (emacs "26"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Local playground for C/C++ programs.
;; `M-x cc-playground` and type you code then make&run it with `C-Return`.

;;

;;; Code:

(require 'compile)
(require 'time-stamp)
(require 'dired)
(eval-when-compile (require 'cl-lib))
(declare-function consult--read "consult")

(defun cc-playground--ensure-consult ()
  "Ensure consult is loaded before using consult--read."
  (require 'consult))
(declare-function evil-open-below "evil-commands")
(declare-function evil-insert "evil-commands")

(defgroup cc-playground nil
  "Options specific to C++ Playground."
  :group 'c)

(defcustom cc-playground-ask-file-name nil
  "Non-nil means we ask for a name for the snippet.

By default it will be created as snippet.cpp"
  :type 'boolean
  :group 'cc-playground)

(defcustom cc-playground-confirm-deletion t
  "Non-nil prompts confirmation on the snippet deletion with `cc-playground-rm'.

By default confirmation required."
  :type 'boolean
  :group 'cc-playground)

(defcustom cc-playground-basedir "~/cc-playground"
  "Base directory for playground snippets."
  :type 'file
  :group 'cc-playground)

(defcustom cc-playground-hook nil
  "Hook when entering playground."
  :type 'hook
  :group 'cc-playground)

(defcustom cc-playground-rm-hook nil
  "Hook when leaving playground."
  :type 'hook
  :group 'cc-playground)

(defvar cc-debug-command "make deb && ./run debug")
(defvar cc-debug-test-command "make deb_test && ./run debug_test")
(defvar cc-release-command "make rel && ./run")
(defvar cc-release-test-command "make rel_test && ./run test")
(defvar cc-bench-command "make bench && ./run bench")
(defvar cc-leetcode-command "leetcode submit ${PWD}/${LEETCODE_ID}.cpp | head -c 1024")

(defun cc-switch-between-src-and-test ()
  "Switch between src and test file."
  (interactive)
  (let ((name (file-name-base (buffer-file-name))))
    (if (string= name "snippet")
        (find-file (concat default-directory "test.cpp"))
      (if (string= name "test")
          (find-file (concat default-directory "snippet.cpp"))))))

;;;###autoload
(define-minor-mode cc-playground-mode
  "A place for playing with c++ code."
  :init-value nil
  :lighter "Play(C/C++)"
  :keymap '(([C-return] . cc-playground-exec)
            ([M-return] . cc-playground-exec-test)
            ([?\M-\r]   . cc-playground-exec-test)
            ([S-return] . cc-playground-rm)))

(defun cc-playground-snippet-file-name(&optional id)
  (if id
      (concat (cc-playground-snippet-unique-dir-leetcode id) "/snippet.cpp")
    (concat (cc-playground-snippet-unique-dir) "/snippet.cpp")))

(defun cc-playground-run (comm)
  "COMM."
  (if (cc-playground-inside)
      (progn
        (save-buffer t)
        (make-local-variable 'compile-command)
        (pcase comm
          ('exec
           (compile cc-release-command t))
          ('debug
           (compile cc-debug-command t))
          ('test
           (compile cc-release-test-command t))
          ('debug-test
           (compile cc-debug-test-command t))
          ('bench
           (compile cc-bench-command t))
          ('leetcode
           (compile cc-leetcode-command t))))))

(defun cc-playground-exec ()
  "Save the buffer then run clang compiler for executing the code."
  (interactive)
  (cc-playground-run 'exec))

(defun cc-playground-debug ()
  "Save the buffer then run tmuxgdb for debugging the code."
  (interactive)
  (cc-playground-run 'debug))

(defun cc-playground-exec-test ()
  "Save the buffer then run clang compiler for executing the test."
  (interactive)
  (cc-playground-run 'test))

(defun cc-playground-debug-test ()
  "Save the buffer then run tmuxgdb for debugging the test."
  (interactive)
  (cc-playground-run 'debug-test))

(defun cc-playground-bench ()
  "Save the buffer then run clang compiler for executing the test."
  (interactive)
  (cc-playground-run 'bench))

(defun cc-playground-leetcode-submit ()
  "Save the buffer then run clang compiler for executing the test."
  (interactive)
  (if (and (cc-playground-inside) (getenv "LEETCODE_ID"))
      (cc-playground-run 'leetcode)))

(defun cc-playground-leetcode-solution ()
  (interactive)
  (insert (shell-command-to-string "leetcode show $LEETCODE_ID --solution")))

(defun cc-playground-add-or-modify-tag (name)
  "Adding or modifying existing tag of a snippet using NAME."
  (interactive "MTag Name: ")
  (if (cc-playground-inside)
      (let* ((oname (string-trim-right (shell-command-to-string (concat "basename " default-directory))))
             (nn (concat default-directory "../"))
             (l (split-string oname "--")))
        (fundamental-mode) ;; weird bug when renaming directory
        (if (= (length l) 1)
            (dired-rename-file default-directory (concat nn name "--" oname) nil)
          (dired-rename-file default-directory (concat nn name "--" (cadr l)) nil))
        (force-mode-line-update))))

(defun cc-playground--lookup (selected candidates &rest _)
  (concat (cdr (assoc selected candidates)) "/snippet.cpp"))

;;;###autoload
(defun cc-playground-find-snippet ()
  "List all snippets using `consult--read'."
  (interactive)
  (cc-playground--ensure-consult)
  (find-file (consult--read
              (cl-remove-if (lambda (it) (or (string-prefix-p "." (car it))
                                             (string-prefix-p "leetcode--" (car it))))
                            (mapcar (lambda (a) (cons (file-name-nondirectory (car a)) (car a)))
                                    (sort
                                     (directory-files-and-attributes cc-playground-basedir t nil 'nosort)
                                     #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))
              :sort nil
              :lookup #'cc-playground--lookup
              :require-match t
              :prompt "Browse cc snippet: "
              :history t ;; disable history
              :category 'expression
              ))
  )

(defun cc-playground-copy ()
  "Copy a playground to a newly generated folder."
  (interactive)
  (if (cc-playground-inside)
      (let* ((snippet-file-name (cc-playground-snippet-file-name))
             (dst-dir (file-name-directory snippet-file-name)))
        (call-process-shell-command (concat "cp -r . " dst-dir))
        (find-file snippet-file-name)
        (run-hooks 'cc-playground-hook))))

(defconst cc-playground--loaddir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that cc-playground was loaded from.")

;;;###autoload
(defun cc-playground (&optional id)
  "Run playground for C++ language in a new buffer."
  (interactive)
  (let ((snippet-file-name (cc-playground-snippet-file-name id)))
    (let* ((dir-name (concat cc-playground--loaddir "cc-playground-templates"))
           (dst-dir (directory-file-name (file-name-directory snippet-file-name))))
      (call-process-shell-command (format "cp -rL %s %s" dir-name dst-dir))
      (if id
          (progn
            (let ((buffer (find-file-noselect (concat dst-dir ".envrc"))))
              (with-current-buffer buffer
                (save-excursion
                  (goto-char (point-max))
                  (newline)
                  (insert (concat "export LEETCODE_ID=" id))
                  (let ((inhibit-message t))
                    (save-buffer)))))
            (find-file snippet-file-name)
            (make-symbolic-link "snippet.cpp" (concat id ".cpp"))
            (forward-line 8)
            (insert (shell-command-to-string (concat "leetcode show -cx -l cpp " id)))
            (goto-char (point-min))
            (save-buffer))
        (find-file snippet-file-name)
        (forward-line 8)
        (evil-open-below 1)))
    (run-hooks 'cc-playground-hook)))

;;;###autoload
(defun cc-playground-leetcode ()
  (interactive)
  (cc-playground--ensure-consult)
  (let* ((candidates (split-string (shell-command-to-string "leetcode ls -L") "\n" t))
         (line (consult--read candidates
                              :prompt "Leetcode: "
                              :require-match t)))
    (when (string-match "\\[ *\\([0-9]+\\)\\]" line)
      (let* ((id (match-string 1 line))
             (l (cl-remove-if-not (lambda (a) (string-prefix-p (concat "leetcode-" id "--") (car a)))
                                  (mapcar (lambda (a) (cons (file-name-nondirectory (car a)) (car a)))
                                          (sort
                                           (directory-files-and-attributes cc-playground-basedir t "^[^.]" 'nosort)
                                           #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))))
        (if l
            (find-file (concat (cdr (car l)) "/snippet.cpp"))
          (cc-playground id))))))

(defun cc-playground-rm ()
  "Remove files of the current snippet together with directory of this snippet."
  (interactive)
  (if (cc-playground-inside)
      (if (or (not cc-playground-confirm-deletion)
              (y-or-n-p (format "Do you want delete whole snippet dir %s? "
                                (file-name-directory (buffer-file-name)))))
          (progn
            (run-hooks 'cc-playground-rm-hook)
            (save-buffer)
            (let ((dir (file-name-directory (buffer-file-name))))
              (delete-directory dir t t)
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (when (equal default-directory dir)
                    (let (kill-buffer-query-functions)
                      (kill-buffer buffer))))))))
    (message "Won't delete this! Because %s is not under the path %s. Remove the snippet manually!"
             (buffer-file-name) cc-playground-basedir)))

(defun cc-playground-snippet-unique-dir ()
  (let ((dir-name (concat cc-playground-basedir "/"
                          (time-stamp-string "default--%Y-%02m-%02d-%02H%02M%02S"))))
    dir-name))

(defun cc-playground-snippet-unique-dir-leetcode (id)
  (let ((dir-name (concat cc-playground-basedir "/"
                          (time-stamp-string (concat "leetcode-" id "--%Y-%02m-%02d-%02H%02M%02S")))))
    dir-name))

(defun cc-playground-inside ()
  "Is the current buffer is valid cc-playground buffer."
  (if (string-match-p (file-truename cc-playground-basedir) (file-truename (buffer-file-name)))
      (bound-and-true-p cc-playground-mode)))

(defun cc-playground-add-library-link (library)
  "Add an -llibrary line for LIBRARY near bottom of file, avoiding duplicates."
  (interactive "M#Library: ")
  (let ((lib (if (string-suffix-p ".a" library)
                 (format "-l:%s \\" library)
               (format "-l%s \\" library))))
    (with-current-buffer (find-file-noselect (concat default-directory ".dir-locals.el"))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^-l.*\\\\$" nil 'stop-at-the-end 1)
        (end-of-line)
        (newline)
        (insert lib)
        (save-buffer)))))

(defun cc-playground--direnv-get-rcfile ()
  (string-trim (shell-command-to-string "direnv status | awk '/Loaded RC path/{ for (i = 4; i <= NF; i++) print $i}'")))

(defun cc-playground-switch-optimization-flag (flag)
  "Switch optimization flag to FLAG."
  (interactive "c#flags: ")
  (when (cc-playground-inside)
    (if (memq flag (list ?0 ?1 ?2 ?3 ?g))
        (let ((buffer (find-file-noselect (cc-playground--direnv-get-rcfile)))
              (flags (format "-O%c" flag)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "export ccls_cxx_flags=" nil 'stop-at-the-end 1)
              (end-of-line)
              (if (re-search-forward "^-O.*" nil t)
                  (replace-match (regexp-quote flags) nil nil)
                (newline)
                (insert flags))
              (let ((inhibit-message t))
                (save-buffer))
              (message "using optimization flag %c" flag))))
      (user-error (format "unknow optimization flag %c. known flags: 0, 1, 2, 3, g." flag)))))

(defun cc-playground-add-compilation-flags (flags)
  "Add compilation flags FLAGS."
  (interactive "M#flags: ")
  (when (cc-playground-inside)
    (let ((buffer (find-file-noselect (cc-playground--direnv-get-rcfile)))
          (flags (format "%s \\" flags)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "export CXXFLAGS=" nil 'stop-at-the-end 1)
          (end-of-line)
          (newline)
          (insert flags)
          (let ((inhibit-message t))
            (save-buffer)))))))

(defun cc-playground-change-compiler ()
  "Change the compiler."
  (interactive)
  (when (cc-playground-inside)
    (let ((buffer (find-file-noselect (cc-playground--direnv-get-rcfile))))
      (display-buffer buffer '((display-buffer-in-side-window) (side . right) (window-width . 0.5)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "export CXX=" nil 'stop-at-the-end 1)
        (evil-insert 1)))))

(provide 'cc-playground)

;;; cc-playground.el ends here
