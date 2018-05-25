;;; amos-company.el --- description -*- lexical-binding: t; -*-

(require 'company)
(require 'cl-lib)

(setq amos-company-files-exclusions nil)

(defun amos-company-files--directory-files (dir prefix)
  ;; Don't use directory-files. It produces directories without trailing /.
  (condition-case err
      (let ((comp (sort (file-name-all-completions prefix dir)
                        (lambda (s1 s2) (string-lessp (downcase s1) (downcase s2))))))
        (when amos-company-files-exclusions
          (setq comp (amos-company-files--exclusions-filtered comp)))
        (if (equal prefix "")
            (delete "../" (delete "./" comp))
          comp))
    (file-error nil)))

(defun amos-company-files--exclusions-filtered (completions)
  (let* ((dir-exclusions (cl-delete-if-not #'amos-company-files--trailing-slash-p
                                           amos-company-files-exclusions))
         (file-exclusions (cl-set-difference amos-company-files-exclusions
                                             dir-exclusions)))
    (cl-loop for c in completions
             unless (if (amos-company-files--trailing-slash-p c)
                        (member c dir-exclusions)
                      (cl-find-if (lambda (exclusion)
                                    (string-suffix-p exclusion c))
                                  file-exclusions))
             collect c)))

(defvar amos-company-files--regexps
  (let* ((root (if (eq system-type 'windows-nt)
                   "[a-zA-Z]:/"
                 "/"))
         (begin (concat "\\(?:\\.\\{1,2\\}/\\|~/\\|" root "\\)")))
    (list (concat "\"\\(" begin "[^\"\n]*\\)")
          (concat "\'\\(" begin "[^\'\n]*\\)")
          (concat "\\(?:[ \t=]\\|^\\)\\(" begin "[^ \t\n]*\\)"))))

(defun amos-company-files--grab-existing-name ()
  ;; Grab the file name.
  ;; When surrounded with quotes, it can include spaces.
  (let (file dir)
    (and (cl-dolist (regexp amos-company-files--regexps)
           (when (setq file (company-grab-line regexp 1))
             (cl-return file)))
         (amos-company-files--connected-p file)
         (setq dir (file-name-directory file))
         (not (string-match "//" dir))
         (file-exists-p dir)
         file)))

(defun amos-company-files--connected-p (file)
  (or (not (file-remote-p file))
      (file-remote-p file nil t)))

(defun amos-company-files--trailing-slash-p (file)
  ;; `file-directory-p' is very expensive on remotes. We are relying on
  ;; `file-name-all-completions' returning directories with trailing / instead.
  (let ((len (length file)))
    (and (> len 0) (eq (aref file (1- len)) ?/))))

(defvar amos-company-files--completion-cache nil)

(defun amos-company-files--complete (prefix)
  (let* ((dir (file-name-directory prefix))
         (file (file-name-nondirectory prefix))
         (key (list file
                    (expand-file-name dir)
                    (nth 5 (file-attributes dir))))
         (completion-ignore-case read-file-name-completion-ignore-case))
    (unless (amos-company-file--keys-match-p key (car amos-company-files--completion-cache))
      (let* ((candidates (mapcar (lambda (f) (concat dir f))
                                 (amos-company-files--directory-files dir file))))
        (setq amos-company-files--completion-cache candidates)))
    (all-completions prefix
                     (cdr amos-company-files--completion-cache))))

(defun amos-company-file--keys-match-p (new old)
  (and (equal (cdr old) (cdr new))
       (string-prefix-p (car old) (car new))))

;;;###autoload
(defun amos-company-files (command &optional arg &rest ignored)
  "`company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'amos-company-files))
    (prefix (amos-company-files--grab-existing-name))
    (candidates (amos-company-files--complete arg))
    (location (cons (dired-noselect
                     (file-name-directory (directory-file-name arg))) 1))
    (post-completion (when (amos-company-files--trailing-slash-p arg)
                       (delete-char -1)))
    (sorted t)
    (no-cache t)))

;;; amos-company.el ends here
