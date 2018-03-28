;;; private/email/config.el -*- lexical-binding: t; -*-

(defvar +email-mu4e-mail-path "~/Mail"
  "The directory path of mu's maildir.")

;;
;; Config
;;

(def-setting! :email (label letvars &optional default-p)
  "Registers an email address for mu4e. The LABEL is a string. LETVARS are a
list of cons cells (VARIABLE . VALUE) -- you may want to modify:

 + `user-full-name' (this or the global `user-full-name' is required)
 + `user-mail-address' (required)
 + `smtpmail-smtp-user' (required for sending mail from Emacs)

OPTIONAL:
 + `mu4e-sent-folder'
 + `mu4e-drafts-folder'
 + `mu4e-trash-folder'
 + `mu4e-refile-folder'
 + `mu4e-compose-signature'

DEFAULT-P is a boolean. If non-nil, it marks that email account as the
default/fallback account."
  `(after! mu4e
     (let ((account-vars ,letvars))
       (when-let (address (cdr (assq 'user-mail-address account-vars)))
         (cl-pushnew address mu4e-user-mail-address-list :test #'equal))
       (let ((context (make-mu4e-context
                       :name ,label
                       :enter-func (lambda () (mu4e-message "Switched to %s" ,label))
                       :leave-func (lambda () (mu4e-clear-caches))
                       :match-func
                       (lambda (msg)
                         (when msg
                           (string-prefix-p (format "/%s" ,label)
                                            (mu4e-message-field msg :maildir))))
                       :vars ,letvars)))
         (push context mu4e-contexts)
         ,(when default-p
            `(setq-default mu4e-context-current context))))))

;;
;; Plugins
;;

(def-package! mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands (mu4e mu4e-compose-new browse-url-mail)
  :config
  (setq
   mail-user-agent 'mu4e-user-agent
   message-kill-buffer-on-exit t
   message-send-mail-function 'message-send-mail-with-sendmail
   message-sendmail-envelope-from 'header
   mu4e-attachment-dir "~/Downloads"
   mu4e-compose-context-policy 'ask-if-none
   mu4e-compose-dont-reply-to-self t
   mu4e-compose-format-flowed t
   mu4e-compose-signature-auto-include t
   mu4e-confirm-quit nil
   mu4e-context-policy 'pick-first
   mu4e-drafts-folder "/drafts"
   mu4e-enable-notifications t
   mu4e-get-mail-command "fmail.sh"
   mu4e-hide-index-messages t
   mu4e-maildir +email-mu4e-mail-path
   mu4e-refile-folder "/archive"
   mu4e-sent-folder "/sent"
   mu4e-split-view 'vertical
   mu4e-trash-folder "/trash"
   mu4e-update-interval nil
   mu4e-use-fancy-chars t
   mu4e-view-image-max-width 800
   mu4e-view-show-addresses t
   mu4e-view-show-images t
   sendmail-program "msmtp"
   mu4e-completing-read-function (cond ((featurep! :completion ivy) #'ivy-completing-read)
                                       ((featurep! :completion helm) #'completing-read)
                                       (t #'ido-completing-read))
   mu4e-maildir-shortcuts '(("/drafts"                    . ?d)
                            ("/sent"                      . ?s)
                            ("/archive"                   . ?a)
                            ("/trash"                     . ?t)
                            ("/inbox"                     . ?j))
   mu4e-headers-fields '((:account . 12)
                         (:human-date . 12)
                         (:flags . 4)
                         (:from . 25)
                         (:subject))
   mu4e-bookmarks `(("\\\\Inbox" "Inbox" ?i)
                    ("\\\\Draft" "Drafts" ?d)
                    ("flag:unread AND \\\\Inbox" "Unread messages" ?u)
                    ("flag:flagged" "Starred messages" ?s)
                    ("date:today..now" "Today's messages" ?t)
                    ("date:7d..now" "Last 7 days" ?w)
                    ("mime:image/*" "Messages with images" ?p)))

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)


  ;; In my workflow, emails won't be moved at all. Only their flags/labels are
  ;; changed. Se we redefine the trash and refile marks not to do any moving.
  ;; However, the real magic happens in `+email|gmail-fix-flags'.
  ;;
  ;; Gmail will handle the rest.
  (setq mu4e-marks (assq-delete-all 'trash mu4e-marks))
  (push '(trash :char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                :action
                (lambda (docid msg target)
                  (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N")))
        mu4e-marks)

  ;; Refile will be my "archive" function.
  (setq mu4e-marks (assq-delete-all 'refile mu4e-marks))
  (push '(refile :char ("r" . "▶")
                 :prompt "refile"
                 :show-target (lambda (target) "archive")
                 :action
                 (lambda (docid msg target)
                   (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N")))
        mu4e-marks)

  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (defun +email|gmail-fix-flags (mark msg)
    (cond ((eq mark 'trash) (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
          ((eq mark 'refile) (mu4e-action-retag-message msg "-\\Inbox"))
          ((eq mark 'flag) (mu4e-action-retag-message msg "+\\Starred"))
          ((eq mark 'unflag) (mu4e-action-retag-message msg "-\\Starred"))))
  (add-hook 'mu4e-mark-execute-pre-hook #'+email|gmail-fix-flags)

  ;; Refresh the current view after marks are executed
  (defun +email*refresh (&rest _) (mu4e-headers-rerun-search))
  (advice-add #'mu4e-mark-execute-all :after #'+email*refresh)

  (when (featurep! :feature spellcheck)
    (add-hook! 'mu4e-compose-mode-hook #'flyspell-mode))

  (add-hook! 'mu4e-compose-mode-hook (setq fill-column 80))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  (after! evil
    (cl-loop for str in '((mu4e-main-mode . normal)
                          (mu4e-view-mode . normal)
                          (mu4e-headers-mode . normal)
                          (mu4e-compose-mode . normal)
                          (mu4e~update-mail-mode . normal))
             do (evil-set-initial-state (car str) (cdr str)))

    (setq mu4e-view-mode-map (make-sparse-keymap)
          ;; mu4e-compose-mode-map (make-sparse-keymap)
          mu4e-headers-mode-map (make-sparse-keymap)
          mu4e-main-mode-map (make-sparse-keymap))

    (map! (:map (mu4e-main-mode-map mu4e-headers-mode-map mu4e-view-mode-map)
            :n "C" #'mu4e-compose-new
            :n "F" #'mu4e-compose-forward
            :n "R" #'mu4e-compose-reply
            :n "C" #'mu4e-compose-new
            :n "E" #'mu4e-compose-edit
            :leader
            :n "," #'mu4e-context-switch
            :n "." #'mu4e-headers-search-bookmark
            :n ">" #'mu4e-headers-search-bookmark-edit
            :n "/" #'mu4e~headers-jump-to-maildir)

          (:map mu4e-main-mode-map
            :n "q"   #'mu4e-quit
            :n "u"   #'mu4e-update-index
            :n "U"   #'mu4e-update-mail-and-index
            :n "j"   #'mu4e~headers-jump-to-maildir
            :n "c"   #'+email/compose
            :n "b"   #'mu4e-headers-search-bookmark)

          (:map mu4e-headers-mode-map
            "C-c C-u" #'mu4e-update-mail-and-index
            "C-l"     #'mu4e-update-index
            :n "q"    #'mu4e~headers-quit-buffer
            :n "s"    #'mu4e-headers-search-edit
            :n "S"    #'mu4e-headers-search-narrow
            :n "RET"  #'mu4e-headers-view-message
            :n "U"    #'mu4e-mark-unmark-all
            :nv "u"   #'mu4e-headers-mark-for-unmark
            :n "v"    #'evil-visual-line
            :nv "D"   #'+email/mark
            :nv "d"   #'+email/mark
            :nv "="   #'+email/mark
            :nv "-"   #'+email/mark
            :nv "+"   #'+email/mark
            :nv "!"   #'+email/mark
            :nv "?"   #'+email/mark
            :nv "r"   #'+email/mark
            :nv "m"   #'+email/mark
            :nv "a"   #'mu4e-headers-action
            :n "x"    #'mu4e-mark-execute-all

            :n "]"  #'mu4e-headers-next-unread
            :n "["  #'mu4e-headers-prev-unread

            (:localleader
              :n "s" 'mu4e-headers-change-sorting
              :n "t" 'mu4e-headers-toggle-threading
              :n "r" 'mu4e-headers-toggle-include-related

              :n "%" #'mu4e-headers-mark-pattern
              :n "t" #'mu4e-headers-mark-subthread
              :n "T" #'mu4e-headers-mark-thread))

          (:map mu4e-view-mode-map
            :n "o" #'link-hint-open-link
            :n "f" #'link-hint-open-link
            :n "a" #'mu4e-view-action
            :n "q" #'mu4e~view-quit-buffer

            :n "C-k" #'mu4e-view-headers-prev
            :n "C-j" #'mu4e-view-headers-next
            :n "[" #'mu4e-view-headers-prev-unread
            :n "]" #'mu4e-view-headers-next-unread

            (:localleader
              :n "%" #'mu4e-view-mark-pattern
              :n "t" #'mu4e-view-mark-subthread
              :n "T" #'mu4e-view-mark-thread

              :n "d" #'mu4e-view-mark-for-trash
              :n "r" #'mu4e-view-mark-for-refile
              :n "m" #'mu4e-view-mark-for-move))

          (:map mu4e~update-mail-mode-map
            :n "q" #'mu4e-interrupt-update-mail)))

  (setq mu4e-user-mail-address-list '("amosbird@gmail.com" "zhengtianqi@software.ict.ac.cn" "zhengtianqi@golaxy.cn"))

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "gmail"
             :enter-func (lambda () (mu4e-message "Switch to the gmail context"))
             ;; :leave-func (lambda () (mu4e-clear-caches))
             :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "amosbird@gmail.com")))
             :vars '(( user-mail-address       . "amosbird@gmail.com" )
                     ( user-full-name          . "Amos Bird" )
                     ( mu4e-compose-signature  . "Amos Bird\namosbird@gmail.com\n")))
           ,(make-mu4e-context
             :name "ict"
             :enter-func (lambda () (mu4e-message "Switch to the ict context"))
             ;; :leave-func (lambda () (mu4e-clear-caches))
             :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "zhengtianqi@software.ict.ac.cn")))
             :vars '(( user-mail-address       . "zhengtianqi@software.ict.ac.cn" )
                     ( user-full-name          . "郑天祺" )
                     ( mu4e-compose-signature  . "郑天祺\n中科院计算所网络数据实验室\n")))
           ,(make-mu4e-context
             :name "work"
             :enter-func (lambda () (mu4e-message "Switch to the golaxy context"))
             ;; :leave-func (lambda () (mu4e-clear-caches))
             :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg :to "zhengtianqi@golaxy.cn")))
             :vars '(( user-mail-address       . "zhengtianqi@golaxy.cn" )
                     ( user-full-name          . "郑天祺" )
                     ( mu4e-compose-signature  . "郑天祺\n中科天玑数据科技股份有限公司\n"))))))

(def-package! mu4e-maildirs-extension
  :after mu4e
  :config
  (mu4e-maildirs-extension-load)
  (add-hook #'mu4e-view-mode-hook (lambda () (hl-line-mode +1))))

;; (def-package! notmuch
;;   :init
;;   (add-to-list 'auto-mode-alist '("amosbird@gmail.com" . notmuch-message-mode))
;;   :config
;;   (require 'notmuch-company)
;;   (require 'notmuch-mua))


;; (def-package! org-mu4e
;;   :commands org-mu4e-compose-org-mode
;;   :init (add-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
;;   :config
;;   (setq org-mu4e-link-query-in-headers-mode nil
;;         org-mu4e-convert-to-html t)

;;   ;; Only render to html once. If the first send fails for whatever reason,
;;   ;; org-mu4e would do so each time you try again.
;;   (add-hook! 'message-send-hook
;;     (setq-local org-mu4e-convert-to-html nil)))

(defun +amos*mu4e-view-verify-msg-popup (&optional msg)
  "Pop-up a little signature verification window for (optional) MSG
or message-at-point."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (path (mu4e-message-field msg :path))
         (cmd (format "%s verify --verbose %s %s"
                      mu4e-mu-binary
                      (shell-quote-argument path)
                      (if mu4e-decryption-policy
                          "--decrypt --use-agent"
                        "")))
         (output (shell-command-to-string cmd)))
    "Output to the temp buffer."
    (let ((buffer-name " *mu4e-verify*"))
      (with-output-to-temp-buffer buffer-name
        (let ((inhibit-read-only t))
          (set-buffer buffer-name)
          (insert output)
          (goto-char (point-min)))
        (setq buffer-read-only t)))))
(advice-add #'mu4e-view-verify-msg-popup :override #'+amos*mu4e-view-verify-msg-popup)

;;;###autoload
(defun +amos/dired-add-attachments (arg)
  "Attach the items from copy ring to one mu4e-compose buffer.

With raw prefix argument \\[universal-argument], do not remove
the selection from the stack so it can be copied again.

With numeric prefix argument, attach the n-th selection from the
copy ring."
  (interactive "P")
  (catch 'loop
    (dolist (buf (buffer-list) ret)
      (when (eq 'mu4e-compose-mode (buffer-local-value 'major-mode buf))
        (switch-to-buffer buf)
        (throw 'loop buf))))
  (save-excursion
    (end-of-buffer)
    (let* ((index (if (numberp arg) arg 0))
           (data (ring-ref dired-ranger-copy-ring index))
           (files (cdr data))
           (attached-files 0))
      (--each files (when (and (file-exists-p it)
                               (not (file-directory-p it)))
                      (mail-add-attachment it)
                      (cl-incf attached-files)))
      (unless arg (ring-remove dired-ranger-copy-ring 0))
      (message (format "Attached %d/%d item%s from copy ring."
                       attached-files
                       (length files)
                       (if (> (length files) 1) "s" ""))))))

(defun +amos/dired-save-attachments (&optional msg)
  "Offer to save multiple email attachments from the current message.
Default is to save all messages, [1..n], where n is the number of
attachments.  You can type multiple values separated by space, e.g.
  1 3-6 8
will save attachments 1,3,4,5,6 and 8.

Furthermore, there is a shortcut \"a\" which so means all
attachments, but as this is the default, you may not need it."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (attachstr (mu4e~view-get-attach-num
                     "Attachment number range (or 'a' for 'all')" msg t))
         (count (hash-table-count mu4e~view-attach-map))
         (attachnums (mu4e-split-ranges-to-numbers attachstr count)))
    (let* ((path (concat (mu4e~get-attachment-dir) "/"))
           (attachdir (mu4e~view-request-attachments-dir path)))
      (dolist (num attachnums)
        (let* ((att (mu4e~view-get-attach msg num))
               (fname  (plist-get att :name))
               (index (plist-get att :index))
               (retry t)
               fpath)
          (while retry
            (setq fpath (expand-file-name (concat attachdir fname) path))
            (setq retry
                  (and (file-exists-p fpath)
                       (not (y-or-n-p
                             (mu4e-format "Overwrite '%s'?" fpath))))))
          (mu4e~proc-extract
           'save (mu4e-message-field msg :docid)
           index mu4e-decryption-policy fpath))))))

;; maybe useful

;; (require 'subr-x)

;; ;;my favourite contacts - these will be put at front of list
;; (setq bjm/contact-file "/homeb/bjm/docs/fave-contacts.txt")

;; (defun bjm/read-contact-list ()
;;   "Return a list of email addresses"
;;   (with-temp-buffer
;;     (insert-file-contents bjm/contact-file)
;;     (split-string (buffer-string) "\n" t)))

;; ;; code from https://github.com/abo-abo/swiper/issues/596
;; (defun bjm/counsel-email-action (contact)
;;   (with-ivy-window
;;     (insert contact)))

;; ;; bind comma to launch new search
;; (defvar bjm/counsel-email-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "," 'bjm/counsel-email-more)
;;     map))

;; (defun bjm/counsel-email-more ()
;;   "Insert email address and prompt for another."
;;   (interactive)
;;   (ivy-call)
;;   (with-ivy-window
;;     (insert ", "))
;;   (delete-minibuffer-contents)
;;   (setq ivy-text ""))

;; ;; ivy contacts
;; (defun bjm/ivy-select-and-insert-contact (&optional start)
;;   (interactive)
;;   ;; make sure mu4e contacts list is updated - I was having
;;   ;; intermittent problems that this was empty but couldn't see why
;;   (mu4e~request-contacts)
;;   (let ((eoh ;; end-of-headers
;;          (save-excursion
;;            (goto-char (point-min))
;;            (search-forward-regexp mail-header-separator nil t)))
;;         ;; append full sorted contacts list to favourites and delete duplicates
;;         (contacts-list
;;          (delq nil (delete-dups (append (bjm/read-contact-list) (mu4e~sort-contacts-for-completion (hash-table-keys mu4e~contacts)))))))

;;     ;; only run if we are in the headers section
;;     (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
;;       (let* ((end (point))
;;            (start
;;             (or start
;;                 (save-excursion
;;                   (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
;;                   (goto-char (match-end 0))
;;                   (point))))
;;            (initial-input (buffer-substring-no-properties start end)))

;;       (delete-region start end)

;;       (ivy-read "Contact: "
;;                 contacts-list
;;                 :re-builder #'ivy--regex
;;                 :sort nil
;;                 :initial-input initial-input
;;                 :action 'bjm/counsel-email-action
;;                 :keymap bjm/counsel-email-map)))))
