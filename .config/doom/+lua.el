(defun lua-busted-fuckups-fix ()
  (save-excursion
    (lua-forward-line-skip-blanks 'back)
    (let* ((current-indentation (current-indentation))
           (line (thing-at-point 'line t))
           (busted-p (s-matches?
                      (rx (+ bol (* space)
                             (or "context" "describe" "it" "setup" "teardown")
                             "("))
                      line)))
      (when busted-p
        (+ current-indentation lua-indent-level)))))
(defun +amos*lua-calculate-indentation-override (old-function &rest arguments)
  (or (lua-busted-fuckups-fix)
      (apply old-function arguments)))
(advice-add #'lua-calculate-indentation-override :around #'+amos*lua-calculate-indentation-override)

(defun lua-find-matching-token-in-line (found-token found-pos token-type &optional direction)
  (let ((line (line-number-at-pos))
        ;; If we are on a middle token, go backwards. If it is a middle-or-open,
        ;; go forwards.
        (search-direction
         (or direction
             (if (or (eq token-type 'open)
                     (eq token-type 'middle-or-open))
                 'forward
               'backward)
             'backward)))
    (save-excursion
      ;; This is required since lua-find-matching-token-word needs point to be
      ;; at the beginning of the keyword.
      (goto-char found-pos)
      (let ((found-match (lua-find-matching-token-word found-token search-direction)))
        (when (and found-match (= line (line-number-at-pos)))
          (point))))))

(defun lua-resolve-token-type (found-token found-pos)
  "Get resolved token type.
 If token type is 'middle-or-open, determine which one it is and
 return it."
  (save-excursion
    (let ((token-type (lua-get-token-type (lua-get-block-token-info found-token))))
      (if (not (eq token-type 'middle-or-open))
          token-type
        (goto-char found-pos)
        (if (not (lua-find-matching-token-word found-token 'backward))
            'open
          'middle)))))

(defun lua-line-indent-impact-current (&optional bound)
  "Calculate how much current line impacts indentation of current line.
 `bound' is set to `line-end-position' by default."
  ;; TODO: Optimization idea: sum all closers and matched-in-line openers until
  ;; an unmatched-in-line opener is met.
  (unless bound
    (setq bound (line-end-position)))
  (save-excursion
    (back-to-indentation)
    ;; Check first token.
    (if (or (lua-comment-or-string-p)
            (not (looking-at lua-indentation-modifier-regexp)))
        0
      (let ((token-type (lua-resolve-token-type (match-string 0) (match-beginning 0))))
        (cond
         ((eq token-type 'middle)
          (- lua-indent-level))
         ((eq token-type 'close)
          ;; Loop over all tokens in line.
          (back-to-indentation)
          (let ((shift 0))
            (while (lua-find-regexp 'forward lua-indentation-modifier-regexp bound)
              (let ((found-token (match-string 0))
                    (found-pos (match-beginning 0)))
                (setq token-type (lua-resolve-token-type found-token found-pos))
                ;; Only if 'close and unmatched.
                (when (and (eq token-type 'close)
                           (not (lua-find-matching-token-in-line found-token found-pos token-type)))
                  (setq shift (- shift lua-indent-level)))))
            (max -4 shift)))
         (t 0))))))

(defun lua-line-indent-impact-next (&optional bound)
  "Calculate how much current line impacts indentation of next line.
 `bound' is set to `line-end-position' by default."
  (unless bound
    (setq bound (line-end-position)))
  (save-excursion
    (back-to-indentation)
    (let ((shift 0)
          (add 0)
          (first-token-type)
          (token-type))
      ;; Shift if first token is 'middle.
      (when (and (not (lua-comment-or-string-p))
                 (looking-at lua-indentation-modifier-regexp)
                 (eq 'middle (setq first-token-type (lua-resolve-token-type (match-string 0) (match-beginning 0)))))
        (setq shift (+ shift lua-indent-level)))
      ;; Loop over all tokens in line.
      (while (lua-find-regexp 'forward lua-indentation-modifier-regexp bound)
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (setq token-type (lua-resolve-token-type found-token found-pos))
          ;; Only if unmatched 'open and unmatched 'close if first token was not
          ;; 'close.
          (unless (lua-find-matching-token-in-line found-token found-pos token-type)
            (cond
             ((eq token-type 'open)
              (setq add (+ add lua-indent-level)))
             ((and (eq token-type 'close) (not (eq first-token-type 'close)))
              (setq add (- add lua-indent-level)))
             (t 0)))))
      (+ shift (min lua-indent-level add)))))

(defun +amos-lua-calculate-indentation-a (&optional parse-start)
  "Return appropriate indentation for current line as Lua code."
  ;; Algorithm: indentation is computed from current line and previous line.
  ;; Current line:
  ;; -1 on every unmatched closer if first token is a closer
  ;; -1 if first token is 'middle
  ;; Previous line:
  ;; +1 on every unmatched opener
  ;; +1 if first token is a 'middle
  ;; -1 on every unmatched closer if first token is not a closer (if first token
  ;;  is a 'middle, then first unmatched closer is actually closing the middle).
  ;;
  ;; To this we add
  ;; + previous line indentation
  ;; +1 if previous line is not a continuation and current-line is
  ;; -1 if previous line is a continuation and current-line is not
  (save-excursion
    (back-to-indentation)
    (let* ((continuing-p (lua-is-continuing-statement-p))
           (cur-line-begin-pos (line-beginning-position))
           (close-factor (lua-line-indent-impact-current)))

      (if (lua-forward-line-skip-blanks 'back)
          (+ (current-indentation)
             (lua-line-indent-impact-next cur-line-begin-pos)
             close-factor
             ;; Previous line is a continuing statement, but not current.
             (if (and (lua-is-continuing-statement-p) (not continuing-p))
                 (- lua-indent-level)
               0)
             ;; Current line is a continuing statement, but not previous.
             (if (and (not (lua-is-continuing-statement-p)) continuing-p)
                 lua-indent-level
               0))
        ;; If there's no previous line, indentation is 0.
        0))))
(advice-add #'lua-calculate-indentation :override #'+amos-lua-calculate-indentation-a)
