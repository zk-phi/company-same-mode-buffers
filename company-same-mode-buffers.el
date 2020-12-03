;; company backend to complete symbols from same mode buffers with
;; flex matching and history

(require 'company)

(defcustom company-same-mode-buffers-case-fold nil
  "When non-nil, candidates are searched with case-fold-search."
  :group 'company-same-mode-buffers
  :type 'boolean)

(defcustom company-same-mode-buffers-partial-match-subword t
  "When non-nil, `aCS' matches `aCamelcaseSymbol'. Works only
when `company-same-mode-buffers-case-fold' is non-nil."
  :group 'company-same-mode-buffers
  :type 'boolean)

(defcustom company-same-mode-buffers-matchers
  '(company-same-mode-buffers-matcher-basic
    company-same-mode-buffers-matcher-partial
    company-same-mode-buffers-matcher-flex)
  "List of company-same-mode-buffers matchers. Works like
  `completion-styles'. Internally a matcher is a function which
  builds query (list of query constructs) from prefix string."
  :group 'company-same-mode-buffers
  :type '(list function))

(defcustom company-same-mode-buffers-minimum-word-length 4
  "Minimum length of words to save."
  :group 'company-same-mode-buffers
  :type 'integer)

(defcustom company-same-mode-buffers-history-file nil
  "When non-nil, save history to a file in order to share
  completion candidates across sessions."
  :group 'company-same-mode-buffers
  :type 'string)

(defcustom company-same-mode-buffers-history-size 20
  "# of same mode buffers (per mode) saved to the history file."
  :group 'company-same-mode-buffers
  :type 'integer)

;; ---- matchers

(defun company-same-mode-buffers-query-construct-any-followed-by (str)
  (cons "\\(?:\\sw\\|\\s_\\)*?" (regexp-quote str)))

(defun company-same-mode-buffers-query-construct-any-word-followed-by (str)
  (cons "\\sw*?" (regexp-quote str)))

(defun company-same-mode-buffers-query-construct-any-lower-followed-by (str)
  (cons "[a-z]*?" (regexp-quote str)))

(defun company-same-mode-buffers-query-construct-exact (str)
  (cons "" (regexp-quote str)))

(defun company-same-mode-buffers-matcher-basic (prefix)
  "A matcher that matches symbols start with PREFIX."
  (mapcar (lambda (s) (company-same-mode-buffers-query-construct-exact s))
          (split-string prefix "" t)))

(defun company-same-mode-buffers-matcher-partial (prefix)
  "A matcher that matches multiword symbols like completoin-style
`partial-completion'."
  (mapcar (lambda (s)
            (cond ((= (char-syntax (aref s 0)) ?_) ; word-separator
                   (company-same-mode-buffers-query-construct-any-word-followed-by s))
                  ((and (<= ?A (aref s 0) ?Z)
                        (not company-same-mode-buffers-case-fold)
                        company-same-mode-buffers-partial-match-subword)
                   (company-same-mode-buffers-query-construct-any-lower-followed-by s))
                  (t
                   (company-same-mode-buffers-query-construct-exact s))))
          (split-string prefix "" t)))

(defun company-same-mode-buffers-matcher-flex (prefix)
  "A matcher of in-order subset of characters like
completion-style `flex'."
  (mapcar (lambda (s) (company-same-mode-buffers-query-construct-any-followed-by s))
          (split-string prefix "" t)))

;; ---- internals

(defvar company-same-mode-buffers-caches-by-major-mode (make-hash-table :test 'eq))
(defvar-local company-same-mode-buffers-cache nil) ; (DIRTY . (SYMBOL ...))

(defun company-same-mode-buffers-save-history ()
  (when company-same-mode-buffers-history-file
    (company-same-mode-buffers-update-cache-other-buffers)
    (company-same-mode-buffers-update-cache (current-buffer))
    ;; drop old entries
    (mapc (lambda (history)
            (let ((pair (nthcdr (1- company-same-mode-buffers-history-size) history)))
              (when pair
                (setcdr pair nil))))
          (hash-table-values company-same-mode-buffers-caches-by-major-mode))
    (with-temp-buffer
      (prin1 company-same-mode-buffers-caches-by-major-mode (current-buffer))
      (write-file company-same-mode-buffers-history-file))))

(defun company-same-mode-buffers-load-history ()
  (when (and company-same-mode-buffers-history-file
             (file-exists-p company-same-mode-buffers-history-file))
    (with-temp-buffer
      (insert-file-contents company-same-mode-buffers-history-file)
      (setq company-same-mode-buffers-caches-by-major-mode (read (current-buffer))))))

(defun company-same-mode-buffers-update-cache (buffer)
  "Put all symbols in the buffer into
`company-same-mode-buffers-cache'."
  (with-current-buffer buffer
    (unless company-same-mode-buffers-cache
      (setq company-same-mode-buffers-cache (cons t nil))
      (push company-same-mode-buffers-cache
            (gethash major-mode company-same-mode-buffers-caches-by-major-mode)))
    (when (car company-same-mode-buffers-cache)
      (setcdr company-same-mode-buffers-cache
              (company-same-mode-buffers-search-current-buffer
               (concat "\\(:?+\\sw\\|\\s_\\)\\{"
                       (number-to-string company-same-mode-buffers-minimum-word-length)
                       ",\\}")))
      (setcar company-same-mode-buffers-cache nil))))

(defun company-same-mode-buffers-invalidate-cache (&rest _)
  (when (and company-same-mode-buffers-cache (not (car company-same-mode-buffers-cache)))
    (setcar company-same-mode-buffers-cache t)))

(defun company-same-mode-buffers-search-current-buffer (regex &optional cursor)
  "Search REGEX in the buffer and return all matching results."
  (let (lst)
    (when cursor
      (save-excursion
        (goto-char cursor)
        (while (search-backward-regexp regex nil t)
          (push (match-string-no-properties 0) lst))))
    (save-excursion
      (goto-char (or cursor (point-min)))
      (while (search-forward-regexp regex nil t)
        (push (match-string-no-properties 0) lst)))
    lst))

(defun company-same-mode-buffers-update-cache-other-buffers ()
  "Update cache for all buffers except for the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (company-same-mode-buffers-update-cache buf))))

(defun company-same-mode-buffers-all-completions (regex prefix)
  "Collect candidates from the current buffer by searching with
REGEX, and other buffers by filtering the chaches with REGEX."
  (delq nil
        (apply 'nconc
               (mapcar (lambda (s) (and (not (string= s prefix)) s))
                       (company-same-mode-buffers-search-current-buffer regex (point)))
               (mapcar (lambda (cache)
                         (mapcar (lambda (s) (and (string-match regex s) s)) (cdr cache)))
                       (gethash major-mode
                                company-same-mode-buffers-caches-by-major-mode)))))

(defun company-same-mode-buffers-query-to-regex (lst)
  "Make a regex from a query (list of `query-construct-*' s)."
  (concat "\\_<"
          (mapconcat (lambda (pair) (concat (car pair) "\\(" (cdr pair) "\\)")) lst "")
          "\\(?:\\s_\\|\\sw\\)*"))

(defun company-same-mode-buffers-plist-to-alist (plist)
  "Convert plist to alist."
  (and plist
       (cons (cons (car plist) (cadr plist))
             (company-same-mode-buffers-plist-to-alist (cddr plist)))))

(defun company-same-mode-buffers-fuzzy-all-completions (prefix)
  (let ((case-fold-search company-same-mode-buffers-case-fold)
        (matchers company-same-mode-buffers-matchers)
        res)
    (while (and (null res) matchers)
      (setq res (company-same-mode-buffers-all-completions
                 (company-same-mode-buffers-query-to-regex (funcall (pop matchers) prefix))
                 prefix)))
    res))

(defun company-same-mode-buffers-make-match-data (candidate prefix)
  (let ((case-fold-search company-same-mode-buffers-case-fold)
        (matchers company-same-mode-buffers-matchers)
        res)
    (while (and (null res) matchers)
      (when (string-match
             (company-same-mode-buffers-query-to-regex (funcall (pop matchers) prefix))
             candidate)
        (setq res (company-same-mode-buffers-plist-to-alist (cddr (match-data t))))))
    res))

(defun company-same-mode-buffers (command &optional arg &rest ignored)
  "like `company-dabbrev' but flex."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-same-mode-buffers-same-mode-buffers))
    (prefix (and (derived-mode-p 'prog-mode)
                 (company-grab-symbol)))
    (no-cache t)
    (duplicates t)
    (match (company-same-mode-buffers-make-match-data arg company-prefix))
    (candidates (progn
                  (company-same-mode-buffers-update-cache-other-buffers)
                  (company-same-mode-buffers-fuzzy-all-completions arg)))))

(defun company-same-mode-buffers-initialize ()
  (add-hook 'after-change-functions 'company-same-mode-buffers-invalidate-cache)
  (add-hook 'kill-emacs-hook 'company-same-mode-buffers-save-history)
  (company-same-mode-buffers-load-history)
  nil)

(provide 'company-same-mode-buffers)
