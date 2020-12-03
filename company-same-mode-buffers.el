;; company backend to complete symbols from same mode buffers with
;; flex matching and history

(require 'company)
(require 'radix-tree)

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
    company-same-mode-buffers-matcher-exact-first-letter-flex-rest
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

(defcustom company-same-mode-buffers-history-store-limit (* 7 24 60 60)
  "How long (in seconds) candidates should be stored in the
  history file."
  :group 'company-same-mode-buffers
  :type 'number)

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

(defun company-same-mode-buffers-matcher-exact-first-letter-flex-rest (prefix)
  "Like `company-same-mode-buffers-matcher-flex' but requires the
first letter to be matched."
  (let ((lst (split-string prefix "" t)))
    (cons (company-same-mode-buffers-query-construct-exact (car lst))
          (mapcar (lambda (s) (company-same-mode-buffers-query-construct-any-followed-by s))
                  (cdr lst)))))

(defun company-same-mode-buffers-matcher-flex (prefix)
  "A matcher of in-order subset of characters like
completion-style `flex'."
  (mapcar (lambda (s) (company-same-mode-buffers-query-construct-any-followed-by s))
          (split-string prefix "" t)))

(defun company-same-mode-buffers-query-to-regex (lst)
  "Make a regex from a query (list of `query-construct-*' s)."
  (concat "\\_<"
          (mapconcat (lambda (pair) (concat (car pair) "\\(" (cdr pair) "\\)")) lst "")
          "\\(?:\\s_\\|\\sw\\)*"))

;; ---- utils

(defun company-same-mode-buffers-search-current-buffer (regex &optional cursor)
  "Search REGEX in the buffer and return all matching
results. When CURSOR is specified, region before cursor and after
cursor are searched separately. In addition, the symbol just
before the cursor is skipped."
  (let (lst)
    (when cursor
      (save-excursion
        (goto-char cursor)
        (when (looking-back "\\_>" (1- (point)))
          (search-backward-regexp "\\_<" nil t))
        (while (search-backward-regexp regex nil t)
          (push (match-string-no-properties 0) lst))))
    (save-excursion
      (goto-char (or cursor (point-min)))
      (while (search-forward-regexp regex nil t)
        (push (match-string-no-properties 0) lst)))
    lst))

(defun company-same-mode-buffers-plist-to-alist (plist)
  "Convert plist to alist."
  (and plist
       (cons (cons (car plist) (cadr plist))
             (company-same-mode-buffers-plist-to-alist (cddr plist)))))

;; ---- radix-tree

(defun company-same-mode-buffers-tree-insert (tree key float-time)
  (radix-tree-insert tree key float-time))

(defun company-same-mode-buffers-tree-search-1 (tree query prefix)
  (cond ((not (consp tree))             ; empty tree
         nil)
        ((null query)                   ; matched
         (let (res)
           (radix-tree-iter-mappings tree (lambda (k v) (push (concat prefix k) res)))
           res))
        (t
         (nconc
          (when (string-match (car query) (caar tree))
            (if (match-beginning 1)
                (let ((substr (substring (caar tree) 0 (match-end 1))))
                  (company-same-mode-buffers-tree-search-1
                   (radix-tree-subtree tree substr)
                   (cdr query)
                   (concat prefix substr)))
              (company-same-mode-buffers-tree-search-1
               (cdar tree) query (concat prefix (caar tree)))))
          (company-same-mode-buffers-tree-search-1 (cdr tree) query prefix)))))

(defun company-same-mode-buffers-tree-search (tree query)
  (company-same-mode-buffers-tree-search-1
   tree
   (mapcar (lambda (pair) (concat "^" (car pair) "\\(?:$\\|\\(" (cdr pair) "\\)\\)")) query)
   ""))

;; ---- internals

(defvar company-same-mode-buffers-cache (make-hash-table :test 'eq)) ; mode -> tree
(defvar-local company-same-mode-buffers-cache-is-dirty t)

(defun company-same-mode-buffers-update-cache (&optional buffer)
  "Put all symbols in the buffer into
`company-same-mode-buffers-cache'."
  (with-current-buffer (or buffer (current-buffer))
    (when (and company-same-mode-buffers-cache-is-dirty
               (derived-mode-p 'prog-mode))
      (let ((tree (gethash major-mode company-same-mode-buffers-cache))
            (symbols (company-same-mode-buffers-search-current-buffer
                      (concat "\\(:?+\\sw\\|\\s_\\)\\{"
                              (number-to-string company-same-mode-buffers-minimum-word-length)
                              ",\\}")))
            (time (float-time)))
        (dolist (s symbols)
          (setq tree (company-same-mode-buffers-tree-insert tree s time)))
        (puthash major-mode tree company-same-mode-buffers-cache)
        (setq company-same-mode-buffers-cache-is-dirty nil)))))

(defun company-same-mode-buffers-invalidate-cache (&rest _)
  (setq company-same-mode-buffers-cache-is-dirty t))

(defun company-same-mode-buffers-update-cache-other-buffers ()
  "Update cache for all buffers except for the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (company-same-mode-buffers-update-cache buf))))

(defun company-same-mode-buffers-all-completions (query prefix)
  "Collect candidates from the current buffer by searching with
REGEX, and other buffers by filtering the chaches with REGEX."
  (nconc (company-same-mode-buffers-search-current-buffer
          (company-same-mode-buffers-query-to-regex query)
          (point))
         (company-same-mode-buffers-tree-search
          (gethash major-mode company-same-mode-buffers-cache)
          query)))

(defun company-same-mode-buffers-fuzzy-all-completions (prefix)
  (let ((case-fold-search company-same-mode-buffers-case-fold)
        (matchers company-same-mode-buffers-matchers)
        res)
    (while (and (null res) matchers)
      (setq res (company-same-mode-buffers-all-completions (funcall (pop matchers) prefix) prefix)))
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

;; ---- save and load

(defun company-same-mode-buffers-history-remove-old-entries ()
  "Remove old entries from the history."
  (let ((newhash (make-hash-table :test 'eq))
        (limit (- (float-time) company-same-mode-buffers-history-store-limit))
        (deleted 0))
    (dolist (mode (hash-table-keys company-same-mode-buffers-cache))
      (let (tree)
        (radix-tree-iter-mappings
         (gethash mode company-same-mode-buffers-cache)
         (lambda (k v)
           (if (<= limit v)
               (setq tree (radix-tree-insert tree k v))
             (setq deleted (1+ deleted)))))
        (when tree
          (puthash mode tree newhash))))
    (setq company-same-mode-buffers-cache newhash)
    (message "company-same-mode-buffers: deleted %d items" deleted)))

(defun company-same-mode-buffers-save-history ()
  (when company-same-mode-buffers-history-file
    (company-same-mode-buffers-update-cache-other-buffers)
    (company-same-mode-buffers-update-cache (current-buffer))
    (with-temp-buffer
      (prin1 company-same-mode-buffers-cache (current-buffer))
      (write-file company-same-mode-buffers-history-file))))

(defun company-same-mode-buffers-load-history ()
  (when (and company-same-mode-buffers-history-file
             (file-exists-p company-same-mode-buffers-history-file))
    (with-temp-buffer
      (insert-file-contents company-same-mode-buffers-history-file)
      (let ((hash (read (current-buffer))))
        (unless (booleanp (caaar (hash-table-values hash))) ; old format
          (setq company-same-mode-buffers-cache hash))))))

(defun company-same-mode-buffers-initialize ()
  (add-hook 'after-change-functions 'company-same-mode-buffers-invalidate-cache)
  (add-hook 'kill-buffer-hook 'company-same-mode-buffers-update-cache)
  (add-hook 'kill-emacs-hook 'company-same-mode-buffers-save-history)
  (run-with-idle-timer 10 nil 'company-same-mode-buffers-history-remove-old-entries)
  (company-same-mode-buffers-load-history)
  nil)

(provide 'company-same-mode-buffers)
