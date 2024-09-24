(require 'cl-lib)
(require 'subr-x)
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
  "Minimum length of the words to be saved."
  :group 'company-same-mode-buffers
  :type 'integer)

(defcustom company-same-mode-buffers-maximum-word-length 80
  "Maximum length of the words to be saved. This may be useful to
  avoid saving long, meaningless words (like a base64 string)."
  :group 'company-same-mode-buffers
  :type 'integer)

(defcustom company-same-mode-buffers-history-file nil
  "When non-nil, save candidates to a file for future sessions."
  :group 'company-same-mode-buffers
  :type 'string)

(defcustom company-same-mode-buffers-history-store-limit (* 7 24 60 60)
  "How long (in seconds) candidates should be stored in the
  history file."
  :group 'company-same-mode-buffers
  :type 'number)

;; ---- matchers

;; A query construct is a pair of 1. prefix-regex (that can be skipped) and 2. a key character (that
;; cannot be skipped)

(defun company-same-mode-buffers-query-construct-any-followed-by (char)
  (cons "\\(?:\\sw\\|\\s_\\)*?" (regexp-quote char)))

(defun company-same-mode-buffers-query-construct-any-word-followed-by (char)
  (cons "\\sw*?" (regexp-quote char)))

(defun company-same-mode-buffers-query-construct-any-lower-followed-by (char)
  (cons "[a-z]*?" (regexp-quote char)))

(defun company-same-mode-buffers-query-construct-exact (char)
  (cons "" (regexp-quote char)))

;; A matcher creates a query, which is a list of query constructs. Unlike regex, matchers can also
;; be used to traverse radix tree with `company-same-mode-buffers-search-tree'.

;; Regex: \(s\)\sw*\(-b\).*
;; -> Matcher: (("" . "s") ("\\sw*" . "-") ("" . "b"))

(defun company-same-mode-buffers-matcher-basic (prefix)
  "A matcher that matches symbols start with PREFIX."
  (mapcar (lambda (s) (company-same-mode-buffers-query-construct-exact s))
          (split-string prefix "" t)))

(defun company-same-mode-buffers-matcher-partial (prefix)
  "A matcher that matches multiword symbols like
`partial-completion' style."
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
before CURSOR is skipped."
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

(defun company-same-mode-buffers-tree-search-1 (subtree query prefix)
  (cond ((null query)                   ; matched
         (let (res)
           (radix-tree-iter-mappings subtree (lambda (k v) (push (concat prefix k) res)))
           res))
        ((not (consp subtree))          ; no match
         nil)
        (t
         (nconc
          (when (string-match (car query) (caar subtree))
            (if (match-beginning 1)
                (let ((substr (substring (caar subtree) 0 (match-end 1))))
                  (company-same-mode-buffers-tree-search-1
                   (radix-tree-subtree subtree substr)
                   (cdr query)
                   (concat prefix substr)))
              (company-same-mode-buffers-tree-search-1
               (cdar subtree) query (concat prefix (caar subtree)))))
          (company-same-mode-buffers-tree-search-1 (cdr subtree) query prefix)))))

(defun company-same-mode-buffers-tree-search (tree query)
  "Search radix-tree with query objects (generated by matchers)."
  (company-same-mode-buffers-tree-search-1
   tree
   (mapcar (lambda (pair) (concat "^" (car pair) "\\(?:$\\|\\(" (cdr pair) "\\)\\)")) query)
   ""))

;; ---- internals

;; per-buffer radix tree for speeding-up regexp search
(defvar-local company-same-mode-buffers-cache nil)
(defvar-local company-same-mode-buffers-cache-is-dirty t)
(defvar-local company-same-mode-buffers--buffer-modified nil)

(defun company-same-mode-buffers-update-cache (&optional buffer)
  "Put all symbols in the buffer into
`company-same-mode-buffers-cache'."
  (with-current-buffer (or buffer (current-buffer))
    (when (and company-same-mode-buffers-cache-is-dirty
               (derived-mode-p 'prog-mode))
      (let ((symbols (company-same-mode-buffers-search-current-buffer
                      (concat "\\(:?+\\sw\\|\\s_\\)\\{"
                              (number-to-string company-same-mode-buffers-minimum-word-length)
                              ","
                              (number-to-string company-same-mode-buffers-maximum-word-length)
                              "\\}"))))
        (dolist (s symbols)
          (setq company-same-mode-buffers-cache
                (radix-tree-insert company-same-mode-buffers-cache s t)))
        (setq company-same-mode-buffers-cache-is-dirty nil)))))

(defun company-same-mode-buffers-invalidate-cache (&rest _)
  (setq company-same-mode-buffers-cache-is-dirty t
        company-same-mode-buffers--buffer-modified t))

(defun company-same-mode-buffers-update-cache-other-buffers ()
  "Update cache for all buffers except for the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (company-same-mode-buffers-update-cache buf))))

(defun company-same-mode-buffers-all-completions (query)
  "Collect candidates matching QUERY from the current buffer and the cache."
  (apply 'nconc
         (company-same-mode-buffers-search-current-buffer
          (company-same-mode-buffers-query-to-regex query)
          (point))
         (mapcar (lambda (b)
                   (when (eq major-mode (buffer-local-value 'major-mode b))
                     (company-same-mode-buffers-tree-search
                      (buffer-local-value 'company-same-mode-buffers-cache b)
                      query)))
                 (buffer-list))))

(defun company-same-mode-buffers-fuzzy-all-completions (prefix)
  "Collect candidates from the current buffer and the cache,
following the matching strategy defiend in
`company-same-mode-buffers-matchers'."
  (let ((case-fold-search company-same-mode-buffers-case-fold)
        (matchers company-same-mode-buffers-matchers)
        res)
    (while (and (null res) matchers)
      (setq res (company-same-mode-buffers-all-completions (funcall (pop matchers) prefix))))
    res))

;; ---- save and load

(defun company-same-mode-buffers-make-save-data-v2 (previous-data)
  ;; alist[time -> alist[mode -> list[symb]]]
  (let ((table (make-hash-table :test 'eq))) ; table[mode -> table[symbol -> (count . write-flag)]]
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when company-same-mode-buffers-cache
          (let ((symbols (or (gethash major-mode table) ; table[symbol -> (count . write-flag)]
                             (puthash major-mode (make-hash-table :test 'equal) table))))
            (radix-tree-iter-mappings
             company-same-mode-buffers-cache
             (lambda (symb _)
               (let* ((oldvalue (or (gethash symb symbols) '(0 . nil)))
                      (count (1+ (car oldvalue)))
                      (write-flag (or (cdr oldvalue) company-same-mode-buffers--buffer-modified)))
                 (puthash symb (cons count write-flag) symbols))))))))
    (let (mode-list)
      (maphash (lambda (mode symbols)
                 (let (symb-list)
                   (maphash (lambda (symb value)
                              (when (and (>= (car value) 2) ; appears in at least two buffers
                                         (cdr value))       ; appears at least one modified buffer
                                (push symb symb-list)))
                            symbols)
                   (when symb-list
                     (push (cons mode symb-list) mode-list))))
               table)
      (cons (cons (float-time) mode-list) previous-data))))

(defun company-same-mode-buffers-load-saved-data-v2 (data)
  ;; alist[time -> alist[mode -> list[symb]]]
  (let ((limit (- (float-time) company-same-mode-buffers-history-store-limit)))
    (dolist (time data)
      (when (<= limit (car time))
        (dolist (mode (cdr time))
          (with-current-buffer (get-buffer-create (format " *company-smb %s*" (car mode)))
            (dolist (symb (cdr mode))
              (insert symb " "))
            (setq major-mode (car mode))))))))

(defun company-same-mode-buffers-save-history ()
  (when company-same-mode-buffers-history-file
    (company-same-mode-buffers-update-cache-other-buffers)
    (company-same-mode-buffers-update-cache (current-buffer))
    (let ((data (company-same-mode-buffers-make-save-data-v2
                 (cdr (company-same-mode-buffers-maybe-parse-history-file))))
          (enable-local-variables nil))
      (with-temp-buffer
        (prin1 (cons 2 data) (current-buffer))
        (write-file company-same-mode-buffers-history-file)))))

(defun company-same-mode-buffers-maybe-parse-history-file ()
  (when (and company-same-mode-buffers-history-file
             (file-exists-p company-same-mode-buffers-history-file))
    (with-temp-buffer
      (insert-file-contents company-same-mode-buffers-history-file)
      (read (current-buffer)))))

(defun company-same-mode-buffers-load-history ()
  (let ((data (company-same-mode-buffers-maybe-parse-history-file)))
    (when data
      (cl-case (car data)
        (2 (company-same-mode-buffers-load-saved-data-v2 (cdr data)))
        (t (error "unknown history file version"))))))

(defun company-same-mode-buffers-initialize ()
  "Load saved history file, and prepare hooks to update the history."
  (add-hook 'after-change-functions 'company-same-mode-buffers-invalidate-cache)
  (add-hook 'kill-buffer-hook 'company-same-mode-buffers-update-cache)
  (add-hook 'kill-emacs-hook 'company-same-mode-buffers-save-history)
  (company-same-mode-buffers-load-history)
  nil)

(provide 'company-same-mode-buffers-core)
