;; company backend to complete symbols from same mode buffers with
;; flex matching and history

(require 'company)
(require 'company-same-mode-buffers-core)

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

(provide 'company-same-mode-buffers)
