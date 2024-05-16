(require 'company-same-mode-buffers-core)

(defun complete-same-mode-buffers ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            (progn
              (company-same-mode-buffers-update-cache-other-buffers)
              (company-same-mode-buffers-fuzzy-all-completions
               (buffer-substring (car bounds) (cdr bounds))))
            :exclusive 'no
            :annotation-function (lambda (_) " smb")))))

(provide 'complete-same-mode-buffers)
