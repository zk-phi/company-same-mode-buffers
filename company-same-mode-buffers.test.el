(require 'company-same-mode-buffers-core)

(ert-deftest company-same-mode-buffers-search-regex ()
  (let ((basic (company-same-mode-buffers-matcher-basic "smb"))
        (partial (company-same-mode-buffers-matcher-partial "s-m-b"))
        (flex-rest (company-same-mode-buffers-matcher-exact-first-letter-flex-rest "smb"))
        (flex (company-same-mode-buffers-matcher-flex "smb")))
    (with-temp-buffer
      (save-excursion
        (insert "smbxxx s-m-b same-mode-buffers company-same-mode-buffers "
                "samemodebuffers companysamemodebuffers never-match"))
      (should (equal '("smbxxx")
                     (company-same-mode-buffers-search-current-buffer
                      (company-same-mode-buffers-query-to-regex basic))))
      (should (equal '("same-mode-buffers" "s-m-b")
                     (company-same-mode-buffers-search-current-buffer
                      (company-same-mode-buffers-query-to-regex partial))))
      (should (equal '("samemodebuffers" "same-mode-buffers" "s-m-b" "smbxxx")
                     (company-same-mode-buffers-search-current-buffer
                      (company-same-mode-buffers-query-to-regex flex-rest))))
      (should (equal '("companysamemodebuffers" "samemodebuffers"
                       "company-same-mode-buffers" "same-mode-buffers" "s-m-b" "smbxxx")
                     (company-same-mode-buffers-search-current-buffer
                      (company-same-mode-buffers-query-to-regex flex)))))))

(ert-deftest company-same-mode-buffers-search-tree ()
  (let ((tree nil)
        (basic (company-same-mode-buffers-matcher-basic "smb"))
        (partial (company-same-mode-buffers-matcher-partial "s-m-b"))
        (flex-rest (company-same-mode-buffers-matcher-exact-first-letter-flex-rest "smb"))
        (flex (company-same-mode-buffers-matcher-flex "smb")))
    (setq tree (radix-tree-insert tree "smbxxx" t))
    (setq tree (radix-tree-insert tree "s-m-b" t))
    (setq tree (radix-tree-insert tree "same-mode-buffers" t))
    (setq tree (radix-tree-insert tree "company-same-mode-buffers" t))
    (setq tree (radix-tree-insert tree "samemodebuffers" t))
    (setq tree (radix-tree-insert tree "companysamemodebuffers" t))
    (setq tree (radix-tree-insert tree "never-match" t))
    (should (equal '("smbxxx")
                   (company-same-mode-buffers-tree-search tree basic)))
    (should (equal '("s-m-b" "same-mode-buffers")
                   (company-same-mode-buffers-tree-search tree partial)))
    (should (equal '("smbxxx" "s-m-b" "same-mode-buffers" "samemodebuffers")
                   (company-same-mode-buffers-tree-search tree flex-rest)))
    (should (equal '("smbxxx" "s-m-b" "same-mode-buffers" "samemodebuffers"
                     "company-same-mode-buffers" "companysamemodebuffers")
                   (company-same-mode-buffers-tree-search tree flex)))))
