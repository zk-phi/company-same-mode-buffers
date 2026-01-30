# company-same-mode-buffers.el / complete-same-mode-buffers.el

![screencast](img/screencast.gif)

Company / completion-at-point backend to complete symbols in same mode buffers (like
`ac-source-words-in-same-mode-buffers`)

with following features:

- Reasonable performance powered by radix-tree
- Optional flex matching
- Optionally save candidates for future sessions

## Installation

load and initialize this package, and,

### company

add to `company-backends`.

```emacs-lisp
(require 'company-same-mode-buffers)
(company-same-mode-buffers-initialize)
(push 'company-same-mode-buffers company-backends)
```

### completion-at-point (corfu)

add to `completion-at-point-functions`.

```emacs-lisp
(require complete-same-mode-buffers)
(company-same-mode-buffers-initialize)
(push 'complete-same-mode-buffers (default-value 'completion-at-point-functions))
```

## Customization
### Matching algorithms

You may disable some of these matching algorithms, if you don't need
them.

```emacs-lisp
(setq company-same-mode-buffers-matchers
      '(company-same-mode-buffers-matcher-basic
        company-same-mode-buffers-matcher-partial
        company-same-mode-buffers-matcher-exact-first-letter-flex-rest
        ;; company-same-mode-buffers-matcher-flex
        ))
```

- `company-same-mode-buffers-matcher-basic`

  `compan` -> `compan*` -> `company-backends`

- `company-same-mode-buffers-matcher-partial`

  `co-ba` -> `co*-ba*` -> `company-backends`

- `company-same-mode-buffers-matcher-exact-first-letter-flex-rest`

  `cbns` -> `c*n*b*s*` -> `company-backends`

- `company-same-mode-buffers-matcher-flex`

  `pbns` -> `*p*n*b*s*` -> `company-backends`

### History file

You may optionally save completion candidates to a file for future sessions.

```emacs-lisp
;; this line MUST be put before "company-same-mode-buffers-initialize"
(setq company-same-mode-buffers-history-file "~/.emacs.d/.company-same-mode-buffers-history")
(company-same-mode-buffers-initialize)
```

## Internals
### Symbol caching

In order to speed-up searching, symbols are collected in per-file radix-trees, called "cache"s.

A known downside is: symbols in non-file buffers (like `*scratch*`) are not completed.

To avoid heavy updating, the current buffer's cache is NOT updated. Instead, symbols in the current buffer are searched by simple regex-search.

### History file internals (v3)

An alist of the form `alist[major-mode => sorted-unique-list[symbol]]` is saved to the history file.

``` emacs-lisp
;; example:
((emacs-lisp-mode "save-excursion" "defun" "defvar")
 (js-mode "function" "return"))
```

Each symbol lists are sorted by most-recent-appearance.

### Saving candidates

When killing Emacs, `company-same-mode-buffers` collects all symbols from per-file caches.
Then for each symbols collected, if the symbol

1. appears in more than two files, and
2. appears in at least one user-modified files,

the symbol is prepended to the new history entry.

To save memory, symbols that appear in only one file (like local variables), and symbols that do not appear in files that user modify (like logs, or files created with code-generators), are not saved.

### Loading history file

All symbols in unexpired history entries are also added to the per-file cache, as an unmodified file with no name (`nil`).

As a result, symbols from the previous sessions are re-prepended when

1. the symbol appears in at least one user-modified, non-temporary buffers

Other symbols from the previous sessions are kept at the same position in the sorted-list.
