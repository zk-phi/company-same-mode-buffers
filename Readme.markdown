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

In order to speed-up searching, symbols are collected in per-buffer radix-trees, called "cache"s.

To avoid heavy updating, the current buffer's cache is NOT updated. Instead, symbols in the current buffer are searched by simple regex-search.

### History file internals (v2)

An alist of the form `alist[timestamp => alist[major-mode => list[symbol]]]` is saved to the history
file.

``` emacs-lisp
;; example:
((<timestamp>
  (emacs-lisp-mode "save-excursion" "defun" "defvar")
  (js-mode "function" "return"))
 (<timestamp>
  (emacs-lisp-mode "defconst" "defun") ; duplicated symbols may saved with different timestamps
  (css-mode "margin")))
```

### Saving candidates

When killing Emacs, `company-same-mode-buffers` visits all `prog-mode` buffers and collects all
symbols in these buffers. Then for each symbols collected, if the symbol

1. appears in more than two buffers, and
2. appears in at least one user-modified buffers,

the symbol is added to the new history entry.

To save memory, symbols that appear in only one buffer (like buffer-local variables), and symbols that do not appear in buffers that user modify (like logs, or files created with code-generators), are not saved.

A known downside is: If a buffer is killed before killing Emacs, symbols in that buffer are NOT
saved.

### Loading history file

All symbols in unexpired history entries are simply inserted into a temporary buffer, with
corresponding major-mode. One temporary buffer is created for one major-mode.

``` emacs-lisp
;; a temporary buffer with emacs-lisp-mode
save-excursion defun defvar defconst defun
```

``` javascript
// a temporary buffer with js-mode
function return
```

``` css
/* a temporary buffer with css-mode */
margin
```

These temporary buffers are also visited when saving the history file. As a result,

new symbols are added to the history file, when

1. the symbol appears in more than two buffers, and
2. the symbol appears in at least one user-modified buffers

and symbols from the previous sessions are re-added (with updated timestamp) when

1. the symbol appears in at least one user-modified, non-temporary buffers

Other symbols from the previous sessions are kept as they are (without updated timestamp).
