# company-same-mode-buffers.el / complete-same-mode-buffers.el

![screencast](img/screencast.gif)

Company / completion-at-point backend to complete symbols in same mode buffers (like
`ac-source-words-in-same-mode-buffers`)

with following features:

- Optional flex matching
- Good performance powered by radix-tree
- History mechanism to share candidates across sessions

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

### History file

You may optionally save current candidates to a file, to share across
sessions.

```emacs-lisp
;; this line must be put before "company-same-mode-buffers-initialize"
(setq company-same-mode-buffers-history-file "~/.emacs.d/.company-same-mode-buffers-history")
```

#### History file internals (v2)
##### Format

An alist of the form `alist[timestamp => alist[major-mode => list[symbol]]]` is saved to the history
file.

``` emacs-lisp
((TIMESTAMP (MAJOR_MODE SYMBOL ...) ...) ...)
```

##### Load

For each history record (of the form `cons[timestamp, alist[major-mode => symbol]]`), symbols are inserted into temporary buffer with appropreate major-mode.

``` emacs-lisp
;; sample history
((<timestamp>
  (emacs-lisp-mode "save-excursion" "defun" "defvar")
  (js-mode "function" "return"))
 (<timestamp>
  (emacs-lisp-mode "defconst")
  (css-mode "margin")))
```

``` emacs-lisp
;; a temporary buffer with emacs-lisp-mode
save-excursion defun defvar defconst
```

``` emacs-lisp
;; a temporary buffer with js-mode
function return
```

``` emacs-lisp
;; a temporary buffer with css-mode
margin
```

Note that each history records' timestamp is compared to `company-same-mode-buffers-history-store-limit`, and if it's older than the threshold, the record is skipped.

##### Save

When killing Emacs, `company-same-mode-buffers` visits all buffers (that derives `prog-mode`, including temporary buffers created by `company-same-mode-buffers`) and collect all symbols. If a symbol appears in more than two buffers, then the symbols is saved to the history file with new timestamp.

This way, we can store symbols ONLY IF it's useful across buffers, and save memory. We don't need file-specific symbols (like internal variable names) to be saved.

Downside: If a buffer is killed before killing Emacs, symbols in that buffer are NOT saved.

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
