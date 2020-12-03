# company-same-mode-buffers.el

![screencast](img/screencast.gif)

Company backend to complete symbols in same mode buffers (like
`ac-source-words-in-same-mode-buffers`)

with following features:

- Optional flex matching
- Good performance powered by radix-tree
- History mechanism to share candidates across sessions

## Installation

load and initialize this package, and add to `company-backends`.

```emacs-lisp
(require 'company-same-mode-buffers)
(company-same-mode-buffers-initialize)
(push 'company-same-mode-buffers company-backends)
```

### History file

You may optionally save current candidates to a file, to share across
sessions.

```emacs-lisp
;; this line must be put before "company-same-mode-buffers-initialize"
(setq company-same-mode-buffers-history-file "~/.emacs.d/.company-same-mode-buffers-history")
```

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
