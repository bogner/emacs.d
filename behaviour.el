(require 'functions)

;; Single character yes/no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ignore case searching,  preserve case replacing
(setq case-fold-search t)
(setq case-replace t)

;; Enforce case when searching for expansions
(set-variable 'dabbrev-case-fold-search nil)

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Inhibit backup files
(setq backup-inhibited t)

;; If the mark is transient, we want delete selection, but we don't
;; normally want a transient mark
(delete-selection-mode 1)
(transient-mark-mode -1)
(setq-default shift-select-mode t)

;; Moving between lines visually is perhaps saner, but breaks
;; macros that deal with multiple lines
(set-variable 'line-move-visual nil)

;; Enable the mouse wheel
(mouse-wheel-mode 1)

;; Make the mouse work in terminals
(xterm-mouse-mode 1)

;; If this isn't nil, there's a chance one of those terrible gtk file
;; dialogs may show up.
(set-variable 'use-file-dialog nil)

;; tramp and partial completion
(setq partial-completion-mode t)
(when (require-or-nil 'tramp)
  (set-variable 'tramp-default-method "ssh"))

;; Unified diffs
(setq-default diff-switches "-up")
(set-variable 'diff-default-read-only t)

;; Ediff: no extra frames, split horizontally for wide windows
(set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)
(set-variable 'ediff-split-window-function
              (lambda (&optional arg)
                (if (> (frame-width) 160)
                    (split-window-horizontally arg)
                  (split-window-vertically arg))))

;; Persistence
(when (require-or-nil 'saveplace)
  (setq-default save-place t))
;; emacs21 doesn't have savehist-mode
(when (boundp 'savehist-mode)
  (savehist-mode 1))

;; Always syntax highlight
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Look for an tags table when we need one
(set-variable 'tags-add-tables nil)
(set-variable 'default-tags-table-function 'find-tags-file)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Update time stamps if they exist
(add-hook 'before-save-hook 'time-stamp)

;; Global indentation rules
(setq-default indent-tabs-mode nil
              standard-indent 2)
;; set tab stops to every 4 spaces (mostly for asm-mode)
(setq-default tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; Ensure newline at EOF
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(set-variable 'next-line-extends-end-of-buffer nil)

;; fill-paragraph should recognize lists, like "1.", "ii)", etc.
;; Otherwise, this is the default from emacs23 with some unicode
;; characters removed, because having them impeded me from using this
;; in emacs21 and emacs22.
(set-variable 'adaptive-fill-regexp
              "[ \t]*\\(\\(\\w+[.)]\\|[-!|#%;>*]+\\)[ \t]*\\)*")

;; File names like foo<1> aren't terribly helpful
(require 'uniquify)
(set-variable 'uniquify-buffer-name-style 'post-forward)

;; Act as a server unless a server is already running. Old emacs
;; doesn't have server-running-p, but we don't like their server
;; anyway.
(require 'server)
(when (and (fboundp 'server-running-p)
           (not (server-running-p)))
  (server-start))
