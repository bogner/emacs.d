;; Single character yes/no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ignore case searching,  preserve case replacing
(setq case-fold-search t)
(setq case-replace t)

;; Inhibit backup files
(setq backup-inhibited t)

;; If the mark is transient, we want delete selection, but we don't
;; normally want a transient mark
(delete-selection-mode 1)
(transient-mark-mode -1)
(setq-default shift-select-mode t)

;; Enable the mouse wheel
(mouse-wheel-mode 1)

;; tramp and partial completion
(setq partial-completion-mode t)
(require 'tramp)
(setq tramp-default-method "ssh")

;; Unified diffs
(setq-default diff-switches "-up")

;; Persistence
(when (require 'saveplace)
  (setq-default save-place t))
(savehist-mode 1)

;; Always syntax highlight
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Global indentation rules
(setq-default indent-tabs-mode nil
              standard-indent 2
              tab-width 2)
;; set tab stops to every 4 spaces (mostly for asm-mode)
(setq-default tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; Ensure newline at EOF
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(setq next-line-extends-end-of-buffer nil)

;; Act as a server
(server-start)
