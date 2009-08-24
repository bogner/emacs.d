(require 'functions)

;; Compilation --- prefix \C-cc to prompt for a compile command
(setq-default compilation-read-command nil)
(global-set-key (kbd "C-c b") (lambda (pfx)
                                (interactive "p")
                                (setenv "buffer" (file-relative-name (buffer-file-name)))
                                (call-interactively 'compile)))

;; dabbrev-completion is more bash-like than dabbrev-expand
(global-set-key (kbd "M-/") (lambda ()
                              (interactive)
                              (dabbrev-completion)
                              (fit-windows)))

;; TODO: think of a better shortcut here
(global-set-key (kbd "M-]") 'fit-windows)

(global-set-key (kbd "C-\;") 'comment-or-uncomment-dwim)

;; C-z in a window system is confusing and useless
;; Don't bother if we don't have suspend-frame
(when (fboundp 'suspend-frame)
  (global-set-key (kbd "C-z") (lambda ()
                                (interactive)
                                (if window-system
                                    (message "i'm afraid i can't do that dave")
                                  (suspend-frame)))))

;; find the file or url at the point, if possible
(global-set-key (kbd "C-<down-mouse-1>") 'ffap-at-mouse)

(global-set-key (kbd "M-n") (lambda (arg)
                              (interactive "p")
                              (line-move-visual (or arg 1))))

(global-set-key (kbd "M-p") (lambda (arg)
                              (interactive "p")
                              (line-move-visual (- (or arg 1)))))

;; buffer-menu is nicer than list-buffers
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(define-key mode-line-buffer-identification-keymap
  [mode-line mouse-2] 'buffer-menu)

(defvar outline-short-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "@" 'outline-mark-subtree)
    (define-key map "n" 'outline-next-visible-heading)
    (define-key map "p" 'outline-previous-visible-heading)
    (define-key map "i" 'show-children)
    (define-key map "s" 'show-subtree)
    (define-key map "d" 'hide-subtree)
    (define-key map "u" 'outline-up-heading)
    (define-key map "f" 'outline-forward-same-level)
    (define-key map "b" 'outline-backward-same-level)
    (define-key map "t" 'hide-body)
    (define-key map "a" 'show-all)
    (define-key map "c" 'hide-entry)
    (define-key map "e" 'show-entry)
    (define-key map "l" 'hide-leaves)
    (define-key map "k" 'show-branches)
    (define-key map "q" 'hide-sublevels)
    (define-key map "o" 'hide-other)
    (define-key map "^" 'outline-move-subtree-up)
    (define-key map "v" 'outline-move-subtree-down)
    (define-key map "<" 'outline-promote)
    (define-key map ">" 'outline-demote)
    (define-key map "m" 'outline-insert-heading)
    map))
(define-key outline-minor-mode-map (kbd "C-c o") outline-short-prefix-map)
