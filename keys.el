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
(global-set-key (kbd "C-x C-f") 'find-file-at-point)
(global-set-key (kbd "C-<down-mouse-1>") 'ffap-at-mouse)

(global-set-key (kbd "M-n") (lambda (arg)
                              (interactive "p")
                              (line-move-visual (or arg 1))))

(global-set-key (kbd "M-p") (lambda (arg)
                              (interactive "p")
                              (line-move-visual (- (or arg 1)))))
