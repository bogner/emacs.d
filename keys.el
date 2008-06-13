;; Compilation --- prefix \C-cc to prompt for a compile command
(setq-default compilation-read-command nil)
(global-set-key "\C-cb" (lambda (pfx)
                          (interactive "p")
                          (setenv "buffer" (buffer-file-name))
                          (call-interactively 'compile)))

;; dabbrev-completion is more bash-like than dabbrev-expand
(global-set-key [?\M-/] (lambda ()
                          (interactive)
                          (dabbrev-completion)
                          (fit-windows)))

;; TODO: think of a better shortcut here
(global-set-key [?\M-\]] 'fit-windows)

(global-set-key [?\C-\;] 'comment-or-uncomment-dwim)

