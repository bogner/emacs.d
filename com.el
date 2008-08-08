(setq rcirc-default-nick "bogner")

;; Servers
(setq rcirc-server-alist '(("irc.freenode.net"
                            :channels ("#haskell" "#xmonad"))))
(when (file-exists-p "rcirc-auth.el") (load "rcirc-auth"))

;; Show buffers with unread messages in mode line
(setq rcirc-track-minor-mode t)

;; Logging
(setq rcirc-log-flag t)

;; Keep 1M worth of 80 character lines in memory
(setq rcirc-buffer-maximum-lines 13107)

;; We would like to clear the unread marker explicitly
(define-key rcirc-mode-map (kbd "C-c C-f")
  (lambda () (interactive) (rcirc-clear-unread (current-buffer))))
