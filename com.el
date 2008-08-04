(setq rcirc-default-nick "bogner")

;; Servers
(setq rcirc-server-alist '(("irc.freenode.net"
                            :channels ("#haskell" "#xmonad"))))
(when (file-exists-p "rcirc-auth.el") (load "rcirc-auth"))

;; Logging
(setq rcirc-log-flag t)

;; Keep 1M worth of 80 character lines in memory
(setq rcirc-buffer-maximum-lines 13107)
