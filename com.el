(setq rcirc-default-nick "bogner")

;; Servers
(setq rcirc-server-alist '(("irc.freenode.net"
                            :channels ("#haskell" "#xmonad"))))
(when (file-exists-p "rcirc-auth.el") (load "rcirc-auth"))

;; Show buffers with unread messages in mode line
(rcirc-track-minor-mode t)

;; Logging
(setq rcirc-log-flag t)

;; Keep 500k worth of 80 character lines in memory
(setq rcirc-buffer-maximum-lines 6554)

;; TODO: make these match based on rcirc-response-formats...
(defun rcirc-next-message (arg)
  "Go to the next message"
  (interactive "*p")
  (or arg (setq arg 1))
  (let ((old-point (point-marker)))
    (end-of-line)
    (if (re-search-forward "<.*?> .*$" nil t arg)
        (beginning-of-line)
      (goto-char old-point))))

(defun rcirc-prev-message (arg)
  "Go to the previous message"
  (interactive "*p")
  (or arg (setq arg 1))
  (when (re-search-backward "<.*?> .*$" nil t arg)
    (beginning-of-line)))

(defun insert-or-command (command)
  "If the text at point is read-only, run command, otherwise, do
self-insert-command"
  (if (get-text-property (point) 'read-only)
      (call-interactively command)
    (call-interactively 'self-insert-command)))

;; Make it easy to read through responses
(define-key rcirc-mode-map (kbd "n")
  (lambda ()
    (interactive)
    (insert-or-command 'rcirc-next-message)))
(define-key rcirc-mode-map (kbd "p")
  (lambda ()
    (interactive)
    (insert-or-command 'rcirc-prev-message)))

;; We would like to clear the unread marker explicitly
(define-key rcirc-mode-map (kbd "C-c C-f")
  (lambda () (interactive) (rcirc-clear-unread (current-buffer))))
