(require 'functions)

;;; RCirc
(when (require-or-nil 'rcirc)
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
    (lambda () (interactive) (rcirc-clear-unread (current-buffer)))))

;;; Gnus / Message
(when (require-or-nil 'mm-uu)
  (set-variable 'mm-uu-diff-groups-regex ".*")
  (assq-delete-all 'diff mm-uu-type-alist)
  (add-to-list 'mm-uu-type-alist
               '(diff "^Index: \\|^diff \\|^--- [A-Za-z0-9_/.-]+"
                      nil
                      mm-uu-diff-extract
                      nil
                      mm-uu-diff-test))
  (mm-uu-configure))

;; darcs defines it's own media type for patches.
(when (require-or-nil 'mm-decode)
  (add-to-list 'mm-inline-media-tests
               '("text/x-darcs-patch" mm-display-patch-inline
                 (lambda (handle) (fboundp 'diff-mode)))))
