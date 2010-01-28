;;; aref.el --- Find all occurences of a symbol in the current buffer

(require 'hide-lines)

(defvar aref-regexp nil)
(defvar aref-history nil)

(defvar aref-mode-map (make-sparse-keymap)
  "Keymap for aref minor mode.")

(define-key aref-mode-map (kbd "<RET>") 'aref-mode)

(define-minor-mode aref-mode "Show only the lines containing a given symbol."
  :lighter " Aref"
  :keymap aref-mode-map
  (if aref-mode
      (aref-mode-start)
    (aref-mode-stop)))

(defun aref-mode-start ()
  (when (null aref-regexp)
    (set-variable 'aref-regexp (aref-get-default-symbol)))
  (when (null-or-empty-p aref-regexp)
    (aref-mode 0)
    (error "No symbol to find"))
  (add-to-history 'aref-history aref-regexp)
  (hide-non-matching-lines aref-regexp)
  (highlight-regexp aref-regexp "match")
  (recenter))

(defun null-or-empty-p (x)
  (or (null x) (eq x "")))

(defun aref-mode-stop ()
    (show-all-invisible)
    (unhighlight-regexp aref-regexp)
    (set-variable 'aref-regexp nil))

(defun aref-get-default-symbol ()
  (thing-at-point 'symbol))

(defun aref-noprompt ()
  "Do an 'again' on a given symbol, using current symbol."
  (interactive)
  (aref-mode 'toggle))

(defun aref-prompt ()
  "Do an 'again' on a given symbol (prompts user for symbol)."
  (interactive)
  (unless aref-mode
    (let* ((default (aref-get-default-symbol))
           (prompt (format "Aref (default %s): " default))
           (symbol (read-string prompt nil 'aref-history default)))
      (set-variable 'aref-regexp symbol)))
  (aref-mode 'toggle))

(provide 'aref)
