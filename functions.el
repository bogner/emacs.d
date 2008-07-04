(defun comment-or-uncomment-line ()
  "Comment the line that the point is on."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (back-to-indentation)
      (comment-or-uncomment-region (point) end))))

(defun comment-or-uncomment-dwim (arg)
  "If transient-mark-mode is t and the mark is active,
comment-or-uncomment-region, otherwise, comment the line that the
point is on."
  (interactive "*P")
  (if (and mark-active transient-mark-mode)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (call-interactively 'comment-or-uncomment-line)))

(defun fit-windows ()
  "Fit all of the windows as to minimize unused space"
  (interactive)
  (walk-windows 'shrink-window-if-larger-than-buffer))

(defun vc-root ()
  "Find the root directory of the current buffer's version control"
  (vc-call root (buffer-file-name)))

(defun exuberant-tags (dir)
  "Generate and visit a tags file in `dir' using exuberant ctags"
  (when (file-newer-than-file-p dir (expand-file-name "TAGS" dir))
    (message (format "generating tags file in %s" dir))
    (shell-command
     (format "cd %s && etags --version | grep 'Exuberant Ctags' && etags -R"
             dir)))
  (set (make-local-variable 'tags-file-name) dir))

(defun auto-tag ()
  "Automatically select tags tables when we're in version
control. Always replaces tags tables instead of adding them."
  (interactive)
  (when (vc-registered (buffer-file-name))
    (exuberant-tags (vc-root))))

(defun require-or-nil (feature)
  "If `feature' exists, require it, else return `nil'."
  (if (locate-library (format "%s" feature))
      (require feature)
    nil))

(provide 'functions)
