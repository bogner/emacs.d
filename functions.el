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
point is on, or if arg is set, comment that many lines."
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

(defun create-tags (dir ext)
  "Create a tags file in 'dir' for files with extension
'ext'. Does nothing if such a tags file already exists"
  (setq tags-file-name (format "%sTAGS_%s" dir ext))
  (when (and (not (file-exists-p tags-file-name))
             (file-accessible-directory-p dir))
    (shell-command
     (format "cd %s && find . -type f -name '*.%s' | etags -o %s -"
             dir ext (file-name-nondirectory tags-file-name)))))

(defun auto-tag ()
  "Automatically create and/or select a tags table"
  (interactive)
  (let* ((file (buffer-file-name))
         (dir default-directory)
         (ext (file-name-extension file)))
    (when (vc-registered file) (setq dir (vc-root)))
    (create-tags dir ext)))

(provide 'functions)
