(c-add-style "bogner"
             '("stroustrup"
               (c-basic-offset . 4)
               (c-offsets-alist . ((arglist-intro . +)
                                   (statement-cont . c-lineup-assignments)))
               (c-tab-always-indents . nil)
               (indent-tabs-mode . nil)))

(c-add-style "bob-beck"
             '("bsd"
               (c-backslash-column . 78)
               (c-indent-comments-syntactically-p . t)
               (c-tab-always-indent . nil)
               (c-cleanup-list . (scope-operator
                                  brace-else-brace))
               (c-offsets-alist . ((arglist-close . 0)
                                   (arglist-cont-nonempty . *)
                                   (statement-cont . *)))
               (indent-tabs-mode . t)
               (show-trailing-whitespace . t)))

(c-add-style "nsfw"
	     '("cc-mode"
	       (indent-tabs-mode . nil)
	       (c-basic-offset . 4)
	       (c-offsets-alist
		(substatement-open . 0)
		(label . 0))))

(defun find-c-style-for-file (rules)
  (catch 'found
    (dolist (rule rules)
      (when (string-match (car rule) buffer-file-name)
        (throw 'found (cdr rule))))))

(defun c-auto-set-style ()
  (let ((styles '(("/snac/cache/" . "gnu")
                  ("/snac/dmg/" . "gnu")
                  ("/stdf/" . "gnu")
                  ("/nsfw/" . "nsfw")
                  (".*" . "bogner"))))
    (c-set-style (or (find-c-style-for-file styles) "bogner"))))

(add-hook 'c-mode-common-hook 'c-auto-set-style)

(provide 'c-styles)
