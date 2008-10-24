(c-add-style "bogner"
             '("stroustrup"
               (c-offsets-alist . ((arglist-intro . +)))
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

(c-add-style "forge"
             '("bsd"
               (c-basic-offset . 4)
               (indent-tabs-mode . t)
               (tab-width . 4)))

(c-add-style "yy"
	     '("k&r"
	       (c-basic-offset . 4)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (cond
	     ((string-match "/cmput415/" buffer-file-name)
              (c-set-style "bob-beck"))
	     ((string-match "/yy/" buffer-file-name)
              (c-set-style "yy")))))

(provide 'styles)
