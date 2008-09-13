(c-add-style "bogner"
             '("stroustrup"
               (c-basic-offset . 2)
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

(provide 'styles)
