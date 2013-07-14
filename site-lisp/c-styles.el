(c-add-style "bogner"
             '("stroustrup"
               (c-basic-offset . 4)
               (c-offsets-alist . ((arglist-intro . +)
                                   (innamespace . 0)
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

(c-add-style "linux-kernel"
             '("linux"
               (indent-tabs-mode . t)
               (c-offsets-alist
                (arglist-cont-nonempty . '(c-lineup-gcc-asm-reg
                                           c-lineup-arglist-tabs-only)))))
(c-add-style "llvm"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-offsets-alist . ((innamespace . 0)))
               (fill-column . 80)
               (indent-tabs-mode . nil)))

(require 'cc-defs)
(eval-when-compile
  (defvar c-syntactic-element)
  (defvar c-basic-offset))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

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
                  ("/src/linux" . "linux-kernel")
                  ("/llvm/" . "llvm")
                  (".*" . "bogner"))))
    (c-set-style (or (find-c-style-for-file styles) "bogner"))))

(add-hook 'c-mode-common-hook 'c-auto-set-style)

(provide 'c-styles)
