(require 'styles)

;; Highlight TODO
(let ((todo-modes '(c-mode c++-mode csharp-mode java-mode asm-mode
                    common-lisp-mode emacs-lisp-mode lisp-mode haskell-mode
                    perl-mode php-mode python-mode ruby-mode
                    apache-mode nxml-mode css-mode
                    latex-mode tex-mode asy-mode)))
  (dolist (mode todo-modes)
    (font-lock-add-keywords
     mode
     '(("\\<\\(TODO\\):" 1 font-lock-warning-face t)))))

;; C modes
(setq c-default-style '((awk-mode . "awk")
                        (other . "bogner")))
(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

;; C# and ASPX
(add-hook 'csharp-mode-hook (lambda () (c-set-style "forge")))
(add-hook 'aspx-mode-hook (lambda ()
                            (setq indent-tabs-mode t)
                            (setq tab-width 2)))

;; Hooks to make haskell mode behave
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Some eye candy for haskell-mode. Note that the prog1 causes the
;; lambda to be highlighted, but I don't really know why.
(font-lock-add-keywords
 'haskell-mode
 '(("\\(\\\\\\)\\(?: ?[A-Za-z_][A-Za-z0-9_]*\\)+ ?->"
    (0 (prog1 () (compose-region (match-beginning 1)
                                 (match-end 1)
                                 #X03BB))))))

;; Set up modes that will be autoloaded
(autoload 'asy-mode "asy-mode")
(autoload 'csharp-mode "csharp-mode")
(autoload 'haskell-mode "haskell-mode")

(defalias 'sgml-mode 'nxml-mode)
(defalias 'html-mode 'nxml-mode)
(defalias 'xml-mode 'nxml-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.asy\\'" . asy-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.\\(hs\\|hsc\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . literate-haskell-mode))
(add-to-list 'auto-mode-alist
             '("\\.\\(aspx\\|xsl\\|xhtml\\|xsd\\|svg\\|rss\\)\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))
