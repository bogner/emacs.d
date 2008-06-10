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
(c-set-offset 'arglist-intro '+)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; C# and ASPX
(add-hook 'csharp-mode-hook (lambda ()
          (setq indent-tabs-mode t)
          (setq c-basic-offset 4)
          (setq tab-width 4)
          (c-set-style "bsd")))
(add-hook 'aspx-mode-hook (lambda ()
                            (setq indent-tabs-mode t)
                            (setq tab-width 2)))

;; Set up modes that will be autoloaded
(autoload 'asy-mode "asy-mode")
(autoload 'csharp-mode "csharp-mode")
(autoload 'haskell-mode "haskell-mode")

(defalias 'sgml-mode 'nxml-mode)
(defalias 'html-mode 'nxml-mode)
(defalias 'xml-mode 'nxml-mode)

(add-to-list 'auto-mode-alist '("\\.asy\\'" . asy-mode))
(add-to-list 'auto-mode-alist '("\\.\\(hs\\|hsc\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . literate-haskell-mode))
(add-to-list 'auto-mode-alist
             '("\\.\\(aspx\\|xsl\\|xhtml\\|xsd\\|svg\\|rss\\)\\'" . nxml-mode))
