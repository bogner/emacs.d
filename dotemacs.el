;; Set personal infos
(setq user-mail-address "mail@justinbogner.com")
(setq user-full-name "Justin Bogner")

;; Set up ~/.emacs.d/site-lisp and subdirectories to be searched for elisps.
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Require useful functions
(load "functions")

;; Customize display
(load "display")

;; Customize behaviour
(load "behaviour")

;; Customize key bindings
(load "keys")

;; Mode-specific customization
(load "modes")
