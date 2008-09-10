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
(require 'functions)

;; compile after loading functions
(byte-recompile-directory "~/.emacs.d/site-lisp/")

;; Customize display
(load "display")

;; Customize behaviour
(load "behaviour")

;; Customize key bindings
(load "keys")

;; Mode-specific customization
(load "modes")

;; Communication with the outside world
(load "net")
