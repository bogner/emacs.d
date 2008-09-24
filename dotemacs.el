;; Set personal infos
(setq user-mail-address "mail@justinbogner.com")
(setq user-full-name "Justin Bogner")

(let ((site-lisp (concat "~" init-file-user "/.emacs.d/site-lisp/")))
  ;; Set up ~/.emacs.d/site-lisp and subdirectories to be searched for elisps.
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let ((default-directory site-lisp))
        (setq load-path (cons site-lisp load-path))
        (normal-top-level-add-subdirs-to-load-path)))

  ;; Require useful functions
  (require 'functions)

  ;; compile after loading functions
  ;; TODO: find out why we do this after loading functions
  (byte-recompile-directory site-lisp))

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
