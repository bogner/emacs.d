;; Set personal infos
(setq user-mail-address "mail@justinbogner.com")
(setq user-full-name "Justin Bogner")

(let ((site-lisp (concat "~" init-file-user "/.emacs.d/site-lisp/")))
  ;; Set up ~/.emacs.d/site-lisp and subdirectories to be searched for elisps.
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let ((default-directory site-lisp))
        (add-to-list 'load-path site-lisp)
        (normal-top-level-add-subdirs-to-load-path)))

  ;; Try to put the system site-lisp at the end of our load path if we
  ;; don't have it already.
  (when (file-directory-p "/usr/share/emacs/site-lisp")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp" t))

  ;; Require useful functions
  (require 'functions)

  ;; compile after loading functions
  ;; TODO: find out why we do this after loading functions
  (byte-recompile-directory site-lisp 0))

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

;; Calendar stuff
(load "diary")
