;; Set personal infos
(setq user-mail-address "Justin Bogner <mail@justinbogner.com>")
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

  ;; recompile all my site-lisp
  (byte-recompile-directory site-lisp 0))

;; Infer the hostname and load hostname-local.el, if it exists.
(let* ((host (car (split-string (shell-command-to-string "hostname"))))
       (local-conf (concat host "-local" ".el")))
  (when (locate-library local-conf)
    (load-library local-conf)))

;; Load my config
(load-library "config")

;; Load config stuff that's not ready for my config
(when (locate-library "incoming")
  (load-library "incoming"))
