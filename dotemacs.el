;; Set up ~/.emacs.d/site-lisp and subdirectories to be searched for elisps.
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Remove startup message
(setq inhibit-startup-message t)

;; menus are for stupid
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)

(transient-mark-mode 0)
(mouse-wheel-mode 1)
;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
;; Blinking cursor wakes up the cpu when reading code
(blink-cursor-mode 0)

;; Always syntax highlight
(global-font-lock-mode t)
;; always use spaces, never tabs
(setq-default indent-tabs-mode nil)
;; set tab stops to every 4 spaces (mostly for asm-mode)
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; Get rid of that annoying prompt that requires one to type
;; in Y-E-S and then press the friggin enter key to confirm.
(defun yes-or-no-p (PROMPT)
  (y-or-n-p PROMPT))

;; don't keep backups
(setq backup-inhibited t)

;; Display the current time
(autoload 'display-time "time" "Display Time" t)
(condition-case err
    (display-time)
  (error (message "Unable to load Time package.")))
(setq display-time-24hr-format nil)
(setq display-time-day-and-date t)

;; Useful keyboard shortcuts
(global-set-key "\C-x c" 'compile)

;; haskell mode
(load "haskell-site-file")

;; Highlight TODO
(let ((todo-modes '(c-mode c++-mode java-mode asm-mode
                           common-lisp-mode emacs-lisp-mode lisp-mode
                           perl-mode php-mode python-mode ruby-mode
                           latex-mode tex-mode haskell-mode)))
  (dolist (mode todo-modes)
    (font-lock-add-keywords
     mode
     '(("\\<\\(TODO\\):" 1 font-lock-warning-face t)))))

;; Set personal infos
(setq user-mail-address "im@justinbogner.com")
(setq user-full-name "Justin Bogner")

(setq partial-completion-mode t)
(require 'tramp)
(setq tramp-default-method "ssh")

(require 'asy-mode)

;; Zenburn color theme
(when window-system
  (require 'zenburn)
  (color-theme-zenburn)
  (set-default-font "BitStream Vera Sans Mono-8.5"))
