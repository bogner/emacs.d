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

;; tramp and partial completion
(setq partial-completion-mode t)
(require 'tramp)
(setq tramp-default-method "ssh")

;; Always syntax highlight
(global-font-lock-mode t)
;; always use spaces, never tabs
(setq-default indent-tabs-mode nil)
;; set tab stops to every 4 spaces (mostly for asm-mode)
(setq-default tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; Useful keyboard shortcuts
(global-set-key "\C-cc" 'compile)

;; Highlight TODO
(let ((todo-modes '(c-mode c++-mode csharp-mode java-mode asm-mode
                           common-lisp-mode emacs-lisp-mode lisp-mode
                           perl-mode php-mode python-mode ruby-mode
                           latex-mode tex-mode haskell-mode)))
  (dolist (mode todo-modes)
    (font-lock-add-keywords
     mode
     '(("\\<\\(TODO\\):" 1 font-lock-warning-face t)))))

(c-set-offset 'arglist-intro '+)

;; haskell mode
(load "haskell-site-file")

;; asymptote
(require 'asy-mode)

;; c# and aspx
(load-library "csharp-mode-0.4.0")
(add-hook 'csharp-mode-hook (lambda ()
          (setq indent-tabs-mode t)
          (setq c-basic-offset 4)
          (setq tab-width 4)
          (c-set-offset 'arglist-intro '+)
          (c-set-style "bsd")))
(define-derived-mode aspx-mode
            html-mode "ASPX"
            "Major mode for ASPX markup.")
(add-to-list 'auto-mode-alist '("\\.aspx\\'" . aspx-mode))
(add-hook 'aspx-mode-hook (lambda ()
                            (setq indent-tabs-mode t)
                            (setq tab-width 2)))

;; Show the whitesp
;(let ((display-table (make-display-table)))
;  (aset display-table 9 (string-to-vector "⇒   "))
;  (aset display-table 32 (string-to-vector "→"))
;  (setq buffer-display-table display-table))

;; Set personal infos
(setq user-mail-address "im@justinbogner.com")
(setq user-full-name "Justin Bogner")

;; Zenburn color theme
(when window-system
  (require 'zenburn)
  (color-theme-zenburn)
  (set-default-font "DejaVu Sans Mono-8.5"))
