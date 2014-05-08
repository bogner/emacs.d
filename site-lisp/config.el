;;; Functions
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr reverse
		 (function
		  (lambda ()
		    (while (and (not (eobp)) (looking-at "\\W"))
		      (forward-char))))
		 'forward-word))))

(defun comment-or-uncomment-line ()
  "Comment the line that the point is on."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (back-to-indentation)
      (comment-or-uncomment-region (point) end))))

(defun comment-or-uncomment-dwim (arg)
  "If transient-mark-mode is t and the mark is active,
comment-or-uncomment-region, otherwise, comment the line that the
point is on."
  (interactive "*P")
  (if (and mark-active transient-mark-mode)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (call-interactively 'comment-or-uncomment-line)))

(defun fit-windows ()
  "Fit all of the windows as to minimize unused space"
  (interactive)
  (walk-windows 'shrink-window-if-larger-than-buffer))

(defun vc-root ()
  "Find the root directory of the current buffer's version control"
  (vc-call root (buffer-file-name)))

(defun exuberant-tags (dir &optional regenerate)
  "Generate and visit a tags file in `dir' using exuberant ctags.
If `regenerate' is `nil', refuses to regenerate existing tags."
  (let ((tags-file (expand-file-name "TAGS" dir)))
    (when (file-newer-than-file-p dir tags-file)
      (if (and (file-exists-p tags-file)
               (not regenerate))
          (message (format "tags file exists in %s, but is out of date" dir))
        (message (format "generating tags file in %s" dir))
        (shell-command
         (format
          "cd %s && etags --version | grep 'Exuberant Ctags' && etags -R"
          dir))))))

(defun auto-tag (&optional regenerate)
  "Automatically select tags tables when we're in version
control. Always replaces tags tables instead of adding them."
  (interactive "p")
  (when (vc-registered (buffer-file-name))
    (exuberant-tags (vc-root) regenerate)))

(defun find-tags-file ()
  "Search upwards in the directory heirarchy for a tags file.  If
no tags file is found, ask for one, and if it doesn't exist,
create one."
  (when buffer-file-name
    (let ((current (file-name-directory buffer-file-name)))
      (while (and (not (file-exists-p (concat current "TAGS")))
                  (not (string= "/" current)))
        (setq current (file-name-directory (directory-file-name current))))
      (when (not (string= "/" current)) (concat current "TAGS")))))

(defun require-or-nil (feature)
  "If `feature' exists, require it, else return `nil'."
  (if (locate-library (format "%s" feature))
      (require feature)
    nil))

(defun rdiff ()
  (interactive)
  (let ((diff-switches
         (concat diff-switches " -r " (mapconcat
                                       (lambda (x) (concat "-x " x))
                                       vc-directory-exclusion-list " "))))
    (call-interactively 'diff)))

(autoload 'which-function "which-func")
(defun which-func ()
  "Mention the current function name in the echo area."
  (interactive)
  (message (concat "Current function: " (which-function))))

(defun git-grep ()
  (interactive)
  (let ((grep-host-defaults-alist
         '((nil
            (grep-command "git --no-pager grep -nH -e ")
            (grep-template "git --no-pager grep <C> -nH -e <R> -- '<F>'"))))
        (grep-files-aliases (cons '("any" . "*") grep-files-aliases)))
    (call-interactively 'lgrep)))

;;; Display
;; Remove startup message
(setq inhibit-startup-message t)

;; Hide menu bar, tool bar, and scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)

;; Disable blinking cursor and silence the bell
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

;; The default mode line likes to be too wide
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Display the time and date
(setq display-time-day-and-date t)
(set-variable 'display-time-load-average nil)
(set-variable 'display-time-format "%Y-%m-%d %H:%M")
(display-time)

;; Compilation window
(setq compilation-window-height 20)
(setq compilation-scroll-output t)
(setq display-buffer-reuse-frames t)

(defvar local-font "DejaVu Sans Mono-9" "The font we'd like to use")
(add-to-list 'default-frame-alist `(font . ,local-font))

(defun fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(when (fboundp 'load-theme)
  (let ((site-lisp (concat "~" init-file-user "/.emacs.d/site-lisp/")))
    (set-variable 'custom-theme-directory site-lisp))
  ;; We avoid the second arg to load-theme for 22/23 support
  (let ((custom-safe-themes t))
    (load-theme 'zenburn))
  ;; Hack to get around the fact that inherit doesn't mean what it used to...
  (load-library "zenburn-theme"))

;;; Behaviour
;; Single character yes/no prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ignore case searching,  preserve case replacing
(setq case-fold-search t)
(setq case-replace t)

;; Enforce case when searching for expansions
(set-variable 'dabbrev-case-fold-search nil)

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Inhibit backup files
(setq backup-inhibited t)

;; If the mark is transient, we want delete selection, but we don't
;; normally want a transient mark
(delete-selection-mode 1)
(transient-mark-mode -1)
(setq-default shift-select-mode t)

;; Moving between lines visually is perhaps saner, but breaks
;; macros that deal with multiple lines
(set-variable 'line-move-visual nil)

;; Focus follows mouse
(set-variable 'mouse-autoselect-window t)

;; Enable the mouse wheel
(mouse-wheel-mode 1)
(set-variable 'mouse-wheel-progressive-speed nil)
(set-variable 'mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . 20)))

;; If this isn't nil, there's a chance one of those terrible gtk file
;; dialogs may show up.
(set-variable 'use-file-dialog nil)

;; Partial completion. No longer needed in 24
(when (boundp 'partial-completion-mode)
  (set-variable 'partial-completion-mode t))

;; Tramp
(when (require-or-nil 'tramp)
  (set-variable 'tramp-default-method "ssh"))

;; Unified diffs
(setq-default diff-switches "-up")
(set-variable 'diff-default-read-only nil)

;; Ediff: no extra frames, split horizontally for wide windows
(set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)
(set-variable 'ediff-split-window-function
              (lambda (&optional arg)
                (if (> (frame-width) 160)
                    (split-window-horizontally arg)
                  (split-window-vertically arg))))

;; Persistence
(when (require-or-nil 'saveplace)
  (setq-default save-place t))
;; emacs21 doesn't have savehist-mode
(when (boundp 'savehist-mode)
  (savehist-mode 1))

;; Always syntax highlight
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Look for an tags table when we need one
(set-variable 'tags-add-tables nil)
(set-variable 'default-tags-table-function 'find-tags-file)

;; Delete trailing whitespace on save
(defvar whitespace-trailing-lines-threshold 10)
(defvar whitespace-delete-on-save t)
(make-variable-buffer-local 'whitespace-delete-on-save)
(defun whitespace-maybe-delete-trailing ()
  (when whitespace-delete-on-save (delete-trailing-whitespace)))
(defun whitespace-toggle-delete-on-save ()
  (save-excursion
    (goto-char (point-min))
    (let ((max whitespace-trailing-lines-threshold))
      (set-variable 'whitespace-delete-on-save
                    (not (re-search-forward "\\s-$" nil t max))))))
(add-hook 'find-file-hook 'whitespace-toggle-delete-on-save)
(add-hook 'before-save-hook 'whitespace-maybe-delete-trailing)

;; Update time stamps if they exist
(add-hook 'before-save-hook 'time-stamp)

;; Global indentation rules
(setq-default indent-tabs-mode nil
              standard-indent 4)

;; Ensure newline at EOF
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(set-variable 'next-line-extends-end-of-buffer nil)

;; fill-paragraph should recognize lists, like "1.", "ii)", etc.
;; Otherwise, this is the default from emacs23 with some unicode
;; characters removed, because having them impeded me from using this
;; in emacs21 and emacs22.
(set-variable 'adaptive-fill-regexp
              "[ \t]*\\(\\(\\w+[.)]\\|[-!|#%;>*]+\\)[ \t]*\\)*")

;; File names like foo<1> aren't terribly helpful
(require 'uniquify)
(set-variable 'uniquify-buffer-name-style 'post-forward)

;; Act as a server unless a server is already running. Old emacs
;; doesn't have server-running-p, but we don't like their server
;; anyway.
(require 'server)
(when (and (fboundp 'server-running-p)
           (not (server-running-p)))
  (server-start))

;;; Key bindings
;; Compilation --- prefix \C-cc to prompt for a compile command
(setq-default compilation-read-command nil)
(defun compile-with-buffer-in-env (command)
  (interactive "p")
  (setenv "buffer" (file-relative-name (buffer-file-name)))
  (call-interactively 'compile))
(global-set-key (kbd "C-c b") 'compile-with-buffer-in-env)

;; dabbrev-completion is more bash-like than dabbrev-expand
(global-set-key (kbd "M-/") (lambda ()
                              (interactive)
                              (dabbrev-completion)
                              (fit-windows)))

;; TODO: think of a better shortcut here
(global-set-key (kbd "M-]") 'fit-windows)

(global-set-key (kbd "C-\;") 'comment-or-uncomment-dwim)

;; C-z in a window system is confusing and useless
;; Don't bother if we don't have suspend-frame
(when (fboundp 'suspend-frame)
  (global-set-key (kbd "C-z") (lambda ()
                                (interactive)
                                (if window-system
                                    (message "i'm afraid i can't do that dave")
                                  (suspend-frame)))))

;; find the file or url at the point, if possible
(global-set-key (kbd "C-<down-mouse-1>") 'ffap-at-mouse)

(global-set-key (kbd "M-n") (lambda (arg)
                              (interactive "p")
                              (line-move-visual (or arg 1))))

(global-set-key (kbd "M-p") (lambda (arg)
                              (interactive "p")
                              (line-move-visual (- (or arg 1)))))

(let ((buffer-function (cond ((fboundp 'ibuffer) 'ibuffer)
                             ((fboundp 'buffer-menu) 'buffer-menu)
                             (t 'list-buffers))))
  (global-set-key (kbd "C-x C-b") buffer-function)
  (define-key mode-line-buffer-identification-keymap
    [mode-line mouse-2] buffer-function))

(eval-when-compile (defun ibuffer-switch-to-saved-filter-groups (name)))
(defvar local-ibuffer-filter-groups '() "Extra ibuffer filter groups")
(when (fboundp 'ibuffer)
  (let ((default-filter-group
          `(("config" (filename . ,(concat "^" (expand-file-name "~") "/\\.")))
            ("docs" (or
                     (mode . Info-mode)
                     (mode . apropos-mode)
                     (mode . help-mode)
                     (mode . Man-mode)))
            ("tmp" (filename . ,(concat "^" (expand-file-name "~/tmp/"))))
            ("gnus" (or
                     (mode . bbdb-mode)
                     (mode . gnus-article-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . message-mode)
                     (name . "^\\.bbdb$")
                     (name . "^\\.newsrc-dribble")))
            ("vc buffers" (or
                           (name . "^\\*vc-")
                           (mode . vc-annotate-mode)))
            ("tramp shells" (name . "^\\*tramp/"))
            ("files" (filename . "")))))
    (set-variable 'ibuffer-saved-filter-groups
                  (append
                   (list (cons "default" default-filter-group))
                   (mapcar
                    (lambda (g)
                      (cons (car g) (append (cdr g) default-filter-group)))
                    local-ibuffer-filter-groups))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

;; Flip isearch and isearch-regexp, since I always want the latter.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; modes

; shut the byte-compiler up about keymaps
(eval-when-compile
  (defvar outline-minor-mode-map)
  (defvar c-mode-base-map))

(unless (require-or-nil 'diminish)
  (defun diminish (x y) nil))

(setq-default outline-minor-mode 1)
(when (require-or-nil 'outline)
  (defvar outline-short-prefix-map
    (let ((map (make-sparse-keymap)))
      (define-key map "@" 'outline-mark-subtree)
      (define-key map "n" 'outline-next-visible-heading)
      (define-key map "p" 'outline-previous-visible-heading)
      (define-key map "i" 'show-children)
      (define-key map "s" 'show-subtree)
      (define-key map "d" 'hide-subtree)
      (define-key map "u" 'outline-up-heading)
      (define-key map "f" 'outline-forward-same-level)
      (define-key map "b" 'outline-backward-same-level)
      (define-key map "t" 'hide-body)
      (define-key map "a" 'show-all)
      (define-key map "c" 'hide-entry)
      (define-key map "e" 'show-entry)
      (define-key map "l" 'hide-leaves)
      (define-key map "k" 'show-branches)
      (define-key map "q" 'hide-sublevels)
      (define-key map "o" 'hide-other)
      (define-key map "^" 'outline-move-subtree-up)
      (define-key map "v" 'outline-move-subtree-down)
      (define-key map "<" 'outline-promote)
      (define-key map ">" 'outline-demote)
      (define-key map "m" 'outline-insert-heading)
      map))
  (define-key outline-minor-mode-map (kbd "C-c o") outline-short-prefix-map)
  (diminish 'outline-minor-mode ""))

(when (require-or-nil 'filladapt)
  (setq-default filladapt-mode 1)
  (diminish 'filladapt-mode ""))

(when (not (fboundp 'subword-mode))
  (defalias 'subword-mode 'c-subword-mode))

;; Highlight TODO
(let ((todo-modes '(c-mode c++-mode csharp-mode java-mode asm-mode
                    common-lisp-mode emacs-lisp-mode lisp-mode haskell-mode
                    perl-mode php-mode python-mode ruby-mode
                    apache-mode nxml-mode css-mode
                    latex-mode tex-mode)))
  (dolist (mode todo-modes)
    (font-lock-add-keywords
     mode
     '(("\\<\\(TODO\\|FIXME\\):" 1 font-lock-warning-face t)))))

(when (require-or-nil 'xcscope)
  (set-variable 'cscope-do-not-update-database 't)

  ;; The [return] binding in xcscope.el doesn't seem to work in terminals
  (define-key cscope-list-entry-keymap (kbd "RET")
    'cscope-select-entry-other-window)

  (defadvice cscope:hook (after adjust-keybindings)
    (local-set-key "\M-." 'cscope-find-global-definition)
    (local-set-key "\M-*" 'cscope-pop-mark))
  (ad-activate 'cscope:hook)

  ;; cscope sets itself up for c and c++, but we have pycscope as well
  (add-hook 'python-mode-hook (function cscope:hook)))

;; C modes
(require 'c-styles)

(setq c-default-style '((awk-mode . "awk")
                        (other . "bogner")))
(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

(when (require-or-nil 'haskell-mode-autoloads)
  ;; Hooks to make haskell mode behave
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;; Some eye candy for haskell-mode. #X03BB is a lambda.
(font-lock-add-keywords
 'haskell-mode
 '(("\\(\\\\\\)\\(?: ?[A-Za-z_][A-Za-z0-9_]*\\)+ ?->"
    (0
     (when (char-displayable-p #X03BB)
       (compose-region (match-beginning 1) (match-end 1) #X03BB))
     'prepend))))

;; Configure geiser for scheme
(when (require-or-nil 'geiser)
  (set-variable 'geiser-repl-use-other-window nil))

;; Tabs should only look like 4 spaces in go
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

;; Set up modes that will be autoloaded
(autoload 'csharp-mode "csharp-mode")
(autoload 'd-mode "d-mode")

(defalias 'sgml-mode 'nxml-mode)
(defalias 'html-mode 'nxml-mode)
(defalias 'xml-mode 'nxml-mode)
;; For some reason nxml-mode doesn't want to bind alt-tab
(set-variable 'nxml-bind-meta-tab-to-complete-flag t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
(add-to-list 'auto-mode-alist
             '("\\.\\(aspx\\|xsl\\|xhtml\\|xsd\\|svg\\|rss\\)\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.arm\\'" . asm-mode))

;;; Web Browsers
(when (eq window-system 'x)
  ; default-browser tends to do something silly in X11
  (set-variable 'browse-url-browser-function 'browse-url-generic)
  (set-variable 'browse-url-generic-program "x-www-browser"))

;;; w3m
(set-variable 'w3m-use-cookies t)
(set-variable 'w3m-key-binding 'info)
(set-variable 'w3m-default-display-inline-images t)
(set-variable 'mm-w3m-safe-url-regexp nil)

;; Some great w3m hackery
(eval-when-compile (defun w3m-view-this-url ()))
(defun w3m-wiki-login ()
  (interactive)
  (goto-char (point-max))
  (when (search-backward "Log In" nil t)
    (w3m-view-this-url)))

(defun w3m-wiki-edit ()
  (interactive)
  (goto-char (point-max))
  (when (search-backward "Edit" nil t)
    (w3m-view-this-url)))

;;; Gnus / Message
(eval-when-compile
  (defun gnus-buffer-live-p (buf))
  (defun message-fetch-field (field))
  (defun gnus-summary-insert-old-articles (n))
  (defvar gnus-article-buffer)
  (defvar gnus-group-mode-map))

(defun infer-from-article (domain)
  (if (gnus-buffer-live-p gnus-article-buffer)
      (with-current-buffer gnus-article-buffer
        (save-excursion
          (let ((re (concat "\\("
                            "\\(\"[A-Za-z0-9]+,? [A-Za-z0-9]+\"\\) *"
                            "\\|"
                            "\\( *[A-Za-z0-9 ]+\\) *"
                            "<\\)?"
                            "\\([A-Za-z0-9_.-]+@"
                            domain
                            "\\)>?"))
                (to (message-fetch-field "to"))
                (cc (message-fetch-field "cc")))
            (cond ((and to (string-match re to)) (match-string 4 to))
                  ((and cc (string-match re cc)) (match-string 4 cc))
                  (t user-mail-address)))))
    user-mail-address))

(defun gnus-group-with-recent (select-fn &optional all)
  "Select a newsgroup using select-fn, but add some recent articles as well"
  (funcall select-fn all)
  (unless all
    (gnus-summary-insert-old-articles (/ (* 2 (frame-height)) 3))))

(defun gnus-group-select-recent (&optional all)
  "Select this newsgroup, displaying some recent articles"
  (interactive "P")
  (gnus-group-with-recent 'gnus-group-select-group all))

(defun gnus-group-read-recent (&optional all)
  "read this newsgroup, displaying some recent articles"
  (interactive "P")
  (gnus-group-with-recent 'gnus-group-read-group all))

(when (require-or-nil 'gnus)
  (setq mail-user-agent 'gnus-user-agent)

  (set-variable 'gnus-gcc-mark-as-read t)

  ;; Appearance
  (set-variable 'gnus-group-mode-line-format "Gnus: %%b")
  (set-variable 'gnus-buttonized-mime-types '("multipart/alternative"
                                              "multipart/mixed"))
  (set-variable 'mm-text-html-renderer 'w3m)

  (set-variable 'gnus-treat-unsplit-urls t)
  (set-variable 'gnus-treat-date-local 'head)

  ;; Address magic
  (set-variable 'message-subscribed-address-functions
                '(gnus-find-subscribed-addresses))

  (define-key gnus-group-mode-map (kbd "\r") 'gnus-group-select-recent)
  (define-key gnus-group-mode-map (kbd "<SPC>") 'gnus-group-read-recent)
  (define-key gnus-article-mode-map (kbd "w") 'gnus-article-fill-long-lines)
  (define-key gnus-summary-mode-map (kbd "w") 'gnus-article-fill-long-lines))

(eval-when-compile
  (defun bbdb-initialize (&rest to-insinuate))
  (defun bbdb-insinuate-gnus ())
  (defun bbdb-insinuate-message ()))
(when (require-or-nil 'bbdb)
  (bbdb-initialize 'gnus 'message)
  (bbdb-insinuate-gnus)
  (bbdb-insinuate-message)
  (set-variable 'bbdb-always-add-addresses nil)
  (set-variable 'bbdb-complete-name-allow-cycling t)
  (set-variable 'bbdb-expand-mail-aliases t)
  (set-variable 'bbdb-quiet-about-name-mismatches 1)
  (set-variable 'bbdb-use-pop-up nil)
  (set-variable 'bbdb-dwim-net-address-allow-redundancy t)
  (add-hook 'message-setup-hook 'bbdb-define-all-aliases))

(when (require-or-nil 'mm-uu)
  (eval-when-compile (defun mm-uu-configure ()))

  (set-variable 'mm-uu-diff-groups-regexp ".*")
  (when (boundp 'mm-uu-type-alist)
    (assq-delete-all 'diff mm-uu-type-alist)
    (add-to-list 'mm-uu-type-alist
                 '(diff "^Index: \\|^diff \\|^--- [A-Za-z0-9_/.-]+"
                        nil
                        mm-uu-diff-extract
                        nil
                        mm-uu-diff-test)))
  (mm-uu-configure))

(defun message-outlook-yank ()
  (interactive)
  (let ((message-cite-function 'message-outlook-cite))
    (message-yank-original)))

(defun message-outlook-cite ()
  "Cite an original message in a horrible outlook style."
  (save-excursion
    (insert-string "-----Original Message-----\n")
    (while (not (looking-at " *$"))
      (cond ((looking-at "From:\\|To:\\|C[Cc]:")
             (save-restriction
               (narrow-to-region (point)
                                 (save-excursion (forward-line 1) (point)))
               (while (search-forward-regexp "\"\\([^\"]+\\)\" <[^<]+>" nil t)
                 (replace-match "\\1" nil nil)))
             (forward-line 1))
            ((not (looking-at "Date:\\|Subject:"))
             (delete-region (point)
                            (save-excursion (forward-line 1) (point))))
            (t (forward-line 1))))))

;; darcs defines it's own media type for patches.
(eval-when-compile (defvar mm-inline-media-tests))
(when (require-or-nil 'mm-decode)
  (add-to-list 'mm-inline-media-tests
               '("text/x-darcs-patch" mm-display-patch-inline
                 (lambda (handle) (fboundp 'diff-mode)))))

;;; diary
(require 'diary-lib)

(setq diary-file "~/.calendar")
(setq calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      diary-number-of-entries 7)
(set-variable 'diary-display-function 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

; the byte compiler likes to complain about appt-display-duration
(eval-when-compile (defvar appt-display-duration))
(defun appt-notify-script (min-to-app new-time appt-msg)
  "Display appointment due in MIN-TO-APP (a string) minutes.
NEW-TIME is a string giving the date. Displays the appointment
message APPT-MSG using an external script"
  (let* ((words (split-string appt-msg))
         (splitat (if (string= "-" (cadr words)) 3 1))
         (header (butlast words (- (length words) splitat)))
         (body (nthcdr splitat words)))
    (shell-command
     (concat (format "notify-send -t %d " (* 1000 appt-display-duration))
             (format "\"Appointment in %s minute(s)\" \"%s\n%s\""
                     min-to-app
                     (combine-and-quote-strings header)
                     (combine-and-quote-strings body))))))

(set-variable 'appt-disp-window-function 'appt-notify-script)
(set-variable 'appt-display-mode-line nil)
(set-variable 'appt-display-diary nil)
(set-variable 'appt-display-duration 180)

(defun diary-print-batch (&optional ndays)
  "Print the diary to stdout in batch mode"
  (interactive "")
  (let ((diary-display-function 'diary-fancy-display))
    (diary-list-entries (calendar-current-date)
                        (or ndays diary-number-of-entries)))
  (message (if (get-buffer diary-fancy-buffer)
               (with-current-buffer diary-fancy-buffer (buffer-string))
             "No entries found")))
