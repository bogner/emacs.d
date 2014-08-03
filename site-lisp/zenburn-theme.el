(deftheme zenburn
  "Just some alien fruit salad to keep you in the zone.")

(defvar zenburn-fg "#dcdccc")
(defvar zenburn-bg "#3f3f3f")
(defvar zenburn-bg+1 "#4f4f4f")
(defvar zenburn-bg+2 "#5f5f5f")
(defvar zenburn-red+1 "#dca3a3")
(defvar zenburn-red "#cc9393")
(defvar zenburn-red-1 "#bc8383")
(defvar zenburn-red-2 "#ac7373")
(defvar zenburn-red-3 "#9c6363")
(defvar zenburn-red-4 "#8c5353")
(defvar zenburn-orange "#dfaf8f")
(defvar zenburn-yellow "#f0dfaf")
(defvar zenburn-yellow-1 "#e0cf9f")
(defvar zenburn-yellow-2 "#d0bf8f")
(defvar zenburn-green-1 "#5f7f5f")
(defvar zenburn-green "#7f9f7f")
(defvar zenburn-green+1 "#8fb28f")
(defvar zenburn-green+2 "#9fc59f")
(defvar zenburn-green+3 "#afd8af")
(defvar zenburn-green+4 "#bfebbf")
(defvar zenburn-cyan "#93e0e3")
(defvar zenburn-blue+1 "#94bff3")
(defvar zenburn-blue "#8cd0d3")
(defvar zenburn-blue-1 "#7cb8bb")
(defvar zenburn-blue-2 "#6ca0a3")
(defvar zenburn-blue-3 "#5c888b")
(defvar zenburn-blue-4 "#4c7073")
(defvar zenburn-magenta "#dc8cc3")

(defgroup zenburn-faces nil
  "Faces used internally to define the zenburn theme"
  :group 'faces)

(defface zenburn-primary-1
  `((t (:foreground ,zenburn-yellow :weight bold)))
  "primary 1" :group 'zenburn-faces)
(defface zenburn-primary-2
  `((t (:foreground ,zenburn-orange :weight bold)))
  "primary 2" :group 'zenburn-faces)
(defface zenburn-primary-3
  '((t (:foreground "#dfdfbf" :weight bold)))
  "primary 3" :group 'zenburn-faces)
(defface zenburn-primary-4
  '((t (:foreground "#dca3a3" :weight bold)))
  "primary 4" :group 'zenburn-faces)
(defface zenburn-primary-5
  '((t (:foreground "#94bff3" :weight bold)))
  "primary 5" :group 'zenburn-faces)
(defface zenburn-highlight-damp
  '((t (:foreground "#88b090" :background "#2e3330")))
  "damp highlight" :group 'zenburn-faces)
(defface zenburn-highlight-alerting
  '((t (:foreground "#e37170" :background "#332323")))
  "alerting highlight" :group 'zenburn-faces)
(defface zenburn-highlight-subtle
  '((t (:background "#464646")))
  "subtle highlight" :group 'zenburn-faces)
(defface zenburn-lowlight-1
  '((t (:foreground "#606060")))
  "lowlight 1" :group 'zenburn-faces)
(defface zenburn-lowlight-2
  '((t (:foreground "#708070")))
  "lowlight 2" :group 'zenburn-faces)
(defface zenburn-yellow
  `((t (:foreground ,zenburn-yellow)))
  "yellow" :group 'zenburn-faces)
(defface zenburn-orange
  `((t (:foreground ,zenburn-orange)))
  "orange" :group 'zenburn-faces)
(defface zenburn-red
  `((t (:foreground ,zenburn-red)))
  "red" :group 'zenburn-faces)
(defface zenburn-green-1
  `((t (:foreground ,zenburn-green-1)))
  "green-1" :group 'zenburn-faces)
(defface zenburn-green
  `((t (:foreground ,zenburn-green)))
  "green" :group 'zenburn-faces)
(defface zenburn-green+1
  `((t (:foreground ,zenburn-green+1)))
  "green+1" :group 'zenburn-faces)
(defface zenburn-green+2
  `((t (:foreground ,zenburn-green+2)))
  "green+2" :group 'zenburn-faces)
(defface zenburn-green+3
  `((t (:foreground ,zenburn-green+3)))
  "green+3" :group 'zenburn-faces)
(defface zenburn-green+4
  `((t (:foreground ,zenburn-green+4)))
  "green+4" :group 'zenburn-faces)
(defface zenburn-blue
  `((t (:foreground ,zenburn-blue)))
  "blue" :group 'zenburn-faces)
(defface zenburn-blue-1
  `((t (:foreground ,zenburn-blue-1)))
  "blue-1" :group 'zenburn-faces)
(defface zenburn-blue-2
  `((t (:foreground ,zenburn-blue-2)))
  "blue-2" :group 'zenburn-faces)
(defface zenburn-blue-3
  `((t (:foreground ,zenburn-blue-3)))
  "blue-3" :group 'zenburn-faces)
(defface zenburn-blue-4
  `((t (:foreground ,zenburn-blue-4)))
  "blue-4" :group 'zenburn-faces)
(defface zenburn-title
  '((t (:inherit variable-pitch :weight bold)))
  "title" :group 'zenburn-faces)

(set-variable 'vc-annotate-background zenburn-bg)
(set-variable 'vc-annotate-color-map
              `(( 20 . ,zenburn-red)
                ( 40 . ,zenburn-red-1)
                ( 60 . ,zenburn-red-2)
                ( 80 . ,zenburn-red-3)
                (100 . ,zenburn-orange)
                (120 . ,zenburn-yellow)
                (140 . ,zenburn-yellow-1)
                (160 . ,zenburn-yellow-2)
                (180 . ,zenburn-green-1)
                (200 . ,zenburn-green)
                (220 . ,zenburn-green+1)
                (240 . ,zenburn-green+2)
                (260 . ,zenburn-green+3)
                (280 . ,zenburn-green+4)
                (300 . ,zenburn-cyan)
                (320 . ,zenburn-blue)
                (340 . ,zenburn-blue-1)
                (360 . ,zenburn-blue-2)))

(set-variable 'ansi-term-color-vector
              [unspecified "#3f3f3f" "#bc8383" "#7f9f7f" "#e0cf9f"
               "#8cc0ff" "#cc6ca3" "#73e0c3" "#dcdccc"])

;; TODO: find a better way to do this...
(setq gnus-mode-line-image-cache
      '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #dcdccc\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\"
};"))

(custom-theme-set-faces
 'zenburn
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:italic t :weight bold))))
 `(default ((t (:background ,zenburn-bg :foreground ,zenburn-fg))))
 '(fixed-pitch ((t (:weight bold))))
 '(italic ((t (:slant italic))))
 '(underline ((t (:underline t))))
 ;; '(variable-pitch ((t (:font "-*-utopia-regular-r-*-*-12-*-*-*-*-*-*-*"))))

 '(font-lock-builtin-face
   ((t (:inherit zenburn-blue))))
 '(font-lock-comment-face
   ((t (:inherit zenburn-green))))
 '(font-lock-comment-delimiter-face
   ((t (:inherit zenburn-lowlight-2))))
 '(font-lock-constant-face
   ((t (:inherit zenburn-primary-4))))
 '(font-lock-doc-face
   ((t (:inherit zenburn-green+1))))
 `(font-lock-function-name-face
   ((t (:foreground ,zenburn-yellow))))
 '(font-lock-keyword-face
   ((t (:inherit zenburn-primary-1))))
 '(font-lock-negation-char-face
   ((t (:inherit zenburn-primary-1))))
 '(font-lock-preprocessor-face
   ((t (:inherit zenburn-blue))))
 '(font-lock-string-face
   ((t (:inherit zenburn-red))))
 '(font-lock-type-face
   ((t (:inherit zenburn-primary-3))))
 `(font-lock-variable-name-face
   ((t (:foreground ,zenburn-yellow))))
 '(font-lock-warning-face
   ((t (:inherit zenburn-highlight-alerting))))

 '(font-lock-pseudo-keyword-face
   ((t (:inherit zenburn-primary-2))))
 '(font-lock-operator-face
   ((t (:inherit zenburn-primary-3))))

 '(term-default-bg ((t (nil))))
 '(term-default-bg-inv ((t (nil))))
 '(term-default-fg ((t (nil))))
 '(term-default-fg-inv ((t (nil))))
 '(term-invisible ((t (nil)))) ;; FIXME: Security risk?
 '(term-invisible-inv  ((t (nil))))
 '(term-bold ((t (:weight bold))))
 '(term-underline ((t (:underline t))))

 ;; FIXME: Map these to ansi-term's faces (`term-red', etc.).
 '(zenburn-term-dark-gray      ((t (:foreground "#709080"))))
 '(zenburn-term-light-blue     ((t (:foreground "#94bff3"))))
 '(zenburn-term-light-cyan     ((t (:foreground "#93e0e3"))))
 '(zenburn-term-light-green    ((t (:foreground "#c3bf9f"))))
 '(zenburn-term-light-magenta  ((t (:foreground "#ec93d3"))))
 '(zenburn-term-light-red      ((t (:foreground "#dca3a3"))))
 '(zenburn-term-light-yellow   ((t (:foreground "#f0dfaf"))))
 '(zenburn-term-white          ((t (:foreground "#ffffff"))))

 '(zenburn-term-black          ((t (:foreground "#000000"))))
 '(zenburn-term-dark-blue      ((t (:foreground "#506070"))))
 '(zenburn-term-dark-cyan      ((t (:foreground "#8cd0d3"))))
 '(zenburn-term-dark-green     ((t (:foreground "#60b48a"))))
 '(zenburn-term-dark-magenta   ((t (:foreground "#dc8cc3"))))
 '(zenburn-term-dark-red       ((t (:foreground "#705050"))))
 '(zenburn-term-dark-yellow    ((t (:foreground "#dfaf8f"))))
 `(zenburn-term-light-gray     ((t (:foreground ,zenburn-fg))))

 '(widget-button
   ((t (:weight bold))))
 `(widget-button-pressed
   ((t (:background ,zenburn-bg+1
                    :box (:line-width 2 :style pressed-button)))))
 `(widget-button-highlight
   ((t (:background ,zenburn-bg+1
                    :box (:line-width 2 :style released-button)))))
 `(widget-button-pressed-highlight
   ((t (:background ,zenburn-bg+1
                    :box (:line-width 2 :style pressed-button)))))
 '(widget-documentation
   ((t (:inherit font-lock-doc-face))))
 `(widget-field
   ((t (:background ,zenburn-bg+2))))
 '(widget-inactive
   ((t (:strike-through t))))
 `(widget-single-line-field
   ((t (:background ,zenburn-bg+2))))

 `(border ((t (:background ,zenburn-bg))))
 '(fringe ((t (:inherit zenburn-highlight-subtle))))
 '(header-line ((t (:inherit zenburn-highlight-damp
                             :box (:color "#2e3330" :line-width 2)))))
 '(mode-line ((t (:foreground "#acbc90" :background "#1e2320"
                              :box (:color "#1e2320" :line-width 2)))))
 '(mode-line-inactive ((t (:background "#2e3330" :foreground "#88b090"
                                       :box (:color "#2e3330" :line-width 2)))))
 `(minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
 `(Buffer-menu-buffer-face ((t (:inherit zenburn-primary-1))))

 '(region ((t (:foreground "#71d3b4" :background "#233323"))))
 `(secondary-selection ((t (:foreground ,zenburn-fg :background "#506070"))))

 '(trailing-whitespace ((t (:inherit font-lock-warning-face))))
 '(highlight ((t (:underline t))))
 '(paren-face ((t (:inherit zenburn-lowlight-1))))
 '(show-paren-mismatch-face ((t (:inherit font-lock-warning-face))))
 '(show-paren-match-face ((t (:inherit font-lock-keyword-face))))
 '(match ((t (:weight bold))))

 `(cursor ((t (:background ,zenburn-fg :foreground ,zenburn-bg))))
 '(hover-highlight ((t (:underline t :foreground "#f8f893"))))
 '(menu ((t nil)))
 '(mouse ((t (:inherit zenburn-foreground))))
 `(scroll-bar ((t (:background ,zenburn-bg+2))))
 `(tool-bar ((t (:background ,zenburn-bg+2))))

 '(ido-first-match-face ((t (:inherit zenburn-primary-1))))
 '(ido-only-match-face ((t (:inherit zenburn-primary-2))))
 `(ido-subdir-face ((t (:foreground ,zenburn-yellow))))

 `(isearch ((t (:foreground ,zenburn-fg :background "#506070"))))
 `(isearch-lazy-highlight-face
   ((t (:foreground ,zenburn-fg :background "#1e2320" :weight normal))))

 '(mtorus-highlight-face ((t (:inherit zenburn-highlight-bluish))))
 '(mtorus-notify-highlight-face ((t (:inherit zenburn-primary-1))))

 '(which-func ((t (:inherit mode-line))))

 '(apt-utils-normal-package-face
   ((t (:inherit zenburn-primary-1))))
 '(apt-utils-virtual-package-face
   ((t (:inherit zenburn-primary-2))))
 '(apt-utils-field-keyword-face
   ((t (:inherit font-lock-doc-face))))
 '(apt-utils-field-contents-face
   ((t (:inherit font-lock-comment-face))))
 '(apt-utils-summary-face
   ((t (:inherit bold))))
 '(apt-utils-description-face
   ((t (:inherit default))))
 '(apt-utils-version-face
   ((t (:inherit zenburn-blue))))
 '(apt-utils-broken-face
   ((t (:inherit font-lock-warning-face))))

 '(breakpoint-enabled-bitmap-face ((t (:inherit zenburn-primary-1))))
 '(breakpoint-disabled-bitmap-face ((t (:inherit font-lock-comment-face))))

 '(calendar-today ((t (:underline nil :inherit zenburn-primary-2))))
 '(diary ((t (:underline nil :inherit zenburn-primary-1))))
 '(holiday ((t (:underline t :inherit zenburn-primary-4))))

 '(bongo-unfilled-seek-bar ((t (:background "#606060"))))

 '(change-log-date-face ((t (:inherit zenburn-blue))))

 '(comint-highlight-input ((t (:inherit zenburn-primary-1))))
 '(comint-highlight-prompt ((t (:inherit zenburn-primary-2))))

 '(compilation-info ((t (:inherit zenburn-primary-1))))
 '(compilation-warning ((t (:inherit font-lock-warning-face))))

 '(cscope-file-face ((t (:inherit zenburn-primary-1))))
 '(cscope-function-face ((t (:inherit font-lock-function-name-face))))
 '(cscope-line-number-face ((t (:inherit font-lock-function-name-face))))
 '(cscope-line-face ((t (nil))))
 '(cscope-mouse-face ((t (:inherit hover-highlight))))
 '(cscope-separator-face ((t (:inherit font-lock-comment-face))))

 ;; TODO
 '(cua-rectangle-face ((t (:inherit region))))

 '(custom-button
   ((t (:inherit fancy-widget-button))))
 '(custom-button-pressed
   ((t (:inherit fancy-widget-button-pressed))))
 '(custom-changed
   ((t (:inherit zenburn-blue))))
 '(custom-comment
   ((t (:inherit font-lock-doc-face))))
 '(custom-comment-tag
   ((t (:inherit font-lock-doc-face))))
 '(custom-documentation
   ((t (:inherit font-lock-doc-face))))
 '(custom-link
   ((t (:inherit zenburn-yellow :underline t))))
 '(custom-tag
   ((t (:inherit zenburn-primary-2))))
 '(custom-group-tag
   ((t (:inherit zenburn-primary-1))))
 '(custom-group-tag-1
   ((t (:inherit zenburn-primary-4))))
 '(custom-invalid
   ((t (:inherit font-lock-warning-face))))
 '(custom-modified
   ((t (:inherit zenburn-primary-3))))
 '(custom-rogue
   ((t (:inhrit font-lock-warning-face))))
 '(custom-saved
   ((t (:underline t))))
 '(custom-set
   ((t (:inverse-video t :inherit zenburn-blue))))
 '(custom-state
   ((t (:inherit font-lock-comment-face))))
 '(custom-variable-button
   ((t (:weight bold :underline t))))
 '(custom-variable-tag
   ((t (:inherit zenburn-primary-2))))

 '(dictionary-button-face ((t (:inherit fancy-widget-button))))
 '(dictionary-reference-face ((t (:inherit zenburn-primary-1))))
 '(dictionary-word-entry-face ((t (:inherit font-lock-keyword-face))))

 '(diff-header ((t (:inherit zenburn-highlight-subtle))))
 '(diff-index ((t (:inherit bold))))
 '(diff-file-header ((t (:inherit bold))))
 '(diff-hunk-header ((t (:inherit zenburn-highlight-subtle))))

 '(diff-added ((t (:inherit zenburn-primary-3))))
 '(diff-removed ((t (:inherit zenburn-blue))))
 '(diff-context ((t (:inherit font-lock-comment-face))))
 `(diff-refine-change ((t (:background ,zenburn-bg+1))))

 `(emms-pbi-song-face ((t (:foreground ,zenburn-yellow))))
 '(emms-pbi-current-face ((t (:inherit zenburn-primary-1))))
 '(emms-pbi-mark-marked-face ((t (:inherit zenburn-primary-2))))

 '(erc-action-face ((t (:inherit erc-default-face))))
 '(erc-bold-face ((t (:weight bold))))
 '(erc-current-nick-face ((t (:inherit zenburn-primary-1))))
 '(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
 `(erc-default-face ((t (:foreground ,zenburn-fg))))
 '(erc-direct-msg-face ((t (:inherit erc-default-face))))
 '(erc-error-face ((t (:inherit font-lock-warning-face))))
 '(erc-fool-face ((t (:inherit zenburn-lowlight-1))))
 '(erc-highlight-face ((t (:inherit hover-highlight))))
 `(erc-input-face ((t (:foreground ,zenburn-yellow))))
 '(erc-keyword-face ((t (:inherit zenburn-primary-1))))
 '(erc-nick-default-face ((t (:inherit bold))))
 '(erc-nick-msg-face ((t (:inherit erc-default-face))))
 '(erc-notice-face ((t (:inherit zenburn-green))))
 '(erc-pal-face ((t (:inherit zenburn-primary-3))))
 '(erc-prompt-face ((t (:inherit zenburn-primary-2))))
 '(erc-timestamp-face ((t (:inherit zenburn-green+1))))
 '(erc-underline-face ((t (:inherit underline))))

 '(circe-highlight-nick-face ((t (:inherit zenburn-primary-1))))
 '(circe-my-message-face ((t (:inherit zenburn-yellow))))
 '(circe-originator-face ((t (:inherit bold))))
 '(circe-prompt-face ((t (:inherit zenburn-primary-1))))
 '(circe-server-face ((t (:inherit font-lock-comment-face))))

 '(rcirc-my-nick ((t (:inherit zenburn-primary-1))))
 '(rcirc-other-nick ((t (:inherit bold))))
 '(rcirc-bright-nick ((t (:foreground "white" :inherit rcirc-other-nick))))
 '(rcirc-dim-nick ((t (:inherit font-lock-comment-face))))
 '(rcirc-nick-in-message ((t (:inherit zenburn-primary-1))))
 '(rcirc-server ((t (:inherit font-lock-comment-face))))
 '(rcirc-server-prefix ((t (:inherit font-lock-comment-delimiter-face))))
 '(rcirc-timestamp ((t (:inherit font-lock-comment-face))))
 '(rcirc-prompt ((t (:inherit zenburn-primary-1))))
 '(rcirc-mode-line-nick ((t (:inherit zenburn-primary-1))))

 '(eshell-prompt-face ((t (:inherit zenburn-primary-1))))
 '(eshell-ls-archive-face ((t (:foreground "#c3bf9f" :weight bold))))
 '(eshell-ls-backup-face ((t (:inherit font-lock-comment-face))))
 '(eshell-ls-clutter-face ((t (:inherit font-lock-comment-face))))
 `(eshell-ls-directory-face ((t (:foreground ,zenburn-blue+1 :weight bold))))
 `(eshell-ls-executable-face ((t (:foreground ,zenburn-red+1 :weight bold))))
 '(eshell-ls-unreadable-face ((t (:inherit zenburn-lowlight-1))))
 '(eshell-ls-missing-face ((t (:inherit font-lock-warning-face))))
 '(eshell-ls-product-face ((t (:inherit font-lock-doc-face))))
 '(eshell-ls-special-face ((t (:inherit zenburn-primary-1))))
 `(eshell-ls-symlink-face ((t (:foreground ,zenburn-cyan :weight bold))))

 '(highlight-current-line-face ((t (:inherit zenburn-highlight-subtle))))

 '(ibuffer-deletion-face ((t (:inherit zenburn-primary-2))))
 '(ibuffer-marked-face ((t (:inherit zenburn-primary-1))))
 '(ibuffer-special-buffer-face ((t (:inherit font-lock-doc-face))))
 '(ibuffer-help-buffer-face ((t (:inherit font-lock-comment-face))))

 '(message-cited-text ((t (:inherit font-lock-comment-face))))
 ;;`(message-cited-text-face ((t (:foreground ,zenburn-blue))))
 '(message-header-name ((t (:inherit zenburn-green+1))))
 '(message-header-other ((t (:inherit zenburn-green))))
 '(message-header-to ((t (:inherit zenburn-primary-1))))
 '(message-header-from ((t (:inherit zenburn-primary-1))))
 '(message-header-cc ((t (:inherit zenburn-primary-1))))
 '(message-header-newsgroups ((t (:inherit zenburn-primary-1))))
 '(message-header-subject ((t (:inherit zenburn-primary-2))))
 '(message-header-xheader ((t (:inherit zenburn-green))))
 '(message-mml ((t (:inherit zenburn-primary-1))))
 '(message-separator ((t (:inherit font-lock-comment-face))))

 '(mm-uu-extract ((t (:inherit zenburn-highlight-damp))))

 '(gnus-header-name ((t (:inherit message-header-name-face))))
 '(gnus-header-content ((t (:inherit message-header-other-face))))
 '(gnus-header-from ((t (:inherit message-header-from-face))))
 '(gnus-header-subject ((t (:inherit message-header-subject-face))))
 '(gnus-header-newsgroups ((t (:inherit message-header-other-face))))

 ;; (gnus-cite-1 ((t (:inherit message-cited-text))))
 `(gnus-cite-1 ((t (:foreground ,zenburn-blue))))
 `(gnus-cite-2 ((t (:foreground ,zenburn-blue-1))))
 `(gnus-cite-3 ((t (:foreground ,zenburn-blue-2))))
 ;;      (gnus-cite-4 ((t (:foreground ,zenburn-blue-3))))
 ;;      (gnus-cite-5 ((t (:foreground ,zenburn-blue-4))))
 ;;      (gnus-cite-6 ((t (:foreground ,zenburn-red-4))))
 ;;      (gnus-cite-5 ((t (:foreground ,zenburn-red-3))))
 `(gnus-cite-4 ((t (:foreground ,zenburn-green+2))))
 `(gnus-cite-5 ((t (:foreground ,zenburn-green+1))))
 `(gnus-cite-6 ((t (:foreground ,zenburn-green))))
 `(gnus-cite-7 ((t (:foreground ,zenburn-red))))
 `(gnus-cite-8 ((t (:foreground ,zenburn-red-1))))
 `(gnus-cite-9 ((t (:foreground ,zenburn-red-2))))
 `(gnus-cite-10 ((t (:foreground ,zenburn-yellow-1))))
 `(gnus-cite-11 ((t (:foreground ,zenburn-yellow))))

 `(gnus-group-news-1-empty ((t (:foreground ,zenburn-yellow))))
 `(gnus-group-news-2-empty ((t (:foreground ,zenburn-green+3))))
 `(gnus-group-news-3-empty ((t (:foreground ,zenburn-green+1))))
 `(gnus-group-news-4-empty ((t (:foreground ,zenburn-blue-2))))
 `(gnus-group-news-5-empty ((t (:foreground ,zenburn-blue-3))))
 `(gnus-group-news-6-empty ((t (:inherit zenburn-lowlight-1))))
 `(gnus-group-news-low-empty ((t (:inherit zenburn-lowlight-1))))

 '(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty-face))))
 '(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty-face))))
 '(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty-face))))
 '(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
 '(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
 '(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
 '(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))

 '(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty-face))))
 '(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty-face))))
 '(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty-face))))
 '(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
 '(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
 '(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
 '(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))

 '(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty-face))))
 '(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty-face))))
 '(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty-face))))
 '(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
 '(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
 '(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
 '(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))

 `(gnus-signature ((t (:foreground ,zenburn-yellow))))

 '(gnus-summary-selected
   ((t (:inherit zenburn-primary-1))))
 '(gnus-summary-cancelled
   ((t (:inherit zenburn-highlight-alerting))))

 '(gnus-summary-low-ticked
   ((t (:inherit zenburn-primary-2))))
 '(gnus-summary-normal-ticked
   ((t (:inherit zenburn-primary-2))))
 '(gnus-summary-high-ticked
   ((t (:inherit zenburn-primary-2))))

 '(gnus-summary-low-unread
   ((t (:inherit zenburn-foreground :weight normal))))
 '(gnus-summary-normal-unread
   ((t (:inherit zenburn-foreground :weight normal))))
 '(gnus-summary-high-unread
   ((t (:inherit zenburn-foreground :weight bold))))

 '(gnus-summary-low-read
   ((t (:inherit zenburn-green :weight normal))))
 '(gnus-summary-normal-read
   ((t (:inherit zenburn-green :weight normal))))
 '(gnus-summary-high-read
   ((t (:inherit zenburn-green :weight bold))))

 '(gnus-summary-low-ancient
   ((t (:inherit zenburn-blue :weight normal))))
 '(gnus-summary-normal-ancient
   ((t (:inherit zenburn-blue :weight normal))))
 '(gnus-summary-high-ancient
   ((t (:inherit zenburn-blue))))

 '(help-argument-name ((t (:weight bold))))

 ;; See also the variable definitions at the top of this file
 '(imaxima-latex-error-face ((t (:inherit font-lock-warning-face))))

 `(info-xref ((t (:foreground ,zenburn-yellow :weight bold))))
 '(info-xref-visited ((t (:inherit info-xref :weight normal))))
 '(info-header-xref ((t (:inherit info-xref))))
 `(info-menu-star ((t (:foreground ,zenburn-orange :weight bold))))
 `(info-menu-5 ((t (:inherit info-menu-star))))
 '(info-node ((t (:weight bold))))
 '(info-header-node ((t (:weight normal))))

 '(jabber-roster-user-chatty
   ((t (:inherit zenburn-primary-1))))
 '(jabber-roster-user-online
   ((t (:inherit zenburn-primary-2))))
 '(jabber-roster-user-away
   ((t (:inherit font-lock-doc-face))))
 '(jabber-roster-user-xa
   ((t (:inherit font-lock-comment-face))))
 '(jabber-roster-user-offline
   ((t (:inherit zenburn-lowlight-1))))
 '(jabber-roster-user-dnd
   ((t (:inherit zenburn-primary-5))))
 '(jabber-roster-user-error
   ((t (:inherit font-lock-warning-face))))

 '(jabber-title-small
   ((t (:inherit zenburn-title :height 1.2))))
 '(jabber-title-medium
   ((t (:inherit jabber-title-small :height 1.2))))
 '(jabber-title-large
   ((t (:inherit jabber-title-medium :height 1.2))))

 '(jabber-chat-prompt-local
   ((t (:inherit zenburn-primary-1))))
 '(jabber-chat-prompt-foreign
   ((t (:inherit zenburn-primary-2))))

 '(jabber-rare-time-face
   ((t (:inherit zenburn-green+1))))

 '(jde-java-font-lock-modifier-face
   ((t (:inherit zenburn-primary-2))))
 '(jde-java-font-lock-doc-tag-face
   ((t (:inherit zenburn-primary-1))))
 '(jde-java-font-lock-constant-face
   ((t (:inherit font-lock-constant-face))))
 '(jde-java-font-lock-package-face
   ((t (:inherit zenburn-primary-3))))
 '(jde-java-font-lock-number-face
   ((t (:inherit font-lock-constant-face))))
 '(jde-java-font-lock-operator-face
   ((t (:inherit font-lock-keyword-face))))
 '(jde-java-font-lock-link-face
   ((t (:inherit zenburn-primary-5 :underline t))))

 '(keywiz-right-face ((t (:inherit zenburn-primary-1))))
 '(keywiz-wrong-face ((t (:inherit font-lock-warning-face))))
 '(keywiz-command-face ((t (:inherit zenburn-primary-2))))

 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-warning-face ((t (:inherit font-lock-warning-face))))
 '(font-latex-sedate-face ((t (:inherit zenburn-primary-1))))
 '(font-latex-title-4-face ((t (:inherit zenburn-title))))

 '(makefile-space-face ((t (:inherit font-lock-warning-face))))
 '(makefile-shell-face ((t (nil))))
 ;; This does not work very well because everything that's highlighted
 ;; inside the shell region will get its own box.
 ;; (makefile-shell-face ((t (:background "#4f4f4f"
 ;;                           :box (:line-width 2 :color "#4f4f4f")))))

 '(mode-line-buffer-id ((t (:inherit zenburn-primary-1))))

 '(nxml-delimited-data-face ((t (:inherit font-lock-string-face))))
 '(nxml-name-face ((t (:inherit zenburn-primary-1))))
 '(nxml-ref-face ((t (:inherit zenburn-primary-5))))
 '(nxml-delimiter-face ((t (:inherit default))))
 '(nxml-text-face ((t (:inherit default))))

 '(nxml-comment-content-face
   ((t (:inherit font-lock-comment-face))))
 '(nxml-comment-delimiter-face
   ((t (:inherit nxml-comment-content-face))))
 '(nxml-processing-instruction-target-face
   ((t (:inherit zenburn-primary-2))))
 '(nxml-processing-instruction-delimiter-face
   ((t (:inherit nxml-processing-instruction-target-face))))
 '(nxml-processing-instruction-content-face
   ((t (:inherit nxml-processing-instruction-target-face))))
 '(nxml-cdata-section-CDATA-face
   ((t (:inherit zenburn-primary-4))))
 '(nxml-cdata-section-delimiter-face
   ((t (:inherit nxml-cdata-section-CDATA-face))))
 '(nxml-cdata-section-content-face
   ((t (:inherit nxml-text-face))))
 '(nxml-entity-ref-name-face
   ((t (:inherit zenburn-primary-5))))
 '(nxml-entity-ref-delimiter-face
   ((t (:inherit nxml-entity-ref-name-face))))
 '(nxml-char-ref-number-face
   ((t (:inherit nxml-entity-ref-name-face))))
 '(nxml-char-ref-delimiter-face
   ((t (:inherit nxml-entity-ref-delimiter-face))))

 '(nxml-tag-delimiter-face ((t (:inherit default))))
 '(nxml-tag-slash-face ((t (:inherit default))))
 '(nxml-element-local-name-face ((t (:inherit zenburn-primary-1))))
 '(nxml-element-prefix-face ((t (:inherit default))))
 '(nxml-element-colon-face ((t (:inherit default))))

 '(nxml-attribute-local-name-face
   ((t (:inherit zenburn-primary-3))))
 '(nxml-namespace-attribute-prefix-face
   ((t (:inherit nxml-attribute-local-name-face))))
 '(nxml-attribute-value-face
   ((t (:inherit font-lock-string-face))))
 '(nxml-attribute-value-delimiter-face
   ((t (:inherit nxml-attribute-value-face))))
 '(nxml-attribute-prefix-face
   ((t (:inherit default))))
 '(nxml-namespace-attribute-xmlns-face
   ((t (:inherit nxml-attribute-prefix-face))))
 '(nxml-attribute-colon-face
   ((t (:inherit default))))
 '(nxml-namespace-attribute-colon-face
   ((t (:inherit nxml-attribute-colon-face))))

 `(org-hide ((t (:foreground ,zenburn-bg+2))))

 ;; TODO
 '(outline-8 ((t (:inherit default))))
 '(outline-7 ((t (:inherit outline-8 :height 1.0))))
 '(outline-6 ((t (:inherit outline-7 :height 1.0))))
 '(outline-5 ((t (:inherit outline-6 :height 1.0))))
 '(outline-4 ((t (:inherit outline-5 :height 1.0))))
 '(outline-3 ((t (:inherit outline-4 :height 1.0))))
 '(outline-2 ((t (:inherit outline-3 :height 1.0))))
 '(outline-1 ((t (:inherit outline-2 :height 1.0))))

 `(rst-level-1-face ((t (:background ,zenburn-bg :inherit zenburn-title))))
 `(rst-level-2-face ((t (:background ,zenburn-bg :inherit zenburn-primary-1))))
 `(rst-level-3-face ((t (:background ,zenburn-bg :inherit zenburn-primary-2))))
 `(rst-level-4-face ((t (:background ,zenburn-bg :inherit zenburn-primary-3))))
 `(rst-level-5-face ((t (:background ,zenburn-bg :inherit zenburn-primary-4))))
 `(rst-level-6-face ((t (:background ,zenburn-bg :inherit zenburn-primary-5))))

 '(setnu-line-number-face ((t (:inherit zenburn-lowlight-2))))

 '(speedbar-button-face ((t (:inherit zenburn-primary-1))))
 '(speedbar-file-face ((t (:inherit zenburn-primary-2))))
 '(speedbar-directory-face ((t (:inherit zenburn-primary-5))))
 '(speedbar-tag-face ((t (:inherit font-lock-function-name-face))))
 '(speedbar-highlight-face ((t (:underline t))))

 '(strokes-char-face ((t (:inherit font-lock-keyword-face))))

 '(todoo-item-header-face
   ((t (:inherit zenburn-primary-1))))
 '(todoo-item-assigned-header-face
   ((t (:inherit zenburn-primary-2))))
 `(todoo-sub-item-header-face
   ((t (:foreground ,zenburn-yellow))))

 '(tuareg-font-lock-governing-face
   ((t (:inherit zenburn-primary-2))))
 '(tuareg-font-lock-interactive-error-face
   ((t (:inherit font-lock-warning-face))))
 '(tuareg-font-lock-interactive-output-face
   ((t (:inherit zenburn-primary-3))))
 '(tuareg-font-lock-operator-face
   ((t (:inherit font-lock-operator-face))))

 '(w3m-form-button
   ((t (:inherit widget-button))))
 '(w3m-form-button-pressed
   ((t (:inherit widget-button-pressed))))
 '(w3m-form-button-mouse
   ((t (:inherit widget-button-pressed))))
 '(w3m-tab-unselected
   ((t (:box (:line-width 1 :style released-button)))))
 '(w3m-tab-selected
   ((t (:box (:line-width 1 :style pressed-button)))))
 '(w3m-tab-unselected-retrieving
   ((t (:inherit (w3m-tab-unselected widget-inactive-face)))))
 '(w3m-tab-selected-retrieving
   ((t (:inherit (w3m-tab-selected widget-inactive-face)))))
 '(w3m-tab-background
   ((t (:inherit zenburn-highlight-subtle))))
 '(w3m-anchor
   ((t (:inherit zenburn-primary-1))))
 '(w3m-arrived-anchor
   ((t (:inherit zenburn-primary-2))))
 '(w3m-image
   ((t (:inherit zenburn-primary-4))))
 '(w3m-form
   ((t (:inherit widget-field-face)))))
