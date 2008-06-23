;; Remove startup message
(setq inhibit-startup-message t)

;; Hide menu bar, tool bar, and scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Display the time and date
(set-variable 'display-time-24hr-format nil)
(setq display-time-day-and-date t)
(display-time)

;; Compilation window
(setq compilation-window-height 20)
(setq compilation-scroll-output t)

;; autoload for zenburn, so it only loads when there's a window system
(autoload 'color-theme-zenburn "zenburn")

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-8.5"))

(when window-system
  (color-theme-zenburn))
