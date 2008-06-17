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

;; Zenburn color theme
(when window-system
  (set-default-font "DejaVu Sans Mono-8.5")
  (require 'zenburn)
  (color-theme-zenburn))
