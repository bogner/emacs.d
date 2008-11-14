(require 'diary-lib)

(setq diary-file "~/.calendar")
(setq calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      diary-number-of-entries 7)
(set-variable 'diary-display-function 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(defun diary-print-batch (&optional ndays)
  "Print the diary to stdout in batch mode"
  (interactive "")
  (let ((diary-display-function 'diary-fancy-display))
    (diary-list-entries (calendar-current-date)
                        (or ndays diary-number-of-entries)))
  (message (if (get-buffer diary-fancy-buffer)
               (with-current-buffer diary-fancy-buffer (buffer-string))
             "No entries found")))
