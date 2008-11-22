(require 'diary-lib)

(setq diary-file "~/.calendar")
(setq calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t
      diary-number-of-entries 7)
(set-variable 'diary-display-function 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

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
