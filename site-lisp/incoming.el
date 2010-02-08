
;; (defun priority< (arg1 arg2)
;;   (let* ((starts-with (lambda (x y) (not (string< x y))))
;;          (val (lambda (x) (cond ((string= x "high") 1)
;;                                 ((starts-with x "med") 2)
;;                                 ((string= x "low") 3)))))
;;     (< (val arg1) (val arg2))))

;; (defun sort-priority (reverse beg end)
;;   "Sort lines in region by words that look like priorities;
;; argument means descending order.  Called from a program, there
;; are three arguments: REVERSE (non-nil means reverse order), BEG
;; and END (region to sort).  The variable `sort-fold-case'
;; determines whether alphabetic case affects the sort order."
;;   (interactive "P\nr")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region beg end)
;;       (goto-char (point-min))
;;       (let ;; To make `end-of-line' and etc. to ignore fields.
;; 	  ((inhibit-field-text-motion t))
;; 	(sort-subr reverse 'forward-line 'end-of-line nil nil priority<)))))
