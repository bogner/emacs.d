(defun set-speedbar-strut ()
  (let ((frame
         (cond ((frame-live-p speedbar-frame) speedbar-frame)
               ((frame-live-p speedbar-cached-frame) speedbar-cached-frame))))
    (message (frame-parameter frame 'outer-window-id))
    (when frame
      (let* ((top (frame-parameter frame 'top))
             (width (frame-pixel-width frame))
             (bottom (+ top (frame-pixel-height frame))))
        (x-change-window-property "_NET_WM_STRUT_PARTIAL"
                                  (list width 0 0 0 top bottom 0 0 0 0 0 0)
                                  frame "CARDINAL" 32 t)))))

(defun clear-speedbar-strut ()
  (let ((frame
         (cond ((frame-live-p speedbar-frame) speedbar-frame)
               ((frame-live-p speedbar-cached-frame) speedbar-cached-frame))))
    (when frame
      (x-change-window-property "_NET_WM_STRUT_PARTIAL"
                                '(0 0 0 0 0 0 0 0 0 0 0 0)
                                frame "CARDINAL" 32 t))))

(remove-hook 'speedbar-after-create-hook 'speedbar-frame-reposition-smartly)
(add-hook 'speedbar-after-create-hook 'set-speedbar-strut)
(add-hook 'speedbar-before-popup-hook 'set-speedbar-strut)
(add-hook 'speedbar-before-delete-hook 'clear-speedbar-strut)
(add-to-list 'speedbar-frame-parameters '(name . "Speedbar"))
