;; -*- lexical-binding: t -*-

(defun display-buffer-split-below-and-attach (buf alist)
  (let ((window (selected-window))
        (height (cdr (assq 'window-height alist)))
        newwin)
    (when height
      (when (floatp height)
        (setq height (round (* height (frame-height)))))
      (setq height (- (max height window-min-height))))
    (setq newwin (window--display-buffer
                  buf
                  (split-window-below height)
                  'window alist display-buffer-mark-dedicated))
    (window-attach newwin window)
    newwin))

(defun window-attach (awindow &optional window)
  "Attach AWINDOW to WINDOW.

This has the following effect.  Whenever WINDOW, defaulting to
the selected window, stops displaying the buffer it currently
displays (e.g., by switching buffers or because it was deleted)
AWINDOW is deleted."
  (unless window (setq window (selected-window)))
  (let ((abuffer (window-buffer awindow))
        (buffer (window-buffer window))
        (hook (make-symbol "window-attach-hook")))
    (fset hook
          (lambda ()
            (when (or (not (window-live-p window))
                      (not (eq buffer (window-buffer window))))
              (remove-hook 'window-configuration-change-hook
                           hook)
              ;; Deleting windows inside wcch leads to errors in
              ;; windows.el .
              (run-with-timer
               0 nil (lambda (win)
                       (when (and (window-live-p win)
                                  (not (eq win (selected-window))))
                         (delete-window win)))
               awindow))))
    (add-hook 'window-configuration-change-hook hook)))

(provide 'pdf-window)
