;;; pdf-history.el --- A simple stack-based history in PDF buffers. -*- lexical-binding: t -*-


;;; Commentary:
;; 

(require 'doc-view)
(require 'pdf-util)

;;; Code:

(defvar-local pdf-history-stack nil
  "The stack of history items.")
(defvar-local pdf-history-index nil
  "The current index into ther `pdf-history-stack'.")

(defvar pdf-history-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "B") 'pdf-history-backward)
    (define-key kmap (kbd "N") 'pdf-history-forward)
    kmap)
  "Keymap used in `pdf-history-minor-mode'.")

;;;###autoload
(define-minor-mode pdf-history-minor-mode
  "Keep a history of previously visited pages.

\\{pdf-history-minor-mode-map}."
  nil nil nil
  (pdf-util-assert-pdf-buffer)
  (pdf-history-clear)
  (cond
   (pdf-history-minor-mode
    (pdf-history-push)
    (add-hook 'pdf-util-after-change-page-hook
              'pdf-history-after-change-page-hook nil t))
   (t
    (remove-hook 'pdf-util-after-change-page-hook
                 'pdf-history-after-change-page-hook t))))

(defun pdf-history-after-change-page-hook ()
  "Push a history item, before leaving this page."
  (when (and pdf-history-minor-mode
             (doc-view-current-page))
    (pdf-history-push)))

(defun pdf-history-push ()
  "Push the current page on the stack.

This function does nothing, if current stack item already
represents the current page."
  (let ((item (pdf-history-create-item)))
    (unless (and pdf-history-stack
                 (equal (nth pdf-history-index
                             pdf-history-stack) item))
      (setq pdf-history-stack
            (last pdf-history-stack
                  (- (length pdf-history-stack)
                     pdf-history-index))
            pdf-history-index 0)
      (push item pdf-history-stack))))

(defun pdf-history-clear ()
  "Remove all history items."
  (interactive)
  (setq pdf-history-stack nil
        pdf-history-index 0))

(defun pdf-history-create-item ()
  "Create a history item representing the current page."
  (list
   (doc-view-current-page)))

(defun pdf-history-beginning-of-history-p ()
  "Return t, if at the beginning of the history."
  (= pdf-history-index 0))

(defun pdf-history-end-of-history-p ()
  "Return t, if at the end of the history."
  (= pdf-history-index
     (1- (length pdf-history-stack))))
  
(defun pdf-history-backward (n)
  "Go N-times backward in the history."
  (interactive "p")
  (cond
   ((and (> n 0)
         (pdf-history-end-of-history-p))
    (error "End of history"))
   ((and (< n 0)
         (pdf-history-beginning-of-history-p))
    (error "Beginning of history"))
   ((/= n 0)
    (let ((i (min (max 0 (+ pdf-history-index n))
                  (1- (length pdf-history-stack)))))
      (prog1
          (- (+ pdf-history-index n) i)
        (pdf-history-goto i))))
   (t 0)))

(defun pdf-history-forward (n)
  "Go N-times forward in the history."
  (interactive "p")
  (pdf-history-backward (- n)))
  
(defun pdf-history-goto (n)
  "Go to item N in the history."
  (when (null pdf-history-stack)
    (error "The history is empty"))
  (cond
   ((>= n (length pdf-history-stack))
    (error "End of history"))
   ((< n 0)
    (error "Beginning of history"))
   (t
    (setq pdf-history-index n)
    (doc-view-goto-page
     (car (nth n pdf-history-stack))))))

(defun pdf-history-debug ()
  "Visualize the history in the header-line."
  (interactive)
  (setq header-line-format
        '(:eval
          (let ((pages (mapcar 'car pdf-history-stack))
                (index pdf-history-index)
                header)
            (dotimes (i (length pages))
              (push (propertize
                     (format "%s" (nth i pages))
                     'face
                     (and (= i index) 'match))
                    header))
            (concat
             "(" (format "%d" index) ")  "
             (mapconcat 'identity (nreverse header) " | "))))))

(provide 'pdf-history)

;;; pdf-history.el ends here
