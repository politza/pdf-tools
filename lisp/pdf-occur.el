;;; pdf-occur.el --- Display matching lines. -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 

(require 'pdf-view)
(require 'pdf-util)
(require 'pdf-info)
(require 'tablist)

;;; Code:

(defvar pdf-occur-history nil
  "The history variable for the pdf-occur command.")

(defvar pdf-occur-buffer-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap tablist-mode-map)
    (define-key kmap (kbd "RET") 'pdf-occur-goto-occurrence)
    (define-key kmap (kbd "C-o") 'pdf-occur-view-occurrence)
    (define-key kmap (kbd "C-c C-f") 'next-error-follow-minor-mode)
    kmap)
  "The keymap used for `pdf-occur-buffer-mode'.")
  
(define-derived-mode pdf-occur-buffer-mode tablist-mode "PDFOccur"
  "Major mode for output from \\[pdf-occur]. \\<pdf-occur-buffer-mode-map>

Move point to one of the items in this buffer, then use \\[pdf-occur-goto-occurrence] to go
to the occurrence that the item refers to, and \\[pdf-occur-view-occurrence] to view the
item in the other window.

\\{pdf-occur-buffer-mode-map}"
  (setq truncate-lines t)
  (setq next-error-function 'pdf-occur-next-error))
  
(defun pdf-occur-next-error (&optional arg reset)
  "Move to the Nth (default 1) next match in an PDF Occur mode buffer.
Compatibility function for \\[next-error] invocations."
  (interactive "p")
  ;; we need to run pdf-occur-find-match from within the Occur buffer
  (with-current-buffer
      ;; Choose the buffer and make it current.
      (if (next-error-buffer-p (current-buffer))
	  (current-buffer)
	(next-error-find-buffer nil nil
				(lambda ()
				  (eq major-mode 'pdf-occur-buffer-mode))))
    (if reset
        (goto-char (point-min))
      (beginning-of-line))
    (when (/= arg 0)
      (when (eobp)
        (forward-line -1))
      (when reset
        (cl-decf arg))
      (let ((line (line-number-at-pos))
            (limit (line-number-at-pos
                    (if (>= arg 0)
                        (1- (point-max))
                      (point-min)))))
        (when (= line limit)
          (error "No more matches"))
        (forward-line
         (if (>= arg 0)
             (min arg (- limit line))
           (max arg (- limit line))))))
    ;; In case the *Occur* buffer is visible in a nonselected window.
    (let ((win (get-buffer-window (current-buffer) t)))
      (if win (set-window-point win (point))))
    (pdf-occur-goto-occurrence)))

(defun pdf-occur-goto-occurrence (&optional event)
  "Go to the occurrence at EVENT.

If EVENT is nil, use occurrence at current line."
  (interactive (list last-nonmenu-event))
  (let ((link
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (tabulated-list-get-id)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (tabulated-list-get-id))))))
    (when link
      (pop-to-buffer (car link))
      (pdf-view-goto-page (cadr link))
      (pdf-util-tooltip-arrow (nth 1 (nth 2 link)) 2))))

(defun pdf-occur-view-occurrence (&optional event)
  "View the occurrence at EVENT.

If EVENT is nil, use occurrence at current line."
  (interactive (list last-nonmenu-event))
  (let ((link
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (tabulated-list-get-id)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (tabulated-list-get-id))))))
    (when link
      (with-selected-window (display-buffer (car link))
        (pdf-view-goto-page (cadr link))
        (pdf-util-tooltip-arrow (nth 1 (nth 2 link)) 2)))))

;;;###autoload
(defun pdf-occur (string &optional buffer)
  "List lines containing STRING in BUFFER."
  (interactive
   (list (read-string
          "List lines containing string: "
          nil 'pdf-occur-history) nil))
  (when (or (null string) (= (length string) 0))
    (error "Not searching for the empty string"))
  (unless buffer (setq buffer (current-buffer)))
  (let ((matches (pdf-info-search-string string nil buffer)))
    (when (null matches)
      (error "No match for %s in buffer %s" string (buffer-name buffer)))
    (with-current-buffer (get-buffer-create "*PDF-Occur*")
      (let* ((page-cmp
              (lambda (e1 e2)
                (let ((p1 (string-to-number
                           (aref (cadr e1) 0)))
                      (p2 (string-to-number
                           (aref (cadr e2) 0))))
                  (<= p1 p2)))))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (pdf-occur-buffer-mode)
        (setq tabulated-list-format
              (apply 'vector
                     `(("Page" 4 ,page-cmp
                        :right-align t)
                       ("" 0 t))))
        (setq tabulated-list-entries
              (pdf-occur-create-entries buffer string matches)
              tabulated-list-sort-key (cons "Page" nil))
        (tabulated-list-init-header)
        (tabulated-list-print)
        (setq next-error-last-buffer (current-buffer))
        (display-buffer (current-buffer))))))
    
(defun pdf-occur-create-entries (buffer string matches)
  (let ((case-fold-search (buffer-local-value
                           'case-fold-search buffer))
        entries)
    (dolist (m matches)
      (let ((page-str (format "%d" (car m)))
            (page (car m))
            page-entries)
        (dolist (match (cdr m))
          (let ((txt (pdf-occur-fontify
                      string (car match))))
            (push (list (list buffer page (cadr match))
                        (vector page-str txt))
                  page-entries)))
        (setq entries (append entries page-entries))))
    entries))

(defun pdf-occur-fontify (string text)
  (with-temp-buffer
    (save-excursion (insert text))
    (while (search-forward string nil t)
      (put-text-property
       (match-beginning 0)
       (match-end 0)
       'face 'match))
    (buffer-string)))

(provide 'pdf-occur)

;;; pdf-occur.el ends here
