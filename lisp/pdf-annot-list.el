;;; pdf-annot-list.el --- List annotations           -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: 

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

(defvar-local pdf-annot-list-document-buffer nil)

(defvar pdf-annot-list-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-f") 'pdf-annot-list-follow-minor-mode)
    (define-key km (kbd "SPC") 'pdf-annot-display-annotation)
    km))

(defun pdf-annot-list-entries ()
  (unless (buffer-live-p pdf-annot-list-document-buffer)
    (error "No PDF document associated with this buffer"))
  (mapcar 'pdf-annot-list-create-entry
          (sort (cl-remove-if 'pdf-annot-deleted-p
                              (pdf-annot-getannots
                               nil 'text pdf-annot-list-document-buffer))
                'pdf-annot-compare)))

(defun pdf-annot-compare (a1 a2)
  (let ((p1 (pdf-annot-get a1 'page))
        (p2 (pdf-annot-get a2 'page)))
    (or (< p1 p2)
        (and (= p1 p2)
             (pdf-annot-compare-edges a1 a2)))))

(defun pdf-annot-compare-edges (a1 a2)
  (let ((e1 (pdf-util-scale-edges
             (pdf-annot-get a1 'edges)
             '(1000 . 1000)))
        (e2 (pdf-util-scale-edges
             (pdf-annot-get a2 'edges)
             '(1000 . 1000))))
    (pdf-util-with-edges (e1 e2)
      (or (< e1-top e2-top)
          (and (= e1-top e2-top)
               (<= e1-left e2-left))))))

(defun pdf-annot-list-create-entry (a)
  (list a (apply 'vector
                 (mapcar
                  (lambda (item)
                    (replace-regexp-in-string
                     "\n" " "
                     item t t))
                  (append
                   (list (if (pdf-annot-modified-properties a) "%" " "))
                   (mapcar (lambda (p)
                             (pdf-annot-print-property a p))
                           '(page color icon))
                   (list
                    (if (pdf-annot-get a 'modified)
                        (pdf-annot-print-property a 'modified)
                      (if (pdf-annot-get a 'created)
                          (pdf-annot-print-property a 'created)
                        "Unknown"))
                    (pdf-annot-print-property a 'label)))))))
    
(define-derived-mode pdf-annot-list-mode tablist-mode "Annots"
  (cl-labels ((writable (str &optional extra)
                (let ((disp (concat str
                                    (propertize (concat "*" extra)
                                                'display
                                                '((height 0.6) (raise 0.2))))))
                  (propertize str 'display disp))))
    (setq tabulated-list-entries 'pdf-annot-list-entries
          tabulated-list-format (vector
                                 '("M" 1 t :read-only t :pad-right 1)
                                 '("Pg." 3 t :read-only t :right-align t)
                                 `(,(writable "Color") 7 t)
                                 `(,(writable "Icon") 8 t)
                                 '("Date" 24
                                   (lambda (d1 d2)
                                     (setq d1 (aref (cadr d1) 5))
                                     (setq d2 (aref (cadr d2) 5))
                                     (pdf-annot-list-compare-dates d1 d2))
                                   :read-only t)
                                 `(,(writable "Label"
                                              "               *editable")
                                   24 t))
          tabulated-list-padding 2)
    (set-keymap-parent pdf-annot-list-mode-map tablist-mode-map)
    (use-local-map pdf-annot-list-mode-map)
    (tabulated-list-init-header)))

(defvar pdf-annot-list-display-buffer-action
  '((display-buffer-reuse-window
     display-buffer-pop-up-window)
    (inhibit-same-window . t)))

(defvar-local pdf-annot-list-buffer nil)

(defun pdf-annot-list-annotations ()
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (unless (cl-remove-if 'pdf-annot-deleted-p (pdf-annot-getannots))
    (error "No annotations in this buffer"))
  (let ((buffer (current-buffer)))
    (with-current-buffer (get-buffer-create
                          (format "*%s's annots*"
                                  (file-name-sans-extension
                                   (buffer-name))))
      (unless (derived-mode-p 'pdf-annot-list-mode)
        (pdf-annot-list-mode))
      (setq pdf-annot-list-document-buffer buffer)
      (tabulated-list-print)
      (setq tablist-context-window-function 'pdf-annot-list-context-function
            tablist-operations-function 'pdf-annot-list-operation-function
            tablist-major-columns '(1 2 3 4 5))
      (let ((list-buffer (current-buffer)))
        (with-current-buffer buffer
          (setq pdf-annot-list-buffer list-buffer)))
      (pop-to-buffer
       (current-buffer)
       pdf-annot-list-display-buffer-action)
      (tablist-move-to-major-column)
      (tablist-display-context-window))
    (add-hook 'pdf-annot-pages-modified-functions
              'pdf-annot-list-update nil t)))

(defun pdf-annot-list-goto-annotation (a)
  (with-current-buffer (pdf-annot-buffer a)
    (unless (and (buffer-live-p pdf-annot-list-buffer)
                 (get-buffer-window pdf-annot-list-buffer))
      (pdf-annot-list-annotations))
    (with-selected-window (get-buffer-window pdf-annot-list-buffer)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (eq a (tabulated-list-get-id))))
        (forward-line))
      (unless (eq a (tabulated-list-get-id))
        (error "Unable to find annotation"))
      (when (invisible-p (point))
        (tablist-suspend-filter t))
      (tablist-move-to-major-column))))
        
  
(defun pdf-annot-list-update (_pages)
  (when (buffer-live-p pdf-annot-list-buffer)
    (with-current-buffer pdf-annot-list-buffer
      (unless tablist-edit-column-minor-mode
        (tablist-revert))
      (tablist-context-window-update))))
  
(defun pdf-annot-list-context-function (annot)
  (with-current-buffer (get-buffer-create "*Contents*")
    (set-window-buffer nil (current-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when annot
        (save-excursion
          (insert
           (pdf-annot-print-tooltip annot))))
      (read-only-mode 1))))

(defun pdf-annot-list-operation-function (op &rest args)
  (cl-ecase op
    (supported-operations '(delete edit-column find-entry complete))
    (edit-column
     (cl-destructuring-bind (id column item)
         args
       (let ((prop (nth column '(nil nil color icon nil label nil))))
         (unless prop
           (error "Invalid column (read-only)"))
         (when (eq prop 'color)
           (setq item (pdf-util-hexcolor item)))
         (pdf-annot-validate-property-value prop item)
         (pdf-annot-put id prop item)
         (cadr (pdf-annot-list-create-entry id)))))
    (delete
     (cl-destructuring-bind (annots)
         args
       (when (buffer-live-p pdf-annot-list-document-buffer)
         (with-current-buffer pdf-annot-list-document-buffer
           (pdf-annot-with-multiple-modifications
             (dolist (a annots)
               (pdf-annot-set-deleted-p a t)))))))
    (find-entry
     (cl-destructuring-bind (a)
         args
       (let ((buffer (pdf-annot-buffer a))
             edit-buffer)
         (with-selected-window (or (get-buffer-window buffer)
                                   (display-buffer buffer))
           (pdf-annot-edit-text a)
           (setq edit-buffer (current-buffer)))
         (let* ((window (get-buffer-window edit-buffer))
                (quit-restore (and window
                                   (window-parameter window 'quit-restore))))
           (when (and window quit-restore)
             (setcar (nthcdr 2 quit-restore) (selected-window)))
           (when window
             (select-window window))))))
    (complete
     (cl-destructuring-bind (_id column _string _pos)
         args
       (let ((prop (cl-case column
                     (2 'color)
                     (3 'icon))))
         (when prop
           (pdf-annot-property-completions prop)))))))

(defun pdf-annot-display-annotation (a)
  (interactive (list (tabulated-list-get-id)))
  (let ((buffer (pdf-annot-buffer a)))
    (with-selected-window (or (get-buffer-window buffer)
                              (display-buffer
                               buffer
                               '(nil (inhibit-same-window . t))))
      (when (pdf-util-page-displayed-p)
        (pdf-util-scroll-to-edges
         (pdf-util-scale-edges
          (pdf-annot-get a 'edges)
          (pdf-view-image-size t)))
        (pdf-annot-highlight a)))))

(defun pdf-annot-list-compare-dates (d1 d2)
  (setq d1 (ignore-errors
             (apply 'encode-time
                  (parse-time-string d1)))
        d2 (ignore-errors
             (apply 'encode-time
                    (parse-time-string d2))))
  (or (not d2)
      (and d1 d2
           (time-less-p d1 d2))))
      

(define-minor-mode pdf-annot-list-follow-minor-mode
  "" nil nil nil
  (pdf-util-assert-derived-mode 'pdf-annot-list-mode)
  (cond
   (pdf-annot-list-follow-minor-mode
    (add-hook 'tablist-selection-changed-functions
              'pdf-annot-display-annotation nil t)
    (let ((a (tabulated-list-get-id)))
      (when a
        (pdf-annot-display-annotation a))))
   (t
    (remove-hook 'tablist-selection-changed-functions
                 'pdf-annot-display-annotation t))))

(provide 'pdf-annot-list)

;;; pdf-annot-list.el ends here
