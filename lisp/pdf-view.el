;;; pdf-view.el --- View PDF documents. -*- lexical-binding:t -*-

;; Copyright (C) 2013  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, doc-view, pdf

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
;;; Code:
;; 

(require 'pdf-util)
(require 'pdf-info)


(defgroup pdf-view nil
  "View PDF documents."
  :group 'pdf-tools)
  
(defcustom pdf-view-epdfrender-program
  (let* ((exec-path (cons
                     (if load-file-name
                         (file-name-directory load-file-name)
                       default-directory)
                       exec-path)))
    (executable-find "epdfrender"))
  "Filename of the epdfrender executable."
  :group 'pdf-view
  :type '(file :must-match t))

(setq pdf-view-epdfrender-program
      (expand-file-name "../epdfrender/epdfrender"))
      
(defvar-local pdf-view-process nil)
  
(defun pdf-view-process-assert-running ()
  "Assert a running process.

If getting the process to run fails, this function throws an
error."
  (interactive "P")
  ;; (when pdf-view-process (delete-process pdf-view-process))
  ;; (pdf-util-assert-pdf-buffer)
  (unless (and (processp pdf-view-process)
               (eq (process-status pdf-view-process)
                   'run))
    (when pdf-view-process
      (delete-process pdf-view-process)
      (setq pdf-view-process nil))
    (unless (and pdf-view-epdfrender-program
                 (file-executable-p pdf-view-epdfrender-program))
      (error "The variable pdf-view-epdfrender-program is unset or not executable: %s"
             pdf-view-epdfrender-program))
    (let ((proc (start-process
                 "epdfrender" (format " *epdfrender<%s>*" (buffer-name))
                 pdf-view-epdfrender-program
                 (buffer-file-name))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-coding-system proc 'binary 'binary)
      (with-current-buffer (process-buffer proc)
        (set-buffer-multibyte nil)
        (erase-buffer))
      (setq pdf-view-process proc))))

(defconst pdf-view-doc-view-converter-cookie
  "bca5ffdc30ffe583600ea364911206f9b6693c7da7a469d1bba8731a9e3bf1e7373bd8b7c1dccb93ceb0ffba4fcfd29976c923454dc1df9a30d1de3d65114f84")

(defvar pdf-view-doc-view-fast nil)

(defun pdf-view-doc-view-converter  (pdf png &optional page callback)
  (cond
   ((and page (> page 0))
    (rename-file
     (pdf-info-renderpage page 1600 pdf-view-doc-view-fast) png t))
   ((null page)
    (dotimes (i (pdf-info-number-of-pages))
      (write-region pdf-view-doc-view-converter-cookie
                    nil (format png (+ i 1)) nil 'no-message))))
  (when callback
    (funcall callback)))

(defadvice doc-view-insert-image (before pdf-view-create-image activate)
  (let* ((file (ad-get-arg 0))
         (size (nth 7 (file-attributes file)))
         pdf page)
    (when (and (eq size (length pdf-view-doc-view-converter-cookie))
               (with-temp-buffer
                 (insert-file-contents-literally file)
                 (equal (buffer-string) pdf-view-doc-view-converter-cookie))
               (setq pdf (or doc-view-buffer-file-name
                             (buffer-file-name)))
               (setq page (and (string-match "page-\\([0-9]+\\)\\....\\'" file)
                               (string-to-number (match-string 1 file)))))
      (pdf-view-doc-view-converter pdf file page))))
         
      

;; (setq-local doc-view-pdf->png-converter-function
;;             'pdf-view-doc-view-converter
;;             doc-view-single-page-converter-function
;;             'pdf-view-doc-view-converter)

(defun pdf-view-setup-converter ()
  (interactive)
  (setq-local doc-view--image-file-pattern
              (if pdf-view-doc-view-fast
                  "page-%s.ppm"
                "page-%s.png")
              doc-view-pdf->png-converter-function
              'pdf-view-doc-view-converter
              doc-view-single-page-converter-function
              'pdf-view-doc-view-converter))

(define-derived-mode pdf-view-mode doc-view-mode "PDF View"
  (progn
    (make-local-variable 'delay-mode-hooks)
    (let
        ((delay-mode-hooks t))
      (doc-view-mode)
      (setq major-mode 'pdf-view-mode)
      (setq mode-name "PDF View")
      (progn
        (if
            (get 'doc-view-mode 'mode-class)
            (put 'pdf-view-mode 'mode-class
                 (get 'doc-view-mode 'mode-class)))
        (if
            (keymap-parent pdf-view-mode-map)
            nil
          (set-keymap-parent pdf-view-mode-map
                             (current-local-map)))
        (let
            ((parent
              (char-table-parent pdf-view-mode-syntax-table)))
          (if
              (and parent
                   (not
                    (eq parent
                        (standard-syntax-table))))
              nil
            (set-char-table-parent pdf-view-mode-syntax-table
                                   (syntax-table))))
        (if
            (or
             (abbrev-table-get pdf-view-mode-abbrev-table :parents)
             (eq pdf-view-mode-abbrev-table local-abbrev-table))
            nil
          (abbrev-table-put pdf-view-mode-abbrev-table :parents
                            (list local-abbrev-table))))
      (use-local-map pdf-view-mode-map)
      (set-syntax-table pdf-view-mode-syntax-table)
      (setq local-abbrev-table pdf-view-mode-abbrev-table)
      (setq pdf-view-mode t)))
  (run-mode-hooks 'pdf-view-mode-hook))

;;(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-doc-view-mode))

(defun pdf-view-get-image (page)
  (let ((image (cdr (assq page pdf-view-cached-images))))
    ;; (clear-image-cache)
    (unless image
      (let ((filename (pdf-info-renderpage
                       page
                       (window-width nil t))))
        (setq image (create-image
                     filename 'imagemagick)))
      (push (cons page image)
            pdf-view-cached-images))
    image))

(defun pdf-view-get-image (page)
  (let ((image (cdr (assq page pdf-view-cached-images))))
    ;; (clear-image-cache)
    (unless image
      (let (filename)
        (unwind-protect
            (progn
              (setq filename (pdf-info-renderpage
                              page
                              1000))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally filename)
                (setq image (create-image
                             (buffer-substring-no-properties 1 (point-max))
                             'imagemagick t))))
          (when (file-exists-p filename)
            (delete-file filename))))
      (push (cons page image)
            pdf-view-cached-images))
    image))



(defvar-local pdf-view-overlay nil)
(defvar-local pdf-view-current-page 0)
(defvar-local pdf-view-cached-images nil)
  
(defun pdf-view-insert-image (image &rest args)
  "Insert the given pbm image.
ARGS is a list of image descriptors."
  (unless pdf-view-overlay
    (setq pdf-view-overlay (make-overlay 1 1)))
  (let ((ol pdf-view-overlay))
    ;; Only insert the image if the buffer is visible.
    (let* ((img-width (and image (car (image-size image))))
           (window-width (window-width)))
      (move-overlay ol (point-min) (point-max))
      (overlay-put ol 'display image))))

(defun pdf-view-next-page ()
  (interactive)
  (when (= pdf-view-current-page
           (pdf-info-number-of-pages))
    (user-error "Last page."))
  (cl-incf pdf-view-current-page)
  (unless (and (input-pending-p)
               (/= pdf-view-current-page
                   (pdf-info-number-of-pages)))
    (pdf-view-insert-image
     (pdf-view-get-image pdf-view-current-page))))

(defun pdf-view-prev-page ()
  (interactive)
  (when (= pdf-view-current-page
           1)
    (user-error "First page."))
  (cl-decf pdf-view-current-page)
  (unless (and (input-pending-p)
               (/= pdf-view-current-page 1))
    (pdf-view-insert-image
     (pdf-view-get-image pdf-view-current-page))))

(defvar pdf-view-test-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-keys kmap
      '(("n" pdf-view-next-page)
        ("p" pdf-view-prev-page)))
    kmap))

(define-derived-mode pdf-view-test-mode nil "PDF View"
  "View PDF documents."
  (remove-overlays)
  (pdf-view-next-page))


  
(provide 'pdf-view)
;;; pdf-view.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:
