;;; pdf-render.el --- Hanlde rendering PDF images. -*- lexical-binding:t -*-

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

(defvar pdf-render-layer-functions nil
  "A list of functions determining what to render.

:initialize
:updated-p
:convert-commands
")

(defvar-local pdf-render-state-alist nil)
(defvar-local pdf-render-redraw-process nil)
(defvar-local pdf-render-redraw-timer nil)

(defun pdf-render-state-load ()
  (let ((default-directory (pdf-render-cache-directory)))
    (setq pdf-render-state-alist
          (when (file-exists-p "render-state.el")
            (with-temp-buffer
              (save-excursion
                (insert-file-contents "render-state.el"))
              (read))))))

(defun pdf-render-state-save ()
  (let ((default-directory (pdf-render-cache-directory)))
    (with-temp-buffer
      (princ pdf-render-state-alist)
      (write-region 1 (point-max) "render-state.el" nil 'no-msg))))

(defun pdf-render-cache-directory ()
  (let ((cache (file-name-as-directory
                (expand-file-name
                 ".pdf-render"
                 (doc-view-current-cache-dir)))))
    (unless (file-exists-p cache)
      (make-directory cache))
    cache))

(defun pdf-render-state (page)
  (declare (gv-setter (lambda (cmds)
                        `(apply 'pdf-render-set-state
                                ,page ',cmds))))
  (cdr (assq page pdf-render-state-alist)))

(defun pdf-render-set-state (page cmds)
  (setq pdf-render-state-alist
        (cons page
              (sha1 (format "%S" cmds)))
        (cl-remove page pdf-render-state-alist :key 'car)))

(defun pdf-render-state-uptodate-p (page cmds)
  (equal (sha1 (format "%S" cmds))
         (pdf-render-state page)))

(defun pdf-render-image-file (page)
  ;; Assumes no conversion in progress.
  (let* ((name (format "page-%d.png" page))
         (file (expand-file-name
                name
                (pdf-render-cache-directory))))
    (unless (file-exists-p file)
      (let ((default-directory (doc-view-current-cache-dir)))
        (unless (file-exists-p name)
          (error "Image file for page %d does not exist: %s"
                 page (expand-file-name name)))
        (copy-file name file)))
    file))

(defun pdf-render-convert-commands (page &optional updated-only-p)
  (delq nil (mapcar (lambda (hook)
                      (when (or (null updated-only-p)
                                (funcall hook :updated-p page))
                        (funcall hook :convert-commands page)))
                    (remq t pdf-render-layer-functions))))

(defun pdf-render-cancel-redraw ()
  (when pdf-render-redraw-timer
    (cancel-timer pdf-render-redraw-timer)
    (setq pdf-render-redraw-timer nil))
  (when pdf-render-redraw-process
    (delete-process pdf-render-redraw-process)
    (setq pdf-render-redraw-process nil))
  nil)

(defun pdf-render-redraw--pages (pages &optional buffer)
  "Only used internally."
  ;; Assumes no conversion (DocView + pdf-render) in progress.
  (save-current-buffer
    (if buffer (set-buffer buffer) (setq buffer (current-buffer)))
    ;; Prefer displayed pages.
    (catch 'done
      (dolist (active (doc-view-active-pages))
        (when (memq active pages)
          (setq pages (cons active (remq active pages)))
          (throw 'done nil))))
    (let* ((page (car pages))
           (cmds (pdf-render-convert-commands page)))
      (cond
       ((null pages)
        ;; (pdf-render-redraw--cleanup)
        )
       ((pdf-render-state-uptodate-p page cmds)
        (pdf-render-redraw--pages (cdr pages) buffer))
       (t
        (let ((in-file (pdf-render-image-file page))
              (out-file (pdf-util-current-image-file page)))
          (setq mode-line-process
                (list (cons 'pdf-render-redraw-process
                            (list (format ":Rendering (%d left)"
                                          (length pages)))))
                pdf-render-redraw-process
                (apply
                 'pdf-util-convert-asynch
                 in-file out-file
                 `(,@cmds
                   ,(lambda (_proc status)
                      (when (and (equal status "finished\n")
                                 (file-exists-p out-file))
                        (pdf-render-set-state page cmds)
                        (clear-image-cache out-file))
                      (pdf-render-redraw--pages (cdr pages) buffer)))))))))))

(defun pdf-render-redraw-document (&optional buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (pdf-util-assert-pdf-buffer)
    (unless (doc-view-already-converted-p)
      (error "Can't render while DocView is still converting"))
    (pdf-render-cancel-redraw)
    (pdf-render-redraw--pages
     (number-sequence 1 (pdf-info-number-of-pages buffer))
     buffer)))

  




(provide 'pdf-render)
;;; pdf-render.el ends here
