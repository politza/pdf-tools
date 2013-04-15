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

:before-redraw
:after-redraw
:updated-p
:redraw-page
")

(defvar-local pdf-render-intialized-p nil)
(defvar-local pdf-render-state-alist nil)
(defvar-local pdf-render-redraw-process nil)
(defvar-local pdf-render-redraw-canceled-p nil)
(defvar-local pdf-render-redraw-timer nil)
(defvar-local pdf-render-temp-file nil)


(defun pdf-render-initialize (&optional force)
  (unless (and pdf-render-intialized-p
               (not force))
    (unless pdf-render-temp-file
      (setq pdf-render-temp-file (make-temp-file "pdf-render")))
    (pdf-render-state-load)
    (add-hook 'kill-buffer-hook 'pdf-render-state-save nil t)
    (add-hook 'pdf-util-after-reconvert-hook 'pdf-render-state-load nil t)
    (setq pdf-render-intialized-p t)))

(defun pdf-render-state-load ()
  (let ((default-directory (pdf-render-cache-directory)))
    (when default-directory
      (setq pdf-render-state-alist
            (when (file-exists-p "render-state.el")
              (with-temp-buffer
                (save-excursion
                  (insert-file-contents "render-state.el"))
                (read)))))))

(defun pdf-render-state-save ()
  (let ((default-directory (pdf-render-cache-directory)))
    (when default-directory
      (with-temp-buffer
        (princ pdf-render-state-alist)
        (write-region 1 (point-max) "render-state.el" nil 'no-msg)))))

(defun pdf-render-cache-directory ()
  (when (file-exists-p (doc-view-current-cache-dir))
    (let ((cache (file-name-as-directory
                  (expand-file-name
                   ".pdf-render"
                   (doc-view-current-cache-dir)))))
      (unless (file-exists-p cache)
        (make-directory cache))
      cache)))

(defun pdf-render-state (page)
  (declare (gv-setter (lambda (cmds)
                        `(apply 'pdf-render-set-state
                                ,page ',cmds))))
  (cdr (assq page pdf-render-state-alist)))

(defun pdf-render-set-state (page cmds)
  (setq pdf-render-state-alist
        `((,page . ,(sha1 (format "%S" cmds)))
          ,@(cl-remove page pdf-render-state-alist :key 'car))))

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
  (apply 'nconc
         (delq nil (mapcar (lambda (hook)
                             (when (or (null updated-only-p)
                                       (funcall hook :updated-p page))
                               (funcall hook :render-page page)))
                           (remq t pdf-render-layer-functions)))))

(defun pdf-render-cancel-redraw ()
  (when pdf-render-redraw-timer
    (cancel-timer pdf-render-redraw-timer)
    (setq pdf-render-redraw-timer nil))
  (when (processp pdf-render-redraw-process)
    (delete-process pdf-render-redraw-process)
    (setq pdf-render-redraw-process nil))
  (setq mode-line-process nil)
  nil)

(defun pdf-render-redraw--1 (jobs &optional buffer)
  "Only used internally."
  ;; Assumes no conversion (DocView + pdf-render) in progress.
  (save-current-buffer
    (if buffer (set-buffer buffer) (setq buffer (current-buffer)))
    ;; Prefer displayed pages.
    (catch 'done
      (dolist (active (doc-view-active-pages))
        (let ((job (assq active jobs)))
          (when job
            (setq jobs (cons job (cl-remove active jobs :key 'car)))
            (throw 'done nil)))))
    (let* ((job (car jobs))
           (page (car job))
           (cmds (cdr job)))
      ;; FIXME: Cleanup temp files, if rendering is cancelled.
      (cond
       ((null jobs)
        (setq pdf-render-redraw-process nil
              mode-line-process nil)
        (run-hook-with-args 'pdf-render-layer-functions :after-render))
       (t
        (setq mode-line-process
              (list (format ":Rendering (%d left)"
                            (length jobs))))
        (let ((in-file (pdf-render-image-file page))
              (out-file (pdf-util-current-image-file page)))
          ;;(delete-file out-file)
          (setq pdf-render-redraw-process
                (apply
                 'pdf-util-convert-asynch
                 in-file pdf-render-temp-file
                 `(,@cmds
                   ,(lambda (_proc status)
                      (when (equal status "finished\n")
                        (pdf-render-set-state page cmds)
                        (copy-file pdf-render-temp-file out-file t)
                        (clear-image-cache out-file)
                        (pdf-render-redraw--1 (cdr jobs) buffer))))))))))))

(defun pdf-render-redraw-document (&optional buffer)
  (interactive)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (pdf-util-assert-pdf-buffer)
    (pdf-render-initialize)
    (unless (doc-view-already-converted-p)
      (error "Can't render while DocView is still converting"))
    (pdf-render-cancel-redraw)
    (run-hook-with-args 'pdf-render-layer-functions :before-render)
    (let* ((pages (number-sequence 1 (pdf-info-number-of-pages buffer)))
           (cmds (mapcar 'pdf-render-convert-commands pages))
           (jobs (cl-mapcar 'cons pages cmds)))
      (pdf-render-redraw--1
       (cl-remove-if
        (lambda (j) (pdf-render-state-uptodate-p (car j) (cdr j)))
        jobs)
       buffer))))

(provide 'pdf-render)
;;; pdf-render.el ends here
