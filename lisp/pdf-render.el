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
")

(defvar-local pdf-render-intialized-p nil)
(defvar-local pdf-render-state-alist nil)
(defvar-local pdf-render-redraw-process/timer nil)
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
                (unless (eobp)
                  (read (current-buffer)))))))))

(defun pdf-render-state-save ()
  (let ((default-directory (pdf-render-cache-directory))
        (state pdf-render-state-alist))
    (when default-directory
      (with-temp-buffer
        (prin1 state (current-buffer))
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

(defun pdf-render-convert-commands (page)
  (apply 'nconc
         (delq nil (mapcar (lambda (h) (funcall h page))
                           (remq t pdf-render-layer-functions)))))

(defun pdf-render-cancel-redraw ()
  (when (timerp pdf-render-redraw-timer)
    (cancel-timer pdf-render-redraw-timer)
    (setq pdf-render-redraw-timer nil))
  (when (processp pdf-render-redraw-process/timer)
    (delete-process pdf-render-redraw-process/timer))
  (when (timerp pdf-render-redraw-process/timer)
    (cancel-timer pdf-render-redraw-process/timer))
  (setq pdf-render-redraw-process/timer nil)
  (setq mode-line-process nil)
  nil)

(defun pdf-render-redraw--1 (pages &optional buffer)
  "Only used internally."
  ;; Assumes no conversion (DocView + pdf-render) in progress.
  (save-current-buffer
    (if buffer (set-buffer buffer) (setq buffer (current-buffer)))
    ;; Prefer displayed pages.
    (catch 'done
      (dolist (active (doc-view-active-pages))
        (let ((page (car (memq active pages))))
          (when page
            (setq pages (cons page (remq active pages)))
            (throw 'done nil)))))
    (if (null pages)
        (progn
          (setq pdf-render-redraw-process/timer nil
                mode-line-process nil))
      (let* ((page (car pages))
             (cmds (pdf-render-convert-commands page))
             (in-file (pdf-render-image-file page))
             (out-file (pdf-util-current-image-file page)))
        (setq mode-line-process
              (and pages (list (format ":Rendering (%d left)"
                                       (length pages)))))        
        (unless cmds
          (pdf-render-set-state page nil)
          (copy-file in-file out-file t)
          (clear-image-cache out-file))
        (setq pdf-render-redraw-process/timer
              (if (or (null cmds)
                      (pdf-render-state-uptodate-p page cmds))
                  (run-with-timer 0.05 nil 'pdf-render-redraw--1
                                  (cdr pages) buffer)
                (apply
                 'pdf-util-convert-asynch
                 in-file pdf-render-temp-file
                 `(,@cmds
                   ,(lambda (_proc status)
                      (when (and (equal status "finished\n")
                                 (buffer-live-p buffer))
                        (with-current-buffer buffer
                          (pdf-render-set-state page cmds)
                          (copy-file pdf-render-temp-file out-file t)
                          (clear-image-cache out-file)
                          (pdf-render-redraw--1 (cdr pages) buffer))))))))))
    (force-mode-line-update)))

(defun pdf-render-redraw-document (&optional buffer)
  (interactive)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (pdf-util-assert-pdf-buffer)
    (pdf-render-initialize)
    (unless (doc-view-already-converted-p)
      (error "Can't render while DocView is still converting"))
    (pdf-render-cancel-redraw) 
    (let* ((pages (number-sequence 1 (pdf-info-number-of-pages buffer))))
      (pdf-render-redraw--1 pages buffer))))

(provide 'pdf-render)
;;; pdf-render.el ends here
