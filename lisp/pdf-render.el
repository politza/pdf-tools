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

(require 'org-macs) ;;org-with-gensyms
(require 'pdf-util)

;;; Code:

(defvar pdf-render-layer-functions nil
  "A list of functions determining what to render.
")

(defvar-local pdf-render-intialized-p nil)
(defvar-local pdf-render-state-alist nil)
(defvar-local pdf-render-redraw-process/timer nil)
(defvar-local pdf-render-schedule-redraw-timer nil)
(defvar-local pdf-render-temp-file nil)
(defvar-local pdf-render-annotate-functions nil)

(defvar pdf-render-debug nil)

(defvar pdf-render-ghostscript-configuration 0)

(defvar pdf-render-inhibit-rendering nil)
(defvar pdf-render-inhibit-annotating nil)

;;
;; DocView Setup
;; 

(defadvice doc-view-insert-image (before pdf-render activate)
  "Not documented."
  (let (ov)
    (when (and (pdf-util-pdf-buffer-p)
               (ad-get-arg 0)
               (file-readable-p (ad-get-arg 0))
               (setq ov (doc-view-current-overlay))
               (window-live-p (overlay-get ov 'window))
               (pdf-util-png-image-size))
      (when pdf-render-inhibit-rendering
        (let* ((+file+ (ad-get-arg 0))
               (+page+ (save-match-data
                       (and (string-match
                             "page-\\([0-9]+\\)\\.png\\'"
                             +file+)             
                            (string-to-number (match-string 1 +file+))))))
          (ad-set-arg 0 (pdf-render-image-file +page+))))
      (ad-set-args 1 (pdf-render-annotate-image
                      (doc-view-current-page)
                      (ad-get-args 1))))))

(defun pdf-render-ghostscript-configure (render-opt)
  "Set ghostscript options from RENDER-OPT.

RENDER-OPT should be a number, with the following meaning.

0 -- Don't render annotations.
1 -- Render annotations, except for links.
2 -- Render annotations and links.

Everything else behaves like 0."
  (interactive
   (list (let ((value 0))
           (if (y-or-n-p "Let Ghostscript render common annotations ?")
               (setq value 1))
           (when (/= value 0)
             (if (y-or-n-p "Let Ghostscript render links as well ?")                 
                 (setq value 2)))
           value)))

  (let ((not-annot-opt "-dShowAnnots=false")
        (link-opts '("-dDOPDFMARKS" "-dPrinted=false")))
    
    (if (not (memq render-opt '(1 2)))
        (add-to-list 'doc-view-ghostscript-options not-annot-opt t)
      (setq doc-view-ghostscript-options
            (remove not-annot-opt doc-view-ghostscript-options)))
    (dolist (o link-opts)
      (if (eq render-opt 2)
          (add-to-list 'doc-view-ghostscript-options o t)
        (setq doc-view-ghostscript-options
              (remove o doc-view-ghostscript-options))))
    (setq pdf-render-ghostscript-configuration
          (if (not (numberp render-opt))
              0
            (min 2 (max 0 render-opt))))))

(defun pdf-render-ghostscript-check (&optional interactive)
  "Check ghostscript's options regarding annotations.

Returns a number with the following meaning.

0 -- gs does not render annotations.
1 -- gs renders annotations, but not links.
2 -- gs renders annotations and links."

  (interactive (list t))
  (let ((not-annot-opt "-dShowAnnots=false")
        (link-opts '("-dDOPDFMARKS" "-dPrinted=false"))
        (value 0))
    (unless (member not-annot-opt doc-view-ghostscript-options)
      (setq value 1)
      (when (and (member (car link-opts) doc-view-ghostscript-options)
                 (member (cadr link-opts) doc-view-ghostscript-options))
        (setq value 2)))
    (when interactive
      (message "Ghostscript is configured %sto render %s"
               (if (= 0 value) "not " "")
               (case value
                 (0 "annotations or links.")
                 (1 "annotations, but not links.")
                 (2 "annotations and links."))))
    value))
    
    
    
(defadvice doc-view-sentinel (before pdf-render-handle-spurious-gs-errors activate)
  "Handle spurious ghostscript errors.

When setting the option -dShowAnnots=false, ghostscript prints
frequently (verision 8.71 anyway) error messages and exits with
an error code, thought all pages were rendered just fine.  Handle
this case, i.e. check if all images were produced, despite the
exit code.  And if this checks out, advice DocView about it."

  (unless (equal "finished\n" (ad-get-arg 1))
    (let ((rargs (reverse (process-command (ad-get-arg 0)))))
      (when (equal doc-view-ghostscript-program
                   (car (last rargs)))
        (let ((pdf (pop rargs))
              out first last)
          (dolist (arg rargs)
            (if (string-match "\\`-d\\(?:\\(LastPage\\)\\|FirstPage\\)=\\([0-9]+\\)\\'" arg)
                (if (match-string 1 arg)
                    (setq first (string-to-number (match-string 2 arg)))
                  (setq last (string-to-number (match-string 2 arg))))
              (if (string-match "\\`-sOutputFile=\\(.*\\)\\'" arg)
                  (setq out (match-string 1 arg)))))
          (when (or (and  first last out
                          (not (string-match "page-%d.png\\'" out))
                          (file-exists-p out))
                    (and pdf out
                         (file-readable-p pdf)
                         (string-match "page-%d.png\\'" out)
                         (catch 'result
                           (setq first 1
                                 last (pdf-info-number-of-pages pdf))
                           (while (< first last)
                             (unless (file-exists-p (format out first))
                               (throw 'result nil))
                             (setq first (1+ first)))
                           t)))
            (ad-set-arg 1 "finished\n")))))))

;;
;; Rendering
;; 

(defun pdf-render-register-annotate-image-function (fn &optional layer)
  (push (cons (or layer 0) fn)
        pdf-render-annotate-functions))

(defun pdf-render-unregister-annotate-function (fn)
  (setq pdf-render-annotate-functions
        (cl-remove fn pdf-render-annotate-functions
                   :key 'cdr)))

(defun pdf-render-annotate-image (page props)
  (if pdf-render-inhibit-annotating
      props
    (let* ((width (or (plist-get :width props)
                      doc-view-image-width))
           (size (pdf-util-png-image-size page))
           (size (if (fboundp 'imagemagick-types)
                     (cons width
                           (round (* (/ (float (cdr size))
                                        (car size)) width)))
                   size)))
      (with-wrapper-hook 
          (nreverse (pdf-render-sorted-layer-functions
                     pdf-render-annotate-functions))
          (page size)
        props))))
      
(defun pdf-render-sorted-layer-functions (functions)
  (mapcar 'cdr (cl-sort (copy-sequence functions)
                        '< :key 'car)))

(defun pdf-render-register-layer-function (fn &optional layer)
  (setq pdf-render-layer-functions
        (cons (cons (or layer 0) fn)
              (cl-remove fn pdf-render-layer-functions
                         :key 'cdr))))

(defun pdf-render-unregister-layer-function (fn)
  (setq pdf-render-layer-functions
        (cl-remove fn pdf-render-layer-functions
                   :key 'cdr)))
  
;; 

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
        (state pdf-render-state-alist)
        print-level print-length)
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
  (unless pdf-render-debug
    (equal (sha1 (format "%S" cmds))
           (pdf-render-state page))))

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
         (mapcar (lambda (h) (funcall h page))
                 (pdf-render-sorted-layer-functions
                  pdf-render-layer-functions))))

(defun pdf-render-cancel-redraw ()
  (when (timerp pdf-render-schedule-redraw-timer)
    (cancel-timer pdf-render-schedule-redraw-timer)
    (setq pdf-render-schedule-redraw-timer nil))
  (when (processp pdf-render-redraw-process/timer)
    (delete-process pdf-render-redraw-process/timer))
  (when (timerp pdf-render-redraw-process/timer)
    (cancel-timer pdf-render-redraw-process/timer))
  (setq pdf-render-redraw-process/timer nil)
  (setq mode-line-process nil)
  nil)

(defvar-local pdf-render-pages-to-render nil)
  
(defun pdf-render-redraw--1 (&optional buffer)
  "Only used internally."
  ;; Assumes no conversion (DocView + pdf-render) in progress.
  (save-current-buffer
    (if buffer (set-buffer buffer) (setq buffer (current-buffer)))
    ;; Prefer displayed pages.
    (let* ((pages pdf-render-pages-to-render)
           (page (or (cl-some (lambda (p) (car (memq p pages)))
                              (doc-view-active-pages))
                     (car pages))))
      (cond
       ((null pages)
        (setq pdf-render-redraw-process/timer nil
              mode-line-process nil))
       (t
        (let* ((cmds (pdf-render-convert-commands page))
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
                (cond
                 ((or (null cmds)
                      (pdf-render-state-uptodate-p page cmds))
                  (setq pdf-render-pages-to-render
                        (remq page pdf-render-pages-to-render))
                  (run-with-timer 0.05 nil 'pdf-render-redraw--1 buffer))
                 (t
                  (apply
                   'pdf-util-convert-asynch
                   in-file pdf-render-temp-file
                   `(,@cmds
                     ,(lambda (_proc status)
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (if (not (equal status "finished\n"))
                                (setq mode-line-process
                                      (list ":*convert error*"))
                              (pdf-render-set-state page cmds)
                              (copy-file pdf-render-temp-file out-file t)
                              (clear-image-cache out-file)
                              (setq pdf-render-pages-to-render
                                    (remq page pdf-render-pages-to-render))
                              (pdf-render-redraw--1 buffer))))))))))))))
    (force-mode-line-update)))

(defun pdf-render-redraw-document (&optional buffer pages)
  (interactive)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (pdf-util-assert-pdf-buffer)
    (pdf-render-initialize)
    (pdf-render-cancel-redraw)
    (unless pdf-render-inhibit-rendering
      (cond
       ((not (doc-view-already-converted-p))
        (setq pdf-render-schedule-redraw-timer
              (run-with-timer 1 nil 'pdf-render-redraw-document
                              (current-buffer) pages)))
       (t
        (setq pdf-render-pages-to-render
              (if  pages 
                  (cl-union (cl-remove-duplicates pages)
                            pdf-render-pages-to-render)
                (number-sequence 1 (pdf-info-number-of-pages))))
        (pdf-render-redraw--1))))))

(defmacro pdf-render-with-redraw (render-fn &optional exclusive-p &rest body)
  (declare (indent 1) (debug t))
  (unless (memq exclusive-p '(t nil))
    (setq body (cons exclusive-p body)
          exclusive-p nil))
  (org-with-gensyms (proc buffer tmp-in tmp-out fn page first-redraw)
    `(progn
       (pdf-util-assert-pdf-window)
       (let* ((,tmp-in
               (make-temp-file
                "pdf-render" nil (concat "." (pdf-util-fast-image-format))))
              (,tmp-out
               (make-temp-file
                "pdf-render" nil (concat "." (pdf-util-fast-image-format))))
              (,page (doc-view-current-page))
              (,fn ,render-fn)
              (,buffer (current-buffer))
              (,first-redraw t)
              ,proc)
         (unwind-protect
             (cl-labels
               ((redraw ()
                  (and (processp ,proc) (delete-process ,proc))
                  (setq ,proc
                        (apply 'pdf-util-convert-asynch
                               ,tmp-in
                               ,tmp-out
                               (append (funcall ,fn ,page)
                                       (list (lambda (_proc status)
                                               (when (and (equal status "finished\n")
                                                          (eq (window-buffer)
                                                              ,buffer)
                                                          (eq ,page (doc-view-current-page)))
                                                 (copy-file ,tmp-out pdf-render-temp-file t)
                                                 (if (null ,first-redraw)
                                                     (clear-image-cache
                                                      pdf-render-temp-file)
                                                   (setq ,first-redraw nil)
                                                   (pdf-render-display-image
                                                    pdf-render-temp-file))))))))))
               (pdf-render-cancel-redraw)
               (pdf-render-initialize)
               (apply 'pdf-util-convert
                      (pdf-render-image-file ,page)
                      ,tmp-in
                      (let ((pdf-render-layer-functions
                             (and (not ,exclusive-p)
                                  (cl-remove ,fn pdf-render-layer-functions
                                             :key 'cdr))))
                        (pdf-render-convert-commands ,page)))
               (progn ,@body))

           (pdf-render-redraw-document
            nil (cons ,page pdf-render-pages-to-render))
           (while (memq ,page pdf-render-pages-to-render)
             (accept-process-output nil 0.005))
           (pdf-render-display-image)
           (when (file-exists-p ,tmp-out)
             (delete-file ,tmp-out))
           (when (file-exists-p ,tmp-in)
             (delete-file ,tmp-in)))))))


(defun pdf-render-redisplay-current-page ()
  (when (pdf-util-page-displayed-p)
    (doc-view-goto-page (doc-view-current-page))))

(defun pdf-render-display-image (&optional file no-annotate)
  (if (null file)
      (pdf-render-redisplay-current-page)
    (unless (file-equal-p
             file
             (pdf-util-current-image-file))
      (let ((type (or (and (fboundp 'imagemagick-types)
                           'imagemagick)
                      (and (equal (file-name-extension file) "png")
                           'png)
                      (image-type file))))
        (unless type
          (error "Unable to display image: %s" file))
        (let ((ov (doc-view-current-overlay))
              (slice (doc-view-current-slice))
              (img (apply 'create-image file
                          type
                          nil
                          (if no-annotate
                              (list :width doc-view-image-width)
                            (pdf-render-annotate-image
                             (doc-view-current-page)
                             (list :width doc-view-image-width))))))
          (overlay-put ov 'display (if slice
                                       (list (cons 'slice slice) img)
                                     img))
          (clear-image-cache file))))))

(defun pdf-render-momentarily (callback &rest spec)
  (pdf-util-assert-pdf-window)
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (page (doc-view-current-page))
         (in-file (pdf-util-current-image-file))
         (out-file (pdf-util-cache-make-filename
                    'pdf-render-momentarily
                    (pdf-util-fast-image-format)
                    (append spec (pdf-render-convert-commands page)))))
    (add-hook 'kill-buffer-hook
              'pdf-render-momentarily-clear-cache nil t)
    (cond
     ((file-exists-p out-file)
      (pdf-render-display-image out-file)
      (when callback
        (run-with-timer 0 nil (lambda nil (funcall callback)))))
     ((null spec)
      (pdf-util-redisplay-current-page)
      (when callback
        (run-with-timer 0 nil (lambda nil (funcall callback)))))
     (t
      (apply
       'pdf-util-convert-asynch
       in-file out-file
       `(,@spec
         ,(lambda (proc status)
            (when (and (equal status "finished\n")
                       (window-live-p window)
                       (eq (window-buffer window) buffer)
                       (pdf-util-pdf-buffer-p buffer))
              (with-selected-window window
                (pdf-render-display-image out-file)
                (when callback
                  (funcall callback)))))))))))
  
(defun pdf-render-momentarily-clear-cache ()
  (pdf-util-assert-pdf-buffer)
  (pdf-util-cache-clear 'pdf-render-momentarily))

(provide 'pdf-render)
;;; pdf-render.el ends here
