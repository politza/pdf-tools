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

(defvar pdf-render-render-functions nil
  "A list of functions determining what to render.")

(defvar-local pdf-render-hotspot-functions nil)

(defvar-local pdf-render-intialized-p nil)
(defvar-local pdf-render-state-alist nil)
(defvar-local pdf-render-redraw-process/timer nil)
(defvar-local pdf-render-schedule-redraw-timer nil)
(defvar-local pdf-render-temp-file nil)


(defvar pdf-render-debug nil)

(defvar pdf-render-ghostscript-configuration 0)

(defvar pdf-render-inhibit-display nil)

;;
;; Rendering
;; 
  
(defun pdf-render-register-hotspot-function (fn &optional layer)
  "Register FN as a hotspot function in the current buffer, using LAYER.

FN will be called in the PDF buffer with the page-number and the
image size \(WIDTH . HEIGHT\) as arguments.  It should return a
list of hotspots applicable to the the :map image-property.

LAYER determines the order: Functions in a higher LAYER will
supercede hotspots in lower ones."
  (push (cons (or layer 0) fn)
        pdf-render-hotspot-functions))

(defun pdf-render-unregister-hotspot-function (fn)
  "Unregister FN as a hotspot function int the current buffer."
  (setq pdf-render-hotspot-functions
        (cl-remove fn pdf-render-hotspot-functions
                   :key 'cdr)))

(defun pdf-render-merge-hotspots (page properties)
  (if pdf-render-inhibit-display
      properties
    (let* ((width (or (plist-get :width properties)
                      doc-view-image-width))
           (ifsize (pdf-util-png-image-size page))
           (size (if (fboundp 'imagemagick-types)
                     (cons width
                           (round (* (/ (float (cdr ifsize))
                                        (car ifsize))
                                     width)))
                   ifsize))
           (hotspots (apply 'nconc
                            (mapcar (lambda (fn)
                                      (funcall fn page size))
                                    (pdf-render-sorted-hotspot-functions
                                     pdf-render-hotspot-functions)))))
      (plist-put properties :map
                 (append (plist-get properties :map)
                         hotspots)))))

(defun pdf-render-sorted-hotspot-functions (functions)
  (mapcar 'cdr (cl-sort (copy-sequence functions)
                        '> :key 'car)))

(defun pdf-render-register-render-function (fn &optional layer)
  "Register FN as a render function using LAYER.

FN will be called in the PDF buffer to be rendered, with the
page-number as a single argument.  It should return a
property-list applicable to the the `pdf-util-convert' funtion.

LAYER determines the order: Functions in a higher LAYER will
paint over lower ones."
  (setq pdf-render-render-functions
        (cons (cons (or layer 0) fn)
              (cl-remove fn pdf-render-render-functions
                         :key 'cdr))))

(defun pdf-render-unregister-render-function (fn)
  (setq pdf-render-render-functions
        (cl-remove fn pdf-render-render-functions
                   :key 'cdr)))

(defun pdf-render-sorted-render-functions (functions)
  (mapcar 'cdr (cl-sort (copy-sequence functions)
                        '< :key 'car)))
;; 

(defun pdf-render-initialize (&optional force)
  (unless (and pdf-render-temp-file
               (file-exists-p pdf-render-temp-file))
    (setq pdf-render-temp-file
          (pdf-util-cache-make-filename 'pdf-render-temp-file)))
  (unless (and pdf-render-intialized-p
               (not force))
    (pdf-render-state-load)
    (add-hook 'kill-buffer-hook 'pdf-render-state-save nil t)
    (add-hook 'pdf-util-after-reconvert-hook 'pdf-render-state-load nil t)
    (setq pdf-render-intialized-p t)))

(defun pdf-render-state-load ()
  (let ((default-directory (pdf-render-cache-directory)))
    (setq pdf-render-state-alist
          (when (and default-directory
                     (file-exists-p "render-state.el"))
            (with-temp-buffer
              (save-excursion
                (insert-file-contents "render-state.el"))
              (unless (eobp)
                (read (current-buffer))))))))

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
                 (pdf-render-sorted-render-functions
                  pdf-render-render-functions))))

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
  
(defun pdf-render-redraw--1 (&optional buffer silent)
  "Only used internally."
  ;; Assumes no conversion (DocView + pdf-render) in progress.
  (save-current-buffer
    (if buffer (set-buffer buffer) (setq buffer (current-buffer)))
    ;; Prefer displayed pages.
    (let* ((pages pdf-render-pages-to-render)
           (page (or (cl-some (lambda (p) (car (memq p pages)))
                              ;; This function results in an error, if
                              ;; the page is not yet displayed in some
                              ;; window.
                              (ignore-errors
                                (doc-view-active-pages)))
                     (car pages))))
      (cond
       ((null pages)
        (setq pdf-render-redraw-process/timer nil
              mode-line-process nil))
       (t
        (let* ((cmds (pdf-render-convert-commands page))
               (in-file (pdf-render-image-file page))
               (out-file (pdf-util-current-image-file page)))
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
                  ;; FIXME: This slows Emacs down, when a lot of
                  ;; buffers are rendered.
                  (run-with-timer 0.05 nil 'pdf-render-redraw--1 buffer))
                 (t
                  (unless silent
                    (setq mode-line-process
                          (and pages (list (format ":Rendering (%d left)"
                                                   (length pages))))))
                  (apply
                   'pdf-util-convert-asynch
                   in-file pdf-render-temp-file
                   `(,@cmds
                     ,(lambda (_proc status)
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (if (not (equal status "finished\n"))
                                (unless silent
                                  (setq mode-line-process
                                        `(,(cond
                                            ((equal status "killed\n")
                                             ":*canceled*")
                                            (t
                                             ":*convert error*")))))
                              (pdf-render-set-state page cmds)
                              (copy-file pdf-render-temp-file out-file t)
                              (clear-image-cache out-file)
                              (setq pdf-render-pages-to-render
                                    (remq page pdf-render-pages-to-render))
                              (pdf-render-redraw--1 buffer))))))))))))))
    (force-mode-line-update)))

(defvar pdf-render-max-processes 3) 
(defun pdf-render-current-number-of-processes ()
  (let ((n 0))
    (dolist (buf (buffer-list))
      (when (buffer-local-value
             'pdf-render-redraw-process/timer buf)
        (setq n (1+ n))))
    n))
  
(defun pdf-render-redraw-document (&optional buffer pages silent)
  (interactive)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (pdf-util-assert-pdf-buffer)
    (pdf-render-initialize)
    (pdf-render-cancel-redraw)
    (add-hook 'kill-buffer-hook 'pdf-render-cancel-redraw nil t)
    (cond
     ((or (not (doc-view-already-converted-p))
          (>= (pdf-render-current-number-of-processes)
              pdf-render-max-processes))
      (setq pdf-render-schedule-redraw-timer
            (run-with-timer 1 nil 'pdf-render-redraw-document
                            (current-buffer) pages)))
     (t
      (setq pdf-render-pages-to-render
            (if  pages 
                (cl-union (cl-remove-duplicates pages)
                          pdf-render-pages-to-render)
              (number-sequence 1 (pdf-info-number-of-pages))))
      (pdf-render-redraw--1 nil silent)))))

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
                      (let ((pdf-render-render-functions
                             (and (not ,exclusive-p)
                                  (cl-remove ,fn pdf-render-render-functions
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
  (pdf-util-assert-pdf-buffer)
  (dolist (win (get-buffer-window-list))
    (with-selected-window win
      (when (pdf-util-page-displayed-p)
        (pdf-util-save-window-scroll
          (doc-view-goto-page (doc-view-current-page)))))))

(defun pdf-render-display-image (&optional file no-hotspots &rest properties)
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
        (unless (plist-get properties :width)
          (setq properties (plist-put properties :width doc-view-image-width)))
        (unless (plist-get properties :pointer)
          (setq properties (plist-put properties :pointer 'arrow)))

        (let* ((ov (doc-view-current-overlay))
               (slice (doc-view-current-slice))
               (img (apply 'create-image file
                           type nil
                           (if no-hotspots properties
                             (pdf-render-merge-hotspots
                              (doc-view-current-page)
                              properties)))))
          (overlay-put ov 'display (if slice
                                       (list (cons 'slice slice) img)
                                     img))
          (clear-image-cache file))))))

(defvar pdf-render-momentarily-process nil)

(defun pdf-render-momentarily (&rest spec)
  (pdf-util-assert-pdf-window)
  (let* ((window (selected-window))
         (image (doc-view-current-image))
         (buffer (window-buffer window))
         (page (doc-view-current-page))
         (in-file (pdf-util-current-image-file))
         (out-file (pdf-util-cache-make-filename
                    'pdf-render-momentarily
                    (pdf-util-fast-image-format))))
    
    (when (processp pdf-render-momentarily-process)
      (delete-process pdf-render-momentarily-process))
    (setq pdf-render-momentarily-process
          (apply
           'pdf-util-convert-asynch
           in-file out-file
           `(,@spec
             ,(lambda (_proc status)
                (when (and (equal status "finished\n")
                           (buffer-live-p buffer)
                           (pdf-util-pdf-buffer-p buffer)
                           (window-live-p window)
                           (eq (window-buffer window) buffer))
                  (with-selected-window window
                    (when (and (eq page (doc-view-current-page)) 
                               (eq image (doc-view-current-image)))
                      (pdf-render-display-image out-file))))))))))

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
      (when pdf-render-inhibit-display
        (let* ((+file+ (ad-get-arg 0))
               (+page+ (save-match-data
                         (and (string-match
                               "page-\\([0-9]+\\)\\.png\\'"
                               +file+)             
                              (string-to-number (match-string 1 +file+))))))
          (when +page+
            (ad-set-arg 0 (pdf-render-image-file +page+)))))
      (ad-set-args 1 (pdf-render-merge-hotspots
                      (doc-view-current-page)
                      (ad-get-args 1))))))

(defun pdf-render-ghostscript-configure (render-opt)
  "Set ghostscript options from RENDER-OPT.

RENDER-OPT should be one of 0,1 or 2.

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
               (cl-case value
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

(provide 'pdf-render)
;;; pdf-render.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; End:
