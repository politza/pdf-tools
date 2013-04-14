;;; pdf-util.el --- PDF Utility functions. -*- lexical-binding: t -*-

;; Copyright (C) 2013  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, pdf

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
;; TODO:
;; * Handle remote and locally cached documents.

;;; Code:
(require 'cl-lib)
(require 'doc-view)
(require 'format-spec)
(require 'gnus-range)
         
;;
;; Variables
;;

(defvar pdf-util-convert-program (executable-find "convert"))

(defvar-local pdf-util-png-image-size-alist nil
  "Alist of cached png image sizes.")

(defvar pdf-util-after-change-page-hook nil
  "A hook ran after turning the page.")

;;
;; Doc View Buffers
;; 

(defun pdf-util-docview-buffer-p (&optional buffer)
  (and (or (null buffer)
           (buffer-live-p buffer))
       (save-current-buffer
         (and buffer (set-buffer buffer))
         (derived-mode-p 'doc-view-mode))))

(defun pdf-util-assert-docview-buffer ()
  (unless (pdf-util-docview-buffer-p)
    (error "Buffer is not in DocView mode")))

(defun pdf-util-pdf-buffer-p (&optional buffer)
  (and (or (null buffer)
           (buffer-live-p buffer))
       (save-current-buffer
         (and buffer (set-buffer buffer))
         (and (derived-mode-p 'doc-view-mode)
              (eq 'pdf doc-view-doc-type)))))

(defun pdf-util-assert-pdf-buffer ()
  (unless (pdf-util-pdf-buffer-p)
    (error "Buffer is not in DocView PDF mode")))

(defadvice doc-view-goto-page (around pdf-util activate)
  "Run `pdf-util-after-change-page-hook'."
  (let ((pdf-util-current-page (doc-view-current-page)))
    ad-do-it
    (unless (eq pdf-util-current-page
                (doc-view-current-page))
      (run-hooks 'pdf-util-after-change-page-hook))))

(defun pdf-util-page-displayed-p ()
  (consp (ignore-errors
           (doc-view-current-image))))

(defun pdf-util-page-displayable-p (&optional page)
  (unless page (setq page (doc-view-current-page)))
  (file-readable-p
   (expand-file-name
    (format "page-%d.png" page)
    (doc-view-current-cache-dir))))
  

(defun pdf-util-current-image-file (&optional page)
  (expand-file-name (format "page-%d.png"
                            (or page (doc-view-current-page)))
                    (doc-view-current-cache-dir)))
;;
;; Handling Edges
;; 

(defmacro pdf-util-with-edges (list-of-edges &rest body)
  (declare (indent 1) (debug (sexp &rest form)))
  (unless (cl-every 'symbolp list-of-edges)
    (signal 'wrong-type-argument (list 'symbolp list-of-edges)))
  (let ((list-of-syms
         (mapcar (lambda (edge)
                   (cons edge (mapcar
                               (lambda (kind)
                                 (intern (format "%s-%s" edge kind)))
                               '(left top right bot width height))))
                 list-of-edges)))
    (let ((lisp (macroexpand-all
                 `(cl-symbol-macrolet
                      ,(apply 'nconc
                              (mapcar
                               (lambda (edge-syms)
                                 (let ((edge (nth 0 edge-syms))
                                       (syms (cdr edge-syms)))
                                   `((,(pop syms) (nth 0 ,edge))
                                     (,(pop syms) (nth 1 ,edge))
                                     (,(pop syms) (nth 2 ,edge))
                                     (,(pop syms) (nth 3 ,edge))
                                     (,(pop syms) (- (nth 2 ,edge)
                                                     (nth 0 ,edge)))
                                     (,(pop syms) (- (nth 3 ,edge)
                                                     (nth 1 ,edge))))))
                               list-of-syms))
                    ,@body))))
      ;; get rid of silly nested (progn (progn ...
      (while (eq 'progn (car-safe (nth 1 lisp)))
        (setq lisp (nth 1 lisp)))
      lisp)))

(defun pdf-util-scale-edges (list-of-edges scale)
  "Scale LIST-OF-EDGES in both directions by SCALE.

SCALE is a cons (SX . SY), by which edges are scaled and defaults
to the scale of the image in the current window."

  (let ((have-list-p (listp (car list-of-edges))))
    (unless have-list-p
      (setq list-of-edges (list list-of-edges)))
    (let* ((sx (car scale))
           (sy (cdr scale))
           (result (mapcar (lambda (edges)
                             (mapcar 'round
                                     (list (* (nth 0 edges) sx)
                                           (* (nth 1 edges) sy)
                                           (* (nth 2 edges) sx)
                                           (* (nth 3 edges) sy))))
                           list-of-edges)))
      (if have-list-p
          result
        (car result)))))

(defun pdf-util-translate-edges (list-of-edges offset &optional inverse-p)
  (let ((have-list-p (listp (car list-of-edges))))
    (if (equal offset '(0 . 0))
        list-of-edges
      (unless have-list-p
        (setq list-of-edges (list list-of-edges)))
      (let* ((ox (if inverse-p
                     (- (car offset))
                   (car offset)))
             (oy (if inverse-p
                     (- (cdr offset))
                   (cdr offset)))
             (result (mapcar (lambda (edges)
                               (mapcar 'round
                                       (list (+ (nth 0 edges) ox)
                                             (+ (nth 1 edges) oy)
                                             (+ (nth 2 edges) ox)
                                             (+ (nth 3 edges) oy))))
                             list-of-edges)))
        (if have-list-p
            result
          (car result))))))

(defun pdf-util-transform-edges (list-of-edges scale offset)
  (pdf-util-translate-edges
   (pdf-util-scale-edges list-of-edges scale)
   offset))

(defun pdf-util-enlarge-edges (list-of-edges dx dy)
  (let ((have-list-p (listp (car list-of-edges))))
    (unless have-list-p
      (setq list-of-edges (list list-of-edges)))
    (let ((result (mapcar (lambda (edges)
                            (list (- (nth 0 edges) dx)
                                  (- (nth 1 edges) dy)
                                  (+ (nth 2 edges) dx)
                                  (+ (nth 3 edges) dy)))
                          list-of-edges)))
      (if have-list-p
          result
        (car result)))))

(defun pdf-utils-edges-contained-p (edges contained &optional epsilon)
  (unless epsilon (setq epsilon 0))
  (pdf-util-with-edges (edges contained)
    (and (<= (- edges-left epsilon)
             contained-left)
         (>= (+ edges-right epsilon)
             contained-right)
         (<= (- edges-top epsilon)
             contained-top)
         (>= (+ edges-bot epsilon)
             contained-bot))))

(defun pdf-utils-edges-disjoint-p (edges1 edges2 &optional epsilon)
  (unless epsilon (setq epsilon 0))
  (pdf-util-with-edges (edges1 edges2)
    (or (<= (- edges2-right epsilon)
            edges1-left)
        (<= (- edges2-bot epsilon)
            edges1-top)
        (>= (+ edges2-left epsilon)
            edges1-right)
        (>= (+ edges2-top epsilon)
            edges1-bot))))

(defun pdf-utils-edges-intersection (e1 e2)
  (pdf-util-with-edges (edges1 e1 e2)
    (let ((left (max e1-left e2-left))
          (top (max e1-top e2-top))
          (right (min e1-right e2-right))
          (bot (min e1-bot e2-bot)))
      (when (and (<= left right)
                 (<= top bot))
        (list left top right bot)))))

(defun pdf-utils-edges-intersection-area (e1 e2)
  (let ((inters (pdf-utils-edges-intersection e1 e2)))
    (if (null inters)
        0
      (pdf-util-with-edges (inters)
        (* inters-width inters-height)))))
  

;;
;; Handling Images In Windows
;; 

(defun pdf-util-image-size (&optional sliced-p)
  (if sliced-p
      (image-display-size (image-get-display-property) t)
    (image-size (doc-view-current-image) t)))

(defun pdf-util-image-offset ()
  (let* ((slice (doc-view-current-slice)))
    (if slice
        (cons (nth 0 slice) (nth 1 slice))
      (cons 0 0))))

(defun pdf-util-set-window-pixel-vscroll (vscroll)
  (setq vscroll (max (round vscroll) 0))
  (set-window-vscroll (selected-window) vscroll t)
  (setf (image-mode-window-get 'vscroll) (window-vscroll))
  nil)

(defun pdf-util-set-window-pixel-hscroll (hscroll)
  (setq hscroll (max 0 (round (/ hscroll (float (frame-char-width))))))
  (setf (image-mode-window-get 'hscroll) hscroll)
  (set-window-hscroll nil hscroll)
  nil)

(defun pdf-util-image-edges-in-window (&optional window)
  "Return the visible edges of some image in WINDOW."
  (let* ((edges (window-inside-pixel-edges window))
         (isize (pdf-util-image-size))
         (offset (pdf-util-image-offset))
         (hscroll (* (window-hscroll window)
                     (frame-char-width (window-frame window))))
         (vscroll (window-vscroll window t))
         (x0 (+ hscroll (car offset)))
         (y0 (+ vscroll (cdr offset)))
         (x1 (min (car isize)
                  (+ x0 (- (nth 2 edges) (nth 0 edges)))))
         (y1 (min (cdr isize)
                  (+ y0 (- (nth 3 edges) (nth 1 edges))))))
    (list x0 y0 x1 y1)))


(defun pdf-util-required-hscroll (edges &optional eager-p context-pixel)
  (unless context-pixel (setq context-pixel 0;; (frame-char-width)
                              ))
  (let* ((win (window-inside-pixel-edges))
         (image-width (car (pdf-util-image-size t)))
         (image-left (* (frame-char-width)
                        (window-hscroll)))
         (edges (pdf-util-translate-edges
                 edges
                 (pdf-util-image-offset) t)))
    (pdf-util-with-edges (win edges)
      (let* ((edges-left (- edges-left context-pixel))
             (edges-right (+ edges-right context-pixel)))
        (if (< edges-left image-left)
            (max 0 (if eager-p
                       (- edges-right win-width)
                     edges-left))
          (if (> (min image-width
                      edges-right)
                 (+ image-left win-width))
              (min (- image-width win-width)
                   (if eager-p
                       edges-left
                     (- edges-right win-width)))))))))

(defun pdf-util-required-vscroll (edges &optional eager-p context-pixel)
  (let* ((win (window-inside-pixel-edges))
         (image-height (cdr (pdf-util-image-size t)))
         (image-top (window-vscroll nil t))
         (edges (pdf-util-translate-edges
                 edges
                 (pdf-util-image-offset) t)))
    (pdf-util-with-edges (win edges)
      (let* ((context-pixel (or context-pixel
                                (* next-screen-context-lines
                                   edges-height)))
             ;;Be careful not to modify edges.
             (edges-top (- edges-top context-pixel))
             (edges-bot (+ edges-bot context-pixel)))
        (if (< edges-top image-top)
            (max 0 (if eager-p
                       (- edges-bot win-height)
                     edges-top))
          (if (> (min image-height
                      edges-bot)
                 (+ image-top win-height))
              (min (- image-height win-height)
                   (if eager-p
                       edges-top
                     (- edges-bot win-height)))))))))

(defmacro pdf-util-save-window-scroll (&rest body)
  (declare (indent 0) (debug t))
  (let ((hscroll (make-symbol "hscroll"))
        (vscroll (make-symbol "vscroll")))
    `(let ((,hscroll (window-hscroll))
           (,vscroll (window-vscroll)))
       (unwind-protect
           (progn ,@body)
         (image-set-window-hscroll ,hscroll)
         (image-set-window-vscroll ,vscroll)))))

(defun pdf-util-display-image (file)
  (if (null file)
      (doc-view-goto-page (doc-view-current-page))
    (let ((type (or (and (fboundp 'imagemagick-types)
                         'imagemagick)
                    (and (equal (file-name-extension file) "png")
                         'png)
                    (image-type file))))
      (unless type
        (error "Unable to display image: %s" file))
      (let ((ov (doc-view-current-overlay))
            (slice (doc-view-current-slice))
            (im (apply 'create-image file
                       type
                       nil
                       (list :width doc-view-image-width))))
        (overlay-put ov 'display (if slice
                                     (list (cons 'slice slice) im)
                                   im))
        (clear-image-cache file)))))

;;
;; Converting Images
;;

(defun pdf-util-assert-convert-program ()
  (unless (and pdf-util-convert-program
               (file-executable-p pdf-util-convert-program))
    (error "The pdf-util-convert-program is unset or non-executable")))

(defun pdf-util-png-image-size (&optional page)
  "Return the image size of the image file of the current PAGE.

This returns a cons \(WIDTH . HEIGHT\) or nil, if not
available (e.g. because it does not exist or is currently written
to)."

  (unless page (setq page (ignore-errors
                            (doc-view-current-page))))
  (when page
    (let ((entry
           (cl-assoc page pdf-util-png-image-size-alist
                     :test 'gnus-member-of-range)))
      (if entry
          (nth 2 entry)
        (let ((page-size (pdf-info-pagesize page)))
          (setq entry (car (cl-member page-size
                                      pdf-util-png-image-size-alist
                                      :key 'cadr :test 'equal)))
          (unless entry
            (let ((size (pdf-util-image-file-size
                         (pdf-util-current-image-file page))))
              (when size
                (setq entry
                      (list nil page-size size))
                (push entry pdf-util-png-image-size-alist))))
          (when entry
            (setcar entry (gnus-range-add
                           (car entry) (list page)))
            (nth 2 entry)))))))
        

(defun pdf-util-image-file-size (image-file)
  (pdf-util-assert-convert-program)
  (with-temp-buffer
    (when (save-excursion
            (= 0 (call-process
                  pdf-util-convert-program
                  nil (current-buffer) nil
                  image-file "-format" "%w %h" "info:")))
      (let ((standard-input (current-buffer)))
        (cons (read) (read))))))

(defun pdf-util-convert-asynch (in-file out-file &rest spec-and-callback)
  (pdf-util-assert-convert-program)
  (let ((callback (car (last spec-and-callback)))
        spec)
    (if (functionp callback)
        (setq spec (butlast spec-and-callback))
      (setq spec spec-and-callback
            callback nil))
    (let* ((cmds (pdf-util-convert--create-commands
                  (pdf-util-png-image-size)
                  spec))
           (proc
            (apply 'start-process "pdf-util-convert"
                   (get-buffer-create " *pdf-util-convert-output*")
                   pdf-util-convert-program
                   (append cmds (list in-file out-file)))))
      (when callback
        (set-process-sentinel proc callback))
      proc)))

(defun pdf-util-convert (in-file out-file &rest spec)
  (pdf-util-assert-convert-program)
  (let* ((cmds (pdf-util-convert--create-commands
                (pdf-util-png-image-size)
                spec))
         (status (apply 'call-process
                        pdf-util-convert-program nil
                        (get-buffer-create " *pdf-util-convert-output*")
                        nil
                        (append cmds (list in-file out-file)))))
    (unless (and (numberp status) (= 0 status))
      (error "The convert program exited with error status: %s" status))
    out-file))

(defun pdf-util-convert--create-commands (image-size spec)
  (let ((fg "red")
        (bg "red")
        formats result cmds s)
    (while (setq s (pop spec))
      (unless spec
        (error "Missing value in convert spec:%s" (cons s spec)))
      (cl-case s
        (:foreground
         (setq fg (pop spec)))
        (:background
         (setq bg (pop spec)))
        (:commands
         (setq cmds (pop spec)))
        (:extra-formats
         (setq formats (pop spec)))
        (:apply
         (dolist (m (pop spec))
           (pdf-util-with-edges (m)
             (let ((alist (append
                           (mapcar (lambda (f)
                                     (cons (car f) (funcall (cdr f))))
                                   formats)
                           `((?g . ,(format "%dx%d+%d+%d"
                                            m-width m-height
                                            m-left m-top))
                             (?x . ,m-left)
                             (?X . ,m-right)
                             (?y . ,m-top)
                             (?Y . ,m-bot)
                             (?w . ,(- m-right m-left))
                             (?h . ,(- m-bot m-top))
                             (?f . ,fg)
                             (?b . ,bg)
                             (?W . ,(nth 0 image-size))
                             (?H . ,(cdr image-size))))))
               (dolist (fmt cmds)
                 (push (format-spec fmt alist) result))))))))
    (nreverse result)))


;;
;; Caching Converted Images
;;

(defun pdf-util-cache-make-filename (dir &optional extension &rest keys)
  (let ((root (pdf-util-cache--get-root-dir)))
    (unless root
      (error "The DocView cache directory is n/a"))
    (let ((file (format "%s.%s" (sha1 (format "%S" keys))
                        extension))
          (dir  (file-name-as-directory
                 (expand-file-name
                  dir
                  root))))
      (unless (file-exists-p dir)
        (make-directory dir))
      (expand-file-name file dir))))

(defun pdf-util-cache-files (dir)
  (interactive)
  (let ((root (pdf-util-cache--get-root-dir)))
    (when root
      (let ((dir (file-name-as-directory
                  (expand-file-name
                   dir
                   root))))
        (when (file-exists-p dir)
          (directory-files
           dir t directory-files-no-dot-files-regexp))))))

(defun pdf-util-cache-clear (dir)
  (interactive)
  (let ((root (pdf-util-cache--get-root-dir)))
    (when root
      (let ((dir (file-name-as-directory
                  (expand-file-name
                   dir
                   root))))
        (when (file-exists-p dir)
          (mapc 'clear-image-cache
                (directory-files
                 dir t directory-files-no-dot-files-regexp t))
          (delete-directory dir t))))))

(defun pdf-util-cache-clear-all ()
  (interactive)
  (let ((dir (pdf-util-cache--get-root-dir)))
    (when (and dir
               (file-exists-p dir))
      (delete-directory dir t))))

(defun pdf-util-cache--get-root-dir ()
  (when (and (doc-view-current-cache-dir)
             (file-exists-p (doc-view-current-cache-dir)))
    (let ((dir (file-name-as-directory
                (expand-file-name
                 ".pdf-util-cache"
                 (doc-view-current-cache-dir)))))
      (unless (file-exists-p dir)
        (make-directory dir)
        (add-hook 'kill-buffer-hook 'pdf-util-cache-clear-all nil t))
      dir)))

;;
;; Various Functions
;; 

(defun pdf-util-tooltip-in-window (text x y &optional window)
  (let* ((we (window-inside-absolute-pixel-edges window))
         (dx (round (+ x (nth 0 we))))
         (dy (round (+ y (nth 1 we))))
         (tooltip-frame-parameters
          `((left . ,dx)
            (top . ,dy)
            ,@tooltip-frame-parameters)))
    (tooltip-show text)))

(defun pdf-util-tooltip-arrow (image-top &optional timeout)
  (when (floatp image-top)
    (setq image-top
          (round (* image-top (cdr (pdf-util-image-size))))))
  (let* (x-gtk-use-system-tooltips ;allow for display property in tooltip
         (dx (+ (or (car (window-margins)) 0)
                (car (window-fringes))))
         (dy image-top)
         (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))
         (vscroll
          (pdf-util-required-vscroll pos))
         (tooltip-frame-parameters
          `((border-width . 0)
            (internal-border-width . 0)
            ,@tooltip-frame-parameters))
         (tooltip-hide-delay (or timeout (* 60 60))))
    (when vscroll
      (pdf-util-set-window-pixel-vscroll vscroll))
    (setq dy (max 0 (- dy
                       (cdr (pdf-util-image-offset))
                       (window-vscroll nil t))))
    (pdf-util-tooltip-in-window
     (propertize
      " " 'display (propertize
		    "\u2192" ;;right arrow
		    'display '(height 2)
		    'face '(:foreground
                            "orange red"
                            :background "white")))
     dx dy)))

(defvar pdf-util-face-colors--cache (make-hash-table))
  
(defun pdf-util-face-colors (face &optional dark-p)
  "Return both colors of FACE as a cons.

Look also in inherited faces.  If DARK-P is non-nil, return dark
colors, otherwise light."
  (let* ((bg (if dark-p 'dark 'light))
         (spec (list (get face 'face-defface-spec)
                     (get face 'theme-face)
                     (get face 'customized-face)))
         (cached (gethash face pdf-util-face-colors--cache)))
    (cl-destructuring-bind (&optional cspec color-alist)
        cached
      (or (and color-alist
               (equal cspec spec)
               (cdr (assq bg color-alist)))
          (let* ((this-bg (frame-parameter nil 'background-mode))
                 (frame-background-mode bg)
                 (f (and (not (eq bg this-bg))
                         (x-create-frame-with-faces '((visibility . nil))))))
            (with-selected-frame (or f (selected-frame))
              (unwind-protect
                  (let ((colors
                         (cons (face-attribute face :foreground nil 'default)
                               (face-attribute face :background nil 'default))))
                    (puthash face `(,(mapcar 'copy-sequence spec)
                                    ((,bg . ,colors) ,@color-alist))
                             pdf-util-face-colors--cache)
                    colors)
                (when (and f (frame-live-p f))
                  (delete-frame f)))))))))


(provide 'pdf-util)

;;; pdf-util.el ends here
