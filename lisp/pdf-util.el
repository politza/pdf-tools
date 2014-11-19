;;; pdf-util.el --- PDF Utility functions. -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

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
;;; Todo:
;; 
;; * Handle remote and locally cached documents.

;;; Code:

(require 'cl-lib)
(require 'format-spec)
(require 'faces)
(require 'pdf-view)


;; * ================================================================== *
;; * Transforming coordinates 
;; * ================================================================== *


(defun pdf-util-scale (list-of-edges-or-pos scale &optional rounding-fn)
  "Scale LIST-OF-EDGES-OR-POS by SCALE.

SCALE is a cons (SX . SY), by which edges/positions are scaled.
If ROUNDING-FN is non-nil, it should be a function of one
argument, a real value, returning a rounded
value (e.g. `ceiling').

The elements in LIST-OF-EDGES-OR-POS should be either a list
\(LEFT TOP RIGHT BOT\) or a position \(X . Y\).

LIST-OF-EDGES-OR-POS may also be a single such element.

Return scaled list of edges if LIST-OF-EDGES-OR-POS was indeed a list,
else return the scaled singleton."

  (let ((have-list-p (listp (car list-of-edges-or-pos))))
    (unless have-list-p
      (setq list-of-edges-or-pos (list list-of-edges-or-pos)))
    (let* ((sx (car scale))
           (sy (cdr scale))
           (result
            (mapcar
             (lambda (edges)
               (cond
                ((consp (cdr edges))
                 (let ((e (list (* (nth 0 edges) sx)
                                (* (nth 1 edges) sy)
                                (* (nth 2 edges) sx)
                                (* (nth 3 edges) sy))))
                   (if rounding-fn
                       (mapcar rounding-fn e)
                     e)))
                (rounding-fn
                 (cons (funcall rounding-fn (* (car edges) sx))
                       (funcall rounding-fn (* (cdr edges) sy))))
                (t
                 (cons (* (car edges) sx)
                       (* (cdr edges) sy)))))
             list-of-edges-or-pos)))
      (if have-list-p
          result
        (car result)))))

(defun pdf-util-scale-to (list-of-edges from to &optional rounding-fn)
  "Scale LIST-OF-EDGES in FROM basis to TO.

FROM and TO should both be a cons \(WIDTH . HEIGTH\).  See also
`pdf-util-scale'."

  (pdf-util-scale list-of-edges
                  (cons (/ (float (car to))
                           (float (car from)))
                        (/ (float (cdr to))
                           (float (cdr from))))
                  rounding-fn))

(defun pdf-util-scale-pixel-to-points (list-of-pixel-edges
                                       &optional rounding-fn displayed-p window)
  "Scale LIST-OF-PIXEL-EDGES to point values.

The result depends on the currently displayed page in WINDOW.
See also `pdf-util-scale'."
  (pdf-util-assert-pdf-window window)
  (pdf-util-scale-to
   list-of-pixel-edges
   (pdf-view-image-size displayed-p window)
   (pdf-cache-pagesize)
   rounding-fn))

(defun pdf-util-scale-points-to-pixel (list-of-points-edges
                                       &optional rounding-fn displayed-p window)
  "Scale LIST-OF-POINTS-EDGES to point values.

The result depends on the currently displayed page in WINDOW.
See also `pdf-util-scale'."
  (pdf-util-assert-pdf-window window)
  (pdf-util-scale-to
   list-of-points-edges
   (pdf-cache-pagesize)
   (pdf-view-image-size displayed-p window)
   rounding-fn))

(defun pdf-util-scale-relative-to-points (list-of-relative-edges
                                          &optional rounding-fn window)
  "Scale LIST-OF-RELATIVE-EDGES to point values.

The result depends on the currently displayed page in WINDOW.
See also `pdf-util-scale'."
  (pdf-util-assert-pdf-window window)
  (pdf-util-scale-to
   list-of-relative-edges
   '(1.0 . 1.0)
   (pdf-cache-pagesize)
   rounding-fn))

(defun pdf-util-scale-points-to-relative (list-of-points-edges
                                          &optional rounding-fn window)
  "Scale LIST-OF-POINTS-EDGES to relative values.

See also `pdf-util-scale'."
  (pdf-util-assert-pdf-window window)
  (pdf-util-scale-to
   list-of-points-edges
   (pdf-cache-pagesize)
   '(1.0 . 1.0)
   rounding-fn))

(defun pdf-util-scale-pixel-to-relative (list-of-pixel-edges
                                         &optional rounding-fn displayed-p window)
  "Scale LIST-OF-PIXEL-EDGES to relative values.

The result depends on the currently displayed page in WINDOW.
See also `pdf-util-scale'."
  (pdf-util-assert-pdf-window window)
  (pdf-util-scale-to
   list-of-pixel-edges
   (pdf-view-image-size displayed-p window)
   '(1.0 . 1.0)
   rounding-fn))


(defun pdf-util-scale-relative-to-pixel (list-of-relative-edges
                                         &optional rounding-fn displayed-p window)
  "Scale LIST-OF-EDGES to match SIZE.

The result depends on the currently displayed page in WINDOW.
See also `pdf-util-scale'."
  (pdf-util-assert-pdf-window window)
  (pdf-util-scale-to
   list-of-relative-edges
   '(1.0 . 1.0)
   (pdf-view-image-size displayed-p window)
   rounding-fn))

(defun pdf-util-translate (list-of-edges-or-pos
                           offset &optional opposite-direction-p)
  "Translate LIST-OF-EDGES-OR-POS by OFFSET

OFFSET should be a cons \(X . Y\), by which to translate
LIST-OF-EDGES-OR-POS.  If OPPOSITE-DIRECTION-P is non-nil
translate by \(-X . -Y\).

See `pdf-util-scale' for the LIST-OF-EDGES-OR-POS argument."

  (let ((have-list-p (listp (car list-of-edges-or-pos))))
    (unless have-list-p
      (setq list-of-edges-or-pos (list list-of-edges-or-pos)))
    (let* ((ox (if opposite-direction-p
                   (- (car offset))
                 (car offset)))
           (oy (if opposite-direction-p
                   (- (cdr offset))
                 (cdr offset)))
           (result
            (mapcar
             (lambda (edges)
               (cond
                ((consp (cdr edges))
                 (list (+ (nth 0 edges) ox)
                       (+ (nth 1 edges) oy)
                       (+ (nth 2 edges) ox)
                       (+ (nth 3 edges) oy)))
                (t
                 (cons (+ (car edges) ox)
                       (+ (cdr edges) oy)))))
             list-of-edges-or-pos)))
      (if have-list-p
          result
        (car result)))))

(defmacro pdf-util-with-edges (list-of-edges &rest body)
  "Provide some convenient macros for the edges in LIST-OF-EDGES.

LIST-OF-EDGES should be a list of variables \(X ...\), each one
holding a list of edges. Inside BODY the symbols X-left, X-top,
X-right, X-bot, X-width and X-height expand to their respective
values."

  (declare (indent 1) (debug (sexp &rest form)))
  (unless (cl-every 'symbolp list-of-edges)
    (error "Argument should be a list of symbols"))
  (let ((list-of-syms
         (mapcar (lambda (edge)
                   (cons edge (mapcar
                               (lambda (kind)
                                 (intern (format "%s-%s" edge kind)))
                               '(left top right bot width height))))
                 list-of-edges)))
    (macroexpand-all
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


;; * ================================================================== *
;; * Scrolling
;; * ================================================================== *

(defun pdf-util-image-displayed-edges (&optional window)
  "Return the visible region of the image in WINDOW.

Returns a list of pixel edges."
  (let* ((edges (window-inside-pixel-edges window))
         (isize (pdf-view-image-size window))
         (offset (pdf-view-image-offset window))
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
  "Return the amount of scrolling nescessary, to make image EDGES visible.

Scroll as little as necessary.  Unless EAGER-P is non-nil, in
which case scroll as much as possible.

Keep CONTEXT-PIXEL pixel of the image visible at the bottom and
top of the window.  CONTEXT-PIXEL defaults to 0.

Return the require hscroll in columns or nil, if scrolling is not
needed."

  (unless context-pixel
    (setq context-pixel 0))
  (let* ((win (window-inside-pixel-edges))
         (image-width (car (pdf-view-image-size t)))
         (image-left (* (frame-char-width)
                        (window-hscroll)))
         (edges (pdf-util-translate
                 edges
                 (pdf-view-image-offset) t)))
    (pdf-util-with-edges (win edges)
      (let* ((edges-left (- edges-left context-pixel))
             (edges-right (+ edges-right context-pixel)))
        (if (< edges-left image-left)
            (/ (max 0 (if eager-p
                          (- edges-right win-width)
                        edges-left))
               (frame-char-width))
          (if (> (min image-width
                      edges-right)
                 (+ image-left win-width))
              (/ (min (- image-width win-width)
                      (if eager-p
                          edges-left
                        (- edges-right win-width)))
                 (frame-char-width))))))))

(defun pdf-util-required-vscroll (edges &optional eager-p context-pixel)
  "Return the amount of scrolling nescessary, to make image EDGES visible.

Scroll as little as necessary.  Unless EAGER-P is non-nil, in
which case scroll as much as possible.

Keep CONTEXT-PIXEL pixel of the image visible at the bottom and
top of the window.  CONTEXT-PIXEL defaults to an equivalent pixel
value of `next-screen-context-lines'.

Return the require vscroll in lines or nil, if scrolling is not
needed."
  
  (let* ((win (window-inside-pixel-edges))
         (image-height (cdr (pdf-view-image-size t)))
         (image-top (window-vscroll nil t))
         (edges (pdf-util-translate
                 edges
                 (pdf-view-image-offset) t)))
    (pdf-util-with-edges (win edges)
      (let* ((context-pixel (or context-pixel
                                (* next-screen-context-lines
                                   (frame-char-height))))
             ;;Be careful not to modify edges.
             (edges-top (- edges-top context-pixel))
             (edges-bot (+ edges-bot context-pixel)))
        (if (< edges-top image-top)
            (/ (max 0 (if eager-p
                          (- edges-bot win-height)
                        edges-top))
               (float (frame-char-height)))
          (if (> (min image-height
                      edges-bot)
                 (+ image-top win-height))
              (/ (min (- image-height win-height)
                      (if eager-p
                          edges-top
                        (- edges-bot win-height)))
                 (float (frame-char-height)))))))))

(defun pdf-util-scroll-to-edges (edges &optional eager-p)
  "Scroll window such that image EDGES are visible.

Scroll as little as necessary.  Unless EAGER-P is non-nil, in
which case scroll as much as possible."
 
  (let ((vscroll (pdf-util-required-vscroll edges eager-p))
        (hscroll (pdf-util-required-hscroll edges eager-p)))
    (when vscroll
      (image-set-window-vscroll vscroll))
    (when hscroll
      (image-set-window-hscroll hscroll))))




;; * ================================================================== *
;; * Various 
;; * ================================================================== *

(defun pdf-util-pdf-buffer-p (&optional buffer)
  (and (or (null buffer)
           (buffer-live-p buffer))
       (save-current-buffer
         (and buffer (set-buffer buffer))
         (derived-mode-p 'pdf-view-mode))))

(defun pdf-util-assert-pdf-buffer (&optional buffer)
  (unless (pdf-util-pdf-buffer-p buffer)
    (error "Buffer is not in PDFView mode")))

(defun pdf-util-pdf-window-p (&optional window)
  (unless window (setq window (selected-window)))
  (and (window-live-p window)
       (with-selected-window window
         (pdf-util-pdf-buffer-p))))
  
(defun pdf-util-assert-pdf-window (&optional window)
  (unless (pdf-util-pdf-window-p window)
    (error "Window's buffer is not in PdfView mode")))

(defun pdf-util-munch-file (filename &optional multibyte-p)
  "Read contents from FILENAME and delete it.

Return the file's content as a unibyte string, unless MULTIBYTE-P
is non-nil."
  (unwind-protect
      (with-temp-buffer
        (set-buffer-multibyte multibyte-p)
        (insert-file-contents-literally filename)
        (buffer-substring-no-properties
         (point-min)
         (point-max)))
    (when (and filename
               (file-exists-p filename))
      (delete-file filename))))

(defun pdf-util-hexcolor (color)
  "Return COLOR in hex-format.

Singal an error, if color is invalid." 
  (let ((values (color-values (string-trim color))))
    (unless values
      (signal 'wrong-type-argument (list 'color-defined-p color)))
    (apply 'format "#%02x%02x%02x"
           (mapcar (lambda (c) (lsh c -8))
                   values))))

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
  (pdf-util-assert-pdf-window)
  (when (floatp image-top)
    (setq image-top
          (round (* image-top (cdr (pdf-view-image-size))))))
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
         (tooltip-hide-delay (or timeout 3)))
    (when vscroll
      (image-set-window-vscroll vscroll))
    (setq dy (max 0 (- dy
                       (cdr (pdf-view-image-offset))
                       (window-vscroll nil t))))
    (when (overlay-get (pdf-view-current-overlay) 'before-string)
      (let* ((e (window-inside-pixel-edges))
             (xw (pdf-util-with-edges (e) e-width)))
        (cl-incf dx (/ (- xw (car (pdf-view-image-size t))) 2))))
    (pdf-util-tooltip-in-window
     (propertize
      " " 'display (propertize
		    "\u2192" ;;right arrow
		    'display '(height 2)
		    'face '(:foreground
                            "orange red"
                            :background "white")))
     dx dy)))

(defvar pdf-util--face-colors-cache (make-hash-table))
  
(defun pdf-util-face-colors (face &optional dark-p)
  "Return both colors of FACE as a cons.

Look also in inherited faces.  If DARK-P is non-nil, return dark
colors, otherwise light."
  (let* ((bg (if dark-p 'dark 'light))
         (spec (list (get face 'face-defface-spec)
                     (get face 'theme-face)
                     (get face 'customized-face)))
         (cached (gethash face pdf-util--face-colors-cache)))
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
                             pdf-util--face-colors-cache)
                    colors)
                (when (and f (frame-live-p f))
                  (delete-frame f)))))))))

(defun pdf-util-window-attach (awindow &optional window)
  "Attach AWINDOW to WINDOW.

This has the following effect.  Whenever WINDOW, defaulting to
the selected window, stops displaying the buffer it currently
displays (e.g., by switching buffers or because it was deleted)
AWINDOW is deleted also."
  (unless window (setq window (selected-window)))
  (let ((buffer (window-buffer window))
        (hook (make-symbol "window-attach-hook")))
    (fset hook
          (lambda ()
            (when (or (not (window-live-p window))
                      (not (eq buffer (window-buffer window))))
              (remove-hook 'window-configuration-change-hook
                           hook)
              ;; Deleting windows inside wcch may cause errors in
              ;; windows.el .
              (run-with-timer
               0 nil (lambda (win)
                       (when (and (window-live-p win)
                                  (not (eq win (selected-window))))
                         (delete-window win)))
               awindow))))
    (add-hook 'window-configuration-change-hook hook)))

(defun display-buffer-split-below-and-attach (buf alist)
  "Display buffer action using `pdf-util-window-attach'."
  (let ((window (selected-window))
        (height (cdr (assq 'window-height alist)))
        newwin)
    (when height
      (when (floatp height)
        (setq height (round (* height (frame-height)))))
      (setq height (- (max height window-min-height))))
    (setq newwin (window--display-buffer
                  buf
                  (split-window-below height)
                  'window alist display-buffer-mark-dedicated))
    (pdf-util-window-attach newwin window)
    newwin))


;; * ================================================================== *
;; * Imagemagick's convert
;; * ================================================================== *

(defcustom pdf-util-convert-program (executable-find "convert")
  "Absolute path to the convert program."
  :group 'pdf-tools
  :type 'executable)

(defcustom pdf-util-fast-image-format nil
  "An image format appropriate for fast displaying.

This should be a cons \(TYPE . EXT\) where type is the Emacs
image-type and EXT the appropriate file extension starting with a
dot. If nil, the value is determined automatically.

Different formats have different properties, with respect to
Emacs loading time, convert creation time and the file-size.  In
general, uncompressed formats are faster, but may need a fair
amount of (temporary) disk space."
  :group 'pdf-tools)

(defun pdf-util-assert-convert-program ()
  (unless (and pdf-util-convert-program
               (file-executable-p pdf-util-convert-program))
    (error "The pdf-util-convert-program is unset or non-executable")))

(defun pdf-util-image-file-size (image-file)
  "Determine the size of the image in IMAGE-FILE.

Returns a cons \(WIDTH . HEIGHT\)."
  (pdf-util-assert-convert-program)
  (with-temp-buffer
    (when (save-excursion
            (= 0 (call-process
                  pdf-util-convert-program
                  nil (current-buffer) nil
                  image-file "-format" "%w %h" "info:")))
      (let ((standard-input (current-buffer)))
        (cons (read) (read))))))

(defun pdf-util-convert (in-file out-file &rest spec)
  "Convert image IN-FILE to OUT-FILE according to SPEC.

IN-FILE should be the name of a file containing an image.  Write
the result to OUT-FILE.  The extension of this filename ususally
determines the resulting image-type.

SPEC is a property list, specifying what the convert programm
should do with the image.  All manipulations operate on a
rectangle, see below.

SPEC may contain the following keys, respectively values.

`:foreground' Set foreground color for all following operations.

`:background' Dito, for the background color.

`:commands' A list of strings representing arguments to convert
for image manipulations.  It may contain %-escape characters, as
follows.  

%f -- Expands to the foreground color.
%b -- Expands to the background color.
%g -- Expands to the geometry of the current rectangle, i.e. WxH+X+Y.
%x -- Expands to the left edge of rectangle.
%X -- Expands to the right edge of rectangle.
%y -- Expands to the top edge of rectangle.
%Y -- Expands to the bottom edge of rectangle.
%w -- Expands to the width of rectangle.
%h -- Expands to the height of rectangle.

Keep in mind, that every element of this list is seen by convert
as a single argument.

`:formats' An alist of additional %-escapes.  Every element
should be a cons \(CHAR . STRING\) or \(CHAR . FUNCTION\).  In
the first case, all occurences of %-CHAR in the above commands
will be replaced by STRING.  In the second case FUNCTION is
called with the current rectangle and it should return the
replacement string.

`:apply' A list of rectangles \(\(LEFT TOP RIGHT BOT\) ...\) in
IN-FILE coordinates. Each such rectangle triggers one execution
of the last commands given earlier in SPEC. E.g. a call like

\(pdf-util-convert
       image-file out-file
       :foreground \"black\"
       :background \"white\"
       :commands '\(\"-fill\" \"%f\" \"-draw\" \"rectangle %x,%y,%X,%Y\"\)
       :apply '\(\(0 0 10 10\) \(10 10 20 20\)\)
       :commands '\(\"-fill\" \"%b\" \"-draw\" \"rectangle %x,%y,%X,%Y\"\)
       :apply '\(\(10 0 20 10\) \(0 10 10 20\)\)\)

would draw a 4x4 checkerboard pattern in the left corner of the
image, while leaving the rest of it as it was.

Returns OUT-FILE.

See url `http://www.imagemagick.org/script/convert.php'."
  (pdf-util-assert-convert-program)
  (let* ((cmds (pdf-util-convert--create-commands spec))
         (status (apply 'call-process
                        pdf-util-convert-program nil
                        (get-buffer-create "*pdf-util-convert-output*")
                        nil
                        `(,in-file ,@cmds ,out-file))))
    (unless (and (numberp status) (= 0 status))
      (error "The convert program exited with error status: %s" status))
    out-file))

(defun pdf-util-convert-asynch (in-file out-file &rest spec-and-callback)
  "Like `pdf-util-convert', but asynchronous.

If the last argument is a function, it is installed as the
process sentinel.

Returns the convert process."
  (pdf-util-assert-convert-program)
  (let ((callback (car (last spec-and-callback)))
        spec)
    (if (functionp callback)
        (setq spec (butlast spec-and-callback))
      (setq spec spec-and-callback
            callback nil))
    (let* ((cmds (pdf-util-convert--create-commands spec))
           (proc
            (apply 'start-process "pdf-util-convert"
                   (get-buffer-create "*pdf-util-convert-output*")
                   pdf-util-convert-program
                   `(,in-file ,@cmds ,out-file))))
      (when callback
        (set-process-sentinel proc callback))
      proc)))

(defun pdf-util-convert-page (&rest specs)
  "Convert image of current page according to SPECS.

Return the converted PNG image as a string.  See also
`pdf-util-convert'."

  (pdf-util-assert-pdf-window)
  (let ((in-file (make-temp-file "pdf-util-convert" nil ".png"))
        (out-file (make-temp-file "pdf-util-convert" nil ".png")))
    (unwind-protect
        (let ((image-data
               (plist-get (cdr (pdf-view-current-image)) :data)))
          (with-temp-file in-file
            (set-buffer-multibyte nil)
            (insert image-data))
          (pdf-util-munch-file
           (apply 'pdf-util-convert
                  in-file out-file specs)))
      (when (file-exists-p in-file)
        (delete-file in-file))
      (when (file-exists-p out-file)
        (delete-file out-file)))))
        

(defun pdf-util-convert--create-commands (spec)
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
        (:formats
         (setq formats (append formats (pop spec) nil)))
        (:apply
         (dolist (m (pop spec))
           (pdf-util-with-edges (m)
             (let ((alist (append
                           (mapcar (lambda (f)
                                     (cons (car f)
                                           (if (stringp (cdr f))
                                               (cdr f)
                                             (funcall (cdr f) m))))
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
                             (?b . ,bg)))))
               (dolist (fmt cmds)
                 (push (format-spec fmt alist) result))))))))
    (nreverse result)))

;; FIXME: Check code below and document.

(defun pdf-utils-edges-empty-p (edges)
  "Return non-nil, if EDGES area is empty." 
  (pdf-util-with-edges (edges)
    (or (<= edges-width 0)
        (<= edges-height 0))))

(defun pdf-utils-edges-inside-p (edges pos &optional epsilon)
  (pdf-utils-edges-contained-p
   edges
   (list (car pos) (cdr pos) (car pos) (cdr pos))
   epsilon))

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

(defun pdf-util-read-image-position (prompt)
  (save-selected-window
    (let ((ev (read-event
               (propertize prompt 'face 'minibuffer-prompt)))
          (buffer (current-buffer)))
      (unless (mouse-event-p ev)
        (error "Not a mouse event"))
      (let ((posn (event-start ev)))
        (unless (and (eq (window-buffer
                          (posn-window posn))
                         buffer)
                     (eq 'image (car-safe (posn-object posn))))
          (error "Invalid image position"))
        posn))))

(defun pdf-util-image-map-mouse-event-proxy (event)
  "Set POS-OR-AREA in EVENT to 1 and unread it."
  (interactive "e")
  (setcar (cdr (cadr event)) 1)
  (setq unread-command-events (list event)))

(defun pdf-util-image-map-divert-mouse-clicks (id &optional buttons)
  (dolist (kind '("" "down-" "drag-"))
    (dolist (b (or buttons '(2 3 4 5 6)))
      (local-set-key
       (vector id (intern (format "%smouse-%d" kind b)))
       'pdf-util-image-map-mouse-event-proxy))))

(defmacro pdf-util-do-events (event-resolution-unread-p condition &rest body)
  "Read EVENTs tracking mouse while CONDITION executing BODY.

Process at most 1/RESOLUTION events per second.  If UNREAD-p is
non-nil, unread the final non-processed event.

\(FN (EVENT RESOLUTION &optional UNREAD-p) CONDITION &rest BODY\)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (cl-destructuring-bind (event resolution &optional unread-p)
      event-resolution-unread-p
    (let ((*seconds (make-symbol "seconds"))
          (*timestamp (make-symbol "timestamp"))
          (*clock (make-symbol "clock"))
          (*unread-p (make-symbol "unread-p"))
          (*resolution (make-symbol "resolution")))
      `(let* ((,*unread-p ,unread-p)
              (,*resolution ,resolution)
              (,*seconds 0)
              (,*timestamp (float-time))
              (,*clock (lambda (&optional secs)
                         (when secs
                           (setq ,*seconds secs
                                 ,*timestamp (float-time)))
                         (- (+ ,*timestamp ,*seconds)
                            (float-time))))
              (,event (read-event)))
         (while ,condition
           (when (<= (funcall ,*clock) 0)
             (progn ,@body)
             (setq ,event nil)
             (funcall ,*clock ,*resolution))
           (setq ,event
                 (or (read-event nil nil
                                 (and ,event
                                      (max 0 (funcall ,*clock))))
                     ,event)))
         (when (and ,*unread-p ,event)
           (setq unread-command-events
                 (append unread-command-events
                         (list ,event))))))))

(defmacro pdf-util-track-mouse-dragging (event-resolution condition &rest body)
  (declare (indent 2) (debug ((symbolp) form body)))
  (let ((ran-once-p (make-symbol "ran-once-p")))
    `(let (,ran-once-p)
       (track-mouse
         (pdf-util-do-events (,@event-resolution t)
             ,condition
           (setq ,ran-once-p t)
           ,@body))
       (when (and ,ran-once-p
                  unread-command-events)
         (setq unread-command-events
               (butlast unread-command-events))))))
     
                                
(provide 'pdf-util)

;;; pdf-util.el ends here
