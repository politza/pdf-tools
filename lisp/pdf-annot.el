;;; pdf-annot.el ---  -*- lexical-binding: t -*-


;; Copyright (C) 2013  Andreas Politz

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
;; TODO:
;; * Handle server quit and more generally outdated datastructures.
;; * Create a context menu
;;   - Delete annotation
;;   - Open attachment
;;   - Browse annotations (-> tablist)
;;   - Hide annotations (global menu)
;;   - Create annotation (global menu)
;;

(require 'pdf-render)
(require 'pdf-attach)
(require 'facemenu) ;;for list-colors-duplicates
(require 'faces) ;;for color-values

;;; Code:

(defgroup pdf-annot nil
  "Annoatation support for PDF documents."
  :group 'pdf-tools)
  
(defcustom pdf-annot-convert-commands
  '(((text file)
     "(" "%i" "-resize" "%wx%h!" "-fuzz" "30%%" "-fill" "%c" "-opaque" "white" ")"
     "-geometry" "+%x+%y" "-composite"
     "-fill" "none")
    (annot-highlight
     ( "-fill" "tomato" "-draw" "roundrectangle %x,%y,%X,%Y,5,5"))
    ((lambda (a)  (and (eq 'link (pdf-annot-type a))
                       (pdf-annot-get a 'color)))
     "-stroke" "%c"
     "-fill" "none"
     "-draw" "rectangle %x,%y,%X,%Y"
     "-stroke" "none"))
  
  "The arguments for the `convert' program.

See `pdf-isearch-convert-commands' for the general format of this
variable.

In addition to the format specs mentioned there, these strings
may also contain specs configured in `pdf-annot-convert-command-specs'."
  :group 'pdf-annot
  :link '(variable-link pdf-isearch-convert-commands))
  
(defcustom pdf-annot-convert-command-specs
  '((?i . pdf-annot-render-get-image)
    (?c . (lambda (a) (pdf-annot-get a 'color "#ffffff")))
    (?Z . (lambda (a)
            (lambda (m) (number-to-string
                         (- (nth 3 m) 15))))))

  "An alist of additional format specs.

Each element should be a cons \(CHAR . REPLACEMENT\), where CHAR
is the format character to be replaced in
`pdf-annot-convert-commands' and REPLACEMENT is a function
receiving a annotation and returning the string to replace the
format spec with."
  :group 'pdf-annot)

(defcustom pdf-annot-rendered-types '(link text file)
  "A list of annoatation types to be rendered.

Only annoatations whoose type is a member of this list are
considered for rendering.

There should be corresponding formats and maybe images for all
types in this list, see `pdf-annot-convert-commands' and
`pdf-annot-render-images'."
  :group 'pdf-annot)
  
(defcustom pdf-annot-renderable-types '(text file)
  "A list of annoatation types potentially renderable.

This list is used when querying the user about what to render.
Corresponding formats and maybe images for all types in this list
should be defined, see `pdf-annot-convert-commands' and
`pdf-annot-render-images'."
  :group 'pdf-annot)

(defcustom pdf-annot-render-images
  '((file . "paperclip.xpm")
    (text . "speech411.xpm"
          ;; "emacs-document_small.xpm"
          ))
          ;; "icons/hicolor/scalable/mimetypes/emacs-document.svg"
  "An alist of images to use when rendering annotations.

This should be a list of \(TYPE . FILE\), where TYPE is either a
symbol or a list of symbols representing an annotation's type and
FILE is the name of an image-file, which will be looked up in
`pdf-annot-render-image-load-path' and `image-load-path'.

It may also be a function, receiving an annotation and returning
the (absolute) filename of an image.

The image is used, when replacing the %i spec in
`pdf-annot-convert-commands'.  For best results, i.e. fast
rendering, images in this list should be of a simple,
uncompressed format, e.g. bmp2."
  :group 'pdf-annot)

(defcustom pdf-annot-render-image-load-path
  (expand-file-name
   "images"
   (if load-file-name
       (file-name-directory load-file-name)
     default-directory))
  "An additional directory for looking up images."
  :group 'pdf-annot)
  
(defvar pdf-annot-render-found-images nil
  "Alist of image filenames to resolved absolute filenames.")

(defvar pdf-annot-annotate-resize-pixel 0)

(defcustom pdf-annot-print-tooltip-functions
  '(pdf-annot-print-tooltip-latex-maybe)
  "A function for pretty printing annotations.

The function receices one argument, the annotation, and it should
either return a string or nil.  In the later case, the annotation
is printed with the default print function."
  :group 'pdf-annot)

(defcustom pdf-annot-print-property-function nil
  "A function for pretty printing annotation properties.

The function receices two arguments, the annotation and a
property, and it should either return a string or nil.  In the
later case, the property is printed with the default print
function."
  :group 'pdf-annot)

(defcustom pdf-annot-tweak-tooltips t
  "Whether this package should tweak some settings regarding tooltips.

If this variable has a non-nil value,

`x-gtk-use-system-tooltips' is set to nil if appropriate, in
order to display text properties;

`tooltip-hide-delay' is set to infinity, in order to not beeing
annoyed while reading the annotations."
  :group 'pdf-annot)

(defconst pdf-annot-types
  '( unknown text link free-text line square
             circle polygon poly-line highlight underline squiggly
             strike-out stamp caret ink popup file sound movie widget screen
             printer-mark trap-net watermark 3d ))
  
(defconst pdf-annot-standard-icons
  '("Note"
    "Comment"
    "Key"
    "Help"
    "NewParagraph"
    "Paragraph"
    "Insert"
    "Cross"
    "Circle"))


;;
;; *Annotation Data Structure
;; 

(defvar-local pdf-annot-annotations nil
  "A hash table of all annotations in the current buffer.")

(defstruct (pdf-annot
            (:constructor pdf-annot-new (buffer properties))
            (:constructor nil))
  ;;Annotation's designated buffer.
  buffer
  ;;A list of properties to be written to the PDF later.
  modified-properties
  ;;List of modified properties and events.
  deleted-p
  ;; The annotations properties.
  properties)

(defun pdf-annot-getannots (&optional pages types buffer)
  "Return a list of annotations from PAGES of TYPES in BUFFER.

See `pdf-info--normalize-pages' for valid values of PAGES.  TYPES
may be a symbol or list of symbols denoting types of annotations.

PAGES defaults to all pages, TYPES to all types and BUFFER to the
current buffer."

  (let ((pages (pdf-info--normalize-pages pages)))
    (save-current-buffer
      (when buffer (set-buffer buffer))
      (unless pdf-annot-annotations
        (setq pdf-annot-annotations
              (make-hash-table)))
      (when (eq 0 (cdr pages)) ;;0 stands for last page.
        (setcdr pages (pdf-info-number-of-pages)))
      (setcar pages (max 1 (car pages)))
      (unless (listp types)
        (setq types (list types)))
      (let (annotations)
        (dotimes (i (1+ (- (cdr pages) (car pages))))
          (let* ((pn (+ i (car pages)))
                 (anots (gethash pn pdf-annot-annotations)))
            (unless anots ;;Fetch all necessary pages at once.
              (dolist (a (pdf-info-getannots pages))
                (let ((pn (cdr (assq 'page a))))
                  (puthash pn (append
                               (gethash pn pdf-annot-annotations)
                               (list (pdf-annot-new (current-buffer) a)))
                           pdf-annot-annotations)))
              ;; Remember pages w/o annotations.
              (dotimes (i (1+ (- (cdr pages) (car pages))))
                (or (gethash (+ i (car pages))
                             pdf-annot-annotations)
                    (puthash (+ i (car pages)) 'none
                             pdf-annot-annotations)))
              (setq anots (gethash pn pdf-annot-annotations)))
            (when (consp anots)
              (if (null types)
                  (setq annotations (append annotations anots))
                (setq annotations
                      (append annotations
                              (cl-remove-if-not
                               (lambda (a)
                                 (memq (pdf-annot-type a) types))
                               anots)
                              nil))))))
        annotations))))

(defun pdf-annot-get (a prop &optional default)
  (or (cdr (assq prop (pdf-annot-properties a)))
      default))
  
(defun pdf-annot-set (a prop val)
  (declare (indent 2))
  (unless (pdf-annot-property-writable-p a prop)
    (error "Property is read-only:%s" prop))
  (pdf-annot-validate-property-value prop val)
  (let ((elt (assq prop (pdf-annot-properties a))))
    (setf (pdf-annot-properties a)
          (cons (cons prop val)
                (delq elt (pdf-annot-properties a))))
    (unless (equal (cdr elt) val)
      (with-current-buffer (pdf-annot-buffer a)
        (set-buffer-modified-p t)
        (pdf-annot-set-property-modified-p a prop t)
        (pdf-annot-run-pages-modified-hooks
         (pdf-annot-get a 'page ))))
    val))

(defvar pdf-annot-pages-modified-functions nil)
(defvar pdf-annot-inhibit-modification-hooks nil)
(defvar-local pdf-annot-modified-pages nil)

(defun pdf-annot-run-pages-modified-hooks (&optional pages)
  (unless (listp pages)
    (setq pages (list pages)))
  (if pdf-annot-inhibit-modification-hooks
      (setq pdf-annot-modified-pages
            (cl-remove-duplicates (append pdf-annot-modified-pages pages)))
    (let ((pages (cl-remove-duplicates
                  (append pdf-annot-modified-pages pages))))
      (setq pdf-annot-modified-pages nil)
      (when pages
        (run-hook-with-args
         'pdf-annot-pages-modified-functions pages)))))

(defmacro pdf-annot-with-multiple-modifications (&rest body)
  (declare (indent 0) (debug t))
  (org-with-gensyms (buffer)
    `(let ((,buffer (current-buffer)))
       (unwind-protect
           (let ((pdf-annot-inhibit-modification-hooks t))
             (progn ,@body))
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             (pdf-annot-run-pages-modified-hooks)))))))

(defun pdf-annot-validate-property-value (prop val &optional noerror)
  (let ((errmsg
         (if (or (null prop) (not (symbolp prop)))
             (format "Invalid property key: %s" prop)
           (cl-case prop
             (page (unless (natnump val)
                     (format "Page should be a natural number: %s" val)))
             ((type id)
              (unless (and val (symbolp val))
                (format "Type or id should be a non-nil symbol: %s" val)))
             (edges
              (unless (and (consp val)
                           (= (length val) 4)
                           (let ((eps 1e-6))
                             (cl-every (lambda (e)
                                         (and (numberp e)
                                              (>= e (- 0 eps))
                                              (<= e (+ 1 eps))))
                                       val)))
                (format "Edges should be a list of 4 numbers each >=0 and <=1: %s" val)))
             (flags
              (unless (natnump val)
                (format "Flags should be a natural number: %s" val)))
             (color
              (unless (or (null val)
                          (and (stringp val)
                               (pdf-annot-colorname-to-hex val)))
                (format "Invalid color spec: %s" val)))
             (contents
              (unless (stringp val)
                (format "Contents should be a string: %s")))
             (popup-isopen
              (unless (symbolp val)
                (format "Popup-isopen should be a flag value: %s" val)))
             ((created modified icon state)
              (unless (or (null val) (stringp val))
                (format "%s should be nil or a string: %s" prop val)))))))
    (if (null errmsg)
        t
      (unless noerror
        (error "%s" errmsg)))))
  
(defun pdf-annot-type (a)
  (cdr (assq 'type (pdf-annot-properties a))))

(defun pdf-annot-set-deleted-p (a flag)
  (unless (or (and (pdf-annot-deleted-p a)
                   flag)
              (and (not (pdf-annot-deleted-p a))
                   (not flag)))
    (setf (pdf-annot-deleted-p a) (not (null flag)))
    (with-current-buffer (pdf-annot-buffer a)
      (set-buffer-modified-p t)
      (pdf-annot-run-pages-modified-hooks
       (pdf-annot-get a 'page))))
  (pdf-annot-deleted-p a))

(defun pdf-annot-set-property-modified-p (a prop flag)
  (let ((set (memq prop (pdf-annot-modified-properties a)))
        (mod (remq prop (pdf-annot-modified-properties a))))
    (setf (pdf-annot-modified-properties a)
          (if flag (cons prop mod) mod))
    flag))

(defun pdf-annot-property-modified-p (a prop)
  (not (null (memq prop (pdf-annot-modified-properties a)))))

(defun pdf-annot-has-property-p (a property)
  (memq property (mapcar 'car (pdf-annot-properties a))))

(defun pdf-annot-is-markup-p (a)
  (memq (pdf-annot-type a)
        '(text
          link
          free-text
          line
          square
          circle
          polygon
          poly-line
          highlight
          underline
          squiggly
          strike-out
          stamp
          caret
          ink
          file
          sound)))  

(defun pdf-annot-is-text-p (a)
  (eq 'text (pdf-annot-type a)))

(defun pdf-annot-has-attachment-p (a)
  (eq 'file (pdf-annot-type a)))

(defun pdf-annot-get-attachment (a)
  (unless (pdf-annot-has-attachment-p a)
    (error "Annotation has no data attached: %s" a))
  (pdf-attach-get-from-annot a))

(defun pdf-annot-date (a)
  (or (cdr (assq 'modified (pdf-annot-properties a)))
      (cdr (assq 'created (pdf-annot-properties a)))))

(defun pdf-annot-image-position (a &optional image-size)
  "Return the position of annotation A in image coordinates.

If IMAGE-SIZE, a cons \(WIDTH . HEIGHT\), is not given, the image
in the selected window is used as a reference."
  (unless image-size
    (setq image-size (pdf-util-image-size)))
  (let ((edges (pdf-util-scale-edges
                (pdf-annot-get a 'edges) image-size)))
    (cons (car edges) (cadr edges))))

(defun pdf-annot-image-set-position (a x y &optional image-size)
  "Set annotation A's position in image.

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
  (unless image-size
    (setq image-size (pdf-util-image-size)))
  (let* ((edges (pdf-annot-get a 'edges))
         (x (/ x (float (car image-size))))
         (y (/ y (float (cdr image-size)))))
    (pdf-util-with-edges (edges)
      (let* ((w edges-width)
             (h edges-height)
             (x (max 0 (min x (- 1 w))))
             (y (max 0 (min y (- 1 h)))))
        (pdf-annot-set a 'edges
          (list x y (+ x w) (+ y h)))))
    (cons x y)))

(defun pdf-annot-image-size (a &optional image-size)
  "Return the size of annotation A in image coordinates.

Return value is a cons of \(WIDTH . HEIGHT\).

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
  (unless image-size
    (setq image-size (pdf-util-image-size)))
  (let ((edges (pdf-util-scale-edges
                (pdf-annot-get a 'edges) image-size)))
    (pdf-util-with-edges (edges)
      (cons edges-width edges-height))))

(defun pdf-annot-image-set-size (a &optional width height image-size)
  "Set annotation A's size in image to WIDTH and/or HEIGHT.

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
  (unless image-size
    (setq image-size (pdf-util-image-size)))
  (let* ((edges (pdf-annot-get a 'edges))
         (dx (and width
                  (/ width (float (car image-size)))))
         (dy (and height
                  (/ height (float (cdr image-size))))))
    (pdf-util-with-edges (edges)
      (pdf-annot-set a 'edges
        (list edges-left
              edges-top
              (if dx (+ edges-left dx) edges-right)
              (if dy (+ edges-top dy) edges-bot))))))
      

(defun pdf-annot-image-edges (a &optional image-size)
  "Return annotation A's edges in image coordinats.

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
  (pdf-util-scale-edges
   (pdf-annot-get a 'edges)
   (or image-size (pdf-util-image-size))))

(defun pdf-annot-image-set-edges (a edges &optional image-size)
  "Set annotation A's edges in it's image.

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
  (unless image-size
    (setq image-size (pdf-util-image-size)))
  (let ((scale (cons (/ 1.0 (car image-size))
                     (/ 1.0 (cdr image-size)))))
    (pdf-annot-set a 'edges
      (pdf-util-scale-edges edges scale))))

(defvar pdf-annot-text-annot-defaults
  `((icon . "Note")
    (color . ,(pdf-annot-colorname-to-hex "pink"))
    (label . ,user-full-name)
    (isopen . nil))
  "An alist of default properties for new text annotations.")
  
;; Standard size for text annotations is 24x24 pixel.
;; Standard origin is roughly at (6,24) .
(defun pdf-annot-add-text-annot (x y &optional page window)
  "Create a new text-annot pointing at X,Y on PAGE in WINDOW."
  (interactive
   (let ((xy (pdf-util-read-image-position
              "Click on the spot where a annotation should be added")))
     (list (car xy) (cdr xy))))
  (let (a)
    (save-selected-window
      (when window (select-window window))
      (pdf-util-assert-pdf-window)
      (unless page (setq page (doc-view-current-page)))
      (let* ((isize (pdf-util-image-size page))
             (size (pdf-info-pagesize page))
             (x1 (- (/ (float x) (car isize))
                    (/ 6.0 (car size))))
             (y1 (- (/ (float y) (cdr isize))
                    (/ 24.0 (cdr size))))
             (x2 (+ x1 (/ 24.0 (car size))))
             (y2 (+ y1 (/ 24.0 (cdr size)))))
        
        (pdf-annot-with-multiple-modifications
         (setq a (pdf-annot-add-text-annot-raw
                  page (list x1 y1 x2 y2)))
         (dolist (elt pdf-annot-text-annot-defaults)
           (pdf-annot-set a (car elt) (cdr elt))))))
    ;; FIXME: Selecting the edit window won't work otherwise.
    (run-with-timer 0.001 nil 'pdf-annot-edit-text a)
    a))

(defun pdf-annot-add-text-annot-at-event (ev)
  (interactive "@e")
  (let ((window (posn-window (event-start ev)))
        (pos (posn-object-x-y (event-start ev))))
    (pdf-annot-add-text-annot
     (car pos) (cdr pos) nil window)))
  
(defun pdf-annot-add-text-annot-raw (page edges &optional buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (pdf-util-assert-pdf-buffer)
    (pdf-annot-validate-property-value 'page page)
    (pdf-annot-validate-property-value 'edges edges)
    (let ((a (pdf-annot-new
              (current-buffer)
              `((type . text)
                (page . ,page)
                (edges . ,edges)))))
      (puthash page
               (append
                (pdf-annot-getannots page)
                (list a))
               pdf-annot-annotations)
      a)))

(defun pdf-annot-revert-document (&optional interactive no-redisplay)
  (interactive (list t))
  (pdf-util-assert-pdf-buffer)
  (when (or (null interactive)
            (y-or-n-p "Abandon all modifications of all annotations in this buffer ?"))
    (setq pdf-annot-annotations nil)
    (set-buffer-modified-p nil)
    (unless no-redisplay
      (pdf-render-redisplay-current-page)
      (pdf-render-redraw-document))))

  
(defun pdf-annot-revert-page (&optional page interactive no-redisplay)
  (interactive "P\np")
  (pdf-util-assert-pdf-buffer)
  (unless page
    ;;FIXME: doc-view-current-page requires a window, but this
    ;;function is used in a dozend places, check them all.  Maybe be
    ;;better to use pdf-info-number-of-pages and some variable ?
    (pdf-util-assert-pdf-window)
    (setq page (doc-view-current-page)))
  (when (or (null interactive)
            (y-or-n-p
             (format "Abandon all modifications of the annotations on %s ?"
                     (if (= page (ignore-errors
                                   (doc-view-current-page)))
                         "the current page"
                       (format "page %d" page)))))
    (let ((modified (cl-remove-if-not (lambda (a)
                                        (or (pdf-annot-modified-properties a)
                                            (pdf-annot-deleted-p a)))
                                      (pdf-annot-getannots page))))
      (puthash page nil pdf-annot-annotations)
      (when modified
        (set-buffer-modified-p t)
        (pdf-annot-run-pages-modified-hooks page)))))

(defun pdf-annot-print-property (a prop)
  (or (and pdf-annot-print-property-function
           (funcall pdf-annot-print-property-function
                    a prop))
      (pdf-annot-print-property-default a prop)))

(defun pdf-annot-print-property-default (a prop)
  (cl-case prop
    (color
     (let ((color (pdf-annot-get a 'color)))
       (propertize (or color "")
                   'face (and color
                              `(:background ,color)))))
    ((edges popup-edges)
     (let ((e (pdf-annot-get a prop)))
       (if (null e)
           ""
         (apply 'format "%2dx%d+%d+%d"
                (pdf-util-with-edges (e)
                  (mapcar (lambda (e)
                            (round (* 100 e)))
                          (list e-left e-top e-width e-height)))))))
    ((created modified)
     (let ((date (pdf-annot-get a prop)))
       (if (null date)
           "Unknown"
         (current-time-string date))))
    ((popup-isopen)
     (if (pdf-annot-get a prop) "%" " "))
    ;; print verbatim
    (t (format "%s" (or (pdf-annot-get a prop) "")))))       

(defun pdf-annot-print-tooltip (a)
  (or (run-hook-with-args-until-success
       'pdf-annot-print-tooltip-functions a)
      (let ((header ))
        (concat
         (pdf-annot-print-tooltip-header a)
         (cl-case (pdf-annot-type a)
           (text
            (pdf-annot-get a 'contents))
           (t
            "Customize `pdf-annot-print-tooltip-functions',\
 to change what is displayed here."))))))

(defun pdf-annot-print-tooltip-header (a)
  (let ((header
         (propertize
          (format "[%s]\n"
                  (mapconcat
                   'identity
                   (mapcar
                    (lambda (prop)
                      (pdf-annot-print-property
                       a prop))
                    (if (pdf-annot-is-text-p a)
                        '(label
                          icon
                          modified)
                      '(type modified)))
                   ";"))
          'face 'header-line)))
    (propertize header 'display header)))


(defvar pdf-annot-latex-string-predicate
  (lambda (str)
    (string-match "\\`[[:space:]]*\\(?:\\$\\|\\\\[[(]\\|\\\\begin\\)" str)))
  
(defun pdf-annot-print-tooltip-latex-maybe (a)
  (when (funcall pdf-annot-latex-string-predicate
                 (pdf-annot-get a 'contents))
    (pdf-annot-print-tooltip-latex a)))

  
(defun pdf-annot-print-tooltip-latex (a)
  (let* ((header (pdf-annot-print-tooltip-header a))
         (contents (pdf-annot-get a 'contents))
         (pngfile (pdf-util-cache-make-filename
                   'pdf-annot-latex
                   "png"
                   header contents))
         (temporary-file-directory
          (pdf-util-cache-make-filename
           'pdf-annot-latex "latex")))

    (unless (file-exists-p temporary-file-directory)
      (make-directory temporary-file-directory))
    (unless (file-exists-p pngfile)
      (org-create-formula-image-with-dvipng
       contents pngfile org-format-latex-options t)
      (clear-image-cache pngfile))
    (concat
     header
     (if (file-exists-p pngfile)
         (propertize
          contents 'display (create-image pngfile))
       (propertize
        contents
        'display
        (concat
         (propertize "Failed to compile latex fragment\n"
                     'face 'error)
         contents))))))
;;
;; *Drawing Annotations
;;

(defun pdf-annot-render-bring-to-front (a)
  (with-current-buffer (pdf-annot-buffer a)
    (let* ((page (pdf-annot-get a 'page))
           (annots (pdf-annot-getannots page)))
      (unless (memq a annots)
        (error "Invalid annotation: %s" a))
      (puthash page (append (delq a annots)
                            (list a))
               pdf-annot-annotations))))

(defun pdf-annot-render-get-image (a)
  "Return the image to use for rendering annotation A.

The image is found by consulting `pdf-annot-render-images'."

  (let ((file (pdf-annot-render-search-alist
               a pdf-annot-render-images)))
    (when file
      (pdf-annot-render-search-image file))))

(defun pdf-annot-render-search-image (file)
  "Search for image FILE.

This searches FILE in `pdf-annot-render-image-load-path' and then
`image-load-path'.  The result is remembered in the variable
`pdf-annot-render-found-images'."
  
  (cdr (or (assoc file pdf-annot-render-found-images)
           (car (push (cons file (image-search-load-path
                                  file
                                  (cons pdf-annot-render-image-load-path
                                        image-load-path)))
                      pdf-annot-render-found-images)))))

(defun pdf-annot-render-search-alist (a alist)
  "Search a value matching annotation A in ALIST.

ALIST is either a list with annotation TYPE as keys, or a list
thereof, or key is a function of one argument.  Search for a
non-nil value.

Or ALIST is a function of one argument, the annotation A. In this
case the result is whatever the funtion returns."

  (if (functionp alist)
      (funcall alist a)
    (let ((type (pdf-annot-type a)))
      (if (symbolp (car alist))
          (memq type alist)
        (catch 'found
          (dolist (elt alist)
            (if (or (and (functionp (car elt))
                         (funcall (car elt) a))
                    (and (consp (car elt))
                         (memq type (car elt)))
                    (eq (car elt) type))
                (throw 'found (cdr elt)))))))))

(defun pdf-annot-convert-command-specs (a)
  "Return an alist of format specs for annotation A.

This function consults `pdf-annot-convert-command-specs' and
returns a resolved format spec list for A."

  (delq nil (mapcar (lambda (elt)
                      (let ((value (funcall (cdr elt) a)))
                        (when value (cons (car elt) value))))
                    pdf-annot-convert-command-specs)))
  
(defun pdf-annot-render-commands (a)
  (let ((cmd (pdf-annot-render-search-alist
              a  pdf-annot-convert-commands))
        (specs (pdf-annot-convert-command-specs a)))
    (when cmd
      (unless (and cmd (listp (car cmd)))
        (setq cmd (list cmd)))
      `(,@cmd ,@specs))))

(defun pdf-annot-render-function (page)
  "The render function for annotations.

To be registered with `pdf-render-register-layer-function'."
  (let ((annots
         (sort
          (pdf-annot-getannots page pdf-annot-rendered-types)
          (lambda (a1 a2)
            (> (length (memq (pdf-annot-type a1)
                             pdf-annot-rendered-types))
               (length (memq (pdf-annot-type a2)
                             pdf-annot-rendered-types))))))
        (size (pdf-util-png-image-size))
        cmds)
    (dolist (a annots)
      (unless (pdf-annot-deleted-p a)
        (let ((cmd (pdf-annot-render-commands a)))
          (when cmd 
            (unless (and cmd (listp (car cmd)))
              (setq cmd (list cmd)))
            (push :commands cmds)
            (push cmd cmds)
            (push :apply cmds)
            (push (list (pdf-util-scale-edges
                         (pdf-annot-get a 'edges) size))
                  cmds)))))
    (nreverse cmds)))

(defun pdf-annot-highlight (a &optional window)
  (save-selected-window
    (when window (select-window window))
    (pdf-util-assert-pdf-window)
    (unless (= (pdf-annot-get a 'page)
               (doc-view-current-page))
      (doc-view-goto-page (pdf-annot-get a 'page)))
    (let ((hcmd (cdr (assq 'annot-highlight
                           pdf-annot-convert-commands)))
          (acmd (pdf-annot-render-commands a))
          (edges (pdf-util-scale-edges
                  (list (pdf-annot-get a 'edges))
                  (pdf-util-png-image-size))))
      (when (and hcmd acmd)
        (pdf-render-momentarily
         :commands hcmd
         :apply edges
         :commands acmd
         :apply edges)))))
           
(defun pdf-annot-annotate-image (fn page size) 
  "The image annotation function.

To be registered with `pdf-render-register-annotate-image-function'."
  (let* ((props (funcall fn page size))
         (annots (pdf-annot-getannots page '(text file)))
         (d pdf-annot-annotate-resize-pixel)
         (pointer '(hand hdrag vdrag arrow))
         (ops '(move resize-horizontally resize-vertically
                     resize-diagonally))
         map)
    (dolist (a annots)
      (unless (pdf-annot-deleted-p a)
        (let* ((e (pdf-util-scale-edges
                   (pdf-annot-get a 'edges)
                   size))
               (ids (let ((id (pdf-annot-get a 'id)))
                      (list id
                            (intern (format "%s-h" id))
                            (intern (format "%s-v" id))
                            (intern (format "%s-d" id)))))
               
               (rects `(((,(nth 0 e) . ,(nth 1 e))
                         . (,(- (nth 2 e) d) . ,(- (nth 3 e) d)))
                        ((,(- (nth 2 e) d) . ,(nth 1 e))
                         . (,(nth 2 e) . ,(nth 3 e)))
                        ((,(nth 0 e) . ,(- (nth 3 e) d))
                         . (,(nth 2 e) . ,(nth 3 e)))
                        ((,(- (nth 2 e) d) . ,(- (nth 3 e) d))
                         . (,(nth 2 e) . ,(nth 3 e)))))
               (help (list (pdf-annot-print-tooltip a)
                           "Drag to resize horizontally."
                           "Drag to resize vertically."
                           "Drag to resize.")))
          (dotimes (i (if (> pdf-annot-annotate-resize-pixel 0) 4 1))
            (push `((rect . ,(pop rects))
                    ,(nth i ids)
                    (pointer
                     ,(nth i pointer)
                     help-echo
                     ,(nth i help)))
                  map)
            (local-set-key
             (vector (nth i ids) 'down-mouse-1)
             `(lambda (ev)
                (interactive "@e")
                (pdf-annot-move/resize-mouse
                 ev ',(nth i ops) ',a 'pdf-annot-edit-text-mouse)))))))
    (plist-put props
               :map
               (append map (plist-get props :map)))))

(defun pdf-annot-redraw-pages (pages)
  "Redraw PAGES of the current buffer."
  (when pages
    (pdf-render-redraw-document nil pages)))

(defun pdf-annot-reannotate-pages (pages)
  "Update PAGES of the current buffer.

This function reinserts the page and thus reapplys it's image
properties, e.g. the hotspots for the mouse."
  (dolist (win (pdf-util-doc-view-windows))
    (with-selected-window win
      (when (and (pdf-util-page-displayed-p)
                 (memq (doc-view-current-page) pages))
        (pdf-render-redisplay-current-page)))))

(defun pdf-annot-set-pointer-shape (shape)
  (if (null shape)
      (set-mouse-color (frame-parameter nil 'mouse-color))
    (let ((x-pointer-shape
           (if (numberp shape) shape
             (if (and (symbolp shape)
                      (boundp shape))
                 (symbol-value shape)))))
      (set-mouse-color (frame-parameter nil 'mouse-color)))))
  
(defun pdf-annot-at-position (pos)
  "Return annotation at POS in the selected window.

POS should be an absolute image position as a cons \(X . Y\).
Return nil, if no annotation was found."
  (let* ((annots (pdf-annot-getannots (doc-view-current-page)))
         (size (pdf-util-image-size))
         (offset (pdf-util-image-offset))
         (rx (+ (/ (car pos) (float (car size)))
                (car offset)))
         (ry (+ (/ (cdr pos) (float (cdr size)))
                (cdr offset)))
         (rpos (cons rx ry)))
    (cl-some (lambda (a)
               (and (pdf-utils-edges-inside-p
                     (pdf-annot-get a 'edges)
                     rpos)
                    a))
             annots)))
  
(defun pdf-annot-move/resize-mouse (event &optional operation annot click-fn)
  "Start moving/resizing annotation at EVENT's position.

EVENT should be a mouse event originating the request and is used
as a reference for OPERATION.

OPERATION should be one of resize-horizontally,
resize-vertically, resize (which means in both directions) or
move, which is also the default.

If CLICK-FN is non-nil, EVENT is a down event and the user
releases the same button without moving the mouse, call this
function with EVENT as sole argument.

ANNOT is the annotation to operate on and defaults to the
annotation at EVENT's start position.

This function does not return until the operation is completed,
i.e. a non mouse-movement event is read."

  (pdf-util-assert-pdf-window (posn-window (event-start event)))
  (select-window (posn-window (event-start event)))
  (let* ((mpos (posn-object-x-y (event-start event)))
         (a (or annot
                (pdf-annot-at-position mpos))))
    (unless a
      (error "No annotation at event's position: %s" event))
    (let* ((apos (pdf-annot-image-position a))
           (pdf-annot-inhibit-modification-hooks t)
           (buffer (current-buffer))
           (offset (cons (- (car mpos) (car apos))
                         (- (cdr mpos) (cdr apos))))
           (edges (pdf-annot-get a 'edges))
           (asize (pdf-annot-image-size a))
           (awidth (car asize))
           (aheight  (cdr asize))
           (hresize (memq operation '(resize-horizontally
                                      resize-diagonally)))
           (vresize (memq operation '(resize-vertically
                                      resize-diagonally)))
           (button (event-basic-type event))
           (ev (track-mouse (read-event))))
      (pdf-annot-render-bring-to-front a)
      (if (not (eq 'mouse-movement (event-basic-type ev)))
          (if (and click-fn
                   (eq button (event-basic-type ev)))
              (funcall click-fn (prog1 ev (setq ev nil))))
        (unwind-protect
            (pdf-render-with-redraw
                'pdf-annot-render-function
              (pdf-annot-set-pointer-shape
               x-pointer-hand2)
              (plist-put (cdr (doc-view-current-image))
                         :pointer 'text)
              (let (make-pointer-invisible
                    pdf-render-annotate-functions)
                (while (eq 'mouse-movement (event-basic-type ev))
                  (when (eq 'image (car-safe (posn-object (event-start ev))))
                    (let ((xy (posn-object-x-y (event-start ev))))
                      (pdf-util-with-edges (edges)
                        (cond
                         ((or hresize vresize)
                          (pdf-annot-image-set-size
                           a (if hresize
                                 (max 5 (+ awidth (- (car xy) (car mpos)))))
                           (if vresize
                               (max 5 (+ aheight (- (cdr xy) (cdr mpos)))))))
                         (t
                          (pdf-annot-image-set-position
                           a (- (car xy) (car offset))
                           (- (cdr xy) (cdr offset)))))))
                    (redraw))
                  (track-mouse
                    (setq ev (read-event))))
                (with-current-buffer buffer
                  (pdf-annot-run-pages-modified-hooks
                   (pdf-annot-get a 'page)))))
          (pdf-annot-set-pointer-shape nil)))
      (when (and ev (not (mouse-event-p ev)))
        (setq unread-command-events
              (list ev))))))


;;
;; *Editing text contents
;; 

(defvar-local pdf-annot-edit-annotation nil)
(put 'pdf-annot-edit-annotation 'permanent-local t)
(defvar-local pdf-annot-edit-buffer nil)

(defvar pdf-annot-edit-text-major-mode
  (lambda ()
    (let ((a pdf-annot-edit-annotation))
      (if (and a (funcall pdf-annot-latex-string-predicate
                          (pdf-annot-get a 'contents)))
          (latex-mode)
        (text-mode)))))
  
(defvar pdf-annot-edit-text-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap text-mode-map)
    (define-key kmap (kbd "C-c C-c") 'pdf-annot-edit-text-commit)
    (define-key kmap (kbd "C-c C-q") 'pdf-annot-edit-text-quit)
    kmap))
      
(define-minor-mode pdf-annot-edit-text-mode
  "Mode for editing the contents of annotations."
  nil nil nil
  (when pdf-annot-edit-text-mode
    (message "%s"
             (substitute-command-keys
              "Press \\[pdf-annot-edit-text-commit] to commit your changes, \\[pdf-annot-edit-text-quit] to abandon them."))))

(put 'pdf-annot-edit-text-mode 'permanent-local t)

(defun pdf-annot-edit-text-finalize (do-save &optional do-kill)
  (when (buffer-modified-p)
    (cond
     ((eq do-save 'ask)
      (save-window-excursion
        (display-buffer (current-buffer) nil (selected-frame))
        (when (y-or-n-p "Save changes to this annotation ?")
          (pdf-annot-edit-text-save-annotation))))
     (do-save
      (pdf-annot-edit-text-save-annotation)))
    (set-buffer-modified-p nil))
  (dolist (win (get-buffer-window-list))
    (quit-window do-kill win)))
        
(defun pdf-annot-edit-text-save-annotation ()
  (when (and pdf-annot-edit-annotation
             (buffer-modified-p))
    (pdf-annot-set pdf-annot-edit-annotation
        'contents
      (buffer-substring-no-properties (point-min) (point-max)))
    (set-buffer-modified-p nil)))

(defun pdf-annot-edit-text-commit ()
  (interactive)
  (pdf-annot-edit-text-finalize t))

(defun pdf-annot-edit-text-quit ()
  (interactive)
  (pdf-annot-edit-text-finalize nil t))

(defun pdf-annot-edit-text-noselect (a)
  (with-current-buffer (pdf-annot-buffer a)
    (when (and (buffer-live-p pdf-annot-edit-buffer)
               (not (eq a pdf-annot-edit-annotation)))
      (with-current-buffer pdf-annot-edit-buffer
        (pdf-annot-edit-text-finalize 'ask)))
    (unless (buffer-live-p pdf-annot-edit-buffer)
      (setq pdf-annot-edit-buffer
            (with-current-buffer (get-buffer-create
                                  (format "*Edit Annotation %s*"
                                          (buffer-name)))
              (pdf-annot-edit-text-mode)
              (current-buffer))))
    (with-current-buffer pdf-annot-edit-buffer
      (setq pdf-annot-edit-annotation a)
      (funcall pdf-annot-edit-text-major-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion (insert (pdf-annot-get a 'contents)))
        (set-buffer-modified-p nil)
        (current-buffer)))))

(defvar pdf-annot-edit-text-display-buffer-action
  '((display-buffer-reuse-window
     display-buffer-split-below-and-attach)
    (inhibit-same-window . t)
    (window-height . 0.25)))

(defun pdf-annot-edit-text (a)
  (let ((window (selected-window)))
    (select-window
     (display-buffer
      (pdf-annot-edit-text-noselect a)
      pdf-annot-edit-text-display-buffer-action))
    (when (and (window-live-p window)
               (eq (window-buffer window)
                   (pdf-annot-buffer a)))
      (with-selected-window window
        (pdf-annot-display-annotation a)))))
  
(defun pdf-annot-edit-text-mouse (ev)
  (interactive "@e")
  (let* ((pos (posn-object-x-y (event-start ev)))
         (a (and pos (pdf-annot-at-position pos))))
    (unless a
      (error "No annotation at this position"))
    (pdf-annot-edit-text a)))

;;
;; *Listing annotations
;;

(defvar-local pdf-annot-list-document-buffer nil)

(defvar pdf-annot-list-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "SPC") 'pdf-annot-display-annotation)
    km))

(defun pdf-annot-list-entries ()
  (unless (buffer-live-p pdf-annot-list-document-buffer)
    (error "No PDF document associated with this buffer"))
  (mapcar 'pdf-annot-list-create-entry
          (cl-remove-if 'pdf-annot-deleted-p
                        (pdf-annot-getannots nil 'text pdf-annot-list-document-buffer))))

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
            tablist-find-entry-function 'pdf-annot-list-find-entry
            tablist-edit-column-function 'pdf-annot-list-edit-column
            tablist-delete-function 'pdf-annot-list-delete-function
            tablist-edit-column-completions-function 'pdf-annot-list-completions
            tablist-major-columns '(1 2 3 4 5)
            tablist-minor-columns '(0))
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

(defun pdf-annot-list-insert (a)
  (let ((new (pdf-annot-add-text-annot (pdf-annot-buffer a))))
    (pdf-annot-set new 'color (pdf-annot-get a 'color))
    (pdf-annot-set new 'page (pdf-annot-get a 'page))
    (pdf-annot-list-create-entry a)))
  
(defun pdf-annot-list-edit-column (id column item)
  (let ((prop (nth column '(nil nil color icon nil label nil))))
    (unless prop
      (error "Invalid column (read-only)"))
    (when (eq prop 'color)
      (setq item (pdf-annot-colorname-to-hex item)))
    (pdf-annot-validate-property-value prop item)
    (pdf-annot-set id prop item)
    (cadr (pdf-annot-list-create-entry id))))

(defun pdf-annot-list-delete-function (annots)
  (when (buffer-live-p pdf-annot-list-document-buffer)
    (with-current-buffer pdf-annot-list-document-buffer
      (pdf-annot-with-multiple-modifications
        (dolist (a annots)
          (pdf-annot-set-deleted-p a t))))))
  
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
          (pdf-util-image-size t)))
        (pdf-annot-highlight a)))))

(defun pdf-annot-list-find-entry (a)
  (let ((buffer (pdf-annot-buffer a)))
    (with-selected-window (or (get-buffer-window buffer)
                              (display-buffer buffer))
      (pdf-annot-edit-text a))
    (let* ((window (get-buffer-window pdf-annot-edit-buffer))
           (quit-restore (and window
                              (window-parameter window 'quit-restore))))
      (when (and window quit-restore)
        (setcar (nthcdr 2 quit-restore) (selected-window)))
      (when window
        (select-window window)))))
  
(defun pdf-annot-list-completions (id column &rest _ignore)
  (let ((prop (case column
               (4 'color)
               (5 'icon))))
    (when prop
      (pdf-annot-property-completions prop))))

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
              'pdf-annot-display-annotation nil t))
   (t
    (remove-hook 'tablist-selection-changed-functions
                 'pdf-annot-display-annotation t))))

   
(provide 'pdf-annot)
;;; pdf-annot.el ends here

;; *Utility functions
;; 

(defun pdf-annot-property-completions (prop)
  (case prop
    (color (pdf-annot-color-completions))
    (icon (copy-sequence pdf-annot-standard-icons))))

(defun pdf-annot-color-completions ()
  (let ((color-list (list-colors-duplicates))
        colors)
    (dolist (cl color-list)
      (dolist (c (reverse cl))
        (push (propertize c 'face `(:background ,c))
              colors)))
    (nreverse colors)))

(defun pdf-annot-colorname-to-hex (color-name)
  (or
   (save-match-data
     (and (string-match "\\` *\\(#[0-9a-fA-F]\\{6\\}\\) *\\'" color-name)
          (match-string 1 color-name)))
   (let ((values (color-values color-name)))
     (unless values
       (error "Invalid color name: %s" color-name))
     (apply 'format "#%02x%02x%02x"
            (mapcar (lambda (c) (lsh c -8))
                    values)))))

;; 

(defadvice doc-view-toggle-display (before pdf-annot-save-document activate)
  "Offer to save modifications to annotations, before switching modes."
  (when (and (pdf-util-pdf-buffer-p)
             pdf-annot-minor-mode
             pdf-info-writing-supported
             (buffer-modified-p)
             (y-or-n-p "Save changes in PDF document ?"))
    (save-buffer)))

(defun pdf-annot-property-writable-p (a prop)
  (and pdf-info-writing-supported
       (or (memq prop '(edges color))
           (and (pdf-annot-is-markup-p a)
                (memq prop '(label contents popup-isopen)))
           (and (pdf-annot-is-text-p a)
                (memq prop '(isopen icon))))))
  
(defun pdf-annot-save-document ()
  (interactive)
  (when (buffer-modified-p)
    (unless pdf-info-writing-supported
      (error "Sorry, writing PDFs not supported by this version of epdfinfo"))
    (let (write-needed)
      (dolist (a (pdf-annot-getannots))
        (when (and (null (pdf-annot-get a 'id))
                   (not (pdf-annot-deleted-p a)))
          (setq write-needed t)
          (let ((id
                 (assq 'id (apply
                            'pdf-info-addannot
                            (pdf-annot-get a 'page)
                            (pdf-annot-get a 'edges)))))
            (push id (pdf-annot-properties a))))
        (when (and (pdf-annot-deleted-p a)
                   (pdf-annot-get a 'id))
          (setq write-needed t)
          (pdf-info-delannot (pdf-annot-get a 'id)))
        (unless (pdf-annot-deleted-p a)
          (let (modifications)
            (dolist (prop (pdf-annot-modified-properties a))
              (push (cons prop (pdf-annot-get a prop))
                    modifications))
            (when modifications
              (setq write-needed t)
              (pdf-info-editannot (pdf-annot-get a 'id)
                                  modifications)))))
      (when write-needed
        (let ((tmpfile (pdf-info-save))
              (old-cache (doc-view-current-cache-dir)))
          (rename-file tmpfile (buffer-file-name) t)
          (setq f tmpfile)
          (unless (file-equal-p
                   (buffer-file-name)
                   doc-view-buffer-file-name)
            (copy-file (buffer-file-name)
                       doc-view-buffer-file-name t))
          ;; Recompute the MD5 dirname.
          (setq doc-view-current-cache-dir nil)
          (let ((new-cache (doc-view-current-cache-dir)))
            (rename-file old-cache new-cache t))))
      (pdf-info-close)
      (clear-visited-file-modtime)
      (pdf-annot-revert-document)
      (set-buffer-modified-p nil)))
  t)
        

(defvar pdf-annot-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [remap doc-view-revert-buffer] 'doc-view-reconvert-doc)
    (define-key kmap (kbd "C-c C-a C-a") 'pdf-annot-add-text-annot)
    (define-key kmap (kbd "C-c C-a a") 'pdf-annot-add-text-annot)
    (define-key kmap (kbd "C-c C-a C-l") 'pdf-annot-list-annotations)
    (define-key kmap (kbd "C-c C-a l") 'pdf-annot-list-annotations)
    (define-key kmap (kbd "C-c C-a C-d") 'pdf-annot-toggle-display-annotations)
    (define-key kmap (kbd "C-c C-a d") 'pdf-annot-toggle-display-annotations)
    (define-key kmap (kbd "C-c C-a C-r") 'pdf-annot-revert-page)
    (define-key kmap (kbd "C-c C-a r") 'pdf-annot-revert-page)
    (define-key kmap (kbd "C-c C-a R") 'pdf-annot-revert-document)
    kmap))

(defun pdf-annot-toggle-display-annotations ()
  (interactive)
  (setq-local pdf-annot-rendered-types 
              (if (memq 'text pdf-annot-rendered-types)
                  '(link)
                '(link text file)))
  (pdf-render-redraw-document))
  
(define-minor-mode pdf-annot-minor-mode
  "Annotation support."
  nil nil nil
  (cond
   (pdf-annot-minor-mode
    (when pdf-annot-tweak-tooltips
      (when (boundp 'x-gtk-use-system-tooltips)
        (setq x-gtk-use-system-tooltips nil))
      (setq tooltip-hide-delay 3600))
    (when (boundp 'doc-view-pdf->png-converter-function)
      (set (make-local-variable 'doc-view-pdf->png-converter-function)
           'doc-view-pdf->png-converter-ghostscript))
    (make-local-variable 'doc-view-ghostscript-options)
    (pdf-render-ghostscript-configure 0)
    (add-hook 'write-contents-functions 'pdf-annot-save-document nil t)
    (pdf-render-register-layer-function 'pdf-annot-render-function 9)
    (pdf-render-register-annotate-image-function 'pdf-annot-annotate-image 9)
    (add-hook 'pdf-annot-pages-modified-functions 'pdf-annot-redraw-pages nil t)
    (add-hook 'pdf-annot-pages-modified-functions
              'pdf-annot-reannotate-pages nil t)
    (add-hook 'pdf-info-after-close-document-hook
              'pdf-annot-revert-document nil t)
    (add-hook 'pdf-util-after-reconvert-hook
              'pdf-render-redraw-document nil t))
   
   (t
    (kill-local-variable 'doc-view-ghostscript-options)
    (when (boundp 'doc-view-pdf->png-converter-function)
      (kill-local-variable 'doc-view-pdf->png-converter-function))
    (remove-hook 'write-contents-functions 'pdf-annot-save-document t)
    (pdf-render-unregister-layer-function 'pdf-annot-render-function)
    (pdf-render-unregister-annotate-function 'pdf-annot-annotate-image)
    (remove-hook 'pdf-annot-pages-modified-functions
                 'pdf-annot-redraw-pages t)
    (remove-hook 'pdf-annot-pages-modified-functions
                 'pdf-annot-reannotate-pages t)
    (remove-hook 'pdf-info-after-close-document-hook
                 'pdf-annot-revert-document t)
    (remove-hook 'pdf-util-after-reconvert-hook 'pdf-render-redraw-document t)))
  (pdf-render-redraw-document)
  (pdf-render-redisplay-current-page))
    
;;
