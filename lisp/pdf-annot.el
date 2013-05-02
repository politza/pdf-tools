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

;;; Code:

(defgroup pdf-annot nil
  "Annoatation support for PDF documents."
  :group 'pdf-tools)
  
(defcustom pdf-annot-convert-commands
  '(((text file)
     "(" "%i" "-resize" "%wx%h!" "-fuzz" "30%%" "-fill" "%c" "-opaque" "white" ")"
     "-geometry" "+%x+%y" "-composite"
     "-stroke" "none"
     "-fill" "none")
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

(defcustom pdf-annot-rendered-types '(text file link)
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

(defvar pdf-annot-inhibit-redraw nil
  "Inhibit redrawing the document when modifying annotations.")

(defvar pdf-annot-annotate-resize-pixel 5)

(defcustom pdf-annot-pp-for-tooltip-function nil
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
disturbed while reading the annotation."
  :group 'pdf-annot)

(defcustom pdf-annot-tablist-formats
  '((page . ("Page" 4 t :right-align t))
    (edges . ("Area(%%)" 13 t))
    (type . ("Type" 4 t))
    (id . ("ID" 12 t))
    (flags . ("Flags" 5 t :right-align t))
    (color . ("Color" 7 t))
    (contents . ("Contents" 24 t))
    (modified . ("Mtime" 24 t))
    (label . ("Label" 16 t))
    (subject . ("Subject" 16 t))
    (opacity . ("Opacity" 8 t))
    (popup-edges . ("Popup(%%)" 17 t))
    (popup-isopen . ("POp" 3 t))
    (created . ("Ctime" 24 t))
    (text-icon . ("Icon" 8 t))
    (text-state . ("State" 10 t))
    ;;
    (modified-p . ("M" 1 t)))
  
  "A alist describing formats for the list display.

The keys in this list should be annotation properties and the
values should have the format of an element of the
`tabulated-list-format' variable."
  :group 'pdf-annot)

(defcustom pdf-annot-tablist-columns
  '(page edges label subject contents)
  ""
  :group 'pdf-annot)

(defun pdf-annot-tablist-entries ()
  (unless pdf-annot-tablist-document
    (error "No PDF document associated with this buffer"))
  (mapcar (lambda (a)
            (list a (apply 'vector (mapcar (lambda (prop)
                                             (replace-regexp-in-string
                                              "\n" " "
                                              (pdf-annot-print-property a prop)
                                              t t))
                                           pdf-annot-tablist-columns))))
          (pdf-annot-getannots
           nil
           pdf-annot-listed-types pdf-annot-tablist-document)))

(defun pdf-annot-tablist-format ()
  (apply 'vector (mapcar (lambda (prop)
                           (or (cdr (assq prop pdf-annot-tablist-formats))
                               '("*Column N/A*" 12 nil)))
                         pdf-annot-tablist-columns)))

(defcustom pdf-annot-listed-types '(text)
  "The types of annotations that should appear in the list buffer."
  :group 'pdf-annot)
  
(defvar-local pdf-annot-tablist-document nil)
  
(define-derived-mode pdf-annot-tablist-mode tablist-mode
  (setq tabulated-list-entries 'pdf-annot-tablist-entries
        tabulated-list-format (pdf-annot-tablist-format))
  (make-local-variable 'pdf-annot-tablist-columns)
  (hl-line-mode 1)
  (tabulated-list-init-header))

(defun pdf-annot-list-annotations ()
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (let ((doc doc-view-buffer-file-name))
    (with-current-buffer (get-buffer-create
                          (format "*%s's annotations*"
                                  (file-name-sans-extension
                                   (buffer-name))))
      (pdf-annot-tablist-mode)
      (setq pdf-annot-tablist-document doc)
      (tabulated-list-print)
      (let ((win (pop-to-buffer (current-buffer))))
        (display-buffer
         (get-buffer-create "foo")
         `((lambda (buf alist)
             (unless (one-window-p)
               (display-buffer-in-atom-window buf alist)))
           (window-height . 15)))
        (current-buffer)))))
  
  
(defvar pdf-util-save-buffer-functions nil)

(defadvice doc-view-toggle-display (before pdf-annot-save-buffer activate)
  "Offer to save modifications to annotations, before switching modes."
  (when (and (pdf-util-pdf-buffer-p)
             pdf-annot-minor-mode
             pdf-annot-writing-supported
             (buffer-modified-p)
             (y-or-n-p "Save changes in PDF document ?"))
    ;; (save-buffer)
    ))

(define-minor-mode pdf-annot-minor-mode
  "Annotation support."
  nil nil t
  (cond
   (pdf-annot-minor-mode
    (when pdf-annot-tweak-tooltips
      (when (boundp 'x-gtk-use-system-tooltips)
        (setq x-gtk-use-system-tooltips nil))
      (setq tooltip-hide-delay 3600))
    (add-hook 'write-contents-functions 'pdf-annot-save-buffer nil t)
    (pdf-render-register-layer-function 'pdf-annot-render-function 9)
    (pdf-render-register-annotate-image-function 'pdf-annot-annotate-image 9))
   
   (t
    (remove-hook 'write-contents-functions 'pdf-annot-save-buffer t)
    (pdf-render-unregister-layer-function 'pdf-annot-render-function)
    (pdf-render-unregister-annotate-function 'pdf-annot-annotate-image)))
  (pdf-render-redraw-document)
  (pdf-util-redisplay-current-page))

;;
;; Annotation Data Structure
;; 

(defun pdf-annot-p (a)
  (and (consp a)
       (eq 'pdf-annot (car a))))

(defvar-local pdf-annot-cached-annotations nil)

(defconst pdf-annot-read-only-properties
  '(page
    type
    id
    flags
    modified
    subject
    opacity
    popup-edges
    created
    state))

(defconst pdf-annot-writeable-properties
  '(color contents label popup-isopen icon isopen))

(defun pdf-annot-new (pdf alist)
  `(pdf-annot
    (document . ,pdf)
    (modified-p)
    (deleted-p)
    ,@alist))

(defun pdf-annot-getannots (&optional pages types file-or-buffer)
  "Return the annotation on PAGES of TYPES in FILE-OR-BUFFER.

PAGES defaults to all pages, TYPES to all types and
FILE-OR-BUFFER to the current buffer."

  (unless pdf-annot-cached-annotations
    (setq pdf-annot-cached-annotations
          (make-hash-table)))
  (let ((pages (pdf-info--normalize-pages pages))
        (pdf (pdf-info--normalize-file-or-buffer file-or-buffer)))
    (when (eq 0 (cdr pages)) ;;0 stands for last page.
      (setcdr pages (pdf-info-number-of-pages pdf)))
    (setcar pages (max 1 (car pages)))
    (unless (listp types)
      (setq types (list types)))
    (let (annotations)
      (dotimes (i (1+ (- (cdr pages) (car pages))))
        (let* ((pn (+ i (car pages)))
               (anots (gethash pn pdf-annot-cached-annotations)))
          (unless anots ;;Fetch all necessary pages at once.
            (dolist (a (pdf-info-getannots pages pdf))
              (let ((pn (cdr (assq 'page a))))
                (puthash pn (append
                             (gethash pn pdf-annot-cached-annotations)
                             (list (pdf-annot-new pdf a)))
                         pdf-annot-cached-annotations)))
            ;; Remember pages w/o annotations.
            (dotimes (i (1+ (- (cdr pages) (car pages))))
              (or (gethash (+ i (car pages))
                           pdf-annot-cached-annotations)
                  (puthash (+ i (car pages)) 'none
                           pdf-annot-cached-annotations)))
            (setq anots (gethash pn pdf-annot-cached-annotations)))
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
      annotations)))

(defun pdf-annot-revert-buffer (&optional interactive)
  (interactive (list t))
  (pdf-util-assert-pdf-buffer)
  (when (or (null interactive)
            (y-or-n-p "Forget about all modifications of all annotations in this buffer ?"))
    (setq pdf-annot-cached-annotations nil)
    (pdf-util-redisplay-current-page)
    (pdf-render-redraw-document)))

(defun pdf-annot-revert-page (&optional page interactive)
  (interactive (list nil t))
  (pdf-util-assert-pdf-buffer)
  (unless page
    ;;FIXME: doc-view-current-page requires a window, but this
    ;;function is used in a dozend places, check them all.  Maybe be
    ;;better to use pdf-info-number-of-pages and some variable ?
    (pdf-util-assert-pdf-window)
    (setq page (doc-view-current-page)))
  (when (or (null interactive)
            (y-or-n-p
             (format "Forget about all modifications of the annotations on %s ?"
                     (if (= page (doc-view-current-page))
                         "the current page"
                       (format "page %d" page)))))
    (puthash page nil pdf-annot-cached-annotations)
    (when (memq page (doc-view-active-pages))
      (pdf-util-redisplay-current-page))
    (pdf-render-redraw-document nil (list page))))

(defun pdf-annot-write (a)
  (unless pdf-annot-writing-supported
    (error "Sorry, modifying PDF is not supported in this version of epdfinfo"))
  (error "Not implemented"))
                        
(defun pdf-annot-get (a prop &optional default)
  (declare (gv-setter (lambda (val)
                        `(progn
                           (when ,default
                             (error "No default value allowed here"))
                           (pdf-annot-set ,a ,prop ,val)))))
  (or (cdr (assq prop (cdr a)))
      default))

;; FIXME: Enforce types of properties (e.g. all string, but...)
(defun pdf-annot-set (a prop val)
  (declare (indent 2))
  (when (memq prop pdf-annot-read-only-properties)
    (error "Property is read-only:%s" prop))
  ;;(pdf-annot-validate-property prop val)
  (let ((curval (assq prop (cdr a))))
    (prog1
        (setcdr a (cons (cons prop val)
                        (delq curval (cdr a))))
      (unless (equal curval val)
        (pdf-annot-set-modified-p a prop t)
        (pdf-annot-redraw-maybe a)
        (when (eq prop 'edges)
          (pdf-annot-reannotate-maybe a))))))

(defun pdf-annot-validate-property (prop val &optional noerror)
  (let ((errmsg
         (if (or (null prop) (not (symbolp prop)))
             (format "Invalid property key: %s" prop)
           (cl-case prop
             (id "The id of an annotation may not be modfified")
             (type
              (unless (and val (symbolp val))
                (format "Type should be a non-nil symbol: %s" val)))
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
                               (save-match-data
                                 (string-match "\\`#[0-9a-fA-F]\\{6\\}\\'" val))))
                (format "Invalid color spec: %s" val)))
             (contents
              (unless (stringp val)
                (format "Contents should be a (possibly empty) string: %s")))
             (popup-isopen
              (unless (symbolp val)
                (format "Popup-isopen should be a flag value: %s" val)))
             ((created modified text-icon text-state)
              (unless (or (null val) (stringp val))
                (format "%s should be nil or a string: %s" prop val)))))))
    (if (null errmsg)
        t
      (unless noerror
        (error "%s" errmsg)))))
  
(defun pdf-annot-type (a)
  (cdr (assq 'type a)))

(defun pdf-annot-deleted-p (a)
  (cdr (assq 'deleted-p a)))

(defun pdf-annot-document (a)
  (cdr (assq 'document-p a)))

(defun pdf-annot-set-modified-p (a prop flag)
  (let* ((curcons (assq 'modified-p (cdr a)))
         (newval (remq prop (cdr curcons))))
    (when flag
      (push prop newval))
    (setcdr a (cons (cons 'modified-p newval)
                    (delq curcons (cdr a))))
    newval))

(defun pdf-annot-modified-p (a &optional prop)
  (let ((mod (cdr (assq 'modified-p (cdr a)))))
    (if prop (memq prop mod)
      (copy-sequence mod))))

(defun pdf-annot-has-property-p (a property)
  (not (null (cl-member property (cdr a) :key 'car))))

(defun pdf-annot-properties (a)
  (mapcar 'car (cdr a)))

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
  (or (cdr (assq 'modified (cdr a)))
      (cdr (assq 'created (cdr a)))))

(defun pdf-annot-pp-for-tooltip (a)
  (or (and pdf-annot-pp-for-tooltip-function
           (funcall pdf-annot-to-string-function a))
      (cl-case (pdf-annot-type a)
        (text
         (let ((header (propertize
                        (format "[%s]\n"
                                (mapconcat
                                 'identity
                                 (remove "unknown"
                                         (delq nil (mapcar
                                                    (lambda (prop)
                                                      (pdf-annot-get a prop))
                                                    '(subject
                                                      label
                                                      text-icon
                                                      text-state
                                                      modified))))
                                 ";"))
                        'face 'header-line 'intangible t
                        'read-only t)))
           (concat
            (propertize
             (number-to-string (float-time))
             'display
             header)
            (pdf-annot-get a 'contents))))
        (file
         (pdf-attach-pp-for-tooltip (pdf-attach-get-from-annot a)))
        (t (format "Annotation of type `%s'" (pdf-annot-type a))))))

(defun pdf-annot-position (a &optional image-size)
  "Return the position of annotation A in image coordinates.

If IMAGE-SIZE, a cons \(WIDTH . HEIGHT\), is not given, the image
in the selected window is used as a reference."
  (unless image-size
    (setq image-size (pdf-util-image-size)))
  (let ((edges (pdf-util-scale-edges
                (pdf-annot-get a 'edges) image-size)))
    (cons (car edges) (cadr edges))))

(defun pdf-annot-set-position (a x y &optional image-size)
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

(defun pdf-annot-size (a &optional image-size)
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

(defun pdf-annot-set-size (a &optional width height image-size)
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
      

(defun pdf-annot-edges (a &optional image-size)
  "Return annotation A's edges in image coordinats.

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
  (pdf-util-scale-edges
   (pdf-annot-get a 'edges)
   (or image-size (pdf-util-image-size))))

(defun pdf-annot-set-edges (a edges &optional image-size)
  "Set annotation A's edges in it's image.

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
  (unless image-size
    (setq image-size (pdf-util-image-size)))
  (let ((scale (cons (/ 1.0 (car image-size))
                     (/ 1.0 (cdr image-size)))))
    (pdf-annot-set a 'edges
      (pdf-util-scale-edges edges scale))))

(defun pdf-annot-print-property (a prop)
  (or (and pdf-annot-print-property-function
           (funcall pdf-annot-print-property-function
                    a prop))
      (pdf-annot-print-property-default a prop)))

(defun pdf-annot-print-property-default (a prop)
  (cl-case prop
    (label
     (let ((color (pdf-annot-get a 'color)))
       (propertize (or (pdf-annot-get a prop)
                       "")
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
    ((popup-isopen modified-p)
     (if (pdf-annot-get a p) "*" " "))
    ;; print verbatim
    (t (format "%s" (or (pdf-annot-get a prop) "")))))       
    
  
  
;;
;; Drawing Annotations
;;

(defun pdf-annot-render-get-image (a)
  "Return the image for annotation A's.

The image is found by consulting `pdf-annot-render-images'."

  (let ((file (pdf-annot-render-search-alist
               a pdf-annot-render-images)))
    (when file
      (pdf-annot-render-search-image file))))

(defun pdf-annot-render-search-image (file)
  "Search FILE in the usual places.

This searches FILE in `pdf-annot-render-image-load-path' and then
`image-load-path'.  The result is remebered in the variable
`pdf-annot-render-found-images' and returned on following
invocations of this function."
  (cdr (or (assoc file pdf-annot-render-found-images)
           (car (push (cons file (image-search-load-path
                                  file
                                  (cons pdf-annot-render-image-load-path
                                        image-load-path)))
                      pdf-annot-render-found-images)))))

(defun pdf-annot-render-search-alist (a alist)
  "Search a value matching  annotation A in ALIST.

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
            (if (or (and (consp (car elt))
                         (memq type (car elt)))
                    (eq (car elt) type)
                    (and (functionp (car elt))
                         (funcall (car elt) a)))
                (throw 'found (cdr elt)))))))))

(defun pdf-annot-convert-command-specs (a)
  "Return an alist of format specs for annotation A.

This function consults `pdf-annot-convert-command-specs' and
returns a resolved format spec list for A."

  (delq nil (mapcar (lambda (elt)
                      (let ((value (funcall (cdr elt) a)))
                        (when value (cons (car elt) value))))
                    pdf-annot-convert-command-specs)))
  
(defun pdf-annot-render-function (page)
  "The render function for annotations.

To be registered with `pdf-render-register-layer-function'."
  (let ((annots (sort
                 (identity;; copy-sequence
                  (pdf-annot-getannots page pdf-annot-rendered-types))
                      (lambda (a1 _)
                        (eq 'link (pdf-annot-type a1)))))
        (size (pdf-util-png-image-size))
        cmds)
    (dolist (a annots)
      (unless (pdf-annot-deleted-p a)
        (let ((cmd (pdf-annot-render-search-alist
                    a  pdf-annot-convert-commands))
              (specs (pdf-annot-convert-command-specs a)))
          (when cmd 
            (unless (and cmd (listp (car cmd)))
              (setq cmd (list cmd)))
            (push :commands cmds)
            (push`(,@cmd ,@specs) cmds)
            (push :apply cmds)
            (push (list (pdf-util-scale-edges
                         (pdf-annot-get a 'edges) size))
                  cmds)))))
    (nreverse cmds)))

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
               (help (list (pdf-annot-pp-for-tooltip a)
                           "Drag to resize horizontally."
                           "Drag to resize vertically."
                           "Drag to resize.")))
          (dotimes (i 4)
            (push `((rect . ,(pop rects))
                    ,(nth i ids)
                    (pointer
                     ,(nth i pointer)
                     help-echo
                     ,(nth i help)))
                  map)
            (global-set-key
             (vector (nth i ids) 'down-mouse-1)
             `(lambda (ev)
                (interactive "@e")
                (pdf-annot-move/resize-mouse
                 ',a ev ',(nth i ops))))))))
    (plist-put props
               :map
               (append map (plist-get props :map)))))

(defun pdf-annot-redraw-maybe (a)
  "Redraw the page of annotation A.

Redraw if A's file is open, A looks valid and
`pdf-annot-inhibit-redraw' is nil."
  (unless pdf-annot-inhibit-redraw
    (let* ((pdf-annot-inhibit-redraw t)
           (page (pdf-annot-get a 'page))
           (edges (pdf-annot-get a 'edges))
           (pdf (pdf-annot-document a))
           (buffer (and pdf (get-file-buffer pdf))))
      (when (and page
                 (> page 0)
                 (consp edges)
                 buffer)
        (pdf-render-redraw-document buffer (list page))))))

(defun pdf-annot-reannotate-maybe (a)
  "Update the page of annotation A.

This function reinserts the page and thus reapplys it's image
properties, e.g. like the hotspots for the mouse.

Update if A's file is open and `pdf-annot-inhibit-redraw' is
nil."
  (unless pdf-annot-inhibit-redraw
    (let* ((pdf-annot-inhibit-redraw t)
           (pdf (pdf-annot-document a))
           (buf (and pdf (get-file-buffer pdf))))
      (when buf
        (dolist (win (pdf-util-doc-view-windows buf))
          (with-selected-window win
            (pdf-util-redisplay-current-page)))))))

(defun pdf-annot-set-pointer-shape (shape)
  (if (null shape)
      (set-mouse-color (frame-parameter nil 'mouse-color))
    (let ((x-pointer-shape
           (if (numberp shape) shape
             (if (and (symbolp shape)
                      (boundp shape))
                 (symbol-value shape)))))
      (set-mouse-color (frame-parameter nil 'mouse-color)))))
  
(defun pdf-annot-move/resize-mouse (a event operation)
  "Start moving/resizing annotation A with the mouse.

EVENT should be a mouse event originating the request and is used
as a reference for OPERATION.

OPERATION should be one of resize-horizontally,
resize-vertically, resize (which means in both directions) or
move.

This function does not return until the operation is completet,
i.e. a non mouse-movement event is read."

  (pdf-util-assert-pdf-window (posn-window (event-start event)))
  (with-selected-window (posn-window (event-start event))
    (let* ((mpos (posn-object-x-y (event-start event)))
           (apos (pdf-annot-position a))
           (pdf-annot-inhibit-redraw t)
           (offset (cons (- (car mpos) (car apos))
                         (- (cdr mpos) (cdr apos))))
           (edges (pdf-annot-get a 'edges))
           (asize (pdf-annot-size a))
           (awidth (car asize))
           (aheight  (cdr asize))
           (hresize (memq operation '(resize-horizontally
                                      resize-diagonally)))
           (vresize (memq operation '(resize-vertically
                                      resize-diagonally))))

      (unwind-protect
          (pdf-render-with-redraw
              'pdf-annot-render-function
            (pdf-annot-set-pointer-shape
             x-pointer-hand2)
            (plist-put (cdr (doc-view-current-image))
                       :pointer 'text)
            (let (make-pointer-invisible ev)
              (while (and (track-mouse
                            (setq ev (read-event)))
                          (eq 'mouse-movement (event-basic-type ev)))
                (when (eq 'image (car-safe (posn-object (event-start ev))))
                  (let ((xy (posn-object-x-y (event-start ev))))
                    (pdf-util-with-edges (edges)
                      (cond
                       ((or hresize vresize)
                        (pdf-annot-set-size
                         a (if hresize
                               (max 5 (+ awidth (- (car xy) (car mpos)))))
                         (if vresize
                             (max 5 (+ aheight (- (cdr xy) (cdr mpos)))))))
                       (t
                        (pdf-annot-set-position
                         a (- (car xy) (car offset))
                         (- (cdr xy) (cdr offset)))))))
                  (redraw)))
              (unless (mouse-event-p ev)
                (setq unread-command-events
                      (list ev)))))
        (pdf-annot-set-pointer-shape nil)))))
  
   
(provide 'pdf-annot)
;;; pdf-annot.el ends here

