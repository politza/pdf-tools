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

(require 'pdf-render)

;;; Code:

(defgroup pdf-annot nil
  "Annoatation support for PDF documents."
  :group 'pdf-tools)
  
(defcustom pdf-annot-convert-commands
  '(((file text)
     "-stroke" "#222222"
     "-fill" "%c"
     "-draw" "rectangle %x,%y,%X,%Y"
     "(" "%i" "-resize" "%wx%h" ")" "-geometry" "+%x+%y" "-composite"
     "-stroke" "none"
     "-fill" "none"))
  "The arguments for the `convert' program.

See `pdf-isearch-convert-commands' for the general format of this
variable.

In addition to the format specs mentioned there, these strings
may also contain specs configured in `pdf-annot-convert-command-specs'."
  :group 'pdf-annot
  :link '(variable-link pdf-isearch-convert-commands))
  
(defcustom pdf-annot-convert-command-specs
  '((?i . pdf-annot-render-get-image)
    (?c . (lambda (a) (pdf-annot-get a 'color "0xffffff"))))

  "An alist of additional format specs.

Each element should be a cons \(CHAR . REPLACEMENT\), where CHAR
is the format character to be replaced in
`pdf-annot-convert-commands' and REPLACEMENT is a function
receiving a annotation and returning the string to replace the
format spec with."
  :group 'pdf-annot)

(defcustom pdf-annot-rendered-types '(text file)
  "A list of annoatation types to be rendered.

Only annoatations whoose type is a member of this list are
considered for rendering.

There should be corresponding formats and maybe images for all
types in this list, see `pdf-annot-convert-commands' and
`pdf-annot-render-images'."
  :group 'pdf-annot)
  

(defcustom pdf-annot-renderable-types '(text file)
  "A list of annoatation types potentially renderable.

This list is used when querying the user about what to render and
there should exist corresponding formats and maybe images for all
types in this list, see `pdf-annot-convert-commands' and
`pdf-annot-render-images'."
  :group 'pdf-annot)

(defcustom pdf-annot-render-images
  '((file . "save.xpm")
    (text . "~/.emacs.d/working/pdf-tools/aux/emacs-document_small.xpm"))
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

(defcustom pdf-annot-pp-to-string-function nil
  "A function for pretty printing annotations.

The function receices one argument, the annotation, and it should
either return a string or nil.  In the later case, the annotation
is printed with the default print function."
  :group 'pdf-annot)

(defcustom pdf-annot-tweak-tooltips t
  "Whether this package should tweak some settings regarding tooltips.

If this variable has a non-nil value,

`x-gtk-use-system-tooltips' is set to nil if appropriate, in
order to display text properties;

`tooltip-hide-delay' is set to infinity, in order to not beeing
disturbed while reading the annotation."
  :group 'pdf-annot)
  
(define-minor-mode pdf-annot-minor-mode
  "Annotation support."
  nil nil t
  (cond
   (pdf-annot-minor-mode
    (when pdf-annot-tweak-tooltips
      (when (boundp 'x-gtk-use-system-tooltips)
        (setq x-gtk-use-system-tooltips nil))
      (setq tooltip-hide-delay 3600))
    (pdf-render-register-layer-function 'pdf-annot-render-function 9)
    (pdf-render-register-annotate-image-function 'pdf-annot-annotate-image 9))
   
   (t
    (pdf-render-unregister-layer-function 'pdf-annot-render-function)
    (pdf-render-unregister-annotate-function 'pdf-annot-annotate-image))))

;;
;; Drawing Annotations
;;

(defun pdf-annot-render-get-image (a)
  "Return the image for annotation A's.

The image is found by consulting `pdf-annot-render-images'."

  (pdf-annot-render-search-image
   (pdf-annot-render-search-alist
    a pdf-annot-render-images)))

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

ALIST is either a list, where every element looks like \(TYPE
. VALUE\) or \((TYPE1 TYPE2 ...\) . VALUE\), where TYPE is an
annotation type.  In this case the result is the VALUE for the
first element, for which A's type is one of TYPE.

Or ALIST is a list of symbols \(TYPE1 TYPE2 ...\) and the result
is non-nil, if A's type is a member of this list.

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
                    (eq (car elt) type))
                (throw 'found (cdr elt)))))))))

(defun pdf-annot-convert-command-specs (a)
  "Return an alist of format specs for annotation A.

This function consults `pdf-annot-convert-command-specs' and
returns a resolved format spec list for A."

  (mapcar (lambda (elt)
            (cons (car elt) (funcall (cdr elt) a)))
          pdf-annot-convert-command-specs))


(defun pdf-annot-render-function (page)
  "The render function for annotations.

To be registered with `pdf-render-register-layer-function'."
  (let ((annots (pdf-annot-getannots page pdf-annot-rendered-types))
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

(defvar pdf-annot-annotate-resize-pixel 5)
  
(defun pdf-annot-annotate-image (fn page size)
  "The image annotation function.

To be registered with `pdf-render-register-annotate-image-function'."
  (let* ((props (funcall fn page size))
         (annots (pdf-annot-getannots page 'text))
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
               (help (list (pdf-annot-pp-to-string a)
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
    (let* ((page (pdf-annot-get a 'page))
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
    (let* ((pdf (pdf-annot-document a))
           (buf (and pdf (get-file-buffer pdf))))
      (when buf
        (dolist (win (pdf-util-doc-view-windows buf))
          (with-selected-window win
            (pdf-util-display-image nil)))))))

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
  
   
;;
;; Annotation Data Structure
;; 

(defun pdf-annot-p (a)
  (and (consp a)
       (eq 'pdf-annot (car a))))

(defvar-local pdf-annot-cached-annotations nil)

(defun pdf-annot-new (pdf alist)
  `(pdf-annot
    (metadata
     (document . ,pdf)
     (modified)
     (deleted))
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
    (when (eq 0 (cdr pages))
      (setcdr pages (pdf-info-number-of-pages pdf)))
    (unless (listp types)
      (setq types (list types)))
    (apply
     'nconc
     (mapcar
      (lambda (pn)
        (let ((anots
               (or (gethash pn pdf-annot-cached-annotations)
                   (dolist (a (pdf-info-getannots pages pdf)
                              (gethash pn pdf-annot-cached-annotations))
                     (puthash (cdr (assq 'page a))
                              (append
                               (gethash (cdr (assq 'page a))
                                        pdf-annot-cached-annotations)
                               (list (pdf-annot-new pdf a)))
                              pdf-annot-cached-annotations)))))
          (if (consp anots)
              (if (null types)
                  anots
                (delq nil (mapcar (lambda (a)
                                    (and (memq (pdf-annot-type a)
                                               types)
                                         a))
                                  anots)))
            ;; Remember that this page has no annotations.
            (unless anots
              (puthash pn 'none pdf-annot-cached-annotations))
            nil)))
      (number-sequence (car pages) (cdr pages))))))
                            

(defun pdf-annot-write (a)
  (unless pdf-annot-write-supported
    (error "Sorry, modifying PDF is not supported in this version of epdfinfo"))
  (error "Not implemented"))
                        
(defun pdf-annot-get (a prop &optional default)
  (declare (gv-setter (lambda (val)
                        `(progn
                           (when ,default
                             (error "No default value allowed here"))
                           (pdf-annot-set ,a ,prop ,val)))))
  (unless (eq prop 'metadata)
    (or (cdr (assq prop (cdr a)))
        default)))

;; FIXME: Enforce types of properties (e.g. all string, but...)
(defun pdf-annot-set (a prop val)
  (declare (indent 2))
  (when (eq prop 'metadata)
    (error "Invalid property: %s" prop))
  ;;(pdf-annot-validate-property prop val)
  (let ((curval (assq prop (cdr a))))
    (prog1
        (setcdr a (cons (cons prop val)
                        (delq curval (cdr a))))
      (unless (equal curval val)
        (pdf-annot-set-modified a t)
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
  
;; metadata
(defun pdf-annot-document (a)
  "Return the PDF file associated with annotation A."
  (cdr (assq 'document (assq 'metadata a))))

(defun pdf-annot-modified-p (a)
  "Return the PDF file associated with annotation A."
  (cdr (assq 'modified (assq 'metadata a))))

(defun pdf-annot-set-modified (a flag)
  "Return the PDF file associated with annotation A."
  (setcdr (assq 'modified (assq 'metadata a))
          flag))

(defun pdf-annot-deleted-p (a)
  "Return the PDF file associated with annotation A."
  (cdr (assq 'deleted (assq 'metadata a))))

(defun pdf-annot-set-deleted (a flag)
  "Return the PDF file associated with annotation A."
  (setcdr (assq 'deleted (assq 'metadata a))
          flag))

(defun pdf-annot-type (a)
  (cdr (assq 'type (cdr a))))

(defun pdf-annot-has-property-p (a property)
  (and (not (eq property 'metadata))
       (not (not (assq property (cdr a))))))

(defun pdf-annot-properties (a)
  (delq 'metadata (mapcar 'car  (cdr a))))

(defun pdf-annot-is-markup-p (a)
  (pdf-annot-has-property-p a 'label))

(defun pdf-annot-is-text-p (a)
  (pdf-annot-has-property-p a 'text-icon))

(defun pdf-annot-has-attachment-p (a)
  (eq 'file (pdf-annot-type a)))

(defun pdf-annot-get-attachment (a)
  (unless (pdf-annot-has-attachment-p a)
    (error "Annotation has no data attached: %s" a))
  (pdf-attach-get-from-annot a))

(defun pdf-annot-pp-to-string (a)
  (or (and pdf-annot-pp-to-string-function
           (funcall pdf-annot-to-string-function a))
      (cl-case (pdf-annot-type a)
        (text
         (let ((header (propertize
                        (format "[%s]\n"
                                (mapconcat
                                 'identity
                                 (delq nil (mapcar
                                            (lambda (prop)
                                              (pdf-annot-get a prop))
                                            '(subject
                                              label
                                              text-icon
                                              text-state
                                              modified)))
                                 ";"))
                        'face 'header-line 'intangible t
                        'read-only t)))
           (concat
            (propertize
             (make-string (length header) ?\s)
             'display
             header)
            (pdf-annot-get a 'contents))))
        (t (format "Annotation of type `%s'" (pdf-annot-type a))))))

(defun pdf-annot-position (a &optional image-size)
  "Return the position of annotation A in image coordinates.

If IMAGE-SIZE is not given, the image in the selected window is
used as a reference."
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

(provide 'pdf-annot)
;;; pdf-annot.el ends here

