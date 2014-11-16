;;; pdf-annot.el --- Annotation support for PDF files.  -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

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


(defgroup pdf-annot nil
  "Annoatation support for PDF documents."
  :group 'pdf-tools)

(defvar pdf-annot-annotate-resize-pixel 5)

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

(defvar pdf-annot-latex-string-predicate
  (lambda (str)
    (string-match "\\`[[:space:]\n]*[$\\]" str)))

(defconst pdf-annot-types
  '(unknown text link free-text line square
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

(defcustom pdf-annot-modified-functions nil
  "Functions to call, when an annotation was modified.

A function on this hook should accept three arguments: A list of
inserted, changed and deleted annotations, of which at least one
list is non-empty."
  :group 'pdf-annot
  :type 'hook)

(defvar pdf-annot-inhibit-modification-hooks nil
  "Non-nil, if running `pdf-annot-modified-functions' should be
  inhibited after some annotation has changed.")

(defvar-local pdf-annot-delayed-modified-annotations nil
  "A plist of not yet propagated modifications.

It contains three entries :change, :delete and :insert.  Each one
holding a list of annotations.")

(defmacro pdf-annot-assert-writable-annotations ()
  (unless (pdf-cache-writable-annotations-p)
    (error "Modifying annotations is
not supported by this version of libpoppler")))

(defun pdf-annot-getannots (&optional pages types buffer)
  "Return a list of annotations on PAGES of TYPES in BUFFER.

See `pdf-info-normalize-pages' for valid values of PAGES.  TYPES
may be a symbol or list of symbols denoting annotation types.

PAGES defaults to all pages, TYPES to all types and BUFFER to the
current buffer."

  (pdf-util-assert-pdf-buffer buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (unless (listp types)
    (setq types (list types)))
  (delq nil (mapcar (lambda (a)
                      (and (memq (pdf-annot-get a 'type) types)
                           (cons `(pdf-annot-buffer . ,buffer)
                                 a)))
                    (pdf-info-getannots pages buffer))))

(defun pdf-annot-get (a property &optional default)
  "Get A's value of PROPERTY.

Return DEFAULT if value is nil."
  (or (cdr (assq property a)) default))

(defun pdf-annot-put (a property value)
  "Set A's PROPERTY to VALUE.

Unless VALUE is `equal' to oldvalue, set A's buffer's modified
flag and run `pdf-annot-modified-functions'.  Signal an error, if
PROPERTY is not modifiable. Return the modified annotation."
  (declare (indent 2))
  (pdf-annot-assert-writable-annotations)
  (when (pdf-annot-deleted-p a)
    (error "Modifying a deleted annotation: %s"
           (pdf-annot-get a 'id "<invalid>")))
  (unless (pdf-annot-property-writable-p a property)
    (error "Modifying a read-only property: %s" property))
  (let ((oldvalue (pdf-annot-get a property)))
    (unless (equal value (pdf-annot-get a property))
      (with-current-buffer (pdf-annot-get-buffer a)
        (let ((edit `((,property . ,value))))
          (pdf-info-editannot
           (pdf-annot-get a 'id) edit)
          (if (null oldvalue)
              (push edit a)
            (setcdr (assq property a) value))
          (set-buffer-modified-p t)
          (pdf-annot-run-modified-hooks a))))
    a))

(defun pdf-annot-equal (a1 a2)
  "Return non-nil, if A1 and A2 are equal.

Two annotations are equal if they belong to the same buffer and
have identical ids."
  (and (eq (pdf-annot-buffer a1)
           (pdf-annot-buffer a2))
       (eq (pdf-annot-get a1 'id)
           (pdf-annot-get a2 'id))))
  
(defun pdf-annot-get-buffer (a)
  "Return annotation A's buffer.

Singal an error if A's buffer was killed."
  (let ((buffer (pdf-annot-get a 'pdf-annot-buffer)))
    (unless (buffer-live-p buffer)
      (error "Buffer of annotation %s is dead"
             (pdf-annot-get a 'id "<invalid>")))
    buffer))

(defun pdf-annot-get-id (a)
  "Return ID property of A."
  (pdf-annot-get a 'id))

(defun pdf-annot-deleted-p (a)
  "Return non-nil, if A was deleted."
  (pdf-annot-get a 'pdf-annot-deleted))

(defun pdf-annot-delete (a)
  "Delete annotation A.

Return the deleted annotation."
  (pdf-annot-assert-writable-annotations)
  (when (pdf-annot-deleted-p a)
    (error "Annotation was already deleted: %s"
           (pdf-annot-get a 'id "<invalid>")))
  (with-current-buffer (pdf-annot-buffer a)
    (pdf-info-delannot
     (pdf-annot-get a 'id))
    (set-buffer-modified-p t)
    (push `(pdf-annot-deleted . t) a)
    (pdf-annot-run-modified-hooks a))
  a)

(defun pdf-annot-run-modified-hooks (&optional operation &rest annotations)
  "Run `pdf-annot-modified-functions' using OPERATION on ANNOTATIONS.

OPERATION should be one of nil, :change, :insert or :delete.  If
nil, annotations should be empty.  If
`pdf-annot-inhibit-modification-hooks' in non-nil, this just
saves ANNOTATIONS and does not call the hooks until later, when
the variable is nil and this function is called again."
  
  (unless (memq operation '(nil :insert :change :delete))
    (error "Invalid operation: %s" operation))
  (when (and (null operation) annotations)
    (error "Missing operation argument"))
  
  (cl-symbol-macrolet ((plist pdf-annot-delayed-modified-annotations))
    (when operation
      (let ((list (plist-get plist operation)))
        (dolist (a annotations)
          (cl-pushnew a list :test 'pdf-annot-equal))
        (setq plist
              (plist-put plist
                         operation list))))
    (let ((changed (plist-get plist :change))
          (inserted (plist-get plist :insert))
          (deleted (plist-get plist :delete)))
      (unless (or pdf-annot-inhibit-modification-hooks
                  (cl-every 'null (list changed inserted deleted)))
        (unwind-protect
            (run-hook-with-args
             'pdf-annot-modified-functions
             (reverse inserted) (reverse changed) (reverse deleted))
          (setq pdf-annot-delayed-modified-annotations nil)))))))

(defmacro pdf-annot-with-atomic-modifications (&rest body)
  "Execute BODY joining multiple modifications.

The effect is, that `pdf-annot-modified-functions' will be called
only once at the end of BODY.

BODY should not modify annotations in a different then the
current buffer, because that won't run the hooks properly."
  (declare (indent 0) (debug t))
  (unwind-protect
      (save-current-buffer
        (let ((pdf-annot-inhibit-modification-hooks t))
          (progn ,@body)))
    (pdf-annot-run-pages-modified-hooks)))

(defun pdf-annot-property-writable-p (a prop)
  "Return non-nil, if A's property PROP may be modified."
  (or (memq prop '(edges color))
      (and (pdf-annot-markup-annotation-p a)
           (memq prop '(label contents popup-isopen)))
      (and (pdf-annot-text-annotation-p a)
           (memq prop '(isopen icon)))))

(defconst pdf-annot-markup-annotation-types
  '(text link free-text line square
         circle polygon poly-line highlight underline squiggly
         strike-out stamp caret ink file sound))

(defun pdf-annot-markup-annotation-p (a)
  (not (null
        (memq (pdf-annot-get a 'type)
              pdf-annot-markup-annotation-types))))

(defun pdf-annot-text-annotation-p (a)
  (eq 'text (pdf-annot-get a 'type)))

(defun pdf-annot-has-attachment-p (a)
  (eq 'file (pdf-annot-get a 'type)))

(defun pdf-annot-get-attachment (a)
  (unless (pdf-annot-has-attachment-p a)
    (error "Annotation has no data attached: %s" a))
  (pdf-annot-attach-get-from-annot a))




;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;
;;                           O L D   C O D E
;;                           
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(require 'pdf-info)
(require 'pdf-render)
(require 'pdf-misc)
(require 'facemenu) ;;for list-colors-duplicates
(require 'faces) ;;for color-values
(require 'org)   ;;org-with-gensyms, org-create-formula-image-with-dvipng
(require 'tablist)
(require 'cl-lib)

;;; Code:

(defun pdf-annot-image-position (a &optional image-size)
  "Return the position of annotation A in image coordinates.

IMAGE-SIZE should be a cons \(WIDTH . HEIGHT\) and defaults to
the page-image of the selected window."
  
  (unless image-size
    (pdf-util-assert-pdf-window)
    (setq image-size (pdf-view-image-size)))
  (let ((e (pdf-util-scale
            (pdf-annot-get a 'edges)
            image-size)))
    (pdf-util-with-edges (e)
      `(,e-x . ,e-y))))

(defun pdf-annot-image-set-position (a x y &optional image-size)
  "Set annotation A's position to X,Y in image coordinates.

See `pdf-annot-image-position' for IMAGE-SIZE."
  
  (unless image-size
    (pdf-util-assert-pdf-window)
    (setq image-size (pdf-view-image-size)))
  (let* ((edges (pdf-annot-get a 'edges))
         (x (/ x (float (car image-size))))
         (y (/ y (float (cdr image-size)))))
    (pdf-util-with-edges (edges)
      (let* ((w edges-width)
             (h edges-height)
             (x (max 0 (min x (- 1 w))))
             (y (max 0 (min y (- 1 h)))))
        (pdf-annot-put a 'edges
          (list x y (+ x w) (+ y h)))))
    (cons x y)))

(defun pdf-annot-image-size (a &optional image-size)
  "Return the size of annotation A in image coordinates.

Returns \(WIDTH . HEIGHT\).

See `pdf-annot-image-position' for IMAGE-SIZE."
  (unless image-size
    (setq image-size (pdf-view-image-size)))
  (let ((edges (pdf-util-scale-edges
                (pdf-annot-get a 'edges) image-size)))
    (pdf-util-with-edges (edges)
      (cons edges-width edges-height))))

(defun pdf-annot-image-set-size (a &optional width height image-size)
  "Set annotation A's size in image to WIDTH and/or HEIGHT.

See `pdf-annot-image-position' for IMAGE-SIZE."
  (unless image-size
    (setq image-size (pdf-view-image-size)))
  (let* ((edges (pdf-annot-get a 'edges))
         (w (and width
                 (/ width (float (car image-size)))))
         (h (and height
                 (/ height (float (cdr image-size))))))
    (pdf-util-with-edges (edges)
      (pdf-annot-put a 'edges
        (list edges-left
              edges-top
              (if w (+ edges-left w) edges-right)
              (if h (+ edges-top h) edges-bot))))))

(defvar pdf-annot-text-annot-defaults
  `((icon . "Note")
    (color . "#ffc0cb")
    (label . ,user-full-name)
    (isopen . nil))
  "An alist of default properties for new text annotations.")
  
;; Standard size for text annotations is 24x24 pixel.  Standard origin
;; is roughly at (6,24).  (For some value of standard, i.e. Adobe
;; Reader)
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
      (let* ((isize (pdf-view-image-size page))
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
           (pdf-annot-put a (car elt) (cdr elt))))))
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
                (edges . ,edges)
                (contents . "")))))
      (puthash page
               (append
                (pdf-annot-getannots page)
                (list a))
               pdf-annot-annotations)
      a)))

(defun pdf-annot-revert-document (&optional interactive)
  (interactive (list t))
  (pdf-util-assert-pdf-buffer)
  (when (or (null interactive)
            (pdf-annot-y-or-n-p 'revert-document
              "Abandon all modifications of all annotations in this buffer ?"))
    (setq pdf-annot-annotations nil)
    (pdf-annot-set-buffer-modified-p nil)
    (pdf-annot-run-pages-modified-hooks
     (mapcar (lambda (a) (pdf-annot-get a 'page))
             (pdf-annot-getannots)))))

(defun pdf-annot-revert-page (&optional page interactive)
  (interactive "P\np")
  (pdf-util-assert-pdf-buffer)
  (unless page
    ;;FIXME: doc-view-current-page requires a window, but this
    ;;function is used in a dozend places, check them all.  Maybe be
    ;;better to use pdf-info-number-of-pages and some variable ?
    (pdf-util-assert-pdf-window)
    (setq page (doc-view-current-page)))
  (when (or (null interactive)
            (pdf-annot-y-or-n-p 'revert-page
              (format "Abandon all modifications of the annotations on %s ?"
                      (if (= page (ignore-errors
                                    (doc-view-current-page)))
                          "the current page"
                        (format "page %d" page)))))
    (puthash page (mapcar (lambda (al)
                            (pdf-annot-new (current-buffer) al))
                          (pdf-info-getannots page))
             pdf-annot-annotations)
    (pdf-annot-run-pages-modified-hooks page)))

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
      (if (pdf-annot-has-attachment-p a)
          (pdf-annot-attach-print-tooltip
           (pdf-annot-get-attachment a))
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
                    (if (pdf-annot-text-annotation-p a)
                        '(label
                          icon
                          modified)
                      '(type modified)))
                   ";"))
          'face 'header-line)))
    (propertize header 'display header)))

(defun pdf-annot-print-tooltip-latex-maybe (a)
  (when (funcall pdf-annot-latex-string-predicate
                 (pdf-annot-get a 'contents))
    (pdf-annot-print-tooltip-latex a)))
  
(defcustom pdf-annot-latex-header
  (concat org-format-latex-header
          "\n\\setlength{\\textwidth}{12cm}")
  "Header used when latex compiling annotations.

The default value is `org-format-latex-header' + \
\"\\n\\\\setlength{\\\\textwidth}{12cm}\"."
  :group 'pdf-annot
  :type 'string)

(defun pdf-annot-print-tooltip-latex (a)
  (with-current-buffer (pdf-annot-buffer a)
    (let* ((header (pdf-annot-print-tooltip-header a))
           (contents (pdf-annot-get a 'contents))
           (pngfile (pdf-util-cache-make-filename
                     'pdf-annot-latex
                     "png"
                     header contents))
           (temporary-file-directory
            (pdf-util-cache-make-filename
             'pdf-annot-latex "latex"))
           (org-format-latex-header
            pdf-annot-latex-header))

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
           contents)))))))

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
           
(defun pdf-annot-hotspot-function (page size) 
  "The image hotspots function.

To be registered with `pdf-render-register-hotspot-function'."
  (let* ((annots (pdf-annot-getannots page '(text file)))
         (d pdf-annot-annotate-resize-pixel)
         (pointer '(hand hdrag vdrag arrow))
         (ops '(move resize-horizontally resize-vertically
                     resize-diagonally))
         hotspots)
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
                  hotspots)
            (local-set-key
             (vector (nth i ids) 'down-mouse-3)
             (lambda (_ev)
               (interactive "@e")
               (popup-menu (pdf-annot-create-context-menu a))))
            (local-set-key
             (vector (nth i ids) 'down-mouse-1)
             `(lambda (ev)
                (interactive "@e")
                (pdf-annot-move/resize-mouse
                 ev ',(nth i ops) ',a
                 ',(cl-case (pdf-annot-type a)
                     (text 'pdf-annot-edit-text)
                     (file 'pdf-annot-attach-find-file-other-window)))))
            (pdf-util-image-map-divert-mouse-clicks (nth i ids) '(2 4 5 6))))))
    hotspots))

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
         (size (pdf-view-image-size))
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
function with ANNOT as sole argument.

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
          (when (and click-fn
                     (eq button (event-basic-type ev)))
            (setq ev nil)
            (funcall click-fn annot))
        (unwind-protect
            (pdf-render-with-redraw
                'pdf-annot-render-function
              (pdf-annot-set-pointer-shape
               x-pointer-hand2)
              (plist-put (cdr (doc-view-current-image))
                         :pointer 'text)
              (let (make-pointer-invisible
                    pdf-render-hotspot-functions)
                (while (eq 'mouse-movement (event-basic-type ev))
                  (when (eq 'image (car-safe (posn-object (event-start ev))))
                    (let ((xy (posn-object-x-y (event-start ev))))
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
                         (- (cdr xy) (cdr offset))))))
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
    (pdf-annot-set-buffer-modified-p nil))
  (dolist (win (get-buffer-window-list))
    (quit-window do-kill win)))
        
(defun pdf-annot-edit-text-save-annotation ()
  (when (and pdf-annot-edit-annotation
             (buffer-modified-p))
    (pdf-annot-put pdf-annot-edit-annotation
        'contents
      (buffer-substring-no-properties (point-min) (point-max)))
    (pdf-annot-set-buffer-modified-p nil)))

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
        (pdf-annot-set-buffer-modified-p nil)
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
    (define-key km (kbd "C-c C-f") 'pdf-annot-list-follow-minor-mode)
    (define-key km (kbd "SPC") 'pdf-annot-display-annotation)
    km))

(defun pdf-annot-list-entries ()
  (unless (buffer-live-p pdf-annot-list-document-buffer)
    (error "No PDF document associated with this buffer"))
  (mapcar 'pdf-annot-list-create-entry
          (sort (cl-remove-if 'pdf-annot-deleted-p
                              (pdf-annot-getannots
                               nil 'text pdf-annot-list-document-buffer))
                'pdf-annot-compare)))

(defun pdf-annot-compare (a1 a2)
  (let ((p1 (pdf-annot-get a1 'page))
        (p2 (pdf-annot-get a2 'page)))
    (or (< p1 p2)
        (and (= p1 p2)
             (pdf-annot-compare-edges a1 a2)))))

(defun pdf-annot-compare-edges (a1 a2)
  (let ((e1 (pdf-util-scale-edges
             (pdf-annot-get a1 'edges)
             '(1000 . 1000)))
        (e2 (pdf-util-scale-edges
             (pdf-annot-get a2 'edges)
             '(1000 . 1000))))
    (pdf-util-with-edges (e1 e2)
      (or (< e1-top e2-top)
          (and (= e1-top e2-top)
               (<= e1-left e2-left))))))

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
  (unless (cl-remove-if 'pdf-annot-deleted-p (pdf-annot-getannots))
    (error "No annotations in this buffer"))
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
            tablist-operations-function 'pdf-annot-list-operation-function
            tablist-major-columns '(1 2 3 4 5))
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

(defun pdf-annot-list-goto-annotation (a)
  (with-current-buffer (pdf-annot-buffer a)
    (unless (and (buffer-live-p pdf-annot-list-buffer)
                 (get-buffer-window pdf-annot-list-buffer))
      (pdf-annot-list-annotations))
    (with-selected-window (get-buffer-window pdf-annot-list-buffer)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (eq a (tabulated-list-get-id))))
        (forward-line))
      (unless (eq a (tabulated-list-get-id))
        (error "Unable to find annotation"))
      (when (invisible-p (point))
        (tablist-suspend-filter t))
      (tablist-move-to-major-column))))
        
  
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

(defun pdf-annot-list-operation-function (op &rest args)
  (cl-ecase op
    (supported-operations '(delete edit-column find-entry complete))
    (edit-column
     (cl-destructuring-bind (id column item)
         args
       (let ((prop (nth column '(nil nil color icon nil label nil))))
         (unless prop
           (error "Invalid column (read-only)"))
         (when (eq prop 'color)
           (setq item (pdf-util-hexcolor item)))
         (pdf-annot-validate-property-value prop item)
         (pdf-annot-put id prop item)
         (cadr (pdf-annot-list-create-entry id)))))
    (delete
     (cl-destructuring-bind (annots)
         args
       (when (buffer-live-p pdf-annot-list-document-buffer)
         (with-current-buffer pdf-annot-list-document-buffer
           (pdf-annot-with-multiple-modifications
             (dolist (a annots)
               (pdf-annot-set-deleted-p a t)))))))
    (find-entry
     (cl-destructuring-bind (a)
         args
       (let ((buffer (pdf-annot-buffer a))
             edit-buffer)
         (with-selected-window (or (get-buffer-window buffer)
                                   (display-buffer buffer))
           (pdf-annot-edit-text a)
           (setq edit-buffer (current-buffer)))
         (let* ((window (get-buffer-window edit-buffer))
                (quit-restore (and window
                                   (window-parameter window 'quit-restore))))
           (when (and window quit-restore)
             (setcar (nthcdr 2 quit-restore) (selected-window)))
           (when window
             (select-window window))))))
    (complete
     (cl-destructuring-bind (_id column _string _pos)
         args
       (let ((prop (cl-case column
                     (2 'color)
                     (3 'icon))))
         (when prop
           (pdf-annot-property-completions prop)))))))

(defun pdf-annot-display-annotation (a)
  (interactive (list (tabulated-list-get-id)))
  (let ((buffer (pdf-annot-buffer a)))
    (with-selected-window (or (get-buffer-window buffer)
                              (display-buffer
                               buffer
                               '(nil (inhibit-same-window . t))))
      (when (pdf-util-page-displayed-p)
        (pdf-util-display-edges
         (pdf-util-scale-edges
          (pdf-annot-get a 'edges)
          (pdf-view-image-size t)))
        (pdf-annot-highlight a)))))

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
              'pdf-annot-display-annotation nil t)
    (let ((a (tabulated-list-get-id)))
      (when a
        (pdf-annot-display-annotation a))))
   (t
    (remove-hook 'tablist-selection-changed-functions
                 'pdf-annot-display-annotation t))))

;;   
;; *Utility functions
;; 

(defun pdf-annot-property-completions (prop)
  (cl-case prop
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
;; 

(defvar pdf-annot-minor-mode-map nil)

(defun pdf-annot-update-minor-mode-map ()
  (interactive)
  (let ((kmap (make-sparse-keymap))
        (smap (make-sparse-keymap)))
    (define-key kmap [remap doc-view-revert-buffer] 'doc-view-reconvert-doc)
    (with-no-warnings
      (define-key kmap (kbd pdf-annot-minor-mode-map-prefix) smap))
    (define-key kmap (kbd "C-c C-f") 'pdf-annot-attach-dired)
    (define-key smap (kbd "C-l") 'pdf-annot-list-annotations)
    (define-key smap (kbd "l") 'pdf-annot-list-annotations)
    (define-key smap (kbd "C-d") 'pdf-annot-toggle-display-annotations)
    (define-key smap (kbd "d") 'pdf-annot-toggle-display-annotations)
    (when (pdf-info-writable-annotations-p)
      (define-key smap (kbd "C-a") 'pdf-annot-add-text-annot)
      (define-key smap (kbd "a") 'pdf-annot-add-text-annot)
      (define-key smap (kbd "C-r") 'pdf-annot-revert-page)
      (define-key smap (kbd "r") 'pdf-annot-revert-page)
      (define-key smap (kbd "R") 'pdf-annot-revert-document))
    (setq-default pdf-annot-minor-mode-map kmap)
    (let ((elt (assq 'pdf-annot-minor-mode minor-mode-map-alist)))
      (when elt
        (setcdr elt pdf-annot-minor-mode-map)))))

(defcustom pdf-annot-minor-mode-map-prefix "C-c C-a"
  "The prefix to use for `pdf-annot-minor-mode-map'.

Use `pdf-annot-update-minor-mode-map' if yout set this variable
after this package was loaded."
  :group 'pdf-annot
  :set (lambda (sym value)
         (set-default sym value)
         (pdf-annot-update-minor-mode-map)))

;;;###autoload
(define-minor-mode pdf-annot-minor-mode
  "Support for PDF Annotations.

\\{pdf-annot-minor-mode-map}"
  nil nil nil
  (cond
   (pdf-annot-minor-mode
    (unless pdf-annot-minor-mode-map
      (pdf-annot-update-minor-mode-map))
    (when pdf-annot-tweak-tooltips
      (when (boundp 'x-gtk-use-system-tooltips)
        (setq x-gtk-use-system-tooltips nil))
      (setq tooltip-hide-delay 3600))
    (when (boundp 'doc-view-pdf->png-converter-function)
      (set (make-local-variable 'doc-view-pdf->png-converter-function)
           'doc-view-pdf->png-converter-ghostscript))
    (make-local-variable 'doc-view-ghostscript-options)
    (pdf-render-ghostscript-configure 0)
    (when (pdf-info-writable-annotations-p)
      (add-hook 'write-contents-functions 'pdf-annot-save-document nil t))
    (pdf-render-register-render-function 'pdf-annot-render-function 9)
    (pdf-render-register-hotspot-function 'pdf-annot-hotspot-function 9)
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
    (pdf-render-unregister-render-function 'pdf-annot-render-function)
    (pdf-render-unregister-hotspot-function 'pdf-annot-hotspot-function)
    (remove-hook 'pdf-annot-pages-modified-functions
                 'pdf-annot-redraw-pages t)
    (remove-hook 'pdf-annot-pages-modified-functions
                 'pdf-annot-reannotate-pages t)
    (remove-hook 'pdf-info-after-close-document-hook
                 'pdf-annot-revert-document t)
    (remove-hook 'pdf-util-after-reconvert-hook 'pdf-render-redraw-document t)))
  (pdf-render-redraw-document)
  (pdf-render-redisplay-current-page))

(defun pdf-annot-save-document ()
  (interactive)
  (when (buffer-modified-p)
    (pdf-info-assert-writable-annotations)
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
      (pdf-annot-set-buffer-modified-p nil)))
  t)

(defun pdf-annot-create-context-menu (a)
  (let ((menu (make-sparse-keymap)))
    (when (and (bound-and-true-p pdf-misc-menu-bar-minor-mode)
               (bound-and-true-p pdf-misc-install-popup-menu))
      (set-keymap-parent menu
                         (lookup-key pdf-misc-menu-bar-minor-mode-map
                                     [menu-bar pdf-tools]))
      (define-key menu [sep-99] menu-bar-separator))
    (when (pdf-info-writable-annotations-p)
      (define-key menu [delete-annotation]
        `(menu-item "Delete annotation"
                    ,(lambda ()
                       (interactive)
                       (when (pdf-annot-y-or-n-p 'delete)
                         (pdf-annot-set-deleted-p a t)
                         (message "Annotation deleted")))
                    :help
                    ,(substitute-command-keys
                      "Delete this annotation"))))
    
    (cl-case (pdf-annot-type a)
      (file
       (define-key menu [open-attachment]
         `(menu-item "Open attachment"
                     ,(lambda ()
                        (interactive)
                        (pdf-annot-attach-find-file-other-window a))
                     :help "Find this attachment in another window")))
      (t
       (define-key menu [goto-annotation]
         `(menu-item "List annotation"
                     ,(lambda ()
                        (interactive)
                        (pdf-annot-highlight a)
                        (pdf-annot-list-annotations)
                        (pdf-annot-list-goto-annotation a))
                     :help "Find annotation in the list buffer"))
       (when (pdf-info-writable-annotations-p)
         (define-key menu [edit-annotation]
           `(menu-item "Edit text"
                       ,(lambda ()
                          (interactive)
                          (pdf-annot-edit-text a))
                       :help "Edit annotations text contents")))))
    menu))
  
;;
;; *Attachments
;; 

(cl-defstruct (pdf-annot-attach
            (:constructor pdf-annot-attach-new (buffer page id properties))
            (:constructor nil))
  buffer
  id
  page
  properties)

(defun pdf-annot-attach-getattachments ()
  "Return the attachments of the current buffer.

The return value includes all attachments from annotations as
well as global, document-level ones."
  (let* ((annots (pdf-annot-getannots nil 'file))
         (docatt (pdf-info-getattachments nil))
         attachments)
    ;; Identify document level attachments by their order.
    ;; Identification is needed for later retrieval of the
    ;; attachment's data.
    (dotimes (i (length docatt))
      (push (pdf-annot-attach-new (current-buffer)
                            0 i (nth i docatt)) attachments))
    ;; Annotations have a unique key attached, no problem here.
    (dolist (a annots)
      (let ((pn (pdf-annot-get a 'page))
            (id (pdf-annot-get a 'id)))
        (push (pdf-annot-attach-new
               (current-buffer)
               pn id
               (pdf-info-getattachment-from-annot id))
              attachments)))
    attachments))

(defun pdf-annot-attach-get-from-annot (a)
  "Return annotation A's attached data.

A should be a file-annotation, otherwise the result is an error."
  (unless (eq (pdf-annot-get a 'type) 'file)
    (error "Annotation has no data attached: %s" a))
  (let* ((id (pdf-annot-get a 'id))
         (buffer (pdf-annot-buffer a))
         (page (pdf-annot-get a 'page))
         (alist (pdf-info-getattachment-from-annot id buffer)))
    (pdf-annot-attach-new buffer page id alist)))

(defun pdf-annot-attach-get (a prop &optional default)
  (or (cdr (assq prop (pdf-annot-attach-properties a)))
      default))

(defun pdf-annot-attach-name (a)
  "Return attachment A's specified filename or nil."
  (cdr (assq 'name (pdf-annot-attach-properties a))))

(defun pdf-annot-attach-description (a)
  "Return attachment A's description or nil."
  (cdr (assq 'description (pdf-annot-attach-properties a))))

(defun pdf-annot-attach-size (a)
  "Return attachment A's size or nil."
  (cdr (assq 'size (pdf-annot-attach-properties a))))

(defun pdf-annot-attach-mtime (a)
  "Return attachment A's modification time or nil."
  (cdr (assq 'mtime (pdf-annot-attach-properties a))))

(defun pdf-annot-attach-ctime (a)
  "Return attachment A's creation time or nil."
  (cdr (assq 'ctime (pdf-annot-attach-properties a))))

(defun pdf-annot-attach-checksum (a)
  "Return attachment A's checksum or nil."
  (cdr (assq 'checksum (pdf-annot-attach-properties a))))

(defun pdf-annot-attach-print-tooltip (a)
  "Return a string describing attachment A."
  (let ((header (propertize
                 (format "File attachment `%s' of %s\n"
                         (or (pdf-annot-attach-name a) "unnamed")
                         (if (pdf-annot-attach-size a)
                             (format "size %d" (file-size-human-readable
                                                (pdf-annot-attach-size a)))
                           "unknown size"))
                 'face 'header-line 'intangible t
                 'read-only t)))
    (concat
     (propertize
      (make-string (length header) ?\s)
      'display
      header)
     (or (pdf-annot-attach-description a) "No description"))))         
  
(defun pdf-annot-attach-from-annotation-p (a)
  "Return t, if attachment A belongs to some annotation.

There are two kinds of attachments,

i.  attachments associated with an annotation and
ii. attachment associated with the whole document.

This function returns t in case i. and nil in case ii. ."
  (symbolp (pdf-annot-attach-id a)))
  
(defun pdf-annot-attach-create-file (a)
  "Return a filename containing the data of attachment A.

The caller owns this file and should delete it, when it is no
longer needed."
  (cl-check-type a pdf-annot-attach)
  (let ((id (pdf-annot-attach-id a)))
    (cond
     ((numberp id)
      ;; This is not very elegant.
      (let ((attachments (pdf-info-getattachments
                          t (pdf-annot-attach-buffer a)))
            data)
        (dotimes (i (length attachments))
          (let ((file (cdr (assq 'file (nth i attachments)))))
            (if (= id i)
                (setq data file)
              (delete-file file))))
        data))
     (t
      (cdr (assq 'file (pdf-info-getattachment-from-annot
                        id t (pdf-annot-attach-buffer a))))))))  

(defun pdf-annot-attach-create-named-file (a &optional dir)
  "Copy attachment A's data to DIR.

This uses A's specified filename and creates directories below
DIR appropriately.  (This may not be entirely accurate,
e.g. directory parts which can't be, for what ever reasons,
created, are simply ignored.)  If DIR already contains a filename
of the same name (inclusive directories), return that name and
don't overwrite it.

DIR defaults to `pdf-annot-attach-default-directory'.  If A does not
specify a filename, the name is chosen randomly and put in DIR.

In any case, return the absolute filename of the created or found
file."

  (unless dir
    (setq dir (pdf-annot-attach-default-directory (pdf-annot-attach-buffer a)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (unless (file-directory-p dir)
    (error "Not a directory: %s" dir))
  (unless (file-writable-p dir)
    (error "Directory not writable: %s" dir))
  
  (let (datafile)
    (unwind-protect
        (let* ((name (pdf-annot-attach-name a))
               (default-directory (expand-file-name dir))
               newfile)

          (setq datafile (pdf-annot-attach-create-file a)
                newfile  (file-name-nondirectory datafile))
          (when name
            (let* ;; Name may be a unix or dos filename, I guess.
                ((parts (split-string name "[/\\]" t))
                 (dirs (butlast parts)))

              (setq newfile (car (last parts)))

              (dolist (d dirs)
                (condition-case nil
                    (progn
                      (unless (file-exists-p d)
                        (make-directory d))
                      (when (file-directory-p d)
                        (setq default-directory (expand-file-name d))))
                  (file-error)
                  (file-already-exists)))))

          (condition-case nil
              (copy-file datafile newfile)
            (file-already-exists))
          (expand-file-name newfile))
      (when (file-exists-p datafile)
        (delete-file datafile)))))

(defun pdf-annot-attach-find-file (a)
  (when (pdf-annot-p a)
    (setq a (pdf-annot-attach-get-from-annot a)))
  (find-file
   (pdf-annot-attach-create-named-file a)))

(defun pdf-annot-attach-find-file-other-window (a)
  (when (pdf-annot-p a)
    (setq a (pdf-annot-attach-get-from-annot a)))
  (pop-to-buffer
   (find-file-noselect
    (pdf-annot-attach-create-named-file a))))
  
(defun pdf-annot-attach-write-directory (&optional buffer directory)
  "Extract all attachments of BUFFER and put them in DIRECTORY.

BUFFER defaults to the current buffer and DIRECTORY to the
subdirectory `doc-view-current-cache-dir'/${PDF-NAME}_attachments.  

The attachments are written under their specified name, if
possible, but existing files are not overwritten.

If BUFFER has no attachments, do nothing and return nil,
otherwise return DIRECTORY."
  
  (pdf-util-assert-pdf-buffer buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (let ((dir (or (and directory
                        (expand-file-name directory))
                   (pdf-annot-attach-default-directory)))
          (attachments (pdf-annot-attach-getattachments)))
      
      (when attachments
        (unless (file-exists-p dir)
          (make-directory dir t))

        (unless (file-directory-p dir)
          (error "Not a directory: %s" dir))

        (dolist (a attachments)
          (pdf-annot-attach-create-named-file a dir))
        dir))))


(defun pdf-annot-attach-default-directory (&optional buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (expand-file-name
     (format "%s_attachments"
             (file-name-sans-extension (buffer-name)))
     (doc-view-current-cache-dir))))
  
(defun pdf-annot-attach-dired (&optional buffer)
  "Visit all attachments of the PDF of BUFFER in dired."
  (interactive)
  (pdf-util-assert-pdf-buffer buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (let ((dir (pdf-annot-attach-default-directory)))
      (unless (file-exists-p dir)
        (setq dir (pdf-annot-attach-write-directory nil dir)))
      (unless dir
        (error "Document has no data attached"))
      (dired-other-window dir))))

(provide 'pdf-annot)
;;; pdf-annot.el ends here

