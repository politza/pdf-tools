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




;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-annot nil
  "Annoatation support for PDF documents."
  :group 'pdf-tools)

(defcustom pdf-annot-activate-handler-functions nil
  "A list of functions to activate a annotation.

The functions on this hook will be called when some annotation is
activated, usually by a mouse-click.  Each one is called with the
annotation as a single argument and it should return a non-nil
value if it has `handled' it.  If no such function exists, the
default handler `pdf-annot-default-handler' will be
called.

This hook is ment to allow for custom annotations.  FIXME:
Implement and describe basic org example."
  :group 'pdf-annot
  :type 'hook)

(defcustom pdf-annot-default-text-annotation-properties
  `((icon . "Note")
    (color . "#ff0000")
    (label . ,user-full-name)
    (popup-is-open . nil))
  "Alist of initial properties for new text annotations.

FIXME: Describe. Restrict keys and values."
  :group 'pdf-annot)

(defcustom pdf-annot-default-markup-annotation-properties
  `((label . ,user-full-name)
    (popup-is-open . nil))
  "Alist of initial properties for new markup annotations.

FIXME: Describe. Restrict keys and values."
  :group 'pdf-annot)

(defcustom pdf-annot-preferred-markup-annotation-colors
  '("red" "lightgoldenrod2")
  "A list of preferred colors for markup annotations."
  :type '(repeat color)
  :group 'pdf-annot)

(defcustom pdf-annot-print-annotation-functions
  '(pdf-annot-print-annotation-latex-maybe)
  "A alist of functions for printing annotations, e.g. for the tooltip.

The functions receive the annotation as single argument and
should return either a string or nil.  The first string returned
will be used.

If all of them return nil, the default function
`pdf-annot-print-annotation-default' is used."
  :group 'pdf-annot
  :type 'hook)

(defcustom pdf-annot-latex-string-predicate
  (lambda (str)
    (and str (string-match "\\`[[:space:]\n]*[$\\]" str)))
  "A predicate for recognizing LaTeX fragments.

It receives a string and should return non-nil, if string is a
LaTeX fragment."
  :group 'pdf-annot
  :type 'function)

(defcustom pdf-annot-latex-header
  (concat org-format-latex-header
          "\n\\setlength{\\textwidth}{12cm}")
  "Header used when latex compiling annotations.

The default value is `org-format-latex-header' + \
\"\\n\\\\setlength{\\\\textwidth}{12cm}\"."
  :group 'pdf-annot
  :type 'string)

(defcustom pdf-annot-tweak-tooltips t
  "Whether this package should tweak some settings regarding tooltips.

If this variable has a non-nil value,

`x-gtk-use-system-tooltips' is set to nil if appropriate, in
order to display text properties;

`tooltip-hide-delay' is set to infinity, in order to not beeing
annoyed while reading the annotations."
  :group 'pdf-annot)


(defcustom pdf-annot-modified-functions nil
  "Functions to call, when an annotation was modified.

A function on this hook should accept one argument: A CLOSURE
containing inserted, changed and deleted annotations.

It may access theses annotations by calling CLOSURE with one of
these arguments:

`:inserted' The list of recently added annotations.

`:deleted' The list of recently deleted annotations.

`:changed' The list of recently changed annotations.

`t' The union of recently added, deleted or changed annotations.

`nil' Just returns nil.

Any other argument signals an error."
  :group 'pdf-annot
  :type 'hook)

(defcustom pdf-annot-activate-created-annotations nil
  "Whether to activate (i.e. edit) created annotations."
  :group 'pdf-annot
  :type 'boolean)


;; * ================================================================== *
;; * Variables and Macros
;; * ================================================================== *

(defconst pdf-annot-rendered-annotation-size '(24 . 24)
  "The Size of text and file annotations in PDF points.

These values are hard-coded in poppler.  And while the size of
these annotations may be changed, i.e. the edges property, it has
no effect on the rendering.")

(defconst pdf-annot-annotation-types
  '(unknown text link free-text line square
            circle polygon poly-line highlight underline squiggly
            strike-out stamp caret ink popup file sound movie widget screen
            printer-mark trap-net watermark 3d )
  "Complete list of annotation types.")
  
(defconst pdf-annot-markup-annotation-types
  '(text link free-text line square
         circle polygon poly-line highlight underline squiggly
         strike-out stamp caret ink file sound)
  "List of defined markup annotation types.")

(defconst pdf-annot-standard-text-icons
  '("Note" "Comment" "Key" "Help" "NewParagraph"
    "Paragraph" "Insert" "Cross" "Circle")
  "A list of standard icon properties for text annotations.")

(defvar pdf-annot-inhibit-modification-hooks nil
  "Non-nil, if running `pdf-annot-modified-functions' should be
  inhibited after some annotation has changed.")

(defvar-local pdf-annot-delayed-modified-annotations nil
  "A plist of not yet propagated modifications.

It contains three entries :change, :delete and :insert.  Each one
having a list of annotations as value.")



;; * ================================================================== *
;; * Minor mode
;; * ================================================================== *

(defcustom pdf-annot-minor-mode-map-prefix (kbd "C-c C-a")
  "The prefix to use for `pdf-annot-minor-mode-map'.

Setting this after the package was loaded has no effect."
  :group 'pdf-annot
  :type 'key-sequence)

(defvar pdf-annot-minor-mode-map 
  (let ((kmap (make-sparse-keymap))
        (smap (make-sparse-keymap)))
    (define-key kmap pdf-annot-minor-mode-map-prefix smap)
    (define-key smap "l" 'pdf-annot-list-annotations)
    (define-key smap "d" 'pdf-annot-toggle-display-annotations)
    (when (pdf-info-writable-annotations-p)
      (define-key smap "t" 'pdf-annot-add-text-annotation)
      (when (pdf-info-markup-annotations-p)
        (define-key smap "m" 'pdf-annot-add-markup-annotation)))
    kmap)
  "Keymap used for `pdf-annot-minor-mode'.")

;;;###autoload
(define-minor-mode pdf-annot-minor-mode
  "Support for PDF Annotations.

\\{pdf-annot-minor-mode-map}"
  nil nil nil
  (cond
   (pdf-annot-minor-mode
    (when pdf-annot-tweak-tooltips
      (when (boundp 'x-gtk-use-system-tooltips)
        (setq x-gtk-use-system-tooltips nil))
      (setq tooltip-hide-delay 3600))
    (pdf-view-add-hotspot-function 'pdf-annot-hotspot-function 9))
   (t
    (pdf-view-remove-hotspot-function 'pdf-annot-hotspot-function)))
  (pdf-view-redisplay t))

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
                        (pdf-annot-display-selected a)
                        (pdf-annot-list-annotations)
                        (pdf-annot-list-goto-annotation a))
                     :help "List annotations in a buffer"))
       (when (pdf-info-writable-annotations-p)
         (define-key menu [edit-annotation]
           `(menu-item "Edit text"
                       ,(lambda ()
                          (interactive)
                          (pdf-annot-edit-contents a))
                       :help "Edit annotations text contents")))))
    menu))


;; * ================================================================== *
;; * Annotation Basics
;; * ================================================================== *

(defun pdf-annot-create (alist &optional buffer)
  "Create a annotation from ALIST in BUFFER.

ALIST should be a property list as returned by
e.g. `pdf-info-getannots'.  BUFFER shoudl be the buffer of the
corresponding PDF document and defaults to the current buffer."
 
  (cons `(pdf-annot-buffer . ,(or buffer (current-buffer)))
        alist))
  
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
                      (and (or (null types)
                               (memq (cdr (assq 'type a)) types))
                           (pdf-annot-create a buffer)))
                    (pdf-info-getannots pages buffer))))

(defun pdf-annot-get (a property &optional default)
  "Get annotation A's value of PROPERTY.

Return DEFAULT, if value is nil."
  (or (cdr (assq property a)) default))

(defun pdf-annot-put (a property value)
  "Set annotation A's PROPERTY to VALUE.

Unless VALUE is `equal' to the current value, sets A's buffer's
modified flag and runs the hook `pdf-annot-modified-functions'.

Signals an error, if PROPERTY is not modifiable.

Returns the modified annotation."
  
  (declare (indent 2))
  (unless (equal value (pdf-annot-get a property))
    (unless (pdf-annot-property-modifiable-p a property)
      (error "Property `%s' is read-only for this annotation"))
    (with-current-buffer (pdf-annot-get-buffer a)
      (setq a (pdf-annot-create
               (pdf-info-editannot
                (pdf-annot-get a 'id)
                `((,property . ,value)))))
      (set-buffer-modified-p t)
      (pdf-annot-run-modified-hooks :change a)
      (pdf-view-redisplay-pages (pdf-annot-get a 'page))))
  a)

(defun pdf-annot-equal (a1 a2)
  "Return non-nil, if annotations A1 and A2 are equal.

Two annotations are equal, if they belong to the same buffer and
have identical id properties."
  (and (eq (pdf-annot-get-buffer a1)
           (pdf-annot-get-buffer a2))
       (eq (pdf-annot-get a1 'id)
           (pdf-annot-get a2 'id))))
  
(defun pdf-annot-get-buffer (a)
  "Return annotation A's buffer."
  (pdf-annot-get a 'pdf-annot-buffer))

(defun pdf-annot-get-id (a)
  "Return id property of A."
  (pdf-annot-get a 'id))

(defun pdf-annot-delete (a)
  "Delete annotation A.

Sets A's buffer's modified flag and runs the hook
`pdf-annot-modified-functions'.

This function alwasy returns nil."
  (with-current-buffer (pdf-annot-get-buffer a)
    (pdf-info-delannot
     (pdf-annot-get a 'id))
    (set-buffer-modified-p t)
    (pdf-annot-run-modified-hooks :delete a))
  nil)

(defun pdf-annot-run-modified-hooks (&optional operation &rest annotations)
  "Run `pdf-annot-modified-functions' using OPERATION on ANNOTATIONS.

OPERATION should be one of nil, :change, :insert or :delete.  If
nil, annotations should be empty.

If `pdf-annot-inhibit-modification-hooks' in non-nil, this just
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
    (let* ((changed (plist-get plist :change))
           (inserted (mapcar (lambda (a)
                               (let ((ac (cl-member a changed :test 'pdf-annot-equal)))
                                 (or ac a)))
                             (plist-get plist :insert)))
           (deleted (plist-get plist :delete))
           (union (cl-union (cl-union changed inserted :test 'pdf-annot-equal)
                            deleted :test 'pdf-annot-equal))
           (closure (lambda (arg)
                      (cl-ecase arg
                        (:inserted (copy-sequence inserted))
                        (:changed (copy-sequence changed))
                        (:deleted (copy-sequence deleted))
                        (t (copy-sequence union))
                        (nil nil)))))
      (unless (or pdf-annot-inhibit-modification-hooks
                  (null union))
        (unwind-protect
            (run-hook-with-args
             'pdf-annot-modified-functions closure)
          (setq pdf-annot-delayed-modified-annotations nil))))))

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

(defun pdf-annot-text-annotation-p (a)
  (eq 'text (pdf-annot-get a 'type)))

(defun pdf-annot-markup-annotation-p (a)
  (not (null
        (memq (pdf-annot-get a 'type)
              pdf-annot-markup-annotation-types))))

(defun pdf-annot-property-modifiable-p (a property)
  (or (memq property '(edges color flags contents))
      (and (pdf-annot-markup-annotation-p a)
           (memq property '(label opacity popup popup-is-open)))
      (and (pdf-annot-text-annotation-p a)
           (memq property '(icon is-open)))))



;; * ================================================================== *
;; * Handling attachments
;; * ================================================================== *

(defun pdf-annot-has-attachment-p (a)
  (eq 'file (pdf-annot-get a 'type)))

(defun pdf-annot-get-attachment (a &optional do-save)
  (unless (pdf-annot-has-attachment-p a)
    (error "Annotation has no data attached: %s" a))
  (pdf-info-getattachment-from-annot
   (pdf-annot-get-id a) do-save))

(defun pdf-annot-display-attachment (a &optional buffer-name
                                       display-action no-select-window-p)
  "Display file annotation A's data in a buffer."
  (let ((att (pdf-annot-get-attachment a t)))
    (unwind-protect
        (let* ((name (or buffer-name
                         (format
                          "*Attachment (p. %d) <%s>*"
                          (pdf-annot-get a 'page)
                          (or (cdr (assq 'name att))
                              "No name"))))
               (buffer (get-buffer-create name)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer))
            (insert-file-contents (cdr (assq 'file att)))
            (let ((buffer-file-name
                   (cdr (assq 'name att))))
              (normal-mode))
            (let ((window (display-buffer (current-buffer) display-action)))
              (unless no-select-window-p
                (select-window window)))))
      (when (and att (file-exists-p (cdr (assq 'file att))))
        (delete-file (cdr (assq 'file att)))))))

(defun pdf-annot-activate-annotation (a)
  (or (run-hook-with-args-until-success
       'pdf-annot-activate-handler-functions
       a)
      (pdf-annot-default-activate-handler a)))

(defun pdf-annot-default-activate-handler (a)
  (cond
   ((pdf-annot-has-attachment-p a)
    (pdf-attach-display-attachment a))
   (t (pdf-annot-edit-contents a))))
    

;; * ================================================================== *
;; * Interfacing with the display
;; * ================================================================== *

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
      `(,e-left . ,e-top))))

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
          (list x y -1 -1))))))

(defun pdf-annot-image-size (a &optional image-size)
  "Return the size of annotation A in image coordinates.

Returns \(WIDTH . HEIGHT\).

See `pdf-annot-image-position' for IMAGE-SIZE."
  (unless image-size
    (pdf-util-assert-pdf-window)
    (setq image-size (pdf-view-image-size)))
  (let ((edges (pdf-util-scale-edges
                (pdf-annot-get a 'edges) image-size)))
    (pdf-util-with-edges (edges)
      (cons edges-width edges-height))))

(defun pdf-annot-image-set-size (a &optional width height image-size)
  "Set annotation A's size in image to WIDTH and/or HEIGHT.

See `pdf-annot-image-position' for IMAGE-SIZE."
  (unless image-size
    (pdf-util-assert-pdf-window)
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

(defun pdf-annot-at-position (pos)
  "Return annotation at POS in the selected window.

POS should be an absolute image position as a cons \(X . Y\).
Return nil, if no annotation was found."
  (let* ((annots (pdf-annot-getannots (pdf-view-current-page)))
         (size (pdf-view-image-size))
         (offset (pdf-view-image-offset))
         (rx (+ (/ (car pos) (float (car size)))
                (car offset)))
         (ry (+ (/ (cdr pos) (float (cdr size)))
                (cdr offset)))
         (rpos (cons rx ry)))
    (cl-some (lambda (a)
               (and (pdf-util-edges-inside-p
                     (pdf-annot-get a 'edges)
                     rpos)
                    a))
             annots)))

;; FIXME: Remove resize code in the near future, since popplers
;; rendering engine does not support it (annotations are rendered with
;; a fixed size).
(defun pdf-annot-mouse-move/resize (event &optional operation annot)
  "Start moving/resizing annotation at EVENT's position.

EVENT should be a mouse event originating the request and is used
as a reference for OPERATION.

OPERATION should be one of resize-horizontally,
resize-vertically, resize (which means in both directions) or
move, which is also the default.

ANNOT is the annotation to operate on and defaults to the
annotation at EVENT's start position.

This function does not return until the operation is completed,
i.e. a non mouse-movement event is read."

  (interactive "@e")
  (pdf-util-assert-pdf-window (posn-window (event-start event)))
  (select-window (posn-window (event-start event)))
  (let* ((mpos (posn-object-x-y (event-start event)))
         (buffer (current-buffer))
         (a (or annot
                (pdf-annot-at-position mpos))))
    (unless a
      (error "No annotation at this position: %s" mpos))
    (let* ((pdf-annot-inhibit-modification-hooks t)
           (pdf-cache-image-inihibit t)
           (apos (pdf-annot-image-position a))
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
           (window (selected-window))
           make-pointer-invisible)
      (when (pdf-util-track-mouse-dragging (ev 0.1)
                (mouse-movement-p ev)
              (when (and (eq window (selected-window))
                         (eq 'image (car-safe (posn-object (event-start ev)))))
                (let ((pdf-view-inhibit-hotspots t)
                      (xy (posn-object-x-y (event-start ev))))
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
                     (- (cdr xy) (cdr offset))))))))
        (pdf-view-redisplay)
        (with-current-buffer buffer
          (pdf-annot-run-modified-hooks))))
    nil))
              
(defun pdf-annot-hotspot-function (page size)
  "Create image hotspots for page PAGE of size SIZE."
  (let ((types (remq 'link pdf-annot-markup-annotation-types)))
    (apply 'nconc (mapcar (lambda (a)
                            (pdf-annot-create-hotspots a size))
                          (pdf-annot-getannots page types)))))
  
(defun pdf-annot-create-hotspots (a size) 
  "Return a list of image hotspots for annotation A."
  (let ((id (pdf-annot-get a 'id))
        (edges (pdf-util-scale
                (or (pdf-annot-get a 'markup-edges)
                    (list (pdf-annot-get a 'edges)))
                size 'round))
        (moveable-p (memq (pdf-annot-get a 'type)
                          '(file text)))
        hotspots)
    (dolist (e edges)
      (pdf-util-with-edges (e)
        (push `((rect . ((,e-left . ,e-top) . (,e-right . ,e-bot)))
                ,id
                (pointer
                 hand
                 help-echo
                 ,(pdf-annot-print-annotation a)))
              hotspots)))
    (pdf-annot-create-hotspot-binding id (if moveable-p 'move) a)
    hotspots))
      
(defun pdf-annot-create-hotspot-binding (id operation annotation)
  ;; Activating
  (local-set-key
   (vector id 'mouse-1)
   (lambda (ev)
     (interactive "@e")
     (pdf-annot-activate-annotation annotation)))
  ;; Move/Resize
  (when operation
    (local-set-key
     (vector id 'down-mouse-1)
     (lambda (ev)
       (interactive "@e")
       (pdf-annot-mouse-move/resize
        ev operation annotation))))
  ;; Context Menu
  (local-set-key
   (vector id 'down-mouse-3)
   (lambda ()
     (interactive "@")
     (popup-menu (pdf-annot-create-context-menu annotation))))
  ;; Everything else
  (local-set-key
   (vector id t)
   'pdf-util-image-map-mouse-event-proxy))

(defun pdf-annot-display-selected (a &optional window)
  "Visually distinguish annotation A in window WINDOW.

Turns to A's page, if nescessary."
  (save-selected-window
    (when window (select-window window))
    (pdf-util-assert-pdf-window)
    (let ((color "black")
          (alpha 0.5)
          (page (pdf-annot-get a 'page))
          (width (car (pdf-view-image-size))))
      (unless (= page (pdf-view-current-page))
        (pdf-view-goto-page page))
      (let ((edges (or (pdf-annot-get a 'markup-edges)
                       (list (pdf-annot-get a 'edges)))))
        (pdf-view-display-image
         (pdf-view-create-image
             (pdf-cache-renderpage-regions
              page width
              `(,color ,alpha ,@edges))))
        (pdf-util-scroll-to-edges
         (pdf-util-scale-relative-to-pixel (car edges)))))))


;; * ================================================================== *
;; * Creating annotations
;; * ================================================================== *

(defun pdf-annot-add-annotation (type edges &optional property-alist page window)
  "Creates and adds a new annotation of type TYPE to the document."

  (unless (memq type (pdf-info-creatable-annotation-types))
    (error "Unsupported annotation type: %s" type))
  (pdf-util-assert-pdf-window window)
  (unless page (setq page (pdf-view-current-page window)))
  (unless (consp (car-safe edges))
    (setq edges (list edges)))
  (when (and (eq type 'text)
             (> (length edges) 1))
    (error "Edges argument should be a single edge-list for text annotations")) 
  (let* ((scaled (pdf-util-scale-pixel-to-relative edges nil nil window))
         (raw (apply 'pdf-info-addannot
                     page
                     (if (eq type 'text)
                         (car scaled)
                       '(0 0 0 0))
                     type
                     nil
                     (if (not (eq type 'text)) scaled)))
         (id (cdr (assq 'id raw))))
    (when property-alist
      (condition-case err
          (setq raw (pdf-info-editannot id property-alist))
        (error
         (pdf-info-delannot id)
         (signal (car err) (cdr err)))))
    (let ((a (pdf-annot-create raw)))
      (set-buffer-modified-p t)
      (pdf-annot-run-modified-hooks :insert a)
      (pdf-view-redisplay-pages page)
      (when pdf-annot-activate-created-annotations
        (pdf-annot-activate-annotation a))
      a)))

(defun pdf-annot-add-text-annotation (x y &optional icon property-alist page window)
  (interactive
   (let* ((default-icon (or (cdr (assq 'icon pdf-annot-default-text-annotation-properties))
                            "Note"))
          (icon (completing-read
                 (format "Icon (default %s): " default-icon)
                 pdf-annot-standard-text-icons nil t nil nil default-icon))
          (posn (pdf-util-read-image-position
                 "Click where a new text annotation should be added ..."))
          (window (posn-window posn)))
     (list (car (posn-object-x-y posn))
           (cdr (posn-object-x-y posn))
           (if (equal icon "") nil icon)
           nil nil (unless (eq window (selected-window))))))
  (let ((isize (pdf-view-image-size window)))
    (unless (and (>= x 0)
                 (< x (car isize)))
      (signal 'args-out-of-range (list x)))
    (unless (and (>= y 0)
                 (< y (cdr isize)))
      (signal 'args-out-of-range (list y)))
    (let ((size (pdf-util-scale-points-to-pixel
                 pdf-annot-default-text-annotation-size 'round)))
      (setcar size (min (car size) (car isize)))
      (setcdr size (min (cdr size) (cdr isize)))
      (cl-decf x (max 0 (- (+ x (car size)) (car isize))))
      (cl-decf y (max 0 (- (+ y (cdr size)) (cdr isize))))
      (pdf-annot-add-annotation
       'text (list x y -1 -1)
       (pdf-annot-merge-alists
        (and icon `((icon . ,icon)))
        property-alist
        pdf-annot-default-text-annotation-properties)
       page window))))

(defun pdf-annot-add-markup-annotation (edges type &optional color
                                              property-alist page window)
  (interactive
   (progn
     (pdf-info-assert-markup-annotations)
     (list
      (prog1
          (pdf-view-active-region)
        (deactivate-mark))
      (let ((type (completing-read "Markup type (default highlight): "
                                   '(squiggly highlight underline strikeout)
                                   nil t)))
        (if (equal type "") 'highlight (intern type)))
      (pdf-annot-read-markup-color))))
  (pdf-annot-add-annotation
   type
   edges
   (pdf-annot-merge-alists
    (and color `((color . ,color)))
    property-alist
    pdf-annot-default-markup-annotation-properties)
   page window))

(defun pdf-annot-read-markup-color (&optional prompt)
  (let* ((defaults (delq nil
                         (cons (cdr (assq 'color
                                          pdf-annot-default-markup-annotation-properties))
                               pdf-annot-preferred-markup-annotation-colors)))
         (prompt
          (format "%s%s: "
                  (or prompt "Color")
                  (if defaults (format " (default %s)" (car defaults)) "")))
         (current-completing-read-function completing-read-function)
         (completing-read-function
          (lambda (prompt collection &optional predicate require-match
                          initial-input hist def inherit-input-method)
            (funcall current-completing-read-function
                     prompt collection predicate require-match
                     initial-input hist
                     defaults
                     inherit-input-method))))
    (read-color prompt)))
  
    
(defun pdf-annot-merge-alists (&rest alists)
  (let (merged)
    (dolist (elt (apply 'append alists))
      (unless (assq (car elt) merged)
        (push elt merged)))
    (nreverse merged)))
  
  

;; * ================================================================== *
;; * Displaying annotation contents
;; * ================================================================== *

(defun pdf-annot-print-property (a property)
  "Pretty print annotation A's property PROPERTY."
  (let ((value (pdf-annot-get a property)))
    (cl-case property
      (color
       (propertize (or value "")
                   'face (and value
                              `(:background ,color))))
      ((created modified)
       (let ((date value))
         (if (null date)
             "Unknown date"
           (current-time-string date))))
      ;; print verbatim
      (opacity
       (let ((opacity (or value 1.0)))
         (format "%d%%" (round (* 100 opacity)))))
      (t (format "%s" value)))))

(defun pdf-annot-print-annotation (a)
  "Pretty print annotation A."
  (or (run-hook-with-args-until-success
       'pdf-annot-print-annotation-functions a)
      (pdf-annot-print-annotation-default a)))

(defun pdf-annot-print-annotation-default (a)
  "Default pretty printer for annotation A.

The result consists of a header (as printed with
`pdf-annot-print-annotation-header') a newline and A's contents
property."
  (concat
   (pdf-annot-print-annotation-header a)
   "\n"
   (pdf-annot-get a 'contents)))

(defun pdf-annot-print-annotation-header (a)
  "Emit a suitable header string for annotation A."
  (let ((header
         (cond
          ((eq 'file (pdf-annot-get a 'type))
           (let ((att (pdf-annot-get-attachment a)))
             (format "File attachment `%s' of %s"
                     (or (cdr (assq 'name att)) "unnamed")
                     (if (cdr (assq 'size att))
                         (format "size %d" (file-size-human-readable
                                            (cdr (assq 'size att))))
                       "unknown size"))))
          (t
           (format "[%s]"
                   (mapconcat
                    'identity
                    (mapcar
                     (lambda (property)
                       (pdf-annot-print-property
                        a property))
                     (remq nil
                           `(,(unless (pdf-annot-text-annotation-p a)
                                'type)
                             label
                             ,(when (pdf-annot-text-annotation-p a)
                                'icon)
                             subject
                             modified)))
                    ";"))))))
    (setq header (propertize header 'face 'header-line
                             'intangible t 'read-only t))
    ;; This `trick' makes the face apply in a tooltip.
    (propertize header 'display header)))

(defun pdf-annot-print-annotation-latex-maybe (a)
  "Maybe print annotation A's content as a LaTeX fragment.

See `pdf-annot-latex-string-predicate'."
  (when (and (functionp pdf-annot-latex-string-predicate)
             (funcall pdf-annot-latex-string-predicate
                      (pdf-annot-get a 'contents)))
    (pdf-annot-print-annotation-latex a)))

(defun pdf-annot-print-annotation-latex (a)
  "Print annotation A's content as a LaTeX fragment.

This compiles A's contents as a LaTeX fragment and puts the
resulting image as a display property on the contents, prefixed
by a header."

  (let (tempfile)
    (unwind-protect
        (with-current-buffer (pdf-annot-get-buffer a)
          (let* ((page (pdf-annot-get a 'page))
                 (header (pdf-annot-print-annotation-header a))
                 (contents (pdf-annot-get a 'contents))
                 (hash (sxhash (format
                                "pdf-annot-print-annotation-latex%s%s%s"
                                page header contents)))
                 (data (pdf-cache-lookup-image page 0 nil hash))
                 (org-format-latex-header
                  pdf-annot-latex-header)
                 (temporary-file-directory
                  (pdf-util-expand-file-name "pdf-annot")))
            (unless (file-directory-p temporary-file-directory)
              (make-directory temporary-file-directory))
            (unless data
              (setq tempfile (make-temp-file "pdf-annot" nil ".png"))
              (org-create-formula-image
               contents tempfile org-format-latex-options t)
              (setq data (pdf-util-munch-file tempfile))
              (if (and (> (length data) 3)
                       (equal (substring data 1 4)
                              "PNG"))
                  (pdf-cache-put-image page 0 data hash)
                (setq data nil)))
            (concat
             header
             "\n"
             (if data
                 (propertize
                  contents 'display (pdf-view-create-image data))
               (propertize
                contents
                'display
                (concat
                 (propertize "Failed to compile latex fragment\n"
                             'face 'error)
                 contents))))))
      (when (and tempfile
                 (file-exists-p tempfile))
        (delete-file tempfile)))))





;; * ================================================================== *
;; * O L D   C O D E
;; * ================================================================== *

(require 'pdf-info)
(require 'pdf-render)
(require 'pdf-misc)
(require 'facemenu) ;;for list-colors-duplicates
(require 'faces) ;;for color-values
(require 'org)   ;;org-with-gensyms, org-create-formula-image-with-dvipng
(require 'tablist)
(require 'cl-lib)

(defun pdf-annot-revert-document (&optional interactive)
  (interactive (list t))
  (pdf-util-assert-pdf-buffer)
  (when (or (null interactive)
            (pdf-annot-y-or-n-p 'revert-document
              "Abandon all modifications of all annotations in this buffer ?"))
    (setq pdf-annot-annotations nil)
    (set-buffer-modified-p nil)
    (pdf-annot-run-pages-modified-hooks
     (mapcar (lambda (a) (pdf-annot-get a 'page))
             (pdf-annot-getannots)))))


;; * ================================================================== *
;; * Editing annotation contents
;; * ================================================================== *

(defvar-local pdf-annot-edit-contents--annotation nil)
(put 'pdf-annot-edit-contents--annotation 'permanent-local t)
(defvar-local pdf-annot-edit-contents--buffer nil)

(defcustom pdf-annot-edit-contents-choose-mode-function
  (lambda (a)
    (if (funcall pdf-annot-latex-string-predicate
                 (pdf-annot-get a 'contents))
        (latex-mode)
      (text-mode)))  
  "FIXME: Not documented."
  :group 'pdf-annot
  :type 'function)

(defcustom pdf-annot-edit-contents-display-buffer-action
  '((display-buffer-reuse-window
     display-buffer-split-below-and-attach)
    (inhibit-same-window . t)
    (window-height . 0.25))
  "Display action when showing the edit buffer."
  :group 'pdf-annot
  :type display-buffer--action-custom-type)

(defvar pdf-annot-edit-contents-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap text-mode-map)
    (define-key kmap (kbd "C-c C-c") 'pdf-annot-edit-contents-commit)
    (define-key kmap (kbd "C-c C-q") 'pdf-annot-edit-contents-abort)
    kmap))

(define-minor-mode pdf-annot-edit-contents-minor-mode
  "Active when editing the contents of annotations."
  nil nil nil
  (when pdf-annot-edit-contents-minor-mode
    (message "%s"
             (substitute-command-keys
              "Press \\[pdf-annot-edit-contents-commit] to commit your changes, \\[pdf-annot-edit-contents-abort] to abandon them."))))

(put 'pdf-annot-edit-contents-minor-mode 'permanent-local t)

(defun pdf-annot-edit-contents-finalize (do-save &optional do-kill)
  (when (buffer-modified-p)
    (cond
     ((eq do-save 'ask)
      (save-window-excursion
        (display-buffer (current-buffer) nil (selected-frame))
        (when (y-or-n-p "Save changes to this annotation ?")
          (pdf-annot-edit-contents-save-annotation))))
     (do-save
      (pdf-annot-edit-contents-save-annotation)))
    (set-buffer-modified-p nil))
  (dolist (win (get-buffer-window-list))
    (quit-window do-kill win)))
        
(defun pdf-annot-edit-contents-save-annotation ()
  (when pdf-annot-edit-contents--annotation
    (pdf-annot-put pdf-annot-edit-contents--annotation
        'contents
      (buffer-substring-no-properties (point-min) (point-max)))
    (set-buffer-modified-p nil)))

(defun pdf-annot-edit-contents-commit ()
  (interactive)
  (pdf-annot-edit-contents-finalize t))

(defun pdf-annot-edit-contents-abort ()
  (interactive)
  (pdf-annot-edit-contents-finalize nil t))

(defun pdf-annot-edit-contents-noselect (a)
  (with-current-buffer (pdf-annot-get-buffer a)
    (when (and (buffer-live-p pdf-annot-edit-contents--buffer)
               (not (eq a pdf-annot-edit-contents--annotation)))
      (with-current-buffer pdf-annot-edit-contents--buffer
        (pdf-annot-edit-contents-finalize 'ask)))
    (unless (buffer-live-p pdf-annot-edit-contents--buffer)
      (setq pdf-annot-edit-contents--buffer
            (with-current-buffer (get-buffer-create
                                  (format "*Edit Annotation %s*"
                                          (buffer-name)))
              (pdf-annot-edit-contents-minor-mode 1)
              (current-buffer))))
    (with-current-buffer pdf-annot-edit-contents--buffer
      (setq pdf-annot-edit-contents--annotation a)
      (funcall pdf-annot-edit-contents-choose-mode-function a)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion (insert (pdf-annot-get a 'contents)))
        (set-buffer-modified-p nil)
        (current-buffer)))))

(defun pdf-annot-edit-contents (a)
  (select-window
   (display-buffer
    (pdf-annot-edit-contents-noselect a)
    pdf-annot-edit-contents-display-buffer-action)))
  
(defun pdf-annot-edit-contents-mouse (ev)
  (interactive "@e")
  (let* ((pos (posn-object-x-y (event-start ev)))
         (a (and pos (pdf-annot-at-position pos))))
    (unless a
      (error "No annotation at this position"))
    (pdf-annot-edit-contents a)))




;; 

(provide 'pdf-annot)
;;; pdf-annot.el ends here
