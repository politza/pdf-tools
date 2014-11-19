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

(defcustom pdf-annot-handler-functions nil
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

(defcustom pdf-annot-initial-annotation-properties
  `((text . ((icon . "Note")
             (color . "#ffc0cb")
             (label . ,user-full-name)
             (popup-isopen . nil))))
  "An alist of initial properties for new annotations.

FIXME: Describe. Restrict keys and values."
  :group 'pdf-annot)

(defcustom pdf-annot-print-property-functions nil
  "A list of functions for pretty printing annotation properties.

The functions receive the annotation and a property as arguments
and should return either a string or nil.  The first string
returned will be used.

If all of them return nil, the default function
`pdf-annot-print-property-default' is used."
  :group 'pdf-annot
  :type 'hook)

(defcustom pdf-annot-print-tooltip-functions
  '(pdf-annot-print-tooltip-latex)
  "A alist of functions for printing annotations.

The functions receive the annotation as single argument and
should return either a string or nil.  The first string returned
will be used as tooltip.

If all of them return nil, the default function
`pdf-annot-print-tooltip-default' is used."
  :group 'pdf-annot
  :type 'hook)

(defcustom pdf-annot-latex-string-predicate
  (lambda (str)
    (string-match "\\`[[:space:]\n]*[$\\]" str))
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

A function on this hook should accept three arguments: A list of
inserted, changed and deleted annotations, of which at least one
is non-empty."
  :group 'pdf-annot
  :type 'hook)



;; * ================================================================== *
;; * Variables and Macros
;; * ================================================================== *

(defconst pdf-annot-annotation-types
  '(unknown text link free-text line square
            circle polygon poly-line highlight underline squiggly
            strike-out stamp caret ink popup file sound movie widget screen
            printer-mark trap-net watermark 3d )
  "Complete list of annotation types.")
  
(defconst pdf-annot-text-markup-annotation-types
  '(text link free-text line square
         circle polygon poly-line highlight underline squiggly
         strike-out stamp caret ink file sound)
  "List of types constituting the set of markup annotations.")

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
    (with-current-buffer (pdf-annot-get-buffer a)
      (setq a (pdf-annot-create
               (pdf-info-editannot
                (pdf-annot-get a 'id)
                `((,property . ,value)))))
      (set-buffer-modified-p t)
      (pdf-annot-run-modified-hooks :change a)))
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
    (let ((changed (plist-get plist :change))
          (inserted (plist-get plist :insert))
          (deleted (plist-get plist :delete)))
      (unless (or pdf-annot-inhibit-modification-hooks
                  (cl-every 'null (list changed inserted deleted)))
        (unwind-protect
            (run-hook-with-args
             'pdf-annot-modified-functions
             (reverse inserted) (reverse changed) (reverse deleted))
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

(defun pdf-annot-text-markup-annotation-p (a)
  (not (null
        (memq (pdf-annot-get a 'type)
              pdf-annot-text-markup-annotation-types))))

(defun pdf-annot-text-annotation-p (a)
  (eq 'text (pdf-annot-get a 'type)))

(defun pdf-annot-has-attachments-p (a)
  (eq 'file (pdf-annot-get a 'type)))


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
          (list x y (+ x w) (+ y h)))))
    (cons x y)))

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
               (and (pdf-utils-edges-inside-p
                     (pdf-annot-get a 'edges)
                     rpos)
                    a))
             annots)))

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
         (a (or annot
                (pdf-annot-at-position mpos))))
    (unless a
      (error "No annotation at this position: %s" mpos))
    (let* ((pdf-annot-inhibit-modification-hooks t)
           (pdf-cache-image-inihibit t)
           (apos (pdf-annot-image-position a))
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
           (window (selected-window))
           make-pointer-invisible)
      (pdf-util-track-mouse-dragging (ev 0.1)
          (mouse-movement-p ev)
        (when (and (eq window (selected-window))
                   (eq 'image (car-safe (posn-object (event-start ev)))))
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
          (pdf-view-redisplay nil t)))
      (pdf-view-redisplay)
      (with-current-buffer buffer
        (pdf-annot-run-modified-hooks)))
    nil))
              
(defun pdf-annot-hotspot-function (page size) 
  "The image hotspots function.

To be added to `pdf-view-add-hotspot-function'."
  (let* ((annots (pdf-annot-getannots page '(text file)))
         (d 5)
         (pointer '(hand hdrag vdrag arrow))
         (ops '(move resize-horizontally resize-vertically
                     resize-diagonally))
         hotspots)
    (dolist (a annots)
      (let* ((e (pdf-util-scale
                 (pdf-annot-get a 'edges)
                 size
                 'round))
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
        (dotimes (i 4)
          (push `((rect . ,(pop rects))
                  ,(nth i ids)
                  (pointer
                   ,(nth i pointer)
                   help-echo
                   ,(nth i help)))
                hotspots)
          (pdf-annot-create-hotspot-binding (nth i ids) (nth i ops) a ))))
    hotspots))

(defun pdf-annot-create-hotspot-binding (id operation annotation)
  ;; Activating
  (local-set-key
   (vector id 'mouse-1)
   (lambda (ev)
     (interactive "@e")
     (pdf-annot-invoke-handler annotation)))
  ;; Move/Resize
  (local-set-key
   (vector id 'down-mouse-1)
   (lambda (ev)
     (interactive "@e")
     (pdf-annot-mouse-move/resize
      ev operation annotation)))
  ;; Context Menu
  (local-set-key
   (vector id 'down-mouse-3)
   (lambda ()
     (interactive "@")
     (popup-menu (pdf-annot-create-context-menu annotation))))
  ;; EE (you know ?)
  (local-set-key
   (vector id t)
   'pdf-util-image-map-mouse-event-proxy))


;; * ================================================================== *
;; * Displaying annotation contents
;; * ================================================================== *

(defun pdf-annot-print-property (a prop)
  (or (run-hook-with-args-until-success
       'pdf-annot-print-property-functions
       a prop)
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
      (pdf-annot-print-tooltip-default a)))

(defun pdf-annot-print-tooltip-default (a)
  (if (pdf-annot-has-attachments-p a)
      (pdf-annot-attach-print-tooltip
       (pdf-annot-get-attachment a))
    (concat
     (pdf-annot-print-tooltip-header a)
     (cl-case (pdf-annot-type a)
       (text
        (pdf-annot-get a 'contents))
       (t
        "Customize `pdf-annot-print-tooltip-functions',\
 to change what is displayed here.")))))

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
  (when (and (fboundp pdf-annot-latex-string-predicate)
             (funcall pdf-annot-latex-string-predicate
                      (pdf-annot-get a 'contents)))
    (pdf-annot-print-tooltip-latex a)))

(defun pdf-annot-print-tooltip-latex (a)
  (with-current-buffer (pdf-annot-get-buffer a)
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




(defun pdf-annot-invoke-handler (a)
  (or (run-hook-with-args-until-success
       'pdf-annot-handler-functions
       a)
      (pdf-annot-default-handler a)))

(defun pdf-annot-add-text-annotation (x y &optional page window)
  "Create a new text-annot at X,Y on PAGE in WINDOW."
  (interactive
   (let ((posn (pdf-util-read-image-position
                "Click on the spot where annotation should be added")))
     (list (car (posn-object-x-y posn))
           (cdr (posn-object-x-y posn))
           (posn-window posn))))
  (pdf-util-assert-pdf-window window)
  (save-selected-window
    (when window (select-window window))
    (unless page (setq page (pdf-view-current-page)))
    (let* ((width (float (car pdf-annot-text-annotation-initial-size)))
           (height (float (cdr pdf-annot-text-annotation-initial-size)))
           (pos (pdf-util-scale-pixel-to-points (cons x y)))
           ;; We center it.
           (x1 (- (car pos) (/ width 2)))
           (y1 (- (cdr pos) (/ height 2)))
           (x2 (+ (car pos) (/ width 2)))
           (y2 (+ (cdr pos) (/ height 2)))
           (a (pdf-annot-create
               (apply 'pdf-info-addannot page
                      (pdf-util-scale-points-to-relative
                       (list x1 y1 x2 y2))))))
      (pdf-annot-with-multiple-modifications
        (dolist (elt pdf-annot-text-annotation-initial-properties)
          (pdf-annot-put a (car elt) (cdr elt))))
      (pdf-view-redisplay t)
      (pdf-annot-invoke-handler a)
      a)))

(defun pdf-annot-mouse-add-text-annotation (ev)
  "Add a text annotation at event EV's position."
  (interactive "@e")
  (let ((window (posn-window (event-start ev)))
        (pos (posn-object-x-y (event-start ev))))
    (pdf-annot-add-text-annotation
     (car pos) (cdr pos) nil window)))

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

(defun pdf-annot-set-pointer-shape (shape)
  (if (null shape)
      (set-mouse-color (frame-parameter nil 'mouse-color))
    (let ((x-pointer-shape
           (if (numberp shape) shape
             (if (and (symbolp shape)
                      (boundp shape))
                 (symbol-value shape)))))
      (set-mouse-color (frame-parameter nil 'mouse-color)))))


;; * ================================================================== *
;; * Editing annotation contents
;; * ================================================================== *

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
      
(defvar pdf-annot-edit-text-display-buffer-action
  '((display-buffer-reuse-window
     display-buffer-split-below-and-attach)
    (inhibit-same-window . t)
    (window-height . 0.25)))

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
  (with-current-buffer (pdf-annot-get-buffer a)
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

(defun pdf-annot-edit-text (a)
  (let ((window (selected-window)))
    (select-window
     (display-buffer
      (pdf-annot-edit-text-noselect a)
      pdf-annot-edit-text-display-buffer-action))
    (when (and (window-live-p window)
               (eq (window-buffer window)
                   (pdf-annot-get-buffer a)))
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
;; *Utility functions
;; 

(defun pdf-annot-property-completions (prop)
  (cl-case prop
    (color (pdf-annot-color-completions))
    (icon (copy-sequence pdf-annot-standard-text-icons))))

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
      (define-key smap (kbd "C-a") 'pdf-annot-add-text-annotation)
      (define-key smap (kbd "a") 'pdf-annot-add-text-annotation)
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
                     :help "List annotations in a buffer"))
       (when (pdf-info-writable-annotations-p)
         (define-key menu [edit-annotation]
           `(menu-item "Edit text"
                       ,(lambda ()
                          (interactive)
                          (pdf-annot-edit-text a))
                       :help "Edit annotations text contents")))))
    menu))
  

(provide 'pdf-annot)
;;; pdf-annot.el ends here

