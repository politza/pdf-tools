;;; pdf-view.el --- View PDF documents. -*- lexical-binding:t -*-

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
;;; Code:
;; 

(require 'image-mode)
(require 'pdf-util)
(require 'pdf-info)
(require 'pdf-cache)
(require 'jka-compr)
(require 'bookmark)


;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-view nil
  "View PDF documents."
  :group 'pdf-tools)

(defcustom pdf-view-display-size 'fit-width
  "The desired size of displayed pages.

This may be one of `fit-height', `fit-width', `fit-page' or a
number as a scale factor applied to the document's size.  Any
other value behaves like `fit-width'."
  :group 'pdf-view
  :type '(choice number
                 (const fit-height)
                 (const fit-width)
                 (const fit-page)))

(make-variable-buffer-local 'pdf-view-display-size)

(defcustom pdf-view-resize-factor 1.25
  "Fractional amount of resizing of one resize command."
  :group 'pdf-view
  :type 'number)
  
(defcustom pdf-view-continuous t
  "In Continuous mode reaching the page edge advances to next/previous page.

When non-nil, scrolling a line upward at the bottom edge of the page
moves to the next page, and scrolling a line downward at the top edge
of the page moves to the previous page."
  :type 'boolean
  :group 'pdf-view)

(defcustom pdf-view-bounding-box-margin 0.05
  "Fractional margin used for slicing with the bounding-box."
  :group 'pdf-view
  :type 'number)

(defcustom pdf-view-use-imagemagick nil
  "Whether imagemagick should be used for rendering.

This variable has no effect, if imagemagick was not compiled into
Emacs or if imagemagick is the only way to display PNG images.
FIXME: Explain dis-/advantages of imagemagick and png."
  :group 'pdf-view
  :type 'boolean)

(defcustom pdf-view-use-scaling nil
  "Whether images should be allowed to be scaled down for rendering.

This variable has no effect, if imagemagick was not compiled into
Emacs or `pdf-view-use-imagemagick' is nil.  FIXME: Explain
dis-/advantages of imagemagick and png."
  :group 'pdf-view
  :type 'boolean)

(defface pdf-view-region
  '((((background dark)) (:inherit region))
    (((background light)) (:inherit region)))
  "Face used to determine the colors of the region."
  :group 'pdf-view
  :group 'pdf-tools-faces)

(defface pdf-view-rectangle
  '((((background dark)) (:inherit highlight))
    (((background light)) (:inherit highlight)))
  "Face used to determine the colors of the highlighted rectangle."
  :group 'pdf-view
  :group 'pdf-tools-faces)

(defcustom pdf-view-midnight-colors '("#839496" . "#002b36" )
  "Colors used when `pdf-view-midnight-minor-mode' is activated.

This should be a cons \(FOREGROUND . BACKGROUND\) of colors."
  :group 'pdf-view
  :type '(cons (color :tag "Foreground")
               (color :tag "Background")))

(defcustom pdf-view-change-page-hook nil
  "Hook run after changing to another page, but before displaying it.

See also `pdf-view-before-change-page-hook' and
`pdf-view-after-change-page-hook'."
  :group 'pdf-view
  :type 'hook)

(defcustom pdf-view-before-change-page-hook nil
  "Hook run before changing to another page.

See also `pdf-view-change-page-hook' and
`pdf-view-after-change-page-hook'."
  :group 'pdf-view
  :type 'hook)

(defcustom pdf-view-after-change-page-hook nil
  "Hook run after changing to and displaying another page.

See also `pdf-view-change-page-hook' and
`pdf-view-before-change-page-hook'."
  :group 'pdf-view
  :type 'hook)

(defcustom pdf-view-use-dedicated-register t
  "Whether to use dedicated register for PDF positions.

If this is non-nil, the commands `pdf-view-position-to-register'
and `pdf-view-jump-to-register' use the buffer-local variable
`pdf-view-register-alist' to store resp. retrieve marked
positions.  Otherwise the common variable `register-alist' is
used."
  :group 'pdf-view
  :type 'boolean)

(defcustom pdf-view-image-relief 0
  "Add a shadow rectangle around the page's image.

See :relief property in Info node `(elisp) Image Descriptors'."
  :group 'pdf-view
  :type '(integer :tag "Pixel")         
  :link '(info-link "(elisp) Image Descriptors"))

(defcustom pdf-view-max-image-width 4800
  "Maximum width of any image displayed in pixel."
  :group 'pdf-view
  :type '(integer :tag "Pixel"))


;; * ================================================================== *
;; * Internal variables and macros
;; * ================================================================== *

(defvar-local pdf-view-active-region nil
  "The active region as a list of edges.

Edge values are relative coordinates.")

(defvar-local pdf-view--have-rectangle-region nil
  "Non-nil if the region is currently rendered as a rectangle.

This variable is set in `pdf-view-mouse-set-region' and used in
`pdf-view-mouse-extend-region' to determine the right choice
regarding display of the region in the later function.")

(defvar-local pdf-view--buffer-file-name nil
  "Local copy of remote file or nil.")

(defvar-local pdf-view--server-file-name nil
  "The servers notion of this buffer's filename.")

(defvar-local pdf-view--next-page-timer nil
  "Timer used in `pdf-view-next-page-command'.")

(defvar-local pdf-view--hotspot-functions nil
  "Alist of hotspot functions.")

(defvar-local pdf-view-register-alist nil
  "Local, dedicated register for PDF positions.")

(defmacro pdf-view-current-page (&optional window)
  `(image-mode-window-get 'page ,window))
(defmacro pdf-view-current-overlay (&optional window)
  `(image-mode-window-get 'overlay ,window))
(defmacro pdf-view-current-image (&optional window)
  `(image-mode-window-get 'image ,window))
(defmacro pdf-view-current-slice (&optional window)
  `(image-mode-window-get 'slice ,window))
(defmacro pdf-view-current-window-size (&optional window)
  `(image-mode-window-get 'window-size ,window))

(defun pdf-view-active-region-p nil
  (not (null pdf-view-active-region)))

(defmacro pdf-view-assert-active-region ()
  `(unless (pdf-view-active-region-p)
     (error "The region is not active")))


;; * ================================================================== *
;; * Major Mode
;; * ================================================================== *

(defvar pdf-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    (define-key map (kbd "Q")         'kill-this-buffer)
    ;; Navigation in the document
    (define-key map (kbd "n")         'pdf-view-next-page-command)
    (define-key map (kbd "p")         'pdf-view-previous-page-command)
    (define-key map (kbd "<next>")    'forward-page)
    (define-key map (kbd "<prior>")   'backward-page)
    (define-key map [remap forward-page]  'pdf-view-next-page-command)
    (define-key map [remap backward-page] 'pdf-view-previous-page-command)
    (define-key map (kbd "SPC")       'pdf-view-scroll-up-or-next-page)
    (define-key map (kbd "S-SPC")     'pdf-view-scroll-down-or-previous-page)
    (define-key map (kbd "DEL")       'pdf-view-scroll-down-or-previous-page)
    (define-key map (kbd "C-n")       'pdf-view-next-line-or-next-page)
    (define-key map (kbd "<down>")    'pdf-view-next-line-or-next-page)
    (define-key map (kbd "C-p")       'pdf-view-previous-line-or-previous-page)
    (define-key map (kbd "<up>")      'pdf-view-previous-line-or-previous-page)
    (define-key map (kbd "M-<")       'pdf-view-first-page)
    (define-key map (kbd "M->")       'pdf-view-last-page)
    (define-key map [remap goto-line] 'pdf-view-goto-page)
    (define-key map (kbd "M-g l")     'pdf-view-goto-label)
    (define-key map (kbd "RET")       'image-next-line)
    ;; Zoom in/out.
    (define-key map "+"               'pdf-view-enlarge)
    (define-key map "="               'pdf-view-enlarge)
    (define-key map "-"               'pdf-view-shrink)
    (define-key map "0"               'pdf-view-scale-reset)
    ;; Fit the image to the window
    (define-key map "W"               'pdf-view-fit-width-to-window)
    (define-key map "H"               'pdf-view-fit-height-to-window)
    (define-key map "P"               'pdf-view-fit-page-to-window)
    ;; Slicing the image
    (define-key map (kbd "s m")       'pdf-view-set-slice-using-mouse)
    (define-key map (kbd "s b")       'pdf-view-set-slice-from-bounding-box)
    (define-key map (kbd "s r")       'pdf-view-reset-slice)
    ;; Reconvert
    (define-key map (kbd "C-c C-c")   'doc-view-mode)
    (define-key map (kbd "g")         'revert-buffer)
    (define-key map (kbd "r")         'revert-buffer)
    ;; Region
    (define-key map [down-mouse-1] 'pdf-view-mouse-set-region)
    (define-key map [M-down-mouse-1] 'pdf-view-mouse-set-region-rectangle)
    (define-key map [C-down-mouse-1] 'pdf-view-mouse-extend-region)
    (define-key map [remap kill-region] 'pdf-view-kill-ring-save)
    (define-key map [remap kill-ring-save] 'pdf-view-kill-ring-save)
    (define-key map [remap mark-whole-buffer] 'pdf-view-mark-whole-page)
    ;; Other
    (define-key map (kbd "C-c C-d") 'pdf-view-dark-minor-mode)
    (define-key map (kbd "m") 'pdf-view-position-to-register)
    (define-key map (kbd "'") 'pdf-view-jump-to-register)
    
    (define-key map (kbd "C-c C-i") 'pdf-view-extract-region-image)
    ;; Rendering
    (define-key map (kbd "C-c C-r m") 'pdf-view-midnight-minor-mode)
    (define-key map (kbd "C-c C-r p") 'pdf-view-printer-minor-mode)
    map)
  "Keymap used by `pdf-view-mode' when displaying a doc as a set of images.")

(define-derived-mode pdf-view-mode special-mode "PDFView"
  "Major mode in PDF buffers.

PDFView Mode is an Emacs PDF viewer.  It displays PDF files as
PNG images in Emacs buffers."
  :group 'pdf-view
  :abbrev-table nil
  :syntax-table nil
  ;; Setup a local copy for remote files.
  (when (and (or jka-compr-really-do-compress
                 (let ((file-name-handler-alist nil))
                   (not (and buffer-file-name
                             (file-readable-p buffer-file-name)))))
             (pdf-tools-pdf-buffer-p))             
    (let ((tempfile (pdf-util-make-temp-file
                     (concat (if buffer-file-name
                                 (file-name-nondirectory
                                  buffer-file-name)
                               (buffer-name))
                             "-"))))
      (write-region nil nil tempfile nil 'no-message)
      (setq-local pdf-view--buffer-file-name tempfile)))

  ;; Setup scroll functions
  (if (boundp 'mwheel-scroll-up-function) ; not --without-x build
      (setq-local mwheel-scroll-up-function
                  #'pdf-view-scroll-up-or-next-page))
  (if (boundp 'mwheel-scroll-down-function)
      (setq-local mwheel-scroll-down-function
                  #'pdf-view-scroll-down-or-previous-page))

  ;; Clearing overlays
  (add-hook 'change-major-mode-hook
            (lambda ()
              (remove-overlays (point-min) (point-max) 'pdf-view t))
            nil t)
  (remove-overlays (point-min) (point-max) 'pdf-view t) ;Just in case.

  ;; Setup other local variables.
  (setq-local mode-line-position
              '(" P" (:eval (number-to-string (pdf-view-current-page)))
                "/" (:eval (number-to-string (pdf-cache-number-of-pages)))))
  (setq-local auto-hscroll-mode nil)
  (setq-local pdf-view--server-file-name (pdf-view-buffer-file-name))
  ;; High values of scroll-conservatively seem to trigger
  ;; some display bug in xdisp.c:try_scrolling .
  (setq-local scroll-conservatively 0)
  (setq-local cursor-type nil)
  (setq-local buffer-read-only t)
  (setq-local view-read-only nil)
  (setq-local bookmark-make-record-function
              'pdf-view-bookmark-make-record)
  (setq-local revert-buffer-function #'pdf-view-revert-buffer)
  ;; No auto-save at the moment.
  (setq-local buffer-auto-save-file-name nil)
  ;; No undo at the moment.
  (unless buffer-undo-list
    (buffer-disable-undo))
  ;; Enable transient-mark-mode, so region deactivation when quitting
  ;; will work.
  (setq-local transient-mark-mode t)

  (add-hook 'window-configuration-change-hook
            'pdf-view-maybe-redisplay-resized-windows nil t)
  (add-hook 'deactivate-mark-hook 'pdf-view-deactivate-region nil t)
  (add-hook 'write-contents-functions
            'pdf-view--write-contents-function nil t)
  (add-hook 'kill-buffer-hook 'pdf-view-close-document nil t)
  (pdf-view-add-hotspot-function
   'pdf-view-text-regions-hotspots-function -9)
  
  ;; Keep track of display info
  (add-hook 'image-mode-new-window-functions
            'pdf-view-new-window-function nil t)
  (image-mode-setup-winprops)

  ;; Setup initial page and start display
  (pdf-view-goto-page (or (pdf-view-current-page) 1)))

(defun pdf-view-buffer-file-name ()
  "Return the local filename of the PDF in the current buffer.

This may be different from `buffer-file-name', when operating on
a local copy of a remote file."
  (or pdf-view--buffer-file-name
      (buffer-file-name)))

(defun pdf-view--write-contents-function ()
  "Function for `write-contents-functions' to save the buffer."
  (when (pdf-util-pdf-buffer-p)
    (let ((tempfile (pdf-info-save pdf-view--server-file-name)))
      (unwind-protect
          (progn
            ;; Order matters here: We need to first copy the new
            ;; content (tempfile) to the PDF, and then close the PDF.
            ;; Since while closing the file (and freeing its resources
            ;; in the process), it may be immediately reopened due to
            ;; redisplay happening inside the pdf-info-close function
            ;; (while waiting for a response from the process.).
            (copy-file tempfile (buffer-file-name) t)
            (pdf-info-close pdf-view--server-file-name)

            (when pdf-view--buffer-file-name
              (copy-file tempfile pdf-view--buffer-file-name t))
            (clear-visited-file-modtime)
            (set-buffer-modified-p nil)
            (setq pdf-view--server-file-name
                  (pdf-view-buffer-file-name))
            t)
        (when (file-exists-p tempfile)
          (delete-file tempfile))))))

(defun pdf-view-revert-buffer (&optional ignore-auto noconfirm)
  "Like `revert-buffer', but preserves the buffer's current modes."
  (interactive (list (not current-prefix-arg)))
  ;; Bind to default so that we can use pdf-view-revert-buffer as
  ;; revert-buffer-function.  A binding of nil is needed in Emacs 24.3, but in
  ;; later versions the semantics that nil means the default function should
  ;; not relied upon.
  (let ((revert-buffer-function (when (fboundp #'revert-buffer--default)
				  #'revert-buffer--default))
        (after-revert-hook
         (cons #'pdf-info-close
               after-revert-hook)))
    (prog1
        (revert-buffer ignore-auto noconfirm 'preserve-modes)
      (pdf-view-redisplay t))))

(defun pdf-view-close-document ()
  "Like `pdf-info-close', but returns immediately."
  (when (pdf-info-running-p)
    (let ((pdf-info-asynchronous 'ignore))
      (pdf-info-close))))


;; * ================================================================== *
;; * Scaling
;; * ================================================================== *

(defun pdf-view-fit-page-to-window ()
  (interactive)
  (setq pdf-view-display-size 'fit-page)
  (image-set-window-vscroll 0)
  (image-set-window-hscroll 0)
  (pdf-view-redisplay t))

(defun pdf-view-fit-height-to-window ()
  (interactive)
  (setq pdf-view-display-size 'fit-height)
  (image-set-window-vscroll 0)
  (pdf-view-redisplay t))

(defun pdf-view-fit-width-to-window ()
  (interactive)
  (setq pdf-view-display-size 'fit-width)
  (image-set-window-hscroll 0)
  (pdf-view-redisplay t))

(defun pdf-view-enlarge (factor)
  (interactive
   (list (float pdf-view-resize-factor)))
  (let* ((size (pdf-view-image-size))
         (pagesize (pdf-cache-pagesize
                    (pdf-view-current-page)))
         (scale (/ (float (car size))
                   (float (car pagesize)))))
    (setq pdf-view-display-size
          (* factor scale))
    (pdf-view-redisplay t)))

(defun pdf-view-shrink (factor)
  (interactive
   (list (float pdf-view-resize-factor)))
  (pdf-view-enlarge (/ 1.0 factor)))

(defun pdf-view-scale-reset ()
  (interactive)
  (setq pdf-view-display-size 1.0)
  (pdf-view-redisplay t))



;; * ================================================================== *
;; * Moving by pages and scrolling
;; * ================================================================== *

(defun pdf-view-goto-page (page &optional window)
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  (unless (and (>= page 1)
               (<= page (pdf-cache-number-of-pages)))
    (error "No such page: %d" page))
  (unless window
    (setq window
          (if (pdf-util-pdf-window-p)
              (selected-window)
            t)))
  (save-selected-window
    ;; Select the window for the hooks below.
    (when (window-live-p window)
      (select-window window))
    (let ((changing-p
           (not (eq page (pdf-view-current-page window)))))
      (when changing-p
        (run-hooks 'pdf-view-before-change-page-hook)
        (setf (pdf-view-current-page window) page)
        (run-hooks 'pdf-view-change-page-hook))
      (when (window-live-p window)
        (pdf-view-redisplay window))
      (when changing-p
        (pdf-view-deactivate-region)
        (force-mode-line-update)
        (run-hooks 'pdf-view-after-change-page-hook))))
  nil)

(defun pdf-view-next-page (&optional n)
  (interactive "p")
  (pdf-view-goto-page (+ (pdf-view-current-page)
                         (or n 1))))

(defun pdf-view-previous-page (&optional n)
  (interactive "p")
  (pdf-view-next-page (- (or n 1))))

(defun pdf-view-next-page-command (&optional n)
  (declare (interactive-only pdf-view-next-page))
  (interactive "p")
  (unless n (setq n 1))
  (when (> (+ (pdf-view-current-page) n)
           (pdf-cache-number-of-pages))
    (user-error "Last page"))
  (when (< (+ (pdf-view-current-page) n) 1)
    (user-error "First page"))
  (let ((pdf-view-inhibit-redisplay t))
    (pdf-view-goto-page
     (+ (pdf-view-current-page) n)))
  (force-mode-line-update)
  (sit-for 0)
  (when pdf-view--next-page-timer
    (cancel-timer pdf-view--next-page-timer)
    (setq pdf-view--next-page-timer nil))
  (if (or (not (input-pending-p))
          (and (> n 0)
               (= (pdf-view-current-page)
                  (pdf-cache-number-of-pages)))
          (and (< n 0)
               (= (pdf-view-current-page) 1)))
      (pdf-view-redisplay)
    (setq pdf-view--next-page-timer
          (run-with-idle-timer 0.001 nil 'pdf-view-redisplay (selected-window)))))

(defun pdf-view-previous-page-command (&optional n)
  (declare (interactive-only pdf-view-previous-page))
  (interactive "p")
  (with-no-warnings
    (pdf-view-next-page-command (- (or n 1)))))

(defun pdf-view-first-page ()
  "View the first page."
  (interactive)
  (pdf-view-goto-page 1))

(defun pdf-view-last-page ()
  "View the last page."
  (interactive)
  (pdf-view-goto-page (pdf-cache-number-of-pages)))

(defun pdf-view-scroll-up-or-next-page (&optional arg)
  "Scroll page up ARG lines if possible, else goto next page.
When `pdf-view-continuous' is non-nil, scrolling upward
at the bottom edge of the page moves to the next page.
Otherwise, goto next page only on typing SPC (ARG is nil)."
  (interactive "P")
  (if (or pdf-view-continuous (null arg))
      (let ((hscroll (window-hscroll))
	    (cur-page (pdf-view-current-page)))
	(when (or (= (window-vscroll) (image-scroll-up arg))
                  ;; Workaround rounding/off-by-one issues.
                  (memq pdf-view-display-size
                        '(fit-height fit-page)))
	  (pdf-view-next-page)
	  (when (/= cur-page (pdf-view-current-page))
	    (image-bob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-scroll-up arg)))

(defun pdf-view-scroll-down-or-previous-page (&optional arg)
  "Scroll page down ARG lines if possible, else goto previous page.
When `pdf-view-continuous' is non-nil, scrolling downward
at the top edge of the page moves to the previous page.
Otherwise, goto previous page only on typing DEL (ARG is nil)."
  (interactive "P")
  (if (or pdf-view-continuous (null arg))
      (let ((hscroll (window-hscroll))
	    (cur-page (pdf-view-current-page)))
	(when (or (= (window-vscroll) (image-scroll-down arg))
                  ;; Workaround rounding/off-by-one issues.
                  (memq pdf-view-display-size
                        '(fit-height fit-page)))
	  (pdf-view-previous-page)
	  (when (/= cur-page (pdf-view-current-page))
	    (image-eob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-scroll-down arg)))

(defun pdf-view-next-line-or-next-page (&optional arg)
  "Scroll upward by ARG lines if possible, else goto next page.
When `pdf-view-continuous' is non-nil, scrolling a line upward
at the bottom edge of the page moves to the next page."
  (interactive "p")
  (if pdf-view-continuous
      (let ((hscroll (window-hscroll))
	    (cur-page (pdf-view-current-page)))
	(when (= (window-vscroll) (image-next-line arg))
	  (pdf-view-next-page)
	  (when (/= cur-page (pdf-view-current-page))
	    (image-bob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-next-line 1)))

(defun pdf-view-previous-line-or-previous-page (&optional arg)
  "Scroll downward by ARG lines if possible, else goto previous page.
When `pdf-view-continuous' is non-nil, scrolling a line downward
at the top edge of the page moves to the previous page."
  (interactive "p")
  (if pdf-view-continuous
      (let ((hscroll (window-hscroll))
	    (cur-page (pdf-view-current-page)))
	(when (= (window-vscroll) (image-previous-line arg))
	  (pdf-view-previous-page)
	  (when (/= cur-page (pdf-view-current-page))
	    (image-eob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-previous-line arg)))

(defun pdf-view-goto-label (label)
  "Goto the page corresponding to LABEL.

Usually the label of a document's page is the same as its
displayed page number."
  (interactive
   (list (let ((labels (pdf-info-pagelabels)))
           (completing-read "Goto label: " labels nil t))))
  (let ((index (cl-position label (pdf-info-pagelabels) :test 'equal)))
    (unless index
      (error "No such label: %s" label))
    (pdf-view-goto-page (1+ index))))  


;; * ================================================================== *
;; * Slicing
;; * ================================================================== *

(defun pdf-view-set-slice (x y width height &optional window)
  "Set the slice of the pages that should be displayed.

X, Y, WIDTH and HEIGHT should be relative coordinates, i.e. in
\[0;1\].  To reset the slice use `pdf-view-reset-slice'."
  (unless (equal (pdf-view-current-slice window)
                 (list x y width height))
    (setf (pdf-view-current-slice window)
          (mapcar (lambda (v)
                    (max 0 (min 1 v)))
                  (list x y width height)))
    (pdf-view-redisplay window)))

(defun pdf-view-set-slice-using-mouse ()
  "Set the slice of the images that should be displayed.
You set the slice by pressing mouse-1 at its top-left corner and
dragging it to its bottom-right corner.  See also
`pdf-view-set-slice' and `pdf-view-reset-slice'."
  (interactive)
  (let ((size (pdf-view-image-size))
        x y w h done)
    (while (not done)
      (let ((e (read-event
		(concat "Press mouse-1 at the top-left corner and "
			"drag it to the bottom-right corner!"))))
	(when (eq (car e) 'drag-mouse-1)
	  (setq x (car (posn-object-x-y (event-start e))))
	  (setq y (cdr (posn-object-x-y (event-start e))))
	  (setq w (- (car (posn-object-x-y (event-end e))) x))
	  (setq h (- (cdr (posn-object-x-y (event-end e))) y))
	  (setq done t))))
    (apply 'pdf-view-set-slice
           (pdf-util-scale
            (list x y w h)
            (cons (/ 1.0 (float (car size)))
                  (/ 1.0 (float (cdr size))))))))

(defun pdf-view-set-slice-from-bounding-box (&optional window)
  "Set the slice from the page's bounding-box.

The result is that the margins are almost completely cropped,
much more accurate than could be done manually using
`pdf-view-set-slice-using-mouse'.

See also `pdf-view-bounding-box-margin'."
  (interactive)
  (let* ((bb (pdf-cache-boundingbox (pdf-view-current-page window)))
         (margin (max 0 (or pdf-view-bounding-box-margin 0)))
         (slice (list (- (nth 0 bb)
                         (/ margin 2.0))
                      (- (nth 1 bb)
                         (/ margin 2.0))
                      (+ (- (nth 2 bb) (nth 0 bb))
                         margin)
                      (+ (- (nth 3 bb) (nth 1 bb))
                         margin))))
    (apply 'pdf-view-set-slice
           (append slice (and window (list window))))))

(defun pdf-view-reset-slice (&optional window)
  "Reset the current slice.

After calling this function the whole page will be visible
again."
  (interactive)
  (when (pdf-view-current-slice window)
    (setf (pdf-view-current-slice window) nil)
    (pdf-view-redisplay window))
  nil)

(define-minor-mode pdf-view-auto-slice-minor-mode
  "Automatically slice pages according to their bounding boxes.

See also `pdf-view-set-slice-from-bounding-box'."
  nil nil nil
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-view-auto-slice-minor-mode
    (dolist (win (get-buffer-window-list nil nil t))
      (when (pdf-util-pdf-window-p win)
        (pdf-view-set-slice-from-bounding-box win)))
    (add-hook 'pdf-view-change-page-hook
              'pdf-view-set-slice-from-bounding-box nil t))
   (t
    (remove-hook 'pdf-view-change-page-hook
                 'pdf-view-set-slice-from-bounding-box t))))


;; * ================================================================== *
;; * Display
;; * ================================================================== *

(defvar pdf-view-inhibit-redisplay nil)
(defvar pdf-view-inhibit-hotspots nil)

(defun pdf-view-image-type ()
  "Return the image-type which should be used.

The return value is either imagemagick (if available and wanted
or if png is not available) or png.

Signal an error, if neither imagemagick nor png is available.

See also `pdf-view-use-imagemagick'."
  (cond ((and pdf-view-use-imagemagick
              (fboundp 'imagemagick-types))
         'imagemagick)
        ((image-type-available-p 'png)
         'png)
        ((fboundp 'imagemagick-types)
         'imagemagick)
        (t
         (error "PNG image supported not compiled into Emacs"))))

(defun pdf-view-use-scaling-p ()
  (and (eq 'imagemagick
           (pdf-view-image-type))
       pdf-view-use-scaling))

(defmacro pdf-view-create-image (data &rest props)
  "Like `create-image', but with set DATA-P and TYPE arguments."
  (declare (indent 1) (debug t))
  `(create-image ,data (pdf-view-image-type) t ,@props
                 :relief (or pdf-view-image-relief 0)))

(defun pdf-view-create-page (page &optional window)
  "Create an image of PAGE for display on WINDOW."
  (let* ((size (pdf-view-desired-image-size page window))
         (data (pdf-cache-renderpage
                page (car size)
                (if (not (pdf-view-use-scaling-p))
                    (car size)
                  (* 2 (car size)))))
         (hotspots (pdf-view-apply-hotspot-functions
                    window page size)))
    (pdf-view-create-image data 
      :width (car size)
      :map hotspots
      :pointer 'arrow)))

(defun pdf-view-image-size (&optional displayed-p window)
  "Return the size in pixel of the current image.

If DISPLAYED-P is non-nil, return the size of the displayed
image.  These values may be different, if slicing is used."
  (if displayed-p
      (with-selected-window (or window (selected-window))
        (image-display-size
         (image-get-display-property) t))
    (image-size (pdf-view-current-image window) t)))

(defun pdf-view-image-offset (&optional window)
  "Return the offset of the current image.

It is equal to \(LEFT . TOP\) of the current slice in pixel."

  (let* ((slice (pdf-view-current-slice window)))
    (cond
     (slice
      (pdf-util-scale-relative-to-pixel
       (cons (nth 0 slice) (nth 1 slice))
       window))
     (t
      (cons 0 0)))))

(defun pdf-view-display-page (page &optional window)
  "Display page PAGE in WINDOW."
  (pdf-view-display-image
   (pdf-view-create-page page window)
   window))

(defun pdf-view-display-image (image &optional window inhibit-slice-p)
  (let ((ol (pdf-view-current-overlay window)))
    (when (window-live-p (overlay-get ol 'window))
      (let* ((size (image-size image t))
             (slice (if (not inhibit-slice-p)
                        (pdf-view-current-slice window)))
             (displayed-width (floor
                               (if slice
                                   (* (nth 2 slice)
                                      (car (image-size image)))
                                 (car (image-size image))))))
        (setf (pdf-view-current-image window) image)
        (move-overlay ol (point-min) (point-max))
        ;; In case the window is wider than the image, center the image
        ;; horizontally.
        (overlay-put ol 'before-string
                     (when (> (window-width window)
                              displayed-width)
                       (propertize " " 'display
                                   `(space :align-to
                                           ,(/ (- (window-width window)
                                                  displayed-width) 2)))))
        (overlay-put ol 'display
                     (if slice
                         (list (cons 'slice
                                     (pdf-util-scale slice size 'round))
                               image)
                       image))
        (let* ((win (overlay-get ol 'window))
               (hscroll (image-mode-window-get 'hscroll win))
               (vscroll (image-mode-window-get 'vscroll win)))
          ;; Reset scroll settings, in case they were changed.
          (if hscroll (set-window-hscroll win hscroll))
          (if vscroll (set-window-vscroll win vscroll)))))))

(defun pdf-view-redisplay (&optional window)
  "Redisplay page in WINDOW.

If WINDOW is t, redisplay pages in all windows."
  (unless pdf-view-inhibit-redisplay
    (if (not (eq t window))
        (pdf-view-display-page
         (pdf-view-current-page window)
         window)
      (dolist (win (get-buffer-window-list nil nil t))
        (pdf-view-display-page
         (pdf-view-current-page win)
         win)))
    (force-mode-line-update)))

(defun pdf-view-redisplay-pages (&rest pages)
  (pdf-util-assert-pdf-buffer)
  (dolist (window (get-buffer-window-list nil nil t))
    (when (memq (pdf-view-current-page window)
                pages)
      (pdf-view-redisplay window))))

(defun pdf-view-maybe-redisplay-resized-windows ()
  "Redisplay some windows needing redisplay."
  (unless (or (numberp pdf-view-display-size)
              (pdf-view-active-region-p)
              (> (minibuffer-depth) 0))
    (dolist (window (get-buffer-window-list nil nil t))
      (let ((stored (pdf-view-current-window-size window))
            (size (cons (window-width window)
                        (window-height window))))
        (unless (equal size stored)
          (setf (pdf-view-current-window-size window) size)
          (unless (or (null stored)
                      (and (eq pdf-view-display-size 'fit-width)
                           (eq (car size) (car stored)))
                      (and (eq pdf-view-display-size 'fit-height)
                           (eq (cdr size) (cdr stored))))
            (pdf-view-redisplay window)))))))

(defun pdf-view-new-window-function (winprops)
  ;; (message "New window %s for buf %s" (car winprops) (current-buffer))
  (cl-assert (or (eq t (car winprops))
                 (eq (window-buffer (car winprops)) (current-buffer))))
  (let ((ol (image-mode-window-get 'overlay winprops)))
    (if ol
        (progn
          (setq ol (copy-overlay ol))
          ;; `ol' might actually be dead.
          (move-overlay ol (point-min) (point-max)))
      (setq ol (make-overlay (point-min) (point-max) nil t))
      (overlay-put ol 'pdf-view t))
    (overlay-put ol 'window (car winprops))
    (unless (windowp (car winprops))
      ;; It's a pseudo entry.  Let's make sure it's not displayed (the
      ;; `window' property is only effective if its value is a window).
      (cl-assert (eq t (car winprops)))
      (delete-overlay ol))
    (image-mode-window-put 'overlay ol winprops)
    ;; Clean up some overlays.
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (windowp (overlay-get ov 'window))
                 (not (window-live-p (overlay-get ov 'window))))
        (delete-overlay ov)))
    (when (and (windowp (car winprops))
               (null (image-mode-window-get 'image winprops)))
      ;; We're not displaying an image yet, so let's do so.  This
      ;; happens when the buffer is displayed for the first time.
      (with-selected-window (car winprops)
        (pdf-view-goto-page
         (or (image-mode-window-get 'page t) 1))))))

(defun pdf-view-desired-image-size (&optional page window)
  (let* ((pagesize (pdf-cache-pagesize
                    (or page (pdf-view-current-page window))))
         (slice (pdf-view-current-slice window))
         (width-scale (/ (/ (float (pdf-util-window-pixel-width window))
                            (or (nth 2 slice) 1.0))
                         (float (car pagesize))))
         (height (- (nth 3 (window-inside-pixel-edges window))
                    (nth 1 (window-inside-pixel-edges window))
                    1))
         (height-scale (/ (/ (float height)
                             (or (nth 3 slice) 1.0))
                          (float (cdr pagesize))))
         (scale width-scale))
    (if (numberp pdf-view-display-size)
        (setq scale (float pdf-view-display-size))
      (cl-case pdf-view-display-size
        (fit-page
         (setq scale (min height-scale width-scale)))
        (fit-height
         (setq scale height-scale))
        (t
         (setq scale width-scale))))
    (let ((width (floor (* (car pagesize) scale)))
	  (height (floor (* (cdr pagesize) scale))))
      (when (> width (max 1 (or pdf-view-max-image-width width)))
	(setq width pdf-view-max-image-width
	      height (* height (/ (float pdf-view-max-image-width) width))))
      (cons (max 1 width) (max 1 height)))))

(defun pdf-view-text-regions-hotspots-function (page size)
  "Return a list of hotspots for text regions on PAGE using SIZE.

This will display a text cursor, when hovering over them."
  (local-set-key [pdf-view-text-region t]
                 'pdf-util-image-map-mouse-event-proxy)
  (mapcar (lambda (region)
            (let ((e (pdf-util-scale region size 'round)))
              `((rect . ((,(nth 0 e) . ,(nth 1 e))
                         . (,(nth 2 e) . ,(nth 3 e))))
                pdf-view-text-region
                (pointer text))))
          (pdf-cache-textregions page)))

(define-minor-mode pdf-view-dark-minor-mode
  "Mode for PDF documents with dark background.

This tells the various modes to use their face's dark colors."
  nil nil nil
  (pdf-util-assert-pdf-buffer)
  ;; FIXME: This should really be run in a hook.
  (when (bound-and-true-p pdf-isearch-active-mode)
    (with-no-warnings
      (pdf-isearch-redisplay)
      (pdf-isearch-message
       (if pdf-view-dark-minor-mode "dark mode" "light mode")))))

(define-minor-mode pdf-view-printer-minor-mode
  "Display the PDF as it would be printed."
  nil " Prn" nil
  (pdf-util-assert-pdf-buffer)
  (let ((enable (lambda ()
                  (pdf-info-setoptions :render/printed t))))
    (cond
     (pdf-view-printer-minor-mode
      (add-hook 'after-save-hook enable nil t)
      (add-hook 'after-revert-hook enable nil t))
     (t
      (remove-hook 'after-save-hook enable t)
      (remove-hook 'after-revert-hook enable t))))
  (pdf-info-setoptions :render/printed pdf-view-printer-minor-mode)
  (pdf-cache-clear-images) 
  (pdf-view-redisplay t))

(define-minor-mode pdf-view-midnight-minor-mode
  "Apply a color-filter appropriate for past midnight reading.

The colors are determined by the variable
`pdf-view-midnight-colors', which see. "

  nil " Mid" nil
  (pdf-util-assert-pdf-buffer)
  ;; FIXME: Maybe these options should be passed stateless to pdf-info-renderpage ?
  (let ((enable (lambda ()
                  (pdf-info-setoptions
                   :render/foreground (or (car pdf-view-midnight-colors) "black")
                   :render/background (or (cdr pdf-view-midnight-colors) "white")
                   :render/usecolors t))))
    (cond
     (pdf-view-midnight-minor-mode
      (add-hook 'after-save-hook enable nil t)
      (add-hook 'after-revert-hook enable nil t)
      (funcall enable))
     (t
      (remove-hook 'after-save-hook enable t)
      (remove-hook 'after-revert-hook enable t)
      (pdf-info-setoptions :render/usecolors nil))))
  (pdf-cache-clear-images)
  (pdf-view-redisplay t))

;; This check uses an implementation detail, which hopefully gets the
;; right answer.
(and (fontp (char-displayable-p ?⎙))
     (setcdr (assq 'pdf-view-printer-minor-mode minor-mode-alist)
             (list " ⎙" )))
(and (fontp (char-displayable-p ?🌙))
     (setcdr (assq 'pdf-view-midnight-minor-mode minor-mode-alist)
             (list  " 🌙" )))


;; * ================================================================== *
;; * Hotspot handling
;; * ================================================================== *

(defun pdf-view-add-hotspot-function (fn &optional layer)
  "Register FN as a hotspot function in the current buffer, using LAYER.

FN will be called in the PDF buffer with the page-number and the
image size \(WIDTH . HEIGHT\) as arguments.  It should return a
list of hotspots applicable to the the :map image-property.

LAYER determines the order: Functions in a higher LAYER will
supercede hotspots in lower ones."
  (push (cons (or layer 0) fn)
        pdf-view--hotspot-functions))

(defun pdf-view-remove-hotspot-function (fn)
  "Unregister FN as a hotspot function in the current buffer."
  (setq pdf-view--hotspot-functions
        (cl-remove fn pdf-view--hotspot-functions
                   :key 'cdr)))

(defun pdf-view-sorted-hotspot-functions ()
  (mapcar 'cdr (cl-sort (copy-sequence pdf-view--hotspot-functions)
                        '> :key 'car)))

(defun pdf-view-apply-hotspot-functions (window page image-size)
  (unless pdf-view-inhibit-hotspots
    (save-selected-window
      (when window (select-window window))
      (apply 'nconc
             (mapcar (lambda (fn)
                       (funcall fn page image-size))
                     (pdf-view-sorted-hotspot-functions))))))


;; * ================================================================== *
;; * Region
;; * ================================================================== *

(defun pdf-view--push-mark ()
  (let (mark-ring)
    (push-mark-command nil))
  (setq deactivate-mark nil))

(defun pdf-view-active-region (&optional deactivate-p)
  "Return the active region, a list of edges.

Deactivate the region if DEACTIVATE-P is non-nil."
  
  (pdf-view-assert-active-region)
  (prog1
      pdf-view-active-region
    (when deactivate-p
      (pdf-view-deactivate-region))))

(defun pdf-view-deactivate-region ()
  "Deactivate the region."
  (interactive)
  (when pdf-view-active-region
    (setq pdf-view-active-region nil)
    (deactivate-mark)
    (pdf-view-redisplay t)))

(defun pdf-view-mouse-set-region (event &optional allow-extend-p
                                        rectangle-p)
  "Selects a region of text using the mouse.

Allow for stacking of regions, if ALLOW-EXTEND-P is non-nil.

Create a rectangular region, if RECTANGLE-P is non-nil.

Stores the region in `pdf-view-active-region'."
  (interactive "@e")
  (setq pdf-view--have-rectangle-region rectangle-p)
  (unless (and (eventp event)
               (mouse-event-p event))
    (signal 'wrong-type-argument (list 'mouse-event-p event)))
  (unless (and allow-extend-p
               (or (null (get this-command 'pdf-view-region-window))
                   (equal (get this-command 'pdf-view-region-window)
                          (selected-window))))
    (pdf-view-deactivate-region))
  (put this-command 'pdf-view-region-window
       (selected-window))
  (let* ((window (selected-window))
         (pos (event-start event))
         (begin-inside-image-p t)
         (begin (if (posn-image pos)
                    (posn-object-x-y pos)
                  (setq begin-inside-image-p nil)
                  (posn-x-y pos)))
         (abs-begin (posn-x-y pos))
         pdf-view-continuous
         region)
    (when (pdf-util-track-mouse-dragging (event 0.15)
            (let* ((pos (event-start event))
                   (end (posn-object-x-y pos))
                   (end-inside-image-p
                    (and (eq window (posn-window pos))
                         (posn-image pos))))
              (when (or end-inside-image-p
                        begin-inside-image-p)
                (cond
                 ((and end-inside-image-p
                       (not begin-inside-image-p))
                  ;; Started selection ouside the image, setup begin.
                  (let* ((xy (posn-x-y pos))
                         (dxy (cons (- (car xy) (car begin))
                                    (- (cdr xy) (cdr begin))))
                         (size (pdf-view-image-size t)))
                    (setq begin (cons (max 0 (min (car size)
                                                  (- (car end) (car dxy))))
                                      (max 0 (min (cdr size)
                                                  (- (cdr end) (cdr dxy)))))
                          ;; Store absolute position for later.
                          abs-begin (cons (- (car xy)
                                             (- (car end)
                                                (car begin)))
                                          (- (cdr xy)
                                             (- (cdr end)
                                                (cdr begin))))
                          begin-inside-image-p t)))
                 ((and begin-inside-image-p
                       (not end-inside-image-p))
                  ;; Moved outside the image, setup end.
                  (let* ((xy (posn-x-y pos))
                         (dxy (cons (- (car xy) (car abs-begin))
                                    (- (cdr xy) (cdr abs-begin))))
                         (size (pdf-view-image-size t)))
                    (setq end (cons (max 0 (min (car size)
                                                (+ (car begin) (car dxy))))
                                    (max 0 (min (cdr size)
                                                (+ (cdr begin) (cdr dxy)))))))))
                (let ((iregion (if rectangle-p
				   (list (min (car begin) (car end))
					 (min (cdr begin) (cdr end))
					 (max (car begin) (car end))
					 (max (cdr begin) (cdr end)))
				 (list (car begin) (cdr begin)
				       (car end) (cdr end)))))
                  (setq region
                        (pdf-util-scale-pixel-to-relative iregion))
                  (pdf-view-display-region
                   (cons region pdf-view-active-region)
                   rectangle-p)
                  (pdf-util-scroll-to-edges iregion)))))
      (setq pdf-view-active-region
            (append pdf-view-active-region
                    (list region)))
      (pdf-view--push-mark))))

(defun pdf-view-mouse-extend-region (event)
  "Extend the currently active region."
  (interactive "@e")
  (pdf-view-mouse-set-region
   event t pdf-view--have-rectangle-region))

(defun pdf-view-mouse-set-region-rectangle (event)
  "Like `pdf-view-mouse-set-region' but displays as a rectangle.

This is more useful for commands like
`pdf-view-extract-region-image'."
  (interactive "@e")
  (pdf-view-mouse-set-region event nil t))

(defun pdf-view-display-region (&optional region rectangle-p)
  (unless region
    (pdf-view-assert-active-region)
    (setq region pdf-view-active-region))
  (let ((colors (pdf-util-face-colors
                 (if rectangle-p 'pdf-view-rectangle 'pdf-view-region)
                 (bound-and-true-p pdf-view-dark-minor-mode)))
        (page (pdf-view-current-page))
        (width (car (pdf-view-image-size))))
    (pdf-view-display-image
     (pdf-view-create-image
         (if rectangle-p
             (pdf-info-renderpage-highlight
              page width nil
              `(,(car colors) ,(cdr colors) 0.35 ,@region))
           (pdf-info-renderpage-text-regions
            page width nil nil
            `(,(car colors) ,(cdr colors) ,@region)))))))
    
(defun pdf-view-kill-ring-save ()
  "Copy the region to the `kill-ring'."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (pdf-view-active-region-text)))
    (pdf-view-deactivate-region)
    (kill-new (mapconcat 'identity txt "\n"))))

(defun pdf-view-mark-whole-page ()
  "Mark the whole page."
  (interactive)
  (pdf-view-deactivate-region)
  (setq pdf-view-active-region
        (list (list 0 0 1 1)))
  (pdf-view--push-mark)
  (pdf-view-display-region))

(defun pdf-view-active-region-text ()
  "Return the text of the active region as a list of strings."
  (pdf-view-assert-active-region)
  (mapcar
   (apply-partially 'pdf-info-gettext (pdf-view-current-page))
   pdf-view-active-region))

(defun pdf-view-extract-region-image (regions &optional page size
                                              output-buffer no-display-p)
  "Create a PNG image of REGIONS.

REGIONS should have the same form as `pdf-view-active-region',
which see.  PAGE and size are the page resp. base-size of the
image from which the image-regions will be created; they default
to `pdf-view-current-page' resp. `pdf-view-image-size'.

Put the image in OUTPUT-BUFFER, defaulting to \"*PDF region
image*\" and display it, unless NO-DISPLAY-P is non-nil.

In case of multiple regions, the resulting image is constructed
by joining them horizontally.  For this operation (and this only)
the `convert' programm is used. "

  (interactive
   (list (if (pdf-view-active-region-p)
             (pdf-view-active-region t)
           '((0 0 1 1)))))
  (unless page
    (setq page (pdf-view-current-page)))
  (unless size
    (setq size (pdf-view-image-size)))
  (unless output-buffer
    (setq output-buffer (get-buffer-create "*PDF image*")))
  (let* ((images (mapcar (lambda (edges)
                           (let ((file (make-temp-file "pdf-view"))
                                 (coding-system-for-write 'binary))
                             (write-region
                              (pdf-info-renderpage
                               page (car size)
                               :crop-to edges)
                              nil file nil 'no-message)
                             file))
                         regions))
         result)
    (unwind-protect
        (progn
          (if (= (length images) 1)
              (setq result (car images))
            (setq result (make-temp-file "pdf-view"))
            ;; Join the images horizontally with a gap of 10 pixel.
            (pdf-util-convert
             "-noop" ;; workaround limitations of this function
             result
             :commands `("("
                         ,@images
                         "-background" "white"
                         "-splice" "0x10+0+0"
                         ")"
                         "-gravity" "Center"
                         "-append"
                         "+gravity"
                         "-chop" "0x10+0+0")                         
             :apply '((0 0 0 0))))
          (with-current-buffer output-buffer
            (let ((inhibit-read-only t))
              (erase-buffer))
            (set-buffer-multibyte nil)
            (insert-file-contents-literally result)
            (image-mode)
            (unless no-display-p
              (pop-to-buffer (current-buffer)))))
      (dolist (f (cons result images))
        (when (file-exists-p f)
          (delete-file f))))))

;; * ================================================================== *
;; * Bookmark + Register Integration
;; * ================================================================== *

(defun pdf-view-bookmark-make-record  (&optional no-page no-slice no-size no-origin)
  "Create a bookmark PDF record.

The optional, boolean args exclude certain attributes."
  (let ((displayed-p (eq (current-buffer)
                         (window-buffer))))
    (cons (buffer-name)
          (append (bookmark-make-record-default nil t 1)
                  `(,(unless no-page
                       (cons 'page (pdf-view-current-page)))
                    ,(unless no-slice
                       (cons 'slice (and displayed-p
                                         (pdf-view-current-slice))))
                    ,(unless no-size
                       (cons 'size pdf-view-display-size))
                    ,(unless no-origin
                       (cons 'origin
                             (and displayed-p
                                  (let ((edges (pdf-util-image-displayed-edges nil t)))
                                    (pdf-util-scale-pixel-to-relative
                                     (cons (car edges) (cadr edges)) nil t)))))
                    (handler . pdf-view-bookmark-jump-handler))))))

;;;###autoload
(defun pdf-view-bookmark-jump-handler (bmk)
  "The bookmark handler-function interface for PDF bookmarks.

See also `pdf-view-bookmark-make-record'."
  (let ((page (bookmark-prop-get bmk 'page))
	(slice (bookmark-prop-get bmk 'slice))
        (size (bookmark-prop-get bmk 'size))
        (origin (bookmark-prop-get bmk 'origin))
	(file (bookmark-prop-get bmk 'filename))
	(show-fn-sym (make-symbol "pdf-view-bookmark-after-jump-hook")))
    (fset show-fn-sym
	  (lambda ()
	    (remove-hook 'bookmark-after-jump-hook show-fn-sym)
	    (unless (derived-mode-p 'pdf-view-mode)
	      (pdf-view-mode))
	    (with-selected-window
		(or (get-buffer-window (current-buffer) 0)
		    (selected-window))
	      (when size
                (setq-local pdf-view-display-size size))
              (when slice
                (apply 'pdf-view-set-slice slice))
	      (when (numberp page)
                (pdf-view-goto-page page))
              (when origin
                (let ((size (pdf-view-image-size t)))
                  (image-set-window-hscroll
                   (round (/ (* (car origin) (car size))
                             (frame-char-width))))
                  (image-set-window-vscroll
                   (round (/ (* (cdr origin) (cdr size))
                             (frame-char-height)))))))))
    (add-hook 'bookmark-after-jump-hook show-fn-sym)
    (set-buffer (or (find-buffer-visiting file)
                    (find-file-noselect file)))))

(defun pdf-view-bookmark-jump (bmk)
  "Switch to bookmark BMK.

This function is like `bookmark-jump', but it always uses the
selected window for display and does not run any hooks.  Also, it
works only with bookmarks created by
`pdf-view-bookmark-make-record'."

  (let* ((file (bookmark-prop-get bmk 'filename))
         (buffer (or (find-buffer-visiting file)
                     (find-file-noselect file))))
    (switch-to-buffer buffer)
    (let (bookmark-after-jump-hook)
      (pdf-view-bookmark-jump-handler bmk)
      (run-hooks 'bookmark-after-jump-hook))))
    
(defun pdf-view-registerv-make ()
  "Create a PDF register entry of the current position."
  (registerv-make
   (pdf-view-bookmark-make-record nil t t)
   :print-func 'pdf-view-registerv-print-func
   :jump-func 'pdf-view-bookmark-jump
   :insert-func (lambda (bmk)
                  (insert (format "%S" bmk)))))

(defun pdf-view-registerv-print-func (bmk)
  "Print a textual representation of bookmark BMK.

This function is used as the `:print-func' property with
`registerv-make'."
  (let* ((file (bookmark-prop-get bmk 'filename))
         (buffer (find-buffer-visiting file))
         (page (bookmark-prop-get bmk 'page))
         (origin (bookmark-prop-get bmk 'origin)))
    (princ (format "PDF position: %s, page %d, %d%%"
                   (if buffer
                       (buffer-name buffer)
                     file)
                   (or page 1)
                   (if origin
                       (round (* 100 (cdr origin)))
                     0)))))

(defmacro pdf-view-with-register-alist (&rest body)
  "Setup the proper binding for `register-alist' in body.

This macro may not work as desired when it is nested.  See also
`pdf-view-use-dedicated-register'."
  (declare (debug t) (indent 0))
  (let ((dedicated-p (make-symbol "dedicated-p")))
    `(let* ((,dedicated-p pdf-view-use-dedicated-register)
            (register-alist
             (if ,dedicated-p
                 pdf-view-register-alist
               register-alist)))
       (unwind-protect
           (progn ,@body)
         (when ,dedicated-p
           (setq pdf-view-register-alist register-alist))))))
    
(defun pdf-view-position-to-register (register)
  "Store current PDF position in register REGISTER.

See also `point-to-register'."
  (interactive
   (list (pdf-view-with-register-alist
           (register-read-with-preview "Position to register: "))))
  (pdf-view-with-register-alist
    (set-register register (pdf-view-registerv-make))))

(defun pdf-view-jump-to-register (register &optional delete return-register)
  "Move point to a position stored in a REGISTER."
  (interactive
   (pdf-view-with-register-alist
     (list 
      (register-read-with-preview "Jump to register: ")
      current-prefix-arg
      (and (or pdf-view-use-dedicated-register
               (local-variable-p 'register-alist))
           (characterp last-command-event)
           last-command-event))))
  (pdf-view-with-register-alist
    (let ((return-pos (and return-register
                           (pdf-view-registerv-make))))
      (jump-to-register register delete)
      (when return-register
        (set-register return-register return-pos)))))

(provide 'pdf-view)

;;; pdf-view.el ends here
