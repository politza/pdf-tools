;;; pdf-misc.el --- Miscellanous commands for PDF buffer.

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

(require 'pdf-util)
(require 'imenu)

;;; Code:

(defvar pdf-misc-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [drag-mouse-1] 'pdf-misc-copy-selection)
    (define-key kmap (kbd "C-w") 'pdf-misc-copy-page)
    (define-key kmap (kbd "C-c C-d") 'pdf-misc-dark-mode)
    (define-key kmap (kbd "I") 'pdf-misc-display-metadata)
    (define-key kmap (kbd "s w") 'pdf-misc-crop-to-window)
    (define-key kmap (kbd "s p") 'pdf-misc-crop-to-page)
    (dolist (key (where-is-internal 'occur))
      (define-key kmap key 'pdf-occur))
    kmap)
  "Keymap used in `pdf-misc-minor-mode'.")


;;;###autoload
(define-minor-mode pdf-misc-minor-mode
  "Miscellanous smaller commands.

\\{pdf-misc-minor-mode-map}"
  nil nil nil)

(defun pdf-misc-copy-selection (ev)
  "Copy the selection represented by EV to the `kill-ring'.

EV should be a mouse-drag event, i.e. this command should be
bound to an appropriate event.

The selection works as usual, e.g. like the region in Emacs."
  (interactive "@e")
  (pdf-util-assert-pdf-buffer)
  (unless (and (eventp ev)
               (mouse-event-p ev)
               (= (length ev) 3))
    (error "This command must be bound to a mouse-drag event"))
  (when (and (posn-image (car (cdr ev)))
             (posn-image (car (cddr ev))))
    (let* ((beg (posn-object-x-y (car (cdr ev))))
           (end (posn-object-x-y (car (cddr ev))))
           (txt (pdf-misc-selection-string
                 (min (car beg) (car end))
                 (min (cdr beg) (cdr end))
                 (max (car beg) (car end))
                 (max (cdr beg) (cdr end)))))
      (message "%s" txt)
      (kill-new txt))))

(defun pdf-misc-copy-page ()
  "Copy the whole page to the `kill-ring'."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (let ((txt (pdf-info-gettext
              (doc-view-current-page) 0 0 1 1)))
    (kill-new txt)
    (message "Page copied")))

(defun pdf-misc-selection-string (x0 y0 x1 y1 &optional page)
  "Return the text of the selection X0 Y0 X1 Y1 on PAGE."
  (pdf-util-assert-pdf-buffer)
  (let* ((size (pdf-util-image-size))
         (x0 (/ x0 (float (car size))))
         (y0 (/ y0 (float (cdr size))))
         (x1 (/ x1 (float (car size))))
         (y1 (/ y1 (float (cdr size)))))
    (pdf-info-gettext
     (or page (doc-view-current-page))
     x0 y0 x1 y1)))

(defun pdf-misc-display-metadata ()
  "Display all available metadata in a separate buffer."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (let* ((file (file-name-nondirectory
                (buffer-file-name)))
         (md (pdf-info-metadata)))
    (with-current-buffer (get-buffer-create "*PDF-Metadata*")
      (let* ((inhibit-read-only t)
             (pad (apply' max (mapcar (lambda (d)
                                        (length (symbol-name (car d))))
                                      md)))
             (fmt (format "%%%ds:%%s\n" pad))
             window)
        (erase-buffer)
        (setq header-line-format file
              buffer-read-only t)
        (font-lock-mode 1)
        (font-lock-add-keywords nil
          '(("^ *\\(\\(?:\\w\\|-\\)+\\):"
             (1 font-lock-keyword-face))))
        (dolist (d md)
          (let ((key (car d))
                (val (cdr d)))
            (cl-case key
              (keywords
               (setq val (mapconcat 'identity val ", "))))
            (let ((beg (+ (length (symbol-name key)) (point) 1))
                  (fill-prefix
                   (make-string (1+ pad) ?\s)))
              (insert (format fmt key val))
              (fill-region beg (point) )))))
      (goto-char 1)
      (display-buffer (current-buffer)))
    md))

(defun pdf-misc-crop-margins (&optional margin)
  "Set the slice from the document's BoundingBox information.

Leave a border of size MARGIN.  Raise an error if the BoundingBox
is unavailable or invalid."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))))
  (unless margin (setq margin 15))
  (let ((bb (doc-view-get-bounding-box)))
    (cond
     ((not bb)
      (message "BoundingBox couldn't be determined"))
     ((or (= 0 (- (nth 2 bb) (nth 0 bb)))
          (= 0 (- (nth 3 bb) (nth 1 bb))))
      (message "BoundingBox is empty"))
     (t
      (let* ((is (image-size (doc-view-current-image) t))
	     (iw (car is))
	     (ih (cdr is))
	     (ps (pdf-info-pagesize (doc-view-current-page)))
	     (bb (pdf-util-scale-edges
                  bb (cons (/ (float iw) (car ps))
                           (/ (float ih) (cdr ps))))))
        (pdf-util-with-edges (bb)
          (doc-view-set-slice (max 0 (- bb-left margin))
                              (max 0 (- ih bb-bot margin))
                              (min iw (+ bb-width (* margin 2)))
                              (min ih (+ bb-height (* margin 2))))))))))


(defun pdf-misc-crop-to-window (&optional margin)
  "Crop the document and scale it to the window's width.

Leave a border of MARGIN."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))))
  (pdf-misc-crop-margins margin)
  (doc-view-fit-width-to-window)
  (set-window-hscroll nil 0))

(defun pdf-misc-crop-to-page (&optional margin)
  "Crop the document and scale it to the current page.

Leave a border of MARGIN."
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))))
  (pdf-misc-crop-margins margin)
  (doc-view-fit-page-to-window)
  (set-window-hscroll nil 0)
  (set-window-vscroll nil 0))


;;
;; Colors
;;

(define-minor-mode pdf-misc-dark-mode
  "Mode for documents with dark background.

This tells `pdf-isearch-minor-mode' to use dark colors."
  nil nil nil
  ;; FIXME: This should really be run in a hook.
  (with-no-warnings
    (when (and (featurep 'pdf-isearch)
               pdf-isearch-active-mode)
      (pdf-isearch-redisplay)
      (pdf-isearch-message
       (if pdf-misc-dark-mode "dark mode" "light mode")))))



;;
;; Mode line
;;

(define-minor-mode pdf-misc-size-indication-minor-mode
  "" nil nil nil
  (pdf-util-assert-docview-buffer)
  (cond
   (pdf-misc-size-indication-minor-mode
    (unless (assq 'pdf-misc-size-indication-minor-mode
                  mode-line-position)
      (setq mode-line-position
            `((pdf-misc-size-indication-minor-mode 
               (:eval (pdf-misc-size-indication)))
              ,@mode-line-position))))
   (t
    (setq mode-line-position
          (cl-remove 'pdf-misc-size-indication-minor-mode
                     mode-line-position :key 'car-safe)))))

(defun pdf-misc-size-indication ()
  (let ((top (= (window-vscroll nil t) 0))
        (bot (>= (+ (- (nth 3 (window-inside-pixel-edges))
                       (nth 1 (window-inside-pixel-edges)))
                    (window-vscroll nil t))
                 (cdr (pdf-util-image-size t)))))
    (cond
     ((and top bot) " All")
     (top " Top")
     (bot " Bot")
     (t (format
         " %d%%%%"
         (ceiling
          (* 100 (/ (float (window-vscroll nil t))
                    (cdr (pdf-util-image-size t))))))))))
  

;; 
;; Menu Bar
;;

(defvar pdf-misc-menu-bar-minor-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap)))

    (define-key map [menu-bar pdf-tools]
      (cons "PDF Tools" menu))
    
    (define-key menu [customize]
      '(menu-item "Customize" pdf-tools-customize
                  :help "Customize PDF Tools"))

    (define-key menu [sep] menu-bar-separator)
    
    (define-key menu [decorate]
      '(menu-item "Decorate Links" pdf-links-toggle-decoration
                  :help "Display or undisplay links (will reconvert document)"
                  :button (:toggle . pdf-links-decorate-p)
                  :visible (featurep 'pdf-links)))

    (define-key menu [dark]
      '(menu-item "Dark Mode" pdf-misc-dark-mode
                  :help "Prefer a different color set"
                  :button (:toggle . pdf-misc-dark-mode)
                  :visible (featurep 'pdf-isearch)))
    
    (define-key menu [sep-0] menu-bar-separator)

    (define-key menu [metadata]
      '(menu-item "Display Metadata" pdf-misc-display-metadata
                  :help "Display information about the document"
                  :visible (featurep 'pdf-misc)))
    
    (define-key menu [outline]
      '(menu-item "Display Outline" pdf-outline
                  :help "Display documents outline"
                  :visible (featurep 'pdf-outline)))

    (define-key menu [sep-1] menu-bar-separator)

    (define-key menu [occur]
      '(menu-item "Occur Document" pdf-occur
                  :help "Display lines containing a string"
                  :visible (featurep 'pdf-occur)))
    
    (define-key menu [isearch]
      '(menu-item "Isearch Document" isearch-forward
                  :help "Interactively search the document"
                  :visible (featurep 'pdf-isearch)))

    (define-key menu [sep-2] menu-bar-separator)

    (define-key menu [copy-page]
      '(menu-item "Copy page" pdf-misc-copy-page
                  :help "Copy the text of the page to the kill-ring"
                  :visible (featurep 'pdf-misc)))

    (define-key menu [sep-3] menu-bar-separator)
    
    (define-key menu [hist-forward]
      '(menu-item "Go Forward" pdf-history-forward
                  :help "Go forward in history"
                  :visible (bound-and-true-p pdf-history-minor-mode)
                  :enable (not (pdf-history-beginning-of-history-p))))
    
    (define-key menu [hist-backward]
      '(menu-item "Go Back" pdf-history-backward
                  :help "Go back in history"
                  :visible (bound-and-true-p pdf-history-minor-mode)
                  :enable (not (pdf-history-end-of-history-p))))
    
     map)
     "The keymap used in `pdf-misc-menu-bar-minor-mode'.")

(define-minor-mode pdf-misc-menu-bar-minor-mode
  "Display a PDF Tools menu in the menu-bar."
  nil nil nil
  (pdf-util-assert-pdf-buffer))


;;
;; Tool Bar
;;

(defun pdf-misc-imenu-menu-map ()
  "Return a imenu menu-bar keymap."
  (let ((menu (imenu--split-menu
               (imenu--split-submenus
                (imenu--make-index-alist t))
               (buffer-name))))
    (imenu--create-keymap
     (car menu)
     (cdr menu)
     'imenu--menubar-select)))

(defvar pdf-misc-tool-bar-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "left-arrow" 'pdf-history-backward
                         'history-backward
                         map
                         :enable '(not (pdf-history-end-of-history-p))
                         :visible '(featurep 'pdf-history)
                         :help "Go backward in history"
                         :label "Backward"
                         :vert-only t)
    
    (tool-bar-local-item "right-arrow" 'pdf-history-forward
                         'history-forward
                         map
                         :enable '(not (pdf-history-beginning-of-history-p))
                         :visible '(featurep 'pdf-history)
                         :help "Go forward in history"
                         :label "Forward"
                         :vert-only t)
    
    (define-key-after map [separator-1] menu-bar-separator)

    (tool-bar-local-item "prev-node" 'doc-view-previous-page
                         'previous-page
                         map
                         :enable '(not (eq 1 (doc-view-current-page)))
                         :help "Go to previous page"
                         :label "Page back")
    (tool-bar-local-item "next-node" 'doc-view-next-page
                         'next-page
                         map
                         :enable '(not (eq  (doc-view-last-page-number)
                                            (doc-view-current-page)))
                         :help "Go to next page"
                         :label "Page forward")
    ;; (tool-bar-local-item-from-menu 'Info-up "up-node" map Info-mode-map
    ;;     			   :vert-only t)
    (define-key-after map [separator-2] menu-bar-separator)
    ;; (tool-bar-local-item-from-menu 'Info-top-node "home" map Info-mode-map
    ;;     			   :vert-only t)
    ;; (tool-bar-local-item-from-menu 'Info-goto-node "jump-to" map Info-mode-map)
    ;; (define-key-after map [separator-3] menu-bar-separator)
    (tool-bar-local-item "index"
                         (lambda nil (interactive)
                                    (popup-menu (pdf-misc-imenu-menu-map)))
                         'imenu
                         map)
    ;; (tool-bar-local-item-from-menu 'Info-search "search" map Info-mode-map
    ;;     			   :vert-only t)
    ;; (tool-bar-local-item-from-menu 'Info-exit "exit" map Info-mode-map
    ;;     			   :vert-only t)
    map)
  "The keymap used in `pdf-misc-tool-bar-minor-mode'.")

(define-minor-mode pdf-misc-tool-bar-minor-mode
  "Display a local PDF tool-bar."
  nil nil t
  (pdf-util-assert-pdf-buffer)
  (set (make-local-variable 'tool-bar-map)
       pdf-misc-tool-bar-minor-mode-map))

(provide 'pdf-misc)

;;; pdf-misc.el ends here
