;;; pdf-misc.el --- Miscellanous commands for PDF buffer.

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

(require 'pdf-view)
(require 'pdf-util)
(require 'imenu)



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defface pdf-misc-region
  '((((background dark)) (:inherit region))
    (((background light)) (:inherit region)))
  "Face used to determine the colors of the region."
  :group 'pdf-misc
  :group 'pdf-tools-faces)

(defcustom pdf-misc-multipage-horizontally nil
  "If non-nil, windows are sorted left to right first."
  :group 'pdf-misc)


;; * ================================================================== *
;; * Local variables
;; * ================================================================== *

(defvar-local pdf-misc-active-region nil
  "The active region \(LEFT TOP RIGHT BOTTOM\) or nil.")




;; * ================================================================== *
;; * Modes
;; * ================================================================== *


(defvar pdf-misc-text-region-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [t] 'pdf-util-image-map-mouse-event-proxy)
    km)
  "Keymap used over pdf-misc-text-region hotspots.")

(defvar pdf-misc-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [down-mouse-1] 'pdf-misc-mouse-set-region)
    (define-key kmap [remap kill-region] 'pdf-misc-kill-ring-save)
    (define-key kmap [remap kill-ring-save] 'pdf-misc-kill-ring-save)
    (define-key kmap [remap mark-whole-buffer] 'pdf-misc-mark-whole-page)
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
  nil nil nil
  (cond
   (pdf-misc-minor-mode
    (add-hook 'deactivate-mark-hook 'pdf-misc-deactivate-region nil t)
    (add-hook 'pdf-view-after-change-page-hook 'pdf-misc-deactivate-region nil t)
    (pdf-view-add-hotspot-function 'pdf-misc-text-regions-hotspots-function -9)
    (local-set-key [pdf-misc-text-region] pdf-misc-text-region-keymap))
   (t
    (pdf-misc-deactivate-region)
    (remove-hook 'deactivate-mark-hook 'pdf-misc-deactivate-region t)
    (remove-hook 'pdf-view-after-change-page-hook 'pdf-misc-deactivate-region t)
    (pdf-view-remove-hotspot-function 'pdf-misc-text-regions-hotspots-function))))


(defvar pdf-misc-multipage-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (set-keymap-parent kmap pdf-view-mode-map)
    (define-key kmap [remap pdf-view-next-page-command]
      'pdf-misc-multipage-next-page)
    (define-key kmap [remap pdf-view-previous-page-command]
      'pdf-misc-multipage-previous-page)
    kmap))
  
(define-minor-mode pdf-misc-multipage-minor-mode
  "View and navigate multiple pages in all windows.

FIXME: Not ready for prime-time."
  nil nil nil
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-misc-multipage-minor-mode
    (pdf-misc-multipage-next-page 0))))

(define-minor-mode pdf-misc-size-indication-minor-mode
  "Provide a working size indication in the mode-line."
  nil nil nil
  (pdf-util-assert-pdf-buffer)
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

(defvar pdf-misc-menu-bar-minor-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap)))

    (define-key map [menu-bar pdf-tools]
      (cons "PDF Tools" menu))
    
    (define-key menu [customize]
      '(menu-item "Customize" pdf-tools-customize
                  :help "Customize PDF Tools"))

    (define-key menu [sep-0]
      '(menu-item "--" nil
                  :visible (and (bound-and-true-p pdf-annot-minor-mode)
                                (pdf-info-writable-annotations-p))))
    
    (define-key menu [revert-document]
      '(menu-item "Revert all annotations" pdf-annot-revert-document
                  :help "Revert all annotations to their saved state"
                  :visible (and (bound-and-true-p pdf-annot-minor-mode)
                                (pdf-info-writable-annotations-p))))
    
    (define-key menu [revert-page]
      '(menu-item "Revert page's annotations" pdf-annot-revert-page
                  :help "Revert annotations on this page to their saved state"
                  :visible (and (bound-and-true-p pdf-annot-minor-mode)
                                (pdf-info-writable-annotations-p))))

    (define-key menu [sep-1]
      '(menu-item "--" nil
                  :visible (bound-and-true-p pdf-annot-minor-mode)))
    
    (define-key menu [render-links]
      '(menu-item "Render Links" pdf-annot-toggle-display-links
                  :help "Display or undisplay links"
                  :button (:toggle . (memq 'link pdf-annot-rendered-types))
                  :visible (bound-and-true-p pdf-annot-minor-mode)))

    (define-key menu [render-annotations]
      '(menu-item "Render Annotations" pdf-annot-toggle-display-annotations
                  :help "Display or undisplay annotations"
                  :button (:toggle . (memq 'text pdf-annot-rendered-types))
                  :visible (bound-and-true-p pdf-annot-minor-mode)))

    (define-key menu [sep-4] menu-bar-separator)

    (define-key menu [copy-region]
      '(menu-item "Copy region" pdf-misc-kill-ring-save
                  :help "Copy the text of the region to the kill-ring"
                  :visible (featurep 'pdf-misc)))
    
    (define-key menu [occur]
      '(menu-item "Occur Document" pdf-occur
                  :help "Display lines containing a string"
                  :visible (featurep 'pdf-occur)))
    
    (define-key menu [isearch]
      '(menu-item "Isearch Document" isearch-forward
                  :help "Interactively search the document"
                  :visible (featurep 'pdf-isearch)))

    (define-key menu [sep-2] menu-bar-separator)

    (define-key menu [list-annotations]
      '(menu-item "Display Annotations" pdf-annot-list-annotations
                  :help "List all annotations"
                  :visible (bound-and-true-p pdf-annot-minor-mode)))

    (define-key menu [dired-attachments]
      '(menu-item "Display Attachments" pdf-annot-attach-dired
                  :help "Display attachments in a dired buffer"
                  :visible (featurep 'pdf-annot)))

    (define-key menu [metadata]
      '(menu-item "Display Metadata" pdf-misc-display-metadata
                  :help "Display information about the document"
                  :visible (featurep 'pdf-misc)))
    
    (define-key menu [outline]
      '(menu-item "Display Outline" pdf-outline
                  :help "Display documents outline"
                  :visible (featurep 'pdf-outline)))

    ;; Context menu only
    (define-key menu [sep-5]
      '(menu-item "--" nil
        :visible (equal last-command-event
                        last-nonmenu-event)))
    
    ;; Context menu only
    (define-key menu [locate-source]
      '(menu-item "Locate TeX source" pdf-sync-mouse-goto-tex
                  :help "Open the TeX source corresponding to this position."
                  :visible (and (featurep 'pdf-sync)
                                (equal last-command-event
                                       last-nonmenu-event))))
    
    ;; Context menu only
    (define-key menu [add-text-annotation]
      '(menu-item "Add text annotation" pdf-annot-add-text-annot-at-event
                  :help "Add a new text annotation"
                  :keys "\\[pdf-annot-add-text-annot]"
                  :visible (and (bound-and-true-p pdf-annot-minor-mode)
                                (pdf-info-writable-annotations-p)
                                (equal last-command-event
                                       last-nonmenu-event))))
    
    (define-key menu [sep-6] menu-bar-separator)
    
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

    (tool-bar-local-item "prev-node" 'pdf-view-previous-page
                         'previous-page
                         map
                         :enable '(not (eq 1 (pdf-view-current-page)))
                         :help "Go to previous page"
                         :label "Page back")
    (tool-bar-local-item "next-node" 'pdf-view-next-page
                         'next-page
                         map
                         :enable '(not (eq  (pdf-view-last-page-number)
                                            (pdf-view-current-page)))
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


(defvar pdf-misc-context-menu-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [down-mouse-3] 'pdf-misc-popup-context-menu)
    kmap))

(define-minor-mode pdf-misc-context-menu-minor-mode
  "Provide a context menu.

\\{pdf-misc-context-menu-minor-mode-map}"
  nil nil nil
  (pdf-util-assert-pdf-buffer))

(define-minor-mode pdf-misc-dark-mode
  "Mode for documents with dark background.

This tells `pdf-isearch-minor-mode' to use dark colors."
  nil nil nil
  (pdf-util-assert-pdf-buffer)
  ;; FIXME: This should really be run in a hook.
  (when (bound-and-true-p pdf-isearch-active-mode)
    (with-no-warnings
      (pdf-isearch-redisplay)
      (pdf-isearch-message
       (if pdf-misc-dark-mode "dark mode" "light mode")))))


;; * ================================================================== *
;; * Region
;; * ================================================================== *

(defmacro pdf-misc-assert-active-region ()
  `(unless pdf-misc-active-region
     (error "The region is not active")))

(defun pdf-misc-deactivate-region ()
  "Deactivate the region."
  (interactive)
  (when pdf-misc-active-region
    (setq pdf-misc-active-region nil)
    (deactivate-mark)
    (pdf-view-redisplay)))

(defun pdf-misc-mouse-set-region (ev)
  "Selects a region of text using the mouse."
  (interactive "@e")
  (unless (and (eventp ev)
               (mouse-event-p ev))
    (signal 'wrong-type-argument (list 'mouse-event-p ev)))
  (pdf-misc-deactivate-region)
  (unless (posn-image (event-start ev))
    (error "No page at this position"))
  (let* ((window (selected-window))
         (beg (posn-object-x-y (event-start ev)))
         (colors (pdf-util-face-colors 'pdf-misc-region
                                       pdf-misc-dark-mode))
         (width (car (pdf-view-image-size)))
         (page (pdf-view-current-page)))
    (pdf-util-doevents (event 0.1)
        (mouse-movement-p event)
      (let* ((pos (event-start event))
             (end (posn-object-x-y pos)))
        (when (and (eq window (posn-window pos))
                   (posn-image pos))
          (setq pdf-misc-active-region
                (list (min (car beg) (car end))
                      (min (cdr beg) (cdr end))
                      (max (car beg) (car end))
                      (max (cdr beg) (cdr end))))
          (pdf-misc-display-active-region page width colors)
          (pdf-util-display-edges pdf-misc-active-region))))
    (when pdf-misc-active-region
      (let ((transient-mark-mode t))
        (push-mark)))))

(defun pdf-misc-display-active-region (&optional page image-width colors)
  (pdf-misc-assert-active-region)
  (unless colors
    (setq colors (pdf-util-face-colors 'pdf-misc-region
                                       pdf-misc-dark-mode)))
  (pdf-view-display-image
   (pdf-view-create-image
       (pdf-info-renderpage-text-regions
        (or page (pdf-view-current-page))
        (or image-width (car (pdf-view-image-size)))
        nil nil `(,(car colors)
                  ,(cdr colors)
                  ,(pdf-util-scale-pixel-to-relative
                    pdf-misc-active-region))))))
    
(defun pdf-misc-kill-ring-save ()
  "Copy the region to the `kill-ring'."
  (interactive)
  (pdf-util-assert-pdf-window)
  (if (null pdf-misc-active-region)
      (error "The region is not active")
    (let* ((txt (apply 'pdf-misc-selection-string
                       pdf-misc-active-region)))
      (pdf-misc-deactivate-region)
      (kill-new txt))))

(defun pdf-misc-mark-whole-page ()
  "Copy the whole page to the `kill-ring'."
  (interactive)
  (pdf-util-assert-pdf-window)
  (let ((size (pdf-view-image-size)))
    (setq pdf-misc-active-region
          (list 0 0 (car size) (cdr size))))
  (let ((transient-mark-mode t))
    (push-mark))
  (pdf-misc-display-active-region))

(defun pdf-misc-selection-string (x0 y0 x1 y1 &optional page)
  "Return the text of the selection X0 Y0 X1 Y1 on PAGE."
  (pdf-util-assert-pdf-buffer)
  (let* ((size (pdf-view-image-size))
         (x0 (/ x0 (float (car size))))
         (y0 (/ y0 (float (cdr size))))
         (x1 (/ x1 (float (car size))))
         (y1 (/ y1 (float (cdr size)))))
    (pdf-info-gettext
     (or page (pdf-view-current-page))
     x0 y0 x1 y1)))

(defun pdf-misc-text-regions-hotspots-function (page size)
  "Return a list of hotspots for text regions on PAGE using SIZE."
  (mapcar (lambda (region)
            (let ((e (pdf-util-scale-edges region size)))
              `((rect . ((,(nth 0 e) . ,(nth 1 e))
                         . (,(nth 2 e) . ,(nth 3 e))))
                pdf-misc-text-region
                (pointer text))))
          (pdf-cache-getselection page 0 0 1 1)))


;; * ================================================================== *
;; * Multipage windows
;; * ================================================================== *

(defun pdf-misc-multipage-get-windows (&optional horizontally-p)
  (sort (get-buffer-window-list (current-buffer))
        (lambda (w1 w2)
          (let* ((e1 (window-edges w1))
                 (e2 (window-edges w2))
                 (l1 (car e1)) (t1 (cadr e1))
                 (l2 (car e2)) (t2 (cadr e2)))
            (if horizontally-p
                (or (< t1 t2)
                    (and (= t1 t2)
                         (< l1 l2)))
              (or (< l1 l2)
                    (and (= l1 l2)
                         (< t1 t2))))))))
                
(defun pdf-misc-multipage-next-page (&optional arg)
  (interactive "p")
  (let* ((windows (pdf-misc-multipage-get-windows
                   pdf-misc-multipage-horizontally))
         (selwin (selected-window))
         (pos (cl-position selwin windows))
         (page (pdf-view-current-page))
         (npages (pdf-info-number-of-pages)))
    (cond
     ((> arg 0)
      (setq arg (min arg (- npages
                            (+ page (- (length windows) pos 1)))))
      (when (<= arg 0)
        (error "End of document"))
      ;; Set page of first window.
      (setq page (- (+ page arg) pos)))
     ((< arg 0)
      (setq arg (max arg (- (- page pos 1))))
      (when (>= arg 0)
        (error "Beginning of document"))
      (setq page (- (+ page arg) pos)))
     (t
      ;; Adjust pages in this case, starting at the first window.
      (setq page (max 1
                      (min (with-selected-window (car windows)
                             (pdf-view-current-page))
                           (- npages (1- (length windows))))))))
    (dolist (w windows)
      (with-selected-window w
        (pdf-view-goto-page page)
        (setq page (min (1+ page) npages))))))

(defun pdf-misc-multipage-adjust-pages ()
  (interactive)
  (pdf-misc-multipage-next-page 0))

(defun pdf-misc-multipage-previous-page (&optional arg)
  (interactive "p")
  (pdf-misc-multipage-next-page (- (or arg 1))))
  



;; * ================================================================== *
;; * Various
;; * ================================================================== *

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

(defun pdf-misc-size-indication ()
  "Return size indication string for the mode-line."
  (let ((top (= (window-vscroll nil t) 0))
        (bot (>= (+ (- (nth 3 (window-inside-pixel-edges))
                       (nth 1 (window-inside-pixel-edges)))
                    (window-vscroll nil t))
                 (cdr (pdf-view-image-size t)))))
    (cond
     ((and top bot) " All")
     (top " Top")
     (bot " Bot")
     (t (format
         " %d%%%%"
         (ceiling
          (* 100 (/ (float (window-vscroll nil t))
                    (cdr (pdf-view-image-size t))))))))))

(defun pdf-misc-popup-context-menu (event)
  "Popup a context menu at position determined by EVENT."
  (interactive "@e")
  (popup-menu
   (lookup-key pdf-misc-menu-bar-minor-mode-map
               [menu-bar pdf-tools])))

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


(provide 'pdf-misc)

;;; pdf-misc.el ends here
