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



(defvar pdf-misc-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    
    kmap)
  "Keymap used in `pdf-misc-minor-mode'.")

;;;###autoload
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

(defvar pdf-misc-context-menu-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [down-mouse-3] 'pdf-misc-popup-context-menu)
    kmap))

(define-minor-mode pdf-misc-context-menu-minor-mode
  "Provide a right-click context menu in PDF buffers.

\\{pdf-misc-context-menu-minor-mode-map}"
  nil nil nil
  (pdf-util-assert-pdf-buffer))

(defun pdf-misc-popup-context-menu (event)
  "Popup a context menu at position determined by EVENT."
  (interactive "@e")
  (popup-menu
   (lookup-key pdf-misc-menu-bar-minor-mode-map
               [menu-bar pdf-tools])))

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

(provide 'pdf-misc)

;;; pdf-misc.el ends here
