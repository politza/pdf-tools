;;; pdf-tools.el --- Support library for PDF documents. -*- lexical-binding:t -*-

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
;; * Installation:
;;
;; Assuming the epdfinfo server build correctly, a Emacs package was
;; created and installed.
;;
;; Put (pdf-tools-install) in your .emacs, that's it.
;;
;; * Usage (in a nutshell):
;;
;; + Isearch
;;
;; Press C-s to enter isearch, try it.  While in isearch mode, C-v and
;; C-M-v turn pages, C-b enters batch mode (try it), C-d enters dark
;; mode (try it).
;;
;; + Outline
;;
;; Press o to enter the outline buffer, assuming the PDF has one.  Or
;; use the menu-bar item 'PDF Outline', the tool bar or the imenu
;; command.
;;
;; + Links and the History
;;
;; After DocView has converted the document, you should start to see
;; links underlined by some red pixel. (This is purely a visual cue.)
;; Click on such a link to activate it, or press f and select it by
;; the keyboard.  Then press B to return to the original page and N to
;; again move to the page of the just followed link.
;; 
;; + Miscellanous
;;
;; Drag the mouse over a region, this kills the selected text.  Press
;; C-w to copy the whole page.  M-s o invokes pdf-occur (but no
;; regexp). I displays some metainformation about the document, s p
;; and s w optimize the display to the page, respectively
;; window-width. C-c C-d enters dark mode, which influences the color
;; choices made (but not retroactively).
;; 

(require 'doc-view)
(require 'pdf-util)
(require 'cus-edit)

;;; Code:

(defgroup pdf-tools nil
  "Support library for PDF documents."
  :group 'doc-view)

(defgroup pdf-tools-faces nil
  "Faces determining the colors used in the pdf-tools package.

In order to customize dark and light colors use
`pdf-tools-customize-faces', or set `custom-face-default-form' to
'all."
  :group 'pdf-tools)

(defconst pdf-tools-modes
  '(pdf-history-minor-mode
    pdf-isearch-minor-mode
    pdf-links-minor-mode
    pdf-misc-minor-mode
    pdf-outline-minor-mode
    pdf-misc-size-indication-minor-mode
    pdf-misc-menu-bar-minor-mode
    pdf-misc-tool-bar-minor-mode
    pdf-annot-minor-mode
    pdf-sync-minor-mode
    pdf-info-auto-revert-minor-mode))
    
(defcustom pdf-tools-enabled-modes pdf-tools-modes 
  "A list of automatically enabled minor-modes.

PDF Tools is build as a series of minor-modes.  This variable and
the function `pdf-tools-install' merely serve as a convenient
wrapper in order to load these modes in current and newly created
PDF buffers."
  :group 'pdf-tools
  :type `(set ,@(mapcar (lambda (mode)
                          `(function-item ,mode))
                        pdf-tools-modes)))

(defcustom pdf-tools-enabled-hook nil
  "A hook ran after PDF Tools is enabled in a buffer."
  :group 'pdf-tools
  :type 'hook)

(defun pdf-tools-set-modes-enabled (enabled-p &optional modes)
  (dolist (m (or modes pdf-tools-enabled-modes))
    (funcall m (if enabled-p 1 -1))))

;;;###autoload
(defun pdf-tools-enable (&optional modes)
  "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-tools-set-modes-enabled t modes)
  (run-hooks 'pdf-tools-enabled-hook))

(defun pdf-tools-disable (&optional modes)
  "Disable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-tools-set-modes-enabled nil modes))

(defun pdf-tools-enable-maybe (&optional modes)
  "Maybe enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'.

Does nothing, if current buffer is not in DocViewPDF Mode."
  (interactive)
  (when (pdf-util-pdf-buffer-p)
    (pdf-tools-enable modes)))

;;;###autoload
(defun pdf-tools-install ()
  "Install PdfTools in all current and future PDF buffers.

See `pdf-tools-enabled-modes'."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (pdf-util-pdf-buffer-p buf)
        (pdf-tools-enable))))
  (add-hook 'doc-view-mode-hook 'pdf-tools-enable-maybe))

(defun pdf-tools-uninstall ()
  "Uninstall PdfTools in all current and future PDF buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (pdf-util-pdf-buffer-p buf)
        (pdf-tools-disable pdf-tools-modes))))
  (remove-hook 'doc-view-mode-hook 'pdf-tools-enable))

(defun pdf-tools-customize ()
  "Customize Pdf Tools."
  (interactive)
  (customize-group 'pdf-tools))

(defun pdf-tools-customize-faces ()
  "Customize PDF Tool's faces."
  (interactive)
  (let ((buffer (format "*Customize Group: %s*"
                        (custom-unlispify-tag-name 'pdf-tools-faces))))
    (when (buffer-live-p (get-buffer buffer))
      (with-current-buffer (get-buffer buffer)
        (rename-uniquely )))
    (customize-group 'pdf-tools-faces)
    (with-current-buffer buffer
      (set (make-local-variable 'custom-face-default-form) 'all))))

(provide 'pdf-tools)

;;; pdf-tools.el ends here
