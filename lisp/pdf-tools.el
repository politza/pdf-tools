;;; pdf-tools.el --- Support library for PDF documents. -*- lexical-binding:t -*-

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
;; See README.org

(require 'pdf-view)
(require 'pdf-util)
(require 'cus-edit)

;;; Code:



;; * ================================================================== *
;; * Customizables
;; * ================================================================== *

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
    pdf-annot-minor-mode
    pdf-sync-minor-mode
    pdf-misc-context-menu-minor-mode
    pdf-cache-prefetch-minor-mode))
    
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

(defconst pdf-tools-auto-mode-alist-entry
  '("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  "The entry to use for `auto-mode-alist'.")
  
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
        (rename-uniquely)))
    (customize-group 'pdf-tools-faces)
    (with-current-buffer buffer
      (set (make-local-variable 'custom-face-default-form) 'all))))


;; * ================================================================== *
;; * Initialization
;; * ================================================================== *


(defun pdf-tools-pdf-buffer-p (&optional buffer)
  "Return non-nil if BUFFER contains a PDF document."
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char 1)
        (looking-at "%PDF")))))

(defun pdf-tools-assert-pdf-buffer (&optional buffer)
  (unless (pdf-tools-pdf-buffer-p buffer)
    (error "Buffer does not contain a PDF document")))
  
(defun pdf-tools-set-modes-enabled (enable &optional modes)
  (dolist (m (or modes pdf-tools-enabled-modes))
    (let ((enabled-p (and (boundp m)
                          (symbol-value m))))
      (unless (or (and enabled-p enable)
                  (and (not enabled-p) (not enable)))
        (funcall m (if enable 1 -1))))))

;;;###autoload
(defun pdf-tools-enable-minor-modes (&optional modes)
  "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (pdf-tools-set-modes-enabled t modes)
  (run-hooks 'pdf-tools-enabled-hook))

(defun pdf-tools-disable-minor-modes (&optional modes)
  "Disable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-tools-set-modes-enabled nil modes))

;;;###autoload
(defun pdf-tools-install ()
  "Install PDF-Tools in all current and future PDF buffers.

See `pdf-view-mode' and `pdf-tools-enabled-modes'."
  (interactive)
  (add-to-list 'auto-mode-alist pdf-tools-auto-mode-alist-entry)
  (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (pdf-tools-pdf-buffer-p)
                 (buffer-file-name))
        (pdf-view-mode)))))

(defun pdf-tools-uninstall ()
  "Uninstall PDF-Tools in all current and future PDF buffers."
  (interactive)
  (setq-default auto-mode-alist
    (remove pdf-tools-auto-mode-alist-entry auto-mode-alist))
  (remove-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (pdf-util-pdf-buffer-p buf)
        (pdf-tools-disable-minor-modes pdf-tools-modes)
        (normal-mode)))))

;;;###autoload
(defun pdf-tools-help ()
  (interactive)
  (help-setup-xref (list #'pdf-tools-help)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ "PDF Tools Help\n\n")
    (princ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    (dolist (m (cons 'pdf-view-mode
                     (sort (copy-sequence pdf-tools-modes) 'string<)))
      (princ (format "`%s' is " m))
      (describe-function-1 m)
      (terpri) (terpri)
      (princ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"))))


;; * ================================================================== *
;; * Debugging
;; * ================================================================== *

(defvar pdf-tools-debug nil
  "Non-nil, if debugging PDF Tools.")

(defun pdf-tools-toggle-debug ()
  (interactive)
  (setq pdf-tools-debug (not pdf-tools-debug))
  (when (called-interactively-p 'any)
    (message "Toggled debugging %s" (if pdf-tools-debug "on" "off"))))

(provide 'pdf-tools)

;;; pdf-tools.el ends here
