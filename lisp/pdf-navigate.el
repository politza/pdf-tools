;;; pdf-navigate.el --- View & navigate the pdf history stack

;; Filename: pdf-navigate.el
;; Description: View & navigate the pdf history stack
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-06-10 20:17:55
;; Version: 0.1
;; Last-Updated: 2013-06-10 20:17:55
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/pdf-tools
;; Keywords: files, wp
;; Compatibility: GNU Emacs 24.3.1
;;
;; Features that might be required by this library:
;;
;; pdf-tools
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Assuming pdf-history-minor-mode is enabled type M-x pdf-navigate to display
;; the `pdf-history-stack' in another window. Then use the up/down arrow keys
;; to navigate the list, RET to visit an item and C-o to display it in the other
;; window.
;; If you have fm.el installed (available from Marmalade or here: https://github.com/vapniks/fm)
;; you can press f to toggle fm-mode which will then update the other window each time you move
;; to another item.
;;
;;;;


;;; Installation:
;;
;; Put pdf-navigate.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'pdf-navigate)

;;; Customize:
;;
;; `pdf-navigate-window-size' : the number of lines to display in the *PDF-History* buffer when fm-mode is active.

;;
;; All of the above can customized by:
;;      M-x customize-group RET pdf-navigate RET
;;

;;; Change log:
;;	
;; 2013/06/10
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require


;;; Code:

(defgroup pdf-navigate nil
  "Commands for viewing and navigating the `pdf-history-stack'."
  :group 'pdf-tools)

(defmacro pdf-navigate-key-cmd (cmd)
  "Call command CMD interactively in the associated pdf file buffer."
  `(lambda nil (interactive)
    (with-selected-window
        (get-buffer-window pdf-navigate-source-buffer)
      (call-interactively ',cmd))))

(defvar pdf-navigate-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "RET") 'pdf-navigate-goto-item)
    (define-key kmap (kbd "C-o") 'pdf-navigate-view-item)
    (define-key kmap (kbd "<down>") 'pdf-navigate-next-item)
    (define-key kmap (kbd "<up>") 'pdf-navigate-previous-item)
    (define-key kmap (kbd "<C-down>") (pdf-navigate-key-cmd doc-view-next-line-or-next-page))
    (define-key kmap (kbd "<C-up>") (pdf-navigate-key-cmd doc-view-previous-line-or-previous-page))
    (define-key kmap (kbd "C-<") (pdf-navigate-key-cmd image-bob))
    (define-key kmap (kbd "C->") (pdf-navigate-key-cmd image-eob))
    (define-key kmap (kbd "<C-prior>") (pdf-navigate-key-cmd doc-view-previous-page))
    (define-key kmap (kbd "<C-next>") (pdf-navigate-key-cmd doc-view-next-page))
    (define-key kmap (kbd "C-M-<") (pdf-navigate-key-cmd doc-view-first-page))
    (define-key kmap (kbd "C-M->") (pdf-navigate-key-cmd doc-view-last-page))
    (define-key kmap (kbd "C-w") (pdf-navigate-key-cmd pdf-misc-copy-page))
    kmap)
  "The keymap used for `pdf-navigate-mode'.")

(define-key pdf-history-minor-mode-map (kbd "C-S-n") 'pdf-navigate)

(defvar pdf-navigate-source-buffer nil
  "The buffer associated with this *PDF-History* buffer")
(make-variable-buffer-local 'pdf-navigate-source-buffer)

(defcustom pdf-navigate-window-size 5
  "The size (in lines) of the *PDF-History* window."
  :group 'pdf-navigate
  :type 'integer)

(define-derived-mode pdf-navigate-mode tabulated-list-mode "PDFNavigate"
  "Major mode for output from \\[pdf-navigate-show-history].
\\<pdf-navigate-mode-map>Move point to one of the items in this buffer, then use
\\[pdf-navigate-goto-item] to go to the page that the item refers to, or
\\[pdf-navigate-view-item] to view the page in the window currently displaying the pdf file.

\\{pdf-navigate-mode-map}"
  (make-local-variable 'mark-buffer)
  (toggle-read-only 1))

;;;###autoload
(defun pdf-navigate (buffer)
  "Display the `pdf-history-stack' in the *PDF-History* buffer."
  (interactive (list (current-buffer)))
  (let ((filename (buffer-file-name buffer))
        (page-cmp
         (lambda (e1 e2)
           (let ((p1 (string-to-number
                      (aref (cadr e1) 0)))
                 (p2 (string-to-number
                      (aref (cadr e2) 0))))
             (<= p1 p2)))))
    (with-current-buffer (get-buffer-create
                          (concat "*PDF-History:"
                                  (file-name-nondirectory filename)
                                  "*"))
      (let ((inhibit-read-only t))
        (erase-buffer))
      (pdf-navigate-mode)
      (setq pdf-navigate-source-buffer buffer)
      (setq tabulated-list-format
            (apply 'vector
                   `(("Page" 4 ,page-cmp :right-align t)
                     ("" 0 t))))
      (setq tabulated-list-entries
            (pdf-navigate-create-entries buffer)
            tabulated-list-sort-key (cons "Page" nil))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (setq next-error-last-buffer (current-buffer))
      (pop-to-buffer (current-buffer))
      ;; TODO - make this robust
      (shrink-window (- (window-body-height) pdf-navigate-window-size))
      (setq fm-window-lines pdf-navigate-window-size))))

(defun pdf-navigate-create-entries (buffer)
  (let (entries)
    (dolist (h (with-current-buffer buffer pdf-history-stack))
      (let* ((page (car h))
             (page-str (propertize (format "%d" page)
                                   'face
                                   (list :background "red"
                                         :foreground "black")))
             (page-txt (pdf-info-gettext page 0 0 1 0.1 buffer)))
        (push (list (list buffer page)
                    (vector page-str page-txt))
              entries)))
    entries))

(defun pdf-navigate-next-item nil
  "Move to the next item in the *PDF-History* buffer."
  (interactive)
  (goto-char (or (next-single-property-change
                  (point) 'tabulated-list-id)
                 (point))))

(defun pdf-navigate-previous-item nil
  "Move to the next item in the *PDF-History* buffer."
  (interactive)
  (goto-char (or (previous-single-property-change
                  (point) 'tabulated-list-id)
                 (point-min))))

(defun pdf-navigate-goto-item (&optional event)
  "Go to the occurrence at EVENT.

If EVENT is nil, use occurrence at current line."
  (interactive (list last-nonmenu-event))
  (let ((link
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (tabulated-list-get-id)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (tabulated-list-get-id))))))
    (when link
      (switch-to-buffer (car link))
      (delete-other-windows)
      (doc-view-goto-page (cadr link)))))

(defun pdf-navigate-view-item (&optional event)
  "Go to the occurrence at EVENT.

If EVENT is nil, use occurrence at current line."
  (interactive (list last-nonmenu-event))
  (let ((link
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (tabulated-list-get-id)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (tabulated-list-get-id))))))
    (when link
      (with-selected-window (display-buffer (car link))
        (doc-view-goto-page (cadr link))))))

;; Add support for follow-mode (fm.el)
(unless (not (featurep 'fm))
  (add-to-list 'fm-modes '(pdf-navigate-mode pdf-navigate-goto-item))
  (add-hook 'pdf-navigate-mode-hook 'fm-start))

(provide 'pdf-navigate)

;; (magit-push)


;;; pdf-navigate.el ends here

