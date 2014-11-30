;;; pdf-sync.el --- Use synctex to correlate LaTeX-Sources with PDF positions. -*- lexical-binding:t -*-
;; Copyright (C) 2013, 2014  Andreas Politz

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

(require 'pdf-view)
(require 'pdf-info)

;;; Code:

(defgroup pdf-sync nil
  "Jump from TeX sources to PDF pages and back."
  :group 'pdf-tools)
  
(defcustom pdf-sync-tex-display-pdf-key "C-c C-g"
  "Key to jump from a TeX buffer to it's PDF file.

This key is added to `TeX-source-correlate-method', when
`pdf-sync-minor-mode' is activated and this map is defined."
  :group 'pdf-sync
  :type 'key-sequence)

(defcustom pdf-sync-goto-tex-hook nil
  "Hook ran after going to a source location.

The hook is run in the TeX buffer."
  :group 'pdf-sync
  :type 'hook)

(defcustom pdf-sync-display-pdf-hook nil
  "Hook ran after displaying the PDF buffer.

The hook is run in the PDF's buffer."
  :group 'pdf-sync
  :type 'hook)

(defcustom pdf-sync-display-pdf-action nil
  "Display action used when displaying PDF buffers."
  :group 'pdf-sync
  :type 'display-buffer--action-custom-type)

(defvar pdf-sync-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [double-mouse-1] 'pdf-sync-mouse-goto-tex)
    kmap))

;;;###autoload
(define-minor-mode pdf-sync-minor-mode
  "Correlate a PDF position with the TeX file.
\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to 'synctex \(before AUCTeX
is loaded\) and enabling `TeX-source-correlate-mode'.

Then \\[pdf-sync-mouse-goto-tex] in the PDF buffer will open the
corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-tex-display-pdf-key' \(the default is `C-c C-g'\)
to `pdf-sync-display-pdf' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX."

  nil nil nil
  (pdf-util-assert-pdf-buffer))

(eval-after-load "tex"
  '(when (and pdf-sync-tex-display-pdf-key
              (boundp 'TeX-source-correlate-map)
              (let ((key (lookup-key
                          TeX-source-correlate-map
                          (kbd pdf-sync-tex-display-pdf-key))))
                (or (null key)
                    (numberp key))))
     (define-key TeX-source-correlate-map
       (kbd pdf-sync-tex-display-pdf-key)
       'pdf-sync-display-pdf)))
  
(defun pdf-sync-mouse-goto-tex (ev)
  "Go to the source corresponding to position at event EV."
  (interactive "@e")
  (let* ((posn (event-start ev))
         (image (posn-image posn))
         (xy (posn-object-x-y posn)))
    (unless image
      (error "Outside of image area"))
    (pdf-sync-goto-tex (car xy) (cdr xy))))
  
(defun pdf-sync-goto-tex (x y)
  "Go to the source corresponding to image coordinates X, Y."
  (cl-destructuring-bind (source line column)
      (pdf-sync-correlate-tex x y)
    (pop-to-buffer (or (find-buffer-visiting source)
                       (find-file-noselect source)))
    (push-mark)
    (when (> line 0)
      (save-restriction
        (widen)
        (goto-char 1)
        (forward-line (1- line))))
    (when (> column 0)
      (forward-char (1- column)))
    (run-hooks 'pdf-sync-goto-tex-hook)))

(defun pdf-sync-correlate-tex (x y)
  "Find the source corresponding to image coordinates X, Y.

Returns a list \(SOURCE LINE COLUMN\)."

  (pdf-util-assert-pdf-window)
  (let ((size (pdf-view-image-size))
        (page (pdf-view-current-page)))
    (setq x (/ x (float (car size)))
          y (/ y (float (cdr size))))
    (cl-destructuring-bind (source line column)
        (pdf-info-synctex-backward-search page x y)
      (list (expand-file-name source)
            line column))))

(defun pdf-sync-display-pdf (&optional line column)
  "Display the PDF location corresponding to LINE, COLUMN."
  (interactive)
  (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
      (pdf-sync-correlate-pdf line column)
    (let ((buffer (or (find-buffer-visiting pdf)
                      (find-file-noselect pdf))))
      (with-selected-window (display-buffer
                             buffer pdf-sync-display-pdf-action)
        (pdf-util-assert-pdf-window)
        (pdf-view-goto-page page)
        (let ((top (* y1 (cdr (pdf-view-image-size)))))
          (pdf-util-tooltip-arrow (round top))))
      (with-current-buffer buffer
        (run-hooks 'pdf-sync-display-pdf-hook)))))

(defun pdf-sync-correlate-pdf (&optional line column)
  "Find the PDF location corresponding to LINE, COLUMN.

Returns a list \(PDF PAGE X1 Y1 X2 Y2\)."
  (unless (fboundp 'TeX-master-file)
    (error "This function works only with AUCTeX"))
  (save-restriction
    (widen)
    (unless line (setq line (line-number-at-pos)))
    (unless column (setq column (current-column))))

  (let ((pdf (expand-file-name
              (with-no-warnings (TeX-master-file "pdf"))))) 
    (condition-case nil
        (cons pdf
              (pdf-info-synctex-forward-search
               (buffer-file-name) line column pdf))
      (error
       ;; Work around quirky filenames in synctex.gz database.
       (let ((master-directory
              (file-name-as-directory
               (expand-file-name
                (with-no-warnings (TeX-master-directory))))))
         (cons pdf
               (pdf-info-synctex-forward-search
                (concat master-directory "./"
                        (file-relative-name
                         (buffer-file-name)
                         master-directory))
                line column pdf)))))))

(provide 'pdf-sync)

;;; pdf-sync.el ends here
