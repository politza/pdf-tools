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
(require 'pdf-util)

;;; Code:

(defgroup pdf-sync nil
  "Jump from TeX sources to PDF pages and back."
  :group 'pdf-tools)
  
(defcustom pdf-sync-forward-display-pdf-key "C-c C-g"
  "Key to jump from a TeX buffer to it's PDF file.

This key is added to `TeX-source-correlate-method', when
command `pdf-sync-minor-mode' is activated and this map is defined."
  :group 'pdf-sync
  :type 'key-sequence)

(defcustom pdf-sync-forward-hook nil
  "Hook ran after going to a source location.

The hook is run in the TeX buffer."
  :group 'pdf-sync
  :type 'hook)

(defcustom pdf-sync-backward-hook nil
  "Hook ran after displaying the PDF buffer.

The hook is run in the PDF's buffer."
  :group 'pdf-sync
  :type 'hook)

(defcustom pdf-sync-forward-display-action nil
  "Display action used when displaying PDF buffers."
  :group 'pdf-sync
  :type 'display-buffer--action-custom-type)

(defcustom pdf-sync-backward-display-action nil
  "Display action used when displaying TeX buffers."
  :group 'pdf-sync
  :type 'display-buffer--action-custom-type)

(defcustom pdf-sync-locate-synctex-file-functions nil
  "A list of functions for locating the synctex database.

Each function on this hook should accept a single argument: The
absolute path of a PDF file.  It should return the absolute path
of the corresponding synctex database or nil, if it was unable to
locate it."
  :group 'pdf-sync
  :type 'hook)

(defvar pdf-sync-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [double-mouse-1] 'pdf-sync-mouse-goto-tex)
    kmap))

(defcustom pdf-sync-backward-redirect-functions nil
  "List of functions which may redirect a backward search.

Functions on this hook should accept three arguments, namely
SOURCE, LINE and COLUMN, where SOURCE is the absolute filename of
the source file and LINE and COLUMN denote the position in the
file.  COLUMN may be negative, meaning unspecified.

These functions should either return nil, if no redirection is
necessary.  Or a list of the same structure, with some or all (or
none) values modified.

AUCTeX installs a function here which changes the backward search
location for synthetic `TeX-region' files back to the equivalent
position in the original tex file."
  :group 'pdf-sync)


;;;###autoload
(define-minor-mode pdf-sync-minor-mode
  "Correlate a PDF position with the TeX file.
\\<pdf-sync-minor-mode-map>
This works via SyncTeX, which means the TeX sources need to have
been compiled with `--synctex=1'.  In AUCTeX this can be done by
setting `TeX-source-correlate-method' to 'synctex \(before AUCTeX
is loaded\) and enabling `TeX-source-correlate-mode'.

Then \\[pdf-sync-backward-search-mouse] in the PDF buffer will open the
corresponding TeX location.

If AUCTeX is your preferred tex-mode, this library arranges to
bind `pdf-sync-forward-display-pdf-key' \(the default is `C-c C-g'\)
to `pdf-sync-forward-search' in `TeX-source-correlate-map'.  This
function displays the PDF page corresponding to the current
position in the TeX buffer.  This function only works together
with AUCTeX."

  nil nil nil
  (pdf-util-assert-pdf-buffer))

(eval-after-load "tex"
  '(when (and pdf-sync-forward-display-pdf-key
              (boundp 'TeX-source-correlate-map)
              (let ((key (lookup-key
                          TeX-source-correlate-map
                          (kbd pdf-sync-forward-display-pdf-key))))
                (or (null key)
                    (numberp key))))
     (define-key TeX-source-correlate-map
       (kbd pdf-sync-forward-display-pdf-key)
       'pdf-sync-forward-search)))



;; * ================================================================== *
;; * Backward search (PDF -> TeX)
;; * ================================================================== *

(defun pdf-sync-backward-search-mouse (ev)
  "Go to the source corresponding to position at event EV."
  (interactive "@e")
  (let* ((posn (event-start ev))
         (image (posn-image posn))
         (xy (posn-object-x-y posn)))
    (unless image
      (error "Outside of image area"))
    (pdf-sync-backward-search (car xy) (cdr xy))))
  
(defun pdf-sync-backward-search (x y)
  "Go to the source corresponding to image coordinates X, Y."
  (cl-destructuring-bind (source line column)
      (pdf-sync-backward-correlate x y)
    (pop-to-buffer (or (find-buffer-visiting source)
                       (find-file-noselect source))
                   pdf-sync-backward-display-action)
    (push-mark)
    (pdf-util-goto-position line column)
    (run-hooks 'pdf-sync-forward-hook)))

(defun pdf-sync-backward-correlate (x y)
  "Find the source corresponding to image coordinates X, Y.

Returns a list \(SOURCE FN\), where SOURCE is the name of the TeX
file and FN a function of no arguments which, when called in the
buffer of the file, will try to move point to the correct
position."

  (pdf-util-assert-pdf-window)
  (let ((size (pdf-view-image-size))
        (page (pdf-view-current-page)))
    (setq x (/ x (float (car size)))
          y (/ y (float (cdr size))))
    (cl-destructuring-bind (source line column)
        (pdf-info-synctex-backward-search page x y)
      (let ((data (list (expand-file-name source)
                        line column)))
        (or (run-hook-with-args-until-success
             'pdf-sync-backward-redirect-functions data)
            data)))))


;; * ================================================================== *
;; * Forward search (TeX -> PDF)
;; * ================================================================== *

(defun pdf-sync-forward-search (&optional line column)
  "Display the PDF location corresponding to LINE, COLUMN."
  (interactive)
  (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
      (pdf-sync-correlate-pdf line column)
    (let ((buffer (or (find-buffer-visiting pdf)
                      (find-file-noselect pdf))))
      (with-selected-window (display-buffer
                             buffer pdf-sync-forward-display-action)
        (pdf-util-assert-pdf-window)
        (pdf-view-goto-page page)
        (let ((top (* y1 (cdr (pdf-view-image-size)))))
          (pdf-util-tooltip-arrow (round top))))
      (with-current-buffer buffer
        (run-hooks 'pdf-sync-backward-hook)))))

(defun pdf-sync-forward-correlate (&optional line column)
  "Find the PDF location corresponding to LINE, COLUMN.

Returns a list \(PDF PAGE X1 Y1 X2 Y2\)."
  (unless (fboundp 'TeX-master-file)
    (error "This function works only with AUCTeX"))
  (unless line (setq line (line-number-at-pos)))
  (unless column (setq column (current-column)))

  (let* ((pdf (expand-file-name
               (with-no-warnings (TeX-master-file "pdf"))))
         (sfilename (pdf-sync-synctex-file-name
                     (buffer-file-name) pdf)))
    (condition-case err
        (cons pdf
              (pdf-info-synctex-forward-search
               (or sfilename
                   (buffer-file-name))
               line column pdf))
      (error
       (if (null sfilename)
           (signal (car err) (cdr err))
         ;; It would be embarassing, if the unmodified buffer-file-name
         ;; would actually work for some reason.
         (condition-case nil
             (cons pdf
                   (pdf-info-synctex-forward-search
                    (buffer-file-name)
                    line column pdf))
           (error (signal (car err) (cdr err)))))))))



;; * ================================================================== *
;; * Dealing with synctex files.
;; * ================================================================== *

(defun pdf-sync-locate-synctex-file (pdffile)
  "Locate the synctex database corresponding to PDFFILE.

Returns either the absolute path of the database or nil.

See als `pdf-sync-locate-synctex-file-functions'."
  (cl-check-type pdffile string)
  (setq pdffile (expand-file-name pdffile))
  (or (run-hook-with-args-until-success
       'pdf-sync-locate-synctex-file-functions pdffile)
      (pdf-sync-locate-synctex-file-default pdffile)))

(defun pdf-sync-locate-synctex-file-default (pdffile)
  "The default function for locating a synctex database for PDFFILE.

See also `pdf-sync-locate-synctex-file'."
  (let ((default-directory
          (file-name-directory pdffile))
        (basename (file-name-sans-extension
                   (file-name-nondirectory pdffile))))
    (cl-labels ((file-if-exists-p (file)
                  (and (file-exists-p file)
                       file)))
      (or (file-if-exists-p
           (expand-file-name (concat basename ".synctex.gz")))
          (file-if-exists-p
           (expand-file-name (concat basename ".synctex")))
          ;; Some pdftex quote the basename.
          (file-if-exists-p
           (expand-file-name (concat "\"" basename "\"" ".synctex.gz")))
          (file-if-exists-p
           (expand-file-name (concat "\"" basename "\"" ".synctex")))))))

(defun pdf-sync-synctex-file-name (filename pdffile)
  "Find SyncTeX filename corresponding to FILENAME in the context of PDFFILE.

This function consults the synctex.gz database of PDFFILE and
searches for a filename, which is `file-equal-p' to FILENAME.
The first such filename is returned, or nil if none was found."

  (when (file-exists-p filename)
    (setq filename (expand-file-name filename))
    (let* ((synctex (pdf-sync-locate-synctex-file pdffile))
           (basename (file-name-nondirectory filename))
           (regexp (format "^ *Input *: *[^:\n]+ *:\\(.*%s\\)$"
                           (regexp-quote basename))))
      (when (and synctex
                 (file-readable-p synctex))
        (with-current-buffer (let ((revert-without-query (list "")))
                               (find-file-noselect synctex))
          ;; Keep point in front of the found filename. It will
          ;; probably be queried for again next time.
          (let ((beg (point))
                (end (point-max)))
            (catch 'found
              (dotimes (_x 2)
                (while (re-search-forward regexp end t)
                  (let ((syncname (match-string-no-properties 1)))
                    (when (and (file-exists-p syncname)
                               (file-equal-p filename syncname))
                      (goto-char (point-at-bol))
                      (throw 'found syncname))))
                (setq end beg
                      beg (point-min))
                (goto-char beg)))))))))
    

;; * ================================================================== *
;; * Compatibility
;; * ================================================================== *

;;;###autoload
(define-obsolete-variable-alias
  'pdf-sync-tex-display-pdf-key
  'pdf-sync-forward-display-pdf-key nil)

;;;###autoload
(define-obsolete-variable-alias
  'pdf-sync-goto-tex-hook
  'pdf-sync-forward-hook nil)

;;;###autoload
(define-obsolete-variable-alias
  'pdf-sync-display-pdf-hook
  'pdf-sync-backward-hook nil)

;;;###autoload
(define-obsolete-variable-alias
  'pdf-sync-display-pdf-action
  'pdf-sync-forward-display-action nil)

(define-obsolete-function-alias
  'pdf-sync-mouse-goto-tex
  'pdf-sync-backward-search-mouse)

(define-obsolete-function-alias
  'pdf-sync-goto-tex
  'pdf-sync-backward-search)

(define-obsolete-function-alias
  'pdf-sync-correlate-tex
  'pdf-sync-backward-correlate)

(define-obsolete-function-alias
  'pdf-sync-display-pdf
  'pdf-sync-forward-search)

(define-obsolete-function-alias
  'pdf-sync-correlate-pdf
  'pdf-sync-forward-correlate)

(provide 'pdf-sync)
;;; pdf-sync.el ends here
