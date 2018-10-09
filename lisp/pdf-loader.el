;;; pdf-loader.el --- Minimal PDF Tools loader       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andreas Politz

;; Author: Andreas Politz <politza@hochschule-trier.de>
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

;;; Code:

(defconst pdf-loader--auto-mode-alist-item
  (copy-sequence "\\.[pP][dD][fF]\\'")
  "The item used in `auto-mode-alist'.")

(defconst pdf-loader--magic-mode-alist-item
  (copy-sequence "%PDF")
  "The item used in`magic-mode-alist'.")


;;;###autoload
(defun pdf-loader-install (&optional no-query-p skip-dependencies-p
                                     no-error-p force-dependencies-p)
  "Prepare Emacs for using PDF Tools.

This function acts as a replacement for `pdf-tools-install' and
makes Emacs load and use PDF Tools as soon as a PDF file is
opened, but not sooner.

The arguments are passed verbatim to `pdf-tools-install', which
see."
  (let ((args (list no-query-p skip-dependencies-p
                    no-error-p force-dependencies-p)))
    (if (featurep 'pdf-tools)
        (apply #'pdf-tools-install args)
      (pdf-loader--install
       (lambda ()
         (apply #'pdf-loader--load args))))))

(defun pdf-loader--load (&rest args)
  (pdf-loader--uninstall)
  (save-selected-window
    (pdf-tools-install args)))

(defun pdf-loader--install (loader)
  (setf (alist-get pdf-loader--auto-mode-alist-item
                   auto-mode-alist nil nil #'equal)
        loader)
  (setf (alist-get pdf-loader--magic-mode-alist-item
                   magic-mode-alist nil nil #'equal)
        loader))

(defun pdf-loader--uninstall ()
  (setf (alist-get pdf-loader--auto-mode-alist-item
                   auto-mode-alist nil :remove #'equal)
        nil)
  (setf (alist-get pdf-loader--magic-mode-alist-item
                   magic-mode-alist nil :remove #'equal)
        nil))

(provide 'pdf-loader)
;;; pdf-loader.el ends here
