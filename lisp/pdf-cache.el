;;; pdf-cache.el --- Cache time-critical or frequent epdfinfo queries. -*- lexical-binding:t -*-

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

(require 'pdf-info)




;; * ================================================================== *
;; * Common cache
;; * ================================================================== *

(defvar-local pdf-cache--common-cache nil)

(defmacro define-cacheable-epdfinfo-command (command)
  "Define a cached version of COMMAND.

COMMAND should be unquoted and needs to have a corresponding
`pdf-info-COMMAND' function.  Defines a function with the same
signature, except for the `file-or-buffer' argument, which will
always be nil \(i.e. the current buffer\).

This function will cache the results corresponding to the arguments,
as compared with equal.  The values remain in memory until the
buffer is killed or reverted."

  (let ((fn (intern (format "pdf-info-%s" command)))
        (defn (intern (format "pdf-cache-%s" command))))
    (unless (fboundp fn)
      (error "Invalid command: %s" command))
    (let ((args (help-function-arglist fn)))
      (unless (eq (car (last args)) 'file-or-buffer)
        (error "Last command function argument should be `file-or-buffer'."))
      (when (memq '&rest args)
        (error "&rest arguments are not supported by this macro"))
      (setq args (butlast args))
      (while (eq (car (last args)) '&optional)
        (setq args (butlast args)))
      (let ((doc (format "Cached version of `%s', which see.\n\n%s"
                         fn (cons 'FN
                                  (mapcar (lambda (a)
                                            (let ((s (symbol-name a)))
                                              (if (eq (aref s 0) ?&)
                                                  s
                                                (upcase s))))
                                          args)))))
        `(defun ,defn ,args ,doc
                (funcall 'pdf-cache--common-get
                         ,@(cons (list 'quote fn)
                                 (cl-remove '&optional args))))))))

(defun pdf-cache--common-get (&rest args)
  (unless pdf-cache--common-cache
    (setq pdf-cache--common-cache
          (make-hash-table :test 'equal))
    (add-hook 'after-revert-hook 'pdf-cache-clear-common-cache nil t))
  (or (gethash args pdf-cache--common-cache)
      (puthash args (apply (car args) (cdr args))
               pdf-cache--common-cache)))

(defun pdf-cache-clear-common-cache (&optional buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (setq pdf-cache--common-cache nil)))

(define-cacheable-epdfinfo-command number-of-pages)
(define-cacheable-epdfinfo-command pagelinks)
(define-cacheable-epdfinfo-command boundingbox)




;; * ================================================================== *
;; * PNG image cache
;; * ================================================================== *

(defcustom pdf-cache-image-cache-limit 64
  "Maximum number of cached PNG images."
  :type 'number
  :group 'pdf-tools)

(defvar-local pdf-cache--image-cache nil)

(defmacro pdf-cache--make-image (page width data hash)
  `(list page width data hash))
(defmacro pdf-cache--image/page (img) `(nth 0 ,img))
(defmacro pdf-cache--image/width (img) `(nth 1 ,img))
(defmacro pdf-cache--image/data (img) `(nth 2 ,img))
(defmacro pdf-cache--image/hash (img) `(nth 3 ,img))

(defun pdf-cache--image-match (image page min-width &optional max-width hash)
  (and (= (pdf-cache--image/page image)
          page)
       (or (null min-width)
           (>= (pdf-cache--image/width image)
               min-width))
       (or (null max-width)
           (<= (pdf-cache--image/width image)
               max-width))
       (or (null hash)
           (= (pdf-cache--image/hash image)
              hash))))

(defun pdf-cache--image-lookup (page min-width &optional max-width hash)
  (car (cl-member (list page min-width max-width hash)
                  pdf-cache--image-cache
                  :test (lambda (spec image)
                          (apply 'pdf-cache--image-match image spec)))))
  
  

(provide 'pdf-cache)

;;; pdf-cache.el ends here
