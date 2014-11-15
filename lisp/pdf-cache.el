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
(require 'pdf-util)



;; * ================================================================== *
;; * Value cache
;; * ================================================================== *

(defvar-local pdf-cache--cache nil)

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
    (let ((args (butlast (help-function-arglist fn))))
      (when (memq '&rest args)
        (error "&rest arguments are not supported by this macro"))
      (when (eq (car (last args)) '&optional)
        (setq args (butlast args)))
      (let ((doc (format "Cached version of `%s', which see.  Recache value, if REFRESH-P
is non-nil.  Be shure, not to modify the return value.\n" fn))
            (refresh-arg (if (memq '&optional args)
                             (list 'refresh-p)
                           (list '&optional 'refresh-p))))
        `(defun ,defn (,@args ,@refresh-arg) ,doc
                (pdf-cache--retrieve
                 refresh-p
                 ,@(cons (list 'quote fn)
                         (remove '&optional args))))))))

(defun pdf-cache--retrieve (refresh-p &rest args)
  (unless pdf-cache--cache
    (setq pdf-cache--cache
          (make-hash-table :test 'equal))
    (add-hook 'after-revert-hook 'pdf-cache-clear-cache nil t)
    (add-hook 'after-save-hook 'pdf-cache-clear-cache nil t))
  ;; FIXME: Create a copy of sequences ?
  (or (and (not refresh-p)
           (gethash args pdf-cache--cache))
      (puthash args (apply (car args) (cdr args))
               pdf-cache--cache)))

(defun pdf-cache-clear (&optional buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (setq pdf-cache--cache nil)))

(define-cacheable-epdfinfo-command number-of-pages)
(define-cacheable-epdfinfo-command pagelinks)
(define-cacheable-epdfinfo-command boundingbox)
(define-cacheable-epdfinfo-command pagesize)
(define-cacheable-epdfinfo-command getselection)


;; * ================================================================== *
;; * PNG image LRU cache
;; * ================================================================== *

(defcustom pdf-cache-image-limit 64
  "Maximum number of cached PNG images per buffer."
  :type 'number
  :group 'pdf-cache
  :group 'pdf-view)

(defvar-local pdf-cache--image-cache nil)

(defmacro pdf-cache--make-image (page width data hash)
  `(list page width data hash))
(defmacro pdf-cache--image/page (img) `(nth 0 ,img))
(defmacro pdf-cache--image/width (img) `(nth 1 ,img))
(defmacro pdf-cache--image/data (img) `(nth 2 ,img))
(defmacro pdf-cache--image/hash (img) `(nth 3 ,img))

(defun pdf-cache--image-match (image page min-width &optional max-width hash)
  "Match IMAGE with specs.

IMAGE should be a list as created by `pdf-cache--make-image'.

Return non-nil, if IMAGE's page is the same as PAGE, it's width
is at least MIN-WIDTH and at most MAX-WIDTH and it's stored
hash-value is `eql' to HASH."
  (and (= (pdf-cache--image/page image)
          page)
       (or (null min-width)
           (>= (pdf-cache--image/width image)
               min-width))
       (or (null max-width)
           (<= (pdf-cache--image/width image)
               max-width))
       (eql (pdf-cache--image/hash image)
            hash)))

(defun pdf-cache-lookup-image (page min-width &optional max-width hash)
  "Return PAGE's PNG data as a string.

Don't modify the cache.  See also `pdf-cache-get-image'."
  (let ((image (car (cl-member
                     (list page min-width max-width hash)
                     pdf-cache--image-cache
                     :test (lambda (spec image)
                             (apply 'pdf-cache--image-match image spec))))))
    (and image
         (pdf-cache--image/data image))))
  
(defun pdf-cache-get-image (page min-width &optional max-width hash)
  "Return PAGE's PNG data as a string.

Return an image of at least MIN-WIDTH and, if non-nil, maximum
width MAX-WIDTH and `eql' hash value.

Remember that image was recently used.

Returns nil, if no matching image was found."
  (let ((cache (cons nil pdf-cache--image-cache))
        image)
    ;; Find it in the cache and remove it.  Therefore we need to find
    ;; the element before it.
    (while (and (cdr cache)
                (not (pdf-cache--image-match
                      (car (cdr cache))
                      page min-width max-width hash)))                    
      (setq cache (cdr cache)))
    (setq image (cadr cache))
    (when (car cache)
      (setcdr cache (cddr cache)))
    ;; Now push it at the front.
    (when image
      (push image pdf-cache--image-cache)
      (pdf-cache--image/data image))))

(defun pdf-cache-put-image (page width data &optional hash)
  "Cache image of PAGE with WIDTH, DATA and HASH.

DATA should the string of a PNG image of width WIDTH and from
page PAGE in the current buffer.  See `pdf-cache-get-image' for
the HASH argument.

This function always returns nil."
  (push (pdf-cache--make-image page width data hash)
        pdf-cache--image-cache)
  (add-hook 'after-revert-hook 'pdf-cache-clear-images nil t)
  (add-hook 'after-save-hook 'pdf-cache-clear-images nil t)
  ;; Forget old image(s).
  (when (> (length pdf-cache--image-cache)
           pdf-cache-image-limit)
    (if (> pdf-cache-image-limit 1)
        (setcdr (nthcdr (1- pdf-cache-image-limit)
                        pdf-cache--image-cache)
                nil)
      (setq pdf-cache--image-cache nil)))
  nil)

(defun pdf-cache-clear-images ()
  "Clear the image cache."
  (setq pdf-cache--image-cache nil))

(defun pdf-cache-renderpage (page min-width &optional max-width)
  "Render PAGE according to MIN-WIDTH and MAX-WIDTH.

Return the PNG data of an image as a string, such that it's width
is at least MIN-WIDTH and, if non-nil, at most MAX-WIDTH.

If such an image is not available in the cache, call
`pdf-info-renderpage' to create one."
  (or (pdf-cache-get-image page min-width max-width)
      (let ((data (pdf-info-renderpage page min-width)))
        (pdf-cache-put-image page min-width data)
        data)))

(defun pdf-cache-renderpage-selection (page width single-line-p
                                            &rest selection)
  "Render PAGE according to WIDTH and SELECTION.

See also `pdf-info-renderpage-selection' and
`pdf-cache-renderpage'."
  (let ((hash (sxhash
               (format "%S" (cons 'renderpage-selection
                                  (cons single-line-p selection))))))
    (or (pdf-cache-get-image page width nil hash)
        (let ((data (apply 'pdf-info-renderpage-selection
                           page width single-line-p nil selection)))
          (pdf-cache-put-image page width data hash)
          data))))

(provide 'pdf-cache)

;;; pdf-cache.el ends here
