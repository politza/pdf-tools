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
  "Maximum number of cached PNG images per buffer."
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
       (or (null hash)
           (eql (pdf-cache--image/hash image)
                hash))))

(defun pdf-cache--image-get (page min-width &optional max-width hash)
  "Return PAGE's PNG data as a string.

Return an image of at least MIN-WIDTH and, if non-nil, maximum
width MAX-WIDTH. If hash is non-nil, return an image which was
previously put \(see `pdf-cache--image-put'\) with an `eql'
value.

Return nil, if no matching image was found."
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

(defun pdf-cache--image-put (page width data &optional hash)
  "Cache image of PAGE with WIDTH, DATA and HASH.

DATA should the string of a PNG image of width WIDTH and from
page PAGE in the current buffer.  See `pdf-cache--image-get' for
the HASH argument.

This function always returns nil."
  (push (pdf-cache--make-image page width data hash)
        pdf-cache--image-cache)
  ;; Forget old image(s).
  (when (> (length pdf-cache--image-cache)
           pdf-cache-image-cache-limit)
    (if (> pdf-cache-image-cache-limit 1)
        (setcdr (nthcdr (1- pdf-cache-image-cache-limit)
                        pdf-cache--image-cache)
                nil)
      (setq pdf-cache--image-cache nil)))
  nil)
                             
(defun pdf-cache--image-create-data (fn &rest args)
  "Create an image by calling FN with ARGS.

FN should return the filename of a PNG image.  Return the images
data as a string."
  (let (filename)
    (unwind-protect
        (progn
          (setq filename (apply fn args))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally filename)
            (propertize
             (buffer-substring-no-properties
              (point-min)
              (point-max)) 'display
              "<image image hidden>")))
      (when (and filename
                 (file-exists-p filename))
        (delete-file filename)))))

(defun pdf-cache-renderpage (page min-width &optional max-width)
  "Render PAGE according to MIN-WIDTH and MAX-WIDTH.

Return the PNG data of an image as a string, such that it's width
is at least MIN-WIDTH and, if non-nil, at most MAX-WIDTH.

If such an image is not available in the cache, call
`pdf-info-renderpage' to creat one."
  (or (pdf-cache--image-get page min-width max-width)
      (let ((data (pdf-cache--image-create-data
                    'pdf-info-renderpage
                    page min-width)))
        (pdf-cache--image-put page min-width data)
        data)))

(provide 'pdf-cache)

;;; pdf-cache.el ends here
