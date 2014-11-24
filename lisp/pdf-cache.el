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
(require 'pdf-annot)
(require 'pdf-view)


;; * ================================================================== *
;; * Customiazations
;; * ================================================================== *

(defcustom pdf-cache-image-limit 64
  "Maximum number of cached PNG images per buffer."
  :type 'integer
  :group 'pdf-cache
  :group 'pdf-view)

(defcustom pdf-cache-prefetch-delay 0.5
  "Idle time in seconds before prefetching images starts."
  :group 'pdf-view
  :type 'number)

(defcustom pdf-cache-prefetch-pages-function
  'pdf-cache-prefetch-pages-function-default
  "A function returning a list of pages to be prefetched.

It is called with no arguments in the PDF window and should
return a list of page-numbers, determining the pages that should
be prefetched and their order."
  :group 'pdf-view
  :type 'function)


;; * ================================================================== *
;; * Value cache
;; * ================================================================== *

(defvar-local pdf-cache--cache nil)

(defmacro define-cacheable-epdfinfo-command (command page-argument)
  "Define a cached version of COMMAND.

COMMAND should be unquoted and needs to have a corresponding
`pdf-info-COMMAND' function.  Defines a function with the same
signature, except for the `file-or-buffer' argument, which will
always be nil \(i.e. the current buffer\).

PAGE-ARGUMENT should be the number of the commands single-page
argument.  It may also be nil, meaning that command is page
independent or t, meaning that command may apply to multiple
pages.  This is used for keeping the cache up-to-date.

This function will cache the results corresponding to the
arguments, as compared with equal.  The values remain in memory
at most until the buffer is killed or reverted."

  (let ((fn (intern (format "pdf-info-%s" command)))
        (defn (intern (format "pdf-cache-%s" command))))
    (unless (fboundp fn)
      (error "Invalid command: %s" command))
    (let ((args (butlast (help-function-arglist fn))))
      (when (memq '&rest args)
        (error "&rest arguments are not supported by this macro"))
      (when (eq (car (last args)) '&optional)
        (setq args (butlast args)))
      (unless (or (null page-argument)
                  (eq page-argument t)
                  (and (integerp page-argument)
                       (>= page-argument 0)
                       (< page-argument
                          (length (remq '&optional args)))))
        (error "Invalid page-argument argument."))
      `(defun ,defn (,@args)
         ,(format "Cached version of `%s', which see.
Make shure, not to modify it's return value.\n" fn)
         (pdf-cache--retrieve
          ,page-argument
          ,@(cons (list 'quote command)
                  (cons (list 'quote fn)
                        (remove '&optional args))))))))

(defun pdf-cache--retrieve (page-argument command fn &rest args)
  (unless pdf-cache--cache
    (setq pdf-cache--cache (make-hash-table))
    (add-hook 'pdf-info-close-document-hook 'pdf-cache-clear nil t)
    (add-hook 'pdf-annot-modified-functions
              'pdf-cache--clear-annotations-pages))  
  (let* ((page (if (numberp page-argument)
                   (or (nth page-argument args) t)
                 page-argument))
         (alist (gethash page pdf-cache--cache))
         (key (cons command args)))
    (or (cdr (assoc key alist))
        (let ((value (apply fn args)))
          (puthash page (cons (cons key value)
                              alist)
                   pdf-cache--cache)
          value))))

(defun pdf-cache-clear ()
  (interactive)
  (clrhash pdf-cache--cache))

(defun pdf-cache-clear-pages (pages)
  (dolist (page (cons t pages))
    (remhash page pdf-cache--cache)))

(defun pdf-cache--clear-annotations-pages (fn)
  (pdf-cache-clear-pages
   (delq nil (mapcar (lambda (a)
                       (pdf-annot-get a 'page))
                     (funcall fn t)))))

(define-cacheable-epdfinfo-command number-of-pages nil)
(define-cacheable-epdfinfo-command pagelinks 0)
(define-cacheable-epdfinfo-command boundingbox 0)
(define-cacheable-epdfinfo-command pagesize 0)
(define-cacheable-epdfinfo-command textlayout 0)


;; * ================================================================== *
;; * PNG image LRU cache
;; * ================================================================== *

(defvar pdf-cache-image-inihibit nil
  "Non-nil, if the image cache should be bypassed.")
  
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
  "Return PAGE's cached PNG data as a string or nil.

Does not modify the cache.  See also `pdf-cache-get-image'."
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
    ;; Find it in the cache and remove it.  We need to find the
    ;; element in front of it.
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
  (unless pdf-cache--image-cache
    (add-hook 'pdf-info-close-document-hook 'pdf-cache-clear-images nil t)
    (add-hook 'pdf-annot-modified-functions
              'pdf-cache--clear-images-from-annotations nil t))
  (push (pdf-cache--make-image page width data hash)
        pdf-cache--image-cache)
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

(defun pdf-cache-remove-image-if (fn)
  "Remove images from the cache according to FN.

FN should be function accepting 4 Arguments \(PAGE WIDTH DATA
HASH\).  It should return non-nil, if the image should be removed
from the cache."
  (setq pdf-cache--image-cache
        (cl-remove-if
         (lambda (image)
           (funcall
            fn
            (pdf-cache--image/page image)
            (pdf-cache--image/width image)
            (pdf-cache--image/data image)
            (pdf-cache--image/hash image)))
         pdf-cache--image-cache)))

(defun pdf-cache--clear-images-from-annotations (fn)
  (let ((pages (delq nil (mapcar (lambda (a)
                                   (pdf-annot-get a 'page))
                                 (funcall fn t)))))
    (pdf-cache-remove-image-if
     (lambda (page &rest _)
       (memq page pages)))))

(defun pdf-cache-renderpage (page min-width &optional max-width)
  "Render PAGE according to MIN-WIDTH and MAX-WIDTH.

Return the PNG data of an image as a string, such that it's width
is at least MIN-WIDTH and, if non-nil, at most MAX-WIDTH.

If such an image is not available in the cache, call
`pdf-info-renderpage' to create one."
  (if pdf-cache-image-inihibit
      (pdf-info-renderpage page min-width)
    (or (pdf-cache-get-image page min-width max-width)
        (let ((data (pdf-info-renderpage page min-width)))
          (pdf-cache-put-image page min-width data)
          data))))

(defun pdf-cache-renderpage-text-regions (page width single-line-p
                                               &rest selection)
  "Render PAGE according to WIDTH, SINGLE-LINE-P and SELECTION.

See also `pdf-info-renderpage-text-regions' and
`pdf-cache-renderpage'."
  (if pdf-cache-image-inihibit
      (apply 'pdf-info-renderpage-text-regions
             page width single-line-p nil selection)
    (let ((hash (sxhash
                 (format "%S" (cons 'renderpage-text-regions
                                    (cons single-line-p selection))))))
      (or (pdf-cache-get-image page width nil hash)
          (let ((data (apply 'pdf-info-renderpage-text-regions
                             page width single-line-p nil selection)))
            (pdf-cache-put-image page width data hash)
            data)))))


;; * ================================================================== *
;; * Prefetching images
;; * ================================================================== *

(defvar-local pdf-cache--prefetch-pages nil
  "Pages to be prefetched.")

(defvar-local pdf-cache--prefetch-timer nil
  "Timer used when prefetching images.")

(defun pdf-cache-prefetch-pages-function-default ()
  (let ((page (pdf-view-current-page)))
    (cl-remove-duplicates
     (cl-remove-if-not
      (lambda (page)
        (and (>= page 1)
             (<= page (pdf-cache-number-of-pages))))
      (append
       ;; +1, -1, +2, -2, ...
       (let ((sign 1)
             (incr 1))
         (mapcar (lambda (i)
                   (setq page (+ page (* sign incr))
                         sign (- sign)
                         incr (1+ incr))
                   page)
                 (number-sequence 1 16)))
       ;; First and last
       (list 1 (pdf-cache-number-of-pages))
       ;; Links
       (mapcar
        'cadddr
        (cl-remove-if-not
         (lambda (link) (eq (cadr link) 'goto-dest))
         (pdf-cache-pagelinks
          (pdf-view-current-page)))))))))

(defun pdf-cache--prefetch-pages (window image-width)
  (when (eq window (selected-window))
    (let ((page (pop pdf-cache--prefetch-pages)))
      (while (and page
                  (pdf-cache-lookup-image
                   page
                   image-width
                   (if (not (pdf-view-use-scaling-p))
                       image-width
                     (* 2 image-width))))
        (setq page (pop pdf-cache--prefetch-pages)))
      (when (null page)
        (pdf-tools-debug "Prefetching done."))
      (when page
        (let ((pdf-info-asynchronous
               (lambda (status data)
                 (when (and (null status)
                            (eq window
                                (selected-window)))
                   (with-current-buffer (window-buffer)
                     (pdf-cache-put-image
                      page image-width data)
                     (image-size (pdf-view-create-page page))
                     (pdf-tools-debug "Prefetched Page %s." page)
                     ;; Avoid max-lisp-eval-depth
                     (run-with-timer
                         0.001 nil 'pdf-cache--prefetch-pages window image-width))))))
          (pdf-info-renderpage page image-width))))))

(defun pdf-cache--prefetch-start (buffer)
  "Start prefetching images in BUFFER."
  (when (and pdf-cache-prefetch-minor-mode
             (not isearch-mode)
             (null pdf-cache--prefetch-pages)
             (eq (window-buffer) buffer)
             (fboundp pdf-cache-prefetch-pages-function))
    (let ((pages (funcall pdf-cache-prefetch-pages-function)))
      (setq pdf-cache--prefetch-pages
            (butlast pages (max 0 (- (length pages)
                                     pdf-cache-image-limit))))
      (pdf-cache--prefetch-pages
       (selected-window)
       (car (pdf-view-desired-image-size))))))

(defun pdf-cache--prefetch-stop ()
  "Stop prefetching images in current buffer."
  (setq pdf-cache--prefetch-pages nil))
  
(defun pdf-cache--prefetch-cancel ()
  "Cancel prefetching images in current buffer."
  (pdf-cache--prefetch-stop)
  (when pdf-cache--prefetch-timer
    (cancel-timer pdf-cache--prefetch-timer))
  (setq pdf-cache--prefetch-timer nil))

(define-minor-mode pdf-cache-prefetch-minor-mode
  "Try to load images which will probably be needed in a while."
  nil nil t
  
  (pdf-util-assert-pdf-buffer)
  (pdf-cache--prefetch-cancel)
  (add-hook 'after-change-major-mode-hook
            'pdf-cache--prefetch-cancel nil t)            
  (cond
   (pdf-cache-prefetch-minor-mode
    (add-hook 'pre-command-hook 'pdf-cache--prefetch-stop nil t)
    (setq pdf-cache--prefetch-timer
          (run-with-idle-timer (or pdf-cache-prefetch-delay 1)
              t 'pdf-cache--prefetch-start (current-buffer))))
   (t
    (remove-hook 'pre-command-hook 'pdf-cache--prefetch-stop t))))

(provide 'pdf-view)
;;; pdf-view.el ends here

(provide 'pdf-cache)

;;; pdf-cache.el ends here
