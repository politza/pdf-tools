;;; pdf-attach.el --- PDF attachments support. -*- lexical-binding: t -*-

;; Copyright (C) 2013  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: doc-view, pdf

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

(require 'pdf-util)
(require 'pdf-info)

;;; Code:

;; Resolve the circular dependency.
(declare-function pdf-annot-getannots "pdf-annot.el"
                  (&optional pages types file-or-buffer))
(declare-function pdf-annot-document "pdf-annot.el"
                  (a))
(declare-function pdf-annot-get "pdf-annot.el"
                  (a prop &optional default))

(defstruct (pdf-attach
            (:constructor pdf-attach-new (document page id properties))
            (:constructor nil))
  document
  id
  page
  properties)

(defun pdf-attach-getattachments (&optional file-or-buffer)
  "Return the attachments of FILE-OR-BUFFER.

FILE-OR-BUFFER defaults to the current buffer.  The return value
includes all attachments from annotations and global,
document-level ones."
  (require 'pdf-annot)
  (let* ((pdf (pdf-info--normalize-file-or-buffer file-or-buffer))
         (annots (pdf-annot-getannots nil 'file pdf))
         (docatt (pdf-info-getattachments nil pdf))
         attachments)
    ;; Identify document level attachments by their order.
    ;; Identification is needed for later retrieval of the
    ;; attachment's data.
    (dotimes (i (length docatt))
      (push (pdf-attach-new pdf 0 i (nth i docatt)) attachments))
    ;; Annotations have a unique key attached, no problem here.
    (dolist (a annots)
      (let ((pn (pdf-annot-get a 'page))
            (id (pdf-annot-get a 'id)))
        (push (pdf-attach-new
               pdf pn id
               (pdf-info-getattachment-from-annot
                id nil pdf))
              attachments)))
    attachments))

(defun pdf-attach-get-from-annot (a)
  "Return annotation A's attached data.

A should be a file-annotation, otherwise the result is an error."
  (require 'pdf-annot)
  (unless (eq (pdf-annot-get a 'type) 'file)
    (error "Annotation has no data attached: %s" a))
  (let* ((id (pdf-annot-get a 'id))
         (pdf (pdf-annot-document a))
         (page (pdf-annot-get a 'page))
         (alist (pdf-info-getattachment-from-annot id nil pdf)))
    (pdf-attach-new pdf page id alist)))

(defun pdf-attach-get (a prop &optional default)
  (or (cdr (assq prop (pdf-attach-properties a)))
      default))

(defun pdf-annot-set (a prop val)
  (declare (indent 2))
  (let ((curval (assq prop (pdf-annot-properties a))))
    (setf (pdf-annot-properties a)
          (cons (cons prop val)
                (delq curval (pdf-annot-properties a))))
    val))

(defun pdf-attach-name (a)
  "Return attachment A's specified filename or nil."
  (cdr (assq 'name (pdf-attach-properties a))))

(defun pdf-attach-description (a)
  "Return attachment A's description or nil."
  (cdr (assq 'description (pdf-attach-properties a))))

(defun pdf-attach-size (a)
  "Return attachment A's size or nil."
  (cdr (assq 'size (pdf-attach-properties a))))

(defun pdf-attach-mtime (a)
  "Return attachment A's modification time or nil."
  (cdr (assq 'mtime (pdf-attach-properties a))))

(defun pdf-attach-ctime (a)
  "Return attachment A's creation time or nil."
  (cdr (assq 'ctime (pdf-attach-properties a))))

(defun pdf-attach-checksum (a)
  "Return attachment A's checksum or nil."
  (cdr (assq 'checksum (pdf-attach-properties a))))

(defun pdf-attach-pp-for-tooltip (a)
  "Return a string describing attachment A."
  (let ((header (propertize
                 (format "File attachment `%s' of %s\n"
                         (or (pdf-attach-name a) "unnamed")
                         (if (pdf-attach-size a)
                             (format "size %d" (file-size-human-readable
                                                (pdf-attach-size a)))
                           "unknown size"))
                 'face 'header-line 'intangible t
                 'read-only t)))
    (concat
     (propertize
      (make-string (length header) ?\s)
      'display
      header)
     (or (pdf-attach-description a) "No description"))))         
  
(defun pdf-attach-from-annotation-p (a)
  "Return t, if attachment A belongs to some annotation.

There are two kinds of attachments,

i.  attachments associated with an annotation and
ii. attachment associated with the whole document.

This function returns t in case i. and nil in case ii. ."
  (symbolp (pdf-attach-id a)))
  
(defun pdf-attach-create-file (a)
  "Return a filename containing the data of attachment A.

The caller owns this file and should delete it, when it is no
longer needed."
  (cl-check-type a pdf-attach)
  (let ((page (pdf-attach-page a))
        (id (pdf-attach-id a)))
    (cond
     ((numberp id)
      ;; This is not very elegant.
      (let ((attachments (pdf-info-getattachments
                          t (pdf-attach-document a)))
            data)
        (dotimes (i (length attachments))
          (let ((file (cdr (assq 'file (nth i attachments)))))
            (if (= id i)
                (setq data file)
              (delete-file file))))
        data))
     (t
      (cdr (assq 'file (pdf-info-getattachment-from-annot
                        id t (pdf-attach-document a))))))))  

(defun pdf-attach-create-named-file (a dir)
  "Copy attachment A's data to DIR.

This uses A's specified filename and creates directories below
DIR appropriately.  (This may not be entirely accurate,
e.g. directory parts which can't be, for what ever reasons,
created, are simply ignored.)  If DIR already contains a filename
of the same name (inclusive directories), return that name and
don't overwrite it.

If A does not specify a filename, the name is chosen randomly and
put in DIR.

In any case, return the absolute filename of the created or found
file."
  
  (unless (file-directory-p dir)
    (error "Not a directory: %s" dir))
  (unless (file-writable-p dir)
    (error "Directory not writable: %s" dir))
  
  (let (datafile)
    (unwind-protect
        (let* ((name (pdf-attach-name a))
               (default-directory (expand-file-name dir))
               newfile)

          (setq datafile (pdf-attach-create-file a)
                newfile  (file-name-nondirectory datafile))
          (when name
            (let* ;; Name may be a unix or dos filename, I guess.
                ((parts (split-string name "[/\\]" t))
                 (dirs (butlast parts)))

              (setq newfile (car (last parts)))

              (dolist (d dirs)
                (condition-case nil
                    (progn
                      (unless (file-exists-p d)
                        (make-directory d))
                      (when (file-directory-p d)
                        (setq default-directory (expand-file-name d))))
                  (file-error)
                  (file-already-exists)))))

          (condition-case nil
              (copy-file datafile newfile)
            (file-already-exists))
          (expand-file-name newfile))
      (when (file-exists-p datafile)
        (delete-file datafile)))))

(defun pdf-attach-create-directory (&optional buffer directory)
  "Extract all attachments of BUFFER and put them in DIRECTORY.

BUFFER defaults to the current buffer and DIRECTORY to the
subdirectory `doc-view-current-cache-dir'/${PDF-NAME}_attachments.  

The attachments are written under their specified name, if
possible, but existing files are not overwritten.

If BUFFER has no attachments, do nothing and return nil,
otherwise return DIRECTORY."
  
  (pdf-util-assert-pdf-buffer buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (let ((dir (or (and directory
                        (expand-file-name directory))
                   (expand-file-name
                    (format "%s_attachments"
                            (file-name-sans-extension (buffer-name)))
                    (doc-view-current-cache-dir))))
          (attachments (pdf-attach-getattachments)))
      
      (when attachments
        (unless (file-exists-p dir)
          (make-directory dir t))

        (unless (file-directory-p dir)
          (error "Not a directory: %s" dir))

        (dolist (a attachments)
          (pdf-attach-create-named-file a dir))
        dir))))


(defun pdf-attach-dired (&optional buffer)
  "Visit all attachments of the PDF of BUFFER in dired."
  (interactive)
  (pdf-util-assert-pdf-buffer buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (let ((dir (expand-file-name
                (format "%s_attachments"
                        (file-name-sans-extension (buffer-name)))
                (doc-view-current-cache-dir))))
      (unless (file-exists-p dir)
        (setq dir (pdf-attach-create-directory nil dir)))
      (unless dir
        (error "Document has no data attached"))
      (dired dir))))
    

(provide 'pdf-attach)
;;; pdf-attach.el ends here

