;;; pdf-annot.el --- Attachment support for PDF files.  -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
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

(defun pdf-attach-make-file-name (name &optional other)
  "Turn NAME into a relative filename, not equal to names in OTHER."
  
  (let* ;; Name may be a unix or dos filename, I guess.
      ((parts (split-string name "[/\\]" t))
       (newname (concat (file-name-as-directory ".") (pop parts)))
       (i 0))

    (dolist (part parts)
      (setq newname
            (concat (file-name-as-directory newname)
                    part)))

    (let ((extension (file-name-extension newname)))
      (while (member newname other)
        (setq newname (format "%s-%d%s"
                              (file-name-sans-extension newname)
                              (cl-incf i)
                              (if extension
                                  (concat "." extension)
                                "")))))
    newname))

(defun pdf-annot-getattachments (&optional do-save)
  "Return the attachments of the current buffer.

See `pdf-info-getattachments' for the DO-SAVE argument and this
function's return value.

Returns all attachments from annotations, as well as
document-level ones."
  (let* ((annots (pdf-annot-getannots nil 'file))
         (attachments (pdf-info-getattachments do-save)))
    (dolist (a annots)
      (let ((pn (pdf-annot-get a 'page))
            (id (pdf-annot-get a 'id)))
        (push (pdf-info-getattachment-from-annot id do-save)
              attachments)))
    attachments))

(defun pdf-attach-create-file (a)
  "Return a filename containing the data of attachment A.

The caller owns this file and should delete it, when it is no
longer needed."
  (cl-check-type a pdf-attach)
  (let ((id (pdf-attach-id a)))
    (cond
     ((numberp id)
      ;; This is not very elegant.
      (let ((attachments (pdf-info-getattachments
                          t (pdf-attach-buffer a)))
            data)
        (dotimes (i (length attachments))
          (let ((file (cdr (assq 'file (nth i attachments)))))
            (if (= id i)
                (setq data file)
              (delete-file file))))
        data))
     (t
      (cdr (assq 'file (pdf-info-getattachment-from-annot
                        id t (pdf-attach-buffer a))))))))  

(defun pdf-attach-create-named-file (a &optional dir)
  "Copy attachment A's data to DIR.

This uses A's specified filename and creates directories below
DIR appropriately.  (This may not be entirely accurate,
e.g. directory parts which can't be, for what ever reasons,
created, are simply ignored.)  If DIR already contains a filename
of the same name (inclusive directories), return that name and
don't overwrite it.

DIR defaults to `pdf-attach-default-directory'.  If A does not
specify a filename, the name is chosen randomly and put in DIR.

In any case, return the absolute filename of the created or found
file."

  (unless dir
    (setq dir (pdf-attach-default-directory (pdf-attach-buffer a)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
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

(defun pdf-attach-find-file (a)
  (when (pdf-annot-p a)
    (setq a (pdf-attach-get-from-annot a)))
  (find-file
   (pdf-attach-create-named-file a)))

(defun pdf-attach-find-file-other-window (a)
  (when (pdf-annot-p a)
    (setq a (pdf-attach-get-from-annot a)))
  (pop-to-buffer
   (find-file-noselect
    (pdf-attach-create-named-file a))))
  
(defun pdf-attach-write-directory (&optional buffer directory)
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
                   (pdf-attach-default-directory)))
          (attachments (pdf-attach-getattachments)))
      
      (when attachments
        (unless (file-exists-p dir)
          (make-directory dir t))

        (unless (file-directory-p dir)
          (error "Not a directory: %s" dir))

        (dolist (a attachments)
          (pdf-attach-create-named-file a dir))
        dir))))


(defun pdf-attach-default-directory (&optional buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (pdf-util-expand-file-name "pdf-attach")))
  
(defun pdf-attach-dired (&optional buffer)
  "Visit all attachments of the PDF of BUFFER in dired."
  (interactive)
  (pdf-util-assert-pdf-buffer buffer)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (let ((dir (pdf-attach-default-directory)))
      (unless (file-exists-p dir)
        (setq dir (pdf-attach-write-directory nil dir)))
      (unless dir
        (error "Document has no data attached"))
      (dired-other-window dir))))

(provide 'pdf-attach)

;;; pdf-attach.el ends here
