;;; pdf-info.el --- Extract infos from pdf-files via a helper process. -*- lexical-binding: t -*-

;; Copyright (C) 2013  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, pdf

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

(require 'tq)
(require 'cl-lib)

(defgroup pdf-info nil
  "Extract infos from pdf-files via a helper process."
  :group 'pdf-tools)
  
(defcustom pdf-info-epdfinfo-program
  (let* ((exec-path (cons
                     (if load-file-name
                         (file-name-directory load-file-name)
                       default-directory)
                       exec-path)))
    (executable-find "epdfinfo"))
  "Filename of the epdfinfo executable."
  :group 'pdf-info
  :type '(file :must-match t))

(defcustom pdf-info-log-buffer nil
  "The name of the log buffer.

If this is non-nil, all communication with the epdfinfo programm
  will be logged to this buffer."
  :group 'pdf-info
  :type '(choice
          (const "*pdf-info-log*")
          (string :tag "Buffer name")
          (const :tag "Logging deactivated" nil)))

(defcustom pdf-info-restart-process-p 'ask
  "What to do when the epdfinfo server died.

This should be one of
nil -- do nothing,
t   -- automatically restart it or
ask -- ask whether to restart or not."
  :group 'pdf-info
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Restart silently" t)
                 (const :tag "Always ask" ask)))

(defconst pdf-info-read-only-properties
  '(page type id flags modified subject opacity popup-edges
         created state))

(defconst pdf-info-writeable-properties
  '(color contents label popup-isopen icon isopen))

;;
;; Internal Variables and Functions
  
(defvar pdf-info-queue t
  "Internally used transmission-queue for the epdfinfo server.")

(defun pdf-info-process ()
  "Return the process object or nil."
  (and pdf-info-queue
       (not (eq t pdf-info-queue))
       (tq-process pdf-info-queue)))

(defun pdf-info-process-assert-running (&optional force)
  "Assert that the epdfinfo process is running.

If it never ran, i.e. `pdf-info-process' is t, start it
unconditionally.

If FORCE is non-nil unconditionally start it, if it is not
running.  Otherwise restart it with respect to the variable
`pdf-info-restart-process-p', which see.

If getting the process to run fails, this function throws an
error."
  (interactive "P")
  (unless (and (processp (pdf-info-process))
               (eq (process-status (pdf-info-process))
                   'run))
    (when (pdf-info-process)
      (tq-close pdf-info-queue)
      (setq pdf-info-queue nil))
    (unless (or force
                (eq pdf-info-queue t)
                (and (eq pdf-info-restart-process-p 'ask)
                     (not noninteractive)
                     (y-or-n-p "The epdfinfo server quit, restart it ? "))
                (and pdf-info-restart-process-p
                     (not (eq pdf-info-restart-process-p 'ask))))
      
      (error "The epdfinfo server quit"))
    (unless (and pdf-info-epdfinfo-program
                 (file-executable-p pdf-info-epdfinfo-program))
      (error "The variable pdf-info-epdfinfo-program is unset or not executable: %s"
             pdf-info-epdfinfo-program))
    (let ((proc (start-process
                 "epdfinfo" "*epdfinfo*" pdf-info-epdfinfo-program)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
      (setq pdf-info-queue (tq-create proc))))
  pdf-info-queue)

(defun pdf-info-log (string &optional outgoing-p)
  "Log STRING as query/response, depending on OUTGOING-P.

This is a no-op, if `pdf-info-log-buffer' is nil."
  (when pdf-info-log-buffer
    (with-current-buffer (get-buffer-create pdf-info-log-buffer)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert ?\n))
        (insert
         (propertize
          (concat (current-time-string) ":")
          'face
          (if outgoing-p
              'font-lock-keyword-face
            'font-lock-function-name-face))
         string)))))

(defun pdf-info-query (cmd &rest args)
  "Query the server useing CMD and ARGS."
  (pdf-info-process-assert-running)
  (unless (symbolp cmd)
    (setq cmd (intern cmd)))
  (let* ((query (concat (mapconcat 'pdf-info-query--escape
                                   (cons cmd args) ":") "\n"))
         response
         (timeout 12)
         (callback (lambda (_ r)
                     (setq response r))))
    (pdf-info-log query t)
    (tq-enqueue
     pdf-info-queue query "^\\.\n" nil callback t)
    (while (and (null response)
                (eq (process-status (pdf-info-process))
                    'run)
                (or (not inhibit-quit)
                    (> timeout 0)))
      (unless (accept-process-output (pdf-info-process) 1)
        (cl-decf timeout)))
    (cond
     (response
      (pdf-info-log response)
      (let ((response (pdf-info-query--parse-response cmd response)))
        (when (and (consp response)
                   (eq 'error (car response)))
          (error "%s" (cadr response)))
        response))
     ((not (eq (process-status (pdf-info-process))
               'run))
      ;; try again
      (apply 'pdf-info-query cmd args))
     (t
      (error "The epdfinfo server timed-out on command %s" cmd)))))

(defun pdf-info-query--escape (arg)
  "Escape ARG for transmision to the server."
  (if (null arg)
      ""
    (with-temp-buffer
      (save-excursion (insert (format "%s" arg)))
      (while (not (eobp))
        (cond
         ((eq (char-after) ?:)
          (insert ?\\))
         ((eq (char-after) ?\n)
          (delete-char 1)
          (insert ?\\ ?n)
          (backward-char)))
        (forward-char))
      (buffer-string))))
  
(defun pdf-info-query--parse-response (cmd response)
  "Parse one epdfinfo RESPONSE to CMD."
  (with-temp-buffer
    (save-excursion (insert response))
    (cond
     ((looking-at "ERR\n")
      (forward-line)
      (list 'error (buffer-substring-no-properties
                    (point)
                    (progn
                      (re-search-forward "^\\.\n")
                      (1- (match-beginning 0))))))
     ((looking-at "OK\n")
      (save-excursion
        ;; FIXME: Hotfix: poppler prints this to stdout, if a
        ;; destination lookup failed. (Is it really stdout ?)
        ;; (while (re-search-forward "failed to look up [A-Z.0-9]+\n" nil t)
        ;;   (replace-match ""))
        )
      (let (result)
        (forward-line)
        (while (not (looking-at "^\\.\n"))
          (push (pdf-info-query--read-record) result))
        (pdf-info-query--transform-response
         cmd (nreverse result))))
     (t
      (error "Got invalid response from epdfinfo server")))))

(defun pdf-info-query--read-record ()
  "Read a single record of the response in current buffer."
  (let (records done (beg (point)))
    (while (not done)
      (cl-case (char-after)
        (?\\
         (delete-char 1)
         (if (not (eq (char-after) ?n))
             (forward-char)
           (delete-char 1)
           (insert ?\n)))
        ((?: ?\n)
         (push (buffer-substring-no-properties
                beg (point)) records)
         (forward-char)
         (setq beg (point)
               done (bolp)))
        (t (forward-char))))
    (nreverse records)))

(defun pdf-info-query--transform-response (cmd response)
  "Transform a RESPONSE to CMD into a convenient lisp form."
  (cl-macrolet ((pop-not-empty (l)
                  `(let ((str (pop ,l)))
                     (and (> (length str) 0) str))))
    (cl-case cmd
      (open nil)
      (close (equal "1" (caar response)))
      (number-of-pages (string-to-number (caar response)))
      (search
       (let ((matches (mapcar (lambda (r)
                                (list
                                 (string-to-number (pop r))
                                 (mapcar 'string-to-number
                                         (split-string (pop r) " " t))
                                 (pop r)))
                              response))
             result)
         (while matches
           (let ((page (caar matches))
                 items)
             (while (and matches
                         (= (caar matches) page))
               (push (cdr (pop matches)) items))
             (push (cons page (nreverse items)) result)))
         (nreverse result)))
      (outline
       (mapcar (lambda (r)
                 (cons (string-to-number (pop r))
                       (pdf-info-query--transform-action r)))
               response))
      (pagelinks
       (mapcar (lambda (r)
                 (cons
                  (mapcar 'string-to-number ;area
                          (split-string (pop r) " " t))
                  (pdf-info-query--transform-action r)))
               response))
      (metadata
       (let ((md (car response)))
         (if (= 1 (length md))
             (list (cons 'title (car md)))
           (list
            (cons 'title (pop md))
            (cons 'author (pop md))
            (cons 'subject (pop md))
            (cons 'keywords-raw (car md))
            (cons 'keywords (split-string (pop md) "[\t\n ]*,[\t\n ]*" t))
            (cons 'creator (pop md))
            (cons 'producer (pop md))
            (cons 'format (pop md))
            (cons 'created (pop md))
            (cons 'modified (pop md))))))
      (gettext
       (or (caar response) ""))
      (features (mapcar 'intern (car response)))
      (pagesize
       (setq response (car response))
       (cons (round (string-to-number (car response)))
             (round (string-to-number (cadr response)))))
      ((getannots getannot)
       (funcall
        (if (eq cmd 'getannot)
            'car
          'identity)
        (mapcar
         (lambda (a)
           `((page . ,(string-to-number (pop a)))
             (edges . ,(mapcar 'string-to-number
                               (split-string (pop a) " " t)))
             (type . ,(intern (pop a)))
             (id . ,(intern (pop a)))
             (flags . ,(string-to-number (pop a)))
             (color . ,(pop-not-empty a))
             (contents . ,(pop a))
             (modified . ,(pop-not-empty a))
             ,@(when a
                 `((label . ,(pop-not-empty a))
                   (subject . ,(pop-not-empty a))
                   (opacity . ,(pop a))
                   (popup-edges . ,(let ((p (pop-not-empty a)))
                                     (when p
                                       (mapcar 'string-to-number
                                               (split-string p " " t)))))
                   (popup-isopen . ,(equal (pop a) "1"))
                   (created . ,(pop-not-empty a))))
             ,@(when a
                 `((icon . ,(pop-not-empty a))
                   (state . ,(pop-not-empty a))
                   (isopen . ,(pop-not-empty a))))))
         response)))
      ((getattachments getattachment-from-annot)
       (funcall
        (if (eq cmd 'getattachment-from-annot)
            'car
          'identity)
        (mapcar (lambda (a)
                  `((name . ,(pop-not-empty a))
                    (description . ,(pop-not-empty a))
                    (size . ,(let ((n (string-to-number (pop a))))
                               (and (>= n 0) n)))
                    (modified . ,(pop-not-empty a))
                    (created . ,(pop-not-empty a))
                    (checksum . ,(pop-not-empty a))
                    (file . ,(pop-not-empty a))))
                response)))
      (save (caar response))
      (t response))))

(defun pdf-info-query--transform-action (action)
  "Transform ACTION response into a convenient Lisp form."
(let ((type (intern (pop action))))
    (cons type
          (cons (pop action)
                (cl-case type
                  (goto-dest
                   (list (string-to-number (pop action))
                         (and (> (length (car action)) 0)
                              (string-to-number (pop action)))))
                  (goto-remote
                   (list (pop action)
                         (string-to-number (pop action))
                         (and (> (length (car action)) 0)
                              (string-to-number (pop action)))
                         (and (> (length (car action)) 0)
                              (string-to-number (pop action)))))
                  (t action))))))

(defun pdf-info--normalize-file-or-buffer (file-or-buffer)
  "Return the PDF file corresponding to FILE-OR-BUFFER.

FILE-OR-BUFFER may be nil, a PDF buffer, the name of a PDF buffer
or a PDF file."
  (unless file-or-buffer (setq file-or-buffer
                               (or doc-view-buffer-file-name
                                   (current-buffer))))
  (when (bufferp file-or-buffer)
    (unless (buffer-live-p file-or-buffer)
      (error "Buffer is not live :%s" file-or-buffer))
    (with-current-buffer file-or-buffer
      (unless (setq file-or-buffer (or doc-view-buffer-file-name
                                       (buffer-file-name file-or-buffer)))
        (error "Buffer is not associated with any file :%s"
               (buffer-name file-or-buffer)))))
  (unless (stringp file-or-buffer)
    (signal 'wrong-type-argument
            (list 'stringp 'bufferp 'null file-or-buffer)))
  ;; is file
  (when (file-remote-p file-or-buffer)
    (error "Processing remote files not supported :%s"
           file-or-buffer))
  (unless (file-readable-p file-or-buffer)
    (error "File not readable :%s" file-or-buffer))
  file-or-buffer)

(defun pdf-info-valid-page-spec-p (pages)
  "The type predicate for a valid page-spec."
  (not (not (ignore-errors (pdf-info--normalize-pages pages)))))

(defun pdf-info--normalize-pages (pages)
  "Normalize PAGES into a form \(FIRST . LAST\).

PAGES may be one of

- a single page number,

- a cons \(FIRST . LAST\),

- \(FIRST . t\), which represents all pages from FIRST to the end
  of the document or

-  nil, which stands for all pages."
  (cond
   ((null pages)
    (cons 0 0))
   ((natnump pages)
    (cons pages pages))
   ((natnump (car pages))
    (cond
     ((null (cdr pages))
      (cons (car pages) (car pages)))
     ((eq (cdr pages) t)
      (cons (car pages) 0))
     ((natnump (cdr pages))
      pages)))
   (t
    (signal 'wrong-type-argument
            (list 'pdf-info-valid-page-spec-p pages)))))


;;
;; High level interface
;;

(defun pdf-info-features ()
  "Return a list of symbols describing compile-time features."
  (pdf-info-query 'features))
                          
(defun pdf-info-open (&optional file-or-buffer password)
  "Open the docüment FILE-OR-BUFFER using PASSWORD.

Generally, docüments are opened and closed automatically on
demand, so this function is rarely needed, unless a PASSWORD is
set on the docüment.

Manually opened docüments are never closed automatically."

  (pdf-info-query
   'open (pdf-info--normalize-file-or-buffer file-or-buffer)
   password))

(defun pdf-info-close (&optional file-or-buffer)
  "Close the document FILE-OR-BUFFER.

Returns t, if the document was actually open, otherwise nil.
This command is rarely needed, see also `pdf-info-open'."
  (pdf-info-query
   'close (pdf-info--normalize-file-or-buffer file-or-buffer)))

(defun pdf-info-metadata (&optional file-or-buffer)
  "Extract the metadata from the document FILE-OR-BUFFER.

This returns an alist of key-value-pairs."
  (pdf-info-query
   'metadata
   (pdf-info--normalize-file-or-buffer file-or-buffer)))
               
(defun pdf-info-search (string &optional file-or-buffer pages)
  "Search for STRING in PAGES of docüment FILE-OR-BUFFER.

See `pdf-info--normalize-pages' for valid PAGES formats.

This function returns an alist \(\((PAGE . MATCHES\) ... \),
where MATCHES represents a list of matches on PAGE.  Each MATCHES
item has the form of \(EDGES TEXT\), where EDGES represent the
coordinates of the match as a list of four values \(LEFT TOP
RIGHT BOTTOM\), these values are relative, i.e. from the interval
\[0;1\].  TEXT is the matched text and may be empty, if extracting
text is not available."

  (let ((pages (pdf-info--normalize-pages pages)))
    (pdf-info-query
     'search
     (pdf-info--normalize-file-or-buffer file-or-buffer)
     (if case-fold-search 1 0)
     (car pages)
     (cdr pages)
     string)))

(defun pdf-info-pagelinks (page &optional file-or-buffer)
  "Return a list of links on PAGE in docüment FILE-OR-BUFFER.

See `pdf-info--normalize-pages' for valid PAGES formats.

This function returns a list \(\(EDGES . ACTION\) ... \), where
EDGES has the same form as in `pdf-info-search'.  ACTION
represents a PDF Action and has the form \(TYPE TITLE . ARGS\),
there TYPE is the type of action, TITLE is, a possibly empty,
name for this action and ARGS is a list of the action's
arguments.

TYPE may be one of

goto-dest -- An internal link to some page.
ARGS has the form \(PAGE TOP\), where PAGE is the page of the
link and TOP it's (relative) vertical position.

goto-remote -- An external link to some document.
ARGS is \(PDFFILE PAGE TOP\), where PDFFILE is the file-name of
the PDF, PAGE the page number and TOP the (relative) horizontal
position.

ur -- An link in form of a URI.
ARGS contains one element, the URI string.

In all casses PAGE may be 0, which means unspecified.  Equally
TOP may be nil."
  (cl-check-type page natnum)
  (pdf-info-query
   'pagelinks
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page))

(defun pdf-info-number-of-pages (&optional file-or-buffer)
  "Return the number of pages in document FILE-OR-BUFFER."
  (pdf-info-query 'number-of-pages
                  (pdf-info--normalize-file-or-buffer
                   file-or-buffer)))

(defun pdf-info-outline (&optional file-or-buffer)
  "Return the PDF outline of document FILE-OR-BUFFER.

This function returns a list \(\(DEPTH . ACTION\) ... \) of
outline items, where DEPTH >= 1 is the depth of this item in the tree
and ACTION has the same format as in `pdf-info-pagelinks', which
see."

  (pdf-info-query
   'outline
   (pdf-info--normalize-file-or-buffer file-or-buffer)))

(defun pdf-info-gettext (page x0 y0 x1 y1 &optional file-or-buffer)
  "On PAGE extract the text of the selection X0 Y0 X1 and Y1.

The coordinates of the selection have to be relative, i.e. in the
interval [0;1].  It may extend to multiple lines, which works as
usual (e.g. like the region in Emacs).

Return the text contained in the selection."

  (pdf-info-query
   'gettext
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page x0 y0 x1 y1))

(defun pdf-info-pagesize (&optional page file-or-buffer)
  "Return the size of PAGE as a cons \(WIDTH . HEIGHT\)

The size is in pixel."
  (pdf-info-query
   'pagesize
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   (or page
       (and (eq (window-buffer)
                (current-buffer))
            (derived-mode-p 'doc-view-mode)
            (doc-view-current-page))
       1)))

(defun pdf-info-quit ()
  "Quit the epdfinfo server."
  (when (and (processp (pdf-info-process))
             (eq (process-status (pdf-info-process))
                 'run))
    (pdf-info-query 'quit)
    (tq-close pdf-info-queue)
    (setq pdf-info-queue nil)))

(defun pdf-info-getannots (&optional pages file-or-buffer)
  "Return the annotations on PAGE.

See `pdf-info--normalize-pages' for valid PAGES formats.  This
function returns the annotations for pages PAGE as a list of
alists.  A element of this list contains the following
key-value-pairs.

page     - It's page number                                         . 
edges    - It's area in relative coordinates                       . 
type     - A symbol describing it' type                             . 
id       - A document-wide unique symbol referencing this annotation . 
flags    - It's flags, binary encoded                              . 
color    - It's color in standard Emacs notation                   . 
contents - The text of this annotation                          . 
modified - The last date of modification                        . 

Additionally, if the annotation is a markup annotation, the
following:

label        - It's label.
subject      - The subject addressed.
opacity      - The level of relative opacity.
popup-edges  - The edges of a possibly associated popup window.
popup-isopen - Whether this window should be displayed open.
created      - The date this markup annotation was created.

Additionally, if the annotation is a markup text annotation, the
following:

text-icon  - A string kind of describing the purpose of this annotation.
text-state - A string, e.g. accepted or rejected." ;FIXME: Use symbols ?
  
  (let ((pages (pdf-info--normalize-pages pages)))
    (pdf-info-query
     'getannots
     (pdf-info--normalize-file-or-buffer file-or-buffer)
     (car pages)
     (cdr pages))))

(defun pdf-info-getannot (id &optional file-or-buffer)
  "Return the annotation for ID.

ID should be a symbol, which was previously returned in a
`pdf-info-getannots' query.

See `pdf-info-getannots' for the kind of return value of this
function."
  (pdf-info-query
   'getannot
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   id))

(defun pdf-info-addannot (id &optional file-or-buffer)
  "Return the annotation for ID.

ID should be a symbol, which was previously returned in a
`pdf-info-getannots' query.

See `pdf-info-getannots' for the kind of return value of this
function."
  (pdf-info-query
   'addannot
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   id))

(defun pdf-info-mvannot (id edges &optional file-or-buffer)
  "Move/Resize annotation ID to fit EDGES. 

ID should be a symbol, which was previously returned in a
`pdf-info-getannots' query."
  (apply 'pdf-info-query
         'mvannot
         (pdf-info--normalize-file-or-buffer file-or-buffer)
         id
         edges))

(defun pdf-info-editannot (id modifications &optional file-or-buffer)
  "Send modifications to annotation ID to the server."

  (let ((cmd-map '((edges . mvannot)
                   (color . setannot-color)
                   (label . setannot-markup-label)
                   (contents . setannot-contents)
                   (popup-isopen . setannot-is-open)
                   (isopen . setannot-is-open)
                   (icon . setannot-text-icon))))
    (dolist (prop (mapcar 'car modifications))
      (unless (assq prop cmd-map)
        (error "This property is read-only: %s" prop)))
    (dolist (elt modifications)
      (let ((cmd (cdr (assq (car elt) cmd-map))))
        (apply 'pdf-info-query
               cmd
               (pdf-info--normalize-file-or-buffer file-or-buffer)
               id
               (if (eq (car elt) 'edges)
                   (cdr elt)
                 (list (cdr elt))))))))

(defun pdf-info-save (&optional file-or-buffer)
  "Save FILE-OR-BUFFER.

This saves the document to a new temporary file, which is
returned."
  (pdf-info-query
   'save
   (pdf-info--normalize-file-or-buffer file-or-buffer)))
   

(defun pdf-info-getattachment-from-annot (id &optional do-save file-or-buffer)
  "Return the attachment associated with annotation ID.

ID should be a symbol which was previously returned in a
`pdf-info-getannots' query, and referencing an attachment of type
`file', otherwise the result in an error.

See `pdf-info-getattachments' for the kind of return value of this
function and the meaning of DO-SAVE."

  (pdf-info-query
   'getattachment-from-annot
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   id
   (if do-save 1 0)))

(defun pdf-info-getattachments (&optional do-save file-or-buffer)
  "Return the document level attachments.

Attachments may be associated with the document or an annotation.
If DO-SAVE is non-nil, also save the attachments data to a local
file, see below.

This function returns the former as a list of alist, where every
element has the following key-value-pairs:

name        - The filename of this attachment.
description - A description of this attachment.
size        - The size in bytes or -1 if n/a.
modified    - The last date of modification.
created     - The date of creation.
checksum    - The MD5 (?) checksum of this attachment.
file        - The name of a tempfile containing the data, only if DO-SAVE is non-nil.

If DO-SAVE is non-nil, the caller should use the
tempfile (e.g. move it) and delete it afterwards."
  (pdf-info-query
   'getattachments
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   (if do-save 1 0)))

(add-hook 'kill-emacs-hook 'pdf-info-quit)

(defconst pdf-info-writing-supported
  (memq 'write-support (pdf-info-features)))

(provide 'pdf-info)

;;; pdf-info.el ends here
