;;; pdf-info.el --- Extract infos from pdf-files via a helper process. -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

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
;; This library represents the Lisp side of the epdfinfo server.  This
;; program works on a command/response basis, but there should be no
;; need to understand the protocol, since every command has a
;; corresponding Lisp-function (see below under `High level
;; interface').
;;
;; Most of these functions receive a file-or-buffer argument, which
;; may be what it says and defaults to the current buffer.  Also, most
;; functions return some sort of alist, with, in most cases,
;; straight-forward key-value-pairs.  Though some may be only
;; understandable in the context of Adobe's PDF spec \(Adobe
;; PDF32000\) or the poppler documentation (e.g. annotation flags).
;;
;; If the poppler library is fairly recent (>= 0.19.4, older versions
;; have a bug, which may corrupt the document), annotations maybe
;; modified to a certain degree, deleted and text-annotations created.
;; The state of these modifications is held in the server.  In order
;; to realize, annotations retrieved or created are referenced by a
;; unique symbol.  Saving these changes creates a new file, the
;; original document is never touched.
 
;;; Todo:
;;
;; + Close documents at some time (e.g. when the buffer is killed)
;; 

;;; Code:

(require 'tq)
(require 'cl-lib)



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

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

(defcustom pdf-info-log t
  "Whether to log the communication with the server.

If this is non-nil, all communication with the epdfinfo programm
will be logged to the buffer \"*pdf-info-log*\"."
  :group 'pdf-info
  :type 'boolean)

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

(defconst pdf-info-text-annot-writable-properties
  '(color contents label icon isopen edges)
  "A list of writable annotation properties.")

(defvar pdf-info-after-close-document-hook nil
  "A hook ran after a document was closed in the server.")



;; * ================================================================== *
;; * Variables
;; * ================================================================== *

(defvar pdf-info-asynchronous nil)
(defvar pdf-info-interruptable nil)
  
(defconst pdf-info-pdf-date-regexp
  ;; Adobe PDF32000.book, 7.9.4 Dates
  (eval-when-compile
    (concat
     ;; allow for preceding garbage
     ;;"\\`"
     "[dD]:"
     "\\([0-9]\\{4\\}\\)"          ;year
     "\\(?:"
     "\\([0-9]\\{2\\}\\)"          ;month
     "\\(?:"
     "\\([0-9]\\{2\\}\\)"          ;day
     "\\(?:"
     "\\([0-9]\\{2\\}\\)"          ;hour
     "\\(?:"
     "\\([0-9]\\{2\\}\\)"          ;minutes
     "\\(?:"
     "\\([0-9]\\{2\\}\\)"          ;seconds
     "\\)?\\)?\\)?\\)?\\)?"
     "\\(?:"
     "\\([+-Zz]\\)"                ;UT delta char
     "\\(?:"
     "\\([0-9]\\{2\\}\\)"          ;UT delta hours
     "\\(?:"
     "'"
     "\\([0-9]\\{2\\}\\)"          ;UT delta minutes
     "\\)?\\)?\\)?"
     ;; "\\'"
     ;; allow for trailing garbage
     )))

(defvar pdf-info--queue t
  "Internally used transmission-queue for the server.

This variable is initially `t', telling the code starting the
server, that it never ran.")


;; * ================================================================== *
;; * Process handling
;; * ================================================================== *

(defun pdf-info-process ()
  "Return the process object or nil."
  (and pdf-info--queue
       (not (eq t pdf-info--queue))
       (tq-process pdf-info--queue)))

(defun pdf-info-process-assert-running (&optional force)
  "Assert a running process.

If it never ran, i.e. `pdf-info-process' is t, start it
unconditionally.

Otherwise, if FORCE is non-nil start it, if it is not running.
Else restart it with respect to the variable
`pdf-info-restart-process-p', which see.

If getting the process to run fails, this function throws an
error."
  (interactive "P")
  (unless (and (processp (pdf-info-process))
               (eq (process-status (pdf-info-process))
                   'run))
    (when (pdf-info-process)
      (tq-close pdf-info--queue)
      (setq pdf-info--queue nil))
    (unless (or force
                (eq pdf-info--queue t)
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
    (let* ((process-connection-type)
           (proc (start-process
                  "epdfinfo" " *epdfinfo*" pdf-info-epdfinfo-program
                  ;; FIXME: Hardcoded filename.
                  "/tmp/epdfinfo.log")))
      (with-current-buffer " *epdfinfo*"
        (erase-buffer))
      (set-process-query-on-exit-flag proc nil)
      (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
      (setq pdf-info--queue (tq-create proc))))
  pdf-info--queue)

(defadvice tq-process-buffer (around bugfix activate)
  "Fix a bug in trunk where the wrong callback gets called."
  (let ((tq (ad-get-arg 0)))
    (if (not (equal (car (process-command (tq-process tq)))
                    pdf-info-epdfinfo-program))
        ad-do-it
      (let ((buffer (tq-buffer tq)))
        (when (buffer-live-p buffer)
          (set-buffer buffer)
          (if (= 0 (buffer-size)) ()
            (if (tq-queue-empty tq)
                (let ((buf (generate-new-buffer "*spurious*")))
                  (copy-to-buffer buf (point-min) (point-max))
                  (delete-region (point-min) (point))
                  (pop-to-buffer buf nil)
                  (error "Spurious communication from process %s, see buffer %s"
                         (process-name (tq-process tq))
                         (buffer-name buf)))
              (goto-char (point-min))
              (if (re-search-forward (tq-queue-head-regexp tq) nil t)
                  (let ((answer (buffer-substring (point-min) (point)))
                        (fn (tq-queue-head-fn tq))
                        (closure (tq-queue-head-closure tq)))
                    (delete-region (point-min) (point))
                    (tq-queue-pop tq)
                    (condition-case err
                        (funcall fn closure answer)
                      (error (message "Error while processing tq callback: %s"
                                      (error-message-string err))))
                    (tq-process-buffer tq))))))))))


;; * ================================================================== *
;; * Sending and receiving
;; * ================================================================== *

(defun pdf-info-query (cmd &rest args)
  "Query the server using CMD and ARGS."
  (pdf-info-process-assert-running)
  (unless (symbolp cmd)
    (setq cmd (intern cmd)))
  (when (and pdf-info-interruptable
             (null pdf-info-asynchronous))
    (error "Synchronous requests are uncancelable"))
  (let* ((query (concat (mapconcat 'pdf-info-query--escape
                                   (cons cmd args) ":") "\n"))
         (callback
          (lambda (closure response)
            (cl-destructuring-bind (status &rest result)
                (pdf-info-query--parse-response cmd response)
              (pdf-info-query--log response)
              (let (pdf-info-asynchronous
                    pdf-info-interruptable)
                (funcall closure status result)))))
         response status done
         (closure (or pdf-info-asynchronous
                      (lambda (s r)
                        (setq status s response r done t)))))
    (pdf-info-query--log query t)
    (tq-enqueue
     pdf-info--queue query "^\\.\n" closure callback)
    (unless pdf-info-asynchronous
      (while (and (not done)
                  (eq (process-status (pdf-info-process))
                      'run))
        (accept-process-output (pdf-info-process) 0.01))
      (when (and (not done)
                 (not (eq (process-status (pdf-info-process))
                          'run)))
        (error "The epdfinfo server quit unexpectedly."))
      (cond
       ((null status) response)
       ((eq status 'error) 
        (error "epdfinfo: %s" response))
       ((eq status 'interrupted)
        (error "epdfinfo: Command was interrupted"))
       (t
        (error "internal error: invalid response status"))))))

(defun pdf-info-cancel-query (cookie)
  (unless (memq cookie
                pdf-info--cancelable-requests)
    (error "Request is unknown or already canceled"))
  (setq pdf-info--cancelable-requests
        (remove cookie pdf-info--cancelable-requests)))
  
(defun pdf-info-query--escape (arg)
  "Escape ARG for transmision to the server."
  (if (null arg)
      ""
    (with-temp-buffer
      (save-excursion (insert (format "%s" arg)))
      (while (not (eobp))
        (cond
         ((memq (char-after) '(?\\ ?:))
          (insert ?\\))
         ((eq (char-after) ?\n)
          (delete-char 1)
          (insert ?\\ ?n)
          (backward-char)))
        (forward-char))
      (buffer-string))))
  
(defun pdf-info-query--parse-response (cmd response)
  "Parse one epdfinfo RESPONSE to CMD.

Returns a cons \(STATUS . RESULT\), where STATUS is one of nil
for a regular response, error for an error \(RESULT contains the
error message\) or interrupted, i.e. the command was
interrupted."
  (with-temp-buffer
    (save-excursion (insert response))
    (cond
     ((looking-at "ERR\n")
      (forward-line)
      (cons 'error (buffer-substring-no-properties
                    (point)
                    (progn
                      (re-search-forward "^\\.\n")
                      (1- (match-beginning 0))))))
     ((looking-at "OK\n")
      (let (result)
        (forward-line)
        (while (not (looking-at "^\\.\n"))
          (push (pdf-info-query--read-record) result))
        (cons nil (pdf-info-query--transform-response
                   cmd (nreverse result)))))
     ((looking-at "INT\n")
      (cons 'interrupted nil))
     (t
      (cons 'error "Invalid server response")))))

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
  "Transform a RESPONSE to CMD into a Lisp form."
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
      (getselection
       (mapcar (lambda (line)
                 (mapcar 'string-to-number
                         (split-string (car line) " " t)))
               response))
      (features (mapcar 'intern (car response)))
      (pagesize
       (setq response (car response))
       (cons (round (string-to-number (car response)))
             (round (string-to-number (cadr response)))))
      ((getannots getannot editannot addannot)
       (funcall
        (if (eq cmd 'getannots)
            'identity
          'car)
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
             (modified . ,(pdf-info-parse-pdf-date (pop-not-empty a)))
             ,@(when a
                 `((label . ,(pop-not-empty a))
                   (subject . ,(pop-not-empty a))
                   (opacity . ,(pop a))
                   (popup-edges . ,(let ((p (pop-not-empty a)))
                                     (when p
                                       (mapcar 'string-to-number
                                               (split-string p " " t)))))
                   (popup-isopen . ,(equal (pop a) "1"))
                   (created . ,(pdf-info-parse-pdf-date (pop-not-empty a)))))
             ,@(when a
                 `((icon . ,(pop-not-empty a))
                   (state . ,(pop-not-empty a))
                   (isopen . ,(equal (pop a) "1"))))))
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
      ((synctex-forward-search boundingbox)
       (mapcar 'string-to-number (car response)))
      (synctex-backward-search
       (cons (caar response)
             (mapcar 'string-to-number (cdar response))))
      (delannot nil)
      ((save renderpage renderpage-selection) (caar response))
      (t response))))

(defun pdf-info-query--transform-action (action)
  "Transform ACTION response into a Lisp form."
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

(defun pdf-info-query--log (string &optional query-p)
  "Log STRING as query/response, depending on QUERY-P.

This is a no-op, if `pdf-info-log' is nil."
  (when pdf-info-log
    (with-current-buffer (get-buffer-create "*pdf-info-log*")
      (buffer-disable-undo)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp)
          (insert ?\n))
        (insert
         (propertize
          (format-time-string "%H:%M:%S ")
          'face
          (if query-p
              'font-lock-keyword-face
            'font-lock-function-name-face))
         string)))))



;; * ================================================================== *
;; * Utility functions
;; * ================================================================== *

(defun pdf-info--normalize-file-or-buffer (file-or-buffer)
  "Return the PDF file corresponding to FILE-OR-BUFFER.

FILE-OR-BUFFER may be nil, a PDF buffer, the name of a PDF buffer
or a PDF file."
  (unless file-or-buffer
    (setq file-or-buffer
          (cl-case major-mode
            (doc-view-mode doc-view-buffer-file-name)
            (pdf-view-mode (pdf-view-buffer-file-name))
            (t (current-buffer)))))
  (when (bufferp file-or-buffer)
    (unless (buffer-live-p file-or-buffer)
      (error "Buffer is not live :%s" file-or-buffer))
    (with-current-buffer file-or-buffer
      (unless (setq file-or-buffer
                    (cl-case major-mode
                      (doc-view-mode doc-view-buffer-file-name)
                      (pdf-view-mode (pdf-view-buffer-file-name))
                      (t (buffer-file-name))))
        (error "Buffer is not associated with any file :%s"
               (buffer-name)))))
  (unless (stringp file-or-buffer)
    (signal 'wrong-type-argument
            (list 'stringp 'bufferp 'null file-or-buffer)))
  ;; is file
  (when (file-remote-p file-or-buffer)
    (error "Processing remote files not supported :%s"
           file-or-buffer))
  ;; (unless (file-readable-p file-or-buffer)
  ;;   (error "File not readable :%s" file-or-buffer))
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

(defun pdf-info-parse-pdf-date (date)
  (when (and date
             (string-match pdf-info-pdf-date-regexp date))
    (let ((year (match-string 1 date))
          (month (match-string 2 date))
          (day (match-string 3 date))
          (hour (match-string 4 date))
          (min (match-string 5 date))
          (sec (match-string 6 date))
          (ut-char (match-string 7 date))
          (ut-hour (match-string 8 date))
          (ut-min (match-string 9 date))
          (tz 0))
      (when (or (equal ut-char "+")
                (equal ut-char "-"))
        (when ut-hour
          (setq tz (* 3600 (string-to-number ut-hour))))
        (when ut-min
          (setq tz (+ tz (* 60 (string-to-number ut-min)))))
        (when (equal ut-char "-")
          (setq tz (- tz))))
      (encode-time
       (if sec (string-to-number sec) 0)
       (if min (string-to-number min) 0)
       (if hour (string-to-number hour) 0)
       (if day (string-to-number day) 1)
       (if month (string-to-number month) 1)
       (string-to-number year)
       tz))))

(defun pdf-info-writable-annotations-p ()
  (not (null (memq 'write-annotations (pdf-info-features)))))

(defun pdf-info-assert-writable-annotations ()
  (unless (memq 'write-annotations (pdf-info-features))
    (error "Writing annotations is not supported by this version of epdfinfo")))



;; * ================================================================== *
;; * High level interface
;; * ================================================================== *

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
  (let* ((pdf (pdf-info--normalize-file-or-buffer file-or-buffer))
         (buffer (find-buffer-visiting pdf)))
    (prog1
        (pdf-info-query 'close pdf)
      (save-current-buffer
        (when (buffer-live-p buffer) (set-buffer buffer))
        (run-hooks 'pdf-info-after-close-document-hook)))))
  
(defun pdf-info-metadata (&optional file-or-buffer)
  "Extract the metadata from the document FILE-OR-BUFFER.

This returns an alist containing some information about the
document."
  (pdf-info-query
   'metadata
   (pdf-info--normalize-file-or-buffer file-or-buffer)))
               
(defun pdf-info-search (string &optional file-or-buffer pages)
  "Search for STRING in PAGES of docüment FILE-OR-BUFFER.

See `pdf-info--normalize-pages' for valid PAGES formats.

This function returns a list \(\((PAGE . MATCHES\) ... \), where
MATCHES represents a list of matches on PAGE.  Each MATCHES item
has a form of \(EDGES TEXT\), where EDGES represent the
coordinates of the match as a list of four values \(LEFT TOP
RIGHT BOTTOM\). TEXT is the matched text and may be empty, if
extracting text is not available in the server."

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
there TYPE is the type of the action, TITLE is a, possibly empty,
name for this action and ARGS a list of the action's arguments.

TYPE may be one of

goto-dest -- This is a internal link to some page.  ARGS has the
form \(PAGE TOP\), where PAGE is the page of the link and TOP
it's vertical position.

goto-remote -- This a external link to some document.  ARGS is of
the form \(PDFFILE PAGE TOP\), where PDFFILE is the filename of
the external PDF, PAGE the page number and TOP the vertical
position.

uri -- A link in form of some URI.  ARGS contains a single
element, namely the URI.

In the first two cases, PAGE may be 0 and TOP be nil, which means
these data is unspecified."
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
outline items, where DEPTH >= 1 is the depth of this element in
the tree and ACTION has the same format as in
`pdf-info-pagelinks', which see."

  (pdf-info-query
   'outline
   (pdf-info--normalize-file-or-buffer file-or-buffer)))

(defun pdf-info-gettext (page x0 y0 x1 y1 &optional selection-style
                              file-or-buffer)
  "Get text on PAGE according to edges X0, Y0, X1 and Y1.

The selection may extend over multiple lines, which works similar
to a Emacs region.

Return the text contained in the selection."

  (pdf-info-query
   'gettext
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page x0 y0 x1 y1
   (cl-case selection-style
     (glyph 0)
     (word 1)
     (line 2)
     (t 0))))

(defun pdf-info-getselection (page x0 y0 x1 y1 &optional selection-style
                                   file-or-buffer)
  "Return the edges of the selection on PAGE.

Arguments are the same as for `pdf-info-gettext'.  Return a list
of edges corresponding to the text that would be a returned by
the aforementioned function when called with the same arguments."

  (pdf-info-query
   'getselection
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page x0 y0 x1 y1
   (cl-case selection-style
     (glyph 0)
     (word 1)
     (line 2)
     (t 0))))

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
    (tq-close pdf-info--queue)
    (setq pdf-info--queue nil)))

(defun pdf-info-getannots (&optional pages file-or-buffer)
  "Return the annotations on PAGE.

See `pdf-info--normalize-pages' for valid PAGES formats.

This function returns the annotations for PAGES as a list of
alists.  Each element of this list describes one annotation and
contains the following keys.

page     - It's page number. 
edges    - It's area.
type     - A symbol describing the annotation's type.
id       - A document-wide unique symbol referencing this annotation.
flags    - It's flags, binary encoded.
color    - It's color in standard Emacs notation.
contents - The text of this annotation.
modified - The last modification date of this annotation.

Additionally, if the annotation is a markup annotation, the
following keys are present.

label        - The annotation's label.
subject      - The subject addressed.
opacity      - The level of relative opacity.
popup-edges  - The edges of a associated popup window or nil.
popup-isopen - Whether this window should be displayed open.
created      - The date this markup annotation was created.

If the annotation is also a markup text annotation, the alist
contains the following keys.

text-icon  - A string describing the purpose of this annotation.
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
`pdf-info-getannots' query.  Signal an error, if an annotation
with ID is not available.

See `pdf-info-getannots' for the kind of return value of this
function."
  (pdf-info-query
   'getannot
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   id))

(defun pdf-info-addannot (page x0 y0 x1 y1 &optional file-or-buffer)
  "Add a new text annotation to PAGE with edges X0, Y0, X1 and Y1.

See `pdf-info-getannots' for the kind of value of this function
returns."
  (pdf-info-assert-writable-annotations)
  (pdf-info-query
   'addannot
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page
   x0 y0 x1 y1))

(defun pdf-info-delannot (id &optional file-or-buffer)
  "Delete the annotation with ID in FILE-OR-BUFFER.

ID should be a symbol, which was previously returned in a
`pdf-info-getannots' query.  Signal an error, if annotation ID
does not exist."
  (pdf-info-assert-writable-annotations)
  (pdf-info-query
   'delannot
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   id))

(defun pdf-info-mvannot (id edges &optional file-or-buffer)
  "Move/Resize annotation ID to fit EDGES.

ID should be a symbol, which was previously returned in a
`pdf-info-getannots' query.  Signal an error, if annotation ID
does not exist."
  (pdf-info-editannot id `((edges . ,edges)) file-or-buffer))

(defun pdf-info-editannot (id modifications &optional file-or-buffer)
  "Send modifications to annotation ID to the server.

ID should be a symbol, which was previously returned in a
`pdf-info-getannots' query.  Signal an error, if annotation ID
does not exist.

MODIFICATIONS is an alist of properties and their new values.

The server must support modifying annotations for this to work.
See `pdf-info-text-annot-writable-properties' for a list of
modifiable properties."

  (pdf-info-assert-writable-annotations)
  (let ((properties (mapcar 'car modifications)))
    (dolist (prop properties)
      (unless (memq prop pdf-info-text-annot-writable-properties)
        (error "This property is not writable: %s" prop)))
    (let ((args (append
                 (if (memq 'edges properties)
                     (cdr (assq 'edges modifications))
                   (list nil nil nil nil))
                 (list
                  (cdr (assq 'color modifications))
                  (cdr (assq 'contents modifications))
                  (cdr (assq 'label modifications))
                  (cdr (assq 'isopen modifications))
                  (cdr (assq 'icon modifications)))))
          (setmask
           (apply 'string
                  (mapcar (lambda (p)
                            (if (memq p properties)
                                ?1 ?0))
                          '(edges color contents label isopen icon)))))
      (apply 'pdf-info-query
             'editannot
             (pdf-info--normalize-file-or-buffer file-or-buffer)
             id
             (cons setmask args)))))

(defun pdf-info-save (&optional file-or-buffer)
  "Save FILE-OR-BUFFER.

This saves the document to a new temporary file, which is
returned and owned by the caller."
  (pdf-info-assert-writable-annotations)
  (pdf-info-query
   'save
   (pdf-info--normalize-file-or-buffer file-or-buffer)))   

(defun pdf-info-getattachment-from-annot (id &optional do-save file-or-buffer)
  "Return the attachment associated with annotation ID.

ID should be a symbol which was previously returned in a
`pdf-info-getannots' query, and referencing an attachment of type
`file', otherwise an error is signalled.

See `pdf-info-getattachments' for the kind of return value of this
function and the meaning of DO-SAVE."

  (pdf-info-query
   'getattachment-from-annot
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   id
   (if do-save 1 0)))

(defun pdf-info-getattachments (&optional do-save file-or-buffer)
  "Return all document level attachments.

If DO-SAVE is non-nil, save the attachments data to a local file,
which is then owned by the caller, see below.

This function returns a list of alists, where every element
contains the following keys.

name        - The filename of this attachment.
description - A description of this attachment.
size        - The size in bytes or -1 if this is not available.
modified    - The last modification date.
created     - The date of creation.
checksum    - A MD5 checksum of this attachment's data.
file        - The name of a tempfile containing the data (only present if
              DO-SAVE is non-nil)."

  (pdf-info-query
   'getattachments
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   (if do-save 1 0)))

(defun pdf-info-synctex-forward-search (source &optional line column file-or-buffer)
  "Perform a forward search with synctex.

SOURCE should be a LaTeX buffer or the absolute filename of a
corresponding file.  LINE and COLUMN represent the position in
the buffer or file.  Finally FILE-OR-BUFFER corresponds to the
PDF document.

Returns a list of \(PAGE LEFT TOP RIGHT BOT\) of coordinates
describing the position in the PDF document corresponding to the
SOURCE location."
  
  (let ((source (if (buffer-live-p (get-buffer source))
                    (buffer-file-name (get-buffer source))
                  source)))
    (pdf-info-query
     'synctex-forward-search
     (pdf-info--normalize-file-or-buffer file-or-buffer)
     source
     (or line 1)
     (or column 1))))
                                        
(defun pdf-info-synctex-backward-search (page &optional x y file-or-buffer)
  "Perform a backward search with synctex.

This find the source location corresponding to the coordinates
\(X . Y\) on PAGE in FILE-OR-BUFFER.

Returns a list \(SOURCE LINE COLUMN\)."

  (pdf-info-query
   'synctex-backward-search
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page
   (or x 0)
   (or y 0)))

(defun pdf-info-renderpage (page width &optional file-or-buffer)
  "Render PAGE with width WIDTH.

Return the filename of the corresponding image, which is owned by
the caller."
  (pdf-info-query
   'renderpage
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page
   width 0))

(defun pdf-info-renderpage-selection (page width &optional file-or-buffer
                                           &rest selections)
  "Render PAGE with width WIDTH using SELECTIONS.

SELECTIONS is a list determining foreground and background color
and the regions to render. So each element should look like
\(FG BG \(LEFT TOP RIGHT BOT\) \(LEFT TOP RIGHT BOT\) ... \) .

For the other args see `pdf-info-renderpage'.

Return the filename of the corresponding image, which is owned by
the caller."
  
  (when (consp file-or-buffer)
    (push file-or-buffer selections)
    (setq file-or-buffer nil))
  
  (apply 'pdf-info-query
         'renderpage-selection
         (pdf-info--normalize-file-or-buffer file-or-buffer)
         page
         width 0
         (apply 'append
                (mapcar
                 (lambda (s)
                   (cl-destructuring-bind (fg bg &rest edges)
                       s
                     (cons
                      (pdf-util-hexcolor fg)
                      (cons (pdf-util-hexcolor bg)
                            (apply 'append edges)))))
                 selections))))
   

(defun pdf-info-boundingbox (page &optional file-or-buffer)
  "Return a bounding-box for PAGE.

Returns a list \(LEFT TOP RIGHT BOT\) of the corresponding
bounding-box."
  
  (pdf-info-query
   'boundingbox
   (pdf-info--normalize-file-or-buffer file-or-buffer)
   page))

(define-minor-mode pdf-info-auto-revert-minor-mode
  "Close the document, after the buffer is reverted.

This ensures that the information retrieved is not outdated, but
will abandon all changes made to annotations."

  nil nil t
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-info-auto-revert-minor-mode
    (add-hook 'after-revert-hook 'pdf-info-close nil t))
   (t
    (remove-hook 'after-revert-hook 'pdf-info-close t))))

(add-hook 'kill-emacs-hook 'pdf-info-quit)

(provide 'pdf-info)

;;; pdf-info.el ends here
