;;; pdf-isearch.el --- Isearch in pdf buffers. -*- lexical-binding: t -*-

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
;;; Todo:
;; 
;; * Add the possibility to limit the search to a range of pages.

(require 'doc-view)
(require 'cl-lib)
(require 'pdf-render)
(require 'pdf-util)
(require 'pdf-info)
(require 'pdf-misc)

;;; Code:

;;
;; *Customizable variables
;;

(defgroup pdf-isearch nil
  "Isearch in pdf buffers."
  :group 'pdf-tools)
  
(defcustom pdf-isearch-convert-commands
  '("-fuzz" "30%%" "-region" "%g"
    "-fill" "%b" "-draw" "color 0,-1 replace")
  "The commands  for the external convert program.

This should be a list of strings, possibly containing special
escape characters.  Every found match produces one such command
in the pipeline of the program.  The format is used with the
function `format-spec' and the following specs are available:

%g -- Expands to the geometry of the match, i.e. WxH+X+Y.
%f -- Expands to the foreground color.
%b -- Expands to the background color.
%x -- Expands to the left edge of the match.
%X -- Expands to the right edge of the match.
%y -- Expands to the top edge of the match.
%Y -- Expands to the bottom edge of the match.
%w -- Expands to the width of the match.
%h -- Expands to the height of the match
%W -- Expands to the width of the file image.
%H -- Expands to the height of the file image.
%s -- Expands to the matched text (FIXME: Not implemented).

Keep in mind, that every element of this list is treated as one
argument for the convert program.  Also note, that the notion of
image-size may be different between Emacs and the image on disk.
All format spec coordinates are with respect to the actual
image (the one convert operates on).

See url `http://www.imagemagick.org/script/convert.php'."
  :group 'pdf-isearch
  :type '(repeat string)
  :link '(url-link "http://www.imagemagick.org/script/convert.php"))

(defface pdf-isearch-match
  '((((background dark)) (:inherit isearch))
    (((background light)) (:inherit isearch)))
  "Face used to determine the colors of the current match."
  ;; :group 'pdf-isearch
  :group 'pdf-tools-faces)

(defface pdf-isearch-lazy
  '((((background dark)) (:inherit lazy-highlight))
    (((background light)) (:inherit lazy-highlight)))
  "Face used to determine the colors of non-current matches."
  ;; :group 'pdf-isearch
  :group 'pdf-tools-faces)

(defface pdf-isearch-batch
  '((((background dark)) (:inherit match))
    (((background light)) (:inherit match)))
  "Face used to determine the colors in `pdf-isearch-batch-mode'."
  ;; :group 'pdf-isearch
  :group 'pdf-tools-faces)

;;
;; * Internal Variables
;; 

(defvar-local pdf-isearch-page nil
  "The page that is currently searched.")

(defvar-local pdf-isearch-current-match nil
  "A list (LEFT TOP RIGHT BOT) of the current match or nil.")
  
(defvar-local pdf-isearch-matches nil
  "A list of matches of the last search.")

(defvar-local pdf-isearch-search-parameter nil
  "A list of search parameter (search-string, regex-p and case-fold).")

(defvar-local pdf-isearch-convert-process nil
  "Process used to convert images.")

;;
;; * Modes
;; 

(defvar pdf-isearch-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-s") 'isearch-forward)
    (define-key kmap (kbd "C-r") 'isearch-backward)
    (define-key kmap (kbd "C-M-s") 'doc-view-search)
    (define-key kmap (kbd "C-M-r") 'doc-view-search-backward)
    (define-key kmap (kbd "M-s o") 'pdf-occur)
    kmap)
  "Keymap used in `pdf-isearch-minor-mode'.")

(defvar pdf-isearch-active-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-d") 'pdf-misc-dark-mode)
    (define-key kmap (kbd "C-b") 'pdf-isearch-batch-mode)
    (define-key kmap (kbd "C-v") 'doc-view-scroll-up-or-next-page)
    (define-key kmap (kbd "M-v") 'doc-view-scroll-down-or-previous-page)
    kmap)
  "Keymap used in `pdf-isearch-active-mode'.

This keymap is used, when actually isearching.  For a command in
this map to work, it usually needs to have a non-nil
isearch-scroll property.")

(progn
  (put 'pdf-isearch-batch-mode 'isearch-scroll t)
  (put 'pdf-misc-dark-mode 'isearch-scroll t)
  (put 'doc-view-scroll-up-or-next-page 'isearch-scroll t)
  (put 'doc-view-scroll-down-or-previous-page 'isearch-scroll t))

;;;###autoload
(define-minor-mode pdf-isearch-minor-mode
  "Isearch mode for PDF buffer.

When this mode is enabled \\[isearch-forward], among other keys,
starts an incremental search in this PDF document.  Since this mode
uses external programs to highlight found matches via
image-processing, proceeding to the next match may be slow.

Therefore two isearch behaviours have been defined: Normal isearch and
batch mode.  The later one is a minor mode
\(`pdf-isearch-batch-mode'\), which when activated inhibits isearch
from stopping at and highlighting every single match, but rather
display them batch-wise.  Here a batch means a number of matches
currently visible in the selected window.

Performance is also greatly influenced by the kind of image the
convert program produces. This may be determined by the variable
`pdf-util-fast-image-format'.

The kind of highlighting is determined by the variable
`pdf-isearch-convert-commands' and the three faces pdf-isearch-match
\(for the current match\), pdf-isearch-lazy \(for all other matches\)
and pdf-isearch-batch \(when in batch mode\), which see.

Colors may also be influenced by the minor-mode
`pdf-misc-dark-mode'.  If this is enabled, each face's dark
colors, are used (see variable `frame-background-mode' etc.),
rather than the light ones.

\\{pdf-isearch-minor-mode-map}
While in `isearch-mode' the following keys are available.

\\{pdf-isearch-active-mode-map}"
  :group 'pdf-isearch
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-isearch-minor-mode
    (set (make-local-variable 'isearch-search-fun-function)
         (lambda nil 'pdf-isearch-search-function))
    (set (make-local-variable 'isearch-push-state-function)
         'pdf-isearch-push-state-function)
    (set (make-local-variable 'isearch-wrap-function)
         'pdf-isearch-wrap-function)
    (set (make-local-variable 'isearch-lazy-highlight) nil)
    ;; Make our commands work in isearch-mode.
    (set (make-local-variable 'isearch-allow-scroll) t)
    (set (make-local-variable 'search-exit-option)
         ;; This maybe edit or t, but edit would suppress our cmds
         ;; in isearch-other-meta-char.
         (not (not search-exit-option)))
    (when (and (boundp 'imagemagick-render-type)
               (= 0 imagemagick-render-type))
      ;; This enormously speeds up rendering.
      (setq imagemagick-render-type 1))
    (add-hook 'isearch-mode-hook 'pdf-isearch-mode-initialize nil t)
    (add-hook 'isearch-mode-end-hook 'pdf-isearch-mode-cleanup nil t)
    (add-hook 'isearch-update-post-hook 'pdf-isearch-update nil t))
   (t
    (kill-local-variable 'search-exit-option)
    (kill-local-variable 'isearch-allow-scroll)
    (kill-local-variable 'isearch-search-fun-function)
    (kill-local-variable 'isearch-push-state-function)
    (kill-local-variable 'isearch-wrap-function)
    (kill-local-variable 'isearch-lazy-highlight)
    (remove-hook 'isearch-update-post-hook 'pdf-isearch-update t)
    (remove-hook 'isearch-mode-hook 'pdf-isearch-mode-initialize t)
    (remove-hook 'isearch-mode-end-hook 'pdf-isearch-mode-cleanup t))))

(define-minor-mode pdf-isearch-active-mode
  "" nil nil nil)

(define-minor-mode pdf-isearch-batch-mode
  "Incrementally search PDF documents in batches.

If this mode is enabled, isearching does not stop at every match,
but rather moves to next one not currently visible.  This
behaviour is much faster than ordinary isearch, since far less
images have to be created."
  nil nil nil
  :group 'pdf-isearch
  (when isearch-mode
    (pdf-isearch-redisplay)
    (pdf-isearch-message
     (if pdf-isearch-batch-mode "batch mode" "isearch mode"))))

;;
;; * Isearch Interface
;;

(defvar pdf-isearch-filter-matches-function nil)

(defun pdf-isearch-search-function (string &rest _)
  "Search for STRING in the current PDF buffer.

This is a Isearch interface function."
  (when (> (length string) 0)
    (let ((same-search-p (pdf-isearch-same-search-p))
          (oldpage pdf-isearch-page)
          (matches (pdf-isearch-search-page string))
          next-match)
      ;; matches is a list of edges ((x0 y1 x1 y2) ...), sorted top to
      ;; bottom ,left to right,
      (unless isearch-forward
        (setq matches (reverse matches)))
      (when pdf-isearch-filter-matches-function
        (setq matches (funcall pdf-isearch-filter-matches-function matches)))
      ;; Where to go next ?
      (setq pdf-isearch-page (doc-view-current-page)
            pdf-isearch-matches matches
            next-match
            (pdf-isearch-next-match
             oldpage pdf-isearch-page
             pdf-isearch-current-match matches
             same-search-p
             isearch-forward)
            pdf-isearch-search-parameter
            (list string nil isearch-case-fold-search))
      (cond
       (next-match
        (setq pdf-isearch-current-match next-match)
        (pdf-isearch-hl-matches next-match matches)
        (pdf-isearch-focus-match next-match)
        ;; Is this necessary ? Does this mess up isearch ?
        (when (or (and (bobp) (not isearch-forward))
                  (and (eobp) isearch-forward))
          (goto-char (1+ (/ (buffer-size) 2))))
        ;; Signal success to isearch.
        (if isearch-forward
            (re-search-forward ".")
          (re-search-backward ".")))
       (t
        (let ((next-page (pdf-isearch-find-next-matching-page
                          string pdf-isearch-page isearch-forward t)))
          (when next-page
            (let ((pdf-render-inhibit-display t))
              (doc-view-goto-page next-page))
            (pdf-isearch-search-function string))))))))

(defun pdf-isearch-push-state-function ()
  "Push the current search state.

This is a Isearch interface function."
  (let ((hscroll (* (window-hscroll) (frame-char-width)))
        (vscroll (window-vscroll nil t))
        (parms pdf-isearch-search-parameter)
        (matches pdf-isearch-matches)
        (match pdf-isearch-current-match)
        (page pdf-isearch-page))
    (lambda (_state)
      (setq pdf-isearch-search-parameter parms
            pdf-isearch-matches matches
            pdf-isearch-current-match match
            pdf-isearch-page page)

      (let ((pdf-render-inhibit-display t))
        (doc-view-goto-page pdf-isearch-page))
      (when pdf-isearch-current-match
        (pdf-isearch-hl-matches
         pdf-isearch-current-match
         pdf-isearch-matches))
      (pdf-util-set-window-pixel-hscroll hscroll)
      (pdf-util-set-window-pixel-vscroll vscroll))))

(defun pdf-isearch-wrap-function ()
  "Go to first or last page.

This is a Isearch interface function."
  (let ((page (if isearch-forward
                  1
                (pdf-info-number-of-pages))))
    (unless (= page (doc-view-current-page))
      (let ((pdf-render-inhibit-display t))
        (doc-view-goto-page page))
      (let ((next-screen-context-lines 0))
        (if (= page 1)
            (image-scroll-down)
          (image-scroll-up)))))
  (setq pdf-isearch-current-match nil))

(defun pdf-isearch-mode-cleanup ()
  "Cleanup after exiting Isearch.

This is a Isearch interface function."
  (pdf-isearch-active-mode -1)
  (pdf-render-redisplay-current-page)
  (pdf-util-cache-clear "pdf-isearch"))

(defun pdf-isearch-mode-initialize ()
  "Initialize isearching.

This is a Isearch interface function."
  (pdf-isearch-active-mode 1)
  (setq pdf-isearch-page (doc-view-current-page)
        pdf-isearch-current-match nil
        pdf-isearch-matches nil
        pdf-isearch-search-parameter nil)
  (goto-char (1+ (/ (buffer-size) 2))))

(defun pdf-isearch-same-search-p (&optional ignore-search-string-p)
  "Return non-nil, if search parameter have not changed.

Parameter inspected are `isearch-string' (unless
IGNORE-SEARCH-STRING-P is t) and `isearch-case-fold-search'.  If
there was no previous search, this function returns t."
  (or (null pdf-isearch-search-parameter)
      (if ignore-search-string-p
          (equal (cdr pdf-isearch-search-parameter)
                 (list isearch-regexp isearch-case-fold-search))
        (equal pdf-isearch-search-parameter
               (list isearch-string isearch-regexp
                     isearch-case-fold-search)))))

(defun pdf-isearch-next-match (last-page this-page last-match
                                         all-matches continued-p
                                         forward-p)
  "Determine the next match."
  (funcall (if pdf-isearch-batch-mode
               'pdf-isearch-next-match-batch
             'pdf-isearch-next-match-isearch)
           last-page this-page last-match
           all-matches continued-p forward-p))

(defun pdf-isearch-focus-match (current-match)
  "Make the CURRENT-MATCH visible in the window."
  (funcall (if pdf-isearch-batch-mode
               'pdf-isearch-focus-match-batch
             'pdf-isearch-focus-match-isearch)
           current-match))

(defun pdf-isearch-redisplay ()
  "Redisplay the current highlighting."
  (pdf-isearch-hl-matches pdf-isearch-current-match
                          pdf-isearch-matches))

(defun pdf-isearch-update ()
  "Update search and redisplay, if necessary."
  (unless (pdf-isearch-same-search-p t)
    (setq pdf-isearch-search-parameter
          (list isearch-string nil isearch-case-fold-search)
          pdf-isearch-matches
          (pdf-isearch-search-page isearch-string))
    (pdf-isearch-redisplay)))

(defun pdf-isearch-message (fmt &rest args)
  "Like `message', but Isearch friendly."
  (unless args (setq args (list fmt) fmt "%s"))
  (let ((msg (apply 'format fmt args)))
    (if isearch-mode
        (let ((isearch-message-suffix-add
               (format " [%s]" msg)))
          (isearch-message)
          (sit-for 1))
      (message "%s" fmt))))

;;
;; * Interface to epdfinfo
;;

(defun pdf-isearch-search-page (string &optional page)
  "Search STRING on PAGE in the current window.

Returns a list of edges (LEFT TOP RIGHT BOTTOM) in image
coordinates, sorted top to bottom, then left to right."

  (unless page (setq page (doc-view-current-page)))
  (let ((case-fold-search isearch-case-fold-search))
    (pdf-util-enlarge-edges
     (pdf-util-scale-edges
      (mapcar 'car (cdar (pdf-info-search
                          string nil page)))
      (pdf-util-image-size))
     1 1)))

(defun pdf-isearch-find-next-matching-page (string page &optional
                                                   forward-p interactive-p)
  "Find STRING after or before page PAGE, according to FORWARD-P.

If INTERACTIVE-P is non-nil, give some progress feedback.
Returns the page number where STRING was found, or nil if there
is no such page."
  ;; Do a exponentially expanding search.
  (let* ((case-fold-search isearch-case-fold-search)
         (incr 1)
         (pages (if forward-p
                    (cons (1+ page)
                          (1+ page))
                  (cons (1- page)
                        (1- page))))
         ;;doc-view may still convert, query the server instead.
         (final-page (and forward-p (pdf-info-number-of-pages)))
         matched-page
         reporter)

    (while (and (null matched-page)
                (or (and forward-p
                         (<= (car pages) final-page))
                    (and (not forward-p)
                         (>= (cdr pages) 1))))
      (let ((matches (pdf-info-search string nil pages)))
        (setq matched-page (if forward-p
                               (caar matches)
                             (caar (last matches)))))
      ;; (logger "%s %s %s" pages matched-page pdf-isearch-search-parameter)
      (setq incr (* incr 2))
      (cond (forward-p
             (setcar pages (1+ (cdr pages)))
             (setcdr pages (min final-page
                                (+ (cdr pages) incr))))
            (t
             (setcdr pages (1- (car pages)))
             (setcar pages (max 1 (- (car pages)
                                     incr)))))
      (when interactive-p
        (when (and (not reporter)
                   (= incr 8)) ;;Don't bother right away.
          (setq reporter
                (apply
                 'make-progress-reporter "Searching"
                 (if forward-p
                     (list (car pages) final-page nil 0)
                   (list 1 (cdr pages) nil 0)))))
        (when reporter
          (progress-reporter-update
           reporter (if forward-p
                        (- (cdr pages) page)
                      (- page (car pages)))))))
    matched-page))

;;
;; * Isearch Behavior
;; 

(defun pdf-isearch-next-match-isearch (last-page this-page last-match
                                                 matches same-search-p
                                                 forward)
  "Default function for choosing the next match.

Implements default isearch behaviour, i.e. it stops at every
match."
  (cond
   ((null last-match)
    ;; Goto first match from top or bottom of the window.
    (let* ((iedges (pdf-util-image-edges-in-window))
           (pos (pdf-util-with-edges (iedges)
                  (if forward
                      (cons iedges-left iedges-top)
                    (cons iedges-right iedges-bot)))))
      (pdf-isearch-nearest-match pos matches forward)))
   ((not (eq last-page this-page))
    ;; First match from top-left or bottom-right of the new
    ;; page.
    (car matches))
   (same-search-p
    ;; Next match after the last one.
    (if last-match
        (cadr (member last-match matches))))
   (matches
    ;; Next match of new search closest to the last one.
    (pdf-isearch-nearest-match
     last-match matches forward))))
  
(defun pdf-isearch-focus-match-isearch (match &optional eager-p)
  "Make the image area in MATCH visible in the selected window."

  (unless (image-get-display-property)
    (error "No image found in buffer"))
  (let ((hscroll (pdf-util-required-hscroll match eager-p))
        (vscroll (pdf-util-required-vscroll match eager-p)))
    (when hscroll
      (pdf-util-set-window-pixel-hscroll hscroll))
    (when vscroll
      (pdf-util-set-window-pixel-vscroll vscroll)))
  nil)

(defun pdf-isearch-next-match-batch (last-page this-page last-match
                                               matches same-search-p
                                               forward-p)
  "Select the next match, unseen in the current search direction."

  (if (or (null last-match)
          (not same-search-p)
          (not (eq last-page this-page)))
      (pdf-isearch-next-match-isearch
       last-page this-page last-match matches same-search-p forward-p)
    (let ((iedges (pdf-util-image-edges-in-window)))
      (pdf-util-with-edges (match iedges)
        (car (cl-remove-if
              ;; Filter matches visible on screen.
              (lambda (match)
                (and (<= match-right iedges-right)
                     (<= match-bot iedges-bot)
                     (>= match-left iedges-left)
                     (>= match-top iedges-top)))
              (cdr (member last-match matches))))))))

(defun pdf-isearch-focus-match-batch (match)
  "Make the image area in MATCH eagerly visible in the selected window."
  (pdf-isearch-focus-match-isearch match t))

;;
;; * Highlighting matches
;;

(defun pdf-isearch-current-colors ()
  "Return the current color set.

The return value depends on `pdf-misc-dark-mode' and
`pdf-isearch-batch-mode'.  It is a list of four colors \(MATCH-FG
MATCH-BG LAZY-FG LAZY-BG\)."
  (let ((dark-p pdf-misc-dark-mode))
    (cond
     (pdf-isearch-batch-mode
      (let ((colors (pdf-util-face-colors 'pdf-isearch-batch dark-p)))
        (list (car colors)
              (cdr colors)
              (car colors)
              (cdr colors))))
     (t
      (let ((match (pdf-util-face-colors 'pdf-isearch-match dark-p))
            (lazy (pdf-util-face-colors 'pdf-isearch-lazy dark-p)))
        (list (car match)
              (cdr match)
              (car lazy)
              (cdr lazy)))))))
  
(defun pdf-isearch-hl-matches (current matches)
  "Highlighting edges CURRENT and MATCHES using the convert program."
  (let* ((page (doc-view-current-page))
         (out-file (pdf-util-cache-make-filename
                    "pdf-isearch"
                    (pdf-util-fast-image-format)
                    page current
                    pdf-isearch-convert-commands
                    pdf-misc-dark-mode
                    pdf-isearch-batch-mode
                    pdf-isearch-search-parameter))
         if-size)
    (cond
     ((file-exists-p out-file)
      (pdf-render-display-image out-file))
     ((and (pdf-util-page-displayed-p)
           (setq if-size (pdf-util-png-image-size)))
      (let* ((image (doc-view-current-image))
             (window (selected-window))
             (buffer (current-buffer))
             (size (pdf-util-image-size))
             (colors (pdf-isearch-current-colors))
             (in-file (pdf-render-image-file page)))
        (let ((scale
               (cons (/ (car if-size) (float (car size)))
                     (/ (cdr if-size) (float (cdr size))))))
          (when pdf-isearch-convert-process
            (delete-process pdf-isearch-convert-process))
          (setq pdf-isearch-convert-process
                (pdf-util-convert-asynch
                 in-file out-file
                 :commands pdf-isearch-convert-commands
                 :foreground (nth 0 colors)
                 :background (nth 1 colors)
                 :apply (and current
                             (list (pdf-util-scale-edges current scale)))
                 :foreground (nth 2 colors)
                 :background (nth 3 colors)
                 :apply (and matches
                             (pdf-util-scale-edges
                              (remove current matches) scale))
                 (lambda (_ status)
                   (cond
                    ((not (equal status "finished\n"))
                     (when (file-exists-p out-file)
                       (delete-file out-file)))
                    (t
                     (when (and (buffer-live-p buffer)
                                (window-live-p window)
                                (eq (window-buffer window)
                                    buffer))
                       (with-selected-window window
                         (when (and (eq major-mode 'doc-view-mode)
                                    isearch-mode
                                    (eq image (doc-view-current-image)))
                           (pdf-render-display-image out-file))))))))))))
     (t (pdf-isearch-message "Unable to display")))))

;;
;; * Utility functions and macros
;;

(defun pdf-isearch-nearest-match (match-or-pos list-of-matches
                                               &optional forward-p)
  "Find the nearest element to MATCH-OR-POS in LIST-OF-MATCHES.

The direction in which to look is determined by FORWARD-P.

MATCH-OR-POS is either a list of edges or a cons (X . Y).
LIST-OF-MATCHES is assumed to be ordered with respect to
FORWARD-P."

  (let ((match (if (not (consp (cdr match-or-pos)))
                   (list (car match-or-pos) (cdr match-or-pos)
                         (car match-or-pos) (cdr match-or-pos))
                 match-or-pos))
        found edges)
    (pdf-util-with-edges (match)
      (while (and (not found)
                  list-of-matches)
        (setq edges (car list-of-matches)
              list-of-matches (cdr list-of-matches))
        (pdf-util-with-edges (edges)
          (when (or (and forward-p
                         (or (>= edges-top match-bot)
                             (and (>= edges-top match-top)
                                  (>= edges-right match-right))))
                    (and (null forward-p)
                         (or (<= edges-bot match-top)
                             (and (<= edges-top match-top)
                                  (<= edges-left match-left)))))
            (setq found edges)))))
    found))

;; Redefinition: This isearch-search function is debugable.
;; (defun isearch-search ()
;;   ;; Do the search with the current search string.
;;   (if isearch-message-function
;;       (funcall isearch-message-function nil t)
;;     (isearch-message nil t))
;;   (if (and (eq isearch-case-fold-search t) search-upper-case)
;;       (setq isearch-case-fold-search
;;             (isearch-no-upper-case-p isearch-string isearch-regexp)))
;;   (condition-case lossage
;;       (let ((inhibit-point-motion-hooks
;;              ;; FIXME: equality comparisons on functions is asking for trouble.
;;              (and (eq isearch-filter-predicate 'isearch-filter-visible)
;;                   search-invisible))
;;             (inhibit-quit nil)
;;             (case-fold-search isearch-case-fold-search)
;;             (retry t))
;;         (setq isearch-error nil)
;;         (while retry
;;           (setq isearch-success
;;                 (isearch-search-string isearch-string nil t))
;;           ;; Clear RETRY unless the search predicate says
;;           ;; to skip this search hit.
;;           (if (or (not isearch-success)
;;                   (bobp) (eobp)
;;                   (= (match-beginning 0) (match-end 0))
;;                   (funcall isearch-filter-predicate
;;                            (match-beginning 0) (match-end 0)))
;;               (setq retry nil)))
;;         (setq isearch-just-started nil)
;;         (if isearch-success
;;             (setq isearch-other-end
;;                   (if isearch-forward (match-beginning 0) (match-end 0)))))

;;     (quit (isearch-unread ?\C-g)
;;           (setq isearch-success nil))

;;     (invalid-regexp
;;      (setq isearch-error (car (cdr lossage)))
;;      (if (string-match
;;           "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
;;           isearch-error)
;;          (setq isearch-error "incomplete input")))

;;     (search-failed
;;      (setq isearch-success nil)
;;      (setq isearch-error (nth 2 lossage)))

;;     ;; (error
;;     ;;  ;; stack overflow in regexp search.
;;     ;;  (setq isearch-error (format "%s" lossage)))
;;     )

;;   (if isearch-success
;;       nil
;;     ;; Ding if failed this time after succeeding last time.
;;     (and (isearch--state-success (car isearch-cmds))
;;          (ding))
;;     (if (functionp (isearch--state-pop-fun (car isearch-cmds)))
;;         (funcall (isearch--state-pop-fun (car isearch-cmds))
;;                  (car isearch-cmds)))
;;     (goto-char (isearch--state-point (car isearch-cmds)))))


(provide 'pdf-isearch)

;;; pdf-isearch.el ends here
