;;; pdf-links.el --- Handle PDF links. -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, multimedia

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

(require 'pdf-info)
(require 'pdf-util)
(require 'pdf-misc)
(require 'pdf-cache)
(require 'pdf-isearch)

;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-links nil
  "Following links in PDF documents."
  :group 'pdf-tools)

(defface pdf-links-read-link
  '((((background dark)) (:background "red" :foreground "yellow"))
    (((background light)) (:background "red" :foreground "yellow")))
  "Face used to determine the colors when reading links."
  ;; :group 'pdf-links
  :group 'pdf-tools-faces)

(defcustom pdf-links-read-link-convert-commands
  '(;;"-font" "FreeMono"
    "-pointsize" "%P"
    "-undercolor" "%f"
    "-fill" "%b"
    "-draw" "text %X,%Y '%c'")

  "The commands for the convert program, when decorating links for reading.
See `pdf-util-convert' for an explanation of the format.

Aside from the description there, two additional escape chars are
available.

%P -- The scaled font pointsize, i.e. IMAGE-WIDTH * SCALE (See
 `pdf-links-convert-pointsize-scale').
%c -- String describing the current link key (e.g. AA, AB,
 etc.)."
  :group 'pdf-links
  :type '(repeat string)
  :link '(variable-link pdf-isearch-convert-commands)
  :link '(url-link "http://www.imagemagick.org/script/convert.php"))

(defcustom pdf-links-convert-pointsize-scale 0.01
  "The scale factor for the -pointsize convert command.

This determines the relative size of the font, when interactively
reading links."
  :group 'pdf-links
  :type '(restricted-sexp :match-alternatives
                          ((lambda (x) (and (numberp x)
                                            (<= x 1)
                                            (>= x 0))))))

(defcustom pdf-links-browse-uri-function
  'org-open-link-from-string
  "The function for handling uri links.

This function should accept one argument, the URI to follow, and
do something with it."
  :group 'pdf-links
  :type 'function)


;; * ================================================================== *
;; * Minor Mode
;; * ================================================================== *

(defvar pdf-links-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "f") 'pdf-links-isearch-link)
    (define-key kmap (kbd "F") 'pdf-links-action-perform)
    kmap))

;;;###autoload
(define-minor-mode pdf-links-minor-mode
  "Handle links in PDF documents.\\<pdf-links-minor-mode-map>

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}"

  nil nil nil
  :group 'pdf-links
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-links-minor-mode
    (pdf-view-add-hotspot-function 'pdf-links-hotspots-function 0))
   (t
    (pdf-view-remove-hotspot-function 'pdf-links-hotspots-function)))
  (pdf-view-redisplay t))

(defun pdf-links-hotspots-function (page size)
  "Create hotspots for links on PAGE using SIZE."
  
  (let ((links (pdf-cache-pagelinks page))
        (id-fmt "link-%d-%d")
        (i 0)
        (pointer 'hand)
        hotspots)
    (dolist (l links)
      (let ((e (pdf-util-scale (car l) size 'round))
            (id (intern (format id-fmt page
                                (cl-incf i)))))
        (push `((rect . ((,(nth 0 e) . ,(nth 1 e))
                         . (,(nth 2 e) . ,(nth 3 e))))
                ,id
                (pointer
                 ,pointer
                 help-echo ,(pdf-links-action-to-string (cdr l))))
              hotspots)
        (local-set-key
         (vector id 'mouse-1)
         (lambda nil
           (interactive "@")
           (pdf-links-action-perform (cdr l))))
        (local-set-key
         (vector id t)
         'pdf-util-image-map-mouse-event-proxy))) 
    (nreverse hotspots)))

(defun pdf-links-action-to-string (action)
  "Return a string representation of ACTION."
  (let ((title (nth 1 action))
        (str
         (cl-case (car action)
           (goto-dest
            (let ((page (nth 2 action)))
              (if (> page 0)
                  (format "Goto page %d" page)
                "Destination not found")))
           (goto-remote
            (let* ((file (nth 2 action))
                   (page (nth 3 action))
                   (exists-p (and file (file-exists-p file))))
              (cond
               (exists-p
                (format "Goto %sfile '%s'"
                        (if (> page 0)
                            (format "p. %d of " page)
                          "")
                        file))
               (t
                (format "Link to nonexistent file '%s'" file)))))
           (launch
            (let* ((prg (nth 2 action))
                   (args (nth 3 action))
                   (exists-p (and prg (file-executable-p prg))))
              (if exists-p
                  (format "Launch '%s' with arguments '%s'"
                          prg args)
                (format "Link to nonexecutable program '%s'" prg))))
           (uri
            (let ((uri (nth 2 action)))
              (if (> (length uri) 0)
                  (format "Link to uri '%s'" uri)
                (format "Link to empty uri"))))
           (t
            (error "Invalid link-type :%s" (car action))))))
    (if (> (length title) 0)
        (concat str (format " (%s)" title))
      str)))

;;;###autoload
(defun pdf-links-action-perform (action)
  "Invoke ACTION, depending on it's type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, action is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's action is invoked.  Additionally, SPC may be used to
scroll the current page."
  (interactive
   (list (or (pdf-links-read-link-action "Activate link (SPC scrolls): ")
             (error "No link selected"))))
  (let ((type (car action))
        ;; (title (cadr action))
        )
    (cl-case type
      ((goto-dest goto-remote)
       (let (file page top)
         (cl-case type
           (goto-dest
            (setq page (nth 2 action)
                  top (nth 3 action))
            (unless (> page 0)
              (error "Link points to nowhere")))
           (goto-remote
            (setq file (nth 2 action)
                  page (nth 3 action)
                  top (nth 4 action))
            (unless (file-exists-p file)
              (error "Link points to nonexistent file %s" file))))

         (when (and file (file-exists-p file))
           (display-buffer
            (or (find-buffer-visiting file)
                (find-file-noselect file))))
         (when (derived-mode-p 'pdf-view-mode)
           (when (> page 0)
             (pdf-view-goto-page page))
           (when top
             ;; Showing the tooltip is somewhat slow.
             (sit-for 0)
             (pdf-util-tooltip-arrow top)))))
      (uri
       (funcall pdf-links-browse-uri-function (nth 2 action)))
      ;; (launch
      ;;  (shell-command (concat (nth 2 action) " " (nth 3 action))))
      (t
       (error "Invalid link:%s" action)))
    nil))
    
(defun pdf-links-read-link-action (prompt)
  "Using PROMPT, interactively read a link-action.

See `pdf-links-action-perform' for the interface."

  (pdf-util-assert-pdf-window)
  (let* ((links (pdf-cache-pagelinks
                 (pdf-view-current-page)))
         (keys (pdf-links-read-link-action--create-keys
                (length links)))
         (key-strings (mapcar (apply-partially 'apply 'string)
                              keys))
         (alist (cl-mapcar 'cons keys links))
         (size (pdf-view-image-size))
         (colors (pdf-util-face-colors
                  'pdf-links-read-link pdf-view-dark-minor-mode))
         (args (list
                :foreground (car colors)
                :background (cdr colors)
                :formats
                `((?c . ,(lambda (_edges) (pop key-strings)))
                  (?P . ,(number-to-string
                          (max 1 (* (cdr size)
                                    pdf-links-convert-pointsize-scale)))))
                :commands pdf-links-read-link-convert-commands
                :apply (pdf-util-scale-relative-to-pixel
                        (mapcar 'car links)))))
    (unless links
      (error "No links on this page"))
    (unwind-protect
        (let ((image-data
               (pdf-cache-get-image 
                (pdf-view-current-page)
                (car size) (car size) 'pdf-links-read-link-action)))
          (unless image-data
            (setq image-data (apply 'pdf-util-convert-page args ))
            (pdf-cache-put-image
             (pdf-view-current-page)
             (car size) image-data 'pdf-links-read-link-action))
          (pdf-view-display-image
           (create-image image-data (pdf-view-image-type) t))
          (cdr (pdf-links-read-link-action--read-chars prompt alist)))
      (pdf-view-redisplay))))

(defun pdf-links-read-link-action--read-chars (prompt alist)
  (catch 'done
    (let (key)
      (while t
        (let* ((chars (append (mapcar 'caar alist)
                              (mapcar 'downcase (mapcar 'caar alist))
                              (list ?\s)))
               (ch (read-char-choice prompt chars)))
          (setq ch (upcase ch))
          (cond
           ((= ch ?\s)
            (when (= (window-vscroll) (image-scroll-up))
              (image-scroll-down (window-vscroll))))
           (t
            (setq alist (delq nil (mapcar (lambda (elt)
                                            (and (eq ch (caar elt))
                                                 (cons (cdar elt)
                                                       (cdr elt))))
                                          alist))
                  key (append key (list ch))
                  prompt (concat prompt (list ch)))
            (when (= (length alist) 1)
              (message nil)
              (throw 'done (cdar alist))))))))))

(defun pdf-links-read-link-action--create-keys (n)
  (when (> n 0)
    (let ((len (1+ (floor (log n 26))))
          keys)
      (dotimes (i n)
        (let (key)
          (dotimes (_x len)
            (push (+ (% i 26) ?A) key)
            (setq i (/ i 26)))
          (push key keys)))
      (nreverse keys))))

(defun pdf-links-isearch-link ()
  (interactive)
  (let* (quit-p
         (isearch-mode-end-hook
          (cons (lambda nil
                  (setq quit-p isearch-mode-end-hook-quit))
                isearch-mode-end-hook))
         (pdf-isearch-filter-matches-function
          'pdf-links-isearch-link-filter-matches)
         (pdf-isearch-narrow-to-page t)
         (isearch-message-prefix-add "(Links)")
         pdf-isearch-batch-mode)
    (isearch-forward)
    (unless (or quit-p (null pdf-isearch-current-match))
      (let* ((page (pdf-view-current-page))
             (match  pdf-isearch-current-match)
             (size (pdf-view-image-size))
             (links (sort (cl-remove-if
                           (lambda (e)
                             (= 0 (pdf-util-edges-intersection-area (car e) match)))
                           (mapcar (lambda (l)
                                     (cons (pdf-util-scale
                                            (car l) size)
                                           (cdr l)))
                                   (pdf-cache-pagelinks page)))
                          (lambda (e1 e2)
                            (> (pdf-util-edges-intersection-area (car e1) match)
                               (pdf-util-edges-intersection-area (car e2) match))))))
        (unless links
          (error "No link found at this position"))
        (pdf-links-action-perform (cdar links))))))
                   
(defun pdf-links-isearch-link-filter-matches (matches)
  (let ((links (pdf-util-scale
                (mapcar 'car (pdf-cache-pagelinks
                              (pdf-view-current-page)))
                (pdf-view-image-size))))
    (cl-remove-if-not
     (lambda (m)
       (cl-some (lambda (l)
                  (pdf-util-with-edges (l m)
                    (let ((area (min (* l-width l-height)
                                     (* m-width m-height))))
                      (>  (/  (pdf-util-edges-intersection-area m l)
                              (float area)) 0.5))))
                links))
     matches)))

(provide 'pdf-links)



;;; pdf-links.el ends here
