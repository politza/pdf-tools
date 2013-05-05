;;; pdf-links.el --- -*- lexical-binding: t -*-

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

(require 'pdf-info)
(require 'pdf-util)
(require 'pdf-misc)
(require 'pdf-render)
(require 'pdf-isearch)

;;; Code:

;;
;; User Options
;; 

(defgroup pdf-links nil
  "Following links in PDF documents."
  :group 'pdf-tools)

(defcustom pdf-links-decorate-p t
  "If links in the document should be visualized.

If this is non-nil, the document is reconverted and it's links
are decorated utilizing the face pdf-links-link and the variable
`pdf-links-convert-commands'.

This action replaces DocView's PNG files, which means links will
still be visible, even if `pdf-links-minor-mode' is deactivated.
To get rid of them the document has to be reconvert.

Setting this value takes effect when `pdf-links-minor-mode' is
enabled.  The face colors used depend on `pdf-misc-dark-mode'."
  :group 'pdf-links
  :type 'boolean)

(defface pdf-links-link
  '((((background dark)) (:background "red"))
    (((background light)) (:background "red")))
  "Face used to determine the colors of decorated links."
  ;; :group 'pdf-links
  :group 'pdf-tools-faces)

(defface pdf-links-read-link
  '((((background dark)) (:background "red" :foreground "yellow"))
    (((background light)) (:background "red" :foreground "yellow")))
  "Face used to determine the colors when reading links."
  ;; :group 'pdf-links
  :group 'pdf-tools-faces)

(defcustom pdf-links-convert-commands
  '("-fill" "none" "-strokewidth" "1.5" "-stroke" "%b" "-draw" "line %x,%Y,%X,%Y"
    "-stroke" "none" "-strokewidth" "1")
  "The commands for the convert program, when decorating links.

See `pdf-isearch-convert-commands' for an explanation of the
format."
  :group 'pdf-links
  :type '(repeat string)
  :link '(variable-link pdf-isearch-convert-commands)
  :link '(url-link "http://www.imagemagick.org/script/convert.php"))

(defcustom pdf-links-read-link-convert-commands
  '(;; "-font" "FreeMono"
    "-pointsize" "%P"
     "-undercolor" "%f"
    "-fill" "%b"
    "-draw" "text %X,%Y '%c'")

  "The commands for the convert program, when decorating links for read.
See `pdf-isearch-convert-commands' for an explanation of the
format.

Aside from the format described there, two additional escape
chars are available.

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

(defvar pdf-links-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "f") 'pdf-links-isearch-link)
    (define-key kmap (kbd "F") 'pdf-links-do-action)
    kmap))
;;
;; Internal Variables
;; 

(defvar-local pdf-links-page-alist nil
  "Alist of pages and corresponding links.")

(defvar-local pdf-links-image-map-alist nil
  "Alist of pages and corresponding image maps.

The keys are \(PAGE . WIDTH\), where WIDTH is the corresponding
image width of the image map.")

(defvar pdf-links-id-counter 0
  "A counter for unique image :map mappings.")

;;
;; Function
;; 

;;;###autoload
(define-minor-mode pdf-links-minor-mode
  "Handle links in PDF documents.

\\<pdf-links-minor-mode-map>If this mode is enabled, most links in
the document may be activated by clicking on them or by pressing
\\[pdf-links-do-action].

Links may be visualized by customizing `pdf-links-decorate-p'
appropriately, which see.  This variable and it's effect may also
be toggled via \\[pdf-links-toggle-decoration].

\\{pdf-links-minor-mode-map}"

  nil nil nil
  :group 'pdf-links
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-links-minor-mode
    (make-local-variable 'pdf-links-decorate-p)
    (add-hook 'kill-buffer-hook 'pdf-links-read-link--clear-cache nil t)
    (pdf-render-register-layer-function 'pdf-links-render-function 0)
    (pdf-render-register-annotate-image-function
     'pdf-links-annotate-image 0))
   (t
    (pdf-render-unregister-layer-function 'pdf-links-render-function)
    (pdf-render-unregister-annotate-function 'pdf-links-annotate-image)))
  (doc-view-goto-page (doc-view-current-page)))

(defun pdf-links-toggle-decoration ()
  "Decorate or undecorate links in the current document." 
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (unless pdf-links-minor-mode
    (pdf-links-minor-mode 1))
  (setq pdf-links-decorate-p (not pdf-links-decorate-p))
  (pdf-render-redraw-document))

(defun pdf-links-after-reconvert-hook ()
  (setq pdf-links-page-alist nil
        pdf-links-image-map-alist nil))

(defun pdf-links-pagelinks (&optional page)
  "Return the cached links for page PAGE.

PAGE defaults to the current page.  See `pdf-info-pagelinks' for
the format of the return value."
  (unless page (setq page (doc-view-current-page)))
  (or (cdr (assq page pdf-links-page-alist))
      (let ((links (pdf-info-pagelinks page)))
        (push (cons page links) pdf-links-page-alist)
        links)))

(defun pdf-links-annotate-image (fn page size)
  "Merge properties for handling links on PAGE with PROPS.

This adds the :map property and defines global mouse bindings
accordingly."
  (let* ((props (funcall fn page size))
         (width (or (plist-get :width props)
                    doc-view-image-width))
         (map (cdr (assoc (cons page width)
                          pdf-links-image-map-alist))))
    (unless map
      (let* ((links (pdf-links-pagelinks page))
             (id-fmt "pdf-link-%d")
             (pointer 'hand))
        (dolist (l links)
          (let ((e (pdf-util-scale-edges (car l) size))
                (id (intern (format id-fmt (cl-incf pdf-links-id-counter)))))
            (push `((rect . ((,(nth 0 e) . ,(nth 1 e))
                             . (,(nth 2 e) . ,(nth 3 e))))
                    ,id
                    (pointer
                     ,pointer
                     help-echo ,(pdf-links-action-to-string (cdr l))))
                  map)
            (global-set-key
             (vector id 'mouse-1)
             (lambda nil
               (interactive "@")
               (pdf-links-do-action (cdr l))))
            (dolist (kind '("" "down-" "drag-"))
              (dotimes (i 9)
                (global-set-key
                 (vector id (intern (format "%smouse-%d" kind (+ i 2))))
                 'pdf-links-other-mouse-click-proxy)))))
        (setq map (nreverse map))
        (push (cons (cons page width) map)
              pdf-links-image-map-alist )))
    (plist-put props
               :map
               (append map (plist-get props :map)))))

(defun pdf-links-other-mouse-click-proxy (ev)
  (interactive "e")
  (setcar (cdr (cadr ev)) 1)
  (setq unread-command-events (list ev)))

(defun pdf-links-render-function (page)
  (when pdf-links-decorate-p
    (let* ((links (mapcar 'car (pdf-links-pagelinks page)))
           (size (pdf-util-png-image-size))
           (colors (pdf-util-face-colors
                    'pdf-links-link pdf-misc-dark-mode))
           cmds)
      (when size
        `(:foreground
                     ,(car colors)
                     :background ,(cdr colors)
                     :commands ,pdf-links-convert-commands
                     :apply ,(pdf-util-scale-edges links size))))))

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
(defun pdf-links-do-action (action)
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
         (when (derived-mode-p 'doc-view-mode)
           (when (> page 0)
             (doc-view-goto-page page))
           (when (and top
                      (pdf-util-page-displayed-p))
             (pdf-util-tooltip-arrow top 2)))))
      (uri
       (funcall pdf-links-browse-uri-function (nth 2 action)))
      ;; (launch
      ;;  (shell-command (concat (nth 2 action) " " (nth 3 action))))
      (t
       (error "Invalid link:%s" action)))
    nil))
    
(defun pdf-links-read-link-action (prompt &optional page)
  "Using PROMPT, interactively read a link-action from PAGE.

See `pdf-links-do-action' for the interface."
  (unless (pdf-util-page-displayed-p)
    (error "Page is not ready"))
  (let* ((links (pdf-links-pagelinks page))
         (in-file (pdf-util-current-image-file page))
         (out-file (pdf-util-cache-make-filename
                    "pdf-links-read-link"
                    (pdf-util-fast-image-format)
                    links))
         (keys (pdf-links-read-link-action--create-keys
                (length links)))
         (key-strings (mapcar (apply-partially 'apply 'string)
                              keys))
         (alist (cl-mapcar 'cons keys links))
         (colors (pdf-util-face-colors
                  'pdf-links-read-link pdf-misc-dark-mode)))
    (unless links
      (error "No links on this page"))
    (unless (file-exists-p out-file)
      (message "Creating image...")
      (pdf-util-convert
       in-file out-file
       :commands (list pdf-links-read-link-convert-commands
                       (cons ?c (lambda (_edges) (pop key-strings)))
                       (cons ?P
                             `(lambda (_edges)
                                ,(max 1 (* (cdr (pdf-util-png-image-size))
                                           pdf-links-convert-pointsize-scale)))))
       :foreground (car colors)
       :background (cdr colors)
       :apply (pdf-util-scale-edges
               (mapcar 'car links)
               (pdf-util-png-image-size))))
    (unwind-protect
        (progn
          (pdf-render-display-image out-file)
          (cdr (pdf-links-read-link-action--read-chars prompt alist)))
      (pdf-util-redisplay-current-page)
      ;; (pdf-links-read-link--clear-cache)
      )))

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

(defun pdf-links-read-link--clear-cache ()
  "Remove prepared image files for interactively reading links."
  (pdf-util-cache-clear "pdf-links-read-link"))

(defun pdf-links-isearch-link ()
  (interactive)
  (let* (quit-p
         (isearch-mode-end-hook
          (cons (lambda nil
                  (setq quit-p isearch-mode-end-hook-quit))
                isearch-mode-end-hook))
         (pdf-isearch-filter-matches-function
          'pdf-links-isearch-link-filter-matches)
         (isearch-message-prefix-add "(Links)"))
    (isearch-forward)
    (unless (or quit-p (null pdf-isearch-current-match))
      (let* ((match  pdf-isearch-current-match)
             (size (pdf-util-image-size))
             (links (sort (cl-remove-if (lambda (e)
                                       (= 0 (pdf-utils-edges-intersection-area (car e) match)))
                                     (mapcar (lambda (l)
                                               (cons (pdf-util-scale-edges
                                                      (car l) size)
                                                     (cdr l)))
                                             (pdf-links-pagelinks)))
                          (lambda (e1 e2)
                            (> (pdf-utils-edges-intersection-area (car e1) match)
                               (pdf-utils-edges-intersection-area (car e2) match))))))
        (unless links
          (error "No link found at this position"))
        (pdf-links-do-action (cdar links))))))
                   
(defun pdf-links-isearch-link-filter-matches (matches)
  (let ((links (pdf-util-scale-edges
                (mapcar 'car (pdf-links-pagelinks))
                (pdf-util-image-size))))
    (cl-remove-if-not
     (lambda (m)
       (cl-some (lambda (l)
                  (pdf-util-with-edges (l m)
                    (let ((area (min (* l-width l-height)
                                     (* m-width m-height))))
                      (>  (/  (pdf-utils-edges-intersection-area m l)
                              (float area)) 0.5))))
                links))
     matches)))

(provide 'pdf-links)



;;; pdf-links.el ends here
