;;; pdf-tools.el --- Support library for PDF documents. -*- lexical-binding:t -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, multimedia
;; Package: pdf-tools
;; Version: 0.80
;; Package-Requires: ((emacs "24.3") (tablist "0.70") (let-alist "1.0.4"))

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
;; PDF Tools is, among other things, a replacement of DocView for PDF
;; files.  The key difference is, that pages are not prerendered by
;; e.g. ghostscript and stored in the file-system, but rather created
;; on-demand and stored in memory.
;;
;; Note: This package requires external libraries and works currently
;; only on GNU/Linux systems.
;;
;; Note: If you ever update it, you need to restart Emacs afterwards.
;;
;; To activate the package put
;;
;; (pdf-tools-install)
;;
;; somewhere in your .emacs.el .
;;
;; M-x pdf-tools-help RET
;;
;; gives some help on using the package and
;;
;; M-x pdf-tools-customize RET
;;
;; offers some customization options.

;; Features:
;;
;; * View
;;   View PDF documents in a buffer with DocView-like bindings.
;;
;; * Isearch
;;   Interactively search PDF documents like any other buffer. (Though
;;   there is currently no regexp support.)
;;
;; * Follow links
;;   Click on highlighted links, moving to some part of a different
;;   page, some external file, a website or any other URI.  Links may
;;   also be followed by keyboard commands.
;;
;; * Annotations
;;   Display and list text and markup annotations (like underline),
;;   edit their contents and attributes (e.g. color), move them around,
;;   delete them or create new ones and then save the modifications
;;   back to the PDF file.
;;
;; * Attachments
;;   Save files attached to the PDF-file or list them in a dired buffer.
;;
;; * Outline
;;   Use imenu or a special buffer to examine and navigate the PDF's
;;   outline.
;;
;; * SyncTeX
;;   Jump from a position on a page directly to the TeX source and
;;   vice-versa.
;;
;; * Misc
;;    + Display PDF's metadata.
;;    + Mark a region and kill the text from the PDF.
;;    + Search for occurrences of a string.
;;    + Keep track of visited pages via a history.

;;; Code:

(require 'pdf-view)
(require 'pdf-util)
(require 'pdf-info)
(require 'cus-edit)
(require 'compile)



;; * ================================================================== *
;; * Customizables
;; * ================================================================== *

(defgroup pdf-tools nil
  "Support library for PDF documents."
  :group 'doc-view)

(defgroup pdf-tools-faces nil
  "Faces determining the colors used in the pdf-tools package.

In order to customize dark and light colors use
`pdf-tools-customize-faces', or set `custom-face-default-form' to
'all."
  :group 'pdf-tools)

(defconst pdf-tools-modes
  '(pdf-history-minor-mode
    pdf-isearch-minor-mode
    pdf-links-minor-mode
    pdf-misc-minor-mode
    pdf-outline-minor-mode
    pdf-misc-size-indication-minor-mode
    pdf-misc-menu-bar-minor-mode
    pdf-annot-minor-mode
    pdf-sync-minor-mode
    pdf-misc-context-menu-minor-mode
    pdf-cache-prefetch-minor-mode
    pdf-view-auto-slice-minor-mode
    pdf-occur-global-minor-mode
    pdf-virtual-global-minor-mode))

(defcustom pdf-tools-enabled-modes
  '(pdf-history-minor-mode
    pdf-isearch-minor-mode
    pdf-links-minor-mode
    pdf-misc-minor-mode
    pdf-outline-minor-mode
    pdf-misc-size-indication-minor-mode
    pdf-misc-menu-bar-minor-mode
    pdf-annot-minor-mode
    pdf-sync-minor-mode
    pdf-misc-context-menu-minor-mode
    pdf-cache-prefetch-minor-mode
    pdf-occur-global-minor-mode
    ;; pdf-virtual-global-minor-mode
    )
  "A list of automatically enabled minor-modes.

PDF Tools is build as a series of minor-modes.  This variable and
the function `pdf-tools-install' merely serve as a convenient
wrapper in order to load these modes in current and newly created
PDF buffers."
  :group 'pdf-tools
  :type `(set ,@(mapcar (lambda (mode)
                          `(function-item ,mode))
                        pdf-tools-modes)))

(defcustom pdf-tools-enabled-hook nil
  "A hook ran after PDF Tools is enabled in a buffer."
  :group 'pdf-tools
  :type 'hook)

(defconst pdf-tools-auto-mode-alist-entry
  '("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  "The entry to use for `auto-mode-alist'.")

(defun pdf-tools-customize ()
  "Customize Pdf Tools."
  (interactive)
  (customize-group 'pdf-tools))

(defun pdf-tools-customize-faces ()
  "Customize PDF Tool's faces."
  (interactive)
  (let ((buffer (format "*Customize Group: %s*"
                        (custom-unlispify-tag-name 'pdf-tools-faces))))
    (when (buffer-live-p (get-buffer buffer))
      (with-current-buffer (get-buffer buffer)
        (rename-uniquely)))
    (customize-group 'pdf-tools-faces)
    (with-current-buffer buffer
      (set (make-local-variable 'custom-face-default-form) 'all))))


;; * ================================================================== *
;; * Initialization
;; * ================================================================== *

;;;###autoload
(defcustom pdf-tools-handle-upgrades t
  "Whether PDF Tools should handle upgrading itself."
  :group 'pdf-tools
  :type 'boolean)

;;;###autoload
(when (and pdf-tools-handle-upgrades
           (boundp 'pdf-info-epdfinfo-program)
           (stringp pdf-info-epdfinfo-program)
           (boundp 'package-user-dir)
           (stringp package-user-dir)
           (stringp load-file-name))
  (let* ((package-dir (file-name-directory load-file-name))
         (server-dir (file-name-directory pdf-info-epdfinfo-program))
         (upgrading-p (and (not (file-equal-p package-dir server-dir))
                           (file-in-directory-p package-dir package-user-dir)
                           (file-in-directory-p server-dir package-user-dir)
                           (file-executable-p pdf-info-epdfinfo-program))))
    (when upgrading-p
      (require 'cl-lib)
      (when (cl-some (lambda (buffer)
                       (and (eq 'pdf-view-mode
                                (buffer-local-value
                                 'major-mode buffer))
                            (buffer-modified-p buffer)))
                     (buffer-list))
        (when (y-or-n-p
               (concat "Warning: Upgrading will abandon ALL pdf modifications,"
                       "save some of them ?"))
          (save-some-buffers
           nil
           (lambda ()
             (and (eq 'pdf-view-mode major-mode)
                  (buffer-modified-p))))))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (eq major-mode 'pdf-view-mode)
            (set-buffer-modified-p nil)
            (fundamental-mode)
            (let ((ov (make-overlay (point-min) (point-max))))
              (overlay-put ov 'pdf-view t)
              (overlay-put ov 'display "Recompiling, stand by...")))))
      (pdf-info-quit)
      (setq pdf-info-epdfinfo-program
            (expand-file-name "epdfinfo" package-dir))
      (let ((build-hook (make-symbol "pdf-tools--upgrade")))
        (fset build-hook
              `(lambda ()
                 (remove-hook 'post-command-hook ',build-hook)
                 (let ((load-path (cons ,package-dir load-path))
                       (elc (directory-files ,package-dir nil "\\.elc\\'")))
                   ;; Recompile because package does it wrong.
                   (let ((load-suffixes '(".el")))
                     (dolist (file elc)
                       (load (file-name-sans-extension file))))
                   (byte-recompile-directory ,package-dir 0 t)
                   (dolist (file elc)
                     (load file)))
                 (pdf-tools-install 'compile 'skip-deps 'no-error)))
        (add-hook 'post-command-hook build-hook)))))

;;;###autoload
(defun pdf-tools--melpa-build-server (&optional build-directory
                                                skip-dependencies-p
                                                callback)
  "Compile the epdfinfo program in BUILD-DIRECTORY.

This is a helper function when installing via melpa.

Don't try to install dependencies if SKIP-DEPENDENCIES-P is non-nil.

CALLBACK may be a function, which will be locally put on
`compilation-finish-functions', which see."

  (if (file-executable-p pdf-info-epdfinfo-program)
      (message "%s" "Server already build.")
    (let* ((make-cmd
            (if (eq system-type 'berkeley-unix) "gmake" "make"))
           (have-apt-and-sudo
            (and (executable-find "apt-get")
                 (executable-find "sudo")))
           (install-server-deps
            (and have-apt-and-sudo
                 (not skip-dependencies-p)
                 (y-or-n-p "Should I try to install dependencies with apt-get ?")))
           (compilation-auto-jump-to-first-error nil)
           (compilation-scroll-output t)
           compilation-buffer
           (compilation-buffer-name-function
            (lambda (&rest _)
              (setq compilation-buffer
                    (generate-new-buffer-name "*compile pdf-tools*")))))
      (unless (eq system-type 'windows-nt)
        (unless (executable-find make-cmd)
          (error "Executable `%s' command not found"
                 make-cmd)))
      (unless build-directory
        (setq build-directory
              (expand-file-name
               "build"
               (file-name-directory pdf-info-epdfinfo-program))))
      (unless (file-directory-p build-directory)
        (error "No such directory: %s" build-directory))
      (if (not (eq system-type 'windows-nt))
          (compile
              (format "%s V=0 -kC '%s' %smelpa-build"
                      make-cmd
                      build-directory
                      (if install-server-deps "install-server-deps " " "))
            install-server-deps)
        (let* ((arch (if (equal "X86_64"
				(upcase (car (split-string system-configuration "-"))))
			 "MINGW64" "MINGW32"))
               (msys2-install-directory
                (file-name-directory (read-file-name "Path to msys2_shell.bat: "))))
          (compile (format "%susr/bin/bash.exe --login -c 'MSYSTEM=%s source /etc/profile; LANG=C make V=0 -kC \"%s\" melpa-build'"
                           msys2-install-directory
                           arch
                           build-directory))))
      (when (and compilation-buffer
                 (buffer-live-p (get-buffer compilation-buffer)))
        (when callback
          (with-current-buffer compilation-buffer
            (add-hook 'compilation-finish-functions callback nil t))))
      compilation-buffer)))


(defun pdf-tools-pdf-buffer-p (&optional buffer)
  "Return non-nil if BUFFER contains a PDF document."
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char 1)
        (looking-at "%PDF")))))

(defun pdf-tools-assert-pdf-buffer (&optional buffer)
  (unless (pdf-tools-pdf-buffer-p buffer)
    (error "Buffer does not contain a PDF document")))

(defun pdf-tools-set-modes-enabled (enable &optional modes)
  (dolist (m (or modes pdf-tools-enabled-modes))
    (let ((enabled-p (and (boundp m)
                          (symbol-value m))))
      (unless (or (and enabled-p enable)
                  (and (not enabled-p) (not enable)))
        (funcall m (if enable 1 -1))))))

;;;###autoload
(defun pdf-tools-enable-minor-modes (&optional modes)
  "Enable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (pdf-tools-set-modes-enabled t modes)
  (run-hooks 'pdf-tools-enabled-hook))

(defun pdf-tools-disable-minor-modes (&optional modes)
  "Disable MODES in the current buffer.

MODES defaults to `pdf-tools-enabled-modes'."
  (interactive)
  (pdf-tools-set-modes-enabled nil modes))

(declare-function pdf-occur-global-minor-mode "pdf-occur.el")
(declare-function pdf-virtual-global-minor-mode "pdf-virtual.el")

;;;###autoload
(defun pdf-tools-install (&optional force-compile-p skip-dependencies-p no-error)
  "Install PDF-Tools in all current and future PDF buffers.

See `pdf-view-mode' and `pdf-tools-enabled-modes'."
  (interactive)
  (cond
   ((not (file-executable-p pdf-info-epdfinfo-program))
    (when (or force-compile-p
              (y-or-n-p "Need to build the server, do it now ? "))
      (pdf-tools--melpa-build-server
       nil
       skip-dependencies-p
       (lambda (buffer _status)
         (when (buffer-live-p buffer)
           (display-buffer buffer))
         (when (file-executable-p pdf-info-epdfinfo-program)
           (let ((pdf-info-restart-process-p t))
             (pdf-tools-install))))))
    (funcall (if no-error 'message 'error)
             "%s" "No executable `epdfinfo' found"))
   (t
    (add-to-list 'auto-mode-alist pdf-tools-auto-mode-alist-entry)
    ;; FIXME: Generalize this sometime.
    (when (memq 'pdf-occur-global-minor-mode
                pdf-tools-enabled-modes)
      (pdf-occur-global-minor-mode 1))
    (when (memq 'pdf-virtual-global-minor-mode
                pdf-tools-enabled-modes)
      (pdf-virtual-global-minor-mode 1))
    (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (not (derived-mode-p 'pdf-view-mode))
                   (pdf-tools-pdf-buffer-p)
                   (buffer-file-name))
          (pdf-view-mode)))))))

(defun pdf-tools-uninstall ()
  "Uninstall PDF-Tools in all current and future PDF buffers."
  (interactive)
  (pdf-info-quit)
  (setq-default auto-mode-alist
    (remove pdf-tools-auto-mode-alist-entry auto-mode-alist))
  (pdf-occur-global-minor-mode -1)
  (pdf-virtual-global-minor-mode -1)
  (remove-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (pdf-util-pdf-buffer-p buf)
        (pdf-tools-disable-minor-modes pdf-tools-modes)
        (normal-mode)))))

;;;###autoload
(defun pdf-tools-help ()
  (interactive)
  (help-setup-xref (list #'pdf-tools-help)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ "PDF Tools Help\n\n")
    (princ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
    (dolist (m (cons 'pdf-view-mode
                     (sort (copy-sequence pdf-tools-modes) 'string<)))
      (princ (format "`%s' is " m))
      (describe-function-1 m)
      (terpri) (terpri)
      (princ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"))))


;; * ================================================================== *
;; * Debugging
;; * ================================================================== *

(defvar pdf-tools-debug nil
  "Non-nil, if debugging PDF Tools.")

(defun pdf-tools-toggle-debug ()
  (interactive)
  (setq pdf-tools-debug (not pdf-tools-debug))
  (when (called-interactively-p 'any)
    (message "Toggled debugging %s" (if pdf-tools-debug "on" "off"))))

(provide 'pdf-tools)

;;; pdf-tools.el ends here
