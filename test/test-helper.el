;;; test-helper --- Test helper for pdf-tools

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'package)
(require 'ert)

(unless (getenv "PACKAGE_TAR")
  (error "Missing package tar. Must be passed by PACKAGE_TAR env variable"))

(defvar pdf-tools-package (expand-file-name (getenv "PACKAGE_TAR")))

(unless (and (file-exists-p pdf-tools-package)
             (string-match "\\.tar\\'" pdf-tools-package))
  (error "Invalid tar package:" pdf-tools-package))

(unless load-file-name
  (error "load-file-name is unset"))

(cd (file-name-directory load-file-name))
(setq package-user-dir (expand-file-name "elpa" (make-temp-file "package" t)))

(defvar cask-elpa (format "../.cask/%s/elpa" emacs-version))

(unless (file-directory-p cask-elpa)
  (error "Do `cask install' first"))
(add-to-list 'package-directory-list
       (format "../.cask/%s/elpa" emacs-version))
(add-hook 'kill-emacs-hook (lambda nil
                             (when (file-exists-p package-user-dir)
                               (delete-directory package-user-dir t))))
(package-initialize)
(package-install-file pdf-tools-package)

;; FIXME: Move functions to new, loadable file.
;; Fake skipped as accepted failures if skip-unless is not available.
(unless (fboundp 'ert--skip-unless)
  (defun skip-unless (arg)
    (unless arg
      (setf (ert-test-expected-result-type
             (car ert--running-tests))
            :failed)
      (ert-fail (list nil)))))

(defun pdf-test-relative-edges-p (edges)
  (and (consp edges)
       (cl-every (lambda (x)
                   (and (numberp x)
                        (<= x 1)
                        (>= x 0)))
                 edges)))

(defmacro pdf-test-with-test-pdf (&rest body)
  (declare (indent 0) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (find-file-noselect
                     (expand-file-name "test.pdf"))))
       (pdf-info-quit)
       (pdf-info-process-assert-running t)
       (unwind-protect
           (with-current-buffer ,buffer ,@body)
         (when (buffer-live-p ,buffer)
           (set-buffer-modified-p nil)
           (kill-buffer))
         (pdf-info-quit)))))

;; ---
(require 'f)
(require 'undercover)
(undercover "lisp/*.el")
(require 'let-alist)
(require 'pdf-info)
(require 'ert)
(require 'pdf-tools)

(provide 'test-helper)
;;; test-helper.el ends here
