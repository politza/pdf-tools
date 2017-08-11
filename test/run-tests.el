
(require 'package)
(require 'ert)
(require 'cl-lib)

(unless (= 1 (length command-line-args-left))
  (error "Missing package tar or too many arguments"))

(defvar pdf-tools-package (expand-file-name (car command-line-args-left)))

(unless (and (file-exists-p pdf-tools-package)
             (string-match "\\.tar\\'" pdf-tools-package))
  (error "Invalid tar package:" pdf-tools-package))

(unless load-file-name
  (error "load-file-name is unset"))

(cd (file-name-directory load-file-name))
(setq package-user-dir (expand-file-name "elpa" (make-temp-file "package" t)))

(defvar cask-elpa
  (cl-labels ((directory-if-exists-p (directory)
                (and (file-directory-p directory)
                     directory)))
    (or (directory-if-exists-p
         (format "../.cask/%s/elpa" emacs-version))
        (directory-if-exists-p
         (format "../.cask/%d.%d/elpa"
                 emacs-major-version emacs-minor-version))
        (error "Do `cask install' first"))))

(add-to-list 'package-directory-list cask-elpa)
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

(defmacro pdf-test-with-pdf (pdf-filename &rest body)
  (declare (indent 0) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (find-file-noselect
                     (expand-file-name ,pdf-filename)))
           (pdf-info-epdfinfo-error-filename (make-temp-file "epdfinfo.log")))
       (unwind-protect
           (progn
             (pdf-info-quit)
             (pdf-info-process-assert-running t)
             (with-current-buffer ,buffer ,@body))
         (when (buffer-live-p ,buffer)
           (with-current-buffer ,buffer
             (set-buffer-modified-p nil)
             (let (kill-buffer-hook)
               (kill-buffer))))
         (when (file-exists-p pdf-info-epdfinfo-error-filename)
           (with-temp-buffer
             (insert-file-contents pdf-info-epdfinfo-error-filename)
             (unless (= 1 (point-max))
               (message ">>> %s <<<" (buffer-string))))
           (delete-file pdf-info-epdfinfo-error-filename))
         (pdf-info-quit)))))

(defmacro pdf-test-with-test-pdf (&rest body)
  `(pdf-test-with-pdf "test.pdf" ,@body))

(defmacro pdf-test-with-encrypted-pdf (&rest body)
  `(pdf-test-with-pdf "encrypted.pdf" ,@body))

(dolist (file (directory-files "." t "\\.ert\\'"))
  (load-file file))
(ert-run-tests-batch-and-exit t)
