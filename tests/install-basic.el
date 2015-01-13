
;; This is a very basic test, which merely checks the basic
;; functionality of the epdfinfo program.

(require 'package)

(defvar pdf (expand-file-name
             "install-test.pdf"
             (file-name-directory
              (or load-file-name default-directory))))

(setq package-user-dir (make-temp-file "pdf-tools.install-basic." t))

(ert-deftest pdf-info ()
  (should-not (pdf-info-open pdf))
  (should (= (pdf-info-number-of-pages pdf) 3))
  (should (= (length (pdf-info-search-string "Hello" nil pdf)) 3))
  (should (pdf-info-close pdf)))

(add-hook 'kill-emacs-hook
          (lambda nil
            (when (file-exists-p package-user-dir)
              (delete-directory package-user-dir t))))

(message "Installing package %s" (buffer-file-name))
(package-install-file (buffer-file-name))
(when (and (bound-and-true-p byte-compile-log-buffer)
           (buffer-live-p (get-buffer byte-compile-log-buffer)))
  (with-current-buffer byte-compile-log-buffer
    (save-excursion
      (goto-char 1)
      (when (re-search-forward ":\\(?:Error\\|Warning\\):" nil t)
        (error "Compilation had errors and/or warnings.")))))

(message "Installing pdf-tools")
(pdf-tools-install)

(message "Running ERT tests")
(ert-run-tests-batch-and-exit)


  
