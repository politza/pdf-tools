
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
  (should (= (length (pdf-info-search "Hello" pdf)) 3))
  (should (pdf-info-close pdf)))

(add-hook 'kill-emacs-hook
          (lambda nil
            (when (file-exists-p package-user-dir)
              (delete-directory package-user-dir t))))

(message "Installing package %s" (buffer-file-name))
(package-install-file (buffer-file-name))
(message "Installing pdf-tools")
(pdf-tools-install)
(message "Running ERT tests")
(ert-run-tests-batch-and-exit)


  
