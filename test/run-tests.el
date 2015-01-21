
(require 'package)

(unless (= 1 (length command-line-args-left))
  (error "Missing package tar or too many arguments"))

(defvar pdf-tools-package (expand-file-name (car command-line-args-left)))

(unless (and (file-exists-p pdf-tools-package)
             (string-match "\\.tar\\'" pdf-tools-package))
  (error "Invalid tar package:" pdf-tools-package))

(unless load-file-name
  (error "load-file-name is unset"))

(cd (file-name-directory load-file-name))
(setq package-user-dir (make-temp-file "package" t))
(add-hook 'kill-emacs-hook (lambda nil
                             (when (file-exists-p package-user-dir)
                               (delete-directory package-user-dir t))))
(package-initialize)
(package-install-file pdf-tools-package)
(dolist (file (directory-files "." t "\\.ert\\'"))
  (load-file file))
(ert-run-tests-batch-and-exit t)
    
