
(cd (file-name-directory load-file-name))

(defvar pdf-info-epdfinfo-program (expand-file-name "../server/epdfinfo"))
(unless (file-executable-p pdf-info-epdfinfo-program)
  (signal 'file-error (list 'file-executable-p pdf-info-epdfinfo-program)))
(dolist (file (directory-files "." t "\\.ert\\'"))
  (load-file file))
(ert-run-tests-batch-and-exit t)
    
