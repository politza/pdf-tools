;; -*- lexical-binding: t -*-

(unless (version< emacs-version "24.4")
(require 'pdf-virtual)
(require 'ert)

(defvar pdf-virtual-test-document
  '(("test.pdf"
     ;; Annotations 3,4,5
     (2 . (0.1805 0.2462 0.4046 0.3392))
     ;; Should match first paragraph.
     (1 . (0.2163 0.1879 0.7848 0.22))
     4 3 5 6)))

(defmacro with-pdf-virtual-test-document (var &rest body)
  (declare (indent 1) (debug t))
  `(let ((,var (pdf-virtual-document-create
                pdf-virtual-test-document)))
     ,@body))

(defmacro with-pdf-virtual-test-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(let ((doc pdf-virtual-test-document))
     (pdf-info-process-assert-running t)
     (with-temp-buffer
       (insert ";; %VPDF 1.0\n\n")
       (let (print-length)
         (pp doc (current-buffer)))
       (pdf-virtual-view-mode)
       (progn ,@body))))

(ert-deftest pdf-virtual-document-create ()
  (let ((doc (pdf-virtual-document-create
              pdf-virtual-test-document)))
    (should (pdf-virtual-document-p doc))
    (should (= (length (pdf-virtual-document-page-array doc))
               6))
    (should (equal (pdf-virtual-document-file-map doc)
                   '(("test.pdf" 1))))))

(ert-deftest pdf-virtual-document-filenames ()
  (with-pdf-virtual-test-document doc
    (should (equal (pdf-virtual-document-filenames doc)
                   '("test.pdf")))))

(ert-deftest pdf-virtual-document-pages ()
  (with-pdf-virtual-test-document doc
    (should (equal '(("test.pdf" (4 . 4) nil)
                     ("test.pdf" (3 . 3) nil)
                     ("test.pdf" (5 . 6) nil))
                   (pdf-virtual-document-pages '(3 . 6) doc)))))

(ert-deftest pdf-virtual-document-page ()
  (with-pdf-virtual-test-document doc
    (should (equal '("test.pdf" 6 nil)
                   (pdf-virtual-document-page 6 doc)))))

(ert-deftest pdf-virtual-document-page-of ()
  (with-pdf-virtual-test-document doc
    (let ((pages '(2 1 4 3 5 6)))
      (dotimes (i (length pages))
        (should (equal (1+ i)
                       (pdf-virtual-document-page-of
                        "test.pdf" (nth i pages) nil doc)))))))

(ert-deftest pdf-virtual-open ()
  (with-pdf-virtual-test-buffer
   (should (progn (pdf-info-open) t))))

(ert-deftest pdf-virtual-close ()
  (with-pdf-virtual-test-buffer
   (should (progn (pdf-info-close) t))))

(ert-deftest pdf-virtual-metadata ()
  (with-pdf-virtual-test-buffer
   (should (consp (pdf-info-metadata)))))

(ert-deftest pdf-virtual-search ()
  (with-pdf-virtual-test-buffer
    (dolist (m (list (pdf-info-search-string "PDF" 2)
                     (pdf-info-search-regexp "PDF" 2)))
      (should (= 2 (length m)))
      (should (equal (mapcar (apply-partially 'alist-get 'page)
                             m)
                     '(2 2)))
      (should (cl-every (lambda (elt)
                          (cl-every 'pdf-test-relative-edges-p elt))
                        (mapcar (apply-partially 'alist-get 'edges)
                                m))))))

(ert-deftest pdf-virtual-pagelinks ()
  (with-pdf-virtual-test-buffer
    (let ((links (pdf-info-pagelinks 4)))
      (should (cl-every 'pdf-test-relative-edges-p
                        (mapcar (apply-partially 'alist-get 'edges)
                                links)))
      (should (equal (mapcar (apply-partially 'alist-get 'type)
                             links)
                     '(goto-dest uri)))
      (should (equal (mapcar (apply-partially 'alist-get 'uri)
                             links)
                     '(nil "http://www.gnu.org"))))))

(ert-deftest pdf-virtual-number-of-pages ()
  (with-pdf-virtual-test-buffer
   (should (= 6 (pdf-info-number-of-pages)))))

(ert-deftest pdf-virtual-outline ()
  (with-pdf-virtual-test-buffer
    (let ((outline (pdf-info-outline)))
      (should (= 8 (length outline)))
      (should (equal (mapcar (apply-partially 'alist-get 'depth)
                             outline)
                     '(1 2 2 2 2 2 3 4)))
      (should (cl-every (lambda (type)
                          (equal type 'goto-dest))
                        (mapcar (apply-partially 'alist-get 'type)
                                (cdr outline)))))))

(ert-deftest pdf-virtual-gettext ()
  (with-pdf-virtual-test-buffer
    (let ((text (pdf-info-gettext 2 '(0 0 1 1))))
      (should
       (= 2 (with-temp-buffer
              (insert text)
              (count-matches "PDF" 1 (point))))))))

(ert-deftest pdf-virtual-getselection ()
  (with-pdf-virtual-test-buffer
   (should (consp (pdf-info-getselection 1 '(0 0 1 1))))
   (should (cl-every 'pdf-test-relative-edges-p
                     (pdf-info-getselection 1 '(0 0 1 1))))))

(ert-deftest pdf-virtual-charlayout ()
  (with-pdf-virtual-test-buffer
   (let ((cl (pdf-info-charlayout 1)))
     (should (eq ?3 (car (car cl))))
     (should (eq ?y (car (car (last cl)))))
     (should (cl-every 'characterp (mapcar 'car cl)))
     (should (cl-every
              (apply-partially
               'cl-every 'pdf-test-relative-edges-p)
              (mapcar 'cdr cl))))))

(ert-deftest pdf-virtual-pagesize ()
  (with-pdf-virtual-test-buffer
    (let* ((os '(612 . 792))
           (s (pdf-info-pagesize 1))
           (ds (cons (* (- 0.4046 0.1879) (car os))
                     (* (- 0.3392 0.2462) (cdr os)))))
      (should (< (abs (- (car s) (car ds))) 10))
      (should (< (abs (- (cdr s) (cdr ds))) 10)))))

(ert-deftest pdf-virtual-getannots ()
  (with-pdf-virtual-test-buffer
    (let ((a (pdf-info-getannots 1)))
      (should (= 3 (length a)))
      (should (equal (sort (copy-sequence '(highlight underline squiggly))
                           'string<)
                     (sort (mapcar (lambda (elt)
                                     (cdr (assq 'type elt)))
                                   a)
                           'string<))))))

(ert-deftest pdf-virtual-getannot ()
  (with-pdf-virtual-test-buffer
    (let* ((a1 (car (pdf-info-getannots 1)))
           (a2 (pdf-info-getannot (cdr (assq 'id a1)))))
      (should (equal a1 a2)))))

(ert-deftest pdf-virtual-addannot ()
  (with-pdf-virtual-test-buffer
   (should-error (pdf-info-addannot 1 '(0 0 1 1) 'text))))

(ert-deftest pdf-virtual-delannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (with-pdf-virtual-test-buffer
  (should-error (pdf-info-delannot
                 (cdr (assq 'id (car (pdf-info-getannots 1))))))))

(ert-deftest pdf-virtual-mvannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (with-pdf-virtual-test-buffer
   (should-error (pdf-info-mvannot
                  (cdr (assq 'id (car (pdf-info-getannots 1))))
                  '(0 0 0 0)))))

(ert-deftest pdf-virtual-editannot ()
  (skip-unless (pdf-info-writable-annotations-p))
  (with-pdf-virtual-test-buffer
   (should-error (pdf-info-editannot
                  (cdr (assq 'id (car (pdf-info-getannots 1))))
                  '((color . "blue"))))))

(ert-deftest pdf-virtual-save ()
  (skip-unless (pdf-info-writable-annotations-p))
  (with-pdf-virtual-test-buffer
   (should-error (pdf-info-save))))

(ert-deftest pdf-virtual-adapter-argument-handling ()
  (let ((enabled-p pdf-virtual-global-minor-mode))
    (unwind-protect
        (progn
          (pdf-virtual-global-minor-mode 1)
          (with-pdf-virtual-test-buffer
            (should (stringp (pdf-info-renderpage 1 100 :alpha 0.1)))
            (should (stringp (pdf-info-renderpage
                              1 100 (current-buffer) :alpha 0.2))))
          (pdf-test-with-test-pdf
            (should (plist-get (pdf-info-setoptions
                                :render/printed t)
                               :render/printed))
            (should-not (plist-get (pdf-info-setoptions
                                    (current-buffer)
                                    :render/printed nil)
                                   :render/printed))
            (should (plist-get (pdf-info-setoptions
                                (buffer-file-name)
                                :render/printed t)
                               :render/printed))))
      (unless enabled-p
        (pdf-virtual-global-minor-mode -1)))))

  ;; (ert-deftest pdf-virtual-getattachment-from-annot ()
  ;;   )

  ;; (ert-deftest pdf-virtual-getattachments ()
  ;;   )

  ;; (ert-deftest pdf-virtual-synctex-forward-search ()
  ;;   )

  ;; (ert-deftest pdf-virtual-synctex-backward-search ()
  ;;   )

  ;; (ert-deftest pdf-virtual-renderpage ()
  ;;   )

  ;; (ert-deftest pdf-virtual-boundingbox ()
  ;;   )

  ;; (ert-deftest pdf-virtual-pagelabels ()
  ;;   )

  ;; (ert-deftest pdf-virtual-setoptions ()
  ;;   )

  ;; (ert-deftest pdf-virtual-getoptions ()
  ;;   )

  )
