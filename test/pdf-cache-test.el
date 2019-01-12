

;; * ================================================================== *
;; * Tests for pdf-cache.el
;; * ================================================================== *

(require 'pdf-cache)
(require 'ert)

(ert-deftest pdf-cache-get-image ()
  (let (pdf-cache--image-cache)
    (should-not (pdf-cache-get-image 1 1))
    (setq pdf-cache--image-cache
          (list
           (pdf-cache--make-image 1 1 "1" nil)
           (pdf-cache--make-image 2 1 "2" nil)
           (pdf-cache--make-image 3 1 "3" nil)))
    (should (equal (pdf-cache-get-image 1 1) "1"))
    (should (equal pdf-cache--image-cache
                   (list
                    (pdf-cache--make-image 1 1 "1" nil)
                    (pdf-cache--make-image 2 1 "2" nil)
                    (pdf-cache--make-image 3 1 "3" nil))))
    (should (equal (pdf-cache-get-image 2 1) "2"))
    (should (equal pdf-cache--image-cache
                   (list
                    (pdf-cache--make-image 2 1 "2" nil)
                    (pdf-cache--make-image 1 1 "1" nil)
                    (pdf-cache--make-image 3 1 "3" nil))))
    (should-not (pdf-cache-get-image 4 1))))

