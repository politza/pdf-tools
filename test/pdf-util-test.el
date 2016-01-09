

;; * ================================================================== *
;; * Tests for pdf-util.el
;; * ================================================================== *

(require 'ert)

(ert-deftest pdf-util-seq-alignment ()
  (let ((s1 '(?a ?b ?c))
        (s2 '(?a ?b ?c ?d))
        (s3 '(-1 ?a ?b ?c))
        (s4 '(?e ?f ?g))
        (s5 '(?A ?B ?C))
        (s6 '(?b)))
    (should (equal '(2 . ((?a . ?a) (?b . ?b) (?c . ?c) (nil . ?d)))
                   (pdf-util-seq-alignment s1 s2)))

    (should (equal '(3 . ((?a . ?a) (?b . ?b) (?c . ?c) (nil . ?d)))
                   (pdf-util-seq-alignment s1 s2 nil 'prefix)))

    (should (equal '(3 . ((nil . -1) (?a . ?a) (?b . ?b) (?c . ?c)))
                   (pdf-util-seq-alignment s1 s3 nil 'suffix)))

    (should (equal '(3 . ((nil . -1) (?a . ?a) (?b . ?b) (?c . ?c)))
                   (pdf-util-seq-alignment s1 s3 nil 'infix)))

    (should (equal '(1 . ((nil . ?a) (?b . ?b) (nil . ?c)))
                   (pdf-util-seq-alignment s6 s1 nil 'infix)))
    
    (should (equal '(-3 . ((?a . ?e) (?b . ?f) (?c . ?g)))
                   (pdf-util-seq-alignment s1 s4 nil)))

    (should (equal '(3 . ((?a . ?e) (?b . ?f) (?c . ?g)))
                   (pdf-util-seq-alignment
                    s1 s4 (lambda (a b) 1))))
    (should (equal '(3 . ((?A . ?a) (?B . ?b) (?C  . ?c)))
                   (pdf-util-seq-alignment
                    s5 s1 (lambda (a b)
                            (if (equal (downcase a)
                                       (downcase b))
                                1
                              -1)))))))
        
