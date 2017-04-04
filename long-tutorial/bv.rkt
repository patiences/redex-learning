#lang racket
(require redex)
(require "developing-a-language.rkt") ;Lambda lang def
(require "subtract.rkt") ; subtract, in
(require "fv.rkt")
(provide (all-defined-out))

;; Exercise 1
;; determines the bound variables in a Lambda expression.
;; A variable x is bound in e_Lambda if x occurs in a lambda-parameter list in e_Lambda

(define-metafunction Lambda
  bv : e -> (x ...)
  [(bv x) ()] ;unbound variable
  [(bv (lambda (x ...) e))
   (x ... x_f ...)
   (where (x_f ...) (bv e))]
  [(bv (e_f e_a ...))
   (x_f ... x_a ... ...)
   (where (x_f ...) (bv e_f))
   (where ((x_a ...) ...) ((bv e_a) ...))])

(test-equal (term (bv (lambda (x) x))) (term (x)))
(test-equal (term (bv (lambda (x y z) x))) (term (x y z)))
(test-equal (term (bv (lambda (x) (lambda (y) (x y)))))
            (term (x y)))
(test-equal (term (bv ((lambda (x y) (x y))
                       (lambda (a b) (a b)))))
            (term (x y a b)))
(test-equal (term (bv ((lambda (x) x)    ;; hmmm....
                       (lambda (x) x))))
            (term (x x)))
(test-equal (term (bv x)) (term ()))
 
