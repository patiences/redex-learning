#lang racket
(require redex)
(require "developing-a-language.rkt") ;Lambda lang def
(require "subtract.rkt") ; subtract, in
(require "fv.rkt")
(provide bv)

;; Exercise 1
;; determines the bound variables in a Lambda expression.
;; A variable x is bound in e_Lambda if x occurs in a lambda-parameter list in e_Lambda

;; FIXME: there has got to be a better way to do this 
(define-metafunction Lambda
  intersection : (any ...) (any ...) -> (any ...)
  [(intersection (any_1 ...) (any_2 ...))
   (intersection_acc (any_1 ...) (any_2 ...) ())])

(define-metafunction Lambda
  intersection_acc : (any ...) (any ...) (any ...) -> (any ...)
  [(intersection_acc () () (any ...)) (any ...)]
  [(intersection_acc () (any_0 ...) (any ...)) (any ...)]
  [(intersection_acc (any_0 ...) () (any ...)) (any ...)]
  [(intersection_acc (any_0) (any_0  any_1 ...) (any ...)) (any ... any_0)]
  [(intersection_acc (any_0 any_1 ...) (any_2 ...) (any ...))
   (intersection_acc (any_1 ...) (any_2 ...) (any ... any_0)) ; add it to end of list
   (where #true (in any_0 (any_2 ...)))]
  [(intersection_acc (any_0 any_1 ...) (any_2 ...) (any ...))
   (intersection_acc (any_1 ...) (any_2 ...) (any ...))
   (where #false (in any_0 (any_2 ...)))])

(define-metafunction Lambda
  bv : e -> (x ...)
  [(bv x) ()] ;unbound variable
  [(bv (lambda (x ...) e))
   (intersection (x_e ...) (x ...))
   (where (x_e ...) (fv e))]
  [(bv (e_f e_a ...))
   (x_f ... x_a ... ...)
   (where (x_f ...) (bv e_f))
   (where ((x_a ...) ...) ((bv e_a) ...))])

(test-equal (term (bv (lambda (x) x))) (term (x)))
(test-equal (term (bv x)) (term ()))
  
(test-equal (term (intersection () ())) (term ()))
(test-equal (term (intersection () (x))) (term ()))
(test-equal (term (intersection (x y z) ())) (term ()))
(test-equal (term (intersection (x) (x))) (term (x)))
(test-equal (term (intersection (x y z) (a b c))) (term ()))
(test-equal (term (intersection (x y z) (x y))) (term (x y)))
(test-results)