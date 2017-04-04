#lang racket
(require redex)
(require "developing-a-language.rkt")
(require "subtract.rkt")

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

(test-equal (term (intersection () ())) (term ()))
(test-equal (term (intersection () (x))) (term ()))
(test-equal (term (intersection (x y z) ())) (term ()))
(test-equal (term (intersection (x) (x))) (term (x)))
(test-equal (term (intersection (x y z) (a b c))) (term ()))
(test-equal (term (intersection (x y z) (x y)))
            (term (x y)))
(test-equal (term (intersection (x y z) (z))) (term (z)))
(test-results)
