#lang racket
(require redex)
(provide TLambda e1 e2 e3)

(define-language TLambda
  (e ::=
     n
     +
     x
     (lambda ((x_!_ t) ...) e)
     (e e ...))
  (t ::= ;; add types! 
     int
     (t ... -> t))
  (x ::= variable-not-otherwise-mentioned))

(define lambda? (redex-match? TLambda e))

(define e1
  (term (lambda ((x int) (f (int -> int))) (+ (f (f x)) (f x)))))
(define e2
  (term (lambda ((x int) (f ((int -> int) -> int))) (f x))))
(define e3
  (term (lambda ((x int) (x (int -> int))) x))) ; duplicate arguments
(define e4
  (term (lambda ((x int) (y (int -> int))) x)))
(define e5
  (term (lambda ((y (int int int -> int))) y)))

(test-equal (lambda? e1) #true)
(test-equal (lambda? e2) #true)
(test-equal (lambda? e3) #false)
(test-equal (lambda? e4) #true)
(test-equal (lambda? e5) #true)
(test-results)