#lang racket
(require redex)
(require "patience.rkt")

(define N5 (term 5))
(define N2 (term 2))
(define IDENTITY (term (lambda (x) x)))
(define MULT (term (lambda (x y) (* x y))))
(define MULT_5_2 (term (MULT N5 N2)))
(define ZERO? (term (if0 0 1 0)))

(test-equal (redex-match? Patience e N5) #true)
(test-equal (redex-match? Patience e N2) #true)
(test-equal (redex-match? Patience e IDENTITY) #true)
(test-equal (redex-match? Patience e MULT) #true)
(test-equal (redex-match? Patience e MULT_5_2) #true)
(test-equal (redex-match? Patience e ZERO?) #true)

(test-->> reduce ZERO? (term 1))
(test-->> reduce (term (+ 5 6 7)) (term 18))
(test-->> reduce
          (term (if0 1 2 3))
          (term 3))
(test-->> reduce (term (if0 0 (+ 0 1) (+ 0 2))) (term 1))
(test-->> reduce (term ((lambda (x) x) 1)) (term 1))
(test-->> reduce (term (+ ((lambda (x) (if0 x 10 20)) 0)
                          ((lambda (x) (if0 1 x x)) 77)))
          (+ 10 77))
(test-results)
