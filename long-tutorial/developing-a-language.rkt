#lang racket
(require redex)
(provide Lambda)

(define-language Lambda
  (e ::= x
         (lambda (x_!_ ...) e) ; args to lambda must be distinct
         (e e ...))
  (x ::= variable-not-otherwise-mentioned))

;; We could have defined unique-vars to check for duplicate args in a λ, but we don't want circular definitions
#;
(define-metafunction Lambda
  unique-vars : x ... -> boolean
  [(unique-vars) #true]
  [(unique-vars x x_1 ... x x_2 ...) #false]
  [(unique-vars x x_1 ...) (unique-vars x_1 ...)])

(define e1 (term y))
(define e2 (term (lambda (y) y)))
(define e3 (term (lambda (x y) y)))
(define e4 (term (,e2 ,e3)))

(define lambda? (redex-match? Lambda e))

(test-equal (lambda? e1) #true)
(test-equal (lambda? e2) #true)
(test-equal (lambda? e3) #true)
(test-equal (lambda? e4) #true)
 
(define eb1 (term (lambda (x x) y)))
(define eb2 (term (lambda (x y) 3)))
 
(test-equal (lambda? eb1) #false)
(test-equal (lambda? eb2) #false)
 
(test-results)