#lang racket
(require redex)
(require "developing-a-language.rkt")
(require "subst.rkt")

(define-extended-language Exceptions Lambda
  (e ::= .... n + (raise e))
  (n ::= integer))

(define-extended-language Exceptions-s Exceptions
  ; eval contexts
  (C ::= hole (e ... C e ...) (lambda (x ...) C) (raise C))
  ; term contexts 
  (E ::= hole (v ... E e ...) (raise E))
  (v ::= n + (lambda (x ...) e))) ; + is a value? 

;an exception-raising construct erases any surrounding evaluation context
(define ->βc
  (reduction-relation
   Exceptions-s
   (--> (in-hole C (in-hole E (raise v))) ; why a v? why wraped in another context?
        (in-hole C (raise v)) ; this does not further reduce 
        (where #false ,(equal? (term E) (term hole)))
        ζ)
   (--> (in-hole C (+ n_1 n_2))
        (in-hole C ,(+ (term n_1) (term n_2)))
        +)
   (--> (in-hole C ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole C (subst ([v_1 x_1] ...) e))
        β_v)))


(define c1
  (term
   ((lambda (x)
      (+ 1 (raise (+ 1 x))))
    0)))

(define c2
  (term
   (lambda (y)
     ((lambda (x)
        (+ 1 (raise (+ (raise -1) x))))
      0))))

(define c3
  (term
   (1 2 3 (raise 4) 5)))

(test-->> ->βc c1 (term (raise 1)))
(test-->> ->βc c2 (term (lambda (y) (raise -1))))
(test-->> ->βc c3 (term (raise 4)))
(test-results)
