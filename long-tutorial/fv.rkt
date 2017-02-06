#lang racket
(require redex)
(require "developing-a-language.rkt") ;Lambda lang def
(require "subtract.rkt") ; subtract, in
(provide fv)

;; SCOPE time! 

; (fv e) computes the sequence of free variables of e
; a variable occurrence of x is free in e 
; if no (lambda (... x ...) ...) dominates its occurrence

(define-metafunction Lambda
  fv : e -> (x ...)
  [(fv x) (x)] ; no lambda = fv 
  [(fv (lambda (x ...) e)) 
   (subtract (x_e ...) x ...) ; free vars in e - vars in λ 
   (where (x_e ...) (fv e))] ; grab the free vars in e
  [(fv (e_f e_a ...)) ; e ::= (e e ...)
   ;;               v need this third ... because you can have 0 or more (x_a ...)s
   ;;                 corresponding to each e in (e e ...)
   (x_f ... x_a ... ...) ; this list could be empty 
   (where (x_f ...) (fv e_f)) ; apply fv to the first exp 
   (where ((x_a ...) ...) ((fv e_a) ...))]) ; map fv onto rest of exps 

(test-equal (term (fv x)) (term (x)))
(test-equal (term (fv (lambda (x) x))) (term ()))
(test-equal (term (fv (lambda (x) (y z x)))) (term (y z)))

(test-results)