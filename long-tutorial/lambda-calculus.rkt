#lang racket
(require redex)
(require "developing-a-language.rkt")
(require "subtract.rkt")
(require "lookup.rkt")
(require "sd.rkt")
(provide Lambda-calculus)

(define-extended-language Lambda-calculus Lambda
  (e ::= .... n) ; es can be nums! 
  (n ::= natural)
  (v ::= (lambda (x ...) e)) ; lams are vals
  
  ; a context is an expression with one hole in lieu of a sub-expression 
  (C ::=
     hole
     (e ... C e ...) ; context can be any expression 
     (lambda (x_!_ ...) C))) ; context is in the exp body 

(define Context? (redex-match? Lambda-calculus C))
(define C1 (term ((lambda (x y) x) hole 1)))
(define C2 (term ((lambda (x y) hole) 0 1)))
(test-equal (Context? C1) #true)
(test-equal (Context? C2) #true)

(test-equal (in-Lambda/n? (term (in-hole ,C1 1))) #true)
(test-equal (in-Lambda/n? (term (in-hole ,C2 1))) #true)
;; in-hole replaces the "hole" in the arg0 with arg1
(test-equal (term (in-hole ,C1 1))
            (term ((lambda (x y) x) 1 1))) ; 1 goes in the hole
(test-equal (term (in-hole ,C2 1))
            (term ((lambda (x y) 1) 0 1)))
(test-equal (term (in-hole (lambda (x y) hole) z))
            (term (lambda (x y) z)))
(test-results)