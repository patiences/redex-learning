#lang racket
(require redex)
(require "developing-a-language.rkt")
(require "subtract.rkt")
(require "lookup.rkt")
(require "sd.rkt")
(require "subst.rkt")
(provide (all-defined-out))

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
;; still has holes, so not in our Lambda+nums lang!  
(test-equal (in-Lambda/n? (term (in-hole ,C1 hole))) #false)
(test-equal (in-Lambda/n? (term (in-hole (lambda (x y) hole) hole))) #false)

;; in-hole replaces the "hole" in the arg0 with arg1
(test-equal (term (in-hole ,C1 1))
            (term ((lambda (x y) x) 1 1))) ; 1 goes in the hole
(test-equal (term (in-hole ,C2 1))
            (term ((lambda (x y) 1) 0 1)))
(test-equal (term (in-hole (lambda (x y) hole) z))
            (term (lambda (x y) z)))
; fill a context with a context... !!!
(test-equal (term (in-hole (lambda (x y) hole) hole))
            (term (lambda (x y) hole)))
(test-results)

;; red relations are not necessarily functions: can be one-to-many 
(define -->β
  (reduction-relation
   Lambda-calculus ; lang 
   (--> (in-hole C
                 ((lambda (x_1 ..._n) e) e_1 ..._n)) ; lambda with some # of args 
        (in-hole C
                 (subst ([e_1 x_1] ...) e))))) ; sub first expression for first arg inside e!

;; call-by-value, fewer intermediate terms because of this restriction 
(define -->βv
  (reduction-relation
   Lambda-calculus
   (--> (in-hole C ((lambda (x_1 ..._n) e) v_1 ..._n)) ; insist all sub-able exps are values
        (in-hole C (subst ([v_1 x_1] ...) e)))))

#;
(traces -->β
        (term ((lambda (x y)
                 ((lambda (f) (f (x 1 y 2)))
                  (lambda (w) 42)))
               ((lambda (x) x) (lambda (a b c) a))
               3)))

#;
(traces -->β (term ((lambda (x)
                    ((lambda (y)
                       y)
                     x))
                  z)))
(test--> -->β
           #:equiv =α/racket
           (term ((lambda (x)
                    ((lambda (y)
                       y)
                     x))
                  z))
           (term ((lambda (x) x) z)) ; sub in inner lambda first 
           (term ((lambda (y) y) z))) ; sub in outer lambda first

#;
(traces -->β (term ((lambda (x y)
                     (x 1 y 2))
                   (lambda (a b c)
                     a)
                   3)))
(test-->> -->β
            (term ((lambda (x y) ; x = (lambda (a b ...)), y = 3
                     (x 1 y 2))
                   (lambda (a b c)
                     a)
                   3))
            1)
