#lang racket
(require redex)
(require "defining-a-language.rkt")
(require "typing.rkt")

;; define our evaluation contexts and values 
(define-extended-language Ev L+Γ
  (p (e ...)) ; program : sequence of expressions 
  (P (e ... E e ...)) ; evaluation contexts for ps (eval can occur in any e)
  (E (v E) ; reduction from left to right
     (E e) ; reduction inside application exps
     (+ v ... E e ...) 
     (if0 E e e)
     (fix E)
     hole)
  (v (λ (x t) ... e) ; λ takes multiple args! Whoa
     (fix v)
     number))

;; metafunction for summation (+ on Redex) 
(define-metafunction Ev
  Σ : number ... -> number
  [(Σ number ...)
   ,(apply + (term (number ...)))])


;; new subst rule
(require redex/tut-subst)
(define-metafunction Ev
  subst : (x v) ... e -> e
  [(subst (x v) ... e)
   ,(subst/proc x?
                (term (x ...))
                (term (v ...))
                (term e))])

(define x? (redex-match Ev x)) ; gets us a predicate that matches the pattern in Ev 

(define red
  (reduction-relation
   Ev                       ; lang 
   #:domain p               ; domain of relation
   ; in-hole is context decomposition  
   (--> (in-hole P (if0 0 e_1 e_2)) ; a program with appropriate if0 exp inside
        (in-hole P e_1) ; put e_1 into same context
        "if0t")
   (--> (in-hole P (if0 v e_1 e_2)) ; v is anything but 0
        (in-hole P e_2) 
        (side-condition (not (equal? 0 (term v))))
        "if0f")
   (--> (in-hole P ((fix (λ (x t) e)) v)) ; allows us to get recursion! 
        (in-hole P (((λ (x t) e) (fix (λ (x t) e))) v))
        "fix")
   (--> (in-hole P ((λ (x t) ..._1 e) v ..._1)) ; must have the right number of values passed to λ 
        (in-hole P (subst (x v) ... e)) ; // FIXME
        "βv") ; "beta" is standard
   (--> (in-hole P (+ number ...)) ; program using + 
        (in-hole P (Σ number ...)) ; reduce by applying Σ? 
        "+")
   (--> (e_1 ... (in-hole E (amb e_2 ...)) e_3 ...) ; list of es, one of which is amb? ;this rule can always reduce ambs first
        (e_1 ... (in-hole E e_2) ... e_3 ...) ; reduce by grabbing the first one
        "amb")))
;; 1 arg
(traces red
        (term
         (((λ (x num) (+ x 1))
           3))))

;; 3 args
(traces red
        (term
         (((λ (x num) (y num) (z num) (+ x y z))
           1 2 3))))


