#lang racket
(require redex)
(require "developing-a-language.rkt")

(define-extended-language Assignments Lambda
  (e ::= .... n
     +
     (void)
     (set! x e))
  (n ::= natural))

; (let ((x_1 x_2) ...) e_1 e_2) binds the current value of x_2 to x_1,
(define-metafunction Assignments
  let : ((x e) ...) e e -> e
  [(let ([x_lhs e_rhs] ...) e_1 e_2)
   ((lambda (x_lhs ...)
      ((lambda (x_dummy) e_2) e_1)) ; evaluates e_1, throws away its value, and finally evaluates e_2 
    e_rhs ...)
   (where (x_dummy) ,(variables-not-in (term (e_1 e_2)) '(dummy)))])


(define-extended-language Assignments-s Assignments
  (E ::= hole (v ... E e ...) (set! x E))
  (σ ::= ((x v) ...)) 
  (v ::= n + (void) (lambda (x ...) e)))
 
; (extend σ x v) adds (x v) to the front of σ
(define-metafunction Assignments-s
  extend : σ (x ...) (any ...) -> σ
  [(extend ((x any) ...) (x_1 ...) (any_1 ...))
   ((x_1 any_1) ... (x any) ...)])
 
; —————————————————————————–
; (lookup Γ x) retrieves x's type from Γ
(define-metafunction Assignments-s
  lookup : any x -> any
  [(lookup ((x_1 any_1) ... (x any_t) (x_2 any_2) ...) x)
   any_t
   (side-condition (not (member (term x) (term (x_1 ...)))))] ; first binding of x
  [(lookup any_1 any_2)
   ,(error 'lookup "not found: ~e in: ~e" (term any_1) (term any_2))])

(define s->βs
  (reduction-relation
   Assignments-s
   #:domain (e σ) ; now expressions come packaged with a σ
   (--> [(in-hole E x) σ]
        [(in-hole E (lookup σ x)) σ]
        "id")
   (--> [(in-hole E (set! x v)) σ]
        [(in-hole E (void)) (extend σ (x) (v))]
        "set")
   (--> [(in-hole E (+ n_1 n_2)) σ]                   ;add
        [(in-hole E ,(+ (term n_1) (term n_2))) σ]
        "+")
   (--> [(in-hole E ((lambda (x ..._n) e) v ..._n)) σ]
        ; add only the arguments not already defined in σ. Why?
        [(in-hole E e) (extend σ (x_new ...) (v ...))] 
        (where (x_new ...) ,(variables-not-in (term σ) (term (x ...)))))))
