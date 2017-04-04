#lang racket
(require redex)
(require "types.rkt")

; (⊢ Γ e t) – the usual type judgment for an LC language

(define-extended-language TLambda-tc TLambda
  (Γ ::= ((x t) ...)))

(define-judgment-form TLambda-tc
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e t)
  [----------------------- "number"
                           (⊢ Γ n int)]
  
  [----------------------- "+"
                           (⊢ Γ + (int int -> int))]
  
  [----------------------- "variable"
                           (⊢ Γ x (lookup Γ x))]
  
  [(⊢ (extend Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (⊢ Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]
  
  [(⊢ Γ e_1 (t_2 ... -> t))
   (⊢ Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (⊢ Γ (e_1 e_2 ...) t)])

; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s
(define-metafunction TLambda-tc
  extend : Γ (x t) ... -> Γ
  [(extend ((x_Γ t_Γ) ...) (x t) ...) ((x t) ... (x_Γ t_Γ) ...)])

(test-equal (term (extend () (x int))) (term ((x int))))
(test-equal (term (extend ((x int)) (x int))) (term ((x int) (x int))))
(test-equal (term (extend ((x (int -> int))) (y int))) (term ((y int) (x (int -> int)))))

; (lookup Γ x) retrieves x's type from Γ
(define-metafunction TLambda-tc
  lookup : Γ x -> t
  [(lookup ((x_1 t_1) ... (x t) (x_2 t_2) ...) x)
   t
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term x))])

(test-equal (term (lookup ((x int) (x (int -> int)) (y int)) x)) (term int))
(test-equal (term (lookup ((x int) (x (int -> int)) (y int)) y)) (term int))

(test-equal (judgment-holds (⊢ () ,e1 (int (int -> int) -> int))) #true)
(test-equal (judgment-holds (⊢ () ,e2 t)) #false)
(displayln  (judgment-holds (⊢ () ,e1 t) t))
(displayln  (judgment-holds (⊢ () ,e2 t) t))
