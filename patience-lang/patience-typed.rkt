#lang racket
(require redex)

;; language definition
(define-language Patience-typed
  ;; expressions 
  (e ::= x
     v
     (e e ...)
     (+ e ...)
     (if e e e)
     (or e ...)
     (and e ...)
     (not e)
     (list e ...)) ;; Grr. FIXME
  ;; variables 
  (x ::= variable-not-otherwise-mentioned)
  ;; values 
  (v ::= n
     b
     (lambda ((x_!_ t) ...) e))
  (n ::= number)
  (b ::= boolean)
  (t ::= num
     bool
     (t ... -> t)
     list)
  (Γ ::= ((x t) ...)))

(define-judgment-form Patience-typed
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e t)

  [----------------------- "number"
   (⊢ Γ n num)]
  
  [(⊢ Γ e_1 num) ...
   ----------------------- "+"
   (⊢ Γ (+ e_1 ...) (num ... -> num))] ;; FIXME 
  
  [----------------------- "variable"
   (⊢ Γ x (lookup Γ x))]
  
  [(⊢ (extend Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (⊢ Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]
  
  [(⊢ Γ e_1 (t_2 ... -> t))
   (⊢ Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (⊢ Γ (e_1 e_2 ...) t)]
  
  [(⊢ Γ e_1 boolean)
   (⊢ Γ e_2 t)
   (⊢ Γ e_3 t)
   ------------------------------------------------- "if"
   (⊢ Γ (if e_1 e_2 e_3) t)]

  [----------------------- "or"
   (⊢ Γ (or e_1 ...) bool)]

  [----------------------- "and"
   (⊢ Γ (and e_1 ...) bool)]

  [----------------------- "not"
   (⊢ Γ (not e_1 ...) bool)]

  [(⊢ Γ e_1 t) ...
   ------------------------------------------------- "list"
   (⊢ Γ (list e_1 ...) list)]) 

(define-metafunction Patience-typed
  extend : Γ (x t) ... -> Γ
  [(extend ((x_Γ t_Γ) ...) (x t) ...) ((x t) ... (x_Γ t_Γ) ...)])

(define-metafunction Patience-typed
  lookup : Γ x -> t
  [(lookup ((x_1 t_1) ... (x t) (x_2 t_2) ...) x)
   t
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term x))])

(define e1
  (term (lambda ((x num) (f (num -> num))) (+ (f (f x)) (f x)))))
(define e2
  (term (lambda ((x num) (f ((num -> num) -> num))) (f x))))
(define e3
  (term (lambda ((x num) (x (num -> num))) x)))
(test-equal (judgment-holds (⊢ () ,e1 (num (num -> num) -> num))) #true)
(test-equal (judgment-holds (⊢ () ,e2 t)) #false)