#lang racket
(require redex)
;; -----------------------------------------------
;; instead of this
#;
[(types (x : t_1 Γ) e t_2)
   ----------------------------------- ;; type-lam
   (types Γ (λ (x t_1) e) (→ t_1 t_2))]

;; we would like to be able to pass multiple args to a λ
#;
[(types ((x : t_1) ...) e t_2)
   ----------------------------------- ;; type-lam
   (types Γ (λ (x t_1) ... e) (→ (t_1 ...) t_2))]
;; -----------------------------------------------

(define-language L 
  (e ::= (e e) 
     (λ (x t) ... e) ; 0 or more args
     x 
     (amb e ...)  
     number 
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t ::= (→ (t ...) t) ;; now we can type multiple args 
     num)
  (x ::= variable-not-otherwise-mentioned))

(define-extended-language L+Γ L
  [Γ ((x t) ... )]) ; now gamma is 0 or more (x : t) guys

(define-judgment-form
  L+Γ
  #:mode (types I I O)  
  #:contract (types Γ e t)

  [--------------------- ;; type-var 
   (types Γ x (lookup Γ x))]
  
  [-------------------- ;; type-num 
   (types Γ number num)]

  [(types (extend Γ (x_1 t_1) ...) e t)
   ----------------------------------- ;; type-lam
   (types Γ (λ ((x_1 t_1) ...) e) (→ (t_1 ...) t))])

(define-metafunction L+Γ ;; FIXME
  extend : Γ (x t) ... -> Γ
  [(extend ((x_Γ t_Γ) ...) (x t) ...) ((x t) ... (x_Γ t_Γ) ...)])

(define-metafunction L+Γ
  lookup : Γ x -> t
  [(lookup ((x_1 t_1) ... (x t) (x_2 t_2) ...) x)
   t
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term x))])

(test-equal
 (judgment-holds
  (types ((x num))
         x
         t)
  t)
  (list (term num)))

; still works for 1 arg?
(test-equal
 (judgment-holds
  (types ()
         (λ (x num) x)
         t)
  t)
 (list (term num)))

; multiple num args 
(test-equal
 (judgment-holds
  (types ()
         (λ (x num) (y num) (z num) x)
         (→ (t_1 ...) t_2))
  t_2)
 (list (term num)))