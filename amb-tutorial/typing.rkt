#lang racket
(require redex)
(require "defining-a-language.rkt")
(provide L+Γ)

;; Let's extend L to include type environments with new non-terminal Γ!
(define-extended-language L+Γ L
  [Γ · (x : t Γ)]) ; dot is empty env

(define-judgment-form
  L+Γ
  #:mode (types I I O)  ;specifies which are inputs and which is output
  #:contract (types Γ e t) 
 
  [(types Γ e_1 (→ t_2 t_3))
   (types Γ e_2 t_2)
   ------------------------- ;; type-app
   (types Γ (e_1 e_2) t_3)]
 
  [(types (x : t_1 Γ) e t_2)
   ----------------------------------- ;; type-lam
   (types Γ (λ (x t_1) e) (→ t_1 t_2))]
 
  [(types Γ e (→ (→ t_1 t_2) (→ t_1 t_2)))
   --------------------------------------- ;;type-fix??? which is the fixed point here?
   (types Γ (fix e) (→ t_1 t_2))]
 
  [--------------------- ;; type-var 
   (types (x : t Γ) x t)]

  ;; if a variable type checks in some env, then it also type checks in an extended env (provided extension does not use the var)
  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))
   ------------------------------------ ;; what rule is this? 
   (types (x_2 : t_2 Γ) x_1 t_1)]
 
  [(types Γ e num) ... ;; provided all es check against num
   ----------------------- ;; type-add 
   (types Γ (+ e ...) num)]
 
  [-------------------- ;; type-num 
   (types Γ number num)]
 
  [(types Γ e_1 num)
   (types Γ e_2 t)
   (types Γ e_3 t)
   ----------------------------- ;; type-ifzero
   (types Γ (if0 e_1 e_2 e_3) t)]

  ;; 'ambiguous' may eval + return value of any exp operand 
  [(types Γ e num) ...
   -------------------------- ;; type-amb 
   (types Γ (amb e ...) num)])

;; implement different as a metafunction using more pattern matching
(define-metafunction L+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])
