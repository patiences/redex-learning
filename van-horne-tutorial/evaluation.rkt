#lang racket
(require redex)

;; Let's avoid substitution from now on by using CLOSURES! Woohoo.
;; Value environments (like type envs) are just more association lists,
;; so we still have lookup and ext
(define-extended-language PCF⇓ PCF
    (V ::= N O (L ρ) ((μ (X : T) L) ρ)) ;; You only need closures when you've got λ guys
    (ρ ::= ((X V) ...)))

;; And here is our eval function... 
(define-judgment-form PCF⇓
    #:mode (⇓ I I I O)
    ;; Pass in our expression, our closures and get a value out 
    #:contract (⇓ M ρ : V)

    ;; Numbers eval to numbas
    [(⇓ N ρ : N)]
    ;; Ops don't bother with closures
    [(⇓ O ρ : O)]
    ;; If it's a λ, let's bind this closure to this λ
    [(⇓ L ρ : (L ρ))]
    ;; What is this?!! ?! 
    [(⇓ (μ (X_f : T_f) L) ρ : ((μ (X_f : T_f) L) ρ))]
    ;; Lookup the value of X within rho! 
    [(lookup ρ X V)
     --------------
     (⇓ X ρ : V)]
    ;; If-exp. Eval M_0 within closure, then check N = 0 or not 
    [(⇓ M_0 ρ : N)
     (where M ,(if (zero? (term N)) (term M_1) (term M_2)))
     (⇓ M ρ : V)
     ---------------------------
     (⇓ (if0 M_0 M_1 M_2) ρ : V)]
    ;; Do arithmetic operations over an arbitrary number of arguments
    ;; Evaluate all terms within the closure 
    [(⇓ M_0 ρ : O)
     (⇓ M_1 ρ : N)
     ...
     (δ (O N ...) N_1)
     -----------------------
     (⇓ (M_0 M_1 ...) ρ : N_1)]
    ;; SAY WHAT?
    [(⇓ M_0 ρ : ((λ ([X_1 : T] ...) M) ρ_1))
     (⇓ M_1 ρ : V_1)
     ...
     (⇓ M (ext ρ_1 (X_1 V_1) ...) : V)
     -----------------------------------
     (⇓ (M_0 M_1 ...) ρ : V)]
    ;; WHAT IS THIS? 
    [(⇓ M_0 ρ : (name f ((μ (X_f : T_f) (λ ([X_1 : T] ...) M)) ρ_1)))
     (⇓ M_1 ρ : V_1)
     ...
     (⇓ M (ext ρ_1 (X_f f) (X_1 V_1) ...) : V)
     -----------------------------------------
     (⇓ (M_0 M_1 ...) ρ : V)])

(test-equal (judgment-holds (⇓ fact-5 () : 120)) #t)
