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
  (e (e e) 
     (λ (x t) ... e) ; 0 or more args
     x 
     (amb e ...)  
     number 
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (→ (t ...) t) ;; now we can type multiple args 
     num)
  (x variable-not-otherwise-mentioned))

(define-extended-language L+Γ L
  [Γ ((x : t) ... )]) ; now gamma is 0 or more (x : t) guys

(define-judgment-form
  L+Γ
  #:mode (types I I O)  
  #:contract (types Γ e t)

  [--------------------- ;; type-var  ;; FIXME: HOW DO WE DEFINE LOOKUP NOW??
   (types (x : t) x t)]
  
  [-------------------- ;; type-num 
   (types Γ number num)]

  [(types ((x : t_1) ...) e t_2)
   ----------------------------------- ;; type-lam
   (types Γ (λ (x t_1) ... e) (→ (t_1 ...) t_2))])

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