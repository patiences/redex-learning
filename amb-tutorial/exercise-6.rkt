#lang racket
(require redex)

;; -----------------------------------------------
;; this is overly restrictive
#;
[(types Γ e num) ...
 -------------------------- ;; type-amb 
 (types Γ (amb e ...) num)]

;; we would rather have this
;; but mode spec is unsatisfied if amb has no subexps
#;
[(types Γ e t) ...
 -----------------------
 (types Γ (amb e ...) t)]
;; -----------------------------------------------

;; Exercise 6
;; Fix this by annotating amb expressions with their types

(define-language L 
  (e (e e) 
     (λ (x t) e) 
     x 
     (amb (e t) ...)  ; each e in amb has a type
     number 
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (→ t t) 
     num)
  (x variable-not-otherwise-mentioned))

; as before
(define-extended-language L+Γ L
  [Γ ((x : t) ... )]) ; now gamma is 0 or more (x : t) guys

(define-metafunction L+Γ
  [(same t_1 t_1) #t]
  [(same t_1 t_2) #f])

(define-judgment-form
  L+Γ
  #:mode (types I I O)  
  #:contract (types Γ e t)

  [-------------------- ;; type-num 
   (types Γ number num)]

  [--------------------- ;; type-var ;; FIXME 
   (types (x : t) x t)]
  
  [(types Γ e_1 t_1)
   (types Γ e_2 t_2) ...
   (side-condition (and (same t_1 t_2) ...)) 
   -----------------------
   (types Γ (amb (e_1 t_1) (e_2 t_2) ...) t_1)]) 

(test-equal
 (judgment-holds ; type-num
  (types ()
         6
         t)
  t)
 (list (term num)))

(test-equal      ; type-var ;; FIXME
 (judgment-holds
  (types ((x : num))
         x
         t)
  t)
 (list (term num)))

; num 
(test-equal
 (judgment-holds
  (types ()
         (amb (x num) (y num) (z num))
         t)
  t)
 (list (term num)))

; num -> num
(test-equal
 (judgment-holds
  (types ()
         (amb (x (→ num num)))
         t)
  t)
 (list (term (→ num num))))