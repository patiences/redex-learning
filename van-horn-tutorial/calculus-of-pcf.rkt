#lang racket
(require redex)

;; defines a binary relation on terms in a given domain (terms M in this case)
(define r
    (reduction-relation
     PCF #:domain M

     ;; form of a clause:
     ;; left hand side, right hand side, side conditions, name
     ;; order irrelevant, and may overlap! 
     ;; μ clause, substitute (X (μ (X : T) M)) for M
     ;; What the heck is this??? 
     (--> (μ (X : T) M)
          (subst (X (μ (X : T) M)) M)
          μ)
     ;; β clause, substitute X for M inside this function application 
     (--> ((λ ([X : T] ...) M_0) M ...)
          (subst (X M) ... M_0)
          β)
     ;; δ clause, what does this implement??! 
     (--> (O N_0 ...) N_1
          (judgment-holds (δ (O N_0 ...) N_1))
          δ)
     ;; if-t clause (if-zero), take M_1 
     (--> (if0 0 M_1 M_2) M_1 if-t)
     ;; if-f clause (if-not-zero), take M_2 
     (--> (if0 N M_1 M_2) M_2
          (side-condition (not (zero? (term N))))
          if-f)))

(define-judgment-form PCF
    #:mode (δ I O)
    #:contract (δ (O N ...) N)
    ;; these guys interpret primitive operations
    ;; unquote "," escapes out of Redex into Racket so ,(+ ...) is calling Racket's +
    [(δ (+ N_0 N_1) ,(+ (term N_0) (term N_1)))]
    [(δ (* N_0 N_1) ,(* (term N_0) (term N_1)))]
    [(δ (sub1 N) ,(sub1 (term N)))]
    [(δ (add1 N) ,(add1 (term N)))])

