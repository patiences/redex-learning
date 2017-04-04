#lang racket
(require redex)
(provide (all-defined-out))

;; language definition
(define-language Patience
  ;; expressions 
  (e ::= x
     v
     (e e ...)
     (+ e ...)
     (if e e e)
     (or e ...)
     (and e ...)
     (not e)
     (list e ...))
  ;; variables 
  (x ::= variable-not-otherwise-mentioned)
  ;; values 
  (v ::= n
     b
     (lambda (x_!_ ...) e))
  (n ::= number)
  (b ::= boolean) 
  ;; evaluation contexts (standard)
  (E ::= hole
     (v ... E e ...)
     (+ v ... E e ...)
     (if E e e)
     (or v ... E e ...)
     (and v ... E e ...)
     (not E)
     (list v ... E e ...)))

(define-metafunction Patience
  subst : ((any x) ...) any -> any
  [(subst [(any_1 x_1) ... (any_x x) (any_2 x_2) ...] x) any_x] ; [any_x/x]x = any_x
  
  [(subst [(any_1 x_1) ...]  x) x] ; nothing to sub for x
  
  ;; We want to do "raw" subst into function body with λ's params first, then the other stuff
  [(subst [(any_1 x_1) ...]  (lambda (x ...) any_body)) ; sub some stuff into lambda 
   (lambda (x_new ...)
     (subst ((any_1 x_1) ...) ; sub all these guys into λ exp body 
            (subst-raw ((x_new x) ...) any_body))) ; sub λ's params into any_body first 
   (where  (x_new ...)  ,(variables-not-in (term any_body) (term (x ...))))] ; grab λ's args that aren't used in the body
  ; (variables-not-in t vars) returns list of distinct vars that don't occur in t
  
  [(subst [(any_1 x_1) ...]  (any ...)) ((subst [(any_1 x_1) ...]  any) ...)] ; map subst onto list
  
  [(subst [(any_1 x_1) ...]  any_*) any_*]) ; throw away the substitutions, they don't occur in any_* 

(define-metafunction Patience
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((x_n1 x_o1) ... (x_new x) (x_n2 x_o2) ...) x) x_new] ; [x_new/x]x = x_new
  [(subst-raw ((x_n1 x_o1) ...)  x) x] ; nothing to sub for x 
  [(subst-raw ((x_n1 x_o1) ...)  (lambda (x ...) any))
   (lambda (x ...) (subst-raw ((x_n1 x_o1) ...)  any))] ; go into function body and sub 
  [(subst-raw [(any_1 x_1) ...]  (any ...)) ; map subst-raw onto list 
   ((subst-raw [(any_1 x_1) ...]  any) ...)]
  [(subst-raw [(any_1 x_1) ...]  any_*) any_*]) ; nothing to sub in list 

;; reduction relation
(define reduce
  (reduction-relation
   Patience
   
   ;; β reduction
   (--> (in-hole E ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e))
        β)
   
   ;; + reduction 
   (--> (in-hole E (+ number ...))
        (in-hole E ,(apply + (term (number ...))))
        add)
   
   ;; if-true
   (--> (in-hole E (if e_pred e_1 e_2))
        (in-hole E e_1)
        if-true
        (side-condition (equal? #t (term e_pred)))) 
   
   ;; if-false
   (--> (in-hole E (if e_pred e_1 e_2))
        (in-hole E e_2)
        if-false
        (side-condition (equal? #f (term e_pred))))
   
   ;; or-true
   (--> (in-hole E (or #true boolean ...))
        (in-hole E #true)
        or-true)
   
   ;; or-false
   (--> (in-hole E (or #false boolean ...))
        (in-hole E (or boolean ...))
        or-false)
   
   ;; and-true
   (--> (in-hole E (and #true boolean ...))
        (in-hole E (and boolean ...))
        and-true)
   
   ;; and-false
   (--> (in-hole E (and #false boolean ...))
        (in-hole E #false)
        and-false)
   
   ;; not-true
   (--> (in-hole E (not #true))
        (in-hole E #false)
        not-true)
   
   ;; not-false
   (--> (in-hole E (not #false))
        (in-hole E #true)
        not-false)))
