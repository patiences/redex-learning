#lang racket
(require redex)
(require "developing-a-language.rkt")
(provide (all-defined-out))

; (subst ([e x] ...) e_*) substitutes e ... for x ... in e_* (hygienically)
(define-metafunction Lambda
  subst : ((any x) ...) any -> any
  [(subst [(any_1 x_1) ... (any_x x) (any_2 x_2) ...] x) any_x] ; [any_x/x]x = any_x
  
  [(subst [(any_1 x_1) ...]  x) x] ; nothing to sub for x

  ;; We want to do "raw" subst into function body with λ's params first, then the other stuff
  ;; WHY?! TODO 
  [(subst [(any_1 x_1) ...]  (lambda (x ...) any_body)) ; sub some stuff into lambda 
   (lambda (x_new ...)
     (subst ((any_1 x_1) ...) ; sub all these guys into λ exp body 
            (subst-raw ((x_new x) ...) any_body))) ; sub λ's params into any_body first 
   (where  (x_new ...)  ,(variables-not-in (term any_body) (term (x ...))))] ; grab λ's args that aren't used in the body
   ; (variables-not-in t vars) returns list of distinct vars that don't occur in t
  
  [(subst [(any_1 x_1) ...]  (any ...)) ((subst [(any_1 x_1) ...]  any) ...)] ; map subst onto list
  
  [(subst [(any_1 x_1) ...]  any_*) any_*]) ; throw away the substitutions, they don't occur in any_* 
 
(define-metafunction Lambda
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((x_n1 x_o1) ... (x_new x) (x_n2 x_o2) ...) x) x_new] ; [x_new/x]x = x_new
  [(subst-raw ((x_n1 x_o1) ...)  x) x] ; nothing to sub for x 
  [(subst-raw ((x_n1 x_o1) ...)  (lambda (x ...) any))
   (lambda (x ...) (subst-raw ((x_n1 x_o1) ...)  any))] ; go into function body and sub 
  [(subst-raw [(any_1 x_1) ...]  (any ...)) ; map subst-raw onto list 
   ((subst-raw [(any_1 x_1) ...]  any) ...)]
  [(subst-raw [(any_1 x_1) ...]  any_*) any_*]) ; nothing to sub in list 
