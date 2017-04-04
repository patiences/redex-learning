#lang racket
(require redex)
(require "developing-a-language.rkt")
(require "lookup.rkt")

;; DON'T DO THIS
#; 
(define-metafunction Env
  let : env e -> any
  [(let ((x_0 e_0) ...) x) ; e is an x 
   (lookup x ((x_0 e0) ...))]
  [(let ((x_0 e_0) ...) (lambda (x ...) e_body)) ; e is a lambda 
   ...]

  [(let ((x_0 e_0) ...) (e_1 e_2 ...))
   (e_1_go e_2_go)
   (where e_1_go (let ((x_0 e_0) ...) e_1))
   (where e_2_go (let ((x_0 e_0) ...)) (e_2 ...))] ; e is a list of exps 

  [(let ((x_0 e_0) ...) natural) natural]) ; e is a natural


(define-metafunction Lambda-calculus
  let : ((x any) ...) any -> any
  [(let ([x_lhs any_rhs] ...) any_body)
   ((lambda (x_lhs ...) any_body) any_rhs ...)])
