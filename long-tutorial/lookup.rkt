#lang racket
(require redex)
(require "developing-a-language.rkt")
(provide (all-defined-out))

;; Environments!
(define-extended-language Env Lambda
  (e ::= .... natural)
  (env ::= ((x e) ...)))

(define e1 (term 1))
(define env1 (term ((x 1) (y 2) (z 3))))

(define Env? (redex-match? Env env))
(test-equal (Env? env1) #true)

;; Exercise 2
;; Design lookup. The metafunction consumes a variable and an environment.
;; It determines the leftmost expression associated with the variable;
;; otherwise it produces #false.(define-metafunction Env
(define-metafunction Env
  lookup : x env -> e or #false
  [(lookup x_0 ((x_1 e_1) ... (x_0 e_0) (x_2 e_2) ...)) e_0]
  [(lookup x_0 env_0) #false])

(test-equal (term (lookup x ((x 1)))) 1)
(test-equal (term (lookup x ())) #false)
(test-equal (term (lookup x ((a 1) (b 2) (c 3)))) #false)
(test-equal (term (lookup x ((z 3) (y 2) (x 1)))) 1)
(test-equal (term (lookup x ((x (lambda (x) 1))))) (term (lambda (x) 1)))

(test-results)
