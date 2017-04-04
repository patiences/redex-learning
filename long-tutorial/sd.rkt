#lang racket
(require redex)
(require "developing-a-language.rkt")
(require "subtract.rkt")
(require "fv.rkt")
(provide (all-defined-out))

;; Now we want to specify an equivalence relation:
;; the relation that virtually eliminates variables from phrases and
;;     replaces them with arrows to their declarations
;;     ie static distance phase of compiling

; (sd e) computes the static distance version of e

; let's add this new (K n) expression to the language 
(define-extended-language SD Lambda
  (e ::= .... (K n)) ; wonder why we define this and not (K n n)... which is what sd seems to return
  (n ::= natural)) ; yay now we have nums

(define sd1 (term (K 1)))
(define sd2 (term 1))

(define SD? (redex-match? SD e))
(test-equal (SD? sd1) #true)

;; metafunction meaning "arrow back to the variable declaration" 
(define-metafunction SD
  sd : any -> any ; we want metafunctions as generic as possible 
  [(sd any_1) (sd/a any_1 ())]) ; call sd/a with an empty list for accumulator trampoline! 
;; sd : e -> e this is too restrictive, tests below will use non-es
;;[(sd e_1) (sd/a e_1 ())]) 

(define-metafunction SD
  sd/a : any ((x ...) ...) -> any 
  ; e ((x ...) ...) -> e ; this is too restrictive; replaced all occurrences of e with any below
  
  [(sd/a x ((x_1 ...) ... (x_0 ... x x_2 ...) (x_3 ...) ...))
   ; bound variable: found x in our accumulated list! 
   (K n_rib n_pos)
   (where n_rib ,(length (term ((x_1 ...) ...)))) ; how many (x_1 ...)s (lambdas/scopes) did we see before x?
   (where n_pos ,(length (term (x_0 ...)))) ; how many x_0s (args in the lambda/scope) did we see before x? 
   (where #false (in x (x_1 ... ...)))] ; make sure we are looking at x when we see it first
  
  [(sd/a (lambda (x ...) any_1) (any_rest ...))
   (lambda () (sd/a any_1 ((x ...) any_rest ...)))] ; go into λ exp body, adding λ's args to our accumulator 
  
  [(sd/a (any_fun any_arg ...) (any_rib ...)) ;; e ::= (e e ...)
   ((sd/a any_fun (any_rib ...)) (sd/a any_arg (any_rib ...)) ...)] ;; go into each e! 
  
  [;(sd/a e_1 e) this is not going to match: empty list is not an e
   (sd/a any_1 any)
   ; a free variable is left alone 
   any_1])

(test-equal (term (sd x)) (term x))
(test-equal (term (sd/a x ())) (term x))
(test-equal (term (sd/a x ((y) (z) (x)))) (term (K 2 0)))
(test-equal (term (sd/a ((lambda (x) x) (lambda (y) y)) ()))
            (term ((lambda () (K 0 0)) (lambda () (K 0 0)))))
(test-equal (term (sd/a (lambda (x) (x (lambda (y) y))) ()))
            (term (lambda () ((K 0 0) (lambda () (K 0 0))))))
(test-equal (term (sd/a (lambda (z x) (x (lambda (y) z))) ()))
            (term (lambda () ((K 0 1) (lambda () (K 1 0))))))
(test-results)

;; Now we can have our equivalence comparison:
; (=α e_1 e_2) determines whether e_1 and e_2 are α equivalent
;; Our Lambda lang plus nums 
(define-extended-language Lambda/n Lambda
  (e ::= .... n)
  (n ::= natural))

(define in-Lambda/n? (redex-match? Lambda/n e))
;; equivalence is having the same static distance
;; huh. why? 
(define-metafunction SD
  =α : any any -> boolean
  [(=α any_1 any_2) ,(equal? (term (sd any_1)) (term (sd any_2)))])
;;=α : e e -> boolean unfortunately this is going to be too restrictive.. we are running tests on non-es 
;;[(=α e e) ,(equal? (term (sd e)) (term (sd e)))]) 

(test-equal (term (=α (lambda (x) x) (lambda (y) y))) #true)
(test-equal (term (=α (lambda (x) (x 1)) (lambda (y) (y 1)))) #true)
(test-equal (term (=α (lambda (x) x) (lambda (y) z))) #false)
(test-equal (term (=α (lambda (x y) x) (lambda (y z) y))) #true)
(test-equal (term (=α (lambda (x) (x 3)) (lambda (x) (x 4)))) #false)
(test-results)

;; Any Any -> Boolean
;; (=α/racket e_1 e_2) determines whether e_1 is α-equivalent to e_2
;; e_1, e_2 are in Lambda or extensions of Lambda that 
;; do not introduce binding constructs beyond lambda 
(define (=α/racket x y) (term (=α ,x ,y)))
