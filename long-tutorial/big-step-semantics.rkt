#lang racket
(require redex)
(require "lambda-calculus.rkt")
(require "subst.rkt")

(define-extended-language Standard Lambda-calculus
  (v ::= n (lambda (x ...) e))
  (E ::=
     hole
     (v ... E e ...)))

;; only reduce from leftmost outermost
(define s->βv
  (reduction-relation
   Standard
   (--> (in-hole E ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole E (subst ((v_1 x_1) ...) e)))))

(define t0
  (term
   ((lambda (x y) (x y))
    ((lambda (x) x) (lambda (x) x))
    ((lambda (x) x) 5))))
(define t0-one-step
  (term
   ((lambda (x y) (x y))
    (lambda (x) x) ; sub (lambda (x) x) for x in x
    ((lambda (x) x) 5))))

; yields only one term, leftmost-outermost
(test--> s->βv t0 t0-one-step)
; but the transitive closure drives it to 5
(test-->> s->βv t0 5)
#;
(traces s->βv t0)

;; Now we can define eval (big-step)
(define-metafunction Standard
                         ; i.e. lambda
  eval-value : e -> v or closure
  [(eval-value e) any_1 (where any_1 (run-value e))])

(define-metafunction Standard
  run-value : e -> v or closure
  [(run-value n) n] ; nums eval to nums
 ;[(run-value v) v] gives us the lam back, rather than 'closure
  [(run-value v) closure] ; if it's a value and not a num, then it's a lambda, and lambda -> lambda
  [(run-value e)
   (run-value e_again)
                    ; reduce e from the outermost/leftmost! 
   (where (e_again) ,(apply-reduction-relation s->βv (term e)))])

; t0 and to-one-step should both eval to 5
(test-equal (term (eval-value ,t0)) 5)
(test-equal (term (eval-value ,t0-one-step)) 5)

(define t1
  (term ((lambda (x) x) (lambda (x) x))))
(test-equal (redex-match? Standard e t1) #true) ; t1 is in our Standard lang
(test-equal (term (eval-value ,t1)) 'closure) ; evals to a closure (lambda (x) x) 

(test-results)
