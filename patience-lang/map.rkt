#lang racket
(require redex)
(require "patience.rkt")

(define-metafunction Patience
  map : (lambda (x ...) e) (list e ..._n) -> (list e ..._n)
  [(map (lambda (x ...) e_body)
        (list e_0 ...))
   (list ((lambda (x ...) e_body) e_0) ...)])

(define IDENTITY (term (lambda (x) x)))
(define f1 (term (map ,IDENTITY (list 1 2 3))))
(test-equal f1 (term (list ((lambda (x) x) 1) ((lambda (x) x) 2) ((lambda (x) x) 3))))
(test-->> reduce f1 (term (list 1 2 3)))

(define FACT? (term (lambda (x) (if x 1 0))))
(define f2 (term (map ,FACT? (list #true #true #true))))
(test-equal f2 (term (list ((lambda (x) (if x 1 0)) #t) ((lambda (x) (if x 1 0)) #t) ((lambda (x) (if x 1 0)) #t))))
(test-->> reduce f2 (term (list 1 1 1)))

(define-metafunction Patience
  ;; fold(f, base, l) -> e_base
  fold : (lambda (x_0 x_1) e) e (list e ...) -> e  ;; FIXME
  
  [(fold (lambda (x_0 x_1) e_body) e_base (list)) e_base] ;done
  
  [(fold (lambda (x_0 x_1) e_body) e_base (list e_0 e_1 ...))
   (fold (lambda (x_0 x_1) e_body) result (list e_0 e_1 ...))
   (where result ((lambda (x_0 x_1) e_body) e_0 e_base))])

(define ADD (term (lambda (x y) (+ x y))))
(define f3 (term (fold ,ADD 0 (list 1 2 3 4 5))))
(test-->> reduce f3 (term 15))


(define-metafunction Patience
  ormap : (lambda (x) any) (list e ...) -> boolean ;;FIXME
  
  [(ormap (lambda (x) e_body)
          (list e_0))
   ((lambda (x) e_body) e_0)]
  
  [(ormap (lambda (x) e_body)
          (list e_0 e ...))
   (ormap (lambda (x) e_body) (list e ...))
   (side-condition (equal? #t (term ((lambda (x) e_body) e_0))))]
  
  [(ormap (lambda (x) e_body)
          (list e_0 e ...))
   #f
   (side-condition (equal? #f (term ((lambda (x) e_body) e_0))))])

;(test-equal (term (ormap (lambda (x) (zero? x)) (list 0))) #t)

(test-results)