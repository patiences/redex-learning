#lang racket
(require redex)
(require "patience.rkt")

(define-metafunction Patience
  map : (lambda (x ...) e) (list e ..._n) -> (list e ..._n)
  [(map (lambda (x ...) e_body)
        (list e_0 ...))
   (list ((lambda (x ...) e_body) e_0) ...)])

(define IDENTITY (term (lambda (x) x)))
(test-equal (term (map ,IDENTITY (list 1 2 3)))
            (term (list 1 2 3)))

(define FACT? (term (lambda (x) (if x 1 0))))
(test-equal (term (map ,FACT? (list #true #true #true)))
            (list 1 1 1))


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

(test-equal (term (ormap (lambda (x) (zero? x)) (list 0))) #t) 
   