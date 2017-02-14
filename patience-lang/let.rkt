#lang racket
(require redex)
(require "patience.rkt")

(define-metafunction Patience
  let : ((x e) ...) e -> e
  [(let ([x_0 e_0] ...) e_body)
   ((lambda (x_0 ...) e_body) e_0 ...)])

(define-metafunction Patience
  let* : ((x e) ...) e -> e   ;; FIXME
  [(let* () e_body) e_body] ; 0 bindings
  [(let* ([x_0 e_0] [x_rest e_rest] ...) e_body) ; >=1 binding
   (let ([x_0 e_0]) recursion)
   ;;((lambda (x_0) recursion) e_0)
   (where recursion (let* ([x_rest e_rest] ...) e_body))])

(define f1 (term (let ([x 1]
                       [y 2]
                       [z 3])
                   (+ x y z))))
(test-equal f1
            (term ((lambda (x y z) (+ x y z)) 1 2 3)))
(test-->> reduce f1 (term 6))

(define f2 (term (let* ([x 1]
                        [y (+ x 1)]
                        [z (+ y 1)])
                   (+ x y z))))
(test-equal f2
            (term (lambda (x) (lambda (y) ((lambda (z) (+ x y z)) 3) 2) 1)))
(test-->> reduce f2 (term 6))
