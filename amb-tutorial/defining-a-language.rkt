#lang racket
(require redex)
(provide L)

(define-language L ; gives a name to a grammar 
  (e (e e) ; application
     (λ (x t) e) ; abstraction
     x ; variables
     (amb e ...) ; amb expressions 
     number 
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (→ t t)
     num)
  (x variable-not-otherwise-mentioned)) ; matches all symbols except literals in grammar (λ, amb, etc)

;; match just e 
(redex-match L e (term ((λ (x num) (amb x 1))
                        (+ 1 2))))
;;(list (match (list
;;              (bind 'e '((λ (x num) (amb x 1)) (+ 1 2))))))

;; match fun and app positions of application expression
(redex-match L (e_1 e_2) (term ((λ (x num) (amb x 1))
                                (+ 1 2))))
;;(list
;; (match (list
;;         (bind 'e_1 '(λ (x num) (amb x 1)))
;;         (bind 'e_2 '(+ 1 2)))))

;; match any non-empty sequence of exps
(redex-match L (e_1 ... e_2 e_3 ...) (term ((+ 1 2) (+ 3 4) (+ 5 6))))

;; Exercise 1
;; match body of λ
(redex-match L
             ((λ (_ _) e) 17) ;; is there some other way to do this? 
             (term ((λ (x num) (+ x 1)) 17)))
;; (list (match (list (bind 'e '(+ x 1)))))

;; Exercise 2
;; match range portion
(redex-match L (→ num t) (term (→ num (→ num num))))
;; (list (match (list (bind 't '(→ num num)))))

;; Exercise 3
;; Design a pattern that matches one way for each adjacent pair of expressions in a sequence
;; produce n matches when there are n+1 expressions in the sequence
(redex-match L (_ ... e_1 e_2 _ ...) (term (1 2 3 4)))
;; (list
;; (match (list (bind 'e_1 1) (bind 'e_2 2)))
;; (match (list (bind 'e_1 2) (bind 'e_2 3)))
;; (match (list (bind 'e_1 3) (bind 'e_2 4))))

;; Exercise 4
;; match odd length lists of expressions, returning one match for each pair that are equidistant from the ends of the sequence
(redex-match L (_ ..._0 ;; any amount of stuff, as long as the same amount of stuff on the other side
                  e_left ;; one e
                  _ ..._1 ;; more stuff, as long as the same amount on the other side
                  _ ;; expression is symmetrical around this point
                  _ ..._1
                  e_right 
                  _ ..._0)
             (term (1 2 3 4 5)))
;;(list
;; (match (list (bind 'e_left 1) (bind 'e_right 5)))
;; (match (list (bind 'e_left 2) (bind 'e_right 4))))