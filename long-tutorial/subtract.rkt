#lang racket
(require redex)
(require "developing-a-language.rkt")
(provide subtract)
(provide in)

; (subtract (x ...) x_1 ...) removes x_1 ... from (x ...)
(define-metafunction Lambda 
  subtract : (x ...) x ... -> (x ...)
  [(subtract (x ...)) (x ...)] ; nothing to subtract
  [(subtract (x ...) x_1 x_2 ...) ; map subtract1 over all the crap we want to subtract 
   (subtract (subtract1 (x ...) x_1) x_2 ...)])

; (subtract1 (x ...) x_1) removes x_1  from (x ...)
(define-metafunction Lambda
  subtract1 : (x ...) x -> (x ...)
  [(subtract1 (x_1 ... x x_2 ...) x) ; x is in our list! 
   (x_1 ... x_2new ...) ; let's get rid of it 
   (where (x_2new ...) (subtract1 (x_2 ...) x)) ; recursively apply subtract1 on the sublist after the first occurrence of x
   (where #false (in x (x_1 ...)))] ; make sure this x is the first occurrence in the list
  [(subtract1 (x ...) x_1) (x ...)]) ; x is not in the list, we're done!

(define-metafunction Lambda
  in : x (x ...) -> boolean
  [(in x (x_1)) #false]
  [(in x (x_1 ... x x_2 ...)) #true]
  [(in x (x_1 ...)) #false])


(test-equal (term (subtract (x y z x) x z)) (term (y)))
(test-equal (term (subtract (x))) (term (x)))

(test-equal (term (subtract1 (x y z x) x)) (term (y z)))
(test-equal (term (subtract1 (x) x)) (term ()))

(test-results)