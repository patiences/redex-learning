#lang racket
(require redex)

;; A reduction strategy that reduces from the left-most/outer-most
;; Call-by-name reduction
(define-extended-language PCFn PCF
  (E ::= hole
     ;; Try reducing the first term in a list of terms 
     (E M ...)
     ;; Arithmetic operations on an arbitrary amount of terms... try reducing the first non-value!
     (O V ... E M ...)
     ;; Try reducing the predicate in an if-exp 
     (if-0 E M M)))

;; Now we can use our relation 
(define -->n (context-closure r PCFn E))

(test-equal (apply-reduction-relation* -->n (term ((λ ([x : num]) (add1 5)))))
            '(6))
;(traces -->n (term ((λ ([x : num]) x) (add1 5))))
;; Now we can use -->n to calc fact-5! (Unlike -->r) WHY?!!! Is this because the rules for
;; r don't allow for "holes" in the predicate position (unable to reduce (sub1 n))??
(test-equal (apply-reduction-relation* -->n (term fact-5)) '(120))


;; A reduction strategy for left-most/outer-most call by value
(define-extended-language PCFv PCF
  (E ::= hole
     ;; Try to reduce the first expression (that is not a value) 
     (V ... E M ...)
     ;; Try to reduce the predicate 
     (if0 E M M)))
(define v
  (extend-reduction-relation
   r PCF #:domain M ;; what is this domain business
   (--> ((λ ([X : T] ...) M_0) V ...) ;; result of reduction should be a (bunch of) value(s)!
        (subst (X V) ... M_0)
        β)))
(define -->v
  (context-closure v PCFv E))

(test-equal (apply-reduction-relation* -->v (term ((λ ([x : num]) (add1 5))))) '(6))
(test-equal (apply-reduction-relation* -->v (term fact-5)) '(120))

;; -->v AND -->n ALWAYS PRODUCE THE SAME RESULT, IF THEY BOTH PRODUCE RESULTS.
;; BUT -->n PRODUCES MORE ANSWERS: WHY?!?!! ! 
(define-term Ω
    ((μ (loop : (num -> num))
        (λ ([x : num])
          (loop x)))
     0))
(test-equal (apply-reduction-relation* -->n (term ((λ ([x : num]) 0) Ω))) '(0))
(test-equal (apply-reduction-relation* -->v (term ((λ ([x : num]) 0) Ω)))'())
;; So redex will terminate even if there is a cycle 


