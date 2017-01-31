#lang racket
(require redex)
(require "defining-a-language.rkt")
(require "typing.rkt")

;; define our evaluation contexts and values 
(define-extended-language Ev L+Γ
  (p (e ...)) ; program : sequence of expressions 
  (P (e ... E e ...)) ; evaluation contexts for ps (eval can occur in any e)
  (E (v E) ; reduction from left to right
     (E e) ; reduction inside application exps
     (+ v ... E e ...) 
     (if0 E e e)
     (fix E)
     hole)
  (v (λ (x t) e)
     (fix v)
     number))

;; metafunction for summation (+ on Redex) 
(define-metafunction Ev
  Σ : number ... -> number
  [(Σ number ...)
   ,(apply + (term (number ...)))])


;; let's use subst/proc
(require redex/tut-subst)
(define-metafunction Ev
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))]) ; use term to extract values of vars, pass to subst/proc
(define x? (redex-match Ev x)) ; gets us a predicate that matches the pattern in Ev 

(define red
  (reduction-relation
   Ev                       ; lang 
   #:domain p               ; domain of relation
   ; in-hole is context decomposition  
   (--> (in-hole P (if0 0 e_1 e_2)) ; a program with appropriate if0 exp inside
        (in-hole P e_1) ; put e_1 into same context
        "if0t")
   (--> (in-hole P (if0 v e_1 e_2)) ; v is anything but 0
        (in-hole P e_2) 
        (side-condition (not (equal? 0 (term v))))
        "if0f")
   (--> (in-hole P ((fix (λ (x t) e)) v)) ; allows us to get recursion! 
        (in-hole P (((λ (x t) e) (fix (λ (x t) e))) v))
        "fix")
   (--> (in-hole P ((λ (x t) e) v)) ; subst-lam 
        (in-hole P (subst x v e)) ; subst v for x in e
        "βv") ; "beta" is standard
   (--> (in-hole P (+ number ...)) ; program using + 
        (in-hole P (Σ number ...)) ; reduce by applying Σ? 
        "+")
   (--> (e_1 ... (in-hole E (amb e_2 ...)) e_3 ...) ; list of es, one of which is amb? ;this rule can always reduce ambs first
        (e_1 ... (in-hole E e_2) ... e_3 ...) ; reduce by grabbing the first one
        "amb")))

;; Exercise 7 
#; ;this is awesome
(traces red
        (term ((+ (amb 1 2)
                  (amb 10 20)))))

;; Exercise 8
;; fn that accepts a num n and evals (ambiguously) to any numbers between n and 0
#;
(define (rand-less-than-n n)
  (if (zero? n)
      0
      (amb n (rand-less-than-n (+ n -1)))))
;; WHY THIS DOESN'T WORK (1/30/17):
;; - We want to be writing in OUR language, E, not Racket (who doesn't know about amb)
;; - Programs in E don't look like this (define ... ), rather they are a list of Es (see def) 
;; - We can't just shove "term"s in the program
;; - But we don't have a way to do a "define" (ie let)
;; - But we do have nums, ifzero, λ and most imporantly fix 

(traces red
        (term ( ; our programs are lists, so have to wrap this in more brackets 
               ((fix (λ (rltn (→ num num)) 
                       (λ (n num)
                         (if0 n
                              0
                              (amb n (rltn (+ n -1)))))))
                5)
               )))

;; Now let's try fibonacci! 
(traces red (term
             (((fix (λ (fib (→ num num))
                     (λ (n num)
                       (if0 n
                            1
                            (if0 (+ n -1)
                                 1
                                 (+ (fib (+ n -1))
                                    (fib (+ n -2))))))))
              5))))

;; Interestingly, this program will step because of the amb is reducible,
;; and according to line 9, eval contexts can appear anywhere in the list
;; But it doesn't produce exactly what I expect... (amb 1 2) will step to 1 2 :S
(traces red
        (term  ((4 4) (amb 1 2))))

;; TESTING!

;; transitive reduction testing
(test-->>
 red
 (term ((if0 1 2 3)))
 (term (3)))

(test-->>
 red
 (term ((+ (amb 3 4)
           (amb 100 200))))
 (term (103 203 104 204)))

;; single step testing
(test-->
 red
 (term ((+ (amb 1 2) 3)))
 (term ((+ 1 3) (+ 2 3))))

(test-->
 red
 (term ((+ 1 2) (+ 3 4)))
 (term (3 (+ 3 4))) ; multiple results possible
 (term ((+ 1 2) 7)))

;(traces red (term (((fix (λ (x (→ num num)) x)) 1))))
;; test-->> errors out with cycles, but can bypass
(test-->>
 red #:cycles-ok
 (term (((fix (λ (x (→ num num)) x)) 1))))

(test-results)