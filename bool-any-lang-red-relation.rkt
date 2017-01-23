#lang racket
(require "bool-any-lang.rkt")
(require redex)

(define bool-any-red
  (reduction-relation
   bool-any-lang
   (--> (in-hole C (or true B)) ;pattern to match (see redex-match)
        (in-hole C true) ;result of rule on the expression
        or-true) ;rule name 
   (--> (in-hole C (or false B))
        (in-hole C B)
        or-false)))

(traces bool-any-red (term true))

(traces bool-any-red (term (or false true)))

(traces bool-any-red (term (or true true)))

(traces bool-any-red (term (or false false)))

(traces bool-any-red
        (term (or (or true false)
                  (or true true))))

(traces bool-any-red (term (or true
                               (or true
                                   (or false
                                       (or false
                                           true))))))

(traces bool-standard-red
         (term (or (or (or (or (or false true) false) true) false) true)))