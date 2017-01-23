#lang racket
(require redex)

(define-language bool-standard-lang
  [B true
     false
     (or B B)]
  [E (or E B) ;only allow holes in the leftmost-outermost position
     hole])

(define bool-standard-red
  (reduction-relation
   bool-standard-lang
   (--> (in-hole E (or true B))
        (in-hole E true)
        or-true)
   (--> (in-hole E (or false B))
        (in-hole E B)
        or-false)))

(traces bool-standard-red (term true))

(traces bool-standard-red (term (or false true)))

(traces bool-standard-red (term (or true true)))

(traces bool-standard-red (term (or false false)))

; All these guys ^ are the same as in bool-any-lang because there is only 1 way to reduce these expressions

(traces bool-standard-red
        (term (or (or true false)
                  (or true true))))

(traces bool-standard-red (term
                           (or true
                               (or true
                                   (or false
                                       (or false
                                           true))))))

(traces bool-standard-red
         (term (or (or (or (or (or false true) false) true) false) true)))

; But these guys ^ must be reduced from left


