#lang racket
(require "bool-any-lang.rkt")
(require redex)

(define bool-any-red
  (reduction-relation
   bool-any-lang
   (--> (in-hole C (or true B))
        (in-hole C true)
        or-true)
   (--> (in-hole C (or false B))
        (in-hole C B)
        or-false)))