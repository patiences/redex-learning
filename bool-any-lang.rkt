;; bool-any-lang from Semantics Engineering with PLT Redex (Felleisen et al.)

#lang racket
(require redex)

(define-language bool-any-lang
  [B true
     false
     (or B B)]
  [C (or C B)
     (or B C)
     hole])

;Some legitimate expressions
(define B1 (term true))
(define B2 (term false))
(define B3 (term (or true false)))
(define B4 (term (or ,B1 ,B2)))
(define B5 (term (or false ,B4)))
(define C1 (term hole))
(define C2 (term (or (or false false) hole)))
(define C3 (term (or hole true)))

;(list (match (list (bind 'B 'true))))
(redex-match bool-any-lang
             B
             (term true))

;(list (match (list (bind 'B '(or true false)))))
(redex-match bool-any-lang
             B
             (term (or true false)))

;(list (match (list (bind 'C hole))))
(redex-match bool-any-lang
             C
             (term hole))

; #f, doesn't match anything 
(redex-match bool-any-lang
             C
             (term (or true false)))

;(list
; (match (list (bind 'B '(or true false))
;              (bind 'C hole)))
; (match (list (bind 'B 'false)
;              (bind 'C '(or true hole))))
; (match (list (bind 'B 'true)
;              (bind 'C '(or hole false)))))
(redex-match bool-any-lang
             (in-hole C B)
             (term (or true false)))

;(list (match (list (bind 'B 'false)
;                   (bind 'C hole))))
(redex-match bool-any-lang
             (in-hole C (or true B))
             (term (or true false)))

;(list
; (match (list (bind 'B '(or true false))
;              (bind 'C hole)))
; (match (list (bind 'B 'false)
;              (bind 'C '(or true hole)))))
(redex-match bool-any-lang
             (in-hole C (or true B))
             (term (or true (or true false))))

