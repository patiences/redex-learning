#lang racket
(require redex)

;; S-expressions: constructed with (term ...) == (quote ...)

;; Language: Set of s-expressions
;; Like BNF grammer!
;; ... is 0 or more repetitions
(define-language L
  ;; Subsets of L defined by non-terminals of the grammar, i.e. M, F, N
  (M ::= N F (M ...))
  (F ::= fred wilma)
  (N ::= 2 7))

;; Is an s-expression an element of one of these sets?
(test-equal true (redex-match? L N (term 2)))
(test-equal false (redex-match? L N (term 9)))
(test-equal false (redex-match? L N (term fred)))
(test-equal true (redex-match? L M (term (((((((fred))))))))))

;; Can match an arbitrary pattern, not just a non-terminal name (M, F, N)
(test-equal false (redex-match? L (N ...) (term 2)))
(test-equal true (redex-match? L (N ...) (term (7 2 7))))
(test-equal true (redex-match? L (M F N) (term (2 fred 7))))
;; Subscripts in patterns distinguish multiple occurrences of the same non-terminal name
(test-equal true (redex-match? L (M_1 M_2) (term (7 (2 fred)))))

;; Metafunctions: functions on terms!
;; Ordered sequence of pattern and template clauses
(define-metafunction L
  swap : M -> M
  [(swap fred) wilma]
  [(swap wilma) fred]
  ;; this clause distributes the swap call to all the elements of the list
  ;; WHY?!! ?!? !?! ?!?!?
  [(swap (M ...)) ((swap M) ...)]
  [(swap M) M])

;; within term, metafunctions are interpreted (not in quote!)
(test-equal '(fred wilma) (term (swap (wilma fred))))
(test-equal '(fred 2 (wilma)) (term (swap (wilma 2 (fred)))))

