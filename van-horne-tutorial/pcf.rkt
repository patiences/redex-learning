#lang racket
(require redex)

(define-language PCF
  (M ::=   ;expressions?
     N O X L
     (μ (X : T) L)
     (M M ...)
     (if0 M M M))
  ;; λ, μ, if0, : etc cannot be used as variable names
  (X ::= variable-not-otherwise-mentioned)
  (L ::= (λ ([X : T] ...) M)) ;; functions
  (V ::= N O L) ;; values are numbers/arithmetic operations/lambdas 
  (N ::= number)
  (O ::= O1 O2) ;;arithmetic guys! 
  (O1 ::= add1 sub1) 
  (O2 ::= + *) ;;some racket arithmetic functions
  (T ::= num (T ... -> T)))

;; Example PCF program
;; define-term bings a name interpreted within term 
(define-term fact-5
  ;; fact is a function that takes a num and produces a num
  ;;     (μ (X : T) L)
  ((μ (fact : (num -> num))
      ;; (λ ([X : T] ...) M)
      (λ ([n : num])
        (if0 n
             1
             (* n (fact (sub1 n))))))))

;; Is this program in the language PCF? 
(test-equal (redex-match? PCF M (term fact-5)) #t)

;; Let's define a type relation for PCF programs!
;; Typing relations are defined in ters of a typing environment (missing in our PCF grammar)

;; Let's define a language extension:
;; PCFT has everything PCF has, plus a notion of type environments Γ
;; Type environments are sequences of variable and type pairs 
(define-extended-language PCFT PCF
  (Γ ::= ((X T) ...)))

;; Let's define operations for association lists like Γ
;; We want to be generic, so we cannnot use the metavariables of the languages we defined
;; So, let's define the language of built-in patterns
(define-language REDEX)
;; Our language has no nonterminals! It only includes built-in patterns, e.g. number, variable, etc

;; Let's define a lookup function!
;; define-judgment-from specifies a relation (a function from inputs to sets of outputs)
;; #:mode specifies which are the inputs and which are outputs
;; #:contract signature of relation 
;; Recursive metafunction that produces the first association of a given key:
(define-judgment-form REDEX
  #:mode (lookup I I O)
  #:contract (lookup ((any any) ...) any any)
  ;; Why do we need "_" and ...? Why the restriction? 
  [(lookup (_ ... (any any_0) _ ...) any any_0)])
;; any pattern matches all values
;; _ pattern matches all values but is NOT a binder and so can't be used on right hand side,
;;   and multiple occurrences don't have to match the same thing

(test-equal (judgment-holds (lookup ((x 1) (y 2) (x 3)) x 1)) true)
(test-equal (judgment-holds (lookup ((x 1) (y 2) (x 3)) x 2)) false)
(test-equal (judgment-holds (lookup ((x 1) (y 2) (x 3)) x 3)) true)
;; x can have value 1 or 3, we can use lookup to not just check the value of x, but to produce it
(test-equal (judgment-holds (lookup ((x 1) (y 2) (x 3)) x any) any) '(1 3))

;; We also need an operation to extend an association list
;; Simple definition that allows for association to be extended w/ arbitrary # of key-value pairs:
(define-metafunction REDEX
  ;; ext1 takes a list of pairs and a single pair, and returns a list of pairs
  ext1 : ((any any) ...) (any any) -> ((any any) ...)
  ;; If the key already exists, override its value any_v0 with any_v1
  [(ext1 (any_0 ... (any_k any_v0) any_1 ...) (any_k any_v1))
   (any_0 ... (any_k any_v1) any_1 ...)]
  ;; If the key doesn't exist, just add the pair to the front of the list
  [(ext1 (any_0 ...) (any_k any_v1))
   ((any_k any_v1) any_0 ...)])

;; ext overrides an existing association if there is one, or prepends new association to the list
(define-metafunction REDEX
  ;; ext takes a list of pairs and an arbitrary number of single pairs, and returns a new list of pairs
  ext : ((any any) ...) (any any) ... -> ((any any) ...)
  ;; Base case: if there are no pairs to add, just return the existing list 
  [(ext any) any]
  ;; Otherwise, add the first pair (any_0) to the result of the recursion
  [(ext any any_0 any_1 ...)
   (ext1 (ext any any_1 ...) any_0)])

;; base case
(test-equal (term (ext ((x 1) (y 2)))) '((x 1) (y 2)))
;; Add a new pair to the front of the list 
(test-equal (term (ext ((x 1) (y 2)) (z 3))) '((z 3) (x 1) (y 2)))
;; Overwrite an existing value  
(test-equal (term (ext ((x 1) (y 2)) (x 10))) '((x 10) (y 2)))
;; Add/Overwrite multiple pairs, note that the first pair/change will end up being the first in the list
(test-equal (term (ext ((x 1) (y 2)) (z 3) (x 10)))
            '((z 3) (x 10) (y 2)))

;; We should assert that λ-bound variables are unique
#;(define-metafunction REDEX
  unique : any ... -> boolean
  ;; _! means that repeated uses of it must be distinct 
  [(unique any_!_1 ...) #t]
   ;; can't we use an else clause?? 
  [(unique _ ...) #f])

;(test-equal (term (unique)) #t)
;(test-equal (term (unique 1)) #t)
;(test-equal (term (unique 1 2 3 2)) #f)

;; Note: we can define metafunctions that produce booleans (predicates) with
;; it cant be used just like a metafunction
(define-relation REDEX
    unique ⊆ any × ...
    [(unique any_!_1 ...)])


;; Now we can define the typing relation ⊢
(define-judgment-form PCFT
    #:mode (⊢ I I I O)
    #:contract (⊢ Γ M : T)

    ;; type-var
    [(lookup Γ X T) ;what is the result of a (lookup ...) with a type? 
     -------------- var
     (⊢ Γ X : T)]

    ;; type-num
    [------------- num
     (⊢ Γ N : num)]

    ;; type-lam
    [----------------------- op1
     (⊢ Γ O1 : (num -> num))]

    ;; type-lam with 2 arguments 
    [--------------------------- op2
     (⊢ Γ O2 : (num num -> num))]

    ;; type-if 
    [(⊢ Γ M_1 : num)
     (⊢ Γ M_2 : T)
     (⊢ Γ M_3 : T)
     --------------------------- if0
     (⊢ Γ (if0 M_1 M_2 M_3) : T)]

    ;; type-μ, with addition of X with type T to Γ
    [(⊢ (ext Γ (X T)) L : T)
     ----------------------- μ
     (⊢ Γ (μ (X : T) L) : T)]

    ;; type-app 
    [(⊢ Γ M_0 : (T_1 ..._1 -> T))
     (⊢ Γ M_1 : T_1) ...
     ----------------------- app
     (⊢ Γ (M_0 M_1 ..._1) : T)]

    ;; type-λ, with addition of X with type T to Γ
    [(unique X ...)
     (⊢ (ext Γ (X T) ...) M : T_n)
     ------------------------------------------ λ
     (⊢ Γ (λ ([X : T] ...) M) : (T ... -> T_n))])

              
;; We can check that a type is in the typing relation:
;; Check (num -> num) matches type-λ
(test-equal (judgment-holds (⊢
                             ()
                             (λ ([x : num]) x) :
                             (num -> num)))
            #t)
;; HOW THE FUCK DO YOU WRITE A LOOKUP OPERATION???!?!??!!?!
;;(test-equal (judgment-holds (⊢
;;                             ((x 1) (y 2))
;;                             1 : num) num)
;;            '())
;; What types are possible for an expression M = 1? '(num) of course! 
(test-equal (judgment-holds (⊢ () 1 : T) T) '(num))
;; Can we do this??? 
(test-equal (judgment-holds (⊢ () (λ ([x : num]) (zero? x)) : T) T) '()) 

;; We can also compute all the types a term has! Put a metavariable in the type position...
(test-equal (judgment-holds (⊢ () (λ ([x : num]) x) : T) T) '((num -> num)))
;; Why doesn't this work? 
(test-equal (judgment-holds (⊢ () fact-5 : T) T) '(num))
(test-equal (judgment-holds (⊢ () add1 : T) T) '((num -> num))) 
(test-equal (judgment-holds (⊢ () (λ ([x : num] [y : num]) (+ y x)) : T) T)
            '((num num -> num)))

;; Graphical illustration of type derivation tree:
#;(show-derivations (build-derivations
                   (⊢ () (λ ([x : num]) (add1 x)) : T)))

;; Ill-formed λ abstractions have no type:
(test-equal (judgment-holds (⊢ () (λ ([x : num] [x : num]) x) : T) T)
            empty)


;; defines a binary relation on terms in a given domain (terms M in this case)
(define r
    (reduction-relation
     PCF #:domain M

     ;; form of a clause:
     ;; left hand side, right hand side, side conditions, name
     ;; order irrelevant, and may overlap! 
     ;; μ clause, substitute (X (μ (X : T) M)) for M
     ;; What the heck is this??? 
     (--> (μ (X : T) M)
          (subst (X (μ (X : T) M)) M)
          μ)
     ;; β clause, substitute X for M inside this function application 
     (--> ((λ ([X : T] ...) M_0) M ...)
          (subst (X M) ... M_0)
          β)
     ;; δ clause, what does this implement??! 
     (--> (O N_0 ...) N_1
          (judgment-holds (δ (O N_0 ...) N_1))
          δ)
     ;; if-t clause (if-zero), take M_1 
     (--> (if0 0 M_1 M_2) M_1 if-t)
     ;; if-f clause (if-not-zero), take M_2 
     (--> (if0 N M_1 M_2) M_2
          (side-condition (not (zero? (term N))))
          if-f)))

(define-judgment-form PCF
    #:mode (δ I O)
    #:contract (δ (O N ...) N)
    ;; these guys interpret primitive operations
    ;; unquote "," escapes out of Redex into Racket so ,(+ ...) is calling Racket's +
    [(δ (+ N_0 N_1) ,(+ (term N_0) (term N_1)))]
    [(δ (* N_0 N_1) ,(* (term N_0) (term N_1)))]
    [(δ (sub1 N) ,(sub1 (term N)))]
    [(δ (add1 N) ,(add1 (term N)))])

;; Goes into δ clause 
(test-equal (apply-reduction-relation r (term (add1 5))) '(6))
;; is this the μ or the β clause?
;; Note that our reduction relation only takes the first substituion step! 
(test-equal (apply-reduction-relation r (term ((λ ([x : num]) x) (add1 5))))
            '((add1 5)))
;; Aaaand reduction can only happen on the outermost bits! 
(test-equal (apply-reduction-relation r (term (sub1 ((λ ([x : num]) x) (add1 5))))) '())

;; To reduce completely:
(test-equal (apply-reduction-relation* r (term (add1 5))) '(6))
(test-equal (apply-reduction-relation* r (term ((λ ([x : num]) x (add1 5)))))
            '(6))
;; Though it still does not enable reductions within terms! 
(test-equal (apply-reduction-relation* r (term (sub1 ((λ ([x : num]) x) (add1 5)))))
            '((sub1 ((λ ((x : num)) x) (add1 5)))))

;; But we can introduce an operation that will make progress if possible based on our reduction relation
(define -->r (compatible-closure r PCF M))
(test-equal (apply-reduction-relation* -->r (term ((λ ([x : num]) x) (add1 5)))) '(6))
(test-equal (apply-reduction-relation* -->r (term (sub1 ((λ ([x : num]) x) (add1 5)))))
'(5))
