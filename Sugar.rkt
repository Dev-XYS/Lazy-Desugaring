#lang racket

(require "Extend.rkt")

(define-core-language λ
  (e ::= v (e e) x)
  (v ::= (λ x e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ x e #:refers-to x))

(define-context λ
  (E ::= hole (E e) (v E)))

(define-reduction
  λ
  (--> ((λ x e) v)
       (substitute e x v)
       "β"))

(define-sugar Sg λ
  (--> (Sg e_1)
       (e_1 (λ x x))))

;(trace Sg (Sg (λ x x)))

(define-core-language Boolean
  (e ::= v (e e) x (if e e e))
  (v ::= (λ x e) true false)
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ x e #:refers-to x))

(define-context Boolean
  (E ::= hole (E e) (v E) (if E e e)))

(define-reduction
  Boolean
  (--> ((λ x e) v)
        (substitute e x v)
        "β")
  (--> (if true e_1 e_2)
       e_1)
  (--> (if false e_1 e_2)
       e_2))

(define-sugar Bool-with-And Boolean
  (--> (And e_1 e_2)
       (if e_1 e_2 false)))

(trace-resugar Bool-with-And (And (And true false) false))
