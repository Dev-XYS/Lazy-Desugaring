#lang racket

(require "Extend.rkt")

(define-core-language Boolean
  (e ::= v (if e e e))
  (v ::= true false))

(define-context Boolean
  (E ::= hole (if E e e)))

(define-reduction
  Boolean
  (--> (if true e_1 e_2)
       e_1)
  (--> (if false e_1 e_2)
       e_2))

(define-sugar Bool-with-And Boolean
  (--> (And e_1 e_2)
       (if e_1 e_2 false)))

;(trace Bool-with-And (And (And true false) false))
(trace-resugar Bool-with-And (And (And true false) false))
