#lang racket

(require "Extend.rkt")

(define-core-language Core
  (e ::= v x (e e) (if e e e) (let (x e) e) (null? e) (car e) (cdr e))
  (v ::= true false number null (cons e e) (λ x e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ x e #:refers-to x)
  (let (x e) e #:refers-to x))

(define-context Core
  (E ::= hole (E e) (v E) (if E e e) (null? E) (car E) (cdr E) (let (x E) e)
     (cons E e) (cons v E)))

(define-reduction
  Core
  (--> ((λ x e) v)
       (substitute e x v))
  (--> (let (x v_1) e_2)
       (substitute e_2 x v_1))
  (--> (if true e_1 e_2)
       e_1)
  (--> (if false e_1 e_2)
       e_2)
  (--> (null? null)
       true)
  (--> (null? (cons v_1 v_2))
       false)
  (--> (car (cons v_1 v_2))
       v_1)
  (--> (cdr (cons v_1 v_2))
       v_2))

(define-sugar Surf Core
  (--> (Map e_1 e_2)
       (let (y e_2)
         (let (x e_1)
           (if (null? y)
               null
               (cons (x (car y)) (Map x (cdr y))))))))

;(trace-resugar Surf (null? (cons 2 null)))
(trace-resugar Surf (Map (λ x x) (cons 2 (cons 3 null))))
