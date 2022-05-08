#lang racket

(require racket/contract)
(require racket/pretty)
(require redex)

(module syntax racket
  (provide (all-defined-out))

  (define (language-with-context-name name)
    (string->symbol (string-append (symbol->string name) "-with-context")))

  (define (language-raw-context-name name)
    (string->symbol (string-append (symbol->string name) "-with-context-raw")))

  (define (reduction-relation-name name)
    (string->symbol (string-append (symbol->string name) "-reduction")))

  (define (resugar-reduction-relation-name name)
    (string->symbol (string-append (symbol->string name) "-resugar-reduction")))

  (define (red->redex r)
    (define rl (cadr r))
    (define rr (caddr r))
    `(--> (in-hole E ,rl)
          (in-hole E ,rr)))

  (define raws (make-hash))
  (define (set-raw k v) (hash-set! raws k v))
  (define (get-raw k) (hash-ref raws k))

  (define (is-e? x)
    (and (symbol? x) (string-prefix? (symbol->string x) "e_")))

  (define (is-v? x)
    (and (symbol? x) (string-prefix? (symbol->string x) "v_")))

  (define (e->v e)
    (string->symbol (string-replace (symbol->string e) "e_" "v_")))

  (define (deep-replace l x y)
    (define (helper l)
      (if (null? l)
          null
          (cons (deep-replace (car l) x y) (helper (cdr l)))))
    (if (list? l)
        (helper l)
        (if (eq? l x) y l)))

  (define (choose a b)
    (if (not (and a b))
        #f
        (if (is-e? a)
            a
            b)))

  (define (single-ellipsis? ctx)
    (and (= (length ctx) 2) (eq? (cadr ctx) '...)))

  (define (match-list-greedy rr ctx ctxs)
    (if (null? rr)
        '()
        (if (match-context (car rr) ctx ctxs)
            (match-list-greedy (cdr rr) ctx ctxs)
            rr)))

  (define (match-list rr ctx ctxs)
    (if (null? rr)
        (if (null? ctx)
            #t
            (single-ellipsis? ctx))
        (if (null? ctx)
            #f
            (if (> (length ctx) 1)
                (if (eq? (list-ref ctx 1) '...)
                    (match-list (match-list-greedy rr (car ctx) ctxs) (cddr ctx) ctxs)
                    (choose (match-context (car rr) (car ctx) ctxs)
                            (match-list (cdr rr) (cdr ctx) ctxs)))
                (choose (match-context (car rr) (car ctx) ctxs)
                        (match-list (cdr rr) (cdr ctx) ctxs))))))

  (define (match-context rr ctx ctxs)
    ; (display rr)(display " ")(displayln ctx)
    (cond
      ((eq? ctx 'hole) (if (is-e? rr)
                           rr
                           #f))
      ((eq? ctx 'E) (match-contexts rr ctxs))
      ((eq? ctx 'v) (is-v? rr))
      ((eq? ctx 'e) #t)
      ((eq? ctx 'x) (symbol? rr))
      ((or (not (list? rr)) (not (list? ctx))) (eq? rr ctx))
      (else (match-list rr ctx ctxs))))

  (define (match-contexts rr ctxs)
    (ormap (lambda (ctx)
             (let ([r (match-context rr ctx ctxs)])
               (if (boolean? r)
                   #f
                   r)))
           ctxs))

  (define (calc-context rl rr ctxs)
    (define rl-lazy null)
    (define rr-lazy null)
    (define (helper rl rr)
      (let ([e (match-contexts rr ctxs)])
        ; (display "step: ")(display rl)(display " ")(displayln rr)
        (if (is-e? e)
            (cons (deep-replace rl e 'E)
                  (helper (deep-replace rl e (e->v e))
                          (deep-replace rr e (e->v e))))
            (begin
              (set! rl-lazy rl)
              (set! rr-lazy rr)
              '()))))
    (cons (helper rl rr) (cons rl-lazy rr-lazy)))

  (pretty-print-columns 50)
  (define (print-wrapper x)
    (pretty-print x) x))

(require 'syntax (for-syntax 'syntax))

;;; tests
;(calc-context '(Sg e_1) '(+ e_1 0) '(hole (+ E e) (+ v E)))
;(calc-context '(Sg e_1 e_2) '(+ e_1 e_2) '(hole (+ E e) (+ v E)))
;(calc-context '(Sg e_1) '(e_1 (λ x x)) '(hole (E e) (v E)))
;(calc-context '(Sg e_1) '((λ x x) e_1) '(hole (E e) (v E)))
;;;

(define-syntax define-core-language
  (lambda (stx)
    (define d (cdr (syntax->datum stx)))
    (define name (car d))
    (define constrs (cdr d))
    (datum->syntax
     stx
     (print-wrapper
      `(define-language ,name ,@constrs)))))

(define-syntax define-context
  (lambda (stx)
    (define d (cdr (syntax->datum stx)))
    (define name (car d))
    (define name-ctx (language-with-context-name name))
    (define name-ctx-raw (language-raw-context-name name))
    (define context (cadr d))
    (define ctxs (cddr context))
    (set-raw name-ctx-raw ctxs)
    (datum->syntax
     stx
     (print-wrapper
      `(define-extended-language ,name-ctx ,name ,context)))))

(define-syntax define-reduction
  (lambda (stx)
    (define d (cdr (syntax->datum stx)))
    (define name (car d))
    (define name-ctx (language-with-context-name name))
    (define name-red (reduction-relation-name name))
    (define reds (map red->redex (cdr d)))
    (datum->syntax
     stx
     (print-wrapper
      `(define ,name-red
         (reduction-relation ,name-ctx ,@reds))))))

(define-syntax define-sugar
  (lambda (stx)
    (define d (cdr (syntax->datum stx)))
    (define sgname (car d))
    (define sgname-ctx (language-with-context-name sgname))
    (define sgname-red (reduction-relation-name sgname))
    (define sgname-resugar-red (resugar-reduction-relation-name sgname))
    (define name (cadr d))
    (define name-ctx (language-with-context-name name))
    (define name-ctx-raw (language-raw-context-name name))
    (define name-red (reduction-relation-name name))
    (define red (caddr d))
    (define rl (cadr red))
    (define rr (caddr red))
    (define resugar-info (calc-context rl rr (get-raw name-ctx-raw)))
    (datum->syntax
     stx
     (print-wrapper
      `(begin
         (define-extended-language ,sgname ,name-ctx
           (e ::= ....
              ,rl))
         (define ,sgname-red
           (extend-reduction-relation ,name-red ,sgname ,(red->redex red)))
         (define-extended-language ,sgname-ctx ,sgname
           (E ::= ....
              ,@(car resugar-info)))
         (define ,sgname-resugar-red
           (extend-reduction-relation ,name-red ,sgname-ctx
                                      (--> (in-hole E ,(cadr resugar-info))
                                           (in-hole E ,(cddr resugar-info))))))))))

(define-syntax trace
  (lambda (stx)
    (define d (cdr (syntax->datum stx)))
    (define name (car d))
    (define name-red (reduction-relation-name name))
    (define t (cadr d))
    (datum->syntax
     stx
     (print-wrapper
      `(traces ,name-red (term ,t))))))

(define-syntax trace-resugar
  (lambda (stx)
    (define d (cdr (syntax->datum stx)))
    (define name (car d))
    (define name-resugar-red (resugar-reduction-relation-name name))
    (define t (cadr d))
    (datum->syntax
     stx
     (print-wrapper
      `(traces ,name-resugar-red (term ,t))))))

(provide
 define-core-language
 define-context
 define-reduction
 define-sugar
 trace
 trace-resugar
 (all-from-out redex))
