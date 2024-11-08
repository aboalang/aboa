#lang racket
;;;
;;; Copyright © 2024 Christopher Augustus
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;;

(define traçar #t)

(define (traçe form . info)
  (if traçar (apply printf (string-append "## TRAÇO: " form "~n") info) '()))

;; READER

(provide (rename-out
  [aboa-read read]
  [aboa-read-syntax read-syntax]))

(define (aboa-read in) (syntax->datum (aboa-read-syntax #f in)))

(define (aboa-read-syntax fonte-path in)
  ;(define fonte-string (port->string in))
  ;(display fonte-string)
  (define fonte-tokinhos (reverse (sequence-fold
    (lambda (acc c)
      (append
        (match c
          [#\newline '(nl)]
          [#\return  '(nl)]
          [_   #:when     (eq? (car acc) 'cm) '()]
          [#\" #:when (or (eq? (car acc) 'sl)
                          (and (list? (car acc))
                               (eq? (caar acc) 'sc))) '(sr)]
          [#\" '(sl)]
          [c   #:when (or (eq? (car acc) 'sl)
                          (and (list? (car acc))
                               (eq? (caar acc) 'sc))) `((sc ,c))]
          [#\] #:when     (eq? (car acc) 'al) '(ae)]
          [#\. #:when     (eq? (car acc) 'po) '(rn)]
          [#\. '(po)] [#\, '(ac)] [#\/ '(ba)]
          [#\_ '(pa)] [#\[ '(al)] [#\] '(ar)] [#\# '(cm)]
          [#\~ '(ca)] [#\( '(ei)] [#\= '(ig)] [#\) '(ef)]
          [#\! '(fl)] [#\^ '(fu)] [#\? '(se)] [#\& '(it)]
          [#\> '(pr)] [#\< '(re)] [#\$ '(pd)] [#\: '(ti)]
          [_   #:when (char-alphabetic? c) `((ab ,c))]
          [_   #:when (char-numeric?    c) `((nu ,c))]
          [_   #:when (char-whitespace? c) '()]
          [_   `((ch ,c))])
        acc))
    '() ; initial acc
    (in-input-port-chars in))))
  (define fonte-tokens (reverse (car (sequence-fold
    (lambda (acc t)
      (match t
        ['sl          (list (car acc) '(""))]
        [(list 'sc c) (list (car acc) (list (string-append (caadr acc) (string c))))]
        ['sr          (list (append   (list (list 'st (caadr acc))) (car acc)))]
        ['pd          (list (car acc) '("$"))]
        [(list 'ab c) #:when (empty? (cdr acc))
                      (list (car acc) (list (string c)))]
        [(list 'ab c) (list (car acc) (list (string-append (caadr acc) (string c))))]
        ['po          #:when (not (empty? (cdr acc)))
                      (list (car acc) (list (string-append (caadr acc) ".")))]
        [_            #:when (not (empty? (cdr acc)))
                      (list (append   (list t) (list (list 'no (caadr acc))) (car acc)))]
        [_            (list (append   (list t                     ) (car acc)))]))
    '(()) ; initial acc
    fonte-tokinhos))))
  ;(fprintf (current-output-port) "~s" fonte-tokens)
  ;(define fonte-datum (read-aboa (open-input-string src-string))) ; racket reader strips out comments
  ;(fprintf (current-output-port) "~a" fonte-datum)
  (define module-datum `(module algoaboa "aboa.rkt" (aboa ',fonte-tokens)))
  (datum->syntax #f module-datum))

;; EXPANDER

(provide (except-out (all-from-out racket) read read-syntax #%module-begin)
         (rename-out (aboa-module-begin #%module-begin)))
(define-syntax (aboa-module-begin form)
  (syntax-case form ()
    [(#%module-begin:id body)
      #'(#%plain-module-begin body)]
    [else
      (raise-syntax-error 'aboa-module-begin
        "#lang reader \"aboa.rkt\" did not provide (#%module-begin ...)"
        (syntax->datum form))]))

(provide aboa)
(define (aboa tokens)
  (traçe "tokens:~n~s" tokens)
  (aval-recur tokens '() (list (current-command-line-arguments) 'argu 'inic 'nãop)))

(define (aval-recur tokens env pilha)
  (if (null? tokens)
    pilha
    (let*
      ([ta                     (car  tokens)]
       [td  (if (pair? tokens) (cdr  tokens) '())]
       [tad (if (pair? td)     (cadr tokens) '())]
       [p0 (match ta
          ['ei #:when (eq? tad 'fu) (cons 'nefu pilha)]
          ['ei #:when (eq? tad 'pr) (cons 'nepr pilha)]
          ['ei                      (cons 'nesi pilha)]
          ['ef                      (cons 'nesf pilha)]
          ['ti                      (cons 'tipo pilha)]
          [(list 'st s) (pilha-lite-assign pilha s)]
          [(list 'no s) (pilha-nome-assign pilha s)]
          [_ (cond [(member ta '(pa))
                    (cons   ta (cons 'opmo pilha))]
                   [(member ta '(ac ca fl ig it pr re se sl))
                    (cons   ta (cons 'opdi pilha))]
                   [else pilha])]
        )]
       [p1 (cond [(realizar-opmo p0)]
                 [(realizar-opdi p0)]
                 [else p0])]
      )
      (if (and traçar (not (eq? p0 p1))) (begin
          (traçe "p0 ~a ~v" (~r (length p0) #:min-width 2) p0)
          (traçe "p1 ~a ~v" (~r (length p1) #:min-width 2) p1)) '())
      ;(if (and (not (equal? nome ""))
      ;                (equal? n    ""))
      ;      (realizar (λ (env) env)
      ;                env traçar "_~v_ ~v" profund (string->symbol nome))
      ;      '())
      ;(if (not (eq? arg r))
      ;      (realizar (λ (env) env)
      ;                env traçar "_~v_ ~v --> ~v" profund arg r)
      ;      '())
        (aval-recur td env p1))))

(define (pilha-lite-estab pilha)
  (if (eq? 'lite (cadr pilha)) pilha (cons "" (cons 'lite pilha))))

(define (pilha-lite-assign pilha str)
  (cons str (cdr (pilha-lite-estab pilha))))

(define (pilha-nome-estab pilha)
  (if (eq? 'nome (cadr pilha)) pilha (cons "" (cons 'nome pilha))))

(define (pilha-nome-assign pilha str)
  (cons str (cdr (pilha-nome-estab pilha))))

(define (pilha-nome-append pilha str)
  (let ([p (pilha-nome-estab pilha)])
    (cons (string-append (car p) str) (cdr p))))

(define (realizar-opmo pilha)
  (if (and (not (empty? pilha)) (eq? 'opmo (cadr pilha)))
      (begin
        ;; TODO ### NO-OP FOR NOW
        (cddr pilha))
      #f))

(define (realizar-opdi pilha)
  (let*-values ([(antop  depop) (splitf-at-right pilha (λ (x) (not (eq? x 'opdi))))])
    (if (< 1 (length antop))
        (begin (traçe "antop ~v" antop)
               (traçe "depop ~v" depop)
          ;; TODO ### NO-OP FOR NOW
          depop)
        #f)))

(define (realizar proc env traçar form . info)
  (traçe form info) (proc env))
