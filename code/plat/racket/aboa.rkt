#lang racket
;;;
;;; Copyright © 2024 Christopher Augustus
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;;

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
          [#\. '(po)] [#\, '(ac)]
          [#\_ '(ag)] [#\[ '(al)] [#\] '(ar)] [#\# '(cm)]
          [#\~ '(ca)] [#\( '(ei)] [#\= '(eq)] [#\) '(ef)]
          [#\! '(fl)] [#\^ '(fu)] [#\? '(if)] [#\& '(it)]
          [#\> '(pr)] [#\< '(re)] [#\$ '(sd)] [#\% '(ty)]
          [_   #:when (char-whitespace? c) '()]
          [_   `((ch ,c))])
        acc))
    '() ; initial acc
    (in-input-port-chars in))))
  (define fonte-tokens (reverse (car (sequence-fold
    (lambda (acc t)
      (match t
        ['sl          (list (car acc) '("")    )]
        [(list 'sc c) (list (car acc) (list (string-append (caadr acc) (string c))))]
        ['sr          (list (append   (list (list 'st (caadr acc))) (car acc)))]
        [_            (list (append   (list t                     ) (car acc)))]))
    '(() ()) ; initial acc
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
  (fprintf (current-output-port) "ABOA TOKENS:\n~s\nABOA ANALISADA:\n" tokens)
  (aval-recur tokens '() (current-command-line-arguments) "" 0 #f))

(define traçar #t)

(define (aval-recur tokens env arg nomeent nivel aplicar)
  (if (null? tokens)
    '()
    (let
      ([m (match (car tokens)
        [(list 'ch c) (cons arg (string-append nomeent (string c)))]
        ;['ei          #:when (eq? (cadr tokens) 'pr)
        ;                (and (list? (car acc))
        ;                     (eq? (caar acc) 'sc))) '(sr)]
        ['po          (cons arg (string-append nomeent "."))]
        ;['pr          (aval-recur tokens env arg nomeent nivel #t)]
        ['sd          (cons arg "$")]
        [(list 'st s) (cons s   "")]
        [_            (cons arg "")]
      )])
      (let
        ([res (car m)] [nomesaí (cdr m)])
        (if (and (not (equal? nomeent ""))
                      (equal? nomesaí ""))
            (realizar (λ (env) env)
                      env traçar "_~v_ ~v" nivel (string->symbol nomeent))
            '())
        (if (not (eq? arg res))
            (realizar (λ (env) env)
                      env traçar "_~v_ ~v --> ~v" nivel arg res)
            '())
        (aval-recur (cdr tokens) env res nomesaí nivel aplicar)))))

(define (realizar proc env traçar form . info)
    (if traçar (apply printf (string-append "TRAÇO: " form "\n") info) '())
    (proc env))
