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

(define (aboa-read-syntax src-path in)
  ;(define src-string (port->string in))
  ;(display src-string)
  (define src-tokens (reverse (sequence-fold
    (lambda (acc c)
      (append
        (match c
          [#\newline '(en)]
          [#\return  '(en)]
          [_   #:when     (eq? (car acc) 'co) '()]
          [#\" #:when (or (eq? (car acc) 'sl)
                          (and (list? (car acc))
                               (eq? (caar acc) 's))) '(sr)]
          [#\" '(sl)]
          [c   #:when (or (eq? (car acc) 'sl)
                          (and (list? (car acc))
                               (eq? (caar acc) 's))) `((s ,c))]
          [#\_ '(ag)]
          [#\[ '(al)]
          [#\] #:when     (eq? (car acc) 'al) '(ae)]
          [#\] '(ar)]
          [#\; '(co)]
          [#\~ '(ca)]
          [#\. #:when     (eq? (car acc) 'dt) '(rn)]
          [#\. '(dt)]
          [#\( '(el)]
          [#\= '(eq)]
          [#\) '(er)]
          [#\! '(fl)]
          [#\^ '(fu)]
          [#\? '(if)]
          [#\& '(it)]
          [#\> '(pr)]
          [#\< '(re)]
          [#\$ '(sd)]
          [#\% '(ty)]
          [_   #:when (char-whitespace? c) '()]
          [_   `((c ,c))])
        acc))
    '() ; initial acc
    (in-input-port-chars in))))
  (fprintf (current-output-port) "~s" src-tokens)
  ;(define src-datum (read-aboa (open-input-string src-string))) ; racket reader strips out comments
  ;(fprintf (current-output-port) "~a" src-datum)
  (define module-datum `(module algoaboa "aboa.rkt" (aboa ',src-tokens)))
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
(define (aboa x) (fprintf (current-output-port) "~s\n" x))
