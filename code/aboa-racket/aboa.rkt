#lang racket
;;;
;;; Copyright © 2024 Christopher Augustus
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;;

;; READER

(require parser-tools/lex)

(provide (rename-out
  [aboa-read read]
  [aboa-read-syntax read-syntax]))

(define (aboa-read in) (syntax->datum (aboa-read-syntax #f in)))

(define (aboa-read-syntax src-path in)
  ;(define src-string (port->string in))
  ;(display src-string)
  (sequence-fold
    (lambda (acc c) (
      (let ([(prev (list-tail (length acc))])
        (match c
          [#\return '()]
          [#\newline 'eol]
          [_ (if (prev == 'comment) '()
                (match c
                  [#; 'comment]
                  [_  'something
                  ]
                )
              )
          ]
        )
      )
    )
    '()
    (in-input-port-chars in)

  ;(define src-datum (read-aboa (open-input-string src-string))) ; racket reader strips out comments
  ;(fprintf (current-output-port) "~a" src-datum)
  (define module-datum `(module algoaboa "aboa.rkt" (aboa ',src-datum)))
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
