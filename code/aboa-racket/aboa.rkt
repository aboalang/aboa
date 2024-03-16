#lang racket
;;;
;;; Copyright © 2024 Christopher Augustus
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;;
(require syntax/strip-context)

(provide read)
(define (read in)
  (syntax->datum
   (read-syntax #f in)))

(provide read-syntax)
(define (read-syntax src-path in)
  (with-syntax ([src-lines (port->string in)])
    (strip-context
     #`(module algoaboa racket
        ;
        ;
        (#%datum . src-lines)
        ;(display 'str)
        ;(display "FUCK OFF\n")
        ))))
  ;(define src-lines (port->lines in))
  ;(define src-datums (map (^ (x) (quote x)) src-lines))
  ;(define module-datum `(module algoaboa racket
  ;                        ,@src-datums))
  ;(datum->syntax #f module-datum))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out (aboa-module-begin #%module-begin)))
(define-syntax (aboa-module-begin form)
  ;;(display form)
  (syntax-case form ()
    [(#%module-begin:id body)
      ;;#'(#%plain-module-begin (display body))]
      #'(#%plain-module-begin body)]
    [else
      (raise-syntax-error 'aboa-module-begin
        "#lang reader \"aboa.rkt\" did not provide (#%module-begin ...)"
        (syntax->datum form))]))
