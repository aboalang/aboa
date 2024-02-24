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
(define (read-syntax src in)
  (with-syntax ([str (port->string in)])
    (strip-context
     #'(module algoaboa "aboa.rkt" str))))

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
