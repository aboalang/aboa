#lang racket
;;;
;;; Copyright © 2024 Christopher Augustus
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;;

(define traçar #f)

(define (traçe form . info) (if traçar
  (apply  printf (string-append "## aboa traço: " form "~n") info) '()))

(define (erro form . info)
  (apply eprintf (string-append "!! aboa error: " form "~n") info))

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
          [#\. '(po)] [#\, '(ac)] [#\/ '(ba)] [#\_ '(pi)]
          [#\[ '(al)] [#\] '(ar)] [#\# '(cm)] [#\~ '(ca)]
          [#\( '(ni)] [#\= '(ig)] [#\) '(nf)] [#\! '(fl)]
          [#\^ '(fu)] [#\* '(pr)] [#\? '(se)] [#\& '(it)]
          [#\> '(ex)] [#\< '(re)] [#\$ '(pd)] [#\: '(ti)]
          ;;[#\- '(tr)] ;; TODO: ### trata como alpha por agora
          [#\-                             `((ab ,c))]
          [_   #:when (and (char-whitespace? c) (eq? (car acc) 'eb)) '()]
          [_   #:when (char-whitespace? c) '(eb)]
          [_   #:when (char-alphabetic? c) `((ab ,c))]
          [_   #:when (char-numeric?    c) `((nu ,c))]
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
  (aval-recur '() tokens '() (list (current-command-line-arguments) 'argu 'inic 'nãop)))

(define (aval-recur tz tokens env pilha)
  (if (null? tokens)
    pilha
    (let*
      ([ta                     (car  tokens)]
       [td  (if (pair? tokens) (cdr  tokens) '())]
       [tad (if (pair? td)     (cadr tokens) '())]
       [p0 (match ta
          [(or 'cm 'eb 'nl)                      pilha ] ;; ignore
          [(or 'fu 'pr) #:when (or (eq? tz 'ex)
                                   (eq? tz 'ni)) pilha ] ;; já usado
          ['ex #:when (eq? tad 'fu)  (cons 'exfu pilha)]
          ['ex #:when (eq? tad 'pr)  (cons 'expr pilha)]
          ['ni #:when (eq? tad 'fu)  (cons 'defu pilha)]
          ['ni #:when (eq? tad 'pr)  (cons 'depr pilha)]
          ['ni                       (cons 'nesi pilha)]
          ['nf                       (cons 'nesf pilha)]
          ['ti                       (cons 'tipo pilha)]
          [(list 'st s) (pilha-lite-assign pilha s)]
          [(list 'no s) (pilha-nome-assign pilha s)]
          [_ (cond [(member ta '(pi))
                    (cons   ta (cons 'opmo pilha))]
                   [(member ta '(ac ca fl ig it re se sl))
                    (cons   ta (cons 'opdi pilha))]
                   [else pilha])]
        )]
       [p1 (cond [(eq? p0 pilha) p0]
                 ;;[(realizar-exfu p0)]
                 [(realizar-expr p0)]
                 ;;[(realizar-opmo p0)]
                 ;;[(realizar-opdi p0)]
                 [else p0])]
      )
      (if (and traçar (not (eq? p0 p1))) (begin
          (traçe "p0 ~a ~v" (~r (length p0) #:min-width 3) p0)
          (traçe "p1 ~a ~v" (~r (length p1) #:min-width 3) p1)) '())
      ;(if (and (not (equal? nome ""))
      ;                (equal? n    ""))
      ;      (realizar (λ (env) env)
      ;                env traçar "_~v_ ~v" profund (string->symbol nome))
      ;      '())
      ;(if (not (eq? arg r))
      ;      (realizar (λ (env) env)
      ;                env traçar "_~v_ ~v --> ~v" profund arg r)
      ;      '())
      (aval-recur ta td env p1))))

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

(define (realizar-expr pilha)
  (if (and (< 3  (length pilha)) (eq? 'expr (caddr pilha)))
      (let ([ppri  (car  pilha)]
            [pseg  (cadr pilha)]
            [pres (cdddr pilha)])
        ;(traçe "ppri ~v" ppri)
        ;(traçe "pseg ~v" pseg)
        ;(traçe "pres ~v" pres)
        (if (not (eq? 'nome pseg))
            (begin (erro ">* missing name of procedure")
                   (cons ppri (cons pseg pres)))
            (match ppri
              ["$io.si"  (exe-pad-io-si  ppri pres)]
              ["$io.sof" (exe-pad-io-sof ppri pres)]
              ;; TODO: ### MUITO MAIS
              [_ (begin (erro ">* unknown procedure named ~a" ppri)
                        pres)])))
      #f))

(define (exe-pad-io-si nome pilha)
  (if (req-arg-lite-em-pilha nome pilha)
      (begin (printf (car pilha))
             (cons (read-line) (cons 'lite (cddr pilha))))
      pilha))

(define (exe-pad-io-sof      nome pilha)
  (if (req-arg-lite-em-pilha nome pilha)
      (begin (printf (car pilha))
             (cddr pilha))
      pilha))

(define (req-arg-lite-em-pilha nome pilha)
  (if (and (< 1 (length pilha)) (eq? 'lite (cadr pilha)))
      pilha
      (begin (erro "literal arg required for ~a" nome)
             #f)))

#| TODO: ### AGORA NÃO USAR

(define (realizar-opmo pilha)
  (if (and (not (empty? pilha)) (eq? 'opmo (cadr pilha)))
      (begin
        ;; TODO ### NO-OP FOR NOW
        (cddr pilha))
      #f))

(define (realizar-pr paira paird)
    (traçe "pr   a ~v" paira) (traçe "pr   d ~v" paird)
    (match paird
      ['("$io.sof" . nome) (printf (car paira))]
      ;; TODO: ### MUITO MAIS
      [_ (traçe "pr não funçiona: ~v > ~v" paird paira)]
    ))

(define (realizar-opdi pilha)
  (let*-values ([(opdid opdia) (splitf-at-right pilha (λ (x) (not (eq? x 'opdi))))])
    (if (< 3 (length opdid))
        (begin ;; TODO ###: HACK THAT WE HAVE RIGHT ARG
          (traçe "opdi a ~v" opdia) (traçe "opdi d ~v" opdid)
          (let* ([a (cons (car opdia) (cadr opdia))]
                 [d (cons (car opdid) (cadr opdid))]
                 [o (caddr opdid)]
                 [p (cddr opdia)]) ;; TODO ###: CONSUMING ALL FOR NOW
            (match o
              ['pr (realizar-pr a d)]
              ;; TODO: ### MUITO MAIS
              [_ (traçe "opdi não funçiona: ~v" o)]
            )
          p))
        #f)))

(define (realizar-pr paira paird)
    (traçe "pr   a ~v" paira) (traçe "pr   d ~v" paird)
    (match paird
      ['("$io.sof" . nome) (printf (car paira))]
      ;; TODO: ### MUITO MAIS
      [_ (traçe "pr não funçiona: ~v > ~v" paird paira)]
    ))

(define (realizar proc env traçar form . info)
  (traçe form info) (proc env))
|#
