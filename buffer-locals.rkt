#lang racket
(provide set-buffer-local!
         ref-buffer-local
         lookup-default)

(require "representation.rkt"
         (for-syntax syntax/parse))

;;;
;;; BUFFER LOCALS
;;;

(module* buffer-top #f
  (provide (rename-out [buffer-top #%top]))
  ; (require (for-syntax racket/base syntax/parse))
  (define-syntax (buffer-top stx)
    (syntax-parse stx
      [(_ . id:id)
       (syntax/loc stx
         (let ()
           (define b (current-buffer))
           (cond
             [(ref-buffer-local b 'id #f) => values]
             [else (lookup-default 'id)])))])))

(define-namespace-anchor default-namespace-anchor)
(define default-namespace (namespace-anchor->namespace default-namespace-anchor))

(define (set-buffer-local! b sym v)
  (define ns (buffer-locals b))
  (namespace-set-variable-value! sym v #f ns))

(define (ref-buffer-local b sym 
                          [on-error (λ () (error 'ref-buffer-local (~a sym " is undefined")))])
  (define (on-failure) (if (procedure? on-error) (on-error) on-error))
  (define ns (buffer-locals b))
  (namespace-variable-value sym #t on-failure ns))

(define (lookup-default sym [on-error #f])
  (namespace-variable-value sym #t on-error default-namespace))
