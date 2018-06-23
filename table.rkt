#lang racket/base
(provide (all-defined-out))
;;;
;;; TABLE
;;;

; Properties and overlays are represented as tables.
; A table is a map from symbols to lists of values.

(require "representation.rkt")

(define (table-add! t s v)
  (define ht (table-ht t))
  (define vs (hash-ref ht s '()))
  (hash-set! ht s (cons v vs)))

(define (table-set! t s v)
  (define ht (table-ht t))
  (hash-set! ht s (list v)))

(define (table-remove! t s v [proc equal?])
  (define ht (table-ht t))
  (define vs (hash-ref ht s '()))
  (hash-set! ht s (remove v vs proc)))

(define (table-remove-all! t s)
  (define ht (table-ht t))
  (hash-remove! ht s))

  
