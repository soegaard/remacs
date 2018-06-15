#lang racket/base
(provide looking-at)

(require "buffer.rkt"
         "mark.rkt"
         "parameters.rkt"
         "point.rkt"
         "representation.rkt"
         "text.rkt")

(define (rest-of-line)
  (define m  (get-point))
  (define p1 (mark-position m))
  (define p2 (position-of-end-of-line m))
  (subtext->string (buffer-text (current-buffer)) p1 p2))

(define (looking-at regexp)
  (and (regexp-match regexp (rest-of-line)) #t))

  
