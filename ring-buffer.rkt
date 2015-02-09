#lang racket/base
(require racket/match racket/format)
;;;
;;; RING BUFFER
;;;

; A ring buffer holds up to max-size elements.
; The next element is inserted at index index.

(struct ring (buffer index size max-size) #:transparent #:mutable)

(define (new-ring [max-size 60])
  (ring (make-vector max-size) 0 0 max-size))

(define (ring-insert! r x)
  (match-define (ring b i s m) r)
  (vector-set! b i x)
  (set-ring-index! r (remainder (+ i 1) m))
  (when (< s m)
    (set-ring-size! r (+ s 1))))

(define (ring-ref r j)
  (match-define (ring b i s m) r)
  (unless (< j s)
    (error 'ring-ref (~a "index " j " larger than ring size " s)))
  (vector-ref b (modulo (- i 1 j) m)))

(module+ test (require rackunit)
  (define r (new-ring 5))
  (for ([i 7]) (ring-insert! r i))
  (check-equal? (for/list ([i 5]) (ring-ref r i))
                '(6 5 4 3 2)))
                

