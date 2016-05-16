#lang racket/base
(require racket/match racket/format)
;;;
;;; RING BUFFER (AKA CIRCULAR STACK)
;;;

(provide new-ring ring-insert! ring-ref ring->list in-ring ring-empty?)

; A ring buffer holds up to max-size elements.
; The next element is inserted at index index.
; Inserting elements in a full buffer will overwrite the oldest element.

(struct ring (buffer index size max-size) #:transparent #:mutable)

; new-ring : [natural] -> ring
;   make ring with room for max-size elements
(define (new-ring [max-size 60])
  (ring (make-vector max-size) 0 0 max-size))

; ring-insert! : ring element -> void
;  insert the element x in the ring r
(define (ring-insert! r x)
  (match-define (ring b i s m) r)
  (vector-set! b i x)
  (set-ring-index! r (remainder (+ i 1) m))
  (when (< s m)
    (set-ring-size! r (+ s 1))))

; ring-ref : ring index -> element
;   return the j'th element in the ring
(define (ring-ref r j)
  (match-define (ring b i s m) r)
  (unless (< j s)
    (error 'ring-ref (~a "index " j " larger than ring size " s)))
  (vector-ref b (modulo (- i 1 j) m)))

; ring->list : ring -> list
;   return a list of the elements in the ring r
(define (ring->list r)
  (match-define (ring b i s m) r)
  (for/list ([j (in-range (- i 1) (- i 1 s) -1)])
    (vector-ref b (modulo j m))))

(define (in-ring r)
  (struct pos (i m))
  (define (pos->elm p) (ring-ref r (pos-i p)))
  (define (next-pos p) (pos (+ (pos-i p) 1) (pos-m p)))
  (define initial-pos  (pos 0 (ring-size r)))
  (define (continue-with-pos? p) (< (pos-i p) (pos-m p)))
  (make-do-sequence
   (λ () (values pos->elm next-pos initial-pos continue-with-pos? #f #f))))

; ring-empty? : ring -> boolean
;   is the ring empty?
(define (ring-empty? r)
  (= (ring-size r) 0))

(module+ test (require rackunit)
  (define r (new-ring 5))
  (for ([i 7]) (ring-insert! r i))
  (check-equal? (for/list ([i 5]) (ring-ref r i))
                '(6 5 4 3 2))
  (check-equal? (ring->list r) '(6 5 4 3 2))
  (check-equal? (for/list ([x (in-ring r)]) x) '(6 5 4 3 2)))