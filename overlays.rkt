#lang racket/base
(provide overlays-ref
         overlays-set!)
         
;;;
;;; OVERLAYS
;;;

; Overlays are not part of the text.
; Overlays belong to a buffer.
; The distinction is important, for example when the same text is displayed in two different buffers.

; Representation:
;   An overlay holds an hash table from symbols (names) to interval maps.
;   


(require data/interval-map
         "representation.rkt")

; overlays-set! : overlay integer integer symbol value -> void
(define (overlays-set! o from to sym val)
  (define ht (overlays-ht o))
  (define im (hash-ref ht sym #f))
  (define before (interval-map-ref im (- from 1) #f))
  (define after  (interval-map-ref im   to       #f))
  (cond
    [(not im)
     ; if the symbol sym has not corresponding interval-map yet,
     ; we create one - and set the value
     (set! im (make-interval-map))
     (hash-set! ht sym im)
     (interval-map-set! from to val)]
    [(and before after (equal? before val) (equal? after val))
     ; the value before and after the interval are associated to the same value,
     ; so we can coalesce the intervals.
     (define-values (from1 _ __)   (interval-map-ref/bounds im (- from 1)))
     (define-values (___ to2 ____) (interval-map-ref/bounds im to))
     (interval-map-set! from1 to2 val)]
    [(and before (equal? before val))
     ; coalesce with previous interval
     (define-values (from1 _ __) (interval-map-ref/bounds im (- from 1)))
     (interval-map-set! from1 to val)]
    [(and after (equal? after val))
     ; coalesce with next interval
     (define-values (_ to1 __) (interval-map-ref/bounds im to))
     (interval-map-set! from to1 val)]
    [else
     ; no coalescing needed
     (interval-map-set! from to val)]))

; overlays-ref : overlay symbol integer -> value
;   If no value is associated with the integer i, then the default argument is used.
;   If the default is a procedure, it is called. If not, the default value is returned.
(define (overlays-ref o sym i [default (λ () (error 'overlays-ref "no mapping found"))])
  (define ht (overlays-ht o))
  (define im (hash-ref ht sym #f))
  (interval-map-ref im i default))


(define (overlays-ref/bounds o sym i [default #f])
  (define ht (overlays-ht o))
  (define im (hash-ref ht sym #f))
  (cond [im                   (interval-map-ref/bounds im i default)]
        [(procedure? default) (default)]
        [else                 default]))

