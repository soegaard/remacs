#lang racket/base
(provide (all-defined-out))
;;;
;;; THREAD-SAFE INTERVAL MAPS
;;;

(require racket/contract/base
         racket/dict
         racket/list
         racket/match
         data/interval-map)

(struct safe-interval-map (sema im))

(define (save-interval-map-wait interval-map)
  (define sema (safe-interval-map-sema interval-map))
  (semaphore-wait sema))

(define (save-interval-map-post interval-map)
  (define sema (safe-interval-map-sema interval-map))
  (semaphore-post sema))


(define (make-safe-interval-map [contents '()]	 
                           #:key-contract   [key-contract any/c]
                           #:value-contract [value-contract any/c])
  (define sema (make-semaphore 1))
  (define im   (make-interval-map contents
                                  #:key-contract   key-contract
                                  #:value-contract value-contract))
  (safe-interval-map sema im))

(define none (gensym "none"))

(define (safe-interval-map-ref interval-map position [default none])
  (save-interval-map-wait interval-map)
  (define im (safe-interval-map-im interval-map))
  (begin0
    (if (eq? default none)
        (interval-map-ref im position)
        (interval-map-ref im position default))
    (save-interval-map-post interval-map)))

(define (safe-interval-map-ref/bounds interval-map position [default none])
  (save-interval-map-wait interval-map)
  (define im (safe-interval-map-im interval-map))
  (begin0
    (if (eq? default none)
        (interval-map-ref/bounds im position)
        (interval-map-ref/bounds im position default))
    (save-interval-map-post interval-map)))

(define (safe-interval-map-set! interval-map start end value)
  (when (not (= start end))
    (define sema (safe-interval-map-sema interval-map))
    (define im (safe-interval-map-im interval-map))
    (semaphore-wait sema)
    (interval-map-set! im start end value)
    (semaphore-post sema)))

(define (uniqify xs)
  (match xs
    ['() '()]
    [(list a) xs]
    [(list* a b cs) (if (= a b)
                        (uniqify (rest xs))
                        (cons a (uniqify (rest xs))))]))

(define (safe-interval-map-positions interval-map)
  (save-interval-map-wait interval-map)
  (define im (safe-interval-map-im interval-map))    
  (begin0
    (let-values ([(froms tos) (for/lists (froms tos) ([k (in-dict-keys im)])
                                (match k [(cons f t) (values f t)]))])
      (uniqify (sort (append froms tos) <)))
    (save-interval-map-post interval-map)))

(define (safe-interval-map-expand! interval-map start end)
  (when (not (= start end))
    (save-interval-map-wait interval-map)
    (define im (safe-interval-map-im interval-map))    
    (interval-map-expand! im start end)
    (save-interval-map-post interval-map)))

(define (safe-interval-map-contract! interval-map start end)
  (save-interval-map-wait interval-map)
  (define im (safe-interval-map-im interval-map))    
  (interval-map-contract! im start end)
  (save-interval-map-post interval-map))
