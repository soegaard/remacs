#lang racket/base
(provide (all-defined-out))

(require racket/list "parameters.rkt" "representation.rkt" "mark.rkt")
(require (for-syntax syntax/parse racket/base "parameters.rkt" "representation.rkt"))

(define (get-point [b (current-buffer)])
  (buffer-point b))

(define (set-point! m [b (current-buffer)])
  (set-buffer-points! b (cons m (rest (buffer-points b)))))


(define-syntax (with-saved-point stx)
  (syntax-parse stx
    [(_with-saved-point body ...)
     (syntax/loc stx
       (let* ([b         (current-buffer)]
              [points    (buffer-points b)]
              [old-point (first points)]
              [new-point (copy-mark old-point)])
         (set-point! new-point b)
         (begin0 (begin body ...)
                 (set-point! old-point b))))]))
