#lang racket/base
(provide (all-defined-out))

;;;
;;; POINT
;;;

(require (for-syntax racket/base syntax/parse "parameters.rkt" "representation.rkt")
         racket/list
         "mark.rkt"
         "parameters.rkt"
         "representation.rkt")

; get-point : buffer -> mark
(define (get-point [b (current-buffer)])
  (buffer-point b))

; point : buffer -> integer
(define (point [b (current-buffer)])
  (mark-position (buffer-point b)))

; set-point! : mark buffer -> void
(define (set-point! m [b (current-buffer)])
  (set-buffer-points! b (cons m (rest (buffer-points b)))))

; SYNTAX   (with-saved-point body ...)
;   Save position of point before evaluating body ...
;   After evalation of body restore the saved position.
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
                 ; (delete-mark! new-point) ; xxx
                 (set-point! old-point b))))]))
