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

; get-point : window -> mark
(define (get-point [b (current-buffer)])
  (buffer-point b))

; point : window -> integer
(define (point [b (current-buffer)])
  (mark-position (buffer-point b)))

; set-point! : mark buffer -> void
#;(define (set-point! m [b (current-buffer)])
    (set-buffer-points! b (cons m (rest (buffer-points b)))))

