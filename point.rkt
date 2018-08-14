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


(define (buffer-goto-char pos [m #f])
  ; todo: add narrowing
  (cond
    [m    (mark-move-to-position! m (position pos))]
    [else (mark-move-to-position! (get-point) (position pos))]))

(define-syntax (with-saved-point stx)
  (syntax-parse stx
    [(_with-saved-point body ...)
     (syntax/loc stx
       (let* ([old-point (point)]
              [our-point (point)])
         (dynamic-wind
          (λ () (buffer-goto-char our-point))
          (λ ()  body ...)
          (λ () (begin
                  (set! our-point (point))
                  (buffer-goto-char old-point))))))]))
