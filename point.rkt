#lang racket/base
(provide (all-defined-out))
;;;
;;; POINT
;;;

; Each buffer has a point. The point is the position where character and strings
; insertions happen. The point is represented as a mark.
; Use  get-point         to get the mark representing the point.
; Use  point             to get the position of the mark.
; Use  with-saved-point  to preserverve the point placement.
; If narrowing is in effect, the point movement is restricted.
; The functions  point-min  and  point-max  in "narrowing.rkt" returns
; the minimum and maximum point positions.

(require (for-syntax racket/base syntax/parse "parameters.rkt" "representation.rkt")
         racket/list
         "mark.rkt"
         "parameters.rkt"
         "representation.rkt")

; get-point : window -> mark
;   get the mark that represent the point
(define (get-point [b (current-buffer)])
  (buffer-point b))

; point : window -> integer
;   get the position of the mark
(define (point [b (current-buffer)])
  (mark-position (buffer-point b)))


(define (buffer-goto-char pos [m #f])
  ; todo: add narrowing
  (cond
    [m    (mark-move-to-position! m (position pos))]
    [else (mark-move-to-position! (get-point) (position pos))]))


; SYNTAX  (with-saved-point body ...)
;   Save the position of the body.
;   Evaluate the body.
;   Restore the original position of the point.
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
