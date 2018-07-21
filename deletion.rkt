#lang racket/base
(provide region-delete
         region-delete-between!)
         
;;;
;;; DELETION
;;;

(require "buffer.rkt"
         "mark.rkt"
         "parameters.rkt"
         "point.rkt"
         "region.rkt"
         "representation.rkt")

; region-delete-between! : [buffer] -> void
;   Delete all characters between positions beg and end.
(define (region-delete-between! beg end [b (current-buffer)])
  (cond    
    [(mark< beg end) (buffer-dirty! b)
                     (define from (position beg))
                     (define to   (position end))
                     (define n    (abs (- to from)))
                     (define end-is-a-mark? (member end (buffer-marks b) eq?))
                     (when end-is-a-mark?
                       (buffer-move-mark-to-position!  end from)
                       (buffer-move-point-to-position! b   to))
                     (buffer-delete-backward-char! b n)
                     (buffer-move-point-to-position! b from)]
    [(mark< end beg) (region-delete-between! end beg b)]
    [else            (void)]))

; region-delete! : [buffer] -> void
;   Delete all characters in region.
(define (region-delete [b (current-buffer)])
  (define mark  (get-mark b))
  (define point (get-point b))
  (when (use-region? b)
    (buffer-dirty! b)
    (region-delete-between! mark point b)
    (mark-deactivate! mark)))

; Note: Emacs has delete-active-region, delete-and-extract-region, and, delete-region
