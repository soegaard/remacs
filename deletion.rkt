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

; region-delete-between! : integer integer -> void
;   Delete all characters between positions beg and end.
(define (region-delete-between! beg end)
  (define b     (current-buffer))
  (define point (buffer-point b))
  (cond    
    [(mark< beg end) (buffer-dirty! b)
                     (define from (position beg))
                     (define to   (position end))
                     (define n    (abs (- to from)))
                     (define end-is-a-mark? (member end (buffer-marks b) eq?))
                     (when end-is-a-mark?
                       #;(mark-move-to-position! end   from)
                       (mark-move-to-position! point to))
                     (buffer-delete-backward-char! b point n)]
    [(mark< end beg) (region-delete-between! end beg)]
    [else            (void)]))

; region-delete! :  -> void
;   Delete all characters in region.
(define (region-delete)
  (define b     (current-buffer))
  (define mark  (buffer-the-mark b))
  (define point (buffer-point b))
  (when (use-region?)
    (buffer-dirty! b)
    (region-delete-between! mark point)
    (mark-deactivate! mark)))

; Note: Emacs has delete-active-region, delete-and-extract-region, and, delete-region
