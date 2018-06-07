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
;   Delete all characters in region.
(define (region-delete-between! beg end [b (current-buffer)])
  (cond    
    [(mark< beg end) (buffer-dirty! b)
                     (define n (- (mark-position end) (mark-position beg)))
                     (define end-is-a-mark? (member end (buffer-marks b) eq?))
                     ; buffer-delete-backward-char! will update the positions
                     ; of all marks in buffer-marks, so if end is a mark (not the point)
                     ; we need to temporarily remove it.
                     (when end-is-a-mark?
                       (set-buffer-marks! b (remove end (buffer-marks b) eq?)))
                     (with-saved-point
                         (begin                           
                           (set-point! end)
                           (buffer-delete-backward-char! b n)))
                     (when end-is-a-mark?
                       (set-buffer-marks! b (cons end (buffer-marks b))))]
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
