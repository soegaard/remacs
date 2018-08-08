#lang racket/base
(provide (all-defined-out))
;;;
;;; NARROWING
;;;

; Narrowing restricts the area displayed and edited.
; The restricted area is delimited by the marks restriction-start
; and restriction-end stored in the buffer structure.

(require "buffer.rkt"
         "commands.rkt"
         "parameters.rkt"
         "mark.rkt"
         "point.rkt"
         "region.rkt"
         "representation.rkt")

(define-interactive (narrow-to-region [start #f] [end #f])
  (displayln 'narrow-to-region (current-error-port))
  (define b (current-buffer))
  ; 1. Check input
  (when (and start (integer? start) (< start 0))
    (error 'narrow-to-region "start must be non-negative, got ~a" start))
  (when (and end (integer? end) (< end (buffer-length b)))
    (error 'narrow-to-region "end must be less than the length of the buffer , got ~a" end))  
  ; 2. Find start and end of restriction.
  ;    Use region if start and end aren't supplied.
  (define from    (or start (region-beginning)))
  (define to      (or end   (region-end)))
  (unless (mark<= from to)
    (set!-values (from to) (to from)))
  ; 3. Set restriction
  (set-buffer-restriction-start! b (new-mark b "*restriction-start*" from #:insertion-type #f))
  (set-buffer-restriction-end!   b (new-mark b "*restriction-end*"   to   #:insertion-type #t))
  ; 4. Move point into restricted area if neceessary
  (define (move-mark-into-restriction m)
    (when (mark< m from)
      (mark-move-to-position! m from))
    (when (mark> m to)
      (mark-move-to-position! m to)))
  (move-mark-into-restriction (get-point))
  (move-mark-into-restriction (get-mark))
  ; 5. Set restriction flag
  (set-buffer-restricted?! b #t))
  
(define-interactive (widen)
  (define b (current-buffer))
  (set-buffer-restricted?! b #f)
  (define from (buffer-restriction-start b))
  (define to   (buffer-restriction-end   b))
  (delete-mark! from)
  (delete-mark! to))

(define (buffer-narrowed? [b (current-buffer)])
  (buffer-restricted? b))

(define (start-of-buffer-position)
  (define b (current-buffer))
  (if (buffer-restricted? b)
      (position (buffer-restriction-start b))
      0))

(define (end-of-buffer-position)
  (define b (current-buffer))
  (if (buffer-restricted? b)
      (position (buffer-restriction-end b))
      (- (buffer-length b) 1)))

(define (point-min)
  (start-of-buffer-position))

(define (point-max)
  (end-of-buffer-position))


