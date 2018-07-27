#lang racket/base
(provide (all-defined-out))
(require racket/list
         "representation.rkt"
         "parameters.rkt"
         "text.rkt"
         "mark.rkt"
         "point.rkt")

;;;
;;; REGIONS
;;;

; The region is the text between point and the first mark.
; The representation of the region is therefore implicitly given by the point and the first mark.

; set-mark-command sets a mark, and then a region exists

(define (region-beginning)
  (define b     (current-buffer))
  (define mark  (buffer-the-mark b))
  (define point (buffer-point b))    
  (min (mark-position mark)
       (mark-position point)))

(define (region-end)
  (define b     (current-buffer))
  (define mark  (buffer-the-mark b))
  (define point (buffer-point b))
  (max (mark-position mark)
       (mark-position point)))

(define (region->string)
  (define b (current-buffer))
  (cond
    [(use-region?)
     (define t (buffer-text b))
     (subtext->string t (region-beginning) (region-end))]
    [else #f]))

(define (region-between-marks->string beg end [b (current-buffer)])
  (define from (min (mark-position beg) (mark-position end)))
  (define to   (max (mark-position beg) (mark-position end)))
  (subtext->string (buffer-text b) from to))

(define (use-region?)
  (define b     (current-buffer))
  (define mark  (buffer-the-mark b))
  (and #t ; (transient-mode-on? b)
       (mark-active? mark)
       (let ()
         (define beg (region-beginning))
         (define end (region-end))
         (and beg end (> end beg)))))

(define (region-mark [b (current-buffer)])
  (buffer-the-mark b))

