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

(define (region-beginning [b (current-buffer)])
  (define marks (buffer-marks b))
  (and (not (empty? marks))
       (let ()
         (define mark (first marks))
         (define point (buffer-point b))
         (min (mark-position mark)
              (mark-position point)))))

(define (region-end [b (current-buffer)])
  (define marks (buffer-marks b))
  (and (not (empty? marks))
       (let ()
         (define mark (first marks))
         (define point (buffer-point b))
         (max (mark-position mark)
              (mark-position point)))))

(define (region->string [b (current-buffer)])
  (cond
    [(use-region? b)
     (define t (buffer-text b))
     (subtext->string t (region-beginning b) (region-end b))]
    [else #f]))

(define (region-between-marks->string beg end [b (current-buffer)])
  (define from (min (mark-position beg) (mark-position end)))
  (define to   (max (mark-position beg) (mark-position end)))
  (subtext->string (buffer-text b) from to))

(define (use-region? b)
  (define marks (buffer-marks b))
  (and #t ; (transient-mode-on? b)
       (not (empty? marks))
       (mark-active? (first marks))
       (let ()
         (define beg (region-beginning b))
         (define end (region-end b))
         (and beg end (> end beg)))))

(define (region-mark [b (current-buffer)])
  (define marks (buffer-marks b))
  (and (not (empty? marks))
       (first marks)))

