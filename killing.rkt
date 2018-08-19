#lang racket/base
(provide (all-defined-out))

;;;
;;; KILLING
;;;

; The kill ring is a list of text blocks.
; The kill rings is shared between all buffers.
; (this allows copy+paste from one buffer to another)

(require racket/class
         racket/gui/base
         "buffer.rkt"
         "deletion.rkt"
         "frame.rkt"
         "mark.rkt"
         "parameters.rkt"
         "point.rkt"
         "region.rkt"
         "representation.rkt"
         "ring-buffer.rkt"
         "string-utils.rkt"
         "text.rkt")

(define kill-ring (new-ring))
(ring-insert! kill-ring "") ; make the kill ring non-empty

(define current-clipboard-at-latest-kill (make-parameter #f))
(define (update-current-clipboard-at-latest-kill)
  (current-clipboard-at-latest-kill 
   (send the-clipboard get-clipboard-string 0)))

(define (kill-ring-insert! s)
  (ring-insert! kill-ring s))

; kill-new : string -> void
;   Insert the string s in the kill ring as the latest kill.
(define (kill-new s)
  (kill-ring-insert! s))

; kill-append : string boolean -> void
;   Append the string s to the latest kill in the kill buffer.
;   If before? is true, prepend it otherwise postpend it.
(define (kill-append s before?)
  (define latest (ring-ref kill-ring 0))
  (define new    (if before?
                     (string-append s latest)
                     (string-append latest s)))
  (ring-set! kill-ring 0 new))

(define (kill-region-between-marks beg end [b (current-buffer)])
  (define s (region-between-marks->string beg end))
  (when s
    (kill-new s)
    (region-delete-between! beg end)))


(define (kill-region [b (current-buffer)])
  (kill-region-between-marks (get-mark) (get-point) b)
  (mark-deactivate! (get-mark))
  (update-current-clipboard-at-latest-kill)
  (refresh-frame))

(define (kill-ring-push-region)
  (define s (region->string))
  (when s
    (kill-ring-insert! s)
    s))

(define (buffer-insert-latest-kill)
  (define s (or (and (not (ring-empty? kill-ring))
                     (ring-ref kill-ring 0))
                ""))
  (buffer-insert-string! (current-buffer) (get-point) s))


; buffer-kill-line : mark -> void
;   Kill text from the given mark to end of line.
;   If the mark is at end of line, the newline is deleted.
;   Note: The mark is at the end of line, if text from mark to newline is all whitespace.
(define (buffer-kill-line [m (get-point)] [called-by-kill-whole-line #f])
  ; setup region, then use kill-ring-push-region and delete-region
  (define b   (current-buffer))
  (define beg (mark-position m))
  (define end (line-end-position m))
  (define rest-of-line (subtext->string (buffer-text b) beg end))
  (define eol? (and (string-whitespace? rest-of-line)
                    (not (= (+ (mark-position m) 1) (position-of-end b)))))
  ; delete to end of line
  (displayln (list 'buffer-kill-line 'beg beg 'enf end))
  (cond
    [eol? (kill-new rest-of-line)
          (region-delete-between! beg end)]
    [else (kill-new rest-of-line)
          (region-delete-between! beg (+ end 1))]))