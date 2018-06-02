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

(define (kill-ring-push-region [b (current-buffer)])
  (define s (region->string b))
  (when s
    (kill-ring-insert! s)
    s))

(define (buffer-insert-latest-kill [b (current-buffer)])
  (define s (or (and (not (ring-empty? kill-ring))
                     (ring-ref kill-ring 0))
                ""))
  (buffer-insert-string-before-point! b s))


; buffer-kill-line : buffer -> void
;   Kill text from point to end of line.
;   If point is at end of line, the newline is deleted.
;   Point is at end of line, if text from point to newline is all whitespace.
(define (buffer-kill-line [b (current-buffer)] [called-by-kill-whole-line #f])
  ; setup region, then use kill-ring-push-region and delete-region
  (define m  (buffer-point b))
  (define p1 (mark-position m))
  (define p2 (position-of-end-of-line m))
  (define rest-of-line (subtext->string (buffer-text b) p1 p2))
  (define eol? (and (string-whitespace? rest-of-line)
                    (not (= (+ (mark-position m) 1) (position-of-end b)))))
  ; delete to end of line
  (buffer-set-mark-to-point b)  
  (buffer-move-point-to-end-of-line! b)
  (when eol?
    (buffer-move-point! b +1)
    #;(forward-char b))
  
  (kill-ring-push-region)
  (region-delete b))

