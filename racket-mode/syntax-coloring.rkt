#lang racket/base
(provide color-buffer)
;;;
;;; SYNTAX COLORING
;;;

(require syntax-color/racket-lexer
         "../buffer.rkt"
         "../buffer-locals.rkt"
         "../colors.rkt"
         "../locals.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../representation.rkt"
         "../simple.rkt")

;;; Colors

; See also "colors.rkt"
(define brown orange)
(define grey  (local text-color))
(define black orange)

; The lexer returns token and the type of token.
; These colors are the standard colors used in DrRacket.

; HASHEQ  color-ht : symbol -> color-or-false
(define color-ht  
  (hasheq 'error               red
          'comment             orange  ; brown
          'sexp-comment        base01  ; brown
          'white-space         #f
          'constant            green
          'string              green
          'no-color            #f
          'parenthesis         base00  ; light grey
          'hash-colon-keyword  blue
          'symbol              blue
          'eof                 #f
          'other               cyan))

; color-buffer : buffer integer integer -> void
(define (color-buffer [b (current-buffer)] [from 0] [to #f])
  ; (log-warning (~a (list 'color-buffer (buffer-name b) from to)))
  ; (displayln (list "racket-mode.rkt" 'color-buffer 'from from 'to to))
  ; (displayln (list "racket-mode: color-buffer"))
  ;; set optional arguments
  (localize ([current-buffer b])
    (unless to (set! to (buffer-length b)))
    ;; turn buffer into input port
    (define in (open-input-buffer b))
    ;; backtrack to a known place outside strings, comments etc.
    (define safe-pos
      (with-saved-point
        (goto-char from)
        (backward-to-open-parenthesis-on-beginning-of-line)
        (point)))
    (file-position in safe-pos)
    ;; call the lexer in a loop and use overlays to record the colors
    (let loop ()
      (define-values (token style paren start end) (racket-lexer in))
      (cond
        [(eof-object? token) (void)]
        [else                ; (writeln (list token style (~a paren) (list start end)))
         (define color (hash-ref color-ht style grey))
         (overlay-set (- start 1) (- end 1) 'color color b)
         (when (< end to)
           (loop))]))))