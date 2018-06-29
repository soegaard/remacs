#lang racket/base
(provide color-buffer)

;;;
;;; RACKET MODE
;;;

(require racket/format
         "buffer.rkt"
         "buffer-locals.rkt"
         "colors.rkt"
         "parameters.rkt"
         "representation.rkt"
         "simple.rkt")

(define (indent-for-tab)
  (insert "Racket!!!"))


;;;
;;; SYNTAX COLORING
;;;

;;; Colors

(define brown orange)
(define grey  (local text-color))
(define black orange)

(define color-ht  
  (hasheq 'error               red
          'comment             brown
          'sexp-comment        brown
          'white-space         #f
          'constant            green
          'string              green
          'no-color            #f
          'parenthesis         base00  ; ligth grey
          'hash-colon-keyword  blue
          'symbol              blue
          'eof                 #f
          'other               black))


; We'll use the builtin racket color lexer
(require syntax-color/racket-lexer)

(define (color-buffer)
  (displayln (list "racket-mode: color-buffer"))
  (define b    (current-buffer))
  (define from 0)
  (define to   (buffer-length b))
  (define in   (open-input-buffer b))  
  (let loop ()
    (define-values (token style paren start end) (racket-lexer in))
    (cond
      [(eof-object? token) (void)]
      [else                (writeln (list token style (~a paren) (list start end)))
                           (define color (hash-ref color-ht style grey))
                           (overlay-set (- start 1) (- end 1) 'color color b)
                           (loop)])))




