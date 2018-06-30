#lang racket/base
(provide color-buffer
         indent-for-tab)

;;;
;;; RACKET MODE
;;;

(require racket/format
         syntax-color/racket-lexer
         "../buffer.rkt"
         "../buffer-locals.rkt"
         "../colors.rkt"
         "../commands.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../representation.rkt"
         "../simple.rkt"
         "../text.rkt")

;;;
;;; INDENTATION
;;;

(define (indent-for-tab)
  (insert "Racket!!!"))


;;;
;;; SYNTAX COLORING
;;;

;;; Colors

; See also "colors.rkt"
(define brown orange)
(define grey  (local text-color))
(define black orange)

; The lexer returns token and the type of token.
; These colors are the standard colors used in DrRacket.

(define color-ht  
  (hasheq 'error               red
          'comment             brown
          'sexp-comment        brown
          'white-space         #f
          'constant            green
          'string              green
          'no-color            #f
          'parenthesis         base00  ; light grey
          'hash-colon-keyword  blue
          'symbol              blue
          'eof                 #f
          'other               black))

(define (color-buffer [b (current-buffer)] [from 0] [to #f])
  (displayln (list 'color-buffer 'from from 'to to))
  ; (displayln (list "racket-mode: color-buffer"))
  ;; set optional arguments
  (unless to (set! to (buffer-length b)))
  ;; turn buffer into input port
  (define in   (open-input-buffer b))
  ;; backtrack to a known place outside strings, comments etc.
  (define safe-pos
    (with-saved-point
        (goto-char from)
        (backward-to-open-parenthesis-on-beginning-of-line)
      (position (point))))
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
                             (loop))])))

; (local! color-buffer color-buffer)

;;;
;;; MOVEMENT
;;;

;;;
;;; RUN
;;;

(define-interactive (racket-run)
  (define b  (new-buffer (new-text) #f (generate-new-buffer-name "*output*")))
  (define p  (make-output-buffer b))
  (define ns (make-empty-namespace))
  (set-window-buffer! (current-window) b)
  (parameterize ([current-buffer      b]
                 [current-output-port p]
                 [current-namespace   ns])
    (thread
     (λ ()
       (let loop ([n 0])
         (displayln n)
         (sleep 1.)
         (loop (+ n 1)))))))
