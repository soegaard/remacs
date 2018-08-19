#lang racket/base
(provide color-buffer
         indent-for-tab
         markdown-mode)

(require racket/class racket/format racket/match racket/set
         syntax-color/racket-lexer
         "../buffer.rkt"
         "../chars.rkt"
         "../buffer-locals.rkt"
         "../colors.rkt"
         "../commands.rkt"
         "../frame.rkt"
         "../locals.rkt"
         "../mark.rkt"
         "../mode.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../representation.rkt"
         "../simple.rkt"
         "../window.rkt"
         "../text.rkt")

;;;
;;; MARKDOWN MODE
;;;

(define-interactive (markdown-mode [b (current-buffer)])
  (define ns (current-namespace))
  (localize ([current-buffer b])
    ; add all commands from fundamental mode
    (fundamental-mode b)
    ; name
    (set-major-mode! 'markdown)
    (set-mode-name!  "Markdown")
    ; keymap
    (local! local-keymap
            (λ (prefix key)
              (match prefix
                [(list)
                 (match key
                   ["M-left"    backward-sexp]
                   ["M-right"   forward-sexp]
                   ["M-S-right" forward-sexp/extend-region]
                   ["M-S-left"  backward-sexp/extend-region]
                   [_           #f])]
                [_ #f])))
    ; import markdown-mode into buffer-locals
    (parameterize ([current-namespace (buffer-locals b)])
      (namespace-attach-module ns 'racket/gui/base)
      (namespace-attach-module ns "markdown-mode/markdown-mode.rkt")
      (namespace-require          "markdown-mode/markdown-mode.rkt"))))

; use markdown-mode automatically for all .md files
(register-auto-mode "md" markdown-mode)




;;;
;;; INDENTATION
;;;

(define (indent-for-tab)
  (insert "    "))


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

;;;
;;; MOVEMENT
;;;

