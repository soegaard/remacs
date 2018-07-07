#lang racket/base
(provide color-buffer
         indent-for-tab
         racket-run
         racket-mode)

(require racket/class racket/format racket/match
         syntax-color/racket-lexer
         "../buffer.rkt"
         "../buffer-locals.rkt"
         "../colors.rkt"
         "../commands.rkt"
         "../frame.rkt"
         "../mode.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../representation.rkt"
         "../simple.rkt"
         "../window.rkt"
         "../text.rkt")

;;;
;;; RACKET MODE
;;;

(define-interactive (racket-mode [b (current-buffer)])
  (fundamental-mode b)       ; add all commands from fundamental mode
  ; name
  (set-major-mode! 'racket)
  (set-mode-name!  "Racket")
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
              [_ #f]))
          b)
  (define ns (current-namespace))
  (parameterize ([current-namespace (buffer-locals b)]
                 [current-buffer    b])
    (namespace-attach-module ns 'racket/gui/base)
    (namespace-attach-module ns 'data/interval-map)
    (namespace-require "racket-mode/racket-mode.rkt")))

(register-auto-mode "rkt" racket-mode)

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

(define color-ht  
  (hasheq 'error               red
          'comment             base01 ; brown
          'sexp-comment        base01 ; brown
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
  ; (displayln (list "racket-mode.rkt" 'color-buffer 'from from 'to to))
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

;;;
;;; MOVEMENT
;;;

;;;
;;; RUN
;;;

; Running the contents of a buffer in racket-mode
; Take 1: No repl - just run program and see output in buffer.
;   1) Setup an output buffer
;   2) Setup an environment in which the program can run without any risk of affecting the editor.
;      Things to consider:
;          - namespace
;          - custodian
;          - memory limit
;          - current-directory
;          - printer
;          - logging
;          - event space (for gui)
;          - custodian (when a custodian is shut down, ports, tcp connections etc
;                       will be terminated)
; Note: A way to break and kill a user program thread is needed.

(define user-running?        #f)  ; is the program in the buffer running
(define user-thread          #f)
(define user-program-buffer  #f)
(define user-repl-buffer     #f)
(define user-custodian       #f)

(define first-run?           #t)

(define-interactive (racket-run)
  ; New output buffer
  ;   - only create new buffer, on first run
  ; Switch to buffer if it is visible,
  ; If not visible then ...

  ;; Make sure buffer is saved to a file
  (set! user-program-buffer (current-buffer))
  (unless (buffer-path user-program-buffer)
    (save-buffer))
  (define program-path (buffer-path user-program-buffer))
  (when program-path
    ;; Setup buffer and window
    (set! first-run? (not user-repl-buffer))
    (define was-first-run? first-run?)
    (define visible?   (and user-repl-buffer (buffer-visible? user-repl-buffer)))
    (cond
      [first-run? (set! first-run? #f)
                  (split-window-right)
                  (other-window)
                  (create-new-buffer "*output*")       ; creates new buffer and switches to it
                  (set! user-repl-buffer (current-buffer))]
      [visible?   (current-window (get-buffer-window user-repl-buffer))
                  (focus-window)
                  (switch-to-buffer user-repl-buffer)]
      [else       (split-window-right)
                  (other-window)
                  (focus-window)
                  (switch-to-buffer user-repl-buffer)])
    ; Note: The current-buffer is now the repl.
    (end-of-buffer)
    ;; Remove previous threads, ports etc.
    (unless first-run?
      (when (custodian? user-custodian)
        (custodian-shutdown-all user-custodian)))
    (when (buffer-path user-program-buffer)
      ;; Setup environment in which to run program
      (define p   (make-output-buffer user-repl-buffer))
      (define ns  (make-base-empty-namespace))
      (define c   (make-custodian (current-custodian)))
      (parameterize ([current-output-port p]
                     [current-namespace   ns]
                     [current-custodian   c])
        ;; Banner
        (unless was-first-run? (displayln "---"))
        (display (banner))
        ;; Start user process
        (set! user-running?  #t)
        (set! user-custodian c)
        (set! user-thread    (thread (λ () (namespace-require program-path))))))))
  