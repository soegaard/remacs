#lang racket/base
(provide color-buffer
         indent-for-tab
         racket-run)

;;;
;;; RACKET MODE
;;;

(require racket/format racket/class
         syntax-color/racket-lexer
         "../buffer.rkt"
         "../buffer-locals.rkt"
         "../colors.rkt"
         "../commands.rkt"
         "../frame.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../representation.rkt"
         "../simple.rkt"
         "../window.rkt"
         "../text.rkt")

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

; (local! color-buffer color-buffer)

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

(define user-running?  #f)  ; is the program in the buffer running
(define user-thread    #f)
(define user-buffer    #f)
(define user-custodian #f)

(define-interactive (racket-run)
  ; New output buffer
  ;   - only create new buffer, on first run
  ; Switch to buffer if it is visible,
  ; If not visible then ...

  ;; Setup buffer and window
  (define first-run? (not user-buffer))
  (define visible?   (and user-buffer (buffer-visible? user-buffer)))
  (cond
    [first-run? (set! first-run? #f)
                (split-window-right)
                (other-window)
                (create-new-buffer "*output*")       ; creates new buffer and switches to it
                (set! user-buffer (current-buffer))]
    [visible?   (switch-to-buffer user-buffer)]
    [else       (split-window-right)
                (other-window)
                (switch-to-buffer user-buffer)])
  ;; Remove previous threads, ports etc.
  (unless first-run?
    (when (custodian? user-custodian)
      (custodian-shutdown-all user-custodian)))
  ;; Setup environment in which to run program
  (define p   (make-output-buffer user-buffer))
  (define ns  (make-base-empty-namespace))
  (define c   (make-custodian (current-custodian)))
  (parameterize ([current-output-port p]
                 [current-namespace   ns]
                 [current-custodian   c])
    (set! user-running? #t)
    (set! user-custodian c)
    (set! user-thread
          (thread
           (λ ()
             (displayln (list 'racket-run "thread"))
             (namespace-require "fact.rkt"))))))
