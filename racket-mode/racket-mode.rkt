#lang racket/base
(provide color-buffer
         indent-for-tab
         racket-run
         racket-mode)

(require racket/class racket/format racket/match racket/set
         syntax-color/racket-lexer
         "../buffer.rkt"
         "../buffer-locals.rkt"
         "../colors.rkt"
         "../commands.rkt"
         "../frame.rkt"
         "../mark.rkt"
         "../mode.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../representation.rkt"
         "../simple.rkt"
         "../window.rkt"
         "../text.rkt")

;; Each buffer in racket-mode has a corresponding repl.
;; We keep track of the repl in a hash table.

; racket-mode-buffers-ht : buffer -> repl
(define racket-mode-buffers-ht (make-hasheq))
(define (register b repl) (hash-set! racket-mode-buffers-ht b repl))
(define (get-repl b)      (hash-ref  racket-mode-buffers-ht b #f))

;; The repl associated with a racket-mode buffer
(struct repl (buffer                
              port                  ; output buffer used by the user-thread
              before-prompt-mark    ; insertion point of the output buffer
              after-prompt-mark     ; beginning of user input
              running?              ; user programming running?
              thread                ; user thread
              custodian             ; custodian controlling thread
              namespace             ; namespace for user thread
              )
  #:transparent #:mutable
  #:extra-name make-repl)

;;;
;;; RACKET MODE
;;;

(define-interactive (racket-mode [b (current-buffer)])
  (define ns (current-namespace))
  (parameterize ([current-buffer b])
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
    ; import racket-mode into buffer-locals
    (parameterize ([current-namespace (buffer-locals b)])
      (namespace-attach-module ns 'racket/gui/base)
      (namespace-attach-module ns 'data/interval-map)
      (namespace-attach-module ns "racket-mode/racket-mode.rkt")
      (namespace-require          "racket-mode/racket-mode.rkt"))
    ; create corresponding repl and register it
    (if (get-repl b)
        (void)                       ; this buffer has been in racket-mode before, reuse old repl
        (register b (create-repl b)))))

; use racket-mode automatically for all rkt files`
(register-auto-mode "rkt" racket-mode)

; create-repl : buffer -> repl
;   Given a buffer in racket-mode, create a corresponding repl buffer
(define (create-repl buffer-in-racket-mode)
  ;;; Buffer and Port
  ;; Setup the output buffer used as the output port by the user program.
  ;; The output buffer inserts data before the current prompt.
  (define buffer (new-buffer (new-text) #f (generate-new-buffer-name "*Racket Repl*")))
  (parameterize ([current-buffer buffer])
    (define b buffer)
    ;; Mode
    (racket-repl-mode b)
    ;; Banner
    (buffer-insert-string-before-point! b (banner))
    ;; Marks before and after prompt
    (define before-prompt-mark (buffer-set-mark-to-point b))
    (define  after-prompt-mark (buffer-set-mark-to-point b))
    (mark-deactivate! before-prompt-mark)
    (mark-deactivate!  after-prompt-mark)
    ;; Insertion into the buffer needs to be before the prompt
    (define port (make-output-buffer b before-prompt-mark))
    ;; Prompt
    (define prompt-string "\n> ")
    (define prompt-length (string-length prompt-string))
    (insert prompt-string) ; after point
    (mark-move! before-prompt-mark (- prompt-length))

    ;;; Namespace and custodian
    (define namespace (make-base-empty-namespace))
    (define custodian (make-custodian (current-custodian)))

    (repl buffer                
          port                  
          before-prompt-mark   
          after-prompt-mark    
          #f                   ; running?             
          #f                   ; thread               
          custodian
          namespace)))



(define (racket-repl-mode [b (current-buffer)])
  (define ns (current-namespace))  ; todo: problem getting random namespace here?
  (parameterize ([current-buffer    b]
                 [current-namespace (buffer-locals b)])
    (fundamental-mode b)       ; add all commands from fundamental mode
    ; name
    (set-major-mode! 'racket-repl)
    (set-mode-name!  "Racket Repl")
    ; keymap
    (local! local-keymap
            (λ (prefix key)
              (match prefix
                [(list)
                 (match key
                   ["return"    racket-repl-eval-or-newline-and-indent]
                   ["M-left"    backward-sexp]
                   ["M-right"   forward-sexp]
                   ["M-S-right" forward-sexp/extend-region]
                   ["M-S-left"  backward-sexp/extend-region]
                   [_           #f])]
                [_ #f]))
            b)
    (parameterize ()
      (namespace-attach-module ns 'racket/gui/base)
      (namespace-attach-module ns 'data/interval-map)
      (namespace-attach-module ns "racket-mode/racket-mode.rkt")
      (namespace-require          "racket-mode/racket-mode.rkt"))))

(define-interactive (racket-repl-eval-or-newline-and-indent)
  "If there is a complete s-expression before point, then evaluate it."
  "Otherwise use racket-newline-and-indent."
  ; TODO: Check for existing
  (define beg0 (with-saved-point (forward-whitespace) (point))) 
  (define beg1 (with-saved-point (forward-whitespace) (forward-sexp) (backward-sexp) (point)))
  (define end  (with-saved-point (forward-sexp) (point)))
  (cond
    ; complete s-expression?
    [(and (= beg0 beg1) (= beg0 end))
     (insert (~a (list 'empty beg0 beg1 end)) "\n")]
    [(= beg0 beg1)
     (insert (~a (list 'complete beg0 beg1 end)) "\n")]
    [else
     (insert (~a (list 'incomplete beg0 beg1 end)) "\n")]))

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
          'comment             orange ; brown
          'sexp-comment        base01 ; brown
          'white-space         #f
          'constant            green
          'string              green
          'no-color            #f
          'parenthesis         base00  ; light grey
          'hash-colon-keyword  blue
          'symbol              blue
          'eof                 #f
          'other               cyan))

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

(define-interactive (racket-run)
  (when (eq? (get-major-mode) 'racket)
    (define user-program-buffer (current-buffer))
    ;; Get repl (create repl if missing)
    (define repl (get-repl user-program-buffer))
    (unless repl
      (set! repl (create-repl user-program-buffer))
      (register user-program-buffer repl))
    (match-define (make-repl repl-buffer port before-prompt-mark after-prompt-mark
                             running? user-thread custodian namespace)
      repl)
    ;; Switch to repl window 
    (define visible? (buffer-visible? repl-buffer))
    (cond
      ; if the repl is visible, we switch to it
      [visible?   (current-window (get-buffer-window repl-buffer))
                  (focus-window)
                  (switch-to-buffer repl-buffer)]
      ; otherwise, we need to split the current window first
      [else       (split-window-right)
                  (other-window)
                  (switch-to-buffer repl-buffer)
                  (focus-window)])    
    ;; The buffer needs to be saved before we can run the program.
    ; 1. Make sure there is a path
    (unless (buffer-path user-program-buffer)
      (save-buffer user-program-buffer))
    (unless (buffer-path user-program-buffer)
      (error 'racket-run "save buffer to file before using racket-run"))
    ; 2. Save the buffer if needed
    (when (buffer-modified? user-program-buffer)
      (save-buffer user-program-buffer))
    (define program-path (buffer-path user-program-buffer))
    ; 3. Shut down any old threads, ports etc.
    (when (custodian? custodian)
      (custodian-shutdown-all custodian))
    ; 4. Create new custodian and namespace
    (set! custodian (make-custodian (current-custodian)))
    (set-repl-custodian! repl custodian)
    (set! namespace (make-base-empty-namespace))
    (set-repl-namespace! repl namespace)
    ; 5. Evaluate the user program
    (parameterize ([current-output-port port]
                   [current-namespace   namespace]
                   [current-custodian   custodian])
      ;; Start user process
      (when user-thread (displayln "\n---")) ; omit on first run
      (set! user-thread (thread (λ () (namespace-require program-path))))
      (set-repl-thread! repl user-thread))))
