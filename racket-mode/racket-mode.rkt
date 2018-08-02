#lang racket/base
(provide color-buffer     ; from "syntax-coloring.rkt"
         indent-for-tab   ; from "indentation.rkt"
         ; ---
         racket-mode
         ; ---
         get-repl     
         racket-run)

;;;
;;; RACKET MODE
;;;

; Racket mode is for editing and running programs written in the Racket programming language.

; The mode provides:
;   - syntax coloring
;   - indentation
;   - a repl

; The implementation of racket-mode is heavily influenced by the implementation
; of racket-mode for Emacs by Greg Hendershott.

; The syntax coloring use the color-lexer from syntax-color/racket-lexer

(require racket/match 
         "../buffer.rkt"
         "../buffer-locals.rkt"
         "../commands.rkt"
         "../locals.rkt"
         "../mark.rkt"
         "../mode.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../representation.rkt"
         "../simple.rkt"
         "../window.rkt"
         "../text.rkt"
         "indentation.rkt"
         "syntax-coloring.rkt")

;;; Representation

;; Each buffer in racket-mode has a corresponding repl.
;; The repl associated with a racket-mode buffer:

(struct repl (buffer                ; contains the repl input and output text
              definitions-buffer    ; contains the program
              out-port              ; output buffer used by the user-thread
              in-port               ; input port
              before-prompt-mark    ; insertion point of the output buffer
              after-prompt-mark     ; beginning of user input
              running?              ; user programming running?
              thread                ; user thread
              custodian             ; custodian controlling thread
              namespace             ; namespace for user thread
              channel               ; used to send expressions to the evaluator
              )
  #:transparent #:mutable
  #:extra-name make-repl)


(define-interactive (racket-mode [b (current-buffer)])
  (define ns (current-namespace))
  (localize ([current-buffer b])
    ; add all commands from fundamental mode
    (fundamental-mode b)
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
                   ["D-d"       hide-definitions]
                   ["return"    break-line-and-indent]
                   ;["D-e"       hide-interaction-window]
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


;;;
;;; PROMPT
;;;

(define prompt-string "> ")
(define prompt-length (string-length prompt-string))

;;;
;;; REPL
;;;

;; Each buffer in racket-mode has has a corresponding repl,
;; We keep track of the association in a hash table.

; racket-mode-buffers-ht : buffer -> repl
(define racket-mode-buffers-ht      (make-hasheq))
(define racket-repl-mode-buffers-ht (make-hasheq))
(define (register b repl)
  (hash-set! racket-mode-buffers-ht b repl)
  (hash-set! racket-repl-mode-buffers-ht (repl-buffer repl) repl))
(define (get-repl b)
  (or (hash-ref racket-mode-buffers-ht      b #f)
      (hash-ref racket-repl-mode-buffers-ht b #f)))

; create-repl : buffer -> repl
;   Given a buffer in racket-mode, create a corresponding repl buffer
(define (create-repl buffer-in-racket-mode)
  (define defn-buffer buffer-in-racket-mode)
  ;;; Buffer and Port
  ;; Setup the output buffer used as the output port by the user program.
  ;; The output buffer inserts data before the current prompt.
  (define buffer (new-buffer (new-text) #f (generate-new-buffer-name "*Racket Repl*")))
  (localize ([current-buffer buffer])
    (define b buffer)
    ;; Mode
    (racket-repl-mode b)
    ;; Banner
    (buffer-insert-string! b (get-point) (banner))
    ;; Marks before and after prompt
    (define before-prompt-mark (buffer-set-mark-to-point b))
    (define  after-prompt-mark (buffer-set-mark-to-point b))
    (mark-deactivate! before-prompt-mark)
    (mark-deactivate!  after-prompt-mark)
    ;; Insertion into the buffer needs to be before the prompt
    (define out-port (make-output-buffer b before-prompt-mark))
    (define in-port  (open-input-string "42"))
    ;; Prompt
    (insert prompt-string) ; after point
    (mark-move! before-prompt-mark (- prompt-length))

    ;;; Namespace and custodian
    (define namespace (make-base-empty-namespace))
    (define custodian (make-custodian (current-custodian)))
    ;;; Channel
    (define channel   (make-channel))

    (repl buffer
          defn-buffer
          out-port
          in-port
          before-prompt-mark   
          after-prompt-mark    
          #f                   ; running?             
          #f                   ; thread               
          custodian
          namespace
          channel)))

(define-interactive (show-definitions)
  ; (displayln show-definitions (current-error-port))
  ; invoked from the buffer with racket-repl-mode
  (define b (current-buffer))
  (define repl-buffer? (hash-ref racket-repl-mode-buffers-ht b #f))
  (when (and repl-buffer? (buffer-visible? b))
    (define repl (get-repl b))
    (define db   (repl-definitions-buffer repl))
    (cond
      [(buffer-visible? db) (define w (get-buffer-window db))
                            (unless w (error 'here))
                            (when w
                              (current-window w)
                              (focus-window w)
                              (delete-other-windows))]
      [(buffer-visible? b)  (switch-to-buffer db)]
      [else                 (error 'huh)])))

  
(define-interactive (hide-definitions)
  ; invoked from the buffer with racket-mode  
  (define b (current-buffer))
  (define definition-buffer? (hash-ref racket-mode-buffers-ht b #f))
  (when (and definition-buffer? (buffer-visible? b))    
    (define repl (get-repl b))
    (define rb   (repl-buffer repl))
    (cond
      [(buffer-visible? rb) (define w (get-buffer-window rb))
                            (current-window w)
                            (focus-window w)
                            (delete-other-windows)]
      [(buffer-visible? b)  (switch-to-buffer rb)]
      [else                 (error 'huh)])))


(define (racket-repl-mode [b (current-buffer)])
  (define ns (current-namespace))  ; todo: problem getting random namespace here?
  (parameterize ([current-namespace (buffer-locals b)])
    (localize ([current-buffer b])
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
                     ["D-e"       show-definitions]
                     [_           #f])]
                  [_ #f]))
              b)
      (parameterize ()
        (namespace-attach-module ns 'racket/gui/base)
        (namespace-attach-module ns 'data/interval-map)
        (namespace-attach-module ns "racket-mode/racket-mode.rkt")
        (namespace-require          "racket-mode/racket-mode.rkt")))))

(define-interactive (racket-repl-eval-or-newline-and-indent)
  "If there is a complete s-expression before point, then evaluate it."
  "Otherwise use racket-newline-and-indent."  
  (define b (current-buffer))
  (localize ([current-buffer b])
    (define r (get-repl b))
    (define m (repl-before-prompt-mark r))
    (define (skip-prompt)       (forward-char prompt-length))
    (define (goto-after-prompt) (goto-char m) (skip-prompt))
    (display 'A (current-error-port))
    (define beg0 (with-saved-point (goto-after-prompt)
                                   (forward-whitespace)
                                   (point)))
    (display 'B (current-error-port))
    (define beg1 (with-saved-point
                   (display 'B1 (current-error-port))
                   (goto-after-prompt)
                   (display 'B2 (current-error-port))
                   (forward-whitespace)
                   (display 'B3 (current-error-port))
                   (forward-sexp)
                   (display 'B4 (current-error-port))
                   (backward-sexp)
                   (display 'B5 (current-error-port))
                   (point)))
    (display 'C (current-error-port))
    (define end  (with-saved-point (goto-after-prompt)
                                   (forward-sexp) (point)))
    (display 'D (current-error-port))
    ; (log-debug (~a (list 'm (position m) 'beg0 beg0 'beg1 beg1 'end end)))
    (cond
      ; complete s-expression?
      [(and (= beg0 beg1) (= beg0 end)) (break-line)]
      [(= beg0 beg1)                    (racket-repl-eval r beg0 end)]
      [else                             (break-line)])))

(define (racket-repl-eval repl beg end)
  (define b  (current-buffer))
  (localize ([current-buffer b])
    (define ch (repl-channel repl))
    (define s  (subtext->string (buffer-text b) beg end))
    (channel-put ch (list beg end s))))

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
    (match-define (make-repl repl-buffer defn-buffer out-port in-port before-prompt-mark
                             after-prompt-mark running? user-thread custodian namespace channel)
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
    (set! namespace (make-base-namespace))
    (set-repl-namespace! repl namespace)
    ; 5. Evaluate the user program
    (parameterize ([current-output-port out-port]
                   ; [current-error-port  out-port]
                   [current-namespace   namespace]
                   [current-custodian   custodian])
      ;; Start user process
      (when user-thread (displayln "\n---")) ; omit on first run
      (set! user-thread
            (thread
             (λ ()
               (define (handle-exn e)
                 (define msg (exn-message e))
                 (define ctx (continuation-mark-set->context (exn-continuation-marks e)))
                 (displayln msg)
                 (displayln ctx))
               (namespace-require program-path)
               (parameterize ([current-namespace (module->namespace program-path)])
                 (let loop ()
                   (display "1" (current-error-port))
                   (match-define (list beg end str) (channel-get channel))
                   (display "2" (current-error-port))
                   (goto-char end)
                   (display "3" (current-error-port))
                   (break-line)
                   (display "4" (current-error-port))
                   (insert prompt-string) ; after point
                   (display "5" (current-error-port))
                   (goto-char (+ end 1) before-prompt-mark)
                   (display "6" (current-error-port))
                   (define stx (read-syntax 'repl (open-input-string str)))
                   (display "7" (current-error-port))
                   (call-with-values
                    (λ () (with-handlers ([exn:fail? handle-exn])
                            (eval (with-syntax ([stx stx])
                                    (syntax/loc #'stx (#%top-interaction . stx))))))
                    (λ vs (for ([v vs])
                            (unless (void? v)
                              (print v))
                            (newline))))
                   (display "8" (current-error-port))
                   (loop))))))
      (set-repl-thread! repl user-thread))))
