#lang racket/base
(provide color-buffer
         get-repl
         indent-for-tab
         racket-run
         racket-mode)

(require (for-syntax racket/base syntax/parse)
         racket/class racket/format racket/list racket/match racket/set racket/string
         syntax-color/racket-lexer
         "../buffer.rkt"
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
         "../search.rkt"
         "../simple.rkt"
         "../window.rkt"
         "../text.rkt")

;; Each buffer in racket-mode has a corresponding repl.

;; The repl associated with a racket-mode buffer
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


;; We keep track of the repl in a hash table.

; racket-mode-buffers-ht : buffer -> repl
(define racket-mode-buffers-ht      (make-hasheq))
(define racket-repl-mode-buffers-ht (make-hasheq))
(define (register b repl)
  (hash-set! racket-mode-buffers-ht b repl)
  (hash-set! racket-repl-mode-buffers-ht (repl-buffer repl) repl))
(define (get-repl b)
  (or (hash-ref racket-mode-buffers-ht      b #f)
      (hash-ref racket-repl-mode-buffers-ht b #f)))


;;;
;;; RACKET MODE
;;;

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

(define (break-line-and-indent)
  (break-line)
  (indent-for-tab))

;;;
;;; PROMPT
;;;

(define prompt-string "> ")
(define prompt-length (string-length prompt-string))

;;;
;;;
;;;


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
  ; TODO: Check for existing
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
;;; INDENTATION
;;;

;;; The variables are dir local in racket-mode.
(define racket-indent-sequence-depth    3)
(define racket-indent-curly-as-sequence #t)

(define (indent-for-tab)
  (indent-line))

(define-interactive (indent-line)
  (define amount (calculate-indent))
  (displayln (list 'indent-line 'amount amount))
  (when amount
    ;; When point is within the leading whitespace, move it past the
    ;; new indentation whitespace. Otherwise preserve its position
    ;; relative to the original text.
    (let ([pos (- (point-max) (point))]
          [beg (begin (beginning-of-line) (point))])
      (skip-chars-forward " \t")
      (unless (= amount (current-column))
        (delete-region beg (point))
        (indent-to amount))
      (when (< (point) (- (point-max) pos))
        (goto-char (- (point-max) pos))))))

(define-syntax (while stx)
  (syntax-parse stx
    [(_while expr body ...)
     (syntax/loc stx
       (let loop ()
         (when expr
           body ...
           (loop))))]))

(define (plain-beginning-of-define)
  (backward-to-open-parenthesis-on-beginning-of-line))

(define (calculate-indent)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is #f, that means don't change the indentation
because the line starts inside a string.
This is `calculate-lisp-indent' distilled to what we actually
need."
  (with-saved-point
    (beginning-of-line)
    (let ([indent-point (point)]
          [state        empty-state])
      ; move back to position with an empty parse state
      (plain-beginning-of-define)
      ; parse forward to the indent-point
      (define state
        (let loop ([s empty-state])
          (if (< (point) indent-point)
              (loop (parse-partial-sexp (point) indent-point
                                        #:state s #:target-depth 0))
              s)))
      (display-state state)
      (let ([str?  (state-inside-string  state)]   ; inside string?
            [cmt?  (state-inside-comment state)]   ; inside comment?
            [last  (state-last-complete  state)]   ; start of last complete s-expr 
            [cont  (state-inner-start    state)]   ; start of inners most parent s-expr (containing)
            [depth (state-depth          state)])  ; paren depth
        (display (list 'str? str? 'cmt? cmt? 'last last 'cont cont 'depth depth)
                 (current-error-port))
        (cond
          ; no indentation if point is in a string or comment
          [(or str? cmt?)                    (displayln "X")
                                             #f]
          [(and (not (= depth 0)) last cont) (displayln "Y")
                                             (indent-function indent-point state)]
          [cont                              (displayln "Z")
                                             (goto-char (+ cont 1)) ; why?
                                             (current-column)]
          [else                              (displayln "W")
                                             (current-column)])))))

(define lisp-body-indent 2)

(define (indent-function indent-point state)
  "Called by `calculate-indent' to get indent column.
INDENT-POINT is the position at which the line being indented begins.
STATE is the `parse-partial-sexp' state for that position.
There is special handling for:
  - forms that begin with a #:keyword (as found in contracts)
  - forms like #hasheq()
  - data sequences when `racket-indent-sequence-depth' is > 0
  - {} forms when `racket-indent-curly-as-sequence' is not nil
See `racket-indent-line' for more information about users setting
the `racket-indent-function` property."
  ; find start of inner-most s-exp
  (goto-char (state-inner-start state))
  ; if we are dealing with a define-like expression,
  ; we will later need to know the indendentation of the body  
  (define body-indent (+ (current-column) lisp-body-indent))
  ; skip the parenthesis of inner most s-exp
  (forward-char 1)
  ; 
  (cond
    [(or (hash-literal-or-keyword?) (data-sequence?))
     (backward-prefix-chars) (current-column)]
    [else
     ; get the head of the inner-most sexp
     (define head (buffer-substring (current-buffer) (point) (begin (forward-sexp 1) (point))))
     ; find out how to indent that type of s-expression
     (define method (get-indent-function-method head))
     
          
     (displayln (list 'head head 'method method 'body-indent body-indent))
     (cond
       [(integer? method)                  (racket-indent-special-form method indent-point state)]
       [(eq? method 'define)               body-indent]
       [method                             (method indent-point state)]
       [(regexp-match "^(def|with-)" head) body-indent] ;just like 'define
       [(regexp-match "^begin" head)       (racket-indent-special-form 0 indent-point state)]
       [else                               (racket-normal-indent indent-point state)])]))

(define racket-indent-function-ht (make-hash))

(define (get-indent-function-method head)
  (define sym (string->symbol head))
  (hash-ref racket-indent-function-ht sym #f))



(define-syntax (inc! stx) (syntax-parse stx [(_inc x) (syntax/loc stx (begin (set! x (+ x 1)) x))]))
(define-syntax (dec! stx) (syntax-parse stx [(_dec x) (syntax/loc stx (begin (set! x (- x 1)) x))]))

(define-syntax (ignore-errors stx)
  (syntax-parse stx
    [(_ignore-errors body ...)
     (syntax/loc stx
       (with-handlers ([exn? (λ (_) #f)])
         body ...))]))

(define (string-match regexp string [start #f])
  ; Return index of start of first match.
  (define ps (regexp-match-positions regexp  string start))
  (and ps
       (car (first ps))))

(define (racket-normal-indent indent-point state)
  ;; Credit: Substantially borrowed from clojure-mode
  (goto-char (state-last-complete state))
  (backward-prefix-chars)
  (let ([last-sexp #f])
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match #rx"[^ \t]"
                               (buffer-substring (line-beginning-position)
                                                 (point)))
            (set! last-sexp (begin0 (point)
                                    (forward-sexp -1))))
          #t)
        ;; Here we've found an arg before the arg we're indenting
        ;; which is at the start of a line.
        (current-column)
        ;; Here we've reached the start of the enclosing sexp (point is
        ;; now at the function name), so the behavior depends on whether
        ;; there's also an argument on this line.
        (begin
          (when (and last-sexp
                     (< last-sexp (line-end-position)))
            ;; There's an arg after the function name, so align with it.
            (goto-char last-sexp))
          (current-column)))))

(define (racket-indent-special-form method indent-point state)
  "METHOD must be a nonnegative integer -- the number of
  \"special\" args that get extra indent when not on the first
  line. Any additinonal args get normal indent."
  ;; Credit: Substantially borrowed from clojure-mode
  (let ([containing-column (with-saved-point
                             (goto-char (state-inner-start state))
                             (current-column))]
        [pos -1])
    (with-handlers
        ;; If indent-point is _after_ the last sexp in the current sexp,
        ;; we detect that by catching the `scan-error'. In that case, we
        ;; should return the indentation as if there were an extra sexp
        ;; at point.
        ([exn? (λ (e) (inc! pos))]) ; Note: this assumes that forward-sexp throws an error
      (while (and (<= (point) indent-point)
                  (not (eob?)))
             (forward-sexp 1)
             (inc! pos)))
      
    (cond [(= method pos)               ;first non-distinguished arg
           (+ containing-column lisp-body-indent)]
          [(< method pos)               ;more non-distinguished args
           (racket-normal-indent indent-point state)]
          [else                         ;distinguished args
           (+ containing-column (* 2 lisp-body-indent))])))

(define (racket-conditional-indent indent-point state looking-at-regexp true false)
  (skip-chars-forward " \t")
  (let ([n (if (looking-at looking-at-regexp) true false)])
    (racket-indent-special-form n indent-point state)))

(define racket-identifier-regexp
  #rx"^[a-zA-Z0-9]+") ; todo: use the racket-lexer
;  (rx (or (syntax symbol) (syntax word) (syntax punctuation))))
;  "A regexp matching valid Racket identifiers."

(define (racket-indent-maybe-named-let indent-point state)
  "Indent a let form, handling named let (let <id> <bindings> <expr> ...)"
  (racket-conditional-indent indent-point state
                             racket-identifier-regexp
                             2 1))

(define (racket-indent-for indent-point state)
  "Indent function for all for/ and for*/ forms EXCEPT
for/fold and for*/fold.
Checks for either of:
  - maybe-type-ann e.g. (for/list : T ([x xs]) x)
  - for/vector optional length, (for/vector #:length ([x xs]) x)"
  (racket-conditional-indent indent-point state
                             #rx"[:#]"
                             3 1))

(define (racket-indent-for/fold indent-point state)
  "Indent function for for/fold and for*/fold."
  ;; check for maybe-type-ann e.g. (for/fold : T ([n 0]) ([x xs]) x)
  (skip-chars-forward " \t\n")
  (if (looking-at ":")
      (racket-indent-special-form 4 indent-point state)
      (racket-indent-for/fold-untyped indent-point state)))

(define (racket-indent-for/fold-untyped indent-point state)
  (let* ((containing-sexp-start  (state-inner-start state))
         (_                      (goto-char containing-sexp-start))
         (containing-sexp-column (current-column))
         (containing-sexp-line   (line-number-at-pos))
         (body-indent            (+ containing-sexp-column lisp-body-indent))
         (clause-indent          #f))
    ;; Move to the open paren of the first, accumulator sexp
    (forward-char 1)    ;past the open paren
    (forward-sexp 2)    ;to the next sexp, past its close paren
    (backward-sexp 1)   ;back to its open paren
    ;; If the first, accumulator sexp is not on the same line as
    ;; `for/fold`, then this is simply specform 2.
    (if (not (= (line-number-at-pos) containing-sexp-line)) ;expensive?
        (racket-indent-special-form 2 indent-point state)
        (begin
          (set! clause-indent (current-column))
          (forward-sexp 1)    ;past close paren
          ;; Now go back to the beginning of the line holding
          ;; the indentation point. Count the sexps on the way.
          (parse-partial-sexp (point) indent-point 1 #t)
          (let ((n 1))
            (while (and (< (point) indent-point)
                        (ignore-errors
                         (inc! n)
                         (forward-sexp 1)
                         (parse-partial-sexp (point) indent-point 1 #t))))
            (if (= 1 n) clause-indent body-indent))))))


(define (hash-literal-or-keyword?)
  "Looking at things like #fl() #hash() or #:keyword ?
The last occurs in Racket contract forms, e.g. (->* () (#:kw kw)).
Returns nil for #% identifiers like #%app."
  (looking-at #rx"#(:|[^%])"))

(define (data-sequence?)
  "Looking at \"data\" sequences where we align under head item?
These sequences include '() `() #() -- and {} when
`racket-indent-curly-as-sequence' is t -- but never #'() #`() ,()
,@().
To handle nested items, we search `backward-up-list' up to
`racket-indent-sequence-depth' times."
  #f
  #;(and (< 0 racket-indent-sequence-depth)
       (with-saved-point
         (define answer 'unknown)
         (define depth  racket-indent-sequence-depth)
         (while (and (eq? answer 'unknown) (< 0 depth))
           (displayln 1)
           (backward-up-list)
           (displayln 2)
           (dec! depth)
           (displayln 3)
           (cond
             [(or
               ;; a quoted '( ) or quasiquoted `( ) list --
               ;; but NOT syntax #'( ) or quasisyntax #`( )
               (and (displayln '3a)
                    (memq (char-before (point)) '(#\' #\`))
                    (eqv? (char-after  (point)) #\()
                    (not (eqv? (char-before (- (point) 1)) #\#)))
               ;; a vector literal: #( )
               (and (displayln '3b)
                    (eqv? (char-before (point)) #\#)
                    (eqv? (char-after  (point)) #\())
               ;; { }
               (and (displayln '3c)
                    racket-indent-curly-as-sequence
                    (let ([t (eqv? (char-after (point)) #\{)])
                      (displayln '3d)
                      t)))
               (displayln 4)
               (set! answer #t)]
             [;; unquote or unquote-splicing
              (and (or (eqv? (char-before (point)) #\,)
                       (and (eqv? (char-before (- (point) 1)) #\,)
                            (eqv? (char-before    (point))    #\@)))
                   (eqv? (char-after (point)) #\())
              (displayln 5)
              (set! answer #f)]))
         answer)))

(define (racket:set-indentation)
  "Set indentation for various Racket forms.
Note that `beg*`, `def*` and `with-*` aren't listed here because
`racket-indent-function' handles those.
Note that indentation is set for the symbol alone, and also with
a : suffix for legacy Typed Racket. For example both `let` and
`let:`. Although this is overzealous in the sense that Typed
Racket does not define its own variant of all of these, it
doesn't hurt to do so."
  (define settings
    '(;; begin* forms default to 0 unless otherwise specified here
      (begin0 1)
      (c-declare 0)
      (c-lambda 2)
      (call-with-input-file   define)
      (call-with-input-file*  define)
      (call-with-output-file  define)
      (call-with-output-file* define)
      (case 1)
      (case-lambda 0)
      (catch 1)
      (class define)
      (class* define)
      (compound-unit/sig 0)
      (cond 0)
      ;; def* forms default to 'define unless otherwise specified here
      (define define)
      (delay 0)
      (do 2)
      (dynamic-wind 0)
      (fn 1) ;alias for lambda (although not officially in Racket)
      (for 1)
      (for/list racket-indent-for)
      (for/vector racket-indent-for)
      (for/hash racket-indent-for)
      (for/hasheq racket-indent-for)
      (for/hasheqv racket-indent-for)
      (for/and racket-indent-for)
      (for/or racket-indent-for)
      (for/lists racket-indent-for/fold)
      (for/first racket-indent-for)
      (for/last racket-indent-for)
      (for/fold racket-indent-for/fold)
      (for/flvector racket-indent-for)
      (for/set racket-indent-for)
      (for/seteq racket-indent-for)
      (for/seteqv racket-indent-for)
      (for/sum racket-indent-for)
      (for/product racket-indent-for)
      (for* 1)
      (for*/list racket-indent-for)
      (for*/vector racket-indent-for)
      (for*/hash racket-indent-for)
      (for*/hasheq racket-indent-for)
      (for*/hasheqv racket-indent-for)
      (for*/and racket-indent-for)
      (for*/or racket-indent-for)
      (for*/lists racket-indent-for/fold)
      (for*/first racket-indent-for)
      (for*/last racket-indent-for)
      (for*/fold racket-indent-for/fold)
      (for*/flvector racket-indent-for)
      (for*/set racket-indent-for)
      (for*/seteq racket-indent-for)
      (for*/seteqv racket-indent-for)
      (for*/sum racket-indent-for)
      (for*/product racket-indent-for)
      (instantiate 2)
      (interface 1)
      (λ 1)
      (lambda 1)
      (lambda/kw 1)
      (let racket-indent-maybe-named-let)
      (let* 1)
      (letrec 1)
      (letrec-values 1)
      (let-values 1)
      (let*-values 1)
      (let+ 1)
      (let-syntax 1)
      (let-syntaxes 1)
      (letrec-syntax 1)
      (letrec-syntaxes 1)
      (letrec-syntaxes+values racket-indent-for/fold-untyped)
      (local 1)
      (let/cc 1)
      (let/ec 1)
      (match 1)
      (match* 1)
      (match-define define)
      (match-lambda 0)
      (match-lambda* 0)
      (match-let 1)
      (match-let* 1)
      (match-let*-values 1)
      (match-let-values 1)
      (match-letrec 1)
      (match-letrec-values 1)
      (match/values 1)
      (mixin 2)
      (module 2)
      (module+ 1)
      (module* 2)
      (opt-lambda 1)
      (parameterize 1)
      (parameterize-break 1)
      (parameterize* 1)
      (quasisyntax/loc 1)
      (receive 2)
      (require/typed 1)
      (require/typed/provide 1)
      (send* 1)
      (shared 1)
      (sigaction 1)
      (splicing-let 1)
      (splicing-letrec 1)
      (splicing-let-values 1)
      (splicing-letrec-values 1)
      (splicing-let-syntax 1)
      (splicing-letrec-syntax 1)
      (splicing-let-syntaxes 1)
      (splicing-letrec-syntaxes 1)
      (splicing-letrec-syntaxes+values racket-indent-for/fold-untyped)
      (splicing-local 1)
      (splicing-syntax-parameterize 1)
      (struct define)
      (syntax-case 2)
      (syntax-case* 3)
      (syntax-rules 1)
      (syntax-id-rules 1)
      (syntax-parse 1)
      (syntax-parser 0)
      (syntax-parameterize 1)
      (syntax/loc 1)
      (syntax-parse 1)
      (test-begin 0)
      (test-case 1)
      (unit define)
      (unit/sig 2)
      (unless 1)
      (when 1)
      (while 1)
      ;; with- forms default to 1 unless otherwise specified here
      ))
  (for ([setting settings])
    (match setting
      [(list key val-spec)
       (define val
         (match val-spec
           [(? integer? i)                  i]
           ['define                         'define]
           ['racket-indent-for              racket-indent-for]
           ['racket-indent-for/fold         racket-indent-for/fold]
           ['racket-indent-for/fold-untyped racket-indent-for/fold-untyped]
           ['racket-indent-maybe-named-let  racket-indent-maybe-named-let]
           [_                               0]))
       (hash-set! racket-indent-function-ht key val)])))
          
(racket:set-indentation)

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
