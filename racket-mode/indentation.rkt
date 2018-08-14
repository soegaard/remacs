#lang racket/base
(provide break-line-and-indent
         racket-indent-for-tab
         indent-line)
;;;
;;; INDENTATION FOR RACKET
;;;

;; This file implements indentation for the Racket programming language.
;; The implementation is heavily based on Greg Hendershott's elisp code
;; for the indentation in racket-mode for Emacs.
;; https://github.com/greghendershott/racket-mode/blob/master/racket-indent.el
         
(require (for-syntax racket/base syntax/parse)
         racket/format racket/list racket/match racket/set
         "../buffer.rkt"
         "../chars.rkt"
         "../commands.rkt"
         "../mark.rkt"
         "../parameters.rkt"
         "../point.rkt"
         "../search.rkt"
         "../simple.rkt")

;;; These variables are dir local in Emacs racket-mode.
(define racket-indent-sequence-depth    3)
(define racket-indent-curly-as-sequence #t)

;;;
;;; GENERAL
;;; 

(define (racket-indent-for-tab)
  (indent-line))

(define (break-line-and-indent)
  (break-line)
  (racket-indent-for-tab))

(define-interactive (indent-line)
  "Indent a single line"
  ;(displayln 'indent-line (current-error-port))
  (define amount (calculate-indent))
  ;(displayln (list 'indent-line 'amount amount))
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

(define (plain-beginning-of-define)
  (backward-to-open-parenthesis-on-beginning-of-line))

(define (calculate-indent)
  ;(displayln 'calculate-indent (current-error-port))
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is #f, that means don't change the indentation
because the line starts inside a string.
This is `calculate-lisp-indent' distilled to what we actually
need."
  (with-saved-point
    (beginning-of-line)
    (define indent-point (point))
    ; move back to position with an empty parse state
    (plain-beginning-of-define)
    ;(displayln (list 'point-at-beginnning-of-define (point)))
    ; parse forward to the indent-point
    (define state
      (let loop ([s empty-state] [old-point #f])
        (if (and (<= (point) indent-point)
                 (not (equal? old-point (point))))
            (loop (parse-partial-sexp (point) indent-point #:state s #:target-depth 0)
                  (point))
            (begin
              #;(when (equal? old-point (point))
                (display-state s)
                (displayln (list 'zz 'indent-point indent-point 'point (point))))
              s))))
    ;(displayln (list 'point-after-state-found (point)))
    ;(display-state state)
    (let ([str?  (state-inside-string  state)]   ; inside string?
          [cmt?  (state-inside-comment state)]   ; inside comment?
          [last  (state-last-complete  state)]   ; start of last complete s-expr 
          [cont  (state-inner-start    state)]   ; start of inners most parent s-expr (containing)
          [depth (state-depth          state)])  ; paren depth
      #;(display (list 'str? str? 'cmt? cmt? 'last last 'cont cont 'depth depth)
               (current-error-port))
      (cond
        ; no indentation if point is in a string or comment
        [(or str? cmt?)                    ;(displayln "X")
                                           #f]
        [(= depth 0)                       0]
        [(and (not (= depth 0)) last cont) ;(displayln "Y")
                                           (indent-function indent-point state)]
        [cont                              ;(displayln "Z")
                                           (goto-char (+ cont 1)) ; why?
                                           (current-column)]
        [else                              ;(displayln "W")
                                           (current-column)]))))

(define lisp-body-indent 2)

(define (indent-function indent-point state)
  ; (displayln 'indent-function (current-error-port))
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
  ; record a few positions relative to the current point (maybe needed later)
  (define no-head-indent (+ (current-column) 1))
  (define body-indent    (+ (current-column) lisp-body-indent))
  ; if we are dealing with a define-like expression,
  ; we will later need to know the indendentation of the body  
  ; skip the parenthesis of inner most s-exp
  (forward-char 1)  
  ; 
  (cond
    [(or (hash-literal-or-keyword?) (data-sequence?))
     ; (displayln "YYY")
     (backward-prefix-chars)
     (current-column)]
    [else
     ; get the head of the inner-most sexp
     (define head (buffer-substring (current-buffer) (point) (begin (forward-sexp 1) (point))))
     ; find out how to indent that type of s-expression
     (define method (get-indent-function-method head)) ; method = #f when head empty          
     ; (displayln (list 'head head 'method method 'body-indent body-indent))
     (cond
       [(only-whitespace-and-newline? head) no-head-indent]
       [(integer? method)                   (racket-indent-special-form method indent-point state)]
       [(eq? method 'define)                body-indent]
       [method                              (method indent-point state)]
       [(regexp-match "^(def|with-)" head)  body-indent] ;just like 'define
       [(regexp-match "^begin" head)        (racket-indent-special-form 0 indent-point state)]
       [else                                (racket-normal-indent indent-point state)])]))

(define racket-indent-function-ht (make-hash))

(define (get-indent-function-method head)
  (define sym (string->symbol head))
  (hash-ref racket-indent-function-ht sym #f))



(define-syntax (inc! stx) (syntax-parse stx [(_inc x) (syntax/loc stx (begin (set! x (+ x 1)) x))]))
(define-syntax (dec! stx) (syntax-parse stx [(_dec x) (syntax/loc stx (begin (set! x (- x 1)) x))]))

(define (only-whitespace-and-newline? str)
  (not (string-match #rx"[^ \t\n]" str)))

(define (string-match regexp string [start 0])
  ; Return index of start of first match.
  (define ps (regexp-match-positions regexp string start))
  (and ps
       (car (first ps))))

(define (racket-normal-indent indent-point state)
  ; (displayln 'racket-normal-indent-function (current-error-port))  
  "Return the indentation column for an s-expression with standard indentation."
  ; In the following examples, the x marks the spot where the point should end up.
  ;    (       (a         (a b     (a b c   (a b c d     (   (a  (a b   (a b     (a b
  ;     x)      x)           x        x        x          b   b     c      c d         c d
  ;                                                       x   x     x      x           x
  ; The first five examples show how to indent in line after the head line.
  ;   The first two examples are special cases.
  ;   The next three examples show that we want to indent to the start of the
  ;   first sexp after the head.  
  ; The last five examples show how to indent a line not directly below the head line:
  ;   Indent to the start of the first sexp on the previous line.
  (define (string-on-line-before-point)
    (buffer-substring (current-buffer) (line-beginning-position) (point)))
  (define (only-whitespace? str)
    (not (string-match #rx"[^ \t]" str)))
  
  ; 1. Find position from where we can use backward-sexp to go backwards.
  (goto-char (state-last-complete state)) ; start of last complete sexp
  (backward-prefix-chars)
  ; 2. Go back until we find the head or an s-expression that appears
  ;    as the first thing on a line. If we find the head, we will need
  ;    to know the position of the s-expression after head (if any).
  (let loop ([after #f])
    (cond
      [(only-whitespace? (string-on-line-before-point))
       (current-column)]
      [else
       (define here (point))
       (backward-sexp)
       (cond [(= here (point)) ; found head
              ; if there an argument after head, move to it
              (when (and after (< after (line-end-position)))
                (goto-char after))
              ; otherwise just align with head
              (current-column)]
             [else
              (loop here)])])))

(define (racket-indent-special-form method indent-point state)
  ; (displayln 'racket-indent-special-form (current-error-port))
  "METHOD must be a nonnegative integer -- the number of
  \"special\" args that get extra indent when not on the first
  line. Any additional args get normal indent."
  ; At entry we know that we are indenting a parenthesized
  ; s-expression. We first find the column of the start parenthesis.
  (define containing-column
    ; todo: make a column-at-position
    (with-saved-point (goto-char (state-inner-start state))
                      (current-column)))
  ; Now we need to count how many complete s-expressions are before
  ; the indent point. 
  (define pos
    (let loop ([count     -1]
               [old-point #f]) ; needed to se if forward-sexp makes progress
      (cond
        ; no progress?
        [(or (eq? (point) old-point) (eob?)) (+ count 1)]
        ; we found the indent point
        [(> (point) indent-point)            count]
        ; move forward again 
        [else                                (forward-sexp)
                                             (loop (+ count 1) (point))])))
  ; we need to figure the index of the s-expression at indent-point      
  (cond [(= method pos)               ;first non-distinguished arg
         (+ containing-column lisp-body-indent)]
        [(< method pos)               ;more non-distinguished args
         (racket-normal-indent indent-point state)]
        [else                         ;distinguished args
         (+ containing-column (* 2 lisp-body-indent))]))

(define (racket-conditional-indent indent-point state looking-at-regexp true false)
  (skip-chars-forward " \t")
  (let ([n (if (looking-at looking-at-regexp) true false)])
    (racket-indent-special-form n indent-point state)))

(define racket-identifier-regexp
  #rx"^[a-zA-Z0-9]+") ; todo: use the racket-lexer
;  (rx (or (syntax symbol) (syntax word) (syntax punctuation))))
;  "A regexp matching valid Racket identifiers."

(define (racket-indent-maybe-named-let indent-point state)
  (displayln 'racket-indent-maybe-named-let (current-error-port))
  "Indent a let form, handling named let (let <id> <bindings> <expr> ...)"
  (racket-conditional-indent indent-point state
                             racket-identifier-regexp
                             2 1))

(define (racket-indent-for indent-point state)
  (displayln 'racket-indent-for (current-error-port))
  "Indent function for all for/ and for*/ forms EXCEPT
for/fold and for*/fold.
Checks for either of:
  - maybe-type-ann e.g. (for/list : T ([x xs]) x)
  - for/vector optional length, (for/vector #:length ([x xs]) x)"
  (racket-conditional-indent indent-point state
                             #rx"[:#]"
                             3 1))

(define (racket-indent-for/fold indent-point state)
  (displayln 'racket-indent-for/fold (current-error-port))
  "Indent function for for/fold and for*/fold."
  ;; check for maybe-type-ann e.g. (for/fold : T ([n 0]) ([x xs]) x)
  (skip-chars-forward " \t\n")
  (if (looking-at ":")
      (racket-indent-special-form 4 indent-point state)
      (racket-indent-for/fold-untyped indent-point state)))

(define (racket-indent-for/fold-untyped indent-point state)
  (displayln 'racket-indent-for/fold-untyped (current-error-port))
  (let* ((containing-sexp-start  (state-inner-start state))
         (_                      (goto-char containing-sexp-start))
         (containing-sexp-column (current-column))
         (containing-sexp-line   (line-number-at-pos))
         (body-indent            (+ containing-sexp-column lisp-body-indent)))
    ;; Move to the open paren of the first, accumulator sexp
    (forward-char)    ;past the open paren
    (forward-sexp)
    (forward-sexp)    ;to the next sexp, past its close paren         
    (backward-sexp)   ;back to its open paren
    ;; If the first, accumulator sexp is not on the same line as
    ;; `for/fold`, then this is simply specform 2.
    (cond
      [(not (= (line-number-at-pos) containing-sexp-line)) ;expensive?
       (racket-indent-special-form 2 indent-point state)]
      [else
       (define clause-indent (current-column))
       (forward-sexp 1)    ;past close paren
       ;; Now go back to the beginning of the line holding
       ;; the indentation point. Count the sexps on the way.
       ; (parse-partial-sexp start   limit #:state [start-state #f] #:target-depth [target-depth #f])
       ; todo: implement stop-before
       ; todo: start state?
       ; (parse-partial-sexp (point) indent-point #:target-depth 1 #:stop-before #t)

       ; Now we need to count how many complete s-expressions are before
       ; the indent point. 
       (define n
         (let loop ([count     -1]
                    [old-point #f]) ; needed to se if forward-sexp makes progress
           (cond
             ; no progress?
             [(or (eq? (point) old-point) (eob?)) (+ count 1)]
             ; we found the indent point
             [(> (point) indent-point)            count]
             ; move forward again 
             [else                                (forward-sexp)
                                                  (loop (+ count 1) (point))])))
       (if (= n 1)
           clause-indent
           body-indent)])))


(define (hash-literal-or-keyword?)
  "Looking at things like #fl() #hash() or #:keyword ?
The last occurs in Racket contract forms, e.g. (->* () (#:kw kw)).
Returns nil for #% identifiers like #%app."
  (looking-at #rx"#(:|[^%])"))

(define (data-sequence?)
  ; (displayln 'data-sequence (current-error-port))
  "Looking at 'data' sequences where we align under head item?
   These sequences include '() `() #() -- and {} when
   racket-indent-curly-as-sequence is #t -- but never #'() #`() ,()
   ,@().
   To handle nested items, we search backward-up-list up to
   racket-indent-sequence-depth times."
  (and (< 0 racket-indent-sequence-depth)
       (with-saved-point
         (let loop ([answer 'unknown])
           (define depth  racket-indent-sequence-depth)
           (when (and (eq? answer 'unknown) (> depth 0))
             ; (displayln 1)
             (backward-up-list)
             ; (displayln 2)
             (dec! depth)
             ; (displayln 3)
             (cond
               [(or
                 ;; a quoted '( ) or quasiquoted `( ) list --
                 ;; but NOT syntax #'( ) or quasisyntax #`( )
                 (and #;(displayln '3a)
                      (memq (char-before (point)) '(#\' #\`))
                      (eqv? (char-after  (point)) #\()
                      (not (eqv? (char-before (- (point) 1)) #\#)))
                 ;; a vector literal: #( )
                 (and #;(displayln '3b)
                      (eqv? (char-before (point)) #\#)
                      (eqv? (char-after  (point)) #\())
                 ;; { }
                 (and #;(displayln '3c)
                      racket-indent-curly-as-sequence
                      (let ([t (eqv? (char-after (point)) #\{)])
                        #;(displayln '3d)
                        t)))
                ;(displayln 4)
                (loop #t)]
               [;; unquote or unquote-splicing
                (and (or (eqv? (char-before (point)) #\,)
                         (and (eqv? (char-before (- (point) 1)) #\,)
                              (eqv? (char-before    (point))    #\@)))
                     (eqv? (char-after (point)) #\())
                ;(displayln 5)
                (loop #f)]))
           (if (eq? answer 'unknown)
               #f
               answer)))))
  
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
