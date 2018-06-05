#lang racket/base
;;;
;;; DEFAULT VALUES
;;;

; If a buffer local variable is unbound, a reference to the variable
; will be be looked up in the default namespace.
; There is only one default namespace, so setting a default variable
; will change the default for all buffers.

;;; Provided Functions

(provide default-namespace  ; the default namespace
         default-value      ; symbol -> value
         default-bound?     ; symbol -> boolean
         set-default!       ; SYNTAX (set-default! id expr)
         set-defaults!      ; SYNTAX (set-defaults! [id expr] ...)
         set-default        ; symbol value -> void
         )

;;; 

(require (for-syntax racket/base syntax/parse)
         racket/format
         "buffer-namespace.rkt")
         
;;; The Default Namespace 

(define default-namespace (new-default-namespace)) ; see "buffer-namespace.rkt"

;;; References

; default-value : symbol -> value
;   Return default (global) value for a buffer local variable.
;   Throw exception if symbol doesn't have a default value.
(define (default-value symbol [on-failure (λ () (error 'default-value (~a symbol " is undefined")))])
  (namespace-variable-value symbol #t on-failure default-namespace))

; default-bound : symbol -> boolean
;   Return boolean indicating whether the symbol has a default value
(define default-bound?
  (let ([unique (gensym "<default-not-bound>")])
    (λ (symbol)
      (with-handlers ([exn? #f])
        (not (eq? unique (namespace-variable-value symbol #t (λ() unique) default-namespace)))))))


;;; Assignments

; SYNTAX (set-default! id expr)
;   Set the default value associated with the identifier id to the value of evaluating
;   the expression expr.
(define-syntax (set-default! stx)
  (syntax-parse stx
    [(_set-default! sym:id expr:expr)
     #'(namespace-set-variable-value! 'sym expr #f default-namespace)]))


; SYNTAX (set-defaults! [symbol expr] ...)
;   Set the defaults values associated with the identifiers value of evaluating
;   the expressions expr.
;   [called setq-default in Emacs] 
(define-syntax (set-defaults! stx)
  (syntax-parse stx
    [(_set-defaults! [sym:id expr:expr] ...)
     #'(begin (set-default! sym expr) ...)]))

; set-default : symbol value -> void
;   Function version of set-default!.
;   Set the default value of symbol to the given value.
(define (set-default symbol value)
  (unless (symbol? symbol) (error 'set-default "expected a symbol, got ~a" symbol))
  (namespace-set-variable-value! symbol value #f default-namespace))

