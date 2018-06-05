#lang racket/base
;;;
;;; BUFFER LOCALS
;;;

; Each buffer has an associated namespace (a mapping from symbols to values).
; The buffer local variables can set and referenced when evaluating expressions in the buffer.
; Setting a buffer local variable only affects the current buffer.
; Setting a buffer local variable can be used by the user to override a default setting
; for a particular buffer. Buffer locals are used by modes to hold settings.

; Referencing an unbound variable will return the default value instead (see default.rkt).

(provide set-buffer-local!
         ref-buffer-local
         local)

(require (for-syntax racket/base syntax/parse)
         racket/format
         "default.rkt"
         "parameters.rkt"
         "representation.rkt")

;;; Variable References
; Since references to top level variables (and unbound variables) are
; special, we define our own #%top here.

(module* buffer-top #f
  (provide (rename-out [buffer-top #%top]))
  (require "parameters.rkt")
  (define (on-error sym) (error 'buffer-top (~a sym " is undefined")))
  (define-syntax (buffer-top stx)
    (syntax-parse stx
      [(_ . id:id)
       (syntax/loc stx
         (cond
           [(namespace-variable-value 'id #t (λ () (on-error 'id))) => values]
           [else (default-value 'id)]))])))

; ref-buffer-local : symbol [buffer] [thunk] -> value
;   Return the value of the variable given by symbol.
;   If the variable is unbound in the buffer namespace, it
;   is looked up in the default namespace.
(define (ref-buffer-local sym
                          [b        (current-buffer)]
                          [on-error (λ () (error 'ref-buffer-local (~a sym " is undefined")))])
  (define (on-failure) (if (procedure? on-error) (on-error) on-error))
  (define (on-unbound) (default-value sym on-failure))
  (define ns (buffer-locals b))
  (namespace-variable-value sym #t on-unbound ns))

(define local ref-buffer-local)

;;; Variable Assigments

; set-buffer-local! : symbol value [buffer] -> void
;   Set the buffer local variable given by the symbol to the given value.
(define (set-buffer-local! sym v [b (current-buffer)])
  (define ns (buffer-locals b))
  (namespace-set-variable-value! sym v #f ns))

