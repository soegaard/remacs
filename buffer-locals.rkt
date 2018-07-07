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

(provide invoke-local
         local
         local!
         ref-buffer-local
         set-buffer-local!)

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


;;; Variable Assigments

; set-buffer-local! : symbol value [buffer] -> void
;   Set the buffer local variable given by the symbol to the given value.
(define (set-buffer-local! sym v [b (current-buffer)])
  (define ns (buffer-locals b))
  (namespace-set-variable-value! sym v #f ns))

; SYNTAX (invoke-lokal id expr ...)
;        (invoke-lokal id expr ... #:buffer b)
;   Lookup the buffer-local variable id in the buffer b (or (current-buffer)),
;   then call it with arguments expr ... .
(define-syntax (invoke-local stx)
  (syntax-parse stx
    [(_invoke-local id:id expr:expr ... #:buffer b)
     (syntax/loc stx
       (let ([f (ref-buffer-local 'id b #f)])
         (cond
           [(procedure? f) (f expr ...)]
           [else           (raise-syntax-error
                            'invoke-local "the buffer local is not bound to a procedure" #'id)])))]
    [(_invoke-local id:id expr:expr ...)
     (syntax/loc stx (invoke-local id expr ... #:buffer (current-buffer)))]
    [_ (raise-syntax-error 'invoke-local "expected (invoke-local id expr ...)" stx)]))
    


(define-syntax (local stx)
  (syntax-parse stx
    [(_local id:id on-error) (syntax/loc stx (ref-buffer-local 'id (current-buffer) on-error))]
    [(_local id:id)          (syntax/loc stx (ref-buffer-local 'id))]
    [_ (raise-syntax-error 'local "expected (local id) or (local id on-error)" stx)]))

(define-syntax (local! stx)
  (syntax-parse stx
    [(_local! id:id e:expr on-error) (syntax/loc stx (set-buffer-local! 'id e on-error))]
    [(_local! id:id e:expr)          (syntax/loc stx (set-buffer-local! 'id e))]
    [_ (raise-syntax-error 'local! "expected (local! id expr) or (local! id expr on-error)" stx)]))
