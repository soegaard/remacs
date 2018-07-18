#lang racket/base
(provide (all-defined-out))

;;;
;;; INTERACTIVE COMMANDS
;;;

;;; Interactive commands are user commands. I.e. the user can
;;; call them via key-bindings or via M-x.

;;; Names of interactive commands are registered in order to 
;;; provide completions for the user.


(require (for-syntax racket/base syntax/parse)
         racket/format
         "completion.rkt"
         "render.rkt")

; all-interactive-commands-ht : string -> command
(define all-interactive-commands-ht (make-hash))
; add-interactive-command : string command -> void
(define (add-interactive-command name cmd)
  (hash-set! all-interactive-commands-ht (~a name) cmd))
; lookup-interactive-command : string -> command-or-#f
(define (lookup-interactive-command cmd-name)
  (hash-ref all-interactive-commands-ht (~a cmd-name) #f))

; SYNTAX (define-interactive name expr ...)
;        (define-interactive (name arg ...) expr ...)
; As define the name is bound to a function, but the
; name is also registered as an interactive command.
(define-syntax (define-interactive stx)
  (syntax-parse stx
    [(d-i name:id expr)
     (syntax/loc stx
       (begin
         (add-name-to-completions 'name)
         (define name expr)
         (add-interactive-command 'name name)))]
    [(d-i (name:id . args) expr ...)
     (syntax/loc stx
       (begin
         (add-name-to-completions 'name)
         (define (name . args)
           ; (displayln 'name (current-error-port))
           (with-suspended-rendering               
               expr ...))
         (add-interactive-command 'name name)))]
    [_ (raise-syntax-error 'define-interactive "bad syntax" stx)]))
