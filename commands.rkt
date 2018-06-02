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

(define all-interactive-commands-ht (make-hash))
(define (add-interactive-command name cmd)
  (hash-set! all-interactive-commands-ht (~a name) cmd))
(define (lookup-interactive-command cmd-name)
  (hash-ref all-interactive-commands-ht (~a cmd-name) #f))

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
           (with-suspended-rendering               
               expr ...))
         (add-interactive-command 'name name)))]
    [_ (raise-syntax-error 'define-interactive "bad syntax" stx)]))
