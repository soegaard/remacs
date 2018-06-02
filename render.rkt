#lang racket/base

(provide with-suspended-rendering)

(require (for-syntax racket/base syntax/parse)
         (for-template "parameters.rkt")
         "parameters.rkt")



(define-syntax (with-suspended-rendering stx)
  (syntax-parse stx
    [(_with-suspended-rendering body ...)
     (syntax/loc stx
       (let ()
         (parameterize ([current-rendering-suspended? #t])
           body ...)
         (current-render-frame (current-frame))))]))
