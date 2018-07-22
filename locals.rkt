#lang racket/base
(provide make-local
         localize)


;;;
;;; LOCALS
;;;

; This provides make-local     and localize which behaves like
; make-parameter and parameterize except the variables aren't
; thread local.

(require (for-syntax racket/base syntax/parse))

(define (make-local initial-value)
  (let ([value initial-value])
    (define local
      (case-lambda
        [()          value]
        [(new-value) (set! value new-value)]))
    local))

(define-syntax (localize stx)
  (syntax-parse stx
    [(_localize ([local:id e:expr] ...) body ...)
     (with-syntax ([(t-old ...) (generate-temporaries #'(local ...))]
                   [(t-new ...) (generate-temporaries #'(local ...))])
       (syntax/loc stx
         (let ([t-old #f] ...
               [t-new e]  ...)
           (set! t-old (local)) ...
           (dynamic-wind
            (λ () (local t-new) ...)              
            (λ ()  body ...)
            (λ () (local t-old) ...)))))]))

#;(test 
   (define x (make-local 42))
   (x)
   (x 43)
   (x)

   (localize ([x 44])
             (displayln (x))
             (x 45)
             (displayln (x)))
   (x))
