#lang racket/base
;;;
;;; COMPLETION
;;;


(provide add-name-to-completions
         completions-lookup
         completions->text
         longest-common-prefix)


(require (only-in srfi/13 string-prefix-length)
         racket/format racket/match racket/string
         "line.rkt"
         "parameters.rkt"
         "text.rkt")

(define completions '())

(define (add-name-to-completions name)
  (set! completions (sort (cons (~a name) completions) string<?)))

(define (completions-lookup partial-name)
  (define r (regexp (~a "^" partial-name)))
  (filter (λ (name) (regexp-match r name))
          completions))

(define (longest-common-prefix xs)
  (match xs
    ['()                ""]
    [(list x)            x]
    [(list "" y zs ...) ""]
    [(list x  y zs ...) (longest-common-prefix 
                         (cons (substring x 0 (string-prefix-length x y)) zs))]))

(define (completions->text so-far cs)
  (define explanation (list (~a "Completions for: " so-far)))
  (new-text (list->lines (for/list ([c (append explanation cs)]) (~a c "\n")))))

