#lang racket/base
(provide (all-defined-out))

;;; 
;;; EMBEDDED VALUES: PROPERTIES AND OVERLAYS
;;;

(require racket/match
         "representation.rkt")

(define (find-property xs sym)
  (findf (λ (x) (and (property? x) (eq? (property-symbol x) sym)))
         xs))
    
; set-property! : property symbol value -> void
(define (set-property! p sym val)
  (unless (and (property? p) (eq? (property-symbol p) sym))
    (error 'set-property! "expected property with symbol ~a, got ~a" sym p))
  (set-embedded-value! val))

; set-first-property! : list symbol value -> boolean
;    Set value of the first property whose symbol is sym.
;    Return #f if no such property was found.
(define (set-first-property! xs sym val)
  (cond
    [(find-property xs sym) => (λ (p) (set-embedded-value! p val) #t)]
    [else                      #f]))


