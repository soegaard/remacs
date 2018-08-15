#lang racket/base
(provide forward-word
         backward-word)

;;;
;;; WORDS
;;;

; In text based modes the document is divided into:
;   characters, words, sentences, and, paragraphs.
; 

(require "chars.rkt"
         "commands.rkt"
         "region.rkt")

(define-interactive (forward-word [n 1])
  "Move point n word forwards."
  "If n is negative, move backwards.
The phrase 'move one word' means move until point the beginning of
a word is found, then move across that word."
  (deactivate-region-mark)  
  (define (forward-word1)
    (forward-char-predicate (λ (c) (not (char-alphabetic? c))))
    (forward-char-predicate char-alphabetic?))
  (cond
    [(= n 1) (forward-word1)]
    [(= n 0) (void)]
    [(< n 0) (backward-word (- n))]
    [else    (for ([_ n]) (forward-word1))]))

(define-interactive (backward-word [n 1])
  (deactivate-region-mark)
  (define (backward-word1)
    (backward-char-predicate (λ (c) (not (char-alphabetic? c))))
    (backward-char-predicate char-alphabetic?))
  (cond
    [(= n 1) (backward-word1)]
    [(= n 0) (void)]
    [(< n 0) (forward-word (- n))]
    [else    (for ([_ n]) (backward-word1))]))
