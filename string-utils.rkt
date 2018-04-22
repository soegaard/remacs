#lang racket
(provide string-whitespace? ; is the string all whitespace ?
         )

;;;
;;; STRINGS
;;;

(define (string-whitespace? s)
  (regexp-match? #px"^[[:space:]]*$" s))

(module+ test (require rackunit))

(module+ test 
  (check-true  (string-whitespace? " \n\t"))
  (check-false (string-whitespace? " x ")))
