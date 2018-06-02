#lang racket/base
(provide message)

;;;
;;; MESSAGES
;;;

; Messages currently appear on the top line in the gui.


(require racket/class
         "parameters.rkt")

(define (message str [msg (current-message)])
  (send msg set-label str))
