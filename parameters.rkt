#lang racket
(provide current-buffer
         current-refresh-frame)

;;;
;;; PARAMETERS
;;;

(define current-buffer        (make-parameter #f))
(define current-refresh-frame (make-parameter void))