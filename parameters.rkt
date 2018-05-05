#lang racket
(provide current-buffer
         current-refresh-frame
         current-default-major-mode
         current-append-next-kill)

;;;
;;; PARAMETERS
;;;

(define current-buffer             (make-parameter #f))
(define current-refresh-frame      (make-parameter void))

(define current-default-major-mode (make-parameter 'fundemental-mode))

(define current-append-next-kill   (make-parameter #f))