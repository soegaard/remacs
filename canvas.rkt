#lang racket/base
(provide canvas-dimensions)

;;;
;;; CANVAS
;;;

(require racket/class)

(define (canvas-dimensions c)
  (define dc (send c get-dc))
  (define xmin 0)
  (define xmax (send c get-width))
  (define ymin 0)
  (define ymax (send c get-height))
  (values xmin xmax ymin ymax))

