#lang racket/base
(provide (all-defined-out))

;;;
;;; FRAMES
;;;

(require racket/list racket/match
         "parameters.rkt"
         "render.rkt"
         "representation.rkt")

(define (refresh-frame [f (current-frame)])
  (unless (current-rendering-suspended?)
    (when (and f (frame? f))
      ((current-render-frame) f))))

(current-refresh-frame refresh-frame)

(define (frame-window-tree [f (current-frame)])
  (define (loop w)
    (match w
      [(horizontal-split-window f _ _ c p b s e l r)               (append (loop l) (loop r))]
      [(vertical-split-window   f _ _ c p b s e u l)               (append (loop u) (loop l))]
      [(window frame panel borders canvas parent buffer start end) (list w)]))
  (flatten (loop (frame-windows f))))
