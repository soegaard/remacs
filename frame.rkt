#lang racket/base
(provide (all-defined-out))
(displayln "FRAME.RKT")
;;;
;;; FRAMES
;;;

(require racket/class racket/list racket/match
         racket/gui/base
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
      [(horizontal-split-window f _ __  c p b  m  l r)                (append (loop l) (loop r))]
      [(vertical-split-window   f _ __  c p b  m  u l)                (append (loop u) (loop l))]
      [(window frame panel borders canvas parent buffer point) (list w)]
      [_ (error)]))
  (flatten (loop (frame-windows f))))

(define (frame->windows f)
  (define (loop ws)
    (match ws
      [(vertical-split-window   _ _ _  _   _ _ _ upper lower)
       (append (loop upper) (loop lower))]
      [(horizontal-split-window _ _ _  _   _ _ _ left right)
       (append (loop left) (loop right))]
      [w (list w)]))
  (loop (frame-windows f)))
