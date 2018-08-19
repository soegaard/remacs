#lang racket/base
;;;
;;; SHOW PAREN MODE
;;;

; This minor mode will color the background of a parenthesized s-expressions,
; when point is immediately before or after a parenthesis.

; Let's denote the point with |.
; We have three situations to color:
;       |(bar)    point before paren
;  (foo)|         point after  paren
;  (foo)|(bar)    point before and after paren

; The left 

