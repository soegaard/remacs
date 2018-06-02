#lang racket/base
(provide (all-defined-out))

;;;
;;; COLORS
;;;

(require racket/class racket/draw)
(require (only-in srfi/1 circular-list))

(define (color? x)
  (is-a? x color%))

(define (hex->color x)
  (define blue  (remainder           x        256))
  (define green (remainder (quotient x   256) 256))
  (define red   (remainder (quotient x 65536) 256))
  (make-object color% red green blue))

(define base03  (hex->color #x002b36)) ; brblack    background   (darkest)
(define base02  (hex->color #x073642)) ; black      background 
(define base01  (hex->color #x586e75)) ; brgreen    content tone (darkest)
(define base00  (hex->color #x657b83)) ; bryellow   content tone

(define base0   (hex->color #x839496)) ; brblue     content tone
(define base1   (hex->color #x93a1a1)) ; brcyan     content tone (brigtest)
(define base2   (hex->color #xeee8d5)) ; white      background
(define base3   (hex->color #xfdf6e3)) ; brwhite    background   (brightest)

(define yellow  (hex->color #xb58900)) ; yellow     accent color
(define orange  (hex->color #xcb4b16)) ; brred      accent color
(define red     (hex->color #xdc322f)) ; red        accent color
(define magenta (hex->color #xd33682)) ; magenta    accent color
(define violet  (hex->color #x6c71c4)) ; brmagenta  accent color
(define blue    (hex->color #x268bd2)) ; blue       accent color
(define cyan    (hex->color #x2aa198)) ; cyan       accent color
(define green   (hex->color #x859900)) ; green      accent color

(define background-color         base03)
(define region-highlighted-color base00)
(define text-color               base1)
(define border-color             base00)

(define border-pen    
  (new pen% [color base00] [width 1] [style 'solid] [cap 'butt] [join 'miter]))

; Note: point-colors starts with the brightest colors.
(define point-colors (circular-list base3  base3  base2  base1 
                                    base0  base00 base01 base02 base03 base03
                                    base03 base03 base02 base01 base00
                                    base0  base1  base2))

