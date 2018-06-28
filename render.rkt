#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base syntax/parse)
         (for-template "parameters.rkt")
         racket/class racket/draw
         "canvas.rkt"
         "colors.rkt"
         "parameters.rkt"
         "representation.rkt")

(define-syntax (with-suspended-rendering stx)
  (syntax-parse stx
    [(_with-suspended-rendering body ...)
     (syntax/loc stx
       (let ()
         (parameterize ([current-rendering-suspended? #t])
           body ...)
         #;((current-render-frame) (current-frame))))]))

;;;
;;; FONT
;;;
(define default-font-size 16)

(define font-style  (make-parameter 'normal))  ; style  in '(normal italic)
(define font-weight (make-parameter 'normal))  ; weight in '(normal bold)
(define font-color  (make-parameter  base1))   ; 

(define the-font-size default-font-size)
(define (font-size [n #f]) (when n (set! the-font-size n)) the-font-size)

(define font-family (make-parameter 'modern))  ; fixed width

(define (use-default-font-settings)
  (font-style  'normal)
  (font-weight 'normal)
  ;(font-size   16)
  (font-family 'modern))

(define font-ht (make-hash))                   ; (list size family style weight) -> font  
(define (get-font)
  (define key (list (font-size) (font-family) (font-style) (font-weight)))
  (define font (hash-ref font-ht key #f))
  (unless font
    (set! font (make-object font% (font-size) (font-family) (font-style) (font-weight)))
    (hash-set! font-ht key font))
  font)

(define (toggle-bold)    (font-weight (if (eq? (font-weight) 'normal) 'bold   'normal)))
(define (toggle-italics) (font-style  (if (eq? (font-style)  'normal) 'italic 'normal)))
(define (default-fixed-font)  (get-font))

(define-syntax (define-stack stx)
  (syntax-parse stx
    [(_define-stack name reset push pop param default)
     (syntax/loc stx
       (begin (define name '())
              (define (reset) (set! name '()))
              (define (push v) (set! name (cons v name)))
              (define (pop)
                (cond [(null? name) (displayln (list "Warning: default used" 'name)) default]
                      [else         (begin0 (car name)
                                            (param (car name))
                                            (set! name (cdr name)))]))))]))

(define-stack color-stack  reset-color-stack  push-color  pop-color  font-color   cyan)
(define-stack weight-stack reset-weight-stack push-weight pop-weight font-weight  'normal)
(define-stack style-stack  reset-style-stack  push-style  pop-style  font-style   'normal)

(define (set-font dc) (send dc set-font (get-font)))

;;; 
;;; LINES
;;;

; The size of a line is the same as the font size plus one.
(define (line-size) (+ (font-size) 1))

(define (number-of-lines-on-screen w)
  (define b (window-buffer w))
  (define c (window-canvas w))
  (define-values (xmin xmax ymin ymax) (canvas-dimensions c))
  (define width  (- xmax xmin))
  (define height (- ymax ymin))
  (define fs (font-size))
  (define ls (line-size)) ; BUG todo this looks wrong: the font-size might not match the pixel size
  ;; Placement of point relative to lines on screen
  (define n (max 0 (quotient height ls)))
  n)

