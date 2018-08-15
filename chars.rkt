#lang racket/base
(provide (all-defined-out))

;;;
;;; Character
;;;

; Characters the bread and butter of a text editor are represented as normal Racket characters.
; Different modes have a need to modify the classification of characters.
; In Emacs the classification of characters is named "syntax classes". In Racket "syntax class"
; is used in syntax-parse, so we use the term category instead.


; Movement:
;   forward-char            : move point forward
;   backward-char           : move point backwards
;   goto-char               : move point to position
;   forward-char-predicate  : move forward while predicate is true
;   backward-char-predicate ; move backward white predicate is true

; Looking:
;   char-before       : char before a mark
;   char-after        : char after  a mark
;   char-before-point : char before the point
;   char-after-point  : char after  the point

; Syntax category:
;   char-category     : the category of a character

; The categories of characters are represented as structs:

(struct syntax-category ())
(struct opener             syntax-category (close)) ; ( [ {
(struct closer             syntax-category (open))  ; ) ] }
(struct blank              syntax-category ())      ; space, tab
(struct comment-ender      syntax-category ())      ; newline
(struct comment-starter    syntax-category ())      ; ;
(struct string-starter     syntax-category (ender)) ; "
(struct symbol-constituent syntax-category ())      ; a-z A-Z 0-9
(struct expression-prefix  syntax-category ())      ; ' , #
; (struct word-constituent   syntax-category ())      ; alphabetic and numbers

; The comments after the categories are examples.

(require racket/format
         "buffer.rkt"
         "commands.rkt"
         "line.rkt"
         "mark.rkt"
         "narrow.rkt"
         "point.rkt"
         "region.rkt"
         "representation.rkt")

(define-interactive (goto-char pos [m #f])
  (define who 'goto-char)
  ; (check-position 'goto-char pos)
  (define i (position pos))
  (unless (and (integer? i) (>= i 0)) (error who (~a "expected index, got " i)))
  (let ([i (clamp (point-min) i (point-max))]) ; handles narrowing
    (cond
      [m    (mark-move-to-position! m           i)]
      [else (mark-move-to-position! (get-point) i)])))


(define-interactive (forward-char [n 1] #:keep-region [keep? #f])
  (define m (get-point))
  (check-mark m)
  (unless keep? (deactivate-region-mark))
  (define-values (start end) (point-min+max))
  (define i (clamp start (+ (mark-position m) n) end))
  (mark-move-to-position! m i))

(define-interactive (backward-char [n 1] #:keep-region [keep? #f])
  (forward-char (- n) #:keep-region keep?))


;;; Looking

(define (char-before m)
  (cond
    [(mark? m) (define l (mark-line m))
               (define c (mark-column m))
               (cond [(= (position m) (point-min)) #f]
                     [(not c)                      #f] ; ??
                     [(= c 0)                      #\newline]
                     [else                         (line-ref l (- c 1))])]
    [(integer? m) (if (= m (point))
                      (char-before (get-point))
                      (with-saved-point
                        (goto-char m)
                        (char-before (get-point))))]
    [else (error 'char-before "expected position")]))

(define (char-before-point)
  (char-before (get-point)))

(define (char-after [m #f])
  (cond
    [(eq? m (get-point)) (char-after-point)]
    [(and (integer? m)
          (= m (point))) (char-after-point)]
    [else                (error 'char-after "todo")]))

(define (char-after-point)
  ; (displayln 'char-after-point (current-error-port))
  (define p   (get-point))
  (define l   (mark-line p))
  (define c   (mark-column p))
  (cond [(= (position p) (point-max))              #f]
        [(not c)                                   #f]  ; empty line?
        [(= c (line-length l))                     #\newline]
        [(> c (line-length l))
         (displayln (~a "column: " c )             (current-error-port))
         (displayln (~a "len: "   (line-length l)) (current-error-port))
         (displayln (~a "line: " (line->string l)) (current-error-port))
         (displayln (~a "point: "  (point))        (current-error-port))
         (displayln p     (current-error-port))
         (error 'char-after-point "internal error")]
        [else                                      (line-ref l c)]))

;;; Skipping

(define (forward-char-predicate pred)
  ; Skip ahead until (pred (char-after-point)) is no longer true
  (let loop ()
    (define c (char-after-point))
    (cond
      [(not c)  (void)]
      [(pred c) (forward-char) (loop)]
      [else     (void)])))

(define (backward-char-predicate pred)
  ; Skip ahead until (pred (char-after-point)) is no longer true
  (let loop ()
    (define c (char-before-point))
    (cond
      [(not c)  (void)]
      [(pred c) (backward-char) (loop)]
      [else     (void)])))

;;;
;;; Syntax Categories
;;;

; Syntax categories are called syntax classes in Emacs


(define syntax-category-ht
  (make-hasheqv (list (cons #\(       (opener #\)))
                      (cons #\[       (opener #\]))
                      (cons #\{       (opener #\}))
                      (cons #\)       (closer #\())
                      (cons #\]       (closer #\[))
                      (cons #\}       (closer #\{))
                      (cons #\"       (string-starter #\"))
                      (cons #\space   (blank))
                      (cons #\tab     (blank))
                      (cons #\newline (comment-ender))
                      (cons #\;       (comment-starter))
                      (cons #\'       (expression-prefix))
                      (cons #\,       (expression-prefix))
                      (cons #\#       (expression-prefix))))) ; @ ?

(define (char-category c)
  (hash-ref syntax-category-ht c #f))
      

(define (add-char-range from to cat)
  (for ([c (in-range (char->integer from) (char->integer to))])
    (hash-set! syntax-category-ht (integer->char c) cat)))

(add-char-range #\a #\z (symbol-constituent))
(add-char-range #\A #\Z (symbol-constituent))
(add-char-range #\0 #\9 (symbol-constituent))
