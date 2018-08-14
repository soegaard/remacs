#lang racket/base
(provide (all-defined-out))

#;(define (position pos)
    (if (mark? pos) (mark-position pos) pos))

;;;
;;; Character
;;;

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