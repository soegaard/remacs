#lang racket
(provide (all-defined-out))

(require "representation.rkt"
         "parameters.rkt"
         "dlist.rkt"
         "line.rkt"
         "text.rkt")
;;;
;;; WORDS
;;;

(define (word-separator? c)
  (char-whitespace? c))


;;;
;;; MARKS
;;;

(define (mark-compare m1 m2 cmp)
  (define (pos m) (if (mark? m) (mark-position m) m))
  (cmp (pos m1) (pos m2)))
(define (mark<  m1 m2) (mark-compare m1 m2 <))
(define (mark=  m1 m2) (mark-compare m1 m2 =))
(define (mark>  m1 m2) (mark-compare m1 m2 >))
(define (mark<= m1 m2) (mark-compare m1 m2 <=))
(define (mark>= m1 m2) (mark-compare m1 m2 >=))

; new-mark : buffer string integer boolean -> mark
(define (new-mark b name [pos 0] [fixed? #f] #:active? [active? #f])
  ; (define link (text-lines (buffer-text b)))
  (define link (text-lines (buffer-text b)))
  (define m (mark b link pos name fixed? active?))
  (set-linked-line-marks! link (set-add (linked-line-marks link) m))
  m)

; copy-mark : mark -> mark
(define (copy-mark m)
  (match m
    [(mark buffer link position name fixed? active?)
     (mark buffer link position name fixed? active?)]))

; mark-deactivate! mark -> void
(define (mark-deactivate! m)
  (set-mark-active?! m #f))

(define (mark-activate! m)
  (set-mark-active?! m #t))

; delete-mark! : mark -> void
;   remove the mark from the line it belongs to
(define (delete-mark! m)
  ; remove mark from line
  (define link (mark-link m))
  (define b (mark-buffer m))
  (set-linked-line-marks! link (set-remove (linked-line-marks link) m))
  ; remove mark from buffer
  (set-buffer-marks! b (filter (λ(x) (not (eq? x m))) (buffer-marks b))))

; mark-move! : mark integer -> void
;  move the mark n characters
(define (mark-move! m n)
  (define b  (mark-buffer m))
  (define p  (mark-position m))
  (define l  (dfirst (mark-link m)))
  (define ln (line-length l))
  (define-values (old-r old-c) (mark-row+column m))
  ; new position
  (define q (if (> n 0)
                (min (+ p n) (max 0 (- (buffer-length b) 1)))
                (max (+ p n) 0)))
  (set-mark-position! m q)
  (define-values (r c) (mark-row+column m))
  (unless (= old-r r)
    ; remove mark from old line
    (define link (mark-link m))
    (set-linked-line-marks! link (set-remove (linked-line-marks link) m))
    ; insert mark in new line
    (define new-link (dlist-move (first-dcons link) r))
    ; (displayln new-link)
    (set-linked-line-marks! new-link (set-add (linked-line-marks new-link) m))
    ; the mark must point to the new line
    (set-mark-link! m new-link)))

; mark-adjust-insertion-after! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were inserted at position p
(define (mark-adjust-insertion-after! m p a)
  (define mp (mark-position m))
  (when (> mp p)
    ; the insertion was before the mark
    (mark-move! m a)))

; mark-adjust-insertion-before! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were inserted at position p
(define (mark-adjust-insertion-before! m p a)
  (define mp (mark-position m))
  (when (>= mp p)
    ; the insertion was before the mark
    (mark-move! m a)))

; mark-adjust-deletion-before! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were deleted before position p
(define (mark-adjust-deletion-before! m p a)
  (define mp (mark-position m))
  (cond 
    ; the entire deletion was before the mark
    [(<= p mp)      (mark-move! m (- a))]
    ; the entire deletion was after the mark
    [(< mp (- p a)) (void)]
    ; overlap
    [else           (mark-move! m (- a (- p mp)))]))

; mark-adjust-deletion-after! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were after before position p
(define (mark-adjust-deletion-after! m p a)
  (define mp (mark-position m))
  (cond 
    ; the entire deletion was after the mark
    [(<= mp p)       (void)]
    ; the entire deletion was before the mark
    [(<= (+ p a) mp) (mark-move! m (- a))]
    ; overlap
    [else            (mark-move! m (- mp p))]))

; clamp : number number number -> number
;   if minimum <= x <= maximum, return x
;   if x < minimum, return minimum
;   if x > maximum, return maximum
(define (clamp minimum x maximum)
  (max minimum (min x maximum)))

; mark-move-to-column! : mark integer -> void
;   move mark to column n (stay at line)
(define (mark-move-to-column! m n)
  (define-values (r c) (mark-row+column m))
  (unless (= n c)
    (let ([n (clamp 0 n c)]) ; stay on same line
      (mark-move! m (- n c)))))


; mark-on-last-line? : mark -> boolean
;    is m on the last line of its buffer?
(define (mark-on-last-line? m)
  (define-values (row col) (mark-row+column m))
  (define t (buffer-text (mark-buffer m)))
  (= (+ row 1) (text-num-lines t)))

; mark-move-beginning-of-line! : mark -> void
;   move the mark to the beginning of its line
(define (mark-move-beginning-of-line! m)
  (define p (mark-position m))
  (define-values (row col) (mark-row+column m))
  (set-mark-position! m (- p col)))

; position-of-end-of-line : [buffer or mark] -> integer
;   return the position just before the newline of the line of point
(define (position-of-end-of-line [b-or-m (current-buffer)])
  (define m (cond
              [(mark?   b-or-m) b-or-m]
              [(buffer? b-or-m) (buffer-point b-or-m)]
              [else (error 'position-of-end-line (~a "expected mark or buffer, got " b-or-m))]))
  (define b (mark-buffer m))
  (define-values (r c) (mark-row+column m))
  (define n (line-length (dlist-ref (text-lines (buffer-text b)) r)))
  (define p (mark-position m))
  (+ p (- n c) -1))

; position-of-beginning-of-line : [buffer or mark] -> integer
;   return the position of the beginning of the line
(define (position-of-beginning-of-line [b-or-m (current-buffer)])
  (define m (cond
              [(mark?   b-or-m) b-or-m]
              [(buffer? b-or-m) (buffer-point b-or-m)]
              [else (error 'position-of-end-line (~a "expected mark or buffer, got " b-or-m))]))
  (define b (mark-buffer m))
  (define-values (r c) (mark-row+column m))
  (define p (mark-position m))
  (- p c))


(define (position-of-end [b (current-buffer)])
  (text-length (buffer-text b)))

(define (mark-move-end-of-buffer! m)
  (define b (mark-buffer m))
  (set-mark-position! m (position-of-end b)))

(define (mark-move-beginning-of-buffer! m)
  (set-mark-position! m 0))


; mark-move-end-of-line! : mark -> void
;   move the mark to the end of its line
(define (mark-move-end-of-line! m)
  (set-mark-position! m (position-of-end-of-line m)))

; mark-move-up! : mark -> void
;   move mark up one line
(define (mark-move-up! m [n 1])
  ; todo : go from mark to line rather than use dlist-move
  (define (move-one!)
    (define p (mark-position m))
    (define-values (row col) (mark-row+column m))
    (unless (= row 0)
      (define link (dlist-move (first-dcons (text-lines (buffer-text (mark-buffer m)))) (- row 1)))
      (define l (dfirst link)) ; line
      (define new-col (min (line-length l) col))
      (define new-pos (- p col (line-length l) (- new-col)))
      (set-mark-position! m new-pos)
      (set-linked-line-marks! link (set-add (linked-line-marks link) m))
      (define old-link (dlist-move link 1))
      (unless (dempty? old-link) ; xxx
        (set-linked-line-marks! old-link (set-remove (linked-line-marks link) m)))))
  (cond
    [(< n 0) (mark-move-down! m (- n))]
    [else    (for ([i (in-range n)])
               (move-one!))]))

; mark-move-down! : mark -> void
;  move mark down one line
(define (mark-move-down! m [n 1])
  (define (move-one!)
    (define p (mark-position m))
    (define-values (row col) (mark-row+column m))
    (define t (buffer-text (mark-buffer m)))
    (unless (= (+ row 1) (text-num-lines t))
      (define d (dlist-move (text-lines t) row))
      (unless (dempty? d)
        (set-linked-line-marks! d (set-remove (linked-line-marks d) m))
        (define l1 (dfirst d))
        (define l2 (dlist-ref d 1))
        (define new-col (min (line-length l2) col))
        (define new-pos (+ p (- (line-length l1) col) new-col))
        (set-mark-position! m new-pos)
        (define d+ (dlist-move d 1))
        (set-linked-line-marks! d+ (set-add (linked-line-marks d+) m)))))
  (cond
    [(< n 0) (mark-move-up! m (- n))]
    [else    (for ([i (in-range n)])
               (move-one!))]))

; mark-backward-word! : mark -> void
;   move mark backward until a word separator is found
(define (mark-backward-word! m)
  (define-values (row col) (mark-row+column m))
  (define t (buffer-text (mark-buffer m)))
  (define l (text-line t row))
  ; first skip whitespace
  (define i (for/first ([i (in-range (- col 1) -1 -1)]
                        #:when (not (word-separator? (line-ref l i))))
              i))
  (cond
    [(or (not i) (= i 0))
     ; continue searching for word at previous line (unless at top line)
     (mark-move-beginning-of-line! m)
     (unless (= row 0)
       (mark-move! m -1)
       (mark-backward-word! m))]
    [else
     ; we have found a word, find the beginning
     (define j (for/first ([j (in-range (or i (- col 1)) -1 -1)]
                           #:when (word-separator? (line-ref l j)))
                 j))
     ; j is now the index of the first word separator
     (mark-move! m (- (if j (- col (+ j 1)) col)))]))

; mark-forward-word! : mark -> void
;   move mark forward until a word separator is found
(define (mark-forward-word! m)
  (define-values (row col) (mark-row+column m))
  (define t (buffer-text (mark-buffer m)))
  (define l (text-line t row))
  (define n (line-length l))
  ; first skip whitespace
  (define i (for/first ([i (in-range col n)]
                        #:when (not (word-separator? (line-ref l i))))
              i))
  (cond
    [(or (not i) (= i (- n 1)))
     ; continue searching for word at next line (unless at bottom line)
     (mark-move-end-of-line! m)
     (unless (= row (- (text-num-lines t) 1))
       (mark-move! m 1)
       (mark-forward-word! m))]
    [else
     ; we have found a word, find the beginning
     (define j (for/first ([j (in-range (or i (- col 1)) n)]
                           #:when (word-separator? (line-ref l j)))
                 j))
     ; j is now the index of the first word separator
     (mark-move! m (if j (- j col) col))]))

(define (mark-move-to-position! m n)
  ; remove mark from its current line
  (define l (mark-link m))
  (set-linked-line-marks! l (set-remove (linked-line-marks l) m))
  ; find the new line
  (define-values (row col) (mark-row+column m))
  (define d (dlist-move (text-lines (buffer-text (mark-buffer m))) row))
  ; add mark to the new line
  (set-linked-line-marks! d (set-add (linked-line-marks d) m))
  ; store the new position
  (set-mark-position! m n))

(define (mark-move-to-row+column! m r c)
  ; get the number of lines in the text
  (define b  (mark-buffer m))
  (define t  (buffer-text b))
  (define nt (text-num-lines t))
  ; remove mark from its current line
  (define l (mark-link m))
  (set-linked-line-marks! l (set-remove (linked-line-marks l) m))
  ; find the new position
  (define n (row+column->position t r c))
  (define end? (>= r nt))
  (define row (if end? (- nt 1) r))
  (define d (dlist-move (text-lines (buffer-text (mark-buffer m))) row))
  ; add mark to the new line
  (set-linked-line-marks! d (set-add (linked-line-marks d) m))
  ; store the new position
  (set-mark-position! m (if end? (- n 1) n)))


(define (mark-move-to-beginning-of-paragraph! m)
  ;; TODO : improve efficiency (i.e. avoid vall to mark-move-up!)
  ;; paragraphs are separated by one or more blank lines
  (mark-move-beginning-of-line! m)
  (let loop ([l (mark-link m)])
    (cond
      [(first-dcons? l)         (void)]
      [(line-blank? (dfirst l)) (mark-move-down! m)]
      [else                     (mark-move-up! m)
                                (loop (mark-link m))]))) ; TODO does up and down update the link ?!!>?
