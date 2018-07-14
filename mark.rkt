#lang racket/base
(provide (except-out (all-defined-out) position))
;;;
;;; MARKERS
;;; 

;; Markers are used to specify a position in the text.
;; Inserting and deleting characters in the text adjust any markers in the text.
;; In other words a mark stays between two characters.

;; A special mark "the mark" is used together with the point to indicate a region.
;; Only an active mark makes a region with point.

(require racket/set racket/match racket/format
         data/interval-map
         "representation.rkt"
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

; position : mark-or-integer -> integer
(define (position mark-or-pos)
  (define pos mark-or-pos)
  (if (mark? pos) (mark-position pos) pos))

; mark-column : mark -> integer
(define (mark-column m)
  (define i   (position m))
  (define im  (text-positions (buffer-text (mark-buffer m))))
  (define-values (start end d) (interval-map-ref/bounds im i #f))
  (and d
       (- i start)))

; mark-compare : mark mark comparator -> boolean
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
  (define m (mark b link 0 name fixed? active?))
  (set-linked-line-marks! link (set-add (linked-line-marks link) m))
  (mark-move-to-position! m pos)
  m)

; copy-mark : mark -> mark
(define (copy-mark m)
  (match m
    [(mark buffer link position name fixed? active?)
     (mark buffer link position name fixed? active?)]))

; mark-deactivate! mark -> void
(define (mark-deactivate! m)
  (set-mark-active?! m #f))

; mark-activate! mark -> void
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
  (define b    (mark-buffer m))
  (define t    (buffer-text b))
  (define im   (text-positions t))
  (define p    (position m))
  (define link (mark-link m))
  (define l    (dfirst link))
  (define ln   (line-length l))
  ; new position
  (define q (if (> n 0)
                (min (+ p n) (max 0 (- (buffer-length b) 1)))
                (max (+ p n) 0)))
  (set-mark-position! m q)
  (define new-link (interval-map-ref im q))
  (unless (eq? link new-link) ; same line?
    ; remove mark from old line    
    (set-linked-line-marks! link (set-remove (linked-line-marks link) m))
    ; insert mark in new line
    ; the mark must point to the new line
    (set-mark-link! m new-link)
    ; (displayln new-link)
    (set-linked-line-marks! new-link (set-add (linked-line-marks new-link) m))))

; mark-adjust-insertion-after! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were inserted at position p
(define (mark-adjust-insertion-after! m p a)
  (define mp (mark-position m))
  (when (>= mp p)
    ; the insertion was after the mark
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
;   move mark to column n (stay at text line)
(define (mark-move-to-column! m n)
  (define c   (mark-column m))
  (define len (line-length (mark-line m)))
  (unless (= n c)
    (let ([n (clamp 0 n (- len 1))]) ; stay on same line
      (set-mark-position! m (+ (mark-position m) (- n c))))))

; mark-on-last-line? : mark -> boolean
;    is m on the last line of its buffer?
(define (mark-on-last-line? m) ; todo (see mark-on-first-line?)
  (last-dcons? (mark-link m)))

; mark-on-first-line? : mark -> boolean
;    is m on the first line of its buffer?
(define (mark-on-first-line? m)
  (first-dcons? (mark-link m)))

; mark-move-beginning-of-line! : mark -> void
;   move the mark to the beginning of its line
(define (mark-move-beginning-of-line! m)
  (define p (mark-position m))
  (define col (mark-column m))
  (when (and p col)
    (set-mark-position! m (- p col))))

; position-of-end-of-line : [buffer or mark] -> integer
;   return the position just before the newline of the line of point
(define (position-of-end-of-line [b-or-m (current-buffer)])
  (define m (cond
              [(mark?   b-or-m) b-or-m]
              [(buffer? b-or-m) (buffer-point b-or-m)]
              [else (error 'position-of-end-line (~a "expected mark or buffer, got " b-or-m))]))
  (define b (mark-buffer m))
  (define c (mark-column m))
  (define p (mark-position m))
  (define l (dfirst (interval-map-ref (text-positions (buffer-text b)) p)))
  (define n (line-length l))
  (+ p (- n c) -1))

; position-of-beginning-of-line : [buffer or mark] -> integer
;   return the position of the beginning of the line
(define (position-of-beginning-of-line [b-or-m (current-buffer)])
  (define m (cond
              [(mark?   b-or-m) b-or-m]
              [(buffer? b-or-m) (buffer-point b-or-m)]
              [else (error 'position-of-end-line (~a "expected mark or buffer, got " b-or-m))]))
  (define c (mark-column m))
  (define p (mark-position m))
  (- p c))


(define (position-of-end [b (current-buffer)])
  (text-length (buffer-text b)))

(define (mark-move-end-of-buffer! m)
  ; remove mark from current line
  (define d (mark-link m))
  (remove-mark-from-linked-line! m d)
  ; add mark to last line
  (define d+ (last-dcons d))
  (add-mark-to-linked-line! m d+)
  (set-mark-link! m d+)
  ; set the position of the end 
  (define b (mark-buffer m))
  (set-mark-position! m (- (position-of-end b) 1)))

(define (mark-move-beginning-of-buffer! m)
  ; remove mark from current line
  (define d (mark-link m))
  (remove-mark-from-linked-line! m d)
  ; add mark to first line
  (define d- (first-dcons d))
  (add-mark-to-linked-line! m d-)
  (set-mark-link! m d-)
  ; set the position of the start
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
    (define p    (mark-position m))
    (when p
      (define col  (mark-column m))
      (define im   (text-positions (buffer-text (mark-buffer m))))
      (define old-link (interval-map-ref im p #f))
      (when old-link
        (unless (mark-on-first-line? m)
          (define link (dprev old-link))
          (define l (dfirst link)) ; line
          (define new-col (min (line-length l) col))
          (define new-pos (- p col (line-length l) (- new-col)))
          (set-mark-position! m new-pos)
          (set-linked-line-marks! link (set-add (linked-line-marks link) m))
          (set-mark-link! m link)
          (unless (dempty? old-link)
            (set-linked-line-marks! old-link (set-remove (linked-line-marks link) m)))))))
  (cond
    [(< n 0) (mark-move-down! m (- n))]
    [else    (for ([i (in-range n)])
               (move-one!))]))

; mark-move-down! : mark -> void
;  move mark down one text line, stay at same column
(define (mark-move-down! m [n 1])
  (define (move-one!)
    (define p   (mark-position m))
    (define col (mark-column m))
    (define t   (buffer-text (mark-buffer m)))
    (define im  (text-positions t))
    (define d   (interval-map-ref im p))
    (unless (mark-on-last-line? m)
      (unless (dempty? d)
        (set-linked-line-marks! d (set-remove (linked-line-marks d) m))
        (define l1 (dfirst d))
        (define l2 (dlist-ref d 1))
        (define new-col (min (line-length l2) col))
        (define new-pos (+ p (- (line-length l1) col) new-col))
        (set-mark-position! m new-pos)
        (define d+ (dlist-move d 1))
        (set-linked-line-marks! d+ (set-add (linked-line-marks d+) m))
        (set-mark-link! m d+))))
  (cond
    [(< n 0) (mark-move-up! m (- n))]
    [else    (for ([i (in-range n)])
               (move-one!))]))

; mark-backward-word! : mark -> void
;   move mark backward until a word separator is found
(define (mark-backward-word! m)
  (define col (mark-column m))
  (define t   (buffer-text (mark-buffer m)))
  (define l   (dfirst (interval-map-ref (text-positions t) (position m))))
  ; first skip whitespace
  (define i (for/first ([i (in-range (- col 1) -1 -1)]
                        #:when (not (word-separator? (line-ref l i))))
              i))
  (cond
    [(or (not i) (= i 0))
     ; continue searching for word at previous line (unless at top line)
     (mark-move-beginning-of-line! m)
     (unless (mark-on-first-line? m)
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
  (define col (mark-column m))  
  (define t (buffer-text (mark-buffer m)))
  (define l (dfirst (interval-map-ref (text-positions t) (position m))))
  (define n (line-length l))
  ; first skip whitespace
  (define i (for/first ([i (in-range col n)]
                        #:when (not (word-separator? (line-ref l i))))
              i))
  (cond
    [(or (not i) (= i (- n 1)))
     ; continue searching for word at next line (unless at bottom line)
     (mark-move-end-of-line! m)
     (unless (mark-on-last-line? m)
       (mark-move! m 1)
       (mark-forward-word! m))]
    [else
     ; we have found a word, find the beginning
     (define j (for/first ([j (in-range (or i (- col 1)) n)]
                           #:when (word-separator? (line-ref l j)))
                 j))
     ; j is now the index of the first word separator
     (mark-move! m (if j (- j col) col))]))

(define (remove-mark-from-linked-line! m link)
  (define l link)
  (set-linked-line-marks! l (set-remove (linked-line-marks l) m)))

(define (add-mark-to-linked-line! m link)
  (define d link)
  (set-linked-line-marks! d (set-add (linked-line-marks d) m)))

(define (mark-move-to-position! m n)
  ; remove mark from its current line
  (define l (mark-link m))
  (remove-mark-from-linked-line! m l)
  ; find the new line
  (define t (buffer-text (mark-buffer m)))
  (define d (interval-map-ref (text-positions t) (mark-position m) #f))
  (unless d ; for debug - remove when cause of error found
    (displayln (~a (list 'mark-move-to-position (mark-name m)
                         'text-length (text-length t)
                         'n n))
               (current-error-port))
    (set! d (interval-map-ref (text-positions t) (- (text-length t) 1))))
  ; add mark to the new line
  (add-mark-to-linked-line! m d)  
  (set-mark-link! m d)
  ; store the new position
  (set-mark-position! m n))

(define (mark-move-to-row+column! m r c)
  ; get the number of lines in the text
  (define b  (mark-buffer m))
  (define t  (buffer-text b))
  (define nt (text-num-lines t))
  ; remove mark from its current line
  (define l (mark-link m))
  (remove-mark-from-linked-line! m l)
  ; find the new position
  (define n (row+column->position t r c))
  (define end? (>= r nt))
  (define row (if end? (- nt 1) r))
  (define d (dlist-move (text-lines (buffer-text (mark-buffer m))) row))
  ; add mark to the new line
  (add-mark-to-linked-line! m d)  
  (set-mark-link! m d)
  ; store the new position
  (set-mark-position! m (if end? (- n 1) n)))


(define (mark-move-to-beginning-of-paragraph! m)
  ;; TODO : improve efficiency (i.e. avoid call to mark-move-up!)
  ;; paragraphs are separated by one or more blank lines
  (mark-move-beginning-of-line! m)
  (let loop ([l (mark-link m)])
    (cond
      [(first-dcons? l)         (void)]
      [(line-blank? (dfirst l)) (mark-move-down! m)]
      [else                     (mark-move-up! m)
                                (loop (mark-link m))]))) ; TODO does up and down update the link ?!!>?

; mark-line : mark -> line
(define (mark-line m)
  (define l (mark-link m))
  (dfirst l))
