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

(require (for-syntax racket/base syntax/parse)
         racket/set racket/match racket/format
         data/interval-map
         "representation.rkt"
         "parameters.rkt"
         "dlist.rkt"
         "line.rkt"
         "text.rkt")

;;;
;;; Representation
;;;

; See "representation.rkt"

; (struct mark (buffer link position name fixed? active?) #:transparent #:mutable)
;   A mark rembers a position in the text of a buffer.
;   The mark stores the link (linked-line) which hold the line.
;   The mark name can be used to refer to the mark.
;     buffer = #f  buffer points no where - only position is valid
;     buffer =     the mark belongs to this buffer
;     link   =     valid if mark belongs to a buffer 
;     fixed? = #f  A normal mark moves when insertions are made to the buffer.
;     fixed? = #t  A fixed-mark remain in place.
;     active? =#t  text between mark and point is an active region 
;     active? =#f  region is non-active (will not be highlighted)


;;;
;;; Invariant
;;;

; When the mark belongs to a buffer, the position of the mark
; must point to a position in the line given by the link.

(define (check-mark m [who #f])
  (when (and m (mark-buffer m))
    (unless (mark-link m)
      (error 'check-mark "mark has buffer, but no link, ~a" m))
    ; find line using the link
    (define l1   (mark-line m))  ; = (dfirst (mark-link m))
    ; find line using the position
    (define i   (position m))
    (define im  (text-positions (buffer-text (mark-buffer m))))
    (define-values (start end d) (interval-map-ref/bounds im i #f))
    (unless d
      (displayln (list 'who who))
      (error 'check-mark "no link found in text-positions: ~a"
             m))
    (define l2  (and d (dfirst d)))
    ; check that l1 and l2 are the same line
    (unless (and d (eq? l1 l2))
      (define msg (~a "internal error: " (mark-name m)
                      " pos: " (mark-position m)
                      " line: " start " " end
                      " line-length: " (line-length l1)))
      (newline     (current-error-port))
      (displayln m (current-error-port))
      (if who
          (raise-syntax-error 'check-mark msg who)
          (error              'check-mark msg)))))

#;(define-syntax (check-mark stx)
  (syntax-parse stx
    [(_check-mark m)
     (syntax/loc stx
       (do-check-mark m #'_check-mark))]
    [(_check-mark m who)
     (syntax/loc stx
       (do-check-mark m #'who))]))

(define (position pos)
  (if (mark? pos) (mark-position pos) pos))

;;;
;;; LINES
;;;

; buffer-line-span : buffer position -> integer integer
;   Return start and end positions of the current line.
;   Restrictions due to narrowing are respected.
(define (buffer-line-span b p)
  (define i   (position p))
  (define im  (text-positions (buffer-text b)))
  (define-values (start end d) (interval-map-ref/bounds im i #f))
  (cond
    [(buffer-restricted? b) (values (max (position (buffer-restriction-start b)) start)
                                    (min (position (buffer-restriction-end   b)) end))]
    [else                   (values start end)]))

(define (mark-line-span m)
  (define who 'mark-line-span)
  (define b (mark-buffer m))
  (unless (buffer? b) (error who (~a "given mark has no buffer" m)))
  (buffer-line-span b m))


;;;
;;; WORDS
;;;

(define (word-separator? c)
  (char-whitespace? c))


;;;
;;; MARKS
;;;


; mark-column : mark -> integer
;   Return the column of the mark relative to the line of which the mark belongs.
(define (mark-column m)
  (unless (mark-buffer m)
    (error 'mark-column "the mark does not belong to a buffer, ~a" m))
  (check-mark m)
  ; Get the start position of the line from the interval map
  (define i   (position m))
  (define im  (text-positions (buffer-text (mark-buffer m))))
  (define-values (start end d) (interval-map-ref/bounds im i #f))
  ; The column is the difference between the mark postion and the start position.
  (define c (and d (- i start)))
  ; Sanity check: a column number must be smaller than the line length.
  (when (and c (> c (line-length (dfirst d))))
    (error 'mark-column "internal error"))
  ; Done
  c)


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
(define (new-mark b name [pos 0] [fixed? #f] #:active? [active? #f] #:insertion-type [type #t])
  ; Make a mark with position 0 and link to the first line
  (define link (text-lines (buffer-text b)))
  (define m (mark b link 0 name fixed? active? type))
  (set-linked-line-marks! link (set-add (linked-line-marks link) m))
  (set-buffer-marks! b (append (buffer-marks b) (list m)))
  ; Move the mark to the given position.
  (mark-move-to-position! m pos)  
  m)

; copy-mark : mark -> mark
(define (copy-mark m)
  (check-mark m)
  (match m
    [(mark buffer link position name fixed? active? insertion-type)
     (mark buffer link position name fixed? active? insertion-type)]))

; mark-deactivate! mark -> void
(define (mark-deactivate! m)
  (check-mark m)
  (set-mark-active?! m #f))

; mark-activate! mark -> void
(define (mark-activate! m)
  (check-mark m)
  (set-mark-active?! m #t))

(define (remove-mark-from-linked-line! m link)
  (define l link)
  (set-linked-line-marks! l (set-remove (linked-line-marks l) m)))

(define (add-mark-to-linked-line! m link)
  (define d link)
  (set-linked-line-marks! d (set-add (linked-line-marks d) m)))

; delete-mark! : mark -> void
;   remove the mark from the line it belongs to
(define (delete-mark! m)
  (unless (mark-buffer m)
    (error 'delete-mark! "the mark does not belong to a buffer, ~a" m))
  (check-mark m)
  ; remove mark from line
  (define link (mark-link m))
  (define b    (mark-buffer m))
  (remove-mark-from-linked-line! m link)  
  ; remove mark from buffer
  (set-buffer-marks! b (filter (λ(x) (not (eq? x m))) (buffer-marks b)))
  (set-mark-buffer! m #f)
  (set-mark-link! m #f))

; mark-move! : mark integer -> void
;  move the mark n characters forward
(define (mark-move! m n)
  (check-mark m)
  (cond
    ; The mark does not belong to a buffer
    [(not (mark-buffer m))
     (set-mark-position! m (+ (mark-position m)))]
    ; The mark does belong to a buffer
    [else     
     (define b    (mark-buffer m))
     (define t    (buffer-text b))
     (define im   (text-positions t))
     (define p    (position m))
     (define link (mark-link m))
     ; find and set new position
     (define q (if (> n 0)
                   (min (+ p n) (max 0 (- (buffer-length b) 1)))
                   (max (+ p n) 0)))
     (set-mark-position! m q)
     ; find and set new link
     (define new-link (interval-map-ref im q))  
     (unless (eq? link new-link) ; same line?
       ; remove mark from old line
       (remove-mark-from-linked-line! m link)       
       ; insert mark in new line
       ; the mark must point to the new line
       (set-mark-link! m new-link)
       ; (displayln new-link)
       (add-mark-to-linked-line! m new-link))
     (void)]))

; mark-adjust! : mark integer -> void
;   add the amount a to the marks position,
;   update the link 
(define (mark-adjust! m a)
  ; Note: This function is used when the surrounding lines have changed.
  ;       It is not possible to use the link. Use the interval map instead.
  ; Note: Don't use check-mark to test here.
  (define b (mark-buffer m))
  (define p (position m))
  (define i (+ p a))   ; new pos
  (cond
    ; mark does not belong to a buffer:
    [(not b) (set-mark-position! m i)]
    ; mark belongs to a buffer:
    [else    (define l      (mark-link m))     ; old link
             (define t      (buffer-text b))
             (define im     (text-positions t))                           
             (define-values (start end d) (interval-map-ref/bounds im i #f))
             (cond
               [(eq? l d) (set-mark-position! m i)]              ; same line 
               [else      (remove-mark-from-linked-line! m l) 
                          (set-mark-link!     m d)
                          (set-mark-position! m i)])])
  (check-mark m))

(define (mark-adjust-due-to-insertion! m p a)
  ; adjust the position of the mark m - an amount of a characters were inserted at position p
  (define mp (mark-position m))
  (cond
    [(= p mp) (when (mark-insertion-type m) ; #t = means move marker
                (mark-adjust! m a))]
    [(< p mp) (mark-adjust! m a)]
    [else     (void)]))

; mark-adjust-insertion-after! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were inserted at position p
(define (mark-adjust-insertion-after! m p a)
  (define mp (mark-position m))
  (when (>= mp p)
    ; the insertion was after the mark
    (mark-adjust! m a)))

; mark-adjust-insertion-before! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were inserted at position p
(define (mark-adjust-insertion-before! m p a)
  (define mp (mark-position m))
  (when (>= mp p)
    ; the insertion was before the mark
    (mark-adjust! m a)))

; mark-adjust-deletion-before! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were deleted before position p
(define (mark-adjust-deletion-before! m p a)
  (define mp (mark-position m))
  (cond 
    ; the entire deletion was before the mark
    [(<= p mp)      (mark-adjust! m (- a))]
    ; the entire deletion was after the mark
    [(< mp (- p a)) (void)]
    ; overlap
    [else           (mark-adjust! m (- a (- p mp)))]))

; mark-adjust-deletion-after! : mark integer natural -> void
;   adjust the position of the mark - an amount of a characters were after before position p
(define (mark-adjust-deletion-after! m p a)
  (define mp (mark-position m))
  (cond 
    ; the entire deletion was after the mark
    [(<= mp p)       (void)]
    ; the entire deletion was before the mark
    [(<= (+ p a) mp) (mark-adjust! m (- a))]
    ; overlap
    [else            (mark-adjust! m (- mp p))]))

; clamp : number number number -> number
;   if minimum <= x <= maximum, return x
;   if x < minimum, return minimum
;   if x > maximum, return maximum
(define (clamp minimum x maximum)
  (max minimum (min x maximum)))

  


; mark-move-to-column! : mark integer -> void
;   move mark to column n (stay at text line)
(define (mark-move-to-column! m n)
  (unless (mark-buffer m)
    (error 'mark-move-to-column! "mark does not belong to a buffer: ~a" m))
  (check-mark m)
  (define c   (mark-column m))
  (define len (line-length (mark-line m)))
  (unless (< c len)
    (error 'mark-move-to-column "internal error"))
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

; mark-move-to-beginning-of-line! : mark -> void
;   move the mark to the beginning of its line
(define (mark-move-to-beginning-of-line! m)
  (unless (mark-buffer m)
    (error 'mark-move-beginning-of-line! "mark does not belong to a buffer: ~a" m))
  (check-mark m)
  (define p   (mark-position m))
  (define col (mark-column m))
  (when (and p col)
    (set-mark-position! m (- p col))))

; line-end-position : mark -> integer
;   return the position just before the newline of the line of mark
;   the position returned is within any restriction
(define (line-end-position [m #f])
  (define who 'line-end-position)
  (set! m (or m (buffer-point (current-buffer))))
  (define b (mark-buffer m))
  (unless (buffer? b) (error who "given mark has no buffer: ~a" m))
  (define p (mark-position m))
  (define-values (start end d) (interval-map-ref/bounds (text-positions (buffer-text b)) p))
  (define pos (max 0 (- end 1)))
  (if (buffer-restricted? b)
      (clamp (position (buffer-restriction-start b))
             pos
             (position (buffer-restriction-end b)))
      pos))

; line-beginning-position : mark -> integer
;   return the position of the beginning of the line of the mark
(define (line-beginning-position [m #f])
  (define who 'line-beginning-position)
  (set! m (or m (buffer-point (current-buffer))))
  (unless (mark? m) (error who (~a "expected mark, got " m)))
  (define b (mark-buffer m))
  (unless (buffer? b) (error who "the given mark does not belong to a buffer: ~a" m))
  (define p (mark-position m))
  (define-values (start end d) (interval-map-ref/bounds (text-positions (buffer-text b)) p))
  (if (buffer-restricted? b)
      (clamp (position (buffer-restriction-start b))
             start
             (position (buffer-restriction-end b)))
      start))

(define (position-of-end [b (current-buffer)])
  (text-length (buffer-text b)))


(define (mark-move-to-end-of-buffer! m)
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

(define (mark-move-to-beginning-of-buffer! m)
  ; remove mark from current line
  (define d (mark-link m))
  (remove-mark-from-linked-line! m d)
  ; add mark to first line
  (define d- (first-dcons d))
  (add-mark-to-linked-line! m d-)
  (set-mark-link! m d-)
  ; set the position of the start
  (set-mark-position! m 0))


; mark-move-to-end-of-line! : mark -> void
;   move the mark to the end of its line
(define (mark-move-to-end-of-line! m)
  (check-mark m)
  (set-mark-position! m (line-end-position m))
  (check-mark m))

; mark-move-up! : mark -> void
;   move mark up one line
(define (mark-move-up! m [n 1])
  (check-mark m)
  ; todo : go from mark to line rather than use dlist-move
  (define (move-one!)
    (check-mark m 'move-one-up!-start)
    (define p (mark-position m))
    (unless p (error 'mark-move-up))
    (when p
      (define col  (mark-column m))
      (define im   (text-positions (buffer-text (mark-buffer m))))
      (define old-link (interval-map-ref im p #f))
      (define-values (start end d) (interval-map-ref/bounds im p))
      (unless (mark-on-first-line? m)
        (define-values (start- end- d-) (interval-map-ref/bounds im (- start 1)))
        (define len  (- end  start))         ; length of current line
        (define len- (- end- start-))        ; length of previous line
        (define new-col (min len- col))      ; stay at current column if possible
        (define new-pos (+ start- new-col))
        (mark-move-to-position! m new-pos))
      
      #;(when old-link
        (unless (mark-on-first-line? m)
          (define link (dprev old-link))
          (define l (dfirst link)) ; line
          (define new-col (min (line-length l) col))
          (define new-pos (- p col (line-length l) (- new-col)))
          (set-mark-position! m new-pos)
          (set-linked-line-marks! link (set-add (linked-line-marks link) m))
          (set-mark-link! m link)
          (unless (dempty? old-link)
            (set-linked-line-marks! old-link (set-remove (linked-line-marks link) m)))
          (check-mark m 'move-one-up!-end)))))
  (cond
    [(< n 0) (mark-move-down! m (- n))]
    [else    (for ([i (in-range n)])
               (move-one!))]))

; mark-move-down! : mark -> void
;  move mark down one text line, stay at same column
(define (mark-move-down! m [n 1])
  (check-mark m #'here0)
  (define (move-one!)
    (check-mark m #'here2)
    (define p   (mark-position m))
    (define col (mark-column m))
    (define t   (buffer-text (mark-buffer m)))
    (define im  (text-positions t))
    (define-values (start end d) (interval-map-ref/bounds im p))
    (unless (mark-on-last-line? m)
      (define-values (start+ end+ d+) (interval-map-ref/bounds im end))
      (define len  (- end  start))         ; length of current line
      (define len+ (- end+ start+))        ; length of next line
      (define new-col (min len+ col))      ; stay at current column if possible
      (define new-pos (+ start+ new-col))
      (mark-move-to-position! m new-pos)
      (check-mark m #'here3))
    (check-mark m #'here4))
  (begin0
    (cond
      [(< n 0) (mark-move-up! m (- n))]
      [else    (for ([i (in-range n)])
                 (move-one!))])
    (check-mark m #'here1)))

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
     (mark-move-to-beginning-of-line! m)
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
     (mark-move-to-end-of-line! m)
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


(define (mark-move-to-position! m n)
  (define who 'mark-move-to-position!)
  (unless (and (integer? n) (>= n 0))
    (error who (~a "expected index, got " n)))
  (unless (mark? m)
    (error who (~a "expected mark, got " m)))
  (check-mark m)
  (define b (mark-buffer m))
  (cond
    [(not b) ; if the mark does not belong to a buffer, ignore the link
     (set-mark-position! m n)]
    [else
     (unless (<= n (max 0 (- (buffer-length b) 1)))
       (displayln "\nmark-move-to-position! error: attempt to move mark beyond end")
       (displayln "--- mark:" (current-error-port))
       (println m (current-error-port))
       (displayln "--- position:" (current-error-port))
       (display "n: " (current-error-port))
       (println n (current-error-port))
       (error 'mark-move-to-position "attempt to move mark beyond end"))
     ; find the new line
     (define t (buffer-text (mark-buffer m)))
     (define d (interval-map-ref (text-positions t) n 'huh))
     ; remove mark from its current line
     (define l (mark-link m))
     (remove-mark-from-linked-line! m l)
     ; add mark to the new line
     ; (todo check that the position n is legal in the first place ...)
     (when (eq? d 'huh)
       (displayln "---")
       (println m)
       (displayln "---")
       (error))
     (unless (linked-line? d)
       (displayln (list mark-move-to-position! n (text-positions t)) (current-error-port)))
     (add-mark-to-linked-line! m d)  
     (set-mark-link! m d)
     ; store the new position
     (set-mark-position! m n)])
  (check-mark m))

(define (mark-move-to-row+column! m r c)
  (unless (mark-buffer m)
    (error 'mark-move-to-row+column!
           "the given mark does not belong to a buffer: ~a" m))
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
  (mark-move-to-beginning-of-line! m)
  (let loop ([l (mark-link m)])
    (cond
      [(first-dcons? l)         (void)]
      [(line-blank? (dfirst l)) (mark-move-down! m)]
      [else                     (mark-move-up! m)
                                (loop (mark-link m))]))) ; TODO does up and down update the link ?!!>?

; mark-line : mark -> line
(define (mark-line m)
  (unless (mark-buffer m)
    (error 'mark-line "given mark does not belong to a buffer: ~a" m))
  (define l (mark-link m))
  (dfirst l))
