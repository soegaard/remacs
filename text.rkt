#lang racket/base
(provide new-text                    ; -> text                create an empty text
         text-append!                ; text text -> text      append the texts
         text-break-line!            ; text pos/mark -> void  break at position
         text-delete-backward-char!  
         text-insert-char-at-mark!
         text-insert-property-at-mark!
         text-insert-string-at-mark!
         text-line                   ; text integer -> line    the i'th line
         text-num-lines
         text-stats         
         text->string
         subtext->string
         path->text)

(require racket/dict racket/string racket/set
         data/interval-map
         "representation.rkt"
         "line.rkt"
         "dlist.rkt")

(define beep void)

;;;
;;; TEXT
;;;

; new-text : -> text
;   create an empty text
(define (new-text [dlines dempty]) ;ok
  (unless (linked-line? dlines)
    (error 'new-text "expected a linked-line, got ~a" dlines))
  (define im   (make-interval-map))
  (cond 
    [(dempty? dlines) (define line (linked-line (new-line "\n") dempty dempty "no-version-yet" '()))
                      (interval-map-set! im 0 1 line)
                      (text line im 1)]
    
    [else             (let loop ([sum 0] [d (first-dcons dlines)])
                        (define l (dfirst d))
                        (define n (line-length l))
                        (define sum+n (+ sum n))
                        (interval-map-set! im sum sum+n d)
                        (if (last-dcons? d)
                            (text dlines im sum+n)
                            (loop sum+n (dnext d))))]))

; text-line : text integer -> line
;   the the ith line
(define (text-line t i)
  (dlist-ref (text-lines t) i))

; text-append! : text text -> text
(define (text-append! t1 t2)
  ; transfer positions from text t2 to text t1
  (define len1 (text-length t1))
  (define pos1 (text-positions t1))
  (for ([(int line) (in-dict (text-positions t2))])
    (define from (car int))
    (define to   (cdr int))
    (interval-map-set! (+ from len1) (+ to len1) line))
  ; 
  (text (dappend! (text-lines t1) (text-lines t2))
        t1 (+ (text-length t1) (text-length t2))))

; text->string : text -> string
;   convert the text to a string
(define (text->string t)
  (string-append*
   (for/list ([l (text-lines t)])
     (line->string l))))

; subtext->string : text integer integer -> string
(define (subtext->string t p1 p2) ; ok
  (set! p1 (if (mark? p1) (mark-position p1) p1))
  (set! p2 (if (mark? p2) (mark-position p2) p2))
  (define im (text-positions t))
  (define-values (s1 e1 d1) (interval-map-ref/bounds im p1))
  (define-values (s2 e2 d2) (interval-map-ref/bounds im p2))
  (define c1 (- p1 s1)) ; column
  (define c2 (- p2 s2))
  (define l1 (dfirst d1))
  (define l2 (dfirst d2))
  (cond
    [(eq? l1 l2) (substring (line->string l1) c1 c2)]
    [else        (string-append*
                  (let loop ([d d1] [strs '()])
                    (define s (line->string (dfirst d)))
                    (cond
                      [(eq? d d1) (loop (dnext d) (cons (substring s c1 (string-length s)) strs))]
                      [(eq? d d2) (reverse (cons (substring s 0 c2) strs))]
                      [else       (loop (dnext d) (cons s strs))])))]))

; path->text : path -> text
;   create a text with contents from the file given by path
(define (path->text path)
  (define (DCons a p n) (linked-line a p n #f (seteq)))
  (with-input-from-file path 
    (λ () (new-text (for/dlist #:dcons DCons ([s (in-lines)])
                      (string->line (string-append s "\n")))))))


; text-num-lines : text -> natural
;   return number of lines in the text
(define (text-num-lines t)
  (dlength (text-lines t)))

(define (text-num-chars t)
  (for/sum ([line (text-lines t)])
    (line-length line)))

(define (text-stats t)
  (define-values (nlines nchars)
    (for/fold ([nl 0] [nc 0]) ([l (text-lines t)])
      (values (+ nl 1) (+ nc (line-length l)))))
  (stats nlines nchars))

(define (interval-map-extend! im p n)
  ; Find the interval i which the position p belongs to.
  ; Extend the interval with n indices.
  ; Increase indices of following intervals.
  (define-values (from to val) (interval-map-ref/bounds im p))
  (interval-map-remove!   im from to)
  (interval-map-contract! im from to)
  (interval-map-expand! im from (+ to n))
  (interval-map-set!    im from (+ to n) val))

(define (interval-map-insert! im from to val)
  (interval-map-expand! im from to)
  (interval-map-set!    im from to val))

(define (text-insert-char-at-mark! t m b c) ; ok
  (define i  (mark-position m))
  (define im (text-positions t))
  (define-values (start end dline) (interval-map-ref/bounds im i))
  (define col (- i start))
  (interval-map-extend! im i 1)
  (line-insert-char! (dfirst dline) c col)
  (set-text-length! t (+ (text-length t) 1)))

(define (text-insert-property-at-mark! t m p)
  (define i  (mark-position m))
  (define im (text-positions t))
  (define-values (start end d) (interval-map-ref/bounds im i))
  (define col (- i start))
  (line-insert-property! (dfirst d) p col))

(define (text-insert-string-at-mark! t m b s) ; ok
  (when (string-contains? s "\n")
    (error 'text-insert-string-at-mark! "got string containing newline, ~a" s))
  ; note: we assume there is no newlines in s
  (define i  (mark-position m))
  (define im (text-positions t))
  (define n  (string-length s))
  (when (> n 0)
    (define-values (start end dline) (interval-map-ref/bounds im i))
    (define l (dfirst dline))
    (define col (- i start))
    (interval-map-extend! im i n)  
    (line-insert-string! l s col)
    (set-text-length! t (+ (text-length t) n))))


; text-break-line! : text position-or-mark -> void
;   break line into two at the given position
(define (text-break-line! t pos) ; ok
  (define i                    (if (mark? pos) (mark-position pos) pos))
  (define im                   (text-positions t))
  (define-values (start end d) (interval-map-ref/bounds im i))
  (define col                  (- i start))
  (define l                    (dfirst d))
  (interval-map-remove!   im start end)
  (interval-map-contract! im start end)
  (define-values (pre post)    (line-split l col))
  (set-dcons-a! d pre)
  (dinsert-after! d post (λ (a p n) (linked-line a p n #f (seteq))))
  (interval-map-insert! im    start        (+ start col 1)         d)
  (interval-map-insert! im (+ start col 1) (+ end 1)        (dnext d))
  (set-text-length! t (+ 1 (text-length t))))

; text-delete-backward-char! : text position -> void
;   delete the char before the given position
(define (text-delete-backward-char! t pos) ; ok
  (define i   (if (mark? pos) (mark-position pos) pos))
  (define im  (text-positions t))
  (define-values (start end d) (interval-map-ref/bounds im i))
  (define col (- i start))
  (define l   (dfirst d))
  (define n   (text-length t))  
  (cond
    [(> col 0) (line-delete-backward-char! l col)
               (interval-map-contract! im i (+ i 1))
               (set-text-length! t (- n 1))]
    [(= i 0)
     (beep "Beginning of buffer")]
    [(= col 0)
     (define-values (pstart pend pd) (interval-map-ref/bounds im (- i 1)))
     ; we need to append this line to the previous
     (define p  (dcons-p d))
     (define pl (dfirst p))
     (set-dcons-a! p (line-append pl l))
     (define affected-marks (linked-line-marks d))
     (for ([m affected-marks])
       (set-mark-link! m p))
     (dcons-remove! d)
     (set-text-length! t (- n 1))
     ; remove previous intervals     
     (interval-map-remove!   im pstart pend)
     (interval-map-remove!   im   start end)
     (interval-map-contract! im pstart end)
     (interval-map-insert!   im pstart (- end 1) pd)]
    [else      ; 
     (error 'todo)]))
