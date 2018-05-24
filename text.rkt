#lang racket
(provide new-text                    ; -> text                 create an empty text
         text-append!                ; text text -> text       append the texts
         text-break-line!            ; text nat nat -> void    break at (row, col)
         text-delete-backward-char!
         text-insert-char-at-mark!
         text-insert-string-at-mark!
         text-line                   ; text integer -> line    the i'th line
         text-num-lines
         text-stats         
         text->string
         subtext->string
         path->text)

(require "representation.rkt"
         "line.rkt"
         "dlist.rkt")

(define beep void)

;;;
;;; TEXT
;;;

; new-text : -> text
;   create an empty text
(define (new-text [lines dempty])
  (cond 
    [(dempty? lines) (text (linked-line (new-line "\n") dempty dempty "no-version-yet" '()) 1)]
    [else            (text lines (for/sum ([l lines])
                                   (line-length l)))]))

; text-line : text integer -> line
;   the the ith line
(define (text-line t i)
  (dlist-ref (text-lines t) i))

; text-append! : text text -> text
(define (text-append! t1 t2)
  (text (dappend! (text-lines t1) (text-lines t2))
        (+ (text-length t1) (text-length t2))))

; text->string : text -> string
;   convert the text to a string
(define (text->string t)
  (apply string-append
    (for/list ([l (text-lines t)])
      (line->string l))))

; subtext->string : text integer integer -> string
(define (subtext->string t p1 p2)
  (define-values (r1 c1) (position-row+column t p1))
  (define-values (r2 c2) (position-row+column t p2))
  (define n (- r2 r1))
  (cond
    [(= r1 r2) (substring (line->string (text-line t r1)) c1 c2)]
    [else      (string-append*
                (for/list ([d (in-dlist (dlist-move (text-lines t) r1))]
                           [i (in-range (+ n 1))])
                  (define s (line->string d))
                  (cond
                    [(= i 0) (substring s c1 (string-length s))]
                    [(= i n) (substring s 0 c2)]
                    [else    s])))]))

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

(define (text-insert-char-at-mark! t m b c)
  (define-values (row col) (mark-row+column m))
  (define l (dlist-ref (text-lines t) row))
  (line-insert-char! l c col)
  (set-text-length! t (+ (text-length t) 1)))

(define (text-insert-string-at-mark! t m b s)
  (when (string-contains? s "\n")
    (error 'text-insert-string-at-mark! "got string containing newline, ~a" s))
  ; note: we assume there is no newlines in s
  (define-values (row col) (mark-row+column m))
  (define l (dlist-ref (text-lines t) row))
  (line-insert-char! l s col)
  (set-text-length! t (+ (text-length t) (string-length s))))


; text-break-line! : text natural natural -> void
;   break line number row into two at index col
(define (text-break-line! t row col)
  (define d (dlist-move (text-lines t) row))
  (define l (dfirst d))
  (define-values (pre post) (line-split l col)) ; xxx
  (set-dcons-a! d pre)
  (dinsert-after! d post (λ (a p n) (linked-line a p n #f (seteq))))
  (set-text-length! t (+ 1 (text-length t))))

; text-delete-backward-char! : text natural natural -> void
;   delete the char at line row before column col
(define (text-delete-backward-char! t row col)
  (define d (dlist-move (text-lines t) row))
  (define l (dfirst d))
  (define n (text-length t))
  (cond
    [(> col 0) (line-delete-backward-char! l col)
               (set-text-length! t (- n 1))]
    [(and (= col 0) (= row 0))
     (beep "Beginning of buffer")]
    [(= col 0) 
     ; we need to append this line to the previous
     (define p (dcons-p d))
     (define pl (dfirst p))
     (set-dcons-a! p (line-append pl l))
     (dcons-remove! d)
     (set-text-length! t (- n 1))]
    [else      ; 
     (error 'todo)]))

