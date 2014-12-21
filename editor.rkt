#lang racket
(module+ test (require rackunit))
(require "dlist.rkt")

;;;
;;; REPRESENTATION
;;;

(struct line (strings length) #:transparent #:mutable)

(struct linked-line dcons (version marks) #:transparent #:mutable)
; the element of a linked-line is a line struct
; marks is a list of marks located on the line
; version will be used for the redisplay code

; properties are copied as part of the text
; overlays are not copied - they are specifically not part of the text
; (struct overlay  (specification) #:transparent)
; (struct property (specification) #:transparent)

(struct text (lines length) #:transparent #:mutable)
; A text being edited is represented as a doubly linked list of lines.

(struct stats (num-lines num-chars))
; The number of lines and number of characters in a text.

(struct buffer (text name path points marks modes cur-line num-chars num-lines modified?)
  #:transparent #:mutable)
; A buffer is the basic unit of text being edited.
; It contains a text being edited.
; The buffer has a name, so the user can refer to the buffer.
; The buffer might have an associated file:
;   path = #f     <=>  buffer is not associated with a file
;   path = path   <=>  reads and writes to the file given by the path
; A point is a position between two characters. 
; Insertions and deletion will happen at the points (usually only one).
; If modified? is true, then the buffer has been modied since the last
; read or save of the file.
; The list modes contains the active modes (see below).
; A buffer can have multiple marks:

(struct mark (buffer position name fixed?) #:transparent #:mutable)
; A mark rembers a position in the text of a buffer
; The mark name can be used to refer to the mark.
; fixed? = #f  A normal mark moves when insertions are made to the buffer.
; fixed? = #t  A fixed-mark remain in place.

; If there is exactly one mark, the area between the point and the mark
; is called a region.

(struct mode (name) #:transparent)
; A mode has a name (displayed in the status bar).

;;;
;;; LINES
;;;

(define (line->string l)
  (apply string-append (line-strings l)))

; string->line : string -> line
(define (string->line s)
  (line (list s) (string-length s)))

(define (new-line s)
  (string->line s))

; list->lines : list-of-strings -> dlist-of-lines
(define (list->lines xs)
  (define (string->line s) (new-line s))
  (define (recur p xs)
    (cond 
      [(null? xs) dempty]
      [else       (define s (car xs))
                  (define l (new-line s))
                  (define d (linked-line l p #f #f '()))
                  (define n (recur d (cdr xs)))
                  (set-dcons-n! d n)
                  d]))
  (cond 
    [(null? xs)   (dlist (new-line "\n") dempty dempty)]
    [else         (define s (car xs))
                  (define l (new-line s))
                  (define d (linked-line l dempty #f #f '()))
                  (define n (recur d (cdr xs)))
                  (set-dcons-n! d n)
                  d]))

; line-ref : line index -> char
;   return the ith character of a line
(define (line-ref l i)
  (when (>= i (line-length l))
    (error 'line-ref "index ~a to large for line, got: ~a" i l))
  (let loop ([i i] [ss (line-strings l)])
    (define s (first ss))
    (define n (string-length s))
    (if (< i n) 
        (string-ref s i)
        (loop (- i n) (rest ss)))))

(module+ test
  (define illead-text 
    (new-text 
     (list->lines
      (list "Sing, O goddess, the anger of Achilles son of Peleus, that brought\n"
            "countless ills upon the Achaeans. Many a brave soul did it send hurrying\n"
            "down to Hades, and many a hero did it yield a prey to dogs and vultures,\n"
            "for so were the counsels of Jove fulfilled from the day on which the\n"
            "son of Atreus, king of men, and great Achilles, first fell out with\n"
            "one another.\n"))))
  
  ; recreate the same text file from scratch
  (define (create-new-test-file path)
    (with-output-to-file path
      (λ() (for ([line (text-lines illead-text)])
             (for ([s (line-strings line)])
               (display s))))
      #:exists 'replace)))

(define (string-insert-char s i c)
  (define n (string-length s))
  (unless (<= i n) (error 'string-insert-char "index too large, got ~a ~a" s i))
  (cond 
    [(= i n) (string-append s (string s))]
    [(= i 0) (string-append (string c) s)]
    [else    (string-append (substring s 0 i) (string c) (substring s i n))]))

; skip-strings : list-of-strings index -> index strings strings
;   If (j, us, vs) is returned,
;   then ss = (append us vs)
;   and  (string-ref (concat ss) i) = (string-ref (first vs) j)
(define (skip-strings ss i)
  (let loop ([i i] [before '()] [after ss])
    (cond 
      [(null? after) (values #f (reverse before) '())]
      [else          (define s (first after))
                     (define n (string-length s))
                     (if (< i n)
                         (values i (reverse before) after)
                         (loop (- i n) (cons s before) (rest after)))])))

; line-insert-char! : line char index -> void
;   insert char c in the line l at index i
(define (line-insert-char! l c i)
  (define n (line-length l))
  (unless (<= i n) (error 'line-insert-char "index i greater than line length, i=~a, l=~a" i l))
  (define-values (j us vs) (skip-strings (line-strings l) i))
  (define v (first vs))
  (define vn (string-length v))
  (define w (cond 
              [(= j 0)  (string-append (string c) v)]
              [(= j vn) (string-append v (string c))]
              [else     (string-append (substring v 0 j) (string c) (substring v j vn))]))
  (set-line-strings! l (append us (cons w (rest vs))))
  (set-line-length!  l (+ n 1)))

; line-insert-string! : line string index -> void
;   insert string t in the line l at index i
(define (line-insert-string! l t i)
  (define n (line-length l))
  (unless (<= i n) (error 'line-insert-string "index i greater than line length, i=~a, l=~a" i l))
  (define-values (j us vs) (skip-strings (line-strings l) i))
  (define v (first vs))
  (define vn (string-length v))
  (define w (cond 
              [(= j 0)  (string-append t v)]
              [(= j vn) (string-append v t)]
              [else     (string-append (substring v 0 j) t (substring v j vn))]))
  (set-line-strings! l (append us (cons w (rest vs))))
  (set-line-length!  l (+ n (string-length t))))

; line-split : line index -> line line
;   split the line in two at the index
(define (line-split l i)
  (define n (line-length l))
  (unless (<= i n) (error 'line-split "index ~a larger than line length ~a, the line is ~a" i n l))
  (define-values (j us vs) (skip-strings (line-strings l) i))  
  (cond
    [(empty? vs) (values l (line '("\n") 0))]
    [else        (define s (first vs))
                 (define sn (string-length s))
                 (define s1 (substring s 0 j))
                 (define s2 (substring s j sn))
                 (values (line (append us (list (string-append s1 "\n"))) (+ i 1))
                         (line (cons s2 (rest vs)) (- n i)))]))

; line-append : line line -> line
;   append two lines, note the line ending of l1 is removed
(define (line-append l1 l2)
  (define ws (let loop ([us (line-strings l1)])
               ; we must remove the newline of the last string of us
               (if (null? (rest us))
                   (cons (substring (first us) 0 (- (string-length (first us)) 1))
                         (line-strings l2))
                   (cons (first us)
                         (loop (rest us))))))
  (line ws (+ (line-length l1) -1 (line-length l2))))

(module+ test
  (check-equal? (line-append (line '("a" "b\n") 3) (line '("c" "d\n") 3))
                (line '("a" "b" "c" "d\n") 5)))

; line-delete-backward-char! : line -> line
(define (line-delete-backward-char! l i)
  (unless (> i 0) (error 'line-delete-backward-char! "got ~a" i))
  (define-values (j us vs) (skip-strings (line-strings l) (- i 1)))
  (write (list 'back-char l i 'j j 'us us 'vs vs)) (newline)
  (define s (first vs))
  (define n (string-length s))
  (define s1 (substring s 0 j)) 
  (define s2 (substring s (+ j 1) n))
  (define ws (append us (cons (string-append s1 s2) (rest vs))))
  (set-line-strings! l ws)
  (set-line-length! l (- (line-length l) 1)))

(module+ test
  (define del-char line-delete-backward-char!)
  (check-equal? (let ([l (line '("abc\n") 4)]) (del-char l 1) l) (line '("bc\n") 3))
  (check-equal? (let ([l (line '("abc\n") 4)]) (del-char l 2) l) (line '("ac\n") 3))
  (check-equal? (let ([l (line '("abc\n") 4)]) (del-char l 3) l) (line '("ab\n") 3))
  (check-equal? (let ([l (line '("ab" "cd\n") 5)]) (del-char l 1) l) (line '("b" "cd\n") 4))
  (check-equal? (let ([l (line '("ab" "cd\n") 5)]) (del-char l 2) l) (line '("a" "cd\n") 4))
  (check-equal? (let ([l (line '("ab" "cd\n") 5)]) (del-char l 3) l) (line '("ab" "d\n") 4))
  (check-equal? (let ([l (line '("ab" "cd\n") 5)]) (del-char l 4) l) (line '("ab" "c\n") 4)))


;;;
;;; TEXT
;;;

; new-text : -> text
;   create an empty text
(define (new-text [lines dempty])
  (displayln lines)
  (cond 
    [(dempty? lines) (text dempty 0)]
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

; path->text : path -> text
;   create a text with contents from the file given by path
(define (path->text path)
  (with-input-from-file path 
    (λ () (new-text (for/dlist ([s (in-lines)])
                      (string->line (string-append s "\n")))))))

(module+ test
  (void (create-new-test-file "illead.txt"))
  ; (displayln "--- illead test file ---")
  ; (write (path->text "illead.txt")) (newline)
  ; (displayln "---")
  ;(write illead-text) (newline)
  ;(displayln "---")
  #;(check-equal? (path->text "illead.txt") illead-text))

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
  (line-insert-char! l c col))

; text-break-line! : text natural natural -> void
;   break line number row into two at index col
(define (text-break-line! t row col)
  (define d (dlist-move (text-lines t) row))
  (define l (dfirst d))
  (define-values (pre post) (line-split l col))
  (set-dcons-a! d pre)
  (dinsert-after! d post)
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

(define beep void)

;;;
;;; MARKS
;;;

(define (new-mark buffer name [pos 0] [fixed? #f])
  (mark buffer pos name fixed?))

; mark-move! : mark integer -> void
;  move the mark n characters
(define (mark-move! m n)
  (define b (mark-buffer m))
  (define p (mark-position m))
  (define q (if (> n 0)
                (min (+ p n) (max 0 (- (buffer-length b) 1)))
                (max (+ p n) 0)))
  (set-mark-position! m q))

; mark-row+column : mark- > integer integer
;   return row and column number for the mark m
(define (mark-row+column m)
  (define b (mark-buffer m))
  (define p (mark-position m))
  (define-values (row col)
    (let/ec return
      (for/fold ([r 0] [q 0]) ([l (text-lines (buffer-text b))])
        (define n (line-length l))
        (if (> (+ q n) p)
            (return r (- p q))
            (values (+ r 1) (+ q n))))))
  (values row col))

; mark-move-beginning-of-line! : mark -> void
;   move the mark to the begining of its line
(define (mark-move-beginning-of-line! m)
  (define p (mark-position m))
  (define-values (row col) (mark-row+column m))
  (set-mark-position! m (- p col)))

; mark-move-end-of-line! : mark -> void
;   move the mark to the end of its line
(define (mark-move-end-of-line! m)
  (define b (mark-buffer m))
  (define p (mark-position m))
  (define-values (row col) (mark-row+column m))
  (define n (line-length (dlist-ref (text-lines (buffer-text b)) row)))
  (set-mark-position! m (+ p (- n col) -1)))

; mark-move-up! : mark -> void
;   move mark up one line
(define (mark-move-up! m)
  (define p (mark-position m))
  (define-values (row col) (mark-row+column m))
  (unless (= row 0)
    (define l (text-line (buffer-text (mark-buffer m)) (- row 1)))
    (define new-col (min (line-length l) col))
    (define new-pos (- p col (line-length l) (- new-col)))
    (set-mark-position! m new-pos)))

; mark-move-down! : mark -> void
;  move mark down one line
(define (mark-move-down! m)
  (define p (mark-position m))
  (define-values (row col) (mark-row+column m))
  (define t (buffer-text (mark-buffer m)))
  (unless (= (+ row 1) (text-num-lines t))
    (define d (dlist-move (text-lines t) row))
    (define l1 (dfirst d))
    (define l2 (dlist-ref d 1))
    (define new-col (min (line-length l2) col))
    (define new-pos (+ p (- (line-length l1) col) new-col))
    (set-mark-position! m new-pos)))

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

;;;
;;; WORDS
;;;

(define (word-separator? c)
  (char-whitespace? c))

;;;
;;; BUFFER
;;;

(define buffer-counter 0)

(define (default-buffer-name)
  (set! buffer-counter (+ buffer-counter 1))
  (~a "*buffer" buffer-counter "*"))

; new-buffer : -> buffer
;   create fresh buffer without an associated file
(define (new-buffer [text (new-text)] [path #f] [name (default-buffer-name)])
  (define point (new-mark #f "*point*"))
  (define points (list point))
  (define marks '())
  (define modes '())
  (define cur-line 0)
  (define stats (text-stats text))
  (define num-lines (stats-num-lines stats))
  (define num-chars (stats-num-chars stats))
  (define modified? #f)
  (define b (buffer text name path points marks modes cur-line num-chars num-lines modified?))
  (set-mark-buffer! point b)
  b)

; save-buffer : buffer -> void
;   save contents of buffer to associated file
;   do nothing if no file is associated
(define (save-buffer! b)
  (define file (buffer-path b))
  (when file
    (with-output-to-file file
      (λ () (for ([line (text-lines (buffer-text b))])
              (for ([s (line-strings line)])
                (display s))))
      #:exists 'replace)
    (set-buffer-modified?! b #f)))

(module+ test
  (provide illead-buffer)
  (define illead-buffer (new-buffer illead-text "illead.txt" "illead"))
  (save-buffer! illead-buffer)
  (check-equal? (path->text "illead.txt") illead-text))

; read-buffer : buffer -> void
;   replace text of buffer with file contents
(define (read-buffer! b)
  (define path (buffer-path b))
  (unless path (error 'read-buffer "no associated file: ~a" b))
  (define text (path->text path))
  (define stats (text-stats text))
  (set-buffer-text! b text)
  (set-buffer-num-lines! b (stats-num-lines stats))
  (set-buffer-num-chars! b (stats-num-chars stats))
  (set-buffer-modified?! b #f))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define b (new-buffer (new-text) "illead.txt" "illead"))
  (read-buffer! b)
  (check-equal? b illead-buffer))

; append-to-buffer-from-file : buffer path -> void
;   append contents of file given by the path p to the text of the buffer b
(define (append-to-buffer-from-file b p)
  (define text-to-append (path->text p))
  (define stats (text-stats text-to-append))
  (set-buffer-text! b (text-append! (buffer-text b) text-to-append))
  (set-buffer-num-lines! b (+ (buffer-num-lines b) (stats-num-lines stats)))
  (set-buffer-num-chars! b (+ (buffer-num-chars b) (stats-num-chars stats)))
  (set-buffer-modified?! b #t))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define append-buffer (new-buffer (new-text)))
  (append-to-buffer-from-file append-buffer "illead.txt")
  (append-to-buffer-from-file append-buffer "illead.txt")
  (save-buffer! b) ; make sure the buffer is unmodified before comparison
  #;(check-equal? (buffer-text append-buffer) (text-append! illead-text illead-text)))

; buffer-point : buffer -> mark
;   return the first mark in the list of points
(define (buffer-point b)
  (first (buffer-points b)))

; buffer-point-set! : buffer mark -> void
;   set the point at the position given by the mark m
(define (buffer-move-point! b n)
  (mark-move! (buffer-point b) n))

(define (buffer-move-point-up! b)
  (mark-move-up! (buffer-point b)))

(define (buffer-move-point-down! b)
  (mark-move-down! (buffer-point b)))

; buffer-backward-word! : buffer -> void
;   move point forward until a word separator is found
(define (buffer-backward-word! b)
  (mark-backward-word! (buffer-point b)))

; buffer-forward-word! : buffer -> void
;   move point to until it a delimiter is found
(define (buffer-forward-word! b)
  (mark-forward-word! (buffer-point b)))


(define (buffer-display b)
  (define (line-display l)
    ; (write l) (newline)
    (display (~a "|" (regexp-replace #rx"\n$" (line->string l) "") "|\n")))
  (define (text-display t)
    (for ([l (text-lines t)])
      (line-display l)))
  (define (status-display)
    (displayln (~a "--- buffer: " (buffer-name b) "    " (if (buffer-modified? b) "*" "saved") 
                   " ---")))
  (text-display (buffer-text b))
  (status-display))

(module+ test
  #;(buffer-display illead-buffer))

(define (buffer-insert-char! b c)
  (define m (buffer-point b))
  (define t (buffer-text b))
  (text-insert-char-at-mark! t m b c))

; buffer-move-point-to-begining-of-line! : buffer -> void
;   move the point to the beginning of the line
(define (buffer-move-point-to-begining-of-line! b)
  (define m (buffer-point b))
  (mark-move-beginning-of-line! m))

; buffer-move-point-to-end-of-line! : buffer -> void
;   move the point to the end of the line
(define (buffer-move-point-to-end-of-line! b)
  (define m (buffer-point b))
  (mark-move-end-of-line! m))

; buffer-length : buffer -> natural
;   return the total length of the text
(define (buffer-length b)
  (text-length (buffer-text b)))

; buffer-break-line! : buffer -> void
;   break line at point
(define (buffer-break-line! b)
  (define m (buffer-point b))
  (define-values (row col) (mark-row+column m))
  (text-break-line! (buffer-text b) row col)
  (mark-move! m 1))

(define (buffer-delete-backward-char b [count 1])
  ; emacs: delete-backward-char
  (define m (buffer-point b))
  (define t (buffer-text b))
  (define-values (row col) (mark-row+column m))
  (text-delete-backward-char! t row col)
  (mark-move! m -1))

;;;
;;; GUI
;;;

(require racket/gui/base)

(define (new-editor-frame b)
  (define min-width  800)
  (define min-height 800)
  (define text-color (make-object color% "black"))
  (define font-size  16)
  (define font-family 'modern) ; fixed width
  (define fixed-font (make-object font% font-size font-family))
  (define frame (new frame% [label "Editor"]))
  (define msg   (new message% [parent frame] [label "No news"]))
  (send msg min-width min-width)
  (define subeditor-canvas%
    (class canvas%
      (define/override (on-char event)
        (send msg set-label "subeditor-canvas key event")
        (define k (send event get-key-code))
        (define ctrl-down? (send event get-control-down))
        (define alt-down?  (send event get-alt-down))
        (define meta-down? (send event get-meta-down))  ; cmd (OS X)
        ; (displayln k)
        (cond
          [ctrl-down?  ; control + something
           (match k
             [#\a         (buffer-move-point-to-begining-of-line! b)]
             [#\e         (buffer-move-point-to-end-of-line! b)]
             [#\p         (buffer-move-point-up! b)]
             [#\n         (buffer-move-point-down! b)]
             [_           (void)])]
          [meta-down?   ; command + something
           (match k
             ['left       (buffer-backward-word! b)]
             ['right      (buffer-forward-word! b)]
             
             [#\d         (buffer-display b)]
             [#\s         (save-buffer! b)]
             [#\w         (save-buffer! b)      ; todo: ask!
                          (send frame on-exit)] ; DrRacket = Close tab
             [_           (void)])]
          [else
           ; no control
           (match k
             [#\return    (buffer-break-line! b)]
             [#\backspace (buffer-delete-backward-char b 1)] ; the backspace key
             [#\rubout    (error 'todo)]                     ; the delete key
             [(? char? k) (buffer-insert-char! b k)
                          (buffer-move-point! b 1)]
             ['left       (buffer-move-point! b -1)]
             ['right      (buffer-move-point! b  1)]
             ['up         (buffer-move-point-up! b)]
             ['down       (buffer-move-point-down! b)]
             ['home       (buffer-move-point-to-begining-of-line! b)] ; fn+left
             ['end        (buffer-move-point-to-end-of-line! b)]      ; fn+right
             [_           (when (char? k)
                            (display "key not handled: ") (write k) (newline))
                          (void)])])
        (send canvas on-paint))
      (define/override (on-paint)
        (define dc (send canvas get-dc))
        (send msg set-label "on-paint")
        (send dc clear)
        (send dc set-text-foreground text-color)
        (send dc set-font fixed-font)
        ; draw line
        (for/fold ([y 0]) ([l (text-lines (buffer-text b))])
          (send dc draw-text (line->string l) 0 y)
          (+ y font-size 1))
        ; draw points
        (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
        (for ([p (buffer-points b)])
          (define-values (r c) (mark-row+column p))
          (define x (* c font-width))
          (define y (* r (+ font-height -2))) ; why -2 ?
          (send dc draw-line x y x (+ y font-height))))
      (super-new)))
  (define canvas (new subeditor-canvas% [parent frame]))
  (send canvas min-client-width  400)
  (send canvas min-client-height 400)
  (send frame show #t))

(module+ test
  (new-editor-frame illead-buffer))

(define (display-file path)
  (with-input-from-file path
    (λ ()
      (for ([l (in-lines)])
        (displayln l)))))
