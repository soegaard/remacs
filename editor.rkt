#lang racket
(module+ test (require rackunit))
(require "dlist.rkt" (for-syntax syntax/parse))

;;;
;;; REPRESENTATION
;;;

(struct line (strings length) #:transparent #:mutable)
; A line is a list of elements of the types:
;   string        represents actual text
;   property      represents a text property e.g. bold
;   overlay       represents ...

; properties are copied as part of the text
; overlays are not copied - they are specifically not part of the text
(struct overlay  (specification) #:transparent)
(struct property (specification) #:transparent)

(struct linked-line dcons (version marks) #:transparent #:mutable)
; the element of a linked-line is a line struct
; marks is a list of marks located on the line
; version will be used for the redisplay code

(struct text (lines length) #:transparent #:mutable)
; A text being edited is represented as a doubly linked list of lines.

(struct stats (num-lines num-chars) #:transparent)
; The number of lines and number of characters in a text.

(module buffer-struct racket/base
  ; buffer-name and buffer-modified? are extendeded to handle current-buffer later on
  (provide (except-out (struct-out buffer) buffer-name buffer-modified?) 
           (rename-out [buffer-name -buffer-name] [buffer-modified? -buffer-modified?]))
  (struct buffer (text name path points marks modes cur-line num-chars num-lines modified?)
    #:transparent #:mutable))
(require (submod "." buffer-struct))
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

(struct mark (buffer link position name fixed?) #:transparent #:mutable)
; A mark rembers a position in the text of a buffer.
; The mark stores the link (linked-line) which hold the line.
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

; string->line : string -> line
(define (string->line s)
  (line (list s) (string-length s)))

; new-line : string ->list
;   antipating extra options for new-line
(define (new-line s)
  (string->line s))

(module+ test (check-equal? (new-line "abc\n") (line '("abc\n") 4)))

; line->string : line -> string
;   return string contents of a line (i.e. remove properties and overlays)
(define (line->string l)
  (apply string-append (filter string? (line-strings l))))

(module+ test (check-equal? (line->string (new-line "abc\n")) "abc\n"))

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

(module+ test (let ([xs '("ab\n" "cd\n")])
                (check-equal? (for/list ([x (list->lines xs)]) x)
                              (map new-line xs))))


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
                     (cond
                       [(string? s) (define n (string-length s))
                                    (if (< i n)
                                        (values i (reverse before) after)
                                        (loop (- i n) (cons s before) (rest after)))]
                       [else         (loop i (cons s before) (rest after))])])))

; line-insert-char! : line char index -> void
;   insert char c in the line l at index i
(define (line-insert-char! l c i)
  (define n (line-length l))
  (unless (<= i n) (error 'line-insert-char! "index i greater than line length, i=~a, l=~a" i l))
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
  (unless (<= i n) (error 'line-insert-string! "index i greater than line length, i=~a, l=~a" i l))
  (define-values (j us vs) (skip-strings (line-strings l) i))
  (define v (first vs))
  (define vn (string-length v))
  (define w (cond 
              [(= j 0)  (string-append t v)]
              [(= j vn) (string-append v t)]
              [else     (string-append (substring v 0 j) t (substring v j vn))]))
  (set-line-strings! l (append us (cons w (rest vs))))
  (set-line-length!  l (+ n (string-length t))))


; line-insert-property! : line property index -> void
;   insert property p in the line l at index i
(define (line-insert-property! l p i)
  (define n (line-length l))
  (unless (<= i n) (error 'line-insert-property! "index i greater than line length, i=~a, l=~a" i l))
  (define-values (j us vs) (skip-strings (line-strings l) i))
  (define v (first vs))
  (define vn (string-length v))
  (define w (cond 
              [(= j 0)  (cons p (list v))]
              [(= j vn) (append (list v) (list p))]
              [else     (list (substring v 0 j) p (substring v j vn))]))
  (set-line-strings! l (append us w (rest vs)))
  (set-line-length!  l (+ n 1)))

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
  ; (write (list 'back-char l i 'j j 'us us 'vs vs)) (newline)
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
  (cond 
    [(dempty? lines) (text (linked-line (new-line "\n") dempty dempty "no-version-yet" '()) 1)]
    ; linked correctly?  
    ; xxx
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
  (displayln (list 'text-stats t))
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

; new-mark : buffer string integer boolean -> mark
(define (new-mark b name [pos 0] [fixed? #f])
  ; (define link (text-lines (buffer-text b)))
  (define link (text-lines (buffer-text b)))
  (define m (mark b link pos name fixed?))
  ; (set-linked-line-marks! link (cons m (linked-line-marks link)))
  m)

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

; buffer-name : [buffer] -> string
;   return name of buffer
(define (buffer-name [b (current-buffer)]) 
  (-buffer-name b))

; all buffers are registered in buffers-ht
(define buffers-ht (make-hash))  ; string -> buffer

; register-buffer : buffer [thunk-or-#f] -> void
;   associate (buffer-name b) to b in buffers-ht
(define (register-buffer b [on-error #f])
  (define name (buffer-name b))
  (if (hash-ref buffers-ht name #f)
      (cond 
        [on-error (on-error)]
        [else (error 'register-buffer 
                     "attempt to register buffer with name already in use: ~a" name)])
      (hash-set! buffers-ht name b)))

; get-buffer : buffer-or-string -> buffer-or-#f
;   return buffer specified by buffer-or-name
(define (get-buffer buffer-or-name)
  (define b buffer-or-name)
  (if (buffer? b) b (hash-ref buffers-ht b #f)))

; generate-new-buffer-name : string -> string
;   generate buffer name not in use
(define (generate-new-buffer-name starting-name)
  (define (name i) (if (= i 1) starting-name (~a starting-name i)))
  (for/first ([i (in-naturals 1)]
              #:unless (get-buffer (name i)))
    (name i)))

; new-buffer : -> buffer
;   create fresh buffer without an associated file
(define (new-buffer [text (new-text)] [path #f] [name (generate-new-buffer-name "buffer")])
  (define b (buffer text name path 
                    '() ; points
                    '() ; marks
                    '() ; modes 
                    0   ; cur-line
                    0   ; num-chars
                    0   ; num-lines
                    #f))  ; modified?
  (define point (new-mark b "*point*"))
  (define points (list point))
  (set-buffer-points! b points)
  (define stats (text-stats text))
  (define num-lines (stats-num-lines stats))
  (set-buffer-num-lines! b num-lines)
  (define num-chars (stats-num-chars stats))
  (set-buffer-num-chars! b num-chars)
  (register-buffer b)
  b)

; generate-new-buffer : string -> buffer
(define (generate-new-buffer name)
  (unless (string? name) (error 'generate-new-buffer "string expected, got ~a" name))
  (new-buffer (new-text) #f (generate-new-buffer-name name)))

(define current-buffer 
  (make-parameter (new-buffer (new-text (list->lines '("The scratch buffer"))) #f "*scratch*")))

; syntax: (save-current-buffer body ...)
;   store current-buffer while evaluating body ...
;   the return value is the result from the last body
(define-syntax (save-current-buffer stx)
  (syntax-parse stx
    [(s-c-b body ...)
     #'(let ([b (current-buffer)])
         (begin0 (begin body ...)
                 (current-buffer b)))]))

; syntax (with-current-buffer buffer-or-name body ...)
;   use buffer-or-name while evaluating body ...,
;   restore current buffer afterwards
(define-syntax (with-current-buffer stx)
  (syntax-parse stx
    [(w-c-b buffer-or-name body ...)
     #'(parameterize ([current-buffer buffer-or-name])
         ; (todo "lookup buffer if it is a name")
         body ...)]))

; TODO syntax  (with-temp-buffer body ...)

; rename-buffer! : string -> string
(define (rename-buffer! new-name [b (current-buffer)] [unique? #t])
  (unless (string? new-name) (error 'rename-buffer "string expected, got " new-name))
  ; todo: check that buffer-name is not in use, if it is signal error unless unique? is false
  ;       in that case generate new name and return it
  (set-buffer-name! b new-name)
  new-name)

(define (buffer-modified? [b (current-buffer)])
  (-buffer-modified? b))


; set-buffer-modified! : any [buffer] -> void
;   set modified?, redisplay mode line
(define (set-buffer-modified! flag [b (current-buffer)])
  ; TODO (redisplay-mode-line-for-current-buffer]
  (when flag (set-buffer-modified?! b #t)))

; get-buffer-create : buffer-or-name -> buffer
;   get buffer with given name, if none exists, create it
(define (get-buffer-create buffer-or-name)
  (define b buffer-or-name)
  (if (buffer? b) b (generate-new-buffer b)))


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
  (define illead-buffer (new-buffer illead-text "illead.txt" (generate-new-buffer-name "illead")))
  (save-buffer! illead-buffer)
  #;(check-equal? (path->text "illead.txt") illead-text))

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
  (define b (new-buffer (new-text) "illead.txt" (generate-new-buffer-name "illead")))
  (read-buffer! b)
  #;(check-equal? b illead-buffer))

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
    (write l) (newline)
    #;(display (~a "|" (regexp-replace #rx"\n$" (line->string l) "") "|\n")))
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

(define (buffer-insert-property! b p)
  (define m (buffer-point b))
  (define t (buffer-text b))
  (define-values (row col) (mark-row+column m))
  (line-insert-property! (dlist-ref (text-lines t) row) p col))


; buffer-point-marker! : buffer -> mark
;   set new mark at point (i.e. "copy point")
#;(define (buffer-point-marker! b)
    (define p (buffer-point b))
    ...)

;;;
;;; WINDOWS
;;;

; A window is an area of the screen used to display a buffer.
; Windows are grouped into frames.
; Each frame contains at least one window.

(define window-ht (make-hash))

(struct window (buffer))

; get-buffer-window : [buffer-or-name] -> window
;   return first window in which buffer is displayed
(define (get-buffer-window [buffer-or-name (current-buffer)])
  (define b (get-buffer buffer-or-name))
  (for/first ([w window-ht]
              #:when (eq? (get-buffer (buffer-name (window-buffer w))) b))
    w))

; get-buffer-window-list : [buffer-or-name] -> list-of-windows
;   get list of all windows in which buffer is displayed
(define (get-buffer-window-list [buffer-or-name (current-buffer)])
  (define b (get-buffer buffer-or-name))
  (for/list ([w window-ht]
             #:when (eq? (get-buffer (window-buffer w)) b))
    w))


;;;
;;; INTERACTIVE COMMANDS
;;;

;; Names from emacs

(define (beginning-of-line) (buffer-move-point-to-begining-of-line! (current-buffer)))
(define (end-of-line)       (buffer-move-point-to-end-of-line! (current-buffer)))
(define (backward-char)     (buffer-move-point! (current-buffer) -1))
(define (forward-char)      (buffer-move-point! (current-buffer) +1))
(define (previous-line)     (buffer-move-point-up! (current-buffer)))
(define (next-line)         (buffer-move-point-down! (current-buffer)))
(define (backward-word)     (buffer-backward-word! (current-buffer)))
(define (forward-word)      (buffer-forward-word! (current-buffer)))


;;;
;;; KEYMAP
;;;

;;; Keys aka key sequences are (to a first approximation) represented as strings.
;;    a     "a"
;;    2     "2"
;;    X     "X"
;; ctrl-a   "\C-a"
;; meta-a   "\M-a"

(struct keymap (bindings) #:transparent)

(define (key-event->key event)
  (define k     (send event get-key-code))
  (define ctrl? (send event get-control-down))
  (define alt?  (send event get-alt-down))
  (define meta? (send event get-meta-down))
  (cond 
    [(or ctrl? alt? meta?) (~a (if ctrl? "C-" "")
                               (if alt?  "A-" "")
                               (if meta? "M-" "")
                               k)]
    [else                  k]))

(define global-keymap
  (λ (prefix key)
    ; (write key) (newline)
    ; if prefix + key event is bound, return thunk
    ; if prefix + key is a prefix return 'prefix
    (match prefix
      [(list)
       (match key
         ['left       backward-char]
         ['right      forward-char]
         ['up         previous-line]
         ['down       next-line]         
         ; Ctrl + something
         ["C-a"       beginning-of-line]
         ["C-b"       backward-char]
         ["C-e"       end-of-line]
         ["C-f"       forward-char]
         ["C-p"       previous-line]
         ["C-n"       next-line]
         ; Cmd + something
         ["M-left"    backward-word]
         ["M-right"   forward-word]
         ["M-b"       (λ () (buffer-insert-property! (current-buffer) (property 'bold)))]
         ["M-i"       (λ () (buffer-insert-property! (current-buffer) (property 'italics)))]
         ["M-d"       (λ () (buffer-display (current-buffer)))]
         ["M-s"       (λ () (save-buffer! (current-buffer)))]
         ["M-w"       'exit #;(λ () (save-buffer! (current-buffer)) #;(send frame on-exit) )]
         [#\return    (λ () (buffer-break-line! (current-buffer)))]
         [#\backspace (λ () (buffer-delete-backward-char (current-buffer) 1))] ; the backspace key
         [#\rubout    (λ () (error 'todo))]                     ; the delete key
         ['home       (λ () (buffer-move-point-to-begining-of-line! (current-buffer)))] ; fn+left
         ['end        (λ () (buffer-move-point-to-end-of-line! (current-buffer)))]      ; fn+right
         ['release    (λ () (void 'ignore))]
         ; place self inserting characters after #\return and friends
         [(? char? k) (λ () 
                        (define b (current-buffer))
                        (buffer-insert-char! b k)
                        (buffer-move-point! b 1))]
         [_           #f])]
      [_ #f])))



;;;
;;; GUI
;;;

(require racket/gui/base)

;;; COLORS

(define (hex->color x)
  (define red   (remainder           x        256))
  (define green (remainder (quotient x   256) 256))
  (define blue  (remainder (quotient x 65536) 256))
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

(define (new-editor-frame b)
  ;;; FONT
  (define font-style  (make-parameter 'normal))  ; style  in '(normal italic)
  (define font-weight (make-parameter 'normal))  ; weight in '(normal bold)
  (define font-size   (make-parameter 16))
  (define font-family (make-parameter 'modern))  ; fixed width
  (define (use-default-font-settings)
    (font-style  'normal)
    (font-weight 'normal)
    (font-size   16)
    (font-family 'modern))
  (define font-ht (make-hash))                   ; (list size family style weight) -> font  
  (define (get-font)
    (define key (list (font-size) (font-family) (font-style) (font-weight)))
    (define font (hash-ref font-ht key #f))
    (unless font
      (set! font (make-object font% (font-size) (font-family) (font-style) (font-weight)))
      (hash-set! font-ht key font))
    font)
  (define default-fixed-font  (get-font))
  (define (toggle-bold)    (font-weight (if (eq? (font-weight) 'normal) 'bold   'normal)))
  (define (toggle-italics) (font-style  (if (eq? (font-style)  'normal) 'italic 'normal)))
  ;;; WINDOW SIZE
  (define min-width  800)
  (define min-height 800)
  ;;; COLORS
  (define background-color base1)
  (define text-color       base03)
  
  (define frame (new frame% [label "Editor"]))
  (define msg   (new message% [parent frame] [label "No news"]))
  (send msg min-width min-width)
  (define subeditor-canvas%
    (class canvas%
      (define/override (on-char event)
        ; TODO syntax  (with-temp-buffer body ...)
        (define key (key-event->key event))
        (unless (equal? key 'release)
          (send msg set-label (~a "key: " key)))
        (match (global-keymap '() key)
          [(? procedure? thunk) (thunk)]
          ['prefix              (error 'on-char "implement prefixes")]
          ['exit                (save-buffer! (current-buffer))
                                (send frame on-exit)]
          [_                    (void)])
        ; todo: don't trigger repaint on every key stroke ...
        (send canvas on-paint))
      (define/override (on-paint)
        (define dc (send canvas get-dc))
        ; reset drawing context
        (use-default-font-settings)
        (send dc set-font default-fixed-font)
        (send dc set-text-mode 'solid) ; solid -> use text background color
        (send dc set-background "white")
        (send dc clear)
        (send dc set-text-background background-color)        
        (send dc set-text-foreground text-color)
        ; (send msg set-label "on-paint")
        ; draw line
        (for/fold ([y 0]) ([l (text-lines (buffer-text b))])
          (define strings (line-strings l))
          (define n (length strings))
          (define (last-string? i) (= i (- n 1)))
          (for/fold ([x 0]) ([s strings] [i (in-range n)])
            (match s 
              [(? string?) (define t ; remove final newline
                             (if (last-string? i) (substring s 0(max 0 (- (string-length s) 1))) s))
                           (define-values (w h _ __) (send dc get-text-extent t))
                           (send dc draw-text t x y)
                           (+ x w)]
              [(property 'bold)     (toggle-bold)    (send dc set-font (get-font)) x]
              [(property 'italics)  (toggle-italics) (send dc set-font (get-font)) x]
              [_ (displayln (~a "Warning: Got " s)) x]))
          (+ y (font-size) 1))
        ; draw points
        (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
        (for ([p (buffer-points b)])
          (define-values (r c) (mark-row+column p))
          (define x (* c font-width))
          (define y (* r (+ font-height -2))) ; why -2 ?
          (send dc draw-line x y x (+ y font-height)))
        #;(send dc set-background (make-object color% "white")))
      (super-new)))
  (define canvas (new subeditor-canvas% [parent frame]))
  (send canvas min-client-width  400)
  (send canvas min-client-height 400)
  (send frame show #t))

(module+ test
  (current-buffer illead-buffer)
  (new-editor-frame illead-buffer))

(define (display-file path)
  (with-input-from-file path
    (λ ()
      (for ([l (in-lines)])
        (displayln l)))))
