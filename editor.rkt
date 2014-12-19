#lang racket
(module+ test (require rackunit))

;;;
;;; REPRENSENTATION
;;;

(struct line (string length) #:transparent #:mutable)

(struct text (lines) #:transparent)
; A text being edited is represented as a list of lines.

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

(struct mark (position name fixed?) #:transparent #:mutable)
; A mark rembers a position in the text.
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
  (line s (string-length s)))

(module+ test
  (define illead-text 
    (text (map string->line
               (list "Sing, O goddess, the anger of Achilles son of Peleus, that brought"
                     "countless ills upon the Achaeans. Many a brave soul did it send hurrying"
                     "down to Hades, and many a hero did it yield a prey to dogs and vultures,"
                     "for so were the counsels of Jove fulfilled from the day on which the"
                     "son of Atreus, king of men, and great Achilles, first fell out with"
                     "one another."))))
  
  ; recreate the same text file from scratch
  (define (create-new-test-file path)
    (with-output-to-file path
      (λ() (for ([line (text-lines illead-text)])
             (displayln (line-string line))))
      #:exists 'replace)))

; line-insert-char! : line char index -> void
;   insert char c in the line l at index i
(define (line-insert-char! l c i)
  (match-define (line s n) l)
  (unless (<= i n) (error 'line-insert-char "index i greater than line length, i=~a, l=~a" i l))
  (define new-string (cond [(= i n) (string-append s (string c))]
                           [(= i 0) (string-append (string c) s)]
                           [else    (string-append (substring s 0 i) (string c) (substring s i n))]))
  (set-line-string! l new-string)
  (set-line-length! l (+ n 1)))

; line-insert-string! : line string index -> void
;   insert string t in the line l at index i
(define (line-insert-string! l t i)
  (match-define (line s n) l)
  (unless (<= i n) (error 'line-insert-char "index i greater than line length, i=~a, l=~a" i l))
  (define new-string (cond [(= i n) (string-append s t)]
                           [(= i 0) (string-append t s)]
                           [else    (string-append (substring s 0 i) t (substring s i n))]))
  (set-line-string! l new-string)
  (set-line-length! l (+ n (string-length t))))

;;;
;;; TEXT
;;;

; new-text : -> text
;   create an empty text
(define (new-text)
  (text '()))

; text-append : text text -> text
(define (text-append t1 t2)
  (text (append (text-lines t1) (text-lines t2))))

; path->text : path -> text
;   create a text with contents from the file given by path
(define (path->text path)
  (with-input-from-file path 
    (λ () (text (for/list ([l (in-lines)]) (string->line l))))))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (check-equal? (path->text "illead.txt") illead-text))

; text-num-lines : text -> natural
;   return number of lines in the text
(define (text-num-lines t)
  (length (text-lines t)))

(define (text-num-chars t)
  (for/sum ([line (text-lines t)])
    (line-length line)))

(define (text-stats t)
  (define-values (nlines nchars)
    (for/fold ([nl 0] [nc 0]) ([l (text-lines t)])
      (values (+ nl 1) (+ nc (line-length l)))))
  (stats nlines nchars))

(define (text-insert-char-at-mark! t m b c)
  (define-values (row col) (mark-row+column m b))
  (define l (list-ref (text-lines t) row))
  (line-insert-char! l c col))


;;;
;;; MARKS
;;;

(define (new-mark name [pos 0] [fixed? #f])
  (mark pos name fixed?))

; mark-move! : mark integer -> void
;  move the mark n characters
(define (mark-move! m n)
  (define p (mark-position m))
  (define q (if (> n 0)
                (+ p n) ; todo: max position in buffer?
                (max 0     (+ p n))))
  (set-mark-position! m q))

; mark-row+column : mark buffer -> integer integer
;   return row and column number for the mark m in the buffer b
(define (mark-row+column m b)
  (define p (mark-position m))
  (define-values (row col)
    (let/ec return
      (for/fold ([r 0] [q 0]) ([l (text-lines (buffer-text b))])
        (define n (line-length l))
        (if (> (+ q n) p)
            (return r (- p q))
            (values (+ r 1) (+ q n))))))
  (values row col))

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
  (define points (list (new-mark "*point*")))
  (define marks '())
  (define modes '())
  (define cur-line 0)
  (define stats (text-stats text))
  (define num-lines (stats-num-lines stats))
  (define num-chars (stats-num-chars stats))
  (define modified? #f)
  (buffer text name path points marks modes cur-line num-chars num-lines modified?))

; save-buffer : buffer -> void
;   save contents of buffer to associated file
;   do nothing if no file is associated
(define (save-buffer! b)
  (define file (buffer-path b))
  (when file
    (with-output-to-file file
      (λ () (for ([line (text-lines (buffer-text b))])
              (displayln (line-string line))))
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
  (unless path (error 'read-buffer "no assoiated file: ~a" b))
  (define text (path->text path))
  (define stats (text-stats text))
  (set-buffer-text! b text)
  (set-buffer-num-lines! b (stats-num-lines stats))
  (set-buffer-num-chars! b (stats-num-chars stats))
  (set-buffer-modified?! b #f))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define b (new-buffer (text '()) "illead.txt" "illead"))
  (read-buffer! b)
  (check-equal? b illead-buffer))

; append-to-buffer-from-file : buffer path -> void
;   append contents of file given by the path p to the text of the buffer b
(define (append-to-buffer-from-file b p)
  (define text-to-append (path->text p))
  (define stats (text-stats text-to-append))
  (set-buffer-text! b (text-append (buffer-text b) text-to-append))
  (set-buffer-num-lines! b (+ (buffer-num-lines b) (stats-num-lines stats)))
  (set-buffer-num-chars! b (+ (buffer-num-chars b) (stats-num-chars stats)))
  (set-buffer-modified?! b #t))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define append-buffer (new-buffer (new-text)))
  (append-to-buffer-from-file append-buffer "illead.txt")
  (append-to-buffer-from-file append-buffer "illead.txt")
  (save-buffer! b) ; make sure the buffer is unmodified before comparison
  (check-equal? (buffer-text append-buffer) (text-append illead-text illead-text)))

; buffer-point : buffer -> mark
;   return the first mark in the list of points
(define (buffer-point b)
  (first (buffer-points b)))

; buffer-point-set! : buffer mark -> void
;   set the point at the position given by the mark m
(define (buffer-move-point! b n)
  (mark-move! (buffer-point b) n))

(define (buffer-display b)
  (define (line-display l)
    (displayln (line-string l)))
  (define (text-display t)
    (for ([l (text-lines t)])
      (line-display l)))
  (define (status-display)
    (newline)
    (displayln (~a "buffer: " (buffer-name b) "    " (if (buffer-modified? b) "*" "saved"))))
  (text-display (buffer-text b))
  (status-display))

(module+ test
  (buffer-display illead-buffer))

(define (buffer-insert-char! b c)
  (define m (buffer-point b))
  (define t (buffer-text b))
  (text-insert-char-at-mark! t m b c))

;;;
;;; WORLD
;;;

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
        (match k
          [(? char? k) (buffer-insert-char! b k)
                       (buffer-move-point! b 1)]
          ['left       (buffer-move-point! b -1)]
          ['right      (buffer-move-point! b  1)]
          [_           (void)])
        (send canvas on-paint))
      (define/override (on-paint)
        (define dc (send canvas get-dc))
        (send msg set-label "on-paint")
        (send dc clear)
        (send dc set-text-foreground text-color)
        (send dc set-font fixed-font)
        ; draw line
        (for/fold ([y 0]) ([l (text-lines (buffer-text b))])          
          (send dc draw-text (line-string l) 0 y)
          (+ y font-size 1))
        ; draw points
        (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
        (for ([p (buffer-points b)])
          (define-values (r c) (mark-row+column p b))
          (define x (* c font-width))
          (define y (* r font-height))
          (send dc draw-line x y x (+ y font-height))))
      (super-new)))
  (define canvas (new subeditor-canvas% [parent frame]))
  (send canvas min-client-width  400)
  (send canvas min-client-height 400)
  (send frame show #t))

(module+ test
  (new-editor-frame illead-buffer))










