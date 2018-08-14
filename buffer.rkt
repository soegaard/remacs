#lang racket/base
(provide (except-out (all-defined-out) refresh-buffer refresh-frame))

;;;
;;; BUFFER
;;;

(require (for-syntax racket/base syntax/parse)
         racket/format racket/list racket/match racket/string
         racket/set
         framework
         "buffer-locals.rkt" 
         "buffer-namespace.rkt"
         "command-loop.rkt"
         "default.rkt"
         "embedded.rkt"
         "line.rkt"
         "locals.rkt"
         "mark.rkt"
         "overlays.rkt"
         "parameters.rkt"
         "point.rkt"
         "region.rkt"
         "representation.rkt"
         "text.rkt")

(module+ test (require rackunit))

;;;
;;; Buffer Registry
;;;

; all buffers are registered in buffers-ht
(define buffers-ht (make-hash))  ; string -> buffer
(define all-buffers '())

; register-buffer : buffer [thunk-or-#f] -> void
;   associate (buffer-name b) to b in buffers-ht,
;   and put it all-buffers
(define (register-buffer b [on-error #f])
  (define name (buffer-name b))
  (if (hash-ref buffers-ht name #f)
      (cond 
        [on-error (on-error)]
        [else (error 'register-buffer 
                     "attempt to register buffer with name already in use: ~a" name)])
      (hash-set! buffers-ht name b))
  (set! all-buffers (cons b all-buffers)))

; deregister-buffer : buffer -> void
;   remove buffer from buffers-ht and all-buffers
(define (deregister-buffer b)
  (define name (buffer-name b))
  (hash-set! buffers-ht name #f)
  (set! all-buffers (remove b all-buffers)))

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

;;;
;;; Buffer Creation
;;;

; new-buffer : -> buffer
;   create fresh buffer without an associated file
(define (new-buffer [text (new-text)] [path #f] [name (generate-new-buffer-name "buffer")])
  (define locals (new-buffer-namespace)) ; see "buffer-namespace.rkt"
  (define overlays (new-overlays))
  (define b (buffer text name path
                    'point      ; point
                    'the-mark   ; the mark
                    'start-mark ; start mark (screen start)
                    'end-mark   ; end mark   (screen end)
                    #f          ; restricted?
                    #f          ; restriction start (narrowing)
                    #f          ; restriction end
                    '()         ; marks
                    '()         ; modes 
                    0           ; cur-line
                    0           ; num-chars
                    0           ; num-lines
                    #f          ; modified?
                    locals      ; locals
                    overlays))  ; overlays
  (define point    (new-mark b "*point*"))
  (set-buffer-point! b point)
  (define the-mark   (new-mark b "*mark-nb*" 0 #f #:active? #f #:insertion-type #f))
  (define start-mark (new-mark b "*start*"   0 #f #:active? #f #:insertion-type #f))
  (define end-mark   (new-mark b "*end*"     0 #f #:active? #f #:insertion-type #f))
  (set-buffer-the-mark!   b the-mark)
  (set-buffer-start-mark! b start-mark)
  (set-buffer-end-mark!   b end-mark)
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

;;; Point and Marks

; buffer-exchange-point-and-mark! : buffer -> void
;   exchange point and mark
(define (buffer-exchange-point-and-mark! [b (current-buffer)])
  (define p (buffer-point    b))
  (define m (buffer-the-mark b))
  (set-buffer-the-mark! b p)
  (set-buffer-point! b m))


;;; Scratch Buffer

(define scratch-text
  '("Welcome to remacs, an Emacs style editor implemented in Racket.\n"
    "The editor is still a work-in-progress.\n"
    "\n"
    "\n"
    "Search for keymap in the source to see the available keybindings.\n"
    "\n"
    " WINDOWS\n"
    "    C-x 9      delete window\n"
    "    C-x 1      delete other windows\n"
    "    C-x 2      splits the window in two vertically\n"
    "    C-x 3      splits the window in two horizontally\n"
    "    C-x o      switch to other window\n"
    "    C-x right  switch to next buffer\n"
    "\n"
    "\n"
    " BUFFERS\n"
    "    C-x h      mark whole buffer\n"
    "    C-x s      save some buffers\n"
    "    C-s        save buffer\n"
    "\n"
    "Happy Racketeering\n"
    "/soegaard"))

(define scratch-buffer (new-buffer (new-text (list->lines scratch-text)) #f "*scratch*"))
(current-buffer scratch-buffer)

; rename-buffer! : string -> string
(define (rename-buffer! new-name [b (current-buffer)] [unique? #t])
  (unless (string? new-name) (error 'rename-buffer "string expected, got " new-name))
  ; todo: check that buffer-name is not in use, if it is signal error unless unique? is false
  ;       in that case generate new name and return it
  (deregister-buffer b)
  (set-buffer-name! b new-name)
  (register-buffer b)
  new-name)


; get-buffer-create : buffer-or-name -> buffer
;   get buffer with given name, if none exists, create it
(define (get-buffer-create buffer-or-name)
  (define b buffer-or-name)
  (if (buffer? b) b (generate-new-buffer b)))

; buffer-open-file-or-create : string-or-file-path -> buffer
(define (buffer-open-file-or-create file-path)
  (define path (if (string? file-path) (string->path file-path) file-path))
  (unless (file-exists? path)
    (close-output-port (open-output-file path)))
  (define filename (last (map path->string (explode-path path))))
  (define text     (path->text path))
  (new-buffer text path (generate-new-buffer-name filename)))


; save-buffer : buffer -> void
;   save contents of buffer to associated file
;   do nothing if no file is associated
(define (save-buffer! b)
  (define file (buffer-path b))
  (unless file
    (set! file (finder:put-file)))
  (when file
    (set-buffer-path! b file)
    (deregister-buffer b)                     ; because name changes!
    (set-buffer-name! b (path->string file))
    (register-buffer b)    
    (with-output-to-file file
      (λ () (for ([line (text-lines (buffer-text b))])
              (for ([s (line-strings line)])
                (display s))))
      #:exists 'replace)
    (set-buffer-modified?! b #f)))

; save-buffer-as : buffer ->
;   get new file name from user, 
;   associate file with buffer,
;   and save it
(define (save-buffer-as! b)
  (define file (finder:put-file))
  (when file
    (set-buffer-path! b file)
    (deregister-buffer b)
    (set-buffer-name! b (path->string file))
    (register-buffer b)
    (save-buffer! b)))

(define (refresh-frame)
  ((current-refresh-frame)))

(define (refresh-buffer [b (current-buffer)])
  ((current-refresh-buffer) b))

#;(make-input-port name read-in peek close
                   ; optionals
                   get-progress-evt
                   commit
                   get-location
                   count-lines!
                   init-position
                   buffer-mode)

#;(define (make-input-buffer b)
    ; State
    (define count-lines? #f)
    ; Setup port
    (define name (buffer-name b)) ; name for input port
    (define read-in
      (λ (mbs) ; a mutable byte string to receive the read data
        ; 1. Fill in mbs
        ; 2. Return number of read bytes
        )))

; TODO implement it properly
(define (open-input-buffer b) ; temporary definition
  (open-input-string
   (text->string
    (buffer-text b))))

; make-output-buffer : buffer -> output-buffer
;   Return an output buffer. When written to, the data is inserted into
;   the buffer b at the position given by the mark. If the mark is #f
;   then the data is inserted at the end of the buffer.
(define (make-output-buffer b m)
  (set! m (or m (buffer-point b)))
  (define point (buffer-point b))
  ;(displayln (list 'make-output-buffer 'current-frame (refresh-frame)))
  ;; State
  (define count-lines? #f)
  ;; Setup port
  (define name (buffer-name b)) ; name for output port
  (define evt  always-evt)      ; writes never block
  (define write-out             ; handles writes to port
    (λ (out-bytes start end buffered? enable-breaks?)
      ; write bytes from out-bytes from index start (inclusive) to index end (exclusive)
      (define the-bytes (subbytes out-bytes start end))
      (define as-string (bytes->string/utf-8 the-bytes))      
      (check-mark m)
      (cond
        [m    (buffer-insert-string! b m       as-string)]
        [else (buffer-insert-string! b (point) as-string)])
      (check-mark m)
      (buffer-dirty! b)
      (send-command
       (parameterize ([current-rendering-suspended? #f])
         (localize ([current-buffer b])
           (refresh-buffer name))))     ; todo how to find the correct the frame?
      ; number of bytes written
      (- end start)))
  (define close                 ; closes port
    (λ () (void)))
  (define write-out-special     ; handles specials?
    #f)                         ; (not yet)
  (define get-write-evt         ; #f or procedure that returns synchronizable event
    #f)
  (define get-write-special-evt ; same for specials
    #f)
  (define get-location          ; #f or procedure that returns 
    (λ ()                       ; line number, column number, and position
      (when count-lines?
        (define-values (row col) (mark-row+column m))
        (values (+ 1 row) col (+ 1 (mark-position m))))))
  (define count-lines!
    (λ () (set! count-lines? #t)))
  (define init-position (+ 1 (mark-position m))) ; 
  (define buffer-mode #f)
  (make-output-port name evt write-out close write-out-special get-write-evt
                    get-write-special-evt get-location count-lines! init-position buffer-mode))


; read-buffer : buffer -> void
;   replace text of buffer with file contents
; BUG this doesn't handle points and marks
#;(define (read-buffer! b)
  (define path (buffer-path b))
  (unless path (error 'read-buffer "no associated file: ~a" b))
  (define text (path->text path))
  (define stats (text-stats text))
  (set-buffer-text! b text)  
  (set-buffer-num-lines! b (stats-num-lines stats))
  (set-buffer-num-chars! b (stats-num-chars stats))
  (set-buffer-modified?! b #f)
  (buffer-dirty! b))


; append-to-buffer-from-file : buffer path -> void
;   append contents of file given by the path p to the text of the buffer b
; BUG this doesn't handle points and marks
#;(define (append-to-buffer-from-file b p)
  (define text-to-append (path->text p))
  (define stats (text-stats text-to-append))
  (set-buffer-text! b (text-append! (buffer-text b) text-to-append))
  (set-buffer-num-lines! b (+ (buffer-num-lines b) (stats-num-lines stats)))
  (set-buffer-num-chars! b (+ (buffer-num-chars b) (stats-num-chars stats)))
  (set-buffer-modified?! b #t)
  (buffer-dirty! b))


(define (buffer-display b [port (current-output-port)])
  (parameterize ([current-output-port port])
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
    (status-display)))

; buffer-insert-string! : mark string -> void
;   insert string at mark,
;   move marks according to adjustment type
(define (buffer-insert-string! b m s)
  (raw-buffer-insert-string! b m s)
  (define p (position m))
  (define n (string-length s))
  (for ([m (buffer-marks b)])
    (mark-adjust-due-to-insertion! m p n)))

; raw-buffer-insert-string! : buffer position string -> void
;   insert string s at position i, do not move any marks
(define (raw-buffer-insert-string! b i s)
  (define m (new-mark b "*temp:raw-buffer-insert-string*" (position i)))
  (define t (buffer-text b))
  (define n (string-length s))
  (buffer-stretch-overlays b m n)
  ; the function text-insert-string-at-mark! works for strings
  ; containing no newlines - so we will need to split the string s
  ; into segments.
  (let loop ([segs (reverse (string-split s "\n" #:trim? #f))])
    (match segs
      [(list)   (void 'done)]
      [(list s) (text-insert-string-at-mark! t m b s)]
      [_        (text-insert-string-at-mark! t m b (first segs))
                (text-break-line! t m) ; no marks adjustment
                (mark-move! m -1) ; due to newline
                (loop (rest segs))]))
  ; delete temporary mark
  (delete-mark! m))

; raw-buffer-insert-char! : buffer mark char -> void
;   insert char at mark
;   Note: this functions does not adjust marks
(define (raw-buffer-insert-char! b m c)
  (define t (buffer-text b))
  (text-insert-char-at-mark! t m b c)
  (buffer-stretch-overlays b m 1)
  (buffer-dirty! b))

; buffer-insert-char! : buffer mark char -> void
;   insert character at mark,
;   move mark according to adjustment type
(define (buffer-insert-char! b mark c)
  (raw-buffer-insert-char! b mark c)
  (define p (mark-position mark))
  (for ([m (buffer-marks (mark-buffer mark))])
    (mark-adjust-due-to-insertion! m p 1)))

; buffer-insert-char-after-mark! : mark char -> void
;   insert character after mark
#;(define (buffer-insert-char-after-mark! m k)
    ; insert character
    (raw-buffer-insert-char! m k)
    ; adjust other marks
    (define b (mark-buffer m))
    (buffer-adjust-marks-due-to-insertion-after! b (mark-position m) 1))

; buffer-insert-char-before-mark! : mark char -> void
;   insert character and move mark
#;(define (buffer-insert-char-before-point! m k)
    (define b (mark-buffer m))
    (raw-buffer-insert-char! b k)
    (buffer-adjust-marks-due-to-insertion-after! b (mark-position m) 1))

; buffer-insert-string-before-mark! : mark string -> void
;   insert string before mark (and move marks)
#;(define (buffer-insert-string-before-point! b m s)
    (define n (string-length s))
    (buffer-insert-string! b m s)
    (buffer-adjust-marks-due-to-insertion-after! b (mark-position m) n))

(define (buffer-adjust-marks-due-to-insertion-after! b n a)
  (for ([m (buffer-marks b)])
    (mark-adjust-insertion-after! m n a)))

(define (buffer-adjust-marks-due-to-insertion-before! b n a)
  (for ([m (buffer-marks b)])
    (mark-adjust-insertion-before! m n a)))

; buffer-break-line! : buffer -> void
;   break line at mark
(define (buffer-break-line! b [m (get-point)])
  (text-break-line! (buffer-text b) m)
  (define n (position m))
  (for ([m (buffer-marks b)])
    (mark-adjust-insertion-after! m n 1))
  #;(when (and (mark? m) (mark-insertion-type m))
      (mark-move! m 1))
  (buffer-dirty! b))

; buffer-delete-backward-char! : buffer mark [natural] -> void
(define (buffer-delete-backward-char! b m [count 1])  
  (define t (buffer-text b))
  (define p (position m))
  (buffer-contract-overlays b p count)  
  (for ([i (max 0 (min p count))]) ; TODO improve efficiency!    
    (text-delete-backward-char! t m)
    (buffer-adjust-marks-due-to-deletion-before! b (mark-position m) 1)
    (buffer-dirty! b)))

(define (buffer-adjust-marks-due-to-deletion-before! b p a)
  (for ([m (buffer-marks b)])
    (mark-adjust-deletion-before! m p a)))

(define (buffer-insert-property-at-position! b i sym val)
  (define t (buffer-text b))
  (define xs (text-embedded t i))
  (unless (set-first-property! xs sym val)
    (text-insert-embedded! t i (property val sym))))

(define (buffer-insert-property-at-point! b sym val)
  (buffer-insert-property-at-position! b (point) sym val))

(define (buffer-remove-property-between! b i j sym)
  (define t (buffer-text b))
  (define xs (text-embedded-between t i j sym))
  (for ([x (in-list xs)])
    (set-property-symbol! x #f)))

(define (buffer-insert-property! b sym val [val-end val])
  ; if the region is active, the property is inserted
  ; before and after the region (consider: are all properties toggles?)
  ; if there are no region the property is simply inserted at point
  (cond
    [(use-region?)
     (define t (buffer-text b))     
     (define rb (region-beginning b))
     (define re (region-end b))
     ; first remove existing 
     (buffer-remove-property-between! b rb re sym)
     ; then insert properties before and after region
     (buffer-insert-property-at-position! b rb sym val)
     (buffer-insert-property-at-position! b re sym val-end)]
    [else
     (buffer-insert-property-at-point! b sym val)]))

(define (buffer-set-mark-to-point [b (current-buffer)])
  (check-mark (get-point))
  ; todo: what should happen with the old mark?
  (define m (new-mark b "*mark-smp*" (point) #f #:active? #t))
  (set-buffer-the-mark! b m)
  (check-mark m)
  m)

(define (buffer-set-the-mark [b (current-buffer)] [m #f])
  (unless (and (buffer? b) (mark? m))
    (error 'buffer-set-mark "expected buffer and mark"))
  ; todo: what should happen with the old mark?
  (define old-the-mark (buffer-the-mark b))
  (set-buffer-the-mark! b m)
  old-the-mark)


; list-next : list any (any any -> boolean)
;   return the element after x,
;   if x is the last element, then return the first element of xs,
;   if x is not found in the list, return #f
(define (list-next xs x =?)
  (match xs
    ['() #f]
    [_   (define first-x (first xs))
         (let loop ([xs xs])
           (cond 
             [(empty? xs)       #f]
             [(=? (first xs) x) (if (empty? (rest xs)) first-x (first (rest xs)))]
             [else              (loop (rest xs))]))]))

(module+ test
  (check-equal? (list-next '(a b c) 'a eq?) 'b)
  (check-equal? (list-next '(a b c) 'b eq?) 'c)
  (check-equal? (list-next '(a b c) 'c eq?) 'a)
  (check-equal? (list-next '(a b c) 'd eq?) #f))


; next-buffer : buffer -> buffer
;   all buffers are in all-buffers, return the one following b
(define (get-next-buffer [b (current-buffer)])
  (define next (list-next all-buffers b eq?))
  (unless (buffer? next)
    (error 'get-next-buffer
           "internal-error: the next buffer is not a buffer: ~a"
           next))
  next)

(define (get-previous-buffer [b (current-buffer)])
  (list-next (reverse all-buffers) b eq?))

(define (buffer-substring b from to)
  (subtext->string (buffer-text b) from to))

;;;
;;; POINT AND MARK
;;;

(define (get-mark [b (current-buffer)])
  (buffer-the-mark b))

(define (push-mark [pos-or-mark 0] [b (current-buffer)] #:name [name "*mark*"])
  (define m  (or (and (mark? pos-or-mark) pos-or-mark)
                 (new-mark b name pos-or-mark)))
  (define ms (buffer-marks b))
  (set-buffer-marks! b (cons m ms))
  m)


;;;
;;; OVERLAYS
;;;

(define (buffer-overlay-range-set! b from to sym val)
  (define os (buffer-overlays b))
  (overlays-set! os (position from) (position to) sym val))

(define (buffer-overlay-set! b sym i val)
  (define j (position i))
  (buffer-overlay-range-set! b sym j (+ j 1) val))

(define (buffer-overlay-ref b sym i [default #f])
  (define os (buffer-overlays b))
  (overlays-ref os sym (position i) default))

(define (buffer-overlay-ref/bounds b sym i [default #f])
  (define os (buffer-overlays b))
  (overlays-ref/bounds os sym (position i) default))

(define (buffer-stretch-overlays b i n)
  (define os (buffer-overlays b))
  (overlays-stretch-all os (position i) n))

(define (buffer-contract-overlays b i n)
  (define os (buffer-overlays b))
  (overlays-contract-all os (position i) n))

(define (buffer-overlay-positions b sym)
  (define os (buffer-overlays b))
  (overlays-positions os sym))

;;;
;;;

