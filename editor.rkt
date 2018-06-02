#lang racket
;;;
;;; INSTRUCTIONS
;;;

; In order to start remacs, open this file in DrRacket and run it.


;;; WIP: Extend render-buffer and render-points to handle text lines longer than screen lines.
;;;      DONE render-buffer now handles lines longer than screen
;;;      DONE change render-points
;;;      DONE fix render-points
;;;      TODO fix mouse clicks (needs to handle wrapped lines)
;;;      TODO make the max screen line size (now 60) a parameter
;;;      TODO make the line wrapping character a buffer-local

;;; BUG  fix completion:  M-x fo <tab> c   leads to error
;;; TODO run fundamental-mode in upstart
;;; TODO cursor blinking stops when menu bar is active ?!
;;; TODO .remacs
;;; TODO buffer narrowing
;;; TODO timestamp for blinking cursor needs to be on a per window base
;;; TODO Introduce double buffering to avoid any flicker.
;;;      https://www.facebook.com/notes/daniel-colascione/buttery-smooth-emacs/10155313440066102/
;;; TODO C-u <digit> ... now sets current-prefix-argument.
;;;      but only self-insert-char actually uses the prefix argument.
;;;      Use current-prefix-argument in other commands as well.
;;; TODO Allow negative numeric prefix
;;; TODO Holding M and typing a number should create a numeric prefix.

;;; TODO Finish eval-buffer
;;;        ok use buffer-local namespace for evaluation
;;;        ok fix new-buffer (buffer-top needs to be required)
;;;        ok catch errors
;;;        - on error: send point to offending expression
;;;        - on error: stop at first expression with error?
;;;        - convenient initial namespace (now racket/base)
;;;        - output where?

;;; TODO Wordwrap
;;; TODO #\tab now inserts 4 space
;;;      But ... if rendering breaks if the a file contains #\tab
;;; TODO Let screen follow cursor rather than disappear to the right (for long lines)
;;; TODO The column position of the cursor when using down should stay the same
;;;      even if one goes across short line.
;;; TODO The height of the highlight coloring is slightly too big.
;;;      This means that render un-highligthed characters on the next line
;;;      removes the bottom of the highligthing.
;;;      Two possible solutions:  1) reduce height
;;;                               2) erase from end of line to right edge of window
;;;      Both solutions needs to go into render-buffer.
;;; TODO test with large file (words.txt)
;;;        ok  - open large file
;;;        ok  - end-of-buffer
;;;        fix - cursor movements at end of file are *slow*
;;; TODO [Sublime] If a selection is found elsewhere, they are boxed
;;; TODO Hydra: https://github.com/abo-abo/hydra
;;; TODO Implement undo
;;; TODO Implement subtext
;;; TODO Implement Move line/selection up   [Sublime]
;;; TODO Implement Move line/selection down

;;; TODO brace matching
;;; TODO recently opened files
;;; TODO indentation
;;; TODO paragraphs
;;; TODO filling
;;; TODO documentation
;;; TODO left and right needs to toggle transient-mode rather than deactivate the mark
;;; TODO Properties and faces
;;; TODO Introduce global that controls which key to use for meta
;;; TODO Implement open-input-buffer
;;; TODO Completions ala http://sublimetext.info/docs/en/extensibility/completions.html

(module+ test (require rackunit))
(require "dlist.rkt" (for-syntax syntax/parse) framework)
(require racket/gui/base)

(require "buffer.rkt"
         "buffer-locals.rkt"
         "canvas.rkt"
         "colors.rkt"
         "commands.rkt"
         "completion.rkt"
         "deletion.rkt"
         "frame.rkt"
         "key-event.rkt"
         "killing.rkt"
         "line.rkt"
         "mark.rkt"
         "message.rkt"
         "parameters.rkt"
         "point.rkt"
         "render.rkt"
         "region.rkt"
         "representation.rkt"
         "simple.rkt"
         "status-line.rkt"
         "string-utils.rkt"
         "text.rkt"
         "window.rkt")


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

(define (remove-last xs)
  (if (null? xs) xs
      (reverse (rest (reverse xs)))))


(define global-keymap
  (λ (prefix key)
    ;(write (list prefix key)) (newline)    
    ; if prefix + key event is bound, return thunk
    ; if prefix + key is a prefix return 'prefix
    ; if unbound and not prefix, return #f
    (define (digits->number ds) (string->number (list->string ds)))
    (define (digit-char? x) (and (char? x) (char<=? #\0 x #\9)))
    ; todo: allow negative numeric prefix
    (match prefix
      [(list "M-x" more ...)
       (match key
         ["ESC"       (message "")
                      #f]
         ["backspace" (define new (remove-last more))
                      (message (string-append* `("M-x " ,@(map ~a new))))
                      `(replace ,(cons "M-x" new))]
         [#\tab       (define so-far (string-append* (map ~a more)))
                      (define cs     (completions-lookup so-far))
                      (cond 
                        [(empty? cs) (message (~a "M-x " so-far key))
                                     'ignore]
                        [else
                         (define b (current-completion-buffer))
                         (unless b 
                           ;; no prev completions buffer => make a new
                           (define bn "*completions*")
                           (define nb (new-buffer (new-text) #f bn))
                           (current-completion-buffer nb)
                           (set! b nb) 
                           ;; show it new window
                           (split-window-right)   ; both windows show same buffer
                           (define ws (frame-window-tree (current-frame)))
                           (define w  (list-next ws (current-window) eq?))
                           (define ob (window-buffer w))
                           (set-window-buffer! w nb)
                           (current-completion-window ob))
                         ;; text in *completion* buffer
                         (define t (completions->text so-far cs))
                         ;; replace text in completions buffer
                         (mark-whole-buffer b)
                         (delete-region b)
                         (buffer-insert-string-before-point! b (text->string t))
                         ;; replace prefix with the longest unique completion
                         (define pre (longest-common-prefix cs))
                         (message (~a "M-x " pre))
                         (list 'replace (cons "M-x" (string->list pre)))])]
         ["return"    (define cmd-name (string-append* (map ~a more)))
                      (define cmd      (lookup-interactive-command cmd-name))
                      (message "")
                      cmd]
         [_           (message (string-append* `("M-x " ,@(map ~a more) ,(~a key))))
                      'prefix])]
      [(list "C-u" (? digit-char? ds) ... x ...)
       (match key
         [(? digit-char?) 'prefix]
         [#\c             (displayln (digits->number ds)) (λ () (move-to-column (digits->number ds)))]
         [else            (current-prefix-argument (digits->number ds))
                          (global-keymap x key)])]
      [(list "ESC") 
       (match key
         [#\b         backward-word]
         [#\f         forward-word]
         [_           #f])]
      [(list "C-x")
       (match key
         [#\t         test]
         [#\0         delete-window]
         [#\1         delete-other-windows]
         [#\2         split-window-below]
         [#\3         split-window-right]
         [#\h         mark-whole-buffer]
         [#\s         save-some-buffers]
         [#\o         other-window]         
         ["C-s"       save-buffer]
         ["C-x"       exchange-point-and-mark]
         ['right      next-buffer]
         ['left       previous-buffer]
         ; ["C-b"     list-buffers]     TODO        
         [_           #f])]
      [(list)
       ; (write (list 'empty-prefix 'key key)) (newline)
       (match key
         ["ESC"          'prefix]
         ["C-x"          'prefix]
         ["C-u"          'prefix]
         ["M-x"          (message "M-x ") 'prefix]
         ; movement
         ['left           backward-char]
         ['right          forward-char]
         ['up             previous-line]
         ['down           next-line]
         ['wheel-down     next-line]
         ['wheel-up       previous-line]
         ["S-left"        backward-char/extend-region]
         ["S-right"       forward-char/extend-region]
         ["S-up"          previous-line/extend-region]
         ["S-down"        next-line/extend-region]         
         ; Ctrl + something
         ["C-a"           beginning-of-line]
         ["C-b"           backward-char]
         ["C-e"           end-of-line]
         ["C-f"           forward-char]
         ["C-k"           kill-line]
         ["D-backspace"   kill-line-to-beginning]
         ["C-l"           recenter-top-bottom]
         ["C-S-backspace" kill-whole-line]
         ["C-p"           previous-line]
         ["C-n"           next-line]
         ["C-w"           kill-region]
         ; Cmd + something
         ["D-a"           mark-whole-buffer]   ; select all 
         ["D-c"           copy-region]         ; copy  (Edit Menu)
         ["D-x"           kill-region]         ; cut   (Edit Menu)
         ["D-v"           insert-latest-kill]  ; paste (Edit Menu)
         ["D-left"        beginning-of-line]
         ["D-right"       end-of-line]
         ["D-down"        end-of-buffer]
         ["D-up"          beginning-of-buffer]
         ["D-S-left"      beginning-of-line/extend-region]   ; todo: should move word wise? 
         ["D-S-right"     end-of-line/extend-region]         ; todo: should move word wise?
         ["D-S-up"        beginning-of-buffer/extend-region] 
         ["D-S-down"      end-of-buffer/extend-region]       
         ["D-o"           open-file-or-create]
         ["D-w"           'exit] ; Cmd-w (mac only)
         ["D-return"      insert-line-after]
         ["D-S-return"    insert-line-before]
         ["D-="           (λ () (text-scale-adjust  1))]
         ["D--"           (λ () (text-scale-adjust -1))]
         ; Meta + something
         ["M-S-@"         mark-word]
         ["M-left"        backward-word]
         ["M-right"       forward-word]
         ["M-S-left"      backward-word/extend-region]
         ["M-S-right"     forward-word/extend-region]
         ["M-b"           (λ () (buffer-insert-property! (current-buffer) (property 'bold)))]
         ["M-i"           (λ () (buffer-insert-property! (current-buffer) (property 'italics)))]
         ["M-f1"          (λ () (buffer-insert-property! 
                                 (current-buffer) (property yellow) (property text-color)))]
         ["M-f2"          (λ () (buffer-insert-property! 
                                 (current-buffer) (property orange) (property text-color)))]
         ["M-f3"          (λ () (buffer-insert-property! 
                                 (current-buffer) (property blue)   (property text-color)))]
         ["f1"            test-buffer-output]
         ; ["M-d"           (λ () (buffer-display (current-buffer)))]
         ["M-d"           kill-word]
         ["M-backspace"   backward-kill-word]
         ["M-s"           save-buffer]
         ["M-S"           save-buffer-as]
         ["M-e"           eval-buffer]
         ["M-w"           'exit #;(λ () (save-buffer! (current-buffer)) #;(send frame on-exit) )]
         [#\return        break-line]
         [#\backspace     backward-delete-char]                                            ; backspace
         [#\rubout        (λ () (error 'todo))]                                            ; delete
         ; make tab insert 4 spaces
         [#\tab           (λ() (define insert (self-insert-command #\space)) (for ([i 4]) (insert)))]
         ['home           (λ () (buffer-move-point-to-beginning-of-line! (current-buffer)))] ; fn+left
         ['end            (λ () (buffer-move-point-to-end-of-line! (current-buffer)))]      ; fn+right
         ['next           page-down] 
         ['prior          page-up]
         ["C-space"       command-set-mark]
         ; place self inserting characters after #\return and friends
         ["space"         (self-insert-command #\space)]
         [(? char? k)     (self-insert-command k)]
         [_               #f])]
      [_ #f])))
(current-global-keymap global-keymap)

;;;
;;; GUI
;;;


(current-point-color point-colors)



(define (sort-numbers xs) (sort xs <))

(define screen-line-length 80)

(define cached-screen-lines-ht (make-hasheq))

(define (render-buffer w)
  (define (marks-between marks from to)
    (for/list ([m marks] #:when (<= from (mark-position m) to))
      (mark-position m)))
  (define (line->screen-lines b l r k p other) ; l = line, r = row, p = position of line start
    ; k screen line number
    ; a text line can be longer that a screen, so we need to break the line into shorter pieces.
    (define line      l)
    (define start-pos p)
    (define end-pos   (+ p (line-length l)))
    (define row       r)
    
    (define len screen-line-length)
    (define strings (line-strings l))
    (define n       (length strings))
    ; first break the line into smaller pieces
    (define pieces
      (append*       
       (for/list ([s (in-list strings)])
         (cond
           [(string? s)
            (define sn (string-length s))
            (define (position-is-in-this-string? x) (and (<= p x) (< x (+ p sn))))
            (define positions-in-string ; find positions of points, marks and wrap posns in the string
              (sort-numbers (append (filter position-is-in-this-string? other)
                                    (marks-between (buffer-marks b)  p (+ p sn))
                                    (marks-between (buffer-points b) p (+ p sn))
                                    (range (+ p len) (+ p sn) len))))
            ; split the string at the mark positions (there might be a color change)
            (define start-positions (cons p positions-in-string))
            (define end-positions   (append positions-in-string (list (+ p sn))))
            (define substrings      (map (λ (start end)
                                           (list start (substring s (- start p) (- end p))))
                                         start-positions end-positions))
            substrings]
           [else (list p s)]))))
    ; second, group strings in screen lines
     (let loop ([ps pieces] [start start-pos] [end start-pos]
                            [c 0] [i k] [l '()] [ls '()])
       ; c = column, d=index in text line, l= current line, ls = lines
      (cond
        [(>= c screen-line-length) ; make new line
         (define sl (screen-line line row i start end (reverse l)))
         (loop ps end end 0 (+ i 1) '() (cons sl ls))]
        [else                      ; on same line, accumulate strings and properties
         (match ps
           [(list)                 ; last line
            (cond [(empty? l) (values (reverse ls) i)]
                  [else       (define sl (screen-line line row i start end (reverse l)))
                              (values (reverse (cons sl ls)) (+ i 1))])]
           [(cons (list p (? string? s)) ps)
            (define n (string-length s))
            (loop ps start (+ end n) (+ c n) i (cons (list p s) l) ls)]
           [(cons (list p x) ps)
            (loop ps start    end       c    i (cons (list p x) l) ls)])])))
  (define (remove-trailing-newline s)
    (or (and (not (equal? s ""))
             (char=? (string-ref s (- (string-length s) 1)) #\newline)
             (substring s 0 (max 0 (- (string-length s) 1))))
        s))
  
  (unless (current-rendering-suspended?)
    (define b  (window-buffer w))
    (define c  (window-canvas w))
    (define dc (send c get-dc))
    ;; Canvas Dimensions
    (define-values (xmin xmax ymin ymax) (canvas-dimensions c))  
    (define num-lines-on-screen   (number-of-lines-on-screen w))
    ;; Font Dimensions
    (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
    ;; Placement of point relative to lines on screen
    (define-values (row col)           (mark-row+column (buffer-point  b)))
    (define-values (start-row end-row) (maybe-recenter-top-bottom #f w))
    (define num-lines-to-skip   start-row)
    ;; Render
    (unless (current-render-points-only?)
      (when b
        ;; Highlighting for region between mark and point
        (define text-background-color (send dc get-text-background))
        (define (set-text-background-color highlight?)
          (define background-color (if highlight? region-highlighted-color text-background-color))
          (send dc set-text-background background-color))
        ;; Placement of region
        (define-values (reg-begin reg-end)
          (if (use-region? b) (values (region-beginning b) (region-end b)) (values #f #f)))
        ; (displayln (list 'first-line first-row-on-screen 'last-line last-row-on-screen))
        (send dc suspend-flush)
        ; draw-string : string real real -> real
        ;   draw string t at (x,y), return point to draw next string
        (define (draw-string t x y)
          (define-values (w h _ __) (send dc get-text-extent t))
          (send dc draw-text t x y)
          (+ x w))
        (define (render-screen-lines dc xmin y sls)
          (let loop ([y y] [cs (map screen-line-contents sls)])
            (match cs
              [(list)       y]
              [(list c)           (render-screen-line dc xmin y c #f)]
              [(cons c cs)  (loop (render-screen-line dc xmin y c #t) cs)])))
        (define (render-screen-line dc xmin y contents wrapped-line-indicator?)
          ; contents = screen line = list of (list position string/properties)
          (define xmax
            (for/fold ([x xmin]) ([p+s contents])
              (match-define (list p s) p+s)
              (when (and reg-begin (<= reg-begin p) (< p reg-end))  (set-text-background-color #t))
              (when (and reg-end   (<= reg-end   p))                (set-text-background-color #f))
              (match (second p+s)
                [(? string?)           (draw-string (remove-trailing-newline s) x y)]
                [(property 'bold)      (toggle-bold)    (send dc set-font (get-font))   x]
                [(property 'italics)   (toggle-italics) (send dc set-font (get-font))   x]
                [(property (? color? c))                (send dc set-text-foreground c) x]
                [_ (displayln (~a "Warning: Got " s))                                   x])))
          (when wrapped-line-indicator?
            (draw-string "↵" xmax (- y 2)))
          (+ y (line-size)))
        ; draw text:
        ;   loop over text lines
        ;   c screen column, ; p the position of start of line
        (define (screen-lines->screen-line-positions xs)
          (define (screen-line->start-position x)
            (match x [(list* (cons p _) more) p] [(list) #f]))
          (map screen-line->start-position xs))
        
        (define-values (_ __ ___ all-screen-lines)
          ; render lines on screen
          (for/fold ([y ymin] [p 0] [n 0] [screen-line-positions '()]) ; n = screen line number
                    ([l (text-lines (buffer-text b))]
                     [i (in-range (+ start-row num-lines-on-screen))])
            (cond
              [(< i num-lines-to-skip)
               (when (and reg-begin (<= reg-begin p) (< p reg-end)) (set-text-background-color #t))
               (when (and reg-end   (<= reg-end   p))               (set-text-background-color #f))
               (values y (+ p (line-length l)) 0 '())]
              [else
               (define region-positions
                 (append (if reg-begin (list reg-begin) '())
                         (if reg-end   (list reg-end)   '())))
               (define-values (sls new-n) (line->screen-lines b l i n p region-positions))
               (define new-y (render-screen-lines dc xmin y sls))
               (values new-y (+ p (line-length l)) new-n
                       (cons (list p sls)
                             screen-line-positions))])))
        (define (screen-lines->screen-line-positions-ranges xs)
          (let loop ([rs '()] [xs xs])
            (match xs
              [(list* (list (cons p1 _) ...)
                      (list (cons p2 _) ...)
                      _)                          (loop (cons (list p1 p2) rs) (rest xs))]
              [(list* (list (cons p1 _) ...)
                      _)                          (reverse (cons (list p1 +inf.0) rs))])))
        ; cache the positions of the visible screen lines,
        ; this is used to render the points
        (hash-set! cached-screen-lines-ht b (reverse all-screen-lines))
        
        ; get point and mark height
        ;(define font-width  (send dc get-char-width))
        ;(define font-height (send dc get-char-height))
        ; (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
        ; Note: The font-height is slightly larger than font-size
        ; (displayln (list 'render-buffer 'font-height font-height 'font-size (font-size)))
        ; resume flush
        (send dc resume-flush)
        (void)))
    ; draw points
    (render-points w start-row end-row)))

(define debug-buffer #f)
(define debug-info #f)
(define (render-points w start-row end-row)
  (define (find-index positions p [index 0])
    (match positions
      [(list* from to _)
       (if (and (<= from p) (< p to)) index (find-index (rest positions) p (+ index 1)))]
      [(list from) (if (>= p from) index #f)]
      [(list) #f]))      
  (unless (current-rendering-suspended?)
    (define b  (window-buffer w))
    (define c  (window-canvas w))
    (define dc (send c get-dc))
    ;; Canvas Dimensions
    (define-values (xmin xmax ymin ymax) (canvas-dimensions c))  
    ; (displayln (milliseconds-delta)) ; expect values around 100
    (define colors (current-point-color))
    (define points-pen (new pen% [color (car colors)]))
    (define now   (remainder (current-milliseconds) 100000))
    (for ([i (quotient now 100)])
      (current-point-color (cdr colors)))
    ;(color-fuel (remainder now 100))
    (define points-off-pen (new pen% [color background-color]))  
    ; get point and mark height
    (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
    (when b
      (define active? (send (window-canvas w) has-focus?))
      (when active?
        (define on? (current-show-points?))
        (for ([p (buffer-points b)])
          ; (define-values (r c) (mark-row+column p))
          (when #t #;(<= start-row r end-row)
            (define n (mark-position p))
            (set! debug-buffer b)
            (define positions+screen-lines (hash-ref cached-screen-lines-ht b))
            (define screen-lines (append* (map second positions+screen-lines)))
            (define sl  (for/first ([sl (in-list screen-lines)]
                                    #:when (and (<=   (screen-line-start-position sl) n)
                                                (<  n (screen-line-end-position sl))))
                          sl))
            (when sl
              ; (error)
              
              (define s (screen-line-start-position sl))
              (define c (- n s))
              (define r (screen-line-screen-row sl))
              (define x (+ xmin (* c  font-width)))
              (define y (+ ymin (* r (line-size))))
              (when (and (<= xmin x xmax) (<= ymin y) (<= y (+ y font-height -1) ymax))
                (define old-pen (send dc get-pen))
                (send dc set-pen (if #t ;on? 
                                     points-pen
                                     points-off-pen))
                (send dc draw-line x y x (min ymax (+ y font-height -1)))
                (send dc set-pen old-pen)))))))))
  
(define (render-window w)
  (define c  (window-canvas w))
  (define dc (send c get-dc))
  (send dc suspend-flush)

  ;; sane defaults
  (use-default-font-settings)
  (send dc set-font (default-fixed-font))
  (send dc set-text-mode 'solid) ; solid -> use text background color
  (send dc set-background background-color)
  (unless (current-render-points-only?)
    (send dc clear))
  
  (send dc set-text-background background-color)
  (send dc set-text-foreground text-color)
  
  ;; render buffer
  (define-values (xmin xmax ymin ymax) (canvas-dimensions c))
  (render-buffer w)
  ;; draw borders (draw borders last to avoid flickering)  
  (define bs (window-borders w))
  (define op (send dc get-pen))
  (send dc set-pen border-pen)
  (when (set-member? bs 'top)
    (send dc draw-line 0 0 xmax 0)
    (set! ymin (+ ymin 1)))
  (when (set-member? bs 'left)
    (send dc draw-line 0 0 0 ymax)
    (set! xmin (+ xmin 1)))
  (send dc set-pen op)

  (send dc resume-flush))

(current-render-window render-window)

(define (render-windows win)
  (match win
    [(horizontal-split-window _ _ _ _ _ _ _ _ left  right) 
     (render-windows left)
     (render-windows right)]
    [(vertical-split-window _ _ _ _ _ _ _ _ upper lower)
     (render-windows upper)
     (render-windows lower)]
    [(window frame panel borders canvas parent buffer start end)
     (render-window  win)]
    [_ (error 'render-window "got ~a" win)]))

(define (frame->windows f)
  (define (loop ws)
    (match ws
      [(vertical-split-window _ _ _ _ _ _ _ _ upper lower)
       (append (loop upper) (loop lower))]
      [(horizontal-split-window _ _ _ _ _ _ _ _ left right)
       (append (loop left) (loop right))]
      [w (list w)]))
  (loop (frame-windows f)))

(define (render-frame f)
  ;; show name of buffer with keyboard focus as frame title
  (define f% (frame-frame% f))
  (define ws (frame->windows f))
  (define w  (for/or ([w ws])
               (and (send (window-canvas w) has-focus?)
                    w)))
  (when (window? w)
    (define n (buffer-name (window-buffer w)))
    (unless (equal? n (send f% get-label))
      (send f% set-label n)))
  ;; render windows
  (render-windows (frame-windows f)))

(current-render-frame render-frame) ; TODO

;;; Mini Canvas
; The bottom line of each frame is a small canvas.
; The mini canvas can be used to display either the Echo Area 
; or a Mini Buffer.

;;; ECHO AREA

; The Echo Area uses the the mini canvas at the bottom of the 
; frame to give messages to the user.

;;; MINI BUFFER

; The mini buffer is a buffer displayed in the mini canvas.
; Most buffer operations are avaialble, but it can not be split.
; <tab>, <space> and <return> are usually bound to completion 
; operations in a minibuffer.

#;(define (message format-string . arguments)
    ; TODO
    ; Display the message in the mini-buffer,
    ; add the message to the *Messages* buffer.
    (define msg (apply format format-string arguments))
    #;(send (frame-echo-area f) set-message s)
    1)




(define make-frame frame)
(define (frame-install-frame%! this-frame)
  ;;; FRAME SIZE
  (define min-width  800)
  (define min-height 400)
  ;;; FRAME  
  (define frame (new frame% [label "Remacs  -  The Racket Editor"] [style '(fullscreen-button)]))
  (set-frame-frame%! this-frame frame)
  (define msg (new message% [parent frame] [label "No news"]))
  (current-message msg)
  (send msg min-width min-width)
  ;;; MENUBAR
  (define (create-menubar)
    (define-syntax (new-menu-item stx)
      (syntax-parse stx  ; add menu item to menu
        [(_ par l sc scm cb) 
         #'(let ([m scm])
             (if m
                 (new menu-item% [label l] [parent par] [shortcut sc] [callback cb] 
                      [shortcut-prefix (if (list? m) m (list m))])
                 (new menu-item% [label l] [parent par] [shortcut sc] [callback cb])))]))
    (define mb (new menu-bar% (parent frame)))
    ;; Remacs Menu
    (define m (new menu% (label "Remacs") (parent mb)))
    (new-menu-item m "About   "   #f #f            (λ (_ e) (about-remacs)))
    ;; File Menu
    (define fm (new menu% (label "File") (parent mb)))
    (new-menu-item fm "New File"   #\n #f           (λ (_ e) (create-new-buffer)))
    (new-menu-item fm "Open"       #\o #f           (λ (_ e) (open-file-or-create)))
    (new-menu-item fm "Save"       #\s #f           (λ (_ e) (save-buffer)))
    (new-menu-item fm "Save As..." #\s '(shift cmd) (λ (_ e) (save-buffer-as)))
    ;; Edit Menu
    (define em (new menu% (label "Edit") (parent mb)))
    (new-menu-item em "Select All" #\a #f (λ (_ e) (mark-whole-buffer)))
    (new-menu-item em "Copy"       #\c #f (λ (_ e) (copy-region)))
    (new-menu-item em "Cut"        #\x #f (λ (_ e) (kill-region)))
    (new-menu-item em "Paste"      #\v #f (λ (_ e) (insert-latest-kill)))
    ;; Edit | Text
    (define etm (new menu% (label "Text") (parent em)))
    (new-menu-item etm "Kill line"         #\k         '(ctl)      (λ (_ e) (kill-line)))
    (new-menu-item etm "Kill Whole Line"   #\k         '(ctl shift)(λ (_ e) (kill-whole-line)))    
    (new-menu-item etm "Kill to Beginning" #\backspace '(cmd)      (λ (_ e) (kill-line-to-beginning)))
    ;; Racket Menu
    (define rm (new menu% (label "Racket") (parent mb)))
    (new-menu-item rm "Run"                #\r #f (λ (_ e) (test-buffer-output)))
    ;; Window Menu
    (define        wm (new menu% (label "Window") (parent mb)))
    (new-menu-item wm "C-x 0       Delete Window"        #f #f (λ (_ e) (delete-window)))
    (new-menu-item wm "C-x 1       Delete Other Windows" #f #f (λ (_ e) (delete-other-windows)))
    (new-menu-item wm "C-x 2       Split Window Below"   #f #f (λ (_ e) (split-window-below)))
    (new-menu-item wm "C-x 3       Split Window Right"   #f #f (λ (_ e) (split-window-right)))
    (new-menu-item wm "C-x o       Other Window"         #f #f (λ (_ e) (other-window)))
    (new-menu-item wm "C-x <right> Next Buffer"          #f #f (λ (_ e) (next-buffer)))
    ;; Buffer Menu
    (define        bm (new menu% (label "Buffer") (parent mb)))
    (new-menu-item bm "C-s s Save Buffer"          #f #f (λ (_ e) (save-buffer)))
    (new-menu-item bm "C-x s Save Some Buffers"    #f #f (λ (_ e) (save-some-buffers)))
    (new-menu-item bm "C-x h Mark Whole Buffer"    #f #f (λ (_ e) (mark-whole-buffer)))
    ;; Evaluation Menu
    (define        evm (new menu% (label "Evaluation") (parent mb)))
    (new-menu-item evm "Evaluate Buffer" #f #f (λ (_ e) (eval-buffer)))
    ;; Help Menu
    (new menu% (label "Help") (parent mb))) 
  (create-menubar)
  ;; PANEL
  ; The holds contains the shown window 
  (define panel (new vertical-panel% 
                     [parent     frame]
                     [min-width  min-width]
                     [min-height min-height]))
  (set-frame-panel! this-frame panel)
  ;;; CANVAS
  ; Non-split windows are rendered into an associated canvas.
  ; (Split windows holds panels of windows and/or subpanels)
  ; Buffers, mini buffers and the echo area are rendered into 
  ; into the canvas of the window to which they belong.
  
  ; (define canvas 'todo #;(create-window-canvas w))
  ; (set-frame-canvas! this-frame canvas) ; XXX
  ;; Status line
  (define status-line (new message% [parent frame] [label "Welcome"]))
  (set-frame-status-line! this-frame status-line)
  (send status-line min-width min-width)
  (define (display-status-line s) (send status-line set-label s))
  (display-status-line "Don't panic")
  (send frame show #t)
  
  ; (struct frame (frame% panel windows mini-window) #:mutable)
  (make-frame frame panel #f #f status-line))

(define (about-remacs)
  (define blank   "")
  (define welcome "
    Welcome to Remacs  -  version 0.01 May 2018.    ")  
  (define blurb   "
    Remacs is a text editor.
    Remacs is extensible.
    Remacs is scriptable.
    Remacs is Emacs-inspired.")
  (define authors "
    Authors:                                        
      - Jens Axel Søgaard")
  (define f (new frame% [label "About Remacs"]))
  (define v (new vertical-pane% [parent f]))
  (for ([l (list welcome blurb blank authors)])
    (new message% [label l] [parent v]))
  (send f show #t))

(define (display-file path)
  (with-input-from-file path
    (λ ()
      (for ([l (in-lines)])
        (displayln l)))))

; This starts the editor.

(module+ main
  (current-buffer scratch-buffer)
  (define f  (frame #f #f #f #f #f))
  (frame-install-frame%! f) ; installs frame% and panel
  
  (define p (frame-panel f))
  (define w (new-window f p scratch-buffer 'root))
  
  (set-frame-windows! f w)
  (current-window w)
  (current-frame f)
  
  (send (window-canvas w) focus))