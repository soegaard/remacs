#lang racket/base
(provide (all-defined-out)
         (all-from-out "narrow.rkt"))

;;;
;;; SIMPLE 
;;;

; This file contains various general editing commands that
; don't belong in a specific major mode.

;; The names of the commands were chosen to match the command names used in Emacs.
;; The index of Emacs commands are here:
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Command-Index.html#Command-Index
;; However it is often better to look at:
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html

;; The interactive commands in this file are all user commands.
;; The user can invoke them either via key bindings or via M-x.
;; See more on interactive commands in "commands.rkt".

(require (for-syntax syntax/parse racket/base)
         racket/class racket/format racket/list racket/match racket/set racket/string
         syntax/to-string
         racket/gui/base
         framework
         "buffer.rkt"
         "buffer-locals.rkt"
         "colors.rkt"
         "commands.rkt"         
         "deletion.rkt"
         "frame.rkt"
         "killing.rkt"
         "line.rkt"
         "locals.rkt"
         "mark.rkt"
         "message.rkt"
         "mode.rkt"
         "narrow.rkt"
         "parameters.rkt"
         "point.rkt"
         "recently-opened.rkt"
         "region.rkt"
         "representation.rkt"
         "region.rkt"
         "render.rkt"
         "search.rkt"
         "status-line.rkt"
         "string-utils.rkt"
         "text.rkt"
         "window.rkt"
         (prefix-in overlays: "overlays.rkt"))

;;;
;;; MOVEMENT
;;;

; beginning-of-line
;   move point to beginning of line (logical line, not screen line)
(define-interactive (beginning-of-line [m (get-point)])
  (goto-char (line-beginning-position m) m))

(define-interactive (end-of-line [m (get-point)])
  (goto-char (line-end-position m) m))

(define-interactive (move-to-column n [m (get-point)])
  ; n = numprefix
  (define-values (start end) (mark-line-span m))
  (goto-char (clamp start (+ start n) end) m))

(define-interactive (forward-char [n 1] #:keep-region [keep? #f])
  (define m (get-point))
  (check-mark m)
  (unless keep? (deactivate-region-mark))
  (define-values (start end) (point-min+max))
  (define i (clamp start (+ (mark-position m) n) end))
  (mark-move-to-position! m i))

(define-interactive (backward-char [n 1] #:keep-region [keep? #f])
  (forward-char (- n) #:keep-region keep?))

(define-interactive (forward-line [n 1])
  ; Move point to start of text line i+n, where i is the current line.
  (deactivate-region-mark)
  (cond
    [(< n 0) (backward-line (- n 1))]
    [(= n 0) (beginning-of-line)]
    [else    (define b (current-buffer))
             (let loop ([n n] [i (position (get-point))])
               (define-values (start end) (buffer-line-span b i))
               (cond [(= n 0) (goto-char start)]
                     [else    (loop (- n 1) (+ end 1))]))]))

(define-interactive (backward-line [n 1])
  ; Move point to start of text line i-n, where i is the current line.
  (deactivate-region-mark)
  (cond
    [(< n 0) (forward-line (- n 1))]
    [(= n 0) (beginning-of-line)]
    [else    (define b (current-buffer))
             (let loop ([n n] [i (position (get-point))])
               (define-values (start end) (buffer-line-span b i))
               (cond [(= n 0) (goto-char start)]
                     [else    (loop (- n 1) (- start 1))]))]))

(define-interactive (next-line [n 1])
  ; Moves point one screen line down. Attempts to keep the column.
  (deactivate-region-mark)
  (define point (get-point))
  (define (next-line1)
    (define n          (local screen-line-length))
    (define len        (line-length (mark-line point)))
    (define col        (mark-column point))
    (define screen-col (remainder col n))
    (define whole      (quotient  len n))   ; number of full screen lines
    (cond       
      [(and (not (mark-on-last-line? point))
            (>= col (* whole n)))             ; we need to move to the next text line
       (beginning-of-line)
       (forward-line)        
       (move-to-column (min screen-col (line-length (mark-line point))))]
      [(<= (+ col n) len)   ; there is room to move an entire screen line (same text line)
       (move-to-column (+ col n))]
      [else          ; there is not room to move an entire line, so go to end of this text line
       (move-to-column (line-length (mark-line point)))]))
  (cond
    [(> n 0) (for ([_ n]) (next-line1))]
    [(< n 0) (previous-line (- n))]
    [else    (void)]))

(define (on-first-line?)
  (define b (current-buffer))
  (define m (get-point))
  (cond
    [(buffer-restricted? b) (on-same-line? m (buffer-restriction-start b))]
    [else                   (mark-on-first-line? m)]))

(define-interactive (previous-line [n 1])
  ; Moves point one screen line up.
  (deactivate-region-mark)
  (define point (get-point))
  (define (previous-line1)
    (cond
      [(on-first-line?)            (void)]
      [else (define n              (local screen-line-length))
            (define len            (line-length (mark-line point)))
            (define col            (mark-column point))
            (define screen-col     (remainder col n))
            (define whole          (quotient  len n))   ; number of full screen lines
            (cond
              [(>= col n)   ; room to move entire screen line (same text line)
               (move-to-column (- col n))]
              [else               
               (beginning-of-line)
               (backward-line)
               (define prev-len (line-length (mark-line point)))
               (move-to-column (+ (- prev-len (remainder prev-len n))
                                  (min screen-col (remainder prev-len n))))])]))
  (cond
    [(> n 0) (for ([_ n]) (previous-line1))]
    [(< n 0) (next-line (- n))]
    [else    (void)]))

(define (forward-char-predicate pred)
  ; Skip ahead until (pred (char-after-point)) is no longer true
  (let loop ()
    (define c (char-after-point))
    (cond
      [(not c)  (void)]
      [(pred c) (forward-char) (loop)]
      [else     (void)])))

(define (backward-char-predicate pred)
  ; Skip ahead until (pred (char-after-point)) is no longer true
  (let loop ()
    (define c (char-before-point))
    (cond
      [(not c)  (void)]
      [(pred c) (backward-char) (loop)]
      [else     (void)])))

(define-interactive (forward-word [n 1])
  (deactivate-region-mark)  
  (define (forward-word1)
    (forward-char-predicate (λ (c) (not (char-alphabetic? c))))
    (forward-char-predicate char-alphabetic?))
  (cond
    [(= n 1) (forward-word1)]
    [(= n 0) (void)]
    [(< n 0) (backward-word (- n))]
    [else    (for ([_ n]) (forward-word1))]))

(define-interactive (backward-word [n 1])
  (deactivate-region-mark)
  (define (backward-word1)
    (backward-char-predicate (λ (c) (not (char-alphabetic? c))))
    (backward-char-predicate char-alphabetic?))
  (cond
    [(= n 1) (backward-word1)]
    [(= n 0) (void)]
    [(< n 0) (forward-word (- n))]
    [else    (for ([_ n]) (backward-word1))]))

(define-interactive (page-down)
  (define w          (current-window))
  (define point      (buffer-point (window-buffer w)))
  (define start-mark (window-start-mark w))
  (define end-mark   (window-end-mark   w))
  (define start-row  (mark-row start-mark))
  (define end-row    (mark-row end-mark))  
  (define delta      (max 0 (- (number-of-lines-on-screen w)
                               (current-next-screen-context-lines))))
  (mark-move-down! point      delta)
  (mark-move-down! start-mark delta)
  (mark-move-down! end-mark   delta)
  (refresh-frame))

(define-interactive (page-up)
  (define w          (current-window))
  (define point      (buffer-point (window-buffer w)))
  (define start-mark (window-start-mark w))
  (define end-mark   (window-end-mark   w))
  (define start-row  (mark-row start-mark))
  (define end-row    (mark-row end-mark))  
  (define delta      (max 0 (- (number-of-lines-on-screen w)
                               (current-next-screen-context-lines))))
  (mark-move-up! point      delta)
  (mark-move-up! start-mark delta)  
  (mark-move-up! end-mark   delta)
  (refresh-frame))

;;;
;;; MARK AND POINT - REGION
;;;


(define-interactive (command-set-mark)
  (buffer-set-mark-to-point (current-buffer)))

(define-interactive (exchange-point-and-mark)
  (unless (get-mark) (new-mark (current-buffer) "*mark-pm*")) ; TODO
  (buffer-exchange-point-and-mark! (current-buffer))
  (mark-activate! (get-mark)))

(define-interactive (mark-word) ; Set mark after next word (doesn't move point)
  (define m (get-mark))
  (define swap exchange-point-and-mark)
  (cond [(and m (mark-active? m))
         (with-saved-point
             (cond
               [(mark<= (get-point) m) (swap) (forward-word)  (swap)]
               [else                   (swap) (backward-word) (swap)]))]
        [else              (command-set-mark) (mark-word)]))


(define (prepare-extend-region)
  (unless (mark-active? (get-mark))
    (set-mark (get-point))))

(define-syntax (save-region-activity stx)
  (syntax-parse stx
    [(_save-region-activity body ...)
     (syntax/loc stx
       (let* ([m (get-mark)] [active? (mark-active? m)])
         (begin0 (begin body ...)
                 (set-mark-active?! m active?))))]))

(define-interactive (backward-char/extend-region)
  (check-mark (get-point))
  (prepare-extend-region)
  (save-region-activity
   (backward-char +1)))
(define-interactive (forward-char/extend-region)
  (check-mark (get-point))
  (prepare-extend-region)
  (save-region-activity
   (forward-char +1)))
(define-interactive (previous-line/extend-region [n 1])
  (prepare-extend-region)
  (save-region-activity
   (previous-line n)))
(define-interactive (next-line/extend-region [n 1])
  (prepare-extend-region)
  (save-region-activity
   (next-line n)))
(define-interactive (forward-word/extend-region) 
  (prepare-extend-region)
  (save-region-activity
   (forward-word)))
(define-interactive (backward-word/extend-region)
  (prepare-extend-region)
  (save-region-activity
   (backward-word)))
(define-interactive (beginning-of-line/extend-region)
  (prepare-extend-region)
  (save-region-activity
   (beginning-of-line)))
(define-interactive (end-of-line/extend-region)
  (prepare-extend-region)
  (save-region-activity
   (end-of-line)))

;;;
;;; BUFFER
;;; 

;;; Buffer Movement
(define-interactive (beginning-of-buffer)
  (goto-char (point-min)))
(define-interactive (end-of-buffer)
  (goto-char (point-max)))
(define-interactive (end-of-buffer/extend-region)
  (prepare-extend-region)
  (end-of-buffer))
(define-interactive (beginning-of-buffer/extend-region)
  (prepare-extend-region)
  (beginning-of-buffer))

;;; Buffer Input and Output
(define-interactive (save-buffer [b (current-buffer)])
  (save-buffer! b)
  (add-recently-opened-file (buffer-path b))
  (refresh-frame))
(define-interactive (save-buffer-as [b (current-buffer)])
  (save-buffer-as! b)
  (add-recently-opened-file (buffer-path b))
  (refresh-frame))
(define-interactive (save-some-buffers  [b (current-buffer)])
  (add-recently-opened-file (buffer-path b))
  (save-buffer b)) ; todo : ask in minibuffer

(define-interactive (open-file-or-create [path (finder:get-file)])
  (when path ; #f = none selected
    ; open buffer and display it in window
    (define b (buffer-open-file-or-create path))
    (window-switch-buffer! (current-window) b)
    ; (todo -switch-start-and-end-marks-here-) ; xxx
    ; Problem: The start and end marks are currently owned by the window.
    ;          Should they be be in the buffer instead?
    ;          Or should we delete them, change their buffer, and add them to the new buffer?  
    (current-buffer b)    
    ; set mode
    (cond [(file-path->mode-function path) => (λ (mode) (mode))]
          [else                               (fundamental-mode)])
    (define f (current-frame))
    ; update recent files
    (add-recently-opened-file path)
    ; make sure a mode change is seen in the status line below
    (send (frame-status-line f) set-label (status-line-hook))
    (refresh-frame f)))

(define (open-file-in-current-window path)
  (when (file-exists? path)
    ; open buffer and display it in window
    (define b (buffer-open-file-or-create path))
    (window-switch-buffer! (current-window) b)
    (current-buffer b)
    ; set mode
    (cond [(file-path->mode-function path) => (λ (mode) (mode))]
          [else                               (fundamental-mode)])
    (define f (current-frame))
    ; update recent files
    (add-recently-opened-file path)
    ; make sure a mode change is seen in the status line below
    (send (frame-status-line f) set-label (status-line-hook))
    (refresh-frame f)))


;;; Buffer Navigation
(define-interactive (next-buffer) ; show next buffer in current window
  (define w (current-window))
  (define b (get-next-buffer))
  (window-switch-buffer! w b)
  (current-buffer b))

(define-interactive (previous-buffer) ; show next buffer in current window
  (define w (current-window))
  (define b (get-previous-buffer))
  (window-switch-buffer! w b)
  (current-buffer b))

(define (switch-to-buffer buffer-or-name)
  "Switch to the given buffer in the current window."
  (define b buffer-or-name)
  (when (string? buffer-or-name)
    (set! b (get-buffer b)))
  (when (buffer? b)
    (window-switch-buffer! (current-window) b)
    (current-buffer b)))

;;;
;;; WINDOWS (27.2 Windows and Frames)
;;; 

(define (focus-window [w (current-window)])
  (send (window-canvas w) focus))

(define-interactive (other-window) ; switch current window and buffer
  (define ws (frame-window-tree (current-frame)))
  (define w (list-next ws (current-window) eq?))
  (current-window w)
  (unless (window? w) (error))
  (current-buffer (window-buffer w))
  (focus-window w))

(define-interactive (delete-window [w (current-window)])
  (window-delete! w))

(define-interactive (delete-other-windows [w (current-window)])
  (define ws (frame-window-tree (window-frame w)))
  (for ([win (in-list ws)])
    (unless (eq? w win)
      (delete-window win)))
  (refresh-frame))

(define (window-list [frame (current-frame)])
  "Return list of windows in the frame.\n
   If frame is #f or omitted the current frame is used."
  (set! frame (or frame (current-frame)))
  (frame->windows frame))

; See window.rkt
#;(define (get-buffer-window-list [buffer-or-name #f])
  "Return list of all windows displaying the buffer."
  (set! buffer-or-name (or buffer-or-name (current-buffer)))
  (define this?
    (cond [(buffer? buffer-or-name) (λ (b) (eq?                 b  buffer-or-name))]
          [else                     (λ (b) (equal? (buffer-name b) buffer-or-name))]))
  (for/list ([w (window-list)]
             #:when (this? (window-buffer w)))
    (window-buffer w)))

(define (buffer-visible? [buffer-or-name #f])
  "Is the buffer visible in the current frame?"
  (not (empty? (get-buffer-window-list buffer-or-name))))
         

;;;
;;; FRAME
;;; 

(define-interactive (maximize-frame [f (current-frame)]) ; maximize / demaximize frame
  (when (frame? f)
    (define f% (frame-frame% f))
    (when (is-a? f% frame%)
      (send f% maximize (not (send f% is-maximized?))))))

;;;
;;;
;;;

(define (position->index pos)
  (if (mark? pos) (mark-position pos) pos))

(define (check-position who what)
  (unless (or (number? what) (mark? what))
    (error who "expected a position (index or mark), got ~a" what)))

(define-interactive (goto-char pos [m #f])
  (define who 'goto-char)
  (check-position 'goto-char pos)
  (define i (position pos))
  (unless (and (integer? i) (>= i 0)) (error who (~a "expected index, got " i)))
  (let ([i (clamp (point-min) i (point-max))]) ; handles narrowing
    (cond
      [m    (mark-move-to-position! m           i)]
      [else (mark-move-to-position! (get-point) i)])))


(define-interactive (set-mark pos)
  (check-position 'set-mark pos)
  (define mb (current-buffer))
  (define m  (buffer-the-mark mb))
  (define b (and (mark? pos) (mark-buffer pos)))
  (cond
    ; the two marks belong to the same buffer
    [(and b (eq? b mb))
     ; set position
     (set-mark-position! m (mark-position pos))
     ; and link
     (remove-mark-from-linked-line! m (mark-link m))
     (define l (mark-link pos))
     (set-mark-link! m l)
     (add-mark-to-linked-line! m l)
     (mark-activate! m)]
    [else
     (with-saved-point
       (goto-char pos)
       (mark-activate!
        (buffer-set-mark-to-point)))]))


(define-interactive (text-scale-adjust m)
  ; (displayln `(text-scale-adjust ,m))
  (font-size (min 40 (max 1 (+ (font-size) m)))))


; create-new-buffer :  -> void
;   create new buffer and switch to it
(define-interactive (create-new-buffer [title "Untitled"])
  (define b (new-buffer (new-text) #f (generate-new-buffer-name title)))
  (window-switch-buffer! (current-window) b)
  (current-buffer b)
  (refresh-frame (current-frame)))



; eval-buffer : -> void
;   Read and evaluate each s-expression in the current buffer one at a time.
(define-interactive (eval-buffer)
  (define b  (current-buffer)) ; we get the buffer here
  ; (displayln (list 'eval-buffer 'before: (current-buffer)))
  (define t  (buffer-text b))
  (define s  (text->string t))
  (define in (open-input-string s))
  (define ns (buffer-locals b))
  (define (read1 in)
    (define stx (read-syntax 'read-from-buffer in))
    (if (syntax? stx)
        (namespace-syntax-introduce stx)
        stx))
  #;(; a few experiments that show current-buffer is not set, when ns is used.
     (parameterize ([current-namespace ns])
       (displayln (list 'eval-buffer 'later (current-buffer))))
     (parameterize ([current-namespace ns])
       (displayln (list 'eval-buffer 'later2 (eval-syntax #'(current-buffer) ns))))
     (parameterize ([current-namespace ns] [current-buffer    b])    
       (displayln (list 'eval-buffer 'later3 (eval-syntax #'(current-buffer) ns))))
     (parameterize ([current-namespace ns] [current-buffer    b])
       (eval `(current-buffer ,b) ns)
       (displayln (list 'eval-buffer 'later3 (eval-syntax #'(current-buffer) ns))))
     (for ([stx (in-port read1 in)])
       (displayln (eval-syntax stx ns))))
  
  (parameterize ([current-namespace ns])
    (localize ([current-buffer    b])
     ; probably eof    
    (for ([stx (in-port read1 in)])
      (with-handlers
          (#;[exn:fail? (λ (e)
                        ; position
                        (define pos (syntax-position stx))
                        (set! pos (and pos (- pos 1))) ; syntax positions count from 1
                        ; expression
                        (define str   (syntax->string stx))
                        (define datum (syntax->datum  stx))
                        (when (list? datum) (set! str (string-append "\"" str "\"")))
                        ; display it
                        (displayln "Error: Exception triggered during evaluation.")
                        (display   "Exn:   ") (displayln e)
                        (display   "Pos:   ") (displayln pos)
                        (display   "Stx:   ") (displayln stx)
                        (display   "Expr:  ") (displayln str))])
        (eval `(current-buffer ,b) ns)  ; sigh - the parameterize doesn't set current-buffer
        (displayln (eval-syntax stx ns)))))))

(define-interactive (test-buffer-output)
  (define b (new-buffer (new-text) #f (generate-new-buffer-name "*output*")))
  (define p (make-output-buffer b))
  (window-switch-buffer! (current-window) b)
  (parameterize ([current-output-port p])
    (localize ([current-buffer      b])
      (thread
       (λ ()
         (let loop ([n 0])
           (displayln n)
           (sleep 1.)
           (loop (+ n 1))))))))

; (self-insert-command k) : -> void
;   insert character k and move point
(define ((self-insert-command k))
  (when (use-region?) (delete-region))
  (define pa (current-prefix-argument))
  (define i (or (and (integer? pa) (positive? pa) pa) 1))
  (define b (current-buffer))
  (define point (get-point))
  (for ([_ (in-range i)])
    ; insert character
    (buffer-insert-char! b point k)
    ; does the character break the line - if so maybe call auto-fill
    (cond [(and (local auto-fill-mode?)
                (set-member? (local auto-fill-chars) k)                
                (or (local auto-fill-function) normal-auto-fill-function))
           => (λ (fill) (fill))])))

(define-interactive (insert-newline)  
  (buffer-break-line! (current-buffer) (get-point)))

(define-interactive (break-line)    ; called newline in Emacs
  ; Insert newline at before point. 
  ; If the column of point is greater than fill-column, auto-fill-function is called.
  ; If the new line is blank, move to left-margin.
  (cond [(and (local auto-fill-mode?)
              (fill-needed?)
              (or (local auto-fill-function)
                  normal-auto-fill-function))
         => (λ (fill) (fill))]
        [else (insert-newline)])
  (cond
    [(and (blank-line?) (local left-margin)) ; move to left-margin ?
     => insert-spaces]))

(define (blank-line?)
  ; Is the line containing point blank?
  (with-saved-point
    (beginning-of-line)
    (looking-at #px"^[[:blank:]]*$")))
  

(define-interactive (insert-line-after)
  ; insert new line after the current line,
  ; place point at beginning of new line
  ; [Sublime: cmd+enter]
  (end-of-line)
  (insert-newline))

(define-interactive (insert-line-before)
  ; insert new line before the current line,
  ; place point at beginning of new line
  ; [Sublime: cmd+enter]
  (beginning-of-line)
  (insert-newline)
  (backward-char))

(define-interactive (delete-region [start #f] [end #f])
  (region-delete start end))

; backward-delete-char
;   Delete n characters backwards.
;   If n=1 and region is active, delete region.
(define-interactive (backward-delete-char [n 1])
  (define b (current-buffer))
  (cond
    [(and (= n 1) (use-region?))  (delete-region)]
    [(not (buffer-restricted? b)) (buffer-delete-backward-char! b (get-point) n)]
    [else                         (define limit (- (point) (point-min)))
                                  (buffer-delete-backward-char! b (get-point) (min limit n))]))

; select all
(define-interactive (mark-whole-buffer [b (current-buffer)])
  (localize ([current-buffer b])
    (end-of-buffer)
    (command-set-mark)
    (beginning-of-buffer)))

(define-interactive (kill-line)
  (buffer-kill-line)
  (update-current-clipboard-at-latest-kill)
  (refresh-frame))

;;;
;;; KILLING LINES
;;;

; buffer-kill-whole-line : [buffer] -> void
;   kill whole line including its newline
(define (buffer-kill-whole-line [b (current-buffer)])
  (localize ([current-buffer b])
    (beginning-of-line b)
    (buffer-kill-line b #t)  
    (forward-char)
    (backward-delete-char)))

; mark-kill-line-to-beginning : mark -> void
;   Kill text from the given mark to beginning of line.
;   If mark is at the beginning of line, the newline is deleted.
;   The mark is at the beginning of line, if text from the given mark to newline is all whitespace.
(define (mark-kill-line-to-beginning [m (get-point)])
  ; TODO : store deleted text in kill ring
  (define p1 (mark-position m))
  (define p2 (line-beginning-position m))
  (define rest-of-line (subtext->string (buffer-text b) p2 p1))
  ; delete to beginning of line
  (define b  (mark-buffer m))
  (let ([old (buffer-set-the-mark b m)])
    (error 'mark-kill-line-to-beginning "todo")
    #;(beginning-of-line b)
    #;(region-delete b)
    ; maybe delete newline
    #;(when (and (string-whitespace? rest-of-line)
                 (not (= (mark-position m) 0)))
        (buffer-backward-delete-char! b))))


(define-interactive (kill-whole-line)
  (buffer-kill-whole-line)
  (update-current-clipboard-at-latest-kill)
  (refresh-frame))

(define-interactive (kill-line-to-beginning)
  (mark-kill-line-to-beginning)
  (update-current-clipboard-at-latest-kill)
  (refresh-frame))

(define-interactive (recenter-top-bottom)
  (maybe-recenter-top-bottom #t))

(define-interactive (insert-latest-kill)
  ; If another application has put any text onto the system clipboard
  ; later than the latest kill, that text is inserted.
  ; Note: The timestamp is ignored in OS X.
  (define time 0) ; current time
  (define s (send the-clipboard get-clipboard-string time))
  (when (use-region?) (backward-delete-char))
  (cond
    [(or (equal? s "") 
         (equal? s (current-clipboard-at-latest-kill)))
     ; no changes to the system clipboard, so latest kill is used
     (buffer-insert-latest-kill)]
    [else
     ; system clipboard is newer
     (buffer-insert-string! (current-buffer) (get-point) s)]))

(define-interactive (copy-region)
  (update-current-clipboard-at-latest-kill)
  (kill-ring-push-region))

(define-interactive (kill-word)
  ; kill to end of word
  (deactivate-region-mark)
  (forward-word/extend-region)
  (kill-region))

(define-interactive (backward-kill-word)
  ; Kill to beginning of word
  (deactivate-region-mark)
  (backward-word/extend-region)
  (kill-region))

;;;
;;; MODES
;;;

; The buffer-local variable  major-mode  holds a symbol representing the major mode.
; Example: the symbol 'fundamental-mode represents the fundamental mode.


(define-interactive (fundamental-mode [b (current-buffer)])
  (set-major-mode! 'fundamental)
  (set-mode-name!  "Fundamental")
  ; add all interactive commands defined here to fundamental mode
  (for ([(name cmd) (in-hash all-interactive-commands-ht)])
    (define sym (string->symbol name))
    (set-buffer-local! sym cmd b)))

(define-interactive (text-mode [b (current-buffer)])
  (fundamental-mode b)       ; add all commands from fundamental mode
  (set-major-mode! 'text)
  (set-mode-name!  "Text"))

(define-interactive (test)
  (define b (current-buffer))
  (mark-whole-buffer b)  
  ; (displayln (text->string (buffer-text b)))
  ; (buffer-insert-string-before-point! (get-point) "x")
  (displayln (text->string (buffer-text b))))



;;;
;;; INSERTION
;;;

; https://www.gnu.org/software/emacs/manual/html_node/elisp/Insertion.html

(define-interactive (insert . strings-and-characters)
  (define b (current-buffer))
  ; Insert strings and characters in the current buffer.
  ; Insert at point, moving the point forward (the insertation is done before point).
  (define strings (for/list ([x strings-and-characters])
                    (cond [(string? x) x]
                          [(char? x)   (string x)]
                          [else        (error 'insert
                                              "expected strings and characters as input, got ~a"
                                              x)])))  
  (buffer-insert-string! b (get-point) (string-append* strings)))


(define-interactive (insert-char character [count 1])
  ; Insert the character count times before point.
  (insert (make-string count character)))

; (define (insert-before-markers) ...)
; (define (insert-buffer-substring from-buffer-or-name &optional start end) ...)
; (define (insert-buffer-substring-no-properties from-buffer-or-name &optional start end)

(define (insert-property! sym val [val-end val])
  (define b (current-buffer))
  (buffer-insert-property! b sym val val-end))

;;;
;;; INDENTATION
;;;


;;; 32.17.1 Indentation Primitives

(define (current-indentation)
  "Column of first non-blank character. "
  "If line is all blank, then the position of the end of line is returned"
  (with-saved-point
    (beginning-of-line)
    (if (blank-line?)
        (line-end-position)
        (forward-whitespace))))

(define (indent-to column [minimum #f])
  "Indent from point to column."
  "If minimum is present, insert at least minimum spaces even if column is exceeded."
  "If point is past column do nothing (except when minimum is given)."
  "Return column of position where indentation ends."
  (define c      (current-column))  
  (define amount (max (if (< c column) (- column c) 0)
                      (or minimum 0)))
  (insert (make-string amount #\space))
  (+ c amount))

;;; 24.1 Indentation Commands

(define-interactive (back-to-indentation)
  "Move to first non-blank character on line."
  (beginning-of-line)
  (let loop ()
    (unless (eol?)
      (when (blank? (char-category (char-after-point)))
        (forward-char)
        (loop)))))

(define (change-indentation column)
  (back-to-indentation)
  (define cur (current-column))
  (define n   (abs (- cur column)))
  (cond
    [(< column cur) (delete-region column (point))]
    [(= column cur) (void)]
    [(> column cur) (insert (make-string n #\space))]))
  
(define-interactive (split-line)
  "Split line at two at point. Keep point at first line, indent second line to align with point."
  ; TODO: Insert fill prefix on new line.
  (define c (current-column))
  (break-line)
  (backward-char)
  (with-saved-point
    (forward-char)
    (change-indentation c)))

(define-interactive (indent-region)
  (define b (current-buffer))
  ; Indent line or region.
  ; Use `indent-line-function` to indent each line.
  (define indent-line (local indent-for-tab))
  (cond
    [(use-region?)
     (define beg     (region-beginning))
     (define end     (region-end))
     ; indenting the lines in the region affects the position of end,
     ; so we need a mark that moves along.
     (define end-m   (new-mark b "*temp" end))
     (backward-char -1 end-m)
     ; go to the beginning of the region and indent each line
     (goto-char beg)
     (let loop ()
       (when (< (point) (mark-position end-m))
         (indent-line)
         (when (< (point) (mark-position end-m))
           (beginning-of-line)
           (forward-line)
           (loop))))
     ; clean up
     (delete-mark! end-m)
     #;(back-to-indentation)]
    [else
     (indent-line)]))

(define (on-same-line? pos1 pos2 [b (current-buffer)])
  (define p1 (position pos1))
  (define p2 (position pos2))
  (define t (buffer-text b))
  (text-on-same-line? t p1 p2))

;;; Indentation helpers

(define-interactive insert-space
  (self-insert-command #\space))

(define-interactive (insert-spaces [n 1])
  (for ([i n]) (insert-space)))

(define-interactive (insert-tab-as-spaces)
  (define (next-greater-in-list y xs default) ; find first element in xs, greater than y
    (define gs (filter (λ (x) (not (<= x y))) (sort xs <)))
    (cond [(null? gs) default] [else (first gs)]))
  (define (next-greater y m)  ; find first integer z greater than y such that m divides z
    (if (zero? (remainder (+ y 1) m)) (+ y 1) (next-greater (+ y 1) m)))
  (define w    (local tab-width))
  (define tabs (local tab-stop-list))

  (unless (and (integer? w) (<= 0 w))
    (message "insert-tab-as-spaces: tab-width is not set to a positive integer, using 8 as tab-width")
    (set! w 8))
  (unless (or (eq? tabs #f) (and (list? tabs) (andmap integer? tabs)))
    (message "insert-tab-as-spaces: tab-stop-list not #f or a list of natural numbers")
    (set! tabs #f))

  (define col               (mark-column (get-point)))
  (define next-by-width     (next-greater col w))
  (define next-tab-stop     (cond [tabs (next-greater-in-list col tabs next-by-width)]
                                  [else next-by-width]))
  (insert-spaces (- next-tab-stop col)))





;;;
;;; 7.7 Blank Lines
;;;

(define-interactive (open-line) ; C-o
  ; Insert newline after point.
  (insert-newline)
  (backward-char)  
  (when (and (local fill-prefix)
             (= (position) (line-beginning-position)))
    (insert (local fill-prefix))))

;;;
;;; 7.9 Cursor Position Information
;;;

(define-interactive (what-line)
  ; Shows line number relative to the accessible portion.
  ; Note: Rewrite when narrowing is introduced.
  (message (~a "Line: " (+ (mark-row (get-point)) 1))))

(define-interactive (line-number-mode)
  ; Toggle line number mode
  (define new (not (local line-number-mode?)))
  (local! line-number-mode? new)
  (message (cond [new  "Line-Number mode enabled"]
                 [else "Line-Number mode disabled"])))

(define-interactive (column-number-mode)
  ; Toggle column number mode
  (define new (not (local column-number-mode?)))
  (local! column-number-mode? new)
  (message (cond [new  "Column-Number mode enabled"]
                 [else "Column-Number mode disabled"])))

(define-interactive (hl-line-mode)
  ; Toggle hl-line-mode?
  (define new (not (local hl-line-mode?)))
  (local! hl-line-mode? new)
  (message (cond [new  "Highlight line mode enabled"]
                 [else "Highlight line mode disabled"])))



; TODO M-=     (define (count-words-region) ...)

        

(define (following-char) ; TODO: use char-after-point instead?
  ; Return character at point.
  ; If point at end position, return #f.
  (define p (mark-position (get-point)))
  (define t (buffer-text (current-buffer)))
  (cond [(>= p (- (text-length t) 1)) #f]
        [else                        (define s (subtext->string t p (+ p 1)))
                                     (if (not (zero? (string-length s)))
                                         (string-ref s 0)
                                         #f)]))

(define-interactive (what-cursor-position) ; C-x =
  (define pos   (mark-position (get-point)))
  (define len   (max 1. (* 1. (buffer-length (current-buffer)))))
  (define pct   (inexact->exact (floor (* (/ (+ pos 1.) len) 100))))
  (define c     (following-char))
  (cond
    [c (define i     (char->integer c))
       (define octal (number->string i 8))
       (define hex   (number->string i 16))
       (message (~a "Char: " c " "
                    "(" i ", #o" octal ", #x" hex ") "
                    "point=" (+ pos 1) " of " len " (" pct "%)"))]
    [else
     (message (~a "point=" (+ pos 1) " of " len " (EOB) "))]))
  
;;;
;;; 25.6 Filling Text
;;;

; When a fill prefix is set, then the fill fill commands will
; remove the prefix from each line before filling, and insert it after filling.

(define-interactive (move-to-left-margin)
  (move-to-column (local left-margin)))

(define-interactive (set-fill-prefix)
  ; make from point to beginning of line
  (define b (current-buffer))
  ; (with-saved-point (beginning-of-line) (set-mark))
  (define pos             (position))
  (define left-margin-pos (with-saved-point (move-to-left-margin) (position)))
  (cond
    [(> pos left-margin-pos) (define s (buffer-substring b left-margin-pos pos))
                             (local! fill-prefix (if (equal? s "") #f s))]
    [else                    (local! fill-prefix #f)])
  (if (local fill-prefix)
      (message (~a "fill-prefix: " (local fill-prefix)))
      (message     "fill-prefix canceled")))

(define-interactive (set-fill-column [column #f]) ; C-x f
  (define old (local fill-column))
  (define new (or column (mark-column (get-point))))
  (local! fill-column new)
  (message (~a "Fill column set to " new " (was " old ")"))
  new)

(define-interactive (auto-fill-mode)
  ; Toggle auto-fill-mode?  
  (if (local auto-fill-mode?) (turn-off-auto-fill) (turn-on-auto-fill)))

(define-interactive (turn-on-auto-fill)
  ; Turn auto-fill-mode on.
  (local! auto-fill-mode? #t)
  (message "Auto fill mode enabled"))

(define-interactive (turn-off-auto-fill)
  ; Turn auto-fill-mode off.
  (local! auto-fill-mode? #f)
  (message "Auto fill mode disabled"))

(define (fill-move-to-break-point)
  ; Move backward one word at a time until we are before fill-column.
  ; Return position of the break point.
  ; If there is no break point before beginning of the line, return #f.
  (define beg (line-beginning-position))
  (define end (+ beg (local fill-column)))
  (let loop ()
    (define p (position))
    (cond [(> p end)   (backward-word) (loop)]
          [(<= p beg)  #f]
          [else        (backward-skip-word-separators #:stay-on-line? #t)
                       (position)])))

(define (backward-skip-word-separators #:stay-on-line? stay?)
  (define beg (if stay? (line-beginning-position) (start-of-buffer-position)))
  (let loop ()
    (define p (position))
    (when (> p beg)
      (when (word-separator? (char-before-point))
        (backward-char)
        (loop)))))

;;; Filling

(define (fill-needed?)
  (define col (- (position) (line-beginning-position)))
  (> col (local fill-column)))

(define-interactive (normal-auto-fill-function)
  (when (fill-needed?)
    (fill-move-to-break-point)
    (insert-newline)))


;;; Positions



(define (eob?)
  ; is point at end of buffer?
  (>= (point) (end-of-buffer-position)))

(define (eol?)
  ; is point at end of line
  (= (point) (line-end-position)))

;;; Looking

(define (char-before m)
  (cond
    [(mark? m) (define l (mark-line m))
               (define c (mark-column m))
               (cond [(= (position m) (point-min)) #f]
                     [(not c)                      #f] ; ??
                     [(= c 0)                      #\newline]
                     [else                         (line-ref l (- c 1))])]
    [(integer? m) (if (= m (point))
                      (char-before (get-point))
                      (with-saved-point
                        (goto-char m)
                        (char-before (get-point))))]
    [else (error 'char-before "expected position")]))

(define (char-before-point)
  (char-before (get-point)))

(define (char-after [m #f])
  (cond
    [(eq? m (get-point)) (char-after-point)]
    [(and (integer? m)
          (= m (point))) (char-after-point)]
    [else                (error 'char-after "todo")]))

(define (char-after-point)
  ; (displayln 'char-after-point (current-error-port))
  (define p   (get-point))
  (define l   (mark-line p))
  (define c   (mark-column p))
  (cond [(= (position p) (point-max))              #f]
        [(not c)                                   #f]  ; empty line?
        [(= c (line-length l))                     #\newline]
        [(> c (line-length l))
         (displayln (~a "column: " c )             (current-error-port))
         (displayln (~a "len: "   (line-length l)) (current-error-port))
         (displayln (~a "line: " (line->string l)) (current-error-port))
         (displayln (~a "point: "  (point))        (current-error-port))
         (displayln p     (current-error-port))
         (error 'char-after-point "internal error")]
        [else                                      (line-ref l c)]))

;;; Syntax Categories
; Syntax categories are called syntax classes in Emacs

(struct syntax-category ())
(struct opener             syntax-category (close)) ; ( [ {
(struct closer             syntax-category (open))  ; ) ] }
(struct blank              syntax-category ())      ; space, tab
(struct comment-ender      syntax-category ())      ; newline
(struct comment-starter    syntax-category ())      ; ;
(struct string-starter     syntax-category (ender)) ; "
(struct symbol-constituent syntax-category ())      ; a-z A-Z 0-9
(struct expression-prefix  syntax-category ())      ; ' , # 

(define syntax-category-ht
  (make-hasheqv (list (cons #\(       (opener #\)))
                      (cons #\[       (opener #\]))
                      (cons #\{       (opener #\}))
                      (cons #\)       (closer #\())
                      (cons #\]       (closer #\[))
                      (cons #\}       (closer #\{))
                      (cons #\"       (string-starter #\"))
                      (cons #\space   (blank))
                      (cons #\tab     (blank))
                      (cons #\newline (comment-ender))
                      (cons #\;       (comment-starter))
                      (cons #\'       (expression-prefix))
                      (cons #\,       (expression-prefix))
                      (cons #\#       (expression-prefix))))) ; @ ?

(define (char-category c)
  (hash-ref syntax-category-ht c #f))
(define (add-char-range from to cat)
  (for ([c (in-range (char->integer from) (char->integer to))])
    (hash-set! syntax-category-ht (integer->char c) cat)))
(add-char-range #\a #\z (symbol-constituent))
(add-char-range #\A #\Z (symbol-constituent))
(add-char-range #\0 #\9 (symbol-constituent))


(define (backward-whitespace)
  "Place the point at the start of the previous whitespace sequence."
  ; 1. Find next whitespace sequence  
  (let loop ()
    (unless (<= (point) (point-min))
      (define c (char-after-point))
      (unless (blank? (char-category c))
        (backward-char)
        (loop))))
  ; 2. Skip whitespace sequence
  (let loop ()
    (unless (<= (point) (point-min))
      (define c (char-after-point))
      (when (blank? (char-category c)) 
        (backward-char)
        (loop)))))

(define (make-skip-char-category char-category-pred)
  (λ ([direction +1])
    (define start (point-min))
    (define end   (point-max))
    (define next  (if (positive? direction) forward-char backward-char))
    (let loop ()
      (when (< start (point) end)
        (define c (char-after-point))
        (when (char-category-pred (char-category c))
          (next)
          (loop))))))

(define (make-skip-unless-char-category char-category-pred)
  (make-skip-char-category (λ (c) (not (char-category-pred c)))))
  
(define skip-whitespace        (make-skip-char-category        blank?))
(define skip-unless-whitespace (make-skip-unless-char-category blank?))

(define (forward-whitespace)
  "Place the point at the end of the next whitespace sequence."
  (skip-unless-whitespace) ; Finds next whitespace sequence
  (skip-whitespace))       ; Skips whitespace sequence

(define (forward-whitespace/quotes [limit #f])
  "Move forward over any whitespace (newline), quotes or quasiquotes - not scanning past limit."
  (let loop ()
    (unless (>= (point) (or limit -inf.0))
      (define c   (char-after-point))
      (define cat (char-category c))
      (when (or (blank? cat)
                (comment-ender? cat)
                (eqv? c #\') (eqv? c #\`))
        (forward-char)
        (loop)))))

(define (backward-whitespace/quotes)
  "Move backward over any whitespace (newline), quotes or quasiquotes"
  (let loop ()
    (define c   (char-before-point))
    (define cat (char-category c))
    (when (or (blank? cat)
              (comment-ender? cat)
              (eqv? c #\') (eqv? c #\`))
      (backward-char)
      (loop))))

(define (backward-prefix-chars)
  "Move backward over any number of expression prefix characters."
  (let loop ()
    (define c (char-before-point))
    (when (expression-prefix? (char-category c))
      (backward-char)
      (loop))))

(define (forward-to-char target-char)
  "Move forward to target char."
  "Point will be to the left of the target character."
  (let loop ()
    (match (char-after-point)
      [#f               #f] ; end reached
      [(== target-char) #f] ; found
      [_                (forward-char)
                        (loop)])))

(define (skip-chars-forward str [limit #f])
  "Move forward while the next char is present in string."
  "If limit is non-false, stop at limit."
  (cond
    [(eob?)                                             (void)]
    [(and limit (>= (point) limit))                     (void)]
    [(string-contains? str (string (char-after-point))) (forward-char)
                                                        (skip-chars-forward str limit)]
    [else                                               (void)]))

(struct state (depth           ; depth in parentheses
               inner-start     ; pos of inner most start paren, #f depth=0
               last-complete   ; start of last complete sexp terminated (paren or non-paren sexp)
               inside-string   ; string ender if inside string
               inside-comment  ; #t if inside comment, number if in nestable comment
               inside-symbol   ; #t if inside symbol or number
               after-quote     ; #t if point right after quote
               comment-style
               comment-start   ; valid if inside-comment is true (position)
               string-start    ; valid if inside-string  is true (position)
               seen            ; private: holds list of expected close characters
               inner-starts    ; stack holding previous values of inner-start
               start-current   ; start position of the current expression
               )
  #:transparent)

(define (display-state s)
  (match-define (state depth inner-start last-complete
                       inside-string inside-comment inside-symbol after-quote
                       comment-style comment-start string-start seen inner-starts start-current)
    s)
  (displayln "State: ")
  (map displayln (list (list 'depth          depth)
                       (list 'inner-start    inner-start)
                       (list 'last-complete  last-complete)
                       (list 'inside-string  inside-string)
                       (list 'inside-comment inside-comment)
                       (list 'inside-symbol  inside-symbol)
                       (list 'after-quote    after-quote)
                       (list 'comment-style  comment-style)
                       (list 'comment-start  comment-start)
                       (list 'string-start   string-start)
                       (list 'seen           seen)
                       (list 'inner-starts   inner-starts)
                       (list 'start-current  start-current)))
  (void))

(require (for-syntax racket/base syntax/parse))

(define empty-state (state 0 #f #f #f #f #f #f #f #f #f '() '() #f))

(define-syntax (push! stx) (syntax-parse stx [(_push! id:id e:expr) #'(set! id (cons e id))]))
(define-syntax (pop! stx) (syntax-parse stx[(_pop! id:id) #'(begin0 (first id) (set! id (rest id)))]))
        
(define (parse-partial-sexp start limit #:state [start-state #f] #:target-depth [target-depth #f])
  ; TODO: set last-complete correctly for non-string, non-parens e.g. for a symbol
  ; (displayln (list 'parse-partial-sexp 'start start 'limit limit))
  "Parse sexp starting at the start position."
  "Parsing stops at the limit position or earlier. Point is set to the position where parsing stops. "
  "If the optional argument target-depth is set, parsing stops when the depth becomes target-depth."

  ; define the loop variables
  (match-define (state depth inner-start last-complete
                       inside-string inside-comment inside-symbol after-quote comment-style
                       comment-start string-start seen inner-starts start-current)
    (or start-state empty-state))

  (set! target-depth (or target-depth             ; target-depth is given explicitly.
                         (and start-state depth)  ; otherwise use depth from state if given,
                         0))                      ; otherwise use 0.

  (define-syntax (create-state stx)
    (syntax-parse stx [(_) (syntax/loc stx
                             (state depth inner-start last-complete inside-string inside-comment
                                    inside-symbol
                                    after-quote comment-style comment-start string-start
                                    seen inner-starts start-current))]))
  
  (define-syntax (complete-parens! stx) ; a parenthesized sexp was completed
    (syntax-parse stx [(_) #'(when inner-start
                               (set! last-complete inner-start)
                               (set! inside-symbol #f)
                               (set! start-current #f))]))
  (define-syntax (complete! stx) ; an (non-parenthesized) sub-sexp was completed
    (syntax-parse stx [(_) #'(when (and (or inside-string inside-symbol)
                                        start-current)
                               (set! last-complete start-current)
                               (set! start-current #f)
                               (set! inside-symbol #f)
                               (set! inside-string #f))]))
  (define (loop i)
    (define-syntax (new! stx)    ; possible new sexp start
      (syntax-parse stx [(_new!) #'(unless (or start-current inside-symbol)
                                     (set! inside-symbol #f)
                                     (set! start-current i))]))
    (define-syntax (new-symbol! stx)    ; possible new sexp start
      (syntax-parse stx [(_new!) #'(unless inside-symbol
                                     (set! inside-symbol #t)
                                     (set! start-current i))]))
    #; (displayln (list i (create-state)))
    (define j i)
    ; Invariant: Exactly one (skip) in all paths not returning (create-state)
    (define (skip) (set! j (+ i 1)) (forward-char))
    (cond
      [(> i limit) (error)]
      [(= i limit)
       ; Set last-complete and start-current to proper values, before returning the parse state
       (define x (char-category (char-after-point)))
       ; (displayln (list 'x (char-after-point) (char-category (char-after-point))))
       (when (and inside-string (not (symbol-constituent? x)))
         (set! inside-string #f)) ; ???
       (when (and inside-symbol (not (symbol-constituent? x)))
         (complete!))
       (create-state)]
      [else
       (define c (char-after-point))
       ; (displayln (list c (and c (char-category c))))
       ; (display (list i c)) (display " ")
       (if
        (not c)         ; end-of-buffer reached
        (create-state)
        (match depth
          [(or 0 (== target-depth))
           (cond
             [inside-string        (skip)
                                   (cond [(eqv? c inside-string) ; end of string?
                                          (set! last-complete string-start)
                                          (set! inside-string #f)
                                          (set! start-current #f)
                                          (create-state)]
                                         [else (loop j)])]
             [inside-comment       (skip)
                                   (match (char-category c)
                                     [(comment-ender) (set! inside-comment #f)]
                                     [_               (void)])
                                   (loop j)]
             [else ; outside strings and comments
              (match (char-category c)
                [(blank)              (complete!) (create-state)] ; space and tab
                [(comment-ender)      (complete!) (create-state)] ; newline
                [(string-starter e)   (complete!) ; order important here!
                                      (set! string-start i)  (set! inside-string e)
                                      (new!)
                                      (skip) (loop j)]
                [(comment-starter)    (set! comment-start i)
                                      (set! inside-comment #t)
                                      (complete!)        (skip) (loop j)]
                [(opener cp)          (complete!)
                                      (skip)
                                      (push! inner-starts inner-start)
                                      (set! inner-start i)      ; needs to be after (skip)
                                      (set! depth (+ depth 1))
                                      (set! seen (cons cp seen))
                                      (loop j)]
                [(closer _)           (skip) (create-state)] ; unexpected closer - do eat
                [(symbol-constituent) (new-symbol!) (skip) (loop j)]
                [_                    (skip) (loop j)])])]
          [d ; inside at least one parenthesis, not at target-depth
           (cond
             [inside-string        (skip)
                                   (when (eqv? c inside-string) ; end of string?
                                     (complete!) 
                                     (set! last-complete string-start)
                                     (set! inside-string #f))
                                   (loop j)]
             [inside-comment       (skip) 
                                   (match (char-category c)
                                     [(comment-ender) (set! comment-start #f)
                                                      (set! inside-comment #f)]
                                     [_               (void)])
                                   (loop j)]
             [else ; outside strings and comments
              (match (char-category c)
                [(blank)              (complete!)             (skip) (loop j)] ; space and tab
                [(comment-ender)      (complete!)             (skip) (loop j)] ; newline
                [(string-starter e)   (set! string-start i) (set! inside-string e)
                                      (complete!) (new!)      (skip) (loop j)]
                [(comment-starter)    (set! comment-start i) (set! inside-comment #t)
                                      (skip) (complete!) (loop j)]
                [(opener cp)          (complete!) (skip) 
                                      (push! inner-starts inner-start)
                                      (set! inner-start i) ; start of innermost par
                                      (set! depth (+ depth 1))
                                      (set! seen (cons cp seen))
                                      (loop j)]
                [(closer op)      (define expected (first seen))
                                  (cond [(eqv? c expected)
                                         (skip) (complete-parens!)
                                         (set! last-complete inner-start)
                                         (set! inner-start (pop! inner-starts))
                                         (set! depth (- depth 1))
                                         (set! seen (rest seen))
                                         (loop j)]
                                        [else
                                         ; (message
                                         ;   (~a "forward-sexp: expected " (first seen)))
                                         (skip) (create-state)])] ; error situation
                [(symbol-constituent) (new-symbol!)           (skip) (loop j)]                
                [_                   (skip) (loop j)])])]))]))
  (forward-whitespace/quotes limit)
  (unless start-current
    (set! start-current (point)))
  (when (> (point) limit) (error 'sigh))
  (let ([result (loop (point))])
    ; (writeln (list 'state: result))
    result))

(define (point-position) (position (get-point)))

(define (at-beginning-of-buffer?)
  (= (point-position) (point-min)))

(define-interactive (backward-to-open-parenthesis-on-beginning-of-line)
  "Moves backwards for a open parenthesis on the beginning of a line."
  (let loop ()
    (beginning-of-line)
    (unless (at-beginning-of-buffer?)
      (unless (eqv? (char-after-point) #\()
        (backward-line)
        (loop)))))

(define (parse-state-at-point)
  (define here (point))
  (backward-to-open-parenthesis-on-beginning-of-line)
  (let loop ([state empty-state])
    (cond
      [(< (point) here)
       (define old-point (point))
       (define new-state (parse-partial-sexp (point) here #:state state #:target-depth 0))
       (define new-point (point))
       (when (= new-point old-point)
         (error 'parse-state-at-point "sigh"))
       (loop new-state)]
      [else
       state])))


(define-interactive (depth-at-point)
  (message (~a "depth: " (state-depth (parse-state-at-point)))))

(define (backward-sexp)
  (backward-whitespace/quotes)
  (define s (parse-state-at-point))
  ; (display-state s)
  (match-define (state depth inner-start last-complete
                       inside-string inside-comment inside-symbol after-quote comment-style
                       comment-start string-start seen inner-starts start-current)
    s)
  (goto-char (max (or last-complete 0)
                  (or start-current 0)
                  (or (and inner-start (+ inner-start 1)) 0))))

(define-interactive (forward-sexp [n 1])
  (define (forward-sexp-1)
    ; Move over one balanced sexp.
    (define here (point))
    ; get to position where parser state is empty
    (backward-to-open-parenthesis-on-beginning-of-line)
    ; (define back (point))
    ; (displayln (list 'forward-sexp 'back-to (point)))
    ; (displayln (list 'before here 'after (point)))
    ; repeatedly call parse-partial-sexp
    ; (define start  (point))
    (define state-here
      (let loop ([state empty-state])
        (cond [(< (point) here)  ; (displayln (list 'forward-sexp 'back-to back 'parse (point) here))
                                 (loop (parse-partial-sexp (point) here
                                                           #:state state #:target-depth 0))]
              [else              state])))
    (define depth-here (state-depth state-here))
    ; (displayln (list 'forward-sexp 'back-to back 'last-parse (point) (position-of-end)))
    ; (displayln (list 'forward-sexp 'state-here state-here))
    (define end-state
      (parse-partial-sexp (point) (position-of-end)
                          #:state state-here #:target-depth depth-here))
    ; (displayln end-state)
    end-state)
  (match n
    [#f (forward-sexp-1)]
    [0  (void)]
    [1  (forward-sexp-1)]
    [n  (if (> n 1)
            (for ([_ n]) (forward-sexp-1))
            (backward-sexp (- n)))]))

(define-interactive (forward-sexp/extend-region)
  (prepare-extend-region)
  (forward-sexp)
  (mark-activate! (region-mark)))

(define-interactive (backward-sexp/extend-region)
  (prepare-extend-region)
  (backward-sexp)
  (mark-activate! (region-mark)))

(define (bob?)
  ; point at beginning of (possible narrowed) buffer?
  (= (point) (point-min)))

(define-interactive (backward-up-list)
  ; move backwards as far as possible at this level
  (let loop ([pos (point)])
    (backward-sexp)
    (unless (= pos (point))
      (loop (point))))
  ; move backwards until we go up a level
  (let loop ()
    (unless (bob?)
      (backward-char)
      (unless (opener? (char-category (char-after-point)))
        (loop)))))

(define-interactive (forward-up-list)
  ; move forwards as far as possible at this level
  (let loop ([pos (point)])
    (forward-sexp)
    (unless (= pos (point))
      (loop (point))))
  ; move forwards until we go up a level
  (let loop ()
    (unless (bob?)
      (forward-char)
      (unless (opener? (char-category (char-before-point)))
        (loop)))))
  

(define-interactive (forward-list [use-message? #t])
  "Move forward over a balanced expression.
First move forward until first opener or end of buffer.
If an opener is found, then move over to the end of a balanced expression.
If the opener doesn't belong to a balanced expression, return false."
  (define here (point)) ; return here on error
  (define (on-error msg)
    (when (and msg use-message?) (message msg))
    (goto-char here) #f)
  (define (loop i depth seen)
    (define j (+ i 1))
    (define c (char-after-point))
    (unless (eob?)
    (if c
        (match depth
           [0 (match (char-category c)
                [(blank)             #f]
                [(closer _)          #f]
                [(comment-ender)     #f]
                [(string-starter e)  #f]
                [(opener cp)         (forward-char)
                                     (loop j (+ depth 1) (cons cp seen))]             
                [_                   #f])]
           [d (match (char-category c)
                [(opener cp)         (forward-char)
                                     (loop j (+ depth 1) (cons cp seen))]
                [(closer op)         (cond [(eqv? c (first seen)) (forward-char)
                                                                  (unless (= depth 1)
                                                                    (loop j (- depth 1) (rest seen)))]
                                           [else
                                            (on-error (~a "forward-list: expected " (first seen)
                                                          " at position " (point)))])]
                [_                   (forward-char)
                                     (loop j depth seen)])])
        (on-error "expected a closer before end of buffer"))))
  (define (forward-non-opener)
    (define c (char-after-point))
    (cond
      [(eob?)          #f] ; end of buffer
      [(not c)         #f] ; end of buffer
      [(not (char? c)) #f] ; surprise ?
      [(char? c)
       (match (char-category c)
         [(opener cp) (void)]
         [(closer op) (on-error (~a "containing expression ends prematurely at position " (point)))]
         [_           (forward-char)
                      (forward-non-opener)])]))
  (forward-non-opener)
  (define c (char-after-point))
  (cond
    [(and c (opener? (char-category c)))  (loop 0 0 '())]
    [else                                 #t]))

(define-interactive (backward-list [use-message? #t])
  "Move backward over a balanced expression.
First move backward until first opener or end of buffer.
If an opener is found, then move over to the beginning of a balanced expression.
If the closer doesn't belong to a balanced expression, return false."
  (define here (point)) ; return here on error
  (define (on-error msg)
    (when (and msg use-message?) (message msg))
    (goto-char here) #f)
  (define (loop i depth seen)
    (define j (+ i 1))
    (define c (char-before-point))
    (if c
         (match depth
           [0 (match (char-category c)
                [(blank)             #f]
                [(comment-ender)     #f]
                [(string-starter e)  #f]
                [(opener _)          #f]
                [(closer op)         (backward-char)
                                     (loop j (+ depth 1) (cons op seen))]             
                [_                   #f])]
           [d (match (char-category c)
                [(closer op)         (backward-char)
                                     (loop j (+ depth 1) (cons op seen))]
                [(opener cp)         (cond [(eqv? c (first seen)) (backward-char)
                                                                  (unless (= depth 1)
                                                                    (loop j (- depth 1) (rest seen)))]
                                           [else
                                            (on-error (~a "backward-list: expected " (first seen)
                                                          " at position " (point)))])]
                [_                   (backward-char)
                                     (loop j depth seen)])])
         (on-error "expected opener before buffer start")))
  (define (backward-non-opener)
    (define c (char-before-point))
    (cond
      [(not c)         (void)] ; end of buffer
      [(not (char? c)) (void)] ; surprise ?
      [(char? c)
       (match (char-category c)
         [(closer op) (void)]
         [(opener cp) (on-error (~a "containing expression ends prematurely at position " (point)))]
         [_           (backward-char)
                      (backward-non-opener)])]))
  (backward-non-opener)
  (define c (char-before-point))
  (cond
    [(and c (closer? (char-category c)))  (loop 0 0 '())]
    [else                                 #f]))

#;(define-interactive (forward-sexp) ; old simple version
  "Move forward over a balanced expression."  
  (define (loop i depth seen)
    (define j (+ i 1))
    (define c (char-after-point))
    (when c
      (match depth
        [0 (match (char-category c)
             [(blank)             #f]
             [(closer _)          #f]
             [(comment-ender)     #f]
             [(string-starter e)  (when (= i 0)
                                    (forward-char) (forward-to-char e) (forward-char))]
             [(opener cp)         (forward-char)
                                  (loop j (+ depth 1) (cons cp seen))]             
             [_                   (forward-char)
                                  (loop j depth seen)])]
        [d (match (char-category c)
             [(string-starter e)  (forward-char) (forward-to-char e) (forward-char)
                                  (loop j depth seen)]
             [(opener cp)         (forward-char)
                                  (loop j (+ depth 1) (cons cp seen))]
             [(closer op)         (cond [(eqv? c (first seen)) (forward-char)
                                                               (unless (= depth 1)
                                                                 (loop (- depth 1) (rest seen)))]
                                        [else (message (~a "forward-sexp: expected " (first seen)))])]
             [(closer op)         #f]
             [_                   (forward-char)
                                  (loop j depth seen)])])))
  (forward-whitespace/quotes)
  (loop 0 0 '()))

#;(define-interactive (backward-sexp) ; old simple version
  "Move backward over a balanced expression."
  (define (loop depth seen)
    (define c (char-before-point))
    (when c
      (match depth
        [0 (match (char-category c)
             [(blank)           #f]
             [(opener _)        #f]
             [(comment-starter) #f]
             [(closer op)       (backward-char)
                                (loop (+ depth 1) (cons op seen))]
             [_                 (backward-char)
                                (loop depth seen)])]
        [d (match (char-category c)
             [(closer cp)         (backward-char)
                                  (loop (+ depth 1) (cons cp seen))]
             [(opener cp)         (cond [(eqv? c (first seen)) (backward-char)
                                                               (unless (= depth 1)
                                                                 (loop (- depth 1) (rest seen)))]
                                        [else
                                         (message (~a "backward-sexp: expected " (first seen)))])]
             [(opener cp)         #f]
             [_                   (backward-char)
                                  (loop depth seen)])])))
  (backward-whitespace/quotes)
  (loop 0 '()))


;(define (form-name-at-point)
;  inner-start
    

;;; 26.4 Moving by Parens

#;(define (forward-sexp)  ; C-M-f
    ; Move over balanced expression
    ; Skip over any white space.
    ; If next character is an opener, move over the corresponding closer.
    ; If next character beins a symbol, string or number, move over.    
    ...)

;;; 32.16 Counting Columns

(define (current-column)
  "Horizontal position of point"
  ; Note: current-column ought to count some characters differently 
  ;       such as tab. The function mark-column works on logical characters.
  (mark-column (get-point)))

  

;;; 37.9 Overlays

(define (overlay-set from to sym val [b (current-buffer)])
  (define i (position from))
  (define j (position to))
  (buffer-overlay-range-set! b (min i j) (max i j) sym val))

(define (overlay-ref sym i [b (current-buffer)])
  (buffer-overlay-ref b sym i))

(define (region-overlay sym val [b (current-buffer)])
  (cond [(use-region?)
         (define rb (region-beginning b))
         (define re (region-end b))
         (buffer-overlay-range-set! b rb re sym val)
         (deactivate-region-mark)]
        [else (void)]))

(define-interactive (balance-parens)
  (define seen (state-seen (parse-state-at-point)))  
  (if (empty? seen)
      (insert-char #\])
      (insert-char (first seen))))

(define (show-paren-ranges b)
  (define before-from  #f)
  (define before-to    #f)
  (define after-from   #f)
  (define after-to     #f)
  (define before-error #f) ; true if matching paren not found
  (define after-error  #f)

  (localize ([current-buffer b])
    (with-saved-point
      (define active? (mark-active? (get-mark)))
      ; before
      (define cb (char-before-point))
      (when (and (char? cb) (closer? (char-category cb)))
        (define to   (point))
        (define from (and (backward-list #f) (point)))
        (goto-char to)
        (cond [from (set! before-from from)
                    (set! before-to   to)]
              [else (set! before-error #t)
                    (set! before-from (- to 1))
                    (set! before-to   to)]))
      ; after
      (define ca (char-after-point))
      (when (and (char? ca) (opener? (char-category ca)))
        (define from (point))
        (define to   (and (forward-list #f) (point)))
        (goto-char from)
        (cond [to   (set! after-from from)
                    (set! after-to   to)]
              [else (set! after-error #t)
                    (set! after-from from)
                    (set! after-to   (+ from 1))]))
      ; restore mark
      (define m (get-mark))
      (if active? 
          (mark-activate! m)
          (mark-deactivate! m)))
    (values before-from  before-to
            after-from   after-to
            before-error after-error)))


(define (line-number-at-pos [pos #f])
  (cond
    [pos (with-saved-point
           (goto-char pos)
           (mark-row (get-point)))]
    [else  (mark-row (get-point))]))
