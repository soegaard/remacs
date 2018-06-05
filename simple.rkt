#lang racket/base
(provide (all-defined-out))

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

(require racket/class racket/list racket/match
         syntax/to-string
         racket/gui/base
         framework
         "buffer.rkt"
         "buffer-locals.rkt"
         "commands.rkt"
         "deletion.rkt"
         "frame.rkt"
         "killing.rkt"
         "line.rkt"
         "mark.rkt"
         "message.rkt"
         "mode.rkt"
         "parameters.rkt"
         "point.rkt"
         "region.rkt"
         "representation.rkt"
         "region.rkt"
         "render.rkt"
         "string-utils.rkt"
         "text.rkt"
         "window.rkt")

;;;
;;; MOVEMENT
;;;

(define-interactive (beginning-of-line)   (buffer-move-point-to-beginning-of-line! (current-buffer)))
(define-interactive (end-of-line)         (buffer-move-point-to-end-of-line!       (current-buffer)))

(define-interactive (move-to-column n)    (buffer-move-to-column! (current-buffer) n)) ; n=numprefix 

(define-interactive (backward-char)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point! (current-buffer) -1))
(define-interactive (forward-char [b (current-buffer)])
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point! b +1))

(define-interactive (previous-line)
  (display-point-line)
  (cond [(region-mark) => mark-deactivate!])  
  (buffer-move-point-up!   (current-buffer)))

(define-interactive (forward-line)
  ; this moves an entire text-line (see next-line for moving a screen line)
  (cond [(region-mark) => mark-deactivate!])
  (define b (current-buffer))
  (if (mark-on-last-line? (buffer-point b))
      (buffer-move-point-to-end-of-line! b)
      (buffer-move-point-down! b)))

(define (display-point-line) ; for debug
  (define point (buffer-point (current-buffer)))
  (writeln (line->string (mark-line point))))

(define-interactive (next-line)
  ; this moves point down a screen line 
  (cond [(region-mark) => mark-deactivate!])  
  (define point  (get-point))
  (cond
    [(mark-on-last-line? point)  (end-of-line)]
    [else (define n        (local 'screen-line-length))
          (define len      (line-length (mark-line point)))
          (define-values   (row col) (mark-row+column point))
          
          (define whole    (quotient  len n))   ; number of full screen lines
          (cond       
            [(>= col (* whole n)) ; we need to move to the next text line
             (beginning-of-line)
             (forward-line)        
             (move-to-column (min (remainder col n) (line-length (mark-line point))))]
            [(<= (+ col n) len)   ; there is room to move an entire screen line (same text line)
             (move-to-column (+ col n))]
            [else          ; there is not room to move an entire line, so go to end of this text line
             (move-to-column (line-length (mark-line point)))])]))

(define-interactive (backward-word)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-backward-word!   (current-buffer)))
(define-interactive (forward-word)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-forward-word!   (current-buffer)))


(define-interactive (page-down [w (current-window)])
  (define point                (buffer-point (window-buffer w)))
  (define start-mark           (window-start-mark w))
  (define end-mark             (window-end-mark w))
  (define-values (start-row _) (mark-row+column start-mark))
  (define-values (end-row  __) (mark-row+column end-mark))  
  (define delta                (max 0 (- (number-of-lines-on-screen w)
                                         (current-next-screen-context-lines))))
  (mark-move-down! point      delta)
  (mark-move-down! start-mark delta)
  (mark-move-down! end-mark   delta)
  (refresh-frame))
(define-interactive (page-up [w (current-window)])
  (define point                (buffer-point (window-buffer w)))
  (define start-mark           (window-start-mark w))
  (define end-mark             (window-end-mark w))
  (define-values (start-row _) (mark-row+column start-mark))
  (define-values (end-row  __) (mark-row+column end-mark))  
  (define delta                (max 0 (- (number-of-lines-on-screen w)
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
  (unless (get-mark) (new-mark (current-buffer) "*mark*")) ; TODO
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
  (define marks (buffer-marks (current-buffer)))
  (cond [(and (not (empty? marks)) (not (mark-active? (first marks))))
         (delete-mark! (first marks))
         (command-set-mark)]
        [(empty? marks)
         (command-set-mark)])
  (mark-activate! (region-mark)))

(define-interactive (backward-char/extend-region)
  (prepare-extend-region)
  (buffer-move-point! (current-buffer) -1))
(define-interactive (forward-char/extend-region)
  (prepare-extend-region)
  (buffer-move-point! (current-buffer) +1))
(define-interactive (previous-line/extend-region) 
  (prepare-extend-region)
  (buffer-move-point-up! (current-buffer)))
(define-interactive (next-line/extend-region)
  (prepare-extend-region)
  (define b (current-buffer))
  (if (mark-on-last-line? (buffer-point b))
      (buffer-move-point-to-end-of-line! b)
      (buffer-move-point-down! b)))
(define-interactive (forward-word/extend-region) 
  (prepare-extend-region)
  (buffer-forward-word! (current-buffer)))
(define-interactive (backward-word/extend-region)
  (prepare-extend-region)
  (buffer-backward-word! (current-buffer)))
(define-interactive (beginning-of-line/extend-region)
  (prepare-extend-region)
  (buffer-move-point-to-beginning-of-line! (current-buffer)))
(define-interactive (end-of-line/extend-region)
  (prepare-extend-region)
  (buffer-move-point-to-end-of-line! (current-buffer)))

(define-interactive (save-buffer)         (save-buffer!    (current-buffer)) (refresh-frame))
(define-interactive (save-buffer-as)      (save-buffer-as! (current-buffer)) (refresh-frame))
(define-interactive (save-some-buffers)   (save-buffer)) ; todo : ask in minibuffer
(define-interactive (beginning-of-buffer [b (current-buffer)])
  (buffer-move-point-to-beginning-of-buffer b))
(define-interactive (end-of-buffer       [b (current-buffer)])
  (buffer-move-point-to-end-of-buffer b))
(define-interactive (end-of-buffer/extend-region)
  (prepare-extend-region)
  (end-of-buffer (current-buffer)))
(define-interactive (beginning-of-buffer/extend-region)
  (prepare-extend-region)
  (beginning-of-buffer (current-buffer)))

(define-interactive (open-file-or-create [path (finder:get-file)])
  (when path ; #f = none selected
    (define b (buffer-open-file-or-create path))
    (set-window-buffer! (current-window) b)
    (current-buffer b)
    (refresh-frame (current-frame))))

(define-interactive (next-buffer) ; show next buffer in current window
  (define w (current-window))
  (define b (get-next-buffer))
  (set-window-buffer! w b)
  (current-buffer b))

(define-interactive (previous-buffer) ; show next buffer in current window
  (define w (current-window))
  (define b (get-previous-buffer))
  (set-window-buffer! w b)
  (current-buffer b))

(define-interactive (other-window) ; switch current window and buffer
  (define ws (frame-window-tree (current-frame)))
  (define w (list-next ws (current-window) eq?))
  (current-window w)
  (current-buffer (window-buffer w))
  (send (window-canvas w) focus))

(define-interactive (delete-window [w (current-window)])
  (window-delete! w))

(define-interactive (delete-other-windows [w (current-window)])
  (define ws (frame-window-tree (window-frame w)))
  (for ([win (in-list ws)])
    (unless (eq? w win)
      (delete-window win)))
  (refresh-frame))

(define-interactive (maximize-frame [f (current-frame)]) ; maximize / demaximize frame
  (when (frame? f)
    (define f% (frame-frame% f))
    (when (is-a? f% frame%)
      (send f% maximize (not (send f% is-maximized?))))))


(define (position->index pos)
  (if (mark? pos) (mark-position pos) pos))

(define (check-position who what)
  (unless (or (number? what) (mark? what))
    (error who "expected a position (index or mark), got ~a" what)))

(define-interactive (goto-char pos)
  (check-position 'goto-char pos)
  ; todo: add narrowing
  (buffer-move-point-to-position! (current-buffer) (position->index pos)))

(define-interactive (set-mark pos)
  (check-position 'set-mark pos)
  (with-saved-point
      (goto-char pos)
      (mark-activate!
       (buffer-set-mark-to-point))))



(define-interactive (text-scale-adjust m)
  (displayln `(text-scale-adjust ,m))
  (font-size (min 40 (max 1 (+ (font-size) m)))))


; create-new-buffer :  -> void
;   create new buffer and switch to it
(define-interactive (create-new-buffer)
  (define b (new-buffer (new-text) #f (generate-new-buffer-name "Untitled")))
  (set-window-buffer! (current-window) b)
  (current-buffer b)
  (refresh-frame (current-frame)))



; eval-buffer : -> void
;   Read and evaluate each s-expression in the current buffer one at a time.
(define-interactive (eval-buffer)
  (define b  (current-buffer)) ; we get the buffer here
  (displayln (list 'eval-buffer 'before: (current-buffer)))
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
  
  (parameterize ([current-namespace ns]
                 [current-buffer    b])
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
        (displayln (eval-syntax stx ns))))))

(define-interactive (test-buffer-output)
  (define b (new-buffer (new-text) #f (generate-new-buffer-name "*output*")))
  (define p (make-output-buffer b))
  (set-window-buffer! (current-window) b)
  (parameterize ([current-buffer      b]
                 [current-output-port p])
    (thread
     (λ ()
       (let loop ([n 0])
         (displayln n)
         (sleep 1.)
         (loop (+ n 1)))))))


; (self-insert-command k) : -> void
;   insert character k and move point
(define ((self-insert-command k))
  ; (display "Inserting: ") (write k) (newline)
  (define b (current-buffer))
  (when (use-region? b) (delete-region b))
  (define pa (current-prefix-argument))
  (define i (or (and (integer? pa) (positive? pa) pa) 1))
  (for ([_ (in-range i)])
    (buffer-insert-char-before-point! b k)))

(define-interactive (break-line [b (current-buffer)])
  (buffer-break-line! b))

(define-interactive (insert-line-after)
  ; insert new line after the current line,
  ; place point at beginning of new line
  ; [Sublime: cmd+enter]
  (end-of-line)
  (break-line))

(define-interactive (insert-line-before)
  ; insert new line before the current line,
  ; place point at beginning of new line
  ; [Sublime: cmd+enter]
  (beginning-of-line)
  (break-line)
  (backward-char))

(define-interactive (delete-region [b (current-buffer)])
  (region-delete b))

(define (buffer-backward-delete-char! [b (current-buffer)] [n 1])
  (if (and (= n 1) (use-region? b))
      (begin
        (delete-region)
        (delete-mark! (region-mark)))
      (buffer-delete-backward-char! b 1)))

; backward-delete-char
;   Delete n characters backwards.
;   If n=1 and region is active, delete region.
(define-interactive (backward-delete-char [n 1])
  (buffer-backward-delete-char! (current-buffer) n))

; select all
(define-interactive (mark-whole-buffer [b (current-buffer)])
  (parameterize ([current-buffer b])
    (end-of-buffer)
    (command-set-mark)
    (beginning-of-buffer)
    (refresh-frame)))

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
  (buffer-move-point-to-beginning-of-line! b)
  (buffer-kill-line b #t)  
  (forward-char b)
  (buffer-backward-delete-char! b))

; buffer-kill-line-to-beginning : buffer -> void
;   Kill text from point to beginning of line.
;   If point is at the beginning of line, the newline is deleted.
;   Point is at the beginning of line, if text from point to newline is all whitespace.
(define (buffer-kill-line-to-beginning [b (current-buffer)])
  ; TODO : store deleted text in kill ring
  (define m (buffer-point b))
  (define p1 (mark-position m))
  (define p2 (position-of-beginning-of-line m))
  (define rest-of-line (subtext->string (buffer-text b) p2 p1))
  ; delete to beginning of line
  (buffer-set-mark-to-point b)
  (buffer-move-point-to-beginning-of-line! b)
  (region-delete b)
  ; maybe delete newline
  (when (and (string-whitespace? rest-of-line)
             (not (= (mark-position m) 0)))
    (buffer-backward-delete-char! b)))


(define-interactive (kill-whole-line)
  (buffer-kill-whole-line)
  (update-current-clipboard-at-latest-kill)
  (refresh-frame))

(define-interactive (kill-line-to-beginning)
  (buffer-kill-line-to-beginning)
  (update-current-clipboard-at-latest-kill)
  (refresh-frame))

(define-interactive (recenter-top-bottom)
  (maybe-recenter-top-bottom #t)) ; todo: changed this from #t

(define-interactive (insert-latest-kill)
  ; If another application has put any text onto the system clipboard
  ; later than the latest kill, that text is inserted.
  ; Note: The timestamp is ignored in OS X.
  (define s (send the-clipboard get-clipboard-string 0))
  (cond
    [(or (equal? s "") 
         (equal? s (current-clipboard-at-latest-kill)))
     ; no changes to the system clipboard, so latest kill is used
     (buffer-insert-latest-kill)]
    [else
     ; system clipboard is newer
     (buffer-insert-string-before-point! (current-buffer) s)
     (refresh-frame)]))

(define-interactive (copy-region)
  (update-current-clipboard-at-latest-kill)
  (kill-ring-push-region))

(define-interactive (kill-word)
  ; kill to end of word
  (cond [(get-mark) => mark-deactivate!])  
  (forward-word/extend-region)
  (kill-region))

(define-interactive (backward-kill-word)
  ; Kill to beginning of word
  (cond [(get-mark) => mark-deactivate!])  
  (backward-word/extend-region)
  (kill-region))

(define-interactive (test) (set-mark 4) (goto-char 10))

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

(define-interactive (racket-mode [b (current-buffer)])
  (fundamental-mode b)       ; add all commands from fundamental mode
  ; name
  (set-major-mode! 'racket)
  (set-mode-name!  "Racket")
  ; keymap
  ;   Demonstrates how to override a keymap
  (set-buffer-local! 'local-keymap (λ (prefix key)
                                     (match prefix
                                       [(list)
                                        (match key
                                          ["return" (λ() (message "foo"))]
                                          [_        #f])]
                                       [_ #f]))
                     b))


