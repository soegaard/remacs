#lang racket
;;; WIP: Extend render-buffer and render-points to handle text lines longer than screen lines.
;;;      DONE render-buffer now handles lines longer than screen
;;;      DONE change render-points
;;;      DONE fix render-points
;;;      TODO make the max screen line size (now 60) a parameter
;;;      TODO make the line wrapping character a buffer-local

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
(require racket/gui/base syntax/to-string)
(require (only-in srfi/1 circular-list))

(require "parameters.rkt"
         "representation.rkt"
         "buffer.rkt"
         "line.rkt"
         "mark.rkt"
         "point.rkt"
         "region.rkt"
         "string-utils.rkt"
         "text.rkt")


;;;
;;; REGION
;;;

; region-delete-between! : [buffer] -> void
;   Delete all characters in region.
(define (region-delete-between! beg end [b (current-buffer)])
  (cond    
    [(mark< beg end) (buffer-dirty! b)
                     (define n (- (mark-position end) (mark-position beg)))
                     (define end-is-a-mark? (member end (buffer-marks b) eq?))
                     ; buffer-delete-backward-char! will update the positions
                     ; of all marks in buffer-marks, so if end is a mark (not the point)
                     ; we need to temporarily remove it.
                     (when end-is-a-mark?
                       (set-buffer-marks! b (remove end (buffer-marks b) eq?)))
                     (with-saved-point
                         (begin                           
                           (set-point! end)
                           (buffer-delete-backward-char! b n)))
                     (when end-is-a-mark?
                       (set-buffer-marks! b (cons end (buffer-marks b))))]
    [(mark< end beg) (region-delete-between! end beg b)]
    [else            (void)]))

; region-delete! : [buffer] -> void
;   Delete all characters in region.
(define (region-delete [b (current-buffer)])
  (define mark  (get-mark b))
  (define point (get-point b))
  (when (use-region? b)
    (buffer-dirty! b)
    (region-delete-between! mark point)
    (mark-deactivate! mark)))

; Note: Emacs has delete-active-region, delete-and-extract-region, and, delete-region



;;;
;;; KILLING
;;;

; The kill ring is a list of text blocks.
; The kill rings is shared between all buffers.
; (this allows copy+paste from one buffer to another)
; 

(require "ring-buffer.rkt")
(define kill-ring (new-ring))
(ring-insert! kill-ring "") ; make the kill ring non-empty

(define current-clipboard-at-latest-kill (make-parameter #f))
(define (update-current-clipboard-at-latest-kill)
  (current-clipboard-at-latest-kill 
   (send the-clipboard get-clipboard-string 0)))

(define (kill-ring-insert! s)
  (ring-insert! kill-ring s))

; kill-new : string -> void
;   Insert the string s in the kill ring as the latest kill.
(define (kill-new s)
  (kill-ring-insert! s))

; kill-append : string boolean -> void
;   Append the string s to the latest kill in the kill buffer.
;   If before? is true, prepend it otherwise postpend it.
(define (kill-append s before?)
  (define latest (ring-ref kill-ring 0))
  (define new    (if before?
                     (string-append s latest)
                     (string-append latest s)))
  (ring-set! kill-ring 0 new))

(define (kill-region-between-marks beg end [b (current-buffer)])
  (define s (region-between-marks->string beg end))
  (when s
    (kill-new s)
    (region-delete-between! beg end)))

(define (kill-region [b (current-buffer)])
  (kill-region-between-marks (get-mark) (get-point) b)
  (mark-deactivate! (get-mark))
  (update-current-clipboard-at-latest-kill)
  (refresh-frame))

; kill-region : ...
;   Delete the region and save the text in the kill ring.
;   Function that kills should use this function.
;   Use delete-region for deletion.
#;(define (kill-region [beg (get-mark)] [end (get-point)] [b (current-buffer)])  
  (define s (kill-ring-push-region b))
  (when s
    (delete-region b)
    (refresh-frame)))

(define (kill-ring-push-region [b (current-buffer)])
  (define s (region->string b))
  (when s
    (kill-ring-insert! s)
    s))

(define (buffer-insert-latest-kill [b (current-buffer)])
  (define s (or (and (not (ring-empty? kill-ring))
                     (ring-ref kill-ring 0))
                ""))
  (buffer-insert-string-before-point! b s)
  #;(refresh-frame))

#;(define (move-line-up [b (current-buffer)])
  (define p  (buffer-point b))
  (define m1 (copy-mark p))
  (define m2 (copy-mark p))
  (mark-move-beginning-of-line! m1)
  (mark-move-end-of-line! m2)
  
  ...
  )
  

; buffer-kill-line : buffer -> void
;   Kill text from point to end of line.
;   If point is at end of line, the newline is deleted.
;   Point is at end of line, if text from point to newline is all whitespace.
(define (buffer-kill-line [b (current-buffer)] [called-by-kill-whole-line #f])
  ; setup region, then use kill-ring-push-region and delete-region
  (define m  (buffer-point b))
  (define p1 (mark-position m))
  (define p2 (position-of-end-of-line m))
  (define rest-of-line (subtext->string (buffer-text b) p1 p2))
  (define eol? (and (string-whitespace? rest-of-line)
                    (not (= (+ (mark-position m) 1) (position-of-end b)))))
  ; delete to end of line
  (buffer-set-mark-to-point b)  
  (buffer-move-point-to-end-of-line! b)
  (when eol?
    (buffer-move-point! b +1)
    #;(forward-char b))
  
  (kill-ring-push-region)
  (delete-region b))

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
  (delete-region b)
  ; maybe delete newline
  (when (and (string-whitespace? rest-of-line)
             (not (= (mark-position m) 0)))
    (buffer-backward-delete-char! b)))

;;;
;;; MESSAGES
;;;

(define current-message (make-parameter #f))

(define (message str [msg (current-message)])
  (send msg set-label str))

;;;
;;; COMPLETIONS
;;;

(define current-completion-buffer (make-parameter #f))
(define current-completion-window (make-parameter #f))

; (require "trie.rkt")
(require (only-in srfi/13 string-prefix-length))
(define completions '())
(define (add-name-to-completions name)
  (set! completions (sort (cons (~a name) completions) string<?)))
(define (completions-lookup partial-name)
  (define r (regexp (~a "^" partial-name)))
  (filter (λ (name) (regexp-match r name))
          completions))
(define (longest-common-prefix xs)
  (match xs
    ['()                ""]
    [(list x)            x]
    [(list "" y zs ...) ""]
    [(list x  y zs ...) (longest-common-prefix 
                         (cons (substring x 0 (string-prefix-length x y)) zs))]))

(define (completions->text so-far cs)
  (define explanation (list (~a "Completions for: " so-far)))
  (new-text (list->lines (for/list ([c (append explanation cs)]) (~a c "\n")))))


;;;
;;; INTERACTIVE COMMANDS
;;;

;;; Interactive commands are user commands. I.e. the user can
;;; call them via key-bindings or via M-x.
;;; Names of interactive commands are registered in order to 
;;; provide completions for the user.


(define all-interactive-commands-ht (make-hash))
(define (add-interactive-command name cmd)
  (hash-set! all-interactive-commands-ht (~a name) cmd))
(define (lookup-interactive-command cmd-name)
  (hash-ref all-interactive-commands-ht (~a cmd-name) #f))

(define-syntax (define-interactive stx)
  (syntax-parse stx
    [(d-i name:id expr)
     (syntax/loc stx
       (begin
         (add-name-to-completions 'name)
         (define name expr)
         (add-interactive-command 'name name)))]
    [(d-i (name:id . args) expr ...)
     (syntax/loc stx
       (begin
         (add-name-to-completions 'name)
         (define (name . args)
           (with-suspended-rendering               
               expr ...))
         (add-interactive-command 'name name)))]
    [_ (raise-syntax-error 'define-interactive "bad syntax" stx)]))

(define-interactive (test) (set-mark 4) (goto-char 10))

;; Names from emacs
(define-interactive (beginning-of-line)   (buffer-move-point-to-beginning-of-line! (current-buffer)))
(define-interactive (end-of-line)         (buffer-move-point-to-end-of-line!       (current-buffer)))

(define-interactive (move-to-column n)    (buffer-move-to-column!  (current-buffer) n)) ; n=numprefix 

(define-interactive (backward-char)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point! (current-buffer) -1))
(define-interactive (forward-char [b (current-buffer)])
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point! b +1))
(define-interactive (previous-line)       
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point-up!   (current-buffer)))
(define-interactive (forward-line)
  ; this moves an entire text-line (see next-line for moving a screen line)
  (cond [(region-mark) => mark-deactivate!])
  (define b (current-buffer))
  (if (mark-on-last-line? (buffer-point b))
      (buffer-move-point-to-end-of-line! b)
      (buffer-move-point-down! b)))
(define-interactive (next-line)
  ; this moves a screen line 
  (cond [(region-mark) => mark-deactivate!])
  (define b (current-buffer))
  (if (mark-on-last-line? (buffer-point b))      
      (buffer-move-point-to-end-of-line! b)
      (buffer-move-point-down! b)))
(define-interactive (backward-word)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-backward-word!   (current-buffer)))
(define-interactive (forward-word)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-forward-word!   (current-buffer)))
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
(define current-next-screen-context-lines (make-parameter 2)) ; TODO use a buffer local?
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
(define-interactive (beginning-of-buffer [b (current-buffer)]) (buffer-move-point-to-position! b 0))
(define-interactive (end-of-buffer       [b (current-buffer)])
  (buffer-move-point-to-position! b (- (buffer-length b) 1)))
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

(define-interactive (command-set-mark)
  (buffer-set-mark-to-point (current-buffer)))

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


; create-new-buffer :  -> void
;   create new buffer and switch to it
(define-interactive (create-new-buffer)
  (define b (new-buffer (new-text) #f (generate-new-buffer-name "Untitled")))
  (set-window-buffer! (current-window) b)
  (current-buffer b)
  (refresh-frame (current-frame)))

; eval-buffer : -> void
;   read the s-expression in the current buffer one at a time,
;   evaluate each s-expression
;   TODO: Note done: Introduce namespace for each buffer
(define-interactive (eval-buffer)
  (define b (current-buffer))
  (define t (buffer-text b))
  (define s (text->string t))
  (define in (open-input-string s))
  (define ns (buffer-locals b))
  (parameterize ([current-namespace ns]
                 [current-buffer    b])
    (define (read1 in)
      (define stx (read-syntax 'read-from-buffer in))
      (if (syntax? stx)
          (namespace-syntax-introduce stx)
          stx)) ; probably eof    
    (for ([stx (in-port read1 in)])
      (with-handlers
          ([exn:fail? (λ (e)
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


;;;
;;; BUFFER LOCALS
;;;

(require "buffer-locals.rkt")

(define (buffer-local-keymap [b (current-buffer)])
  (ref-buffer-local b 'local-keymap #f))


;;; TODO : What should the default namespace be?


;;;
;;; MODES
;;;

; The buffer-local variable  major-mode  holds a symbol representing the major mode.
; Example: the symbol 'fundamental-mode represents the fundamental mode.

(require "mode.rkt")

(define-interactive (fundamental-mode)
  (set-major-mode! 'fundamental)
  (set-mode-name!  "Fundamental")
  ; add all interactive commands defined here to fundamental mode
  (for ([(name cmd) (in-hash all-interactive-commands-ht)])
    (define sym (string->symbol name))
    (set-buffer-local! (current-buffer) sym cmd)))

(define-interactive (text-mode)
  (fundamental-mode)       ; add all commands from fundamental mode
  (set-major-mode! 'text)
  (set-mode-name!  "Text"))

(define-interactive (racket-mode [b (current-buffer)])
  (fundamental-mode)       ; add all commands from fundamental mode
  ; name
  (set-major-mode! 'racket)
  (set-mode-name!  "Racket")
  ; keymap
  (set-buffer-local!
   b 'local-keymap
   (λ (prefix key)
     (match prefix
       [(list)
        (match key
          ["return" (λ() (message "foo"))]
          [_        #f])]
       [_ #f]))))

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
  #;(newline)
  #;(begin
      (write (list 'key-event->key
                   'key                (send event get-key-code)
                   'other-shift        (send event get-other-shift-key-code)
                   'other-altgr        (send event get-other-altgr-key-code)
                   'other-shift-altgr  (send event get-other-shift-altgr-key-code)
                   'other-caps         (send event get-other-caps-key-code)))
      (newline))
  (define shift? (send event get-shift-down))
  (define alt?   (send event get-alt-down))
  (define ctrl?  (send event get-control-down))
  (define caps?  (send event get-caps-down))
  (define cmd?   (case (system-type 'os)
                   ; racket reports cmd down as meta down
                   [(macosx) (send event get-meta-down)]
                   ; other systems do not have cmd
                   [else     #f]))  
  (define meta?  (case (system-type 'os)
                   ; use the alt key as meta
                   [(macosx) (send event get-alt-down)]
                   [else     (send event get-meta-down)]))    ; mac: cmd, pc: alt, unix: meta
  #;(displayln (list 'shift shift? 'alt alt? 'ctrl ctrl? 'meta meta? 'cmd cmd? 'caps caps?))
  
  (define c      (send event get-key-code))
  ; k = key without modifier
  (define k      (cond
                   [(and ctrl? alt?)  c]
                   [cmd?              c]
                   [alt?              (or (and (symbol? c) c)
                                          (send event get-other-altgr-key-code))] ; OS X: 
                   [else              c]))
  
  (define alt-is-meta? #t)
  (define cmd-is-meta? #f)
  (define (alt   k) 
    (cond
      [alt-is-meta? k]
      [alt?         (~a "A-" k)]
      [else         k]))
  (define (ctrl  k) (if ctrl?  (~a "C-" k) k))
  (define (cmd   k) 
    (cond
      [cmd-is-meta? k]
      [cmd?         (~a "D-" k)]
      [else         k]))
  (define (meta  k) (if meta?  (~a "M-" k) k))
  (define (shift k) (if shift? (~a "S-" k) k))
  
  (let ([k (match k 
             ['escape     "ESC"] 
             [#\backspace "backspace"]
             [#\return    "return"]
             [#\space     "space"]
             [_ k])])
    (cond 
      [(eq? k 'control)      'control] ; ignore control + nothing
      [(and (symbol? c) meta? shift?) (~a "M-S-" k)]
      [(or ctrl? alt? meta? cmd?)     (alt (ctrl (cmd (meta (shift k)))))]
      [(and shift? (eq? k 'shift))    'shift]
      [(and shift? (symbol? k))       (~a "S-" k)]
      [else                           k])))

(define (remove-last xs)
  (if (null? xs) xs
      (reverse (rest (reverse xs)))))

(define current-prefix-argument (make-parameter #f)) ; set by C-u

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

;;;
;;; STATUS LINE
;;;

; The status line is shown at the bottom of a buffer window.
(define (status-line-hook)
  (define b (current-buffer))
  (define-values (row col) (mark-row+column (buffer-point b)))
  (define save-status (if (buffer-modified? b) "***" "---"))
  (~a save-status  
      "   " "Buffer: "          (buffer-name) "    " "(" row "," col ")"
      "   " "Position: " (mark-position (buffer-point (current-buffer)))
      "   " "Length: "   (buffer-length (current-buffer))
      "   " "Mode: "     "(" (get-mode-name) ")"))

;;;
;;; WINDOWS
;;;

; A window is an area of the screen used to display a buffer.
; Windows are grouped into frames.
; Each frame contains at least one window.

(define all-windows '())
(define current-window (make-parameter #f))

; new-window : frame panel buffer -> window
(define (new-window f panel b [parent #f] #:borders [borders #f])
  ; parent is the parent window, #f means no parent parent window
  (define bs (or borders (seteq)))
  (define start (new-mark b "*start*"))
  (define end   (new-mark b "*end*"))
  (mark-move! end (buffer-length b))
  (define w (window f panel bs #f parent b start end))
  (window-install-canvas! w panel)
  (set! all-windows (cons w all-windows))
  w)

; get-buffer-window : [buffer-or-name] -> window
;   return first window in which buffer is displayed
(define (get-buffer-window [buffer-or-name (current-buffer)])
  (define b (get-buffer buffer-or-name))
  (for/first ([w all-windows]
              #:when (eq? (get-buffer (buffer-name (window-buffer w))) b))
    w))

; get-buffer-window-list : [buffer-or-name] -> list-of-windows
;   get list of all windows in which buffer is displayed
(define (get-buffer-window-list [buffer-or-name (current-buffer)])
  (define b (get-buffer buffer-or-name))
  (for/list ([w all-windows]
             #:when (eq? (get-buffer (window-buffer w)) b))
    w))

; get-buffer-frame-list : [buffer-or-name] -> list-of-frame
;   get list of all frames in which buffer is displayed
(define (get-buffer-frame-list [buffer-or-name (current-buffer)])
  (define ws (get-buffer-window-list buffer-or-name))
  (set->list (list->set (map window-frame ws))))

(define (get-buffer-frame [buffer-or-name (current-buffer)])
  (define fs (get-buffer-frame-list buffer-or-name))
  (if (empty? fs) #f (first fs)))

(define (refresh-buffer [buffer-or-name (current-buffer)])
  (refresh-frame (get-buffer-frame buffer-or-name)))
(current-refresh-buffer refresh-buffer)

; REMINDER
;    (struct window (frame canvas parent buffer) #:mutable)
;    (struct horizontal-split-window window (left  right) #:mutable)

; split-window-right : [window] -> void
;   split the window in two, place the new window at the right
;
; Implementation note:
;   The (frame-panel f) holds the panel that display the current windows
;   Make this panel the left son of a new horisontal panel
;   and add a new panel to the right for the new window.
(define (split-window-right [w (current-window)])
  (define f (window-frame w))
  ; the parent p of a window w might be a horizontal- or vertical window
  (define b  (window-buffer w))
  (define bs (window-borders w))
  (define c  (window-canvas w))
  (define p  (window-parent w))
  (define root? (not (window? p)))
  ; the new split window get the parent of our old window
  (define parent-panel (if root? (frame-panel f) (window-panel p)))
  (define pan (new horizontal-panel% [parent parent-panel]))
  (define sp  (horizontal-split-window f pan bs #f p #f #f #f w #f))
  ; the old window get a new parent
  (set-window-parent! w sp)
  (send c reparent pan)
  ;; A little space before the next window
  ; (new horizontal-pane% [parent pan] [min-height 2] [stretchable-height #f])
  ; now create the new window to the right
  (define bs2 (set-add bs 'left))
  (define w2 (new-window f pan b sp #:borders bs2))
  (set-horizontal-split-window-right! sp w2)
  ; replace the parent window with the new split window
  (cond 
    [root? ; root window?
     (set-frame-windows! f sp)]
    [(horizontal-split-window? p)
     ; is w a left or a right window?
     (if (eq? (horizontal-split-window-left p) w) 
         (begin
           (set-horizontal-split-window-left!  p sp)
           (send (window-canvas (horizontal-split-window-right p)) reparent parent-panel))
         (set-horizontal-split-window-right! p sp))]
    [(vertical-split-window? p)
     ; is w above or below?
     (if (eq? (vertical-split-window-above p) w) 
         (begin
           (set-vertical-split-window-above! p sp)
           (send (window-canvas (vertical-split-window-below p)) reparent parent-panel))
         (set-vertical-split-window-below! p sp))])
  (send c focus))

(define (split-window-below [w (current-window)])
  (define f (window-frame w))
  ; the parent p of a window w might be a horizontal- or vertical window
  (define b  (window-buffer w))
  (define bs (window-borders w))
  (define c  (window-canvas w))
  (define p  (window-parent w))
  
  (define root? (not (window? p)))
  ; the parent of the new split window (sp), is the parent the window (w) to be split
  (define parent-panel (if root? (frame-panel f) (window-panel p)))
  (define new-panel    (new vertical-panel% [parent parent-panel]))
  (define sp (vertical-split-window f new-panel bs #f p #f #f #f w #f))
  ; the split window becomes he parent of the old window
  (set-window-parent! w sp)
  ; this means that the canvas of w, now belongs the the new panel
  (send c reparent new-panel)
  ; The bottom of the split window contains a new window, showing the same buffer
  ; The new window is required to draw the top border
  (define bs2 (set-add bs 'top))
  (define w2 (new-window f new-panel b sp #:borders bs2))
  (set-vertical-split-window-below! sp w2)
  ; The split window takes the place of w in the parent of w
  (cond 
    [root? 
     (set-frame-windows! f sp)]
    [(horizontal-split-window? p)
     ; is w a left or a right window?
     (if (eq? (horizontal-split-window-left p) w) 
         (begin
           (set-horizontal-split-window-left!  p sp)
           (send (window-canvas (horizontal-split-window-right p)) reparent parent-panel))
         (set-horizontal-split-window-right! p sp))]
    [(vertical-split-window? p)
     ; is w above or a?
     (if (eq? (vertical-split-window-above p) w)
         (begin 
           (set-vertical-split-window-above! p sp)
           (send (window-canvas (vertical-split-window-below p)) reparent parent-panel))
         (set-vertical-split-window-below! p sp))]
    [else (error "Internal Error")])
  (send c focus))

(define (left-window?  hw w) (eq? (horizontal-split-window-left  hw) w))
(define (right-window? hw w) (eq? (horizontal-split-window-right hw) w))
(define (above-window? hw w) (eq? (vertical-split-window-above   hw) w))
(define (below-window? hw w) (eq? (vertical-split-window-below   hw) w))

; replace : any any list -> list
;   copy zs but replace occurences of x with y
(define (replace x y zs)
  (for/list ([z (in-list zs)])
    (if (eq? z x) y z)))

(define (window-delete! w)
  (set! all-windows (remq w all-windows))
  (define fp (window-panel (frame-windows (current-frame))))
  (send fp begin-container-sequence)
  (define (window-backend w)
    ; split-windows are backed by a panel holding subwindows,
    ; whereas a single window is backed by a canvas
    (if (split-window? w) (window-panel  w) (window-canvas w)))
  ; to delete the window w, it must be removed from its parent
  (define p (window-parent w))
  ; only split windows can hold subwindows
  (unless (split-window? p)
    (error 'window-delete "can't delete window"))
  ;; since the parent is a split window, it must hold another window:
  (define ow ; other window
    (cond [(vertical-split-window? p) 
           (if (above-window? p w)
               (vertical-split-window-below p)
               (vertical-split-window-above p))]
          [(horizontal-split-window? p) 
           (if (left-window? p w)
               (horizontal-split-window-right p)
               (horizontal-split-window-left p))]
          [else (error 'window-delete! "internal error")]))
  ;; The sole purpose of the parent is to hold w and ow, 
  ;; since w is to be deleted, the parent p is no longer needed.
  ;; To replace p with ow, we need to grab the grand parent and replace parent with other window
  (define gp (window-parent p))
  (set-window-parent! ow gp)
  (cond [(horizontal-split-window? gp)
         (if (left-window? gp p)
             (set-horizontal-split-window-left!  gp ow)
             (set-horizontal-split-window-right! gp ow))]
        [(vertical-split-window? gp)
         (if (above-window? gp p)
             (set-vertical-split-window-above! gp ow)
             (set-vertical-split-window-below! gp ow))]
        [else (void)])    
  ;; if the current window is deleted, we need to make a new window the current one.
  (when (eq? (current-window) w) (current-window ow))
  (current-buffer (window-buffer (current-window)))
  ;; The window structures are now updated, but the gui panels need to be updated too.
  (cond
    [(eq? gp 'root)
     (set-window-borders! ow '()) ; root has no borders
     (define f (window-frame w))
     (set-frame-windows! f ow)
     (define panel (frame-panel f))
     (send panel change-children  (λ (cs) '()))
     (send (window-backend ow) reparent panel)]
    [else ; gp is a split window
     (set-window-borders! ow (window-borders p))
     (define panel (window-panel gp))
     ; make ow a child of the grand parent
     (send (window-backend ow) reparent panel)
     ; now the ow is last child, so we need to move to the where p is
     (send panel change-children 
           (λ (cs) (replace (window-backend p) (window-backend ow)
                            (filter (λ(c) (not (eq? c (window-backend ow))))
                                    cs))))])
  (send fp end-container-sequence)
  ;; send keyboard focus to other window
  (send (window-backend ow) focus))

;;;
;;; FRAMES
;;;

(define current-frame (make-parameter #f))

(define (refresh-frame [f (current-frame)])
  (unless (current-rendering-suspended?)
    (when (and f (frame? f))
      (render-frame f))))

(current-refresh-frame refresh-frame)

(define (frame-window-tree [f (current-frame)])
  (define (loop w)
    (match w
      [(horizontal-split-window f _ _ c p b s e l r)               (append (loop l) (loop r))]
      [(vertical-split-window   f _ _ c p b s e u l)               (append (loop u) (loop l))]
      [(window frame panel borders canvas parent buffer start end) (list w)]))
  (flatten (loop (frame-windows f))))

;;;
;;; COLORS
;;;

(define (color? x)
  (is-a? x color%))

(define (hex->color x)
  (define blue  (remainder           x        256))
  (define green (remainder (quotient x   256) 256))
  (define red   (remainder (quotient x 65536) 256))
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

(define background-color         base03)
(define region-highlighted-color base00)
(define text-color               base1)
(define border-color             base00)

(define border-pen    
  (new pen% [color base00] [width 1] [style 'solid] [cap 'butt] [join 'miter]))

; Note: point-colors starts with the brightest colors.
(define point-colors (circular-list base3  base3  base2  base1 
                                    base0  base00 base01 base02 base03 base03
                                    base03 base03 base02 base01 base00
                                    base0  base1  base2))

;;;
;;; FONT
;;;
(define default-font-size 16)

(define font-style  (make-parameter 'normal))  ; style  in '(normal italic)
(define font-weight (make-parameter 'normal))  ; weight in '(normal bold)
(define the-font-size default-font-size)
(define (font-size [n #f]) (when n (set! the-font-size n)) the-font-size)
(define font-family (make-parameter 'modern))  ; fixed width
(define (use-default-font-settings)
  (font-style  'normal)
  (font-weight 'normal)
  ;(font-size   16)
  (font-family 'modern))
(define font-ht (make-hash))                   ; (list size family style weight) -> font  
(define (get-font)
  (define key (list (font-size) (font-family) (font-style) (font-weight)))
  (define font (hash-ref font-ht key #f))
  (unless font
    (set! font (make-object font% (font-size) (font-family) (font-style) (font-weight)))
    (hash-set! font-ht key font))
  font)
(define (toggle-bold)    (font-weight (if (eq? (font-weight) 'normal) 'bold   'normal)))
(define (toggle-italics) (font-style  (if (eq? (font-style)  'normal) 'italic 'normal)))
(define (default-fixed-font)  (get-font))

(define-interactive (text-scale-adjust m)
  (displayln `(text-scale-adjust ,m))
  (font-size (min 40 (max 1 (+ (font-size) m)))))

;;;
;;; CANVAS
;;;

(define (canvas-dimensions c)
  (define dc (send c get-dc))
  (define xmin 0)
  (define xmax (send c get-width))
  (define ymin 0)
  (define ymax (send c get-height))
  (values xmin xmax ymin ymax))

;;; 
;;; LINES
;;;

; The size of a line is the same as the font size plus one.
(define (line-size) (+ (font-size) 1))

(define (number-of-lines-on-screen w)
  (define b (window-buffer w))
  (define c (window-canvas w))
  (define-values (xmin xmax ymin ymax) (canvas-dimensions c))
  (define width  (- xmax xmin))
  (define height (- ymax ymin))
  (define fs (font-size))
  (define ls (line-size)) ; BUG todo this looks wrong: the font-size might not match the pixel size
  ;; Placement of point relative to lines on screen
  (define n (max 0 (quotient height ls)))
  n)

;;;
;;; GUI
;;;


(current-point-color point-colors)

(define-syntax (with-suspended-rendering stx)
  (syntax-parse stx
    [(_with-suspended-rendering body ...)
     (syntax/loc stx
       (let ()
         (parameterize ([current-rendering-suspended? #t])
           body ...)
         (render-frame (current-frame))))]))


(define (maybe-recenter-top-bottom [force? #f] [w (current-window)])
  ; move current buffer line to center of window
  (define b                    (window-buffer w))
  (define n                    (number-of-lines-on-screen w))
  (define-values (row col)     (mark-row+column (buffer-point b)))
  (define start-mark           (window-start-mark w))
  (define end-mark             (window-end-mark w))
  (define-values (start-row _) (mark-row+column start-mark))
  (define-values (end-row  __) (mark-row+column end-mark))  
  ;(displayln (list 'before: 'row row 'start-row start-row 'end-row end-row 'n num-lines-on-screen))
  (when (or force?
            (not (and (<= start-row row) (< row (+ start-row n)))))
    ;(define n num-lines-on-screen)
    (define n/2 (quotient n 2))
    (define new-start-row (max (- row n/2) 0))
    (define new-end-row   (+ new-start-row n))
    (mark-move-up! start-mark (- start-row new-start-row))
    (mark-move-up! end-mark   (-   end-row  new-end-row))
    (set! start-row new-start-row)
    (set! end-row   new-end-row))
  (values start-row end-row))

(define (sort-numbers xs) (sort xs <))

(define screen-line-length 40)


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
            (unless sl (error))
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
              (send dc set-pen old-pen))))))))

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


; create-window-canvas : window panel% -> canvas
; this-window : the non-gui structure representing the window used to display a buffer.
; f           : the non-gui structure representing the frame of the window
; panel       : the panel which the canvas has as parent
(define (window-install-canvas! this-window panel)
  (define f (window-frame this-window))
  ;;; PREFIX 
  ; keeps track of key pressed so far
  (define prefix '())
  (define (add-prefix! key) (set! prefix (append prefix (list key))))
  (define (clear-prefix!)   (set! prefix '()))

  (define handle-mouse
    (let ([left-down? #f]
          [last-left-click-row #f] [last-left-click-col #f])          
      (λ (e type)
      (unless (member type '(left-down left-up motion))
        (error "expected 'left-down, 'left-up or 'motion got: ~a" type))
      ; a left click will move the point to the location clicked
      (define (exact r) (exact-floor r))
      (define-values (x y) (values (exact (send e get-x)) (exact (send e get-y))))
      (define c  (window-canvas this-window))
      (define dc (send c get-dc))
      ; Canvas Dimensions
      (define-values (xmin xmax ymin ymax) (canvas-dimensions c))
      ;; Dimensions
      (define width  (- xmax xmin))
      (define height (- ymax ymin))
      (define fs (exact (font-size)))
      (define ls (exact (line-size)))
      (define num-lines-on-screen   (max 0 (quotient height ls)))
      (define n num-lines-on-screen)    
      (define-values (w h _ __) (send dc get-text-extent "x")) ; font width and height
      (set! h (exact h)) (set! w (exact w))
      ; compute screen row and column
      (define screen-row (quotient y ls))
      (define screen-col (quotient x w))      
      ; find first row displayed on screen
      (define start-mark              (window-start-mark this-window))
      (define-values (start-row ___)  (mark-row+column start-mark))
      ; Add start-row and row to get the buffer start row
      (define row (+ start-row screen-row))
      (define col screen-col)  ; TODO: change this when wrapping of long lines gets support
      (cond        
        [(eq? type 'left-down)
         ; register where left mouse clicked happened
         ; wait for left-up or motion to do anythin         
         (set! left-down? #t)
         (set! last-left-click-row row)
         (set! last-left-click-col col)]
        [(or (eq? type 'left-up)
             (and (eq? type 'motion) left-down?))
         ; register mouse up
         (when (eq? type 'left-up) (set! left-down? #f))
         ; for both up and motion, create region
         (define m (get-mark))
         (define b (window-buffer this-window))
         (define p (buffer-point b))
         (when (and m (mark-active? m)) (mark-deactivate! m))
         (cond   ; no drag
           [(and (equal? last-left-click-row row) (equal? last-left-click-col col)) 
            (mark-move-to-row+column! p row col)]
           [else ; mouse dragged
            (mark-move-to-row+column! p row col)
            (unless m (buffer-set-mark-to-point b) (set! m (get-mark)))
            (when (and last-left-click-row last-left-click-col)
              (mark-move-to-row+column! m last-left-click-row last-left-click-col))
            (mark-activate! m)])
         (maybe-recenter-top-bottom #t this-window)
         (refresh-frame)]))))
  
  (define window-canvas%
    (class canvas%
      (inherit has-focus?)
      ;; Buffer
      (define the-buffer #f)
      (define (set-buffer b) (set! the-buffer b))
      (define (get-buffer b) the-buffer)
      ;;; Focus Events
      (define/override (on-event e) ; mouse events
        (define type (send e get-event-type))
        ;(displayln type)
        (case type
          [(left-down) (handle-mouse e 'left-down)]
          [(left-up)   (handle-mouse e 'left-up)]
          [(motion)    (handle-mouse e 'motion)]))
      (define/override (on-focus event)
        (define w this-window)
        (define b (window-buffer w))
        ; (displayln (list 'on-focus (buffer-name b)))
        (current-buffer b)
        (current-window w))
      ;; Key Events
      (define/override (on-char event)
        (current-point-color point-colors) ; make points visible when keyboard is active
        ; TODO syntax  (with-temp-buffer body ...)
        (define key-code (send event get-key-code))
        (unless (equal? key-code 'release)
          (define key (key-event->key event))
          ; (send msg set-label (~a "key: " key))
          (define local-keymap (buffer-local-keymap (current-buffer)))
          (define binding (or (and local-keymap (local-keymap prefix key))
                              (global-keymap prefix key)
                              ; If A-<key> is unbound, then use the character as-is.
                              ; This makes A-a insert å.
                              (and (char? key-code)
                                   (global-keymap prefix key-code))))
          (match binding
            [(? procedure? thunk)  (clear-prefix!) (thunk) (current-prefix-argument #f)]
            [(list 'replace pre)   (set! prefix pre)]
            ['prefix               (add-prefix! key)]
            ['ignore               (void)]
            ['exit                ; (save-buffer! (current-buffer))
             ; TODO : Ask how to handle unsaved buffers
             (send (frame-frame% f) on-exit)]
            ['release             (void)]
            [_                    (unless (equal? (send event get-key-code) 'release)
                                    (when (and (empty? prefix) key)
                                      (message (~a "<" key "> undefined")))
                                    (clear-prefix!))]))
        ; todo: don't trigger repaint on every key stroke ...
        (send canvas on-paint))
      ;; Rendering
      (public on-paint-points)
      (define (display-status-line s)
        (when (eq? (window-buffer this-window) (current-buffer))
          (send (frame-status-line f) set-label s)))
      (define (on-paint-points on?) ; render points only
        (unless (current-rendering-suspended?)
          (parameterize ([current-render-points-only? #t]
                         [current-show-points?        on?])
            (display-status-line (status-line-hook))
            (render-window this-window))))
      (define/override (on-paint) ; render everything
        (unless (current-rendering-suspended?)
          (parameterize ([current-show-points? #t])
            (display-status-line (status-line-hook))
            (render-frame f))))
      (super-new)))
  (define canvas (new window-canvas% [parent panel]))
  (set-window-canvas! this-window canvas)
  (send canvas min-client-width  20)
  (send canvas min-client-height 20)
  ;;; blinking cursor / point
  ; start update-points thread
  (thread (λ () (let loop ([on? #t])
                  (sleep/yield 0.1)
                  (unless (current-rendering-suspended?)
                    (send canvas on-paint-points on?))
                  (loop (not on?)))))
  canvas)

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
                 

(module+ test
  (define ib illead-buffer)
  ;(current-buffer ib)
  (current-buffer scratch-buffer)
  (define f  (frame #f #f #f #f #f))
  (frame-install-frame%! f) ; installs frame% and panel
  
  (define p (frame-panel f))
  (define w (new-window f p scratch-buffer 'root))
  
  ;(define sp (vertical-split-window f #f #f #f #f #f #f))  
  ; (define w  (window f #f c sp ib))
  ; (define c2 #f)
  ; (define w2 (window f #f c2 sp (get-buffer "*scratch*")))
  ; (set-vertical-split-window-above! sp w)
  ; (set-vertical-split-window-below! sp w2)
  ; (set-frame-windows! f sp)
  
  (set-frame-windows! f w)
  (current-window w)
  (current-frame f)
  
  (send (window-canvas w) focus))

(define (display-file path)
  (with-input-from-file path
    (λ ()
      (for ([l (in-lines)])
        (displayln l)))))
