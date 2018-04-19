#lang racket
;;; TODO Finish eval-buffer
;;;        ok use buffer-local namespace for evaluation
;;;        - fix new-buffer (buffer-top needs to be required)
;;;        - convenient initial namespace (now racket/base)
;;;        - catch errors
;;;        - output where?
;;; TODO mark-whole-buffer  (normally bound to C-x h)
;;; TODO Wordwrap
;;; TODO #\tab now inserts 4 space
;;;      But ... if a rendering breaks if the a file contains #\tab
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
;;; TODO Modes
;;; TODO previous-buffer (parallel to next-buffer)
;;; TODO Introduce global that controls which key to use for meta
;;; TODO Implement open-input-buffer
;;; TODO Allow negative numeric prefix
;;; TODO Holding M and typing a number should create a numeric prefix.
;;; TODO Completions ala http://sublimetext.info/docs/en/extensibility/completions.html

(module+ test (require rackunit))
(require "dlist.rkt" (for-syntax syntax/parse) framework)
(require racket/gui/base)
(require (only-in srfi/1 circular-list))

(require "parameters.rkt"
         "representation.rkt"
         "buffer.rkt"
         "line.rkt"
         "mark.rkt"
         "region.rkt"
         "string-utils.rkt"
         "text.rkt")

;;;
;;; LINES
;;;

(module+ test
  (define illead-text 
    (new-text 
     (list->lines
      (list "x\n"))))
  #;(define illead-text 
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


;;;
;;; TEXT
;;;

(module+ test
  (void (create-new-test-file "illead.txt"))
  ; (displayln "--- illead test file ---")
  ; (write (path->text "illead.txt")) (newline)
  ; (displayln "---")
  ; (write illead-text) (newline)
  ; (displayln "---")
  #;(check-equal? (path->text "illead.txt") illead-text))

;;;
;;; BUFFER
;;;

(module+ test
  (provide illead-buffer)
  (define illead-buffer (new-buffer illead-text "illead.txt" (generate-new-buffer-name "illead")))
  (save-buffer! illead-buffer)
  #;(check-equal? (path->text "illead.txt") illead-text))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define b (new-buffer (new-text) "illead.txt" (generate-new-buffer-name "illead")))
  (read-buffer! b)
  #;(check-equal? b illead-buffer))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define append-buffer (new-buffer (new-text)))
  (append-to-buffer-from-file append-buffer "illead.txt")
  (append-to-buffer-from-file append-buffer "illead.txt")
  (save-buffer! b) ; make sure the buffer is unmodified before comparison
  #;(check-equal? (buffer-text append-buffer) (text-append! illead-text illead-text)))

;;;
;;; REGION
;;;

; Note: Emacs has delete-active-region, delete-and-extract-region, and, delete-region

; region-delete! : [buffer] -> void
;   Delete all characters in region.
(define (region-delete [b (current-buffer)])
  (when (use-region? b)
    (buffer-dirty! b)
    (define marks (buffer-marks b))
    (define mark  (first marks))
    (define point (buffer-point b))
    (cond
      [(mark< mark point) (define n (- (mark-position point) (mark-position mark)))
                          (buffer-delete-backward-char! b n)]
      [(mark< point mark) (define n (- (mark-position mark) (mark-position point)))
                          (buffer-move-point! b n)
                          (buffer-delete-backward-char! b n)]
      [else               (void)])
    (mark-deactivate! mark)))

;;;
;;; KILLING
;;;

; The kill ring is a list of text blocks.
; The kill rings is shared between all buffers.
; (this allows copy+paste from one buffer to another)
; 

(require "ring-buffer.rkt")
(define kill-ring (new-ring))
(define current-clipboard-at-latest-kill (make-parameter #f))

(define (update-current-clipboard-at-latest-kill)
  (current-clipboard-at-latest-kill 
   (send the-clipboard get-clipboard-string 0)))

(define (kill-ring-insert! s)
  (ring-insert! kill-ring s))

(define (kill-region [b (current-buffer)])
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
  (refresh-frame))

; buffer-kill-line : buffer -> void
;   Kill text from point to end of line.
;   If point is at end of line, the newline is deleted.
;   Point is at end of line, if text from point to newline is all whitespace.
(define (buffer-kill-line [b (current-buffer)] [called-by-kill-whole-line #f])
  ; TODO : store deleted text in kill ring
  (define m (buffer-point b))
  (define p1 (mark-position m))
  (define p2 (position-of-end-of-line m))
  (define rest-of-line (subtext->string (buffer-text b) p1 p2))
  ; delete to end of line
  (buffer-set-mark b)
  (buffer-move-point-to-end-of-line! b)
  (delete-region b)
  ; maybe delete newline
  (unless called-by-kill-whole-line
    (when (and (string-whitespace? rest-of-line)
               (not (= (+ (mark-position m) 1) (position-of-end b))))
      (forward-char b)
      (buffer-backward-delete-char! b))))

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
  (buffer-set-mark b)
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
;;; BUFFER LOCALS
;;;

(define (set-buffer-local! b sym v)
  (define ns (buffer-locals b))
  (namespace-set-variable-value! sym v #f ns))

(define (ref-buffer-local b sym 
                          [on-error (λ () (error 'ref-buffer-local (~a sym " is undefined")))])
  (define (on-failure) (if (procedure? on-error) (on-error) on-error))
  (define ns (buffer-locals b))
  (namespace-variable-value sym #t on-failure ns))

(define-namespace-anchor default-namespace-anchor)
(define default-namespace (namespace-anchor->namespace default-namespace-anchor))
;;; TODO : What should the default namespace be?

(define (lookup-default sym [on-error #f])
  (namespace-variable-value sym #t on-error default-namespace))


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
         (define (name . args) expr ...)
         (add-interactive-command 'name name)))]
    [_ (raise-syntax-error 'define-interactive "bad syntax" stx)]))

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
(define-interactive (next-line)
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
  (buffer-forward-word!    (current-buffer)))

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
  (buffer-move-point-down! (current-buffer)))
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

(define-interactive (other-window) ; switch current window and buffer
  (define ws (frame-window-tree (current-frame)))
  (define w (list-next ws (current-window) eq?))
  (current-window w)
  (current-buffer (window-buffer w))
  (send (window-canvas w) focus))

(define-interactive (delete-window [w (current-window)])
  (window-delete! w))

(define-interactive (maximize-frame [f (current-frame)]) ; maximize / demaximize frame
  (when (frame? f)
    (define f% (frame-frame% f))
    (when (is-a? f% frame%)
      (send f% maximize (not (send f% is-maximized?))))))

(define-interactive (command-set-mark)
  (buffer-set-mark (current-buffer)))

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
  (parameterize ([current-namespace ns])
    (define (read1 in)
      (define stx (read-syntax 'read-from-buffer in))
      (if (syntax? stx)
          (namespace-syntax-introduce stx)
          stx)) ; probably eof
    (for ([stx (in-port read1 in)])
      (displayln (eval-syntax stx ns))))) ; todo : catch errors here

; (self-insert-command k) : -> void
;   insert character k and move point
(define ((self-insert-command k))
  ; (display "Inserting: ") (write k) (newline)
  (define b (current-buffer))
  (when (use-region? b) (delete-region b))
  (buffer-insert-char-before-point! b k))

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

(define-interactive (mark-whole-buffer [b (current-buffer)])
  (parameterize ([current-buffer b])
    (end-of-buffer)
    (command-set-mark)
    (beginning-of-buffer)))


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

(define global-keymap
  (λ (prefix key)
    ; (write (list prefix key)) (newline)    
    ; if prefix + key event is bound, return thunk
    ; if prefix + key is a prefix return 'prefix
    ; if unbound and not prefix, return #f
    (define (digits->number ds) (string->number (list->string ds)))
    (define (digit-char? x) (and (char? x) (char<=? #\0 x #\9)))
    ; todo: allow negativ numeric prefix
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
      [(list "C-u" (? digit-char? ds) ...)
       (displayln "HERE")
       (match key
         [(? digit-char?) 'prefix]
         [#\c             (displayln "X") (λ () (move-to-column (digits->number ds)))]
         [else            #f])]
      [(list "ESC") 
       (match key
         [#\b         backward-word]
         [#\f         forward-word]
         [_           #f])]
      [(list "C-x")
       (match key
         [#\0         delete-window]
         [#\2         split-window-below]
         [#\3         split-window-right]
         [#\s         save-some-buffers]
         [#\o         other-window]
         ["C-s"       save-buffer]
         ['right      next-buffer]
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
         ; Meta + something
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
         ["M-d"           (λ () (buffer-display (current-buffer)))]
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
      "  " "Buffer: "          (buffer-name) "    " "(" row "," col ")"
      "  " "Position: " (mark-position (buffer-point (current-buffer)))
      "  " "Length: "   (buffer-length (current-buffer))))

;;;
;;; WINDOWS
;;;

; A window is an area of the screen used to display a buffer.
; Windows are grouped into frames.
; Each frame contains at least one window.

(define window-ht (make-hash))
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
  w)

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
  (when (and f (frame? f))
    (render-frame f)))

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

(define point-colors (circular-list base03 base03 base02 base01 base00
                                    base0  base1  base2  base3  base3  base2  base1 
                                    base0  base00 base01 base02 base03 base03))

;;;
;;; FONT
;;;

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
(define (toggle-bold)    (font-weight (if (eq? (font-weight) 'normal) 'bold   'normal)))
(define (toggle-italics) (font-style  (if (eq? (font-style)  'normal) 'italic 'normal)))
(define default-fixed-font  (get-font))

;;;
;;; GUI
;;;

(define current-render-points-only? (make-parameter #f))
(define current-show-points?        (make-parameter #f))
(define current-point-color         (make-parameter point-colors)) ; circular list of colors

(define (maybe-recenter-top-bottom [force? #f] [w (current-window)])
  ; move current buffer line to center of window
  (define b (window-buffer w))
  (define c (window-canvas w))
  ; Canvas Dimensions
  (define xmin 0)
  (define xmax (send c get-width))
  (define ymin 0)
  (define ymax (send c get-height))
  ;; Dimensions
  (define width  (- xmax xmin))
  (define height (- ymax ymin))
  (define fs (font-size))
  (define ls (+ fs 1)) ; BUG todo this looks wrong: the font-size might not math the pixel size
  ;; Placement of point relative to lines on screen
  (define num-lines-on-screen   (max 0 (quotient height ls)))
  (define n num-lines-on-screen)
  (define-values (row col)      (mark-row+column (buffer-point b)))
  (define start-mark            (window-start-mark w))
  (define end-mark              (window-end-mark w))
  (define-values (start-row _)  (mark-row+column start-mark))
  (define-values (end-row   __) (mark-row+column end-mark))  
  ;(displayln (list 'before: 'row row 'start-row start-row 'end-row end-row 'n num-lines-on-screen))
  (when (or force?
            (not (and (<= start-row row) (< row (+ start-row n)))))
    (define n num-lines-on-screen)
    (define n/2 (quotient n 2))
    (define new-start-row (max (- row n/2) 0))
    (define new-end-row   (+ new-start-row n))
    (mark-move-up! start-mark (- start-row new-start-row))
    (mark-move-up! end-mark   (-   end-row  new-end-row))
    (set! start-row new-start-row)
    (set! end-row   new-end-row))
  (values start-row end-row))

(define (render-buffer w)
  (define b  (window-buffer w))
  (define c  (window-canvas w))
  (define dc (send c get-dc))
  ;; Canvas Dimensions
  (define xmin 0)
  (define xmax (send c get-width))
  (define ymin 0)
  (define ymax (send c get-height))
  ;; Dimensions
  (define width  (- xmax xmin))
  (define height (- ymax ymin))
  (define fs (font-size))
  (define ls (+ fs 1)) ; linesize -- 1 pixel for spacing
  ;; Placement of point relative to lines on screen
  (define num-lines-on-screen   (max 0 (quotient height ls)))
  (define-values (row col)      (mark-row+column (buffer-point  b)))
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
      ; draw text
      (for/fold ([y ymin] [p 0]) ; p the position of start of line
                ([l (text-lines (buffer-text b))]
                 [i (in-range (+ start-row num-lines-on-screen))])
        (cond
          [(< i num-lines-to-skip)
           (when (and reg-begin (<= reg-begin p) (< p reg-end)) (set-text-background-color #t))
           (when (and reg-end   (<= reg-end   p))               (set-text-background-color #f))
           (values y (+ p (line-length l)))]
          [else
           (define strings (line-strings l))
           (define n (length strings))
           (define (sort-numbers xs) (sort xs <))
           (for/fold ([x xmin] [p p]) ([s strings] [i (in-range n)])
             ; p is the start position of the string s
             (match s
               [(? string?)
                (define sn (string-length s))
                ; find positions of points and marks in the string
                (define positions-in-string
                  (sort-numbers
                   (append (for/list ([m (buffer-marks b)]  #:when (<= p (mark-position m) (+ p sn)))
                             (mark-position m))
                           (for/list ([m (buffer-points b)] #:when (<= p (mark-position m) (+ p sn)))
                             (mark-position m)))))
                ; split the string at the mark positions (there might be a color change)
                (define start-positions (cons p positions-in-string))
                (define end-positions   (append positions-in-string (list (+ p sn))))
                (define substrings      (map (λ (start end) (substring s (- start p) (- end p)))
                                             start-positions end-positions))
                ; draw the strings one at a time
                (define-values (next-x next-p)
                  (for/fold ([x x] [p p]) ([t substrings])
                    ; turn region colors on/off
                    (when (and reg-begin (<= reg-begin p) (< p reg-end)) 
                      (set-text-background-color #t))
                    (when (and reg-end   (<= reg-end   p))               
                      (set-text-background-color #f))
                    (define u ; remove final newline if present
                      (or (and (not (equal? t ""))
                               (char=? (string-ref t (- (string-length t) 1)) #\newline)
                               (substring t 0 (max 0 (- (string-length t) 1))))
                          t))
                    (values (draw-string u x y) (+ p (string-length t)))))                
                ; return the next x position
                (values next-x next-p)]        
               [(property 'bold)     (toggle-bold)    (send dc set-font (get-font))   (values x p)]
               [(property 'italics)  (toggle-italics) (send dc set-font (get-font))   (values x p)]
               [(property (? color? c))               (send dc set-text-foreground c) (values x p)]
               [_ (displayln (~a "Warning: Got " s))                                  (values x p)]))
           (values (+ y ls)
                   (+ p (line-length l)))]))
      ; get point and mark height
      ;(define font-width  (send dc get-char-width))
      ;(define font-height (send dc get-char-height))
      (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
      ; Note: The font-height is slightly larger than font-size
      ; (displayln (list 'render-buffer 'font-height font-height 'font-size (font-size)))
      ; resume flush
      (send dc resume-flush)
      (void)))
  ; draw points
  (render-points w start-row end-row))

; the parameter last-milliseconds tracks time when points are rendered,
; this is used to fade the colors correctly 
(define last-milliseconds (make-parameter #f))
(define (millisseconds-delta)
  (define now  (current-milliseconds))
  (define last (last-milliseconds))
  (unless last (last-milliseconds now) (set! last (- now 1)))
  (define delta (- now (last-milliseconds)))
  (last-milliseconds now)
  delta)

(define color-fuel (make-parameter 0))
(define (render-points w start-row end-row)
  (define b  (window-buffer w))
  (define c  (window-canvas w))
  (define dc (send c get-dc))
  ;; Canvas Dimensions
  (define xmin 0)
  (define xmax (send c get-width))
  (define ymin 0)
  (define ymax (send c get-height))
  ; ---
  ; (displayln (millisseconds-delta)) ; expect values around 100
  (define colors (current-point-color))
  (define points-pen (new pen% [color (car colors)]))
  (define fuel  (color-fuel))
  (define delta (millisseconds-delta))
  (for ([i (quotient (+ fuel delta) 100)])
    (current-point-color (cdr colors)))
  (color-fuel (remainder (+ fuel delta) 100))
  (define points-off-pen (new pen% [color background-color]))
  
  ; get point and mark height
  (define-values (font-width font-height _ __) (send dc get-text-extent "M"))
  (when b
    (define active? (send (window-canvas w) has-focus?))
    (when active?
      (define on? (current-show-points?))
      (for ([p (buffer-points b)])
        (define-values (r c) (mark-row+column p))
        (when #t #;(<= start-row r end-row)
          (define x (+ xmin (* c    font-width)))
          (define y (+ ymin (* (- r start-row) (+ font-height -2)))) ; why -2 ?
          (when (and (<= xmin x xmax) (<= ymin y) (<= y (+ y font-height -1) ymax))
            (define old-pen (send dc get-pen))
            (send dc set-pen (if #t ;on? 
                                 points-pen
                                 points-off-pen))
            (send dc draw-line x y x (min ymax (+ y font-height -1)))
            (send dc set-pen old-pen)))))))

(define (render-window w)
  (define c  (window-canvas w))
  (define dc (send c get-dc))
  (send dc suspend-flush)

  ;; sane defaults
  (use-default-font-settings)
  (send dc set-font default-fixed-font)
  (send dc set-text-mode 'solid) ; solid -> use text background color
  (send dc set-background background-color)
  (unless (current-render-points-only?)
    (send dc clear))
  
  (send dc set-text-background background-color)
  (send dc set-text-foreground text-color)
  
  ;; render buffer
  (define xmin 0)
  (define xmax (send c get-width))
  (define ymin 0)
  (define ymax (send c get-height))
  
  ;; draw borders
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
  (render-buffer w)

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
  
  (define window-canvas%
    (class canvas%
      (inherit has-focus?)
      ;; Buffer
      (define the-buffer #f)
      (define (set-buffer b) (set! the-buffer b))
      (define (get-buffer b) the-buffer)
      ;;; Focus Events
      (define/override (on-focus event)
        (define w this-window)
        (define b (window-buffer w))
        ; (displayln (list 'on-focus (buffer-name b)))
        (current-buffer b)
        (current-window w))
      ;; Key Events
      (define/override (on-char event)
        ; TODO syntax  (with-temp-buffer body ...)
        (define key-code (send event get-key-code))
        (unless (equal? key-code 'release)
          (define key (key-event->key event))
          ; (send msg set-label (~a "key: " key))
          (define binding (or (global-keymap prefix key)
                              ; If A-<key> is unbound, then use the character as-is.
                              ; This makes A-a insert å.
                              (and (char? key-code)
                                   (global-keymap prefix key-code))))
          (match binding
            [(? procedure? thunk)  (clear-prefix!) (thunk)]
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
        (parameterize ([current-render-points-only? #t]
                       [current-show-points?        on?])
          (display-status-line (status-line-hook))
          (render-frame f)))     
      (define/override (on-paint) ; render everything
        (parameterize ([current-show-points? #t])
          (display-status-line (status-line-hook))
          (render-frame f)))
      (super-new)))
  (define canvas (new window-canvas% [parent panel]))
  (set-window-canvas! this-window canvas)
  (send canvas min-client-width  20)
  (send canvas min-client-height 20)
  ; start update-points thread
  (thread (λ () (let loop ([on? #t])
                  (sleep/yield 0.1)
                  (send canvas on-paint-points on?)
                  (loop (not on?)))))
  canvas)

(define make-frame frame)
(define (frame-install-frame%! this-frame)
  ;;; FRAME SIZE
  (define min-width  800)
  (define min-height 800)
  ;;; FRAME  
  (define frame (new frame% [label "Editor"] [style '(fullscreen-button)]))
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
    ;; File Menu
    (define fm (new menu% (label "File") (parent mb)))
    (new-menu-item fm "New File"   #\n #f           (λ (_ e) (create-new-buffer)))
    (new-menu-item fm "Open"       #\o #f           (λ (_ e) (open-file-or-create)))
    (new-menu-item fm "Save"       #\s #f           (λ (_ e) (save-buffer)))
    (new-menu-item fm "Save As..." #\s '(shift cmd) (λ (_ e) (save-buffer-as)))
    ;; Edit Menu
    (define em (new menu% (label "Edit") (parent mb)))
    (new-menu-item em "Copy"  #\c #f (λ (_ e) (copy-region)))
    (new-menu-item em "Cut"   #\x #f (λ (_ e) (kill-region)))
    (new-menu-item em "Paste" #\v #f (λ (_ e) (insert-latest-kill)))
    ;; Edit | Text
    (define etm (new menu% (label "Text") (parent em)))
    (new-menu-item etm "Kill line"         #\k         '(ctl)      (λ (_ e) (kill-line)))
    (new-menu-item etm "Kill Whole Line"   #\k         '(ctl shift)(λ (_ e) (kill-whole-line)))    
    (new-menu-item etm "Kill to Beginning" #\backspace '(cmd)      (λ (_ e) (kill-line-to-beginning)))
    
    ;; Help Menu
    (new menu% (label "Help") (parent mb))) 
  (create-menubar)
  ;; PANEL
  ; The holds contains the shown window 
  (define panel (new vertical-panel% 
                     [parent frame]
                     [min-width min-width]
                     [min-height 200]))
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


