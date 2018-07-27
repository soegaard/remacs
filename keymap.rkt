#lang racket/base
(provide (all-defined-out))

(require racket/format racket/list racket/match racket/string
         "buffer.rkt"
         "buffer-locals.rkt"
         "colors.rkt"
         "completion.rkt"
         "commands.rkt"
         "embedded.rkt"
         "frame.rkt"
         "killing.rkt"
         "mark.rkt"
         "message.rkt"
         "parameters.rkt"
         "point.rkt"
         "representation.rkt"
         "simple.rkt"
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
    ; (writeln (list 'global-keymap: 'prefix prefix 'key key))
    ; if prefix + key event is bound, return thunk
    ; if prefix + key is a prefix return 'prefix
    ; if unbound and not prefix, return #f
    (define (digits->number ds) (string->number (list->string ds)))
    (define (digit-char? x) (and (char? x) (char<=? #\0 x #\9)))
    ; todo: allow negative numeric prefix
    (match prefix      
      [(list "M-x" more ...)
       (match key
         [(or "ESC" "C-g")  (message "") 'clear-prefix]
         ["backspace"       (define new (remove-last more))
                            (message (string-append* `("M-x " ,@(map ~a new))))
                            `(replace ,(cons "M-x" new))]
         [#\tab             (cond
                              [(equal? (last more) "C-g")
                               (void)]
                              [else          
                               (define so-far (string-append* (map ~a more)))
                               (define cs     (completions-lookup so-far))
                               (writeln cs)
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
                                  (buffer-insert-string! (get-point) (text->string t))
                                  ;; replace prefix with the longest unique completion
                                  (define pre (longest-common-prefix cs))
                                  (message (~a "M-x " pre))
                                  (list 'replace (cons "M-x" (string->list pre)))])])]
         ["return"          (define cmd-name (string-append* (map ~a more)))
                            (define cmd      (lookup-interactive-command cmd-name))
                            (cond [cmd   (message "") cmd]
                                  [else  (match prefix
                                           [(list _M-x c ...)
                                            (message (~a "M-x " (apply ~a c) " [Unbound]"))
                                            (list 'replace (cons "M-x" c))])])]
         [_                 (message (string-append* `("M-x " ,@(map ~a more) ,(~a key))))
                            'prefix])]
      [(list "C-u" (? digit-char? ds) ... x ...)
       (match key
         [(? digit-char?) 'prefix]
         [#\c             (displayln (digits->number ds)) (λ () (move-to-column (digits->number ds)))]
         [else            (current-prefix-argument (digits->number ds))
                          (global-keymap x key)])]
      [(list "M-g" more ...)
       (match key         
         [#\b         (λ () (region-overlay 'weight 'bold))]
         [#\i         (λ () (region-overlay 'style  'italic))]
         ['shift     '(replace ("M-g"))]
         [#\Y         (λ () (region-overlay 'color   yellow))] 
         [#\O         (λ () (region-overlay 'color   orange))] 
         [#\R         (λ () (region-overlay 'color   red))]
         [#\G         (λ () (region-overlay 'color   green))]
         [#\B         (λ () (region-overlay 'color   blue))]
         [_           #f])]
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
         [#\=         what-cursor-position]
         [#\.         set-fill-prefix]
         [#\f         set-fill-column]
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
         ["M-g"          (message "M-g ") 'prefix]
         ; movement
         ['left           backward-char]
         ['right          forward-char]
         ; ['up             previous-line]
         ['up             previous-line]  ; screen line up
         ['down           next-line]      ; screen line down
         ['wheel-down     next-line]
         ['wheel-up       previous-line]
         ["S-left"        backward-char/extend-region]
         ["S-right"       forward-char/extend-region]
         ["S-up"          previous-line/extend-region]
         ["S-down"        next-line/extend-region]         
         ; Ctrl + something
         ["C-a"           move-beginning-of-line]
         ["C-b"           backward-char]
         ["C-e"           move-end-of-line]
         ["C-f"           forward-char]
         ["C-k"           kill-line]
         ["D-backspace"   kill-line-to-beginning]
         ["C-l"           recenter-top-bottom]
         ["C-S-backspace" kill-whole-line]
         ["C-p"           previous-line]
         ["C-n"           next-line]
         ["C-o"           open-line]
         ["C-w"           kill-region]
         ; Cmd + something
         ["D-a"           mark-whole-buffer]   ; select all 
         ["D-c"           copy-region]         ; copy  (Edit Menu)
         ["D-x"           kill-region]         ; cut   (Edit Menu)
         ["D-v"           insert-latest-kill]  ; paste (Edit Menu)
         ["D-left"        move-beginning-of-line]
         ["D-right"       move-end-of-line]
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
         ["C-M-left"      backward-list] ; xxx
         ["C-M-right"     forward-list]         
         ["M-S-left"      backward-word/extend-region]
         ["M-S-right"     forward-word/extend-region]
         ["f1"            test-buffer-output]
         ; ["M-d"           (λ () (buffer-display (current-buffer)))]
         ["M-d"           kill-word]
         ["M-backspace"   backward-kill-word]
         ["M-s"           save-buffer]
         ["M-S"           save-buffer-as]
         ["M-e"           eval-buffer]
         ["M-w"           'exit #;(λ () (save-buffer! (current-buffer)) #;(send frame on-exit) )]
         [#\return        break-line]
         [#\backspace     backward-delete-char]   ; backspace
         [#\rubout        (λ () (error 'todo))]   ; delete
         ; make tab insert 4 spaces
         [#\tab           (local indent-for-tab)]
         ['home           move-beginning-of-line] ; fn+left
         ['end            move-end-of-line]       ; fn+right
         ['next           page-down] 
         ['prior          page-up]
         ["C-space"       command-set-mark]
         ; place self inserting characters after #\return and friends
         ["space"         (self-insert-command #\space)]
         [(? char? k)     (self-insert-command k)]
         [_               #f])]
      [_ #f])))

